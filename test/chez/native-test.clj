;; jolt.native gate: native regions (Carp-style owned arrays, compiled to C).
;; Each accepted program is compiled twice with a tracking allocator, by cc (as
;; jolt build compiles it) and by the JIT (MIR, as the REPL and jolt run do),
;; and run both ways on identical copies of its inputs. The two must agree on
;; the result, the exception (class and message) and what they did to the input
;; arrays, and leave no live allocation and no bad free behind, on success and
;; on every error exit. The answer must also match an oracle that is not the
;; region's own C: the body run as plain Clojure (rows marked :raw, where Jolt
;; means the same thing), and test/native/golden.edn, which holds what each row
;; answered when regions still had a Jolt fallback (overflow, the JVM's
;; exceptions, and the rest Jolt's own arithmetic does differently).
;; Rejected programs must fail to compile with the expected message.
;; Needs cc and the JIT (make mirlib). Run: bin/jolt run test/chez/native-test.clj
;; NATIVE_GOLDEN=write adds the cc answers of rows golden.edn lacks (after
;; checking what they answer) and drops entries no row produces; the entries
;; it keeps are still checked, never re-recorded.
(ns native-test
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [jolt.scheme :as scm]
            [jolt.native.ast :as ast]
            [jolt.native.memory :as mem]
            [jolt.native.emit :as emit]
            [jolt.native.rt :as rt]
            [jolt.native :refer [defnative defextern]]))

(def failures (atom []))
(defn fail! [msg] (swap! failures conj msg) (println "FAIL" msg))

(def work-dir (str (System/getProperty "user.dir") "/target/native-test"))
(.mkdirs (java.io.File. work-dir))

;; A per-library allocation tracker: every live block in a small registry, a
;; free of anything not in it (double free, wild pointer) raises the bad flag.
;; No headers and no attributes before a declaration, which c2mir refuses.
(defn tracker [sym]
  (str "#include <stdint.h>\n#include <stddef.h>\n"
       "void *calloc(size_t, size_t); void *malloc(size_t); void free(void *);\n"
       "static void *jn_live_set[65536]; static int64_t jn_live_n = 0, jn_bad = 0, jn_peak = 0;\n"
       "static void *jn_track(void *p) { if (p) { if (jn_live_n < 65536) jn_live_set[jn_live_n++] = p; else jn_bad = 1;"
       " if (jn_live_n > jn_peak) jn_peak = jn_live_n; } return p; }\n"
       "static void jn_untrack(void *p) { if (!p) return; for (int64_t i = 0; i < jn_live_n; i++)"
       " if (jn_live_set[i] == p) { jn_live_set[i] = jn_live_set[--jn_live_n]; free(p); return; } jn_bad = 1; }\n"
       "#define JN_CALLOC(n, s) jn_track(calloc((n), (s)))\n"
       "#define JN_MALLOC(n) jn_track(malloc(n))\n"
       "#define JN_FREE(p) jn_untrack(p)\n"
       "int64_t " sym "_live(void) { return jn_live_n; }\n"
       "int64_t " sym "_bad(void) { return jn_bad; }\n"
       "int64_t " sym "_peak(void) { int64_t p = jn_peak; jn_peak = jn_live_n; return p; }\n"))

(def flv (scm/call "record-accessor" (scm/call "record-rtd" (double-array 1)) 0))

;; One shared, plain definition of the cc regions' status (this harness calls
;; regions from one thread). Each region is its own shared object here, and an
;; object with its own _Thread_local takes a pthread key (macOS allows 512; a
;; run of ~510 regions aborted); an extern _Thread_local shared across dlopen'd
;; objects reads back stale under Android's emulated TLS. The region objects
;; name jn_rt after their source so an --as-needed linker keeps it. The JIT's
;; regions keep their status in the JIT (jolt_mir_status).
(def rt-so (str work-dir "/jn_rt.dylib"))
(spit (str work-dir "/jn_rt.c")
      (str "#include <stdint.h>\n"
           "typedef struct { int64_t code; int64_t a; int64_t b; double d; int64_t len;"
           " uintptr_t stack_lo; uintptr_t stack_thr; } jn_out;\n"
           "jn_out jn_tls;\n"))
(let [p (.start (ProcessBuilder. ^java.util.List ["cc" "-O2" "-shared" "-fPIC" "-o" rt-so (str work-dir "/jn_rt.c")]))]
  (when-not (zero? (.waitFor p)) (throw (ex-info "cc failed for jn_rt" {}))))
(scm/call "load-shared-object" rt-so)

;; C functions for the defextern rows: linked into every cc region object like
;; jn_rt, and loaded into the process so their jolt.ffi bindings and the JIT
;; resolve them too
(def ext-so (str work-dir "/jn_ext.dylib"))
(spit (str work-dir "/jn_ext.c")
      (str "#include <stdint.h>\n"
           "int64_t jn_ext_mix(int64_t a, double b) { return a * 3 + (int64_t)(b * 10.0); }\n"
           "_Bool jn_ext_odd(int64_t a) { return (a & 1) != 0; }\n"
           "double jn_ext_half(double x) { return x / 2.0; }\n"
           "static int64_t jn_ext_count;\n"
           "void jn_ext_tick(void) { jn_ext_count++; }\n"
           "int64_t jn_ext_ticks(void) { return jn_ext_count; }\n"
           "double jn_ext_sum(double *xs, int64_t n) { double s = 0; for (int64_t i = 0; i < n; i++) s += xs[i]; return s; }\n"
           "void jn_ext_scale(double *xs, int64_t n, double k) { for (int64_t i = 0; i < n; i++) xs[i] *= k; }\n"
           "void jn_ext_fill(int64_t *xs, int64_t n, int64_t v) { for (int64_t i = 0; i < n; i++) xs[i] = v + i; }\n"
           "int64_t jn_ext_lsum(int64_t *xs, int64_t n) { int64_t s = 0; for (int64_t i = 0; i < n; i++) s += xs[i]; return s; }\n"
           ;; a SIMD kernel a region's (on \"defined(__AVX2__)\" ...) body calls:
           ;; AVX2 code by its own target attribute on x86-64, a plain loop elsewhere
           "static int64_t jn_ext_dots;\n"
           "int64_t jn_ext_dot_calls(void) { return jn_ext_dots; }\n"
           "#if defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))\n"
           "#include <immintrin.h>\n"
           "__attribute__((target(\"avx2\"))) double jn_ext_dot_avx2(double *xs, int64_t n, double *ys, int64_t m) {\n"
           "  jn_ext_dots++; __m256d acc = _mm256_setzero_pd(); int64_t i = 0;\n"
           "  for (; i + 4 <= n; i += 4) acc = _mm256_add_pd(acc, _mm256_mul_pd(_mm256_loadu_pd(xs + i), _mm256_loadu_pd(ys + i)));\n"
           "  double l[4]; _mm256_storeu_pd(l, acc); double s = l[0] + l[1] + l[2] + l[3];\n"
           "  for (; i < n; i++) s += xs[i] * ys[i]; (void)m; return s; }\n"
           "#else\n"
           "double jn_ext_dot_avx2(double *xs, int64_t n, double *ys, int64_t m) {\n"
           "  jn_ext_dots++; double s = 0; for (int64_t i = 0; i < n; i++) s += xs[i] * ys[i]; (void)m; return s; }\n"
           "#endif\n"))
(let [p (.start (ProcessBuilder. ^java.util.List ["cc" "-O2" "-shared" "-fPIC" "-o" ext-so (str work-dir "/jn_ext.c")]))]
  (when-not (zero? (.waitFor p)) (throw (ex-info "cc failed for jn_ext" {}))))
(scm/call "load-shared-object" ext-so)
(def counter (atom 0))

(defn- caller
  "The region's entry and accessors, each a foreign procedure FP makes from a C
  name, as a fn that calls the region as the backend's call site does."
  [r sym fp]
  (let [[argtypes ret] (emit/foreign-types r)
        ;; the default (safe) call: collect-safe, borrowed arrays locked around it
        entry (fp sym (str "(" (str/join " " argtypes) ")") ret true)
        status (fp (str sym "_status") "()" "integer-64" false)
        err-l (fp (str sym "_err_l") "(integer-64)" "integer-64" false)
        err-d (fp (str sym "_err_d") "()" "double" false)
        lock (scm/proc "lock-object")
        unlock (scm/proc "unlock-object")
        copy-out (case (:type r)
                   [:array :double] (fp (str sym "_copy_out") "(scheme-object uptr integer-64)" "void" false)
                   [:array :long] (fp (str sym "_copy_out") "(scheme-object uptr integer-64)" "integer-64" false)
                   nil)
        elem (when (= [:array :long] (:type r)) (fp (str sym "_elem") "(uptr integer-64)" "integer-64" false))
        free (when (= [:array :long] (:type r)) (fp (str sym "_free") "(uptr)" "void" false))
        q (fn [n] (fp (str sym "_" n) "()" "integer-64" false))]
    {:live (q "live") :bad (q "bad") :peak (q "peak")
     :call (fn [& args]
             (let [cargs (mapv (fn [p a] (if (ast/borrowed? (:type p)) (flv a) a)) (:params r) args)
                   pinned (keep (fn [[p a]] (when (ast/borrowed? (:type p)) a)) (map vector (:params r) cargs))
                   _ (run! lock pinned)
                   res (try (apply entry cargs) (finally (run! unlock pinned)))
                   code (status)]
               (when-not (zero? code)
                 (rt/raise! code (err-l 0) (err-l 1) (err-d)))
               (cond
                 (= [:array :long] (:type r))
                 (scm/call "jolt-native-wrap-longs" (err-l 2) res copy-out elem free)
                 (ast/owned? (:type r))
                 (let [n (err-l 2) arr (double-array n)]
                   (copy-out (flv arr) res n)
                   arr)
                 (#{:unit :never} (:type r)) nil
                 :else res)))}))

(defn- cc-compile
  "SRC built by cc into its own shared object, loaded process-wide (like a
  statically linked region; ffi/load-library scopes a library to its defcfns)."
  [sym src]
  (let [c (str work-dir "/" sym ".c")
        so (str work-dir "/" sym ".dylib")
        _ (spit c src)
        cmd ["cc" "-O2" "-ffp-contract=off" "-shared" "-fPIC" "-Wall" "-Werror" "-DJN_EXTERN_STATUS"
             "-Wno-unused-variable" "-Wno-unused-but-set-variable" "-Wno-unused-function" "-o" so c rt-so ext-so
             ;; bionic links no libm into a shared object unasked (atan2 went unresolved)
             "-lm"]
        p (.start (ProcessBuilder. ^java.util.List cmd))
        err (slurp (.getErrorStream p))]
    (when-not (zero? (.waitFor p))
      (throw (ex-info (str "cc failed for " sym ":\n" err) {:src src})))
    (scm/call "load-shared-object" so)
    (fn [name args ret collect-safe?]
      (scm/eval-string (str "(foreign-procedure " (when collect-safe? "__collect_safe ")
                            "\"" name "\" " args " " ret ")")))))

(defn- jit-compile
  "SRC compiled by the JIT, as a region's call site compiles it outside a build."
  [sym src]
  (let [m (scm/call "jolt-mir-compile-source" src)]
    (when (string? m)
      (throw (ex-info (str "the JIT failed for " sym ": " m) {:src src})))
    (fn [name args ret collect-safe?]
      (let [addr (scm/call "jolt-mir-lookup" m name)]
        (when-not addr (throw (ex-info (str "the JIT defined no " name) {})))
        (scm/eval-string (str "(foreign-procedure " (when collect-safe? "__collect_safe ")
                              addr " " args " " ret ")"))))))

(defn compile-region
  "params + body -> {:call f :live f :bad f :peak f :jit {...the same, JIT'd}
  :region r :c src}. With cc-only, no :jit."
  [params body & {:keys [cc-only]}]
  (let [r (mem/check-region (ast/analyze-region params body))
        sym (str "jn_t" (swap! counter inc))
        src (str (tracker sym) (emit/emit-region r sym))
        cc (caller r sym (cc-compile sym src))
        jit (when-not cc-only (caller r sym (jit-compile sym src)))]
    (assoc cc :jit jit :region r :c src)))

;; Android/Termux, by ffi.ss's rule (as jolt-locale-probe-test.clj has it):
;; c2mir cannot read bionic's system headers
(def android? (boolean (or (System/getenv "TERMUX_VERSION") (System/getenv "ANDROID_ROOT"))))

;; the backend runs each argument through the host check for its kind before
;; the call; the harness does the same
(def arg-checks {[:ref :double] "jolt-native-arg-doubles" [:ref :long] "jolt-native-arg-longs"
                 [:ref :byte] "jolt-native-arg-bytes"
                 :long "jolt-native-arg-long" :double "jolt-native-arg-double" :bool "jolt-native-arg-bool"})
;; An array's check answers the backing it tested, which the call site hands C;
;; :call takes the array itself, so the harness keeps it.
(defn checked [r f]
  (fn [& args]
    (apply f (map (fn [p a] (let [v (scm/call (arg-checks (:type p)) a)] (if (ast/borrowed? (:type p)) a v)))
                  (:params r) args))))

(defn outcome [f args]
  (try {:value (f args)}
       (catch Throwable e {:throw (.getName (class e)) :msg (.getMessage e)})))

(defn same-double? [a b]
  (or (and (Double/isNaN a) (Double/isNaN b))
      (and (== a b) (= (Math/copySign 1.0 a) (Math/copySign 1.0 b)))))

(def longs-class (class (long-array 0)))
(defn same? [a b]
  (cond
    (and (double? a) (double? b)) (same-double? a b)
    (and (instance? longs-class a) (instance? longs-class b)) (= (vec a) (vec b))
    (and (instance? (class (double-array 0)) a) (instance? (class (double-array 0)) b))
    (and (= (alength a) (alength b)) (every? true? (map same-double? a b)))
    :else (= a b)))

(defn same-outcome? [a b]
  (if (contains? a :value)
    (and (contains? b :value) (same? (:value a) (:value b)))
    (= a b)))

(defn copy-args [args]
  (mapv #(if (or (instance? (class (double-array 0)) %) (instance? longs-class %)) (aclone %) %) args))

;; --- the golden oracle ---------------------------------------------------------
;; [label occurrence arg-index] -> {:out outcome :inputs [args after]}, each value
;; in a form = can compare exactly: NaN, the infinities and -0.0 as tags, arrays
;; as tagged vectors.
(def golden-path "test/native/golden.edn")
(def golden-write? (= "write" (System/getenv "NATIVE_GOLDEN")))
(def golden (atom (edn/read-string (slurp golden-path))))
(def golden-occ (atom {}))
(def golden-checked (atom 0))
;; every key a row produced this run: a golden entry no row produces (a renamed,
;; reordered or deleted row) fails the run rather than going unchecked
(def golden-seen (atom #{}))

(defn gnorm [v]
  (cond
    (double? v) (cond (Double/isNaN v) [:nan]
                      (Double/isInfinite v) [:inf (if (pos? v) 1 -1)]
                      (and (zero? v) (neg? (Math/copySign 1.0 v))) [:neg-zero]
                      :else v)
    (instance? (class (double-array 0)) v) {:doubles (mapv gnorm v)}
    (instance? longs-class v) {:longs (vec v)}
    (map? v) (into {} (map (fn [[k x]] [k (gnorm x)]) v))
    :else v))

(defn- golden=
  "WANT and GOT the same, a double allowed one ulp: golden.edn was recorded on
  glibc, and another libm (bionic, macOS) rounds sin, exp and the rest a last
  bit differently. cc and the JIT, on one machine, are compared exactly."
  [want got]
  (cond
    (and (double? want) (double? got)) (or (= want got) (<= (Math/abs (- want got)) (Math/ulp want)))
    (and (map? want) (map? got)) (and (= (set (keys want)) (set (keys got)))
                                      (every? #(golden= (get want %) (get got %)) (keys want)))
    (and (sequential? want) (sequential? got)) (and (= (count want) (count got))
                                                    (every? true? (map golden= want got)))
    :else (= want got)))

(defn- check-golden [label k c ca]
  (let [got {:out (gnorm c) :inputs (mapv gnorm ca)}
        want (get @golden k)]
    (swap! golden-seen conj k)
    (cond
      want (do (swap! golden-checked inc)
               (when-not (golden= want got)
                 (fail! (str label " " (pr-str (peek k)) ": want " (pr-str want) " (golden.edn) got " (pr-str got)))))
      golden-write? (swap! golden assoc k got))))

(defn check-run
  "Run one program as cc's C and as the JIT's on each arg vector; compare them
  with each other, with the body as plain Clojure (:raw) and with golden.edn."
  [label params body arg-vecs & {:keys [raw cc-only]}]
  (let [compiled (try (compile-region params body :cc-only cc-only)
                      (catch Throwable e (fail! (str label ": compile failed: " (.getMessage e))) nil))]
    (when compiled
      (let [r (:region compiled)
            jit (:jit compiled)
            raw-f (when raw (eval (list* 'fn (vec params) body)))
            occ (get (swap! golden-occ update label (fnil inc -1)) label)]
        (doseq [[gi args] (map-indexed vector arg-vecs)]
          (let [ca (copy-args args) ja (copy-args args) ra (copy-args args)
                c (outcome #(apply (checked r (:call compiled)) %) ca)]
            (when jit
              (let [j (outcome #(apply (checked r (:call jit)) %) ja)]
                (when-not (same-outcome? c j)
                  (fail! (str label " " (pr-str args) ": cc " (pr-str c) " vs JIT " (pr-str j))))
                (when-not (every? true? (map same? ca ja))
                  (fail! (str label " " (pr-str args) ": inputs after cc and JIT differ")))))
            (check-golden label [label occ gi] c ca)
            (when raw-f
              (let [rr (outcome #(apply raw-f %) ra)]
                (when-not (same-outcome? rr c)
                  (fail! (str label " " (pr-str args) ": region " (pr-str c)
                              " vs body as plain Clojure " (pr-str rr))))))
            (doseq [[how h] (cond-> [["cc" compiled]] jit (conj ["JIT" jit]))]
              (let [live ((:live h)) bad ((:bad h))]
                (when-not (zero? live) (fail! (str label " " (pr-str args) ": " how " leaked " live " block(s)")))
                (when-not (zero? bad) (fail! (str label " " (pr-str args) ": " how " bad free")))))))
        compiled))))

(defn check-reject [label params body msg-re]
  (let [r (try (mem/check-region (ast/analyze-region params body)) ::accepted
               (catch Throwable e e))]
    (cond
      (= r ::accepted) (fail! (str label ": expected a compile error, was accepted"))
      (not (:jolt.native/error (ex-data r))) (fail! (str label ": wrong error " (pr-str r)))
      (not (re-find msg-re (.getMessage ^Throwable r)))
      (fail! (str label ": message " (pr-str (.getMessage ^Throwable r)) " does not match " msg-re)))))

(defn da [& xs] (double-array xs))

;; --- accepted programs -------------------------------------------------------

(check-run "sum" '[^doubles xs]
           '((let [n (alength xs)]
               (loop [i 0 s 0.0] (if (< i n) (recur (inc i) (+ s (aget xs i))) s))))
           [[(da)] [(da 1.5)] [(da 1 2 3 4.25)]] :raw true)

;; 8192+ elements: make-jolt-array pins the backing flvector (na-pin-large!);
;; the flvector header C reads must be the same either way
(check-run "large pinned input" '[^doubles xs]
           '((let [n (alength xs)]
               (loop [i 0 s 0.0] (if (< i n) (recur (inc i) (+ s (aget xs i))) (+ s (double n))))))
           [[(double-array (range 8191))] [(double-array (range 8192))] [(double-array (range 100000))]]
           :raw true)

(check-run "fill and return owned" '[^long n]
           '((let [a (double-array n)] (dotimes [i n] (aset a i (* i 0.5))) a))
           [[0] [1] [7] [-1]] :raw true)

(check-run "aclone input, scale, return" '[^doubles xs ^double k]
           '((let [a (aclone xs)] (dotimes [i (alength a)] (aset a i (* k (aget a i)))) a))
           [[(da 1 2 3) 2.0] [(da) 3.0]] :raw true)

(check-run "move through let" '[]
           '((let [a (double-array 3) b a] (aset b 0 1.0) b))
           [[]] :raw true)

(check-run "if picks one owned array, frees the other" '[^boolean c]
           '((let [a (double-array 2) b (double-array 3)] (if c a b)))
           [[true] [false]] :raw true)

(check-run "branch-created temporaries" '[^boolean c ^long n]
           '((if c (let [t (double-array n)] (aset t 0 2.5) (aget t 0)) -1.0))
           [[true 3] [false 3] [true 0]] :raw true)

(check-run "double buffering through recur" '[^long k]
           '((loop [i 0 cur (double-array 4) nxt (double-array 4)]
               (if (< i k)
                 (do (dotimes [j 4] (aset nxt j (+ (aget cur j) (double (+ i j)))))
                     (recur (inc i) nxt cur))
                 cur)))
           [[0] [1] [5]] :raw true)

(let [c (check-run "temporary per iteration" '[^long k]
                   '((loop [i 0 s 0.0]
                       (if (< i k)
                         (let [t (double-array 8)] (aset t 0 1.0) (recur (inc i) (+ s (aget t 0))))
                         s)))
                   [[0] [1000]] :raw true)]
  (when c
    ((:peak c))
    ((:call c) 5000)
    (let [peak ((:peak c))]
      (when (> peak 1) (fail! (str "temporary per iteration: " peak " blocks live at once, want 1"))))))

(check-run "in-place mutation of a borrowed input" '[^doubles xs]
           '((dotimes [i (alength xs)] (aset xs i (* 2.0 (aget xs i)))))
           [[(da 1 2 3)] [(da)]] :raw true)

(check-run "outer value moved on the exit path only" '[^long k]
           '((let [a (double-array 2)]
               (loop [i 0] (if (< i k) (recur (inc i)) a))))
           [[0] [3]] :raw true)

(check-run "n-body step" '[^doubles b ^long steps]
           '((dotimes [_ steps]
               (dotimes [i 5]
                 (let [oi (* i 7)]
                   (loop [j (inc i)]
                     (when (< j 5)
                       (let [oj (* j 7)
                             dx (- (aget b oi) (aget b oj))
                             dy (- (aget b (+ oi 1)) (aget b (+ oj 1)))
                             dz (- (aget b (+ oi 2)) (aget b (+ oj 2)))
                             d2 (+ (* dx dx) (* dy dy) (* dz dz))
                             mag (/ 0.01 (* d2 (Math/sqrt d2)))]
                         (aset b (+ oi 3) (- (aget b (+ oi 3)) (* dx (aget b (+ oj 6)) mag)))
                         (aset b (+ oj 3) (+ (aget b (+ oj 3)) (* dx (aget b (+ oi 6)) mag)))
                         (recur (inc j)))))))
               (dotimes [i 5]
                 (let [oi (* i 7)] (aset b oi (+ (aget b oi) (* 0.01 (aget b (+ oi 3))))))))
             (aget b 0))
           [[(double-array (map #(+ 0.5 (* % 0.37)) (range 35))) 100]] :raw true)

;; --- range facts: a fast copy without the checks they prove can't fire -------
;; (jolt.native.range). The guard on the array parameters picks the copy; each
;; row runs once where the fast copy runs and once where the checked one must,
;; and the two halves must still agree on results and exceptions.

(defn- check-eq [label got want]
  (when-not (= got want) (fail! (str label ": want " (pr-str want) " got " (pr-str got)))))

(defnative sum3 [xs] (+ (aget xs 0) (aget xs 1) (aget xs 2)))

(def nbody-params '[^doubles b ^long steps])
(def nbody-body
  '((dotimes [_ steps]
      (dotimes [i 5]
        (let [oi (* i 7)]
          (loop [j (inc i)]
            (when (< j 5)
              (let [oj (* j 7)
                    dx (- (aget b oi) (aget b oj))
                    mag (/ 0.01 (* dx dx))]
                (aset b (+ oi 3) (- (aget b (+ oi 3)) (* dx (aget b (+ oj 6)) mag)))
                (recur (inc j))))))))
    (aget b 0)))

(defn- fast-copy
  "The fast copy of the entry's body: from its guard to the checked copy."
  [src]
  (let [entry (subs src (str/index-of src "double jn_"))
        g (str/index-of entry "if (x")]
    (when g (subs entry g (str/index-of entry "} else {" g)))))

(let [src (emit/emit-region (mem/check-region (ast/analyze-region nbody-params nbody-body)) "jn_x")
      fast (fast-copy src)]
  (check-eq "n-body: a guard picks a fast copy" (some? fast) true)
  (check-eq "n-body: the guard asks for 35 elements" (boolean (and fast (str/includes? fast ".len > 34LL"))) true)
  (check-eq "n-body: the fast copy keeps no overflow, cast or bounds check"
            (when fast (re-find #"overflow|INT32_MIN|jn_tls\.code = 1" fast)) nil))

(check-run "n-body: fast copy, and the checked copy on a short array" nbody-params nbody-body
           [[(double-array (map #(+ 0.5 (* % 0.37)) (range 35))) 50]
            [(double-array (map #(+ 0.5 (* % 0.37)) (range 34))) 3]
            [(double-array 0) 0]
            [(double-array 30) 1]])

(check-run "a loop guarded by alength needs no bounds check" '[^doubles xs]
           '((loop [i 0 s 0.0]
               (if (< i (alength xs)) (recur (inc i) (+ s (aget xs i))) s)))
           [[(da 1 2 3)] [(da)]])

(check-run "an index past the guarded one still raises" '[^doubles xs]
           '((loop [i 0 s 0.0]
               (if (< i (alength xs)) (recur (inc i) (+ s (aget xs (+ i 1)))) s)))
           [[(da 1 2 3)] [(da)] [(da 5)]])

(check-run "dotimes over a let-bound alength" '[^doubles xs]
           '((let [n (alength xs)] (dotimes [i n] (aset xs i (* 2.0 (aget xs i)))) (aget xs 0)))
           [[(da 1 2 3)] [(da 4)] [(da)]])

;; a loop variable bound to (alength a) is a's length only while every recur
;; passes that length on; one that recur grows must keep the bounds check
(check-run "a loop-carried alength that recur changes keeps its bounds check" '[^doubles xs]
           '((loop [i 0 n (alength xs) s 0.0]
               (if (< i n) (recur (inc i) (+ n 5) (+ s (aget xs i))) s)))
           [[(da 1)] [(da)] [(da 1 2 3)]])
(check-run "a loop-carried alength of a loop array that recur replaces" '[^doubles xs ^doubles ys]
           '((loop [b xs n (alength b) i 0 s 0.0]
               (if (< i n) (recur ys n (inc i) (+ s (aget b i))) s)))
           [[(da 1 2 3) (da 1)] [(da) (da)] [(da 7) (da)] [(da 1 2) (da 4 5)]])
(let [fast (fast-copy (emit/emit-region
                       (mem/check-region
                        (ast/analyze-region '[^doubles xs]
                                            '((loop [i 0 n (alength xs) s 0.0]
                                                (if (< i n) (recur (inc i) n (+ s (aget xs i))) s)))))
                       "jn_x"))]
  (check-eq "a loop-carried alength passed on unchanged gets a fast copy" (some? fast) true)
  (check-eq "a loop-carried alength passed on unchanged still proves the index"
            (when fast (re-find #"jn_tls\.code = 1" fast)) nil))

(check-run "a count up to Long/MAX_VALUE cannot overflow its inc" '[^long n ^long k]
           '((loop [i (- n k)] (if (< i n) (recur (inc i)) i)))
           [[9223372036854775807 3] [10 2] [-9223372036854775807 1]])

(check-run "an inc past the guard still overflows" '[^long n]
           '((loop [i (- n 2)] (if (<= i n) (recur (inc i)) i)))
           [[9223372036854775807] [5]])

(check-run "rem and mod by a positive constant index an owned array" '[^long k]
           '((let [a (long-array 7)]
               (dotimes [i 20] (aset a (mod (- i k) 7) i) (aset a (rem (* i k) 7) (- i)))
               (aget a 3)))
           [[3] [-9223372036854775807] [0] [9223372036854775807]])

(check-run "a negative rem index still raises" '[^long k]
           '((let [a (double-array 7)] (aset a (rem k 7) 1.5) (aget a 0)))
           [[3] [-3] [0]])

(check-run "a helper's array parameter gets its own guard" '[^doubles xs]
           '((sum3 xs))
           [[(da 1 2 3)] [(da 1 2)] [(da 1 2 3 4)]])

;; a user macro expands inside the region; its syntax-quoted output names
;; clojure.core/let and clojure.core/* which the region reads unqualified
(defmacro sq [x] `(let [v# ~x] (* v# v#)))
(defmacro sum-to [n] `(loop [i# 0 s# 0] (if (< i# ~n) (recur (inc i#) (+ s# i#)) s#)))
(check-run "user macros inside a region" '[^double a ^long n]
           '((+ (sq (+ a 1.0)) (double (sum-to n))))
           [[2.0 10] [-1.0 0]] :raw true)

;; --- long arrays: a borrowed one is an fxvector of fixnums ----------------------
(defn la [& xs] (long-array xs))
(def big59 576460752303423488)          ; 2^59, still a fixnum
(def past-fixnum 4611686018427387904)   ; 2^62: jolt widens a long-array to hold it

(check-run "sum a borrowed long-array" '[^longs ls]
           '((loop [i 0 s 0] (if (< i (alength ls)) (recur (inc i) (+ s (aget ls i))) s)))
           [[(la)] [(la 1 2 3)] [(la -5 big59 7)]] :raw true)
(check-run "store into a borrowed long-array" '[^longs ls]
           '((dotimes [i (alength ls)] (aset ls i (* 2 (aget ls i)))))
           [[(la 1 -2 3)] [(la big59)]])
(check-run "a store past the fixnum range raises" '[^longs ls ^long k]
           '((aset ls 0 (* (aget ls 0) k)))
           [[(la big59) 2] [(la big59) 1] [(la 3) 4]])
(check-run "a long-array already widened is refused" '[^longs ls] '((alength ls))
           [[(let [a (long-array 2)] (aset a 0 past-fixnum) a)]])
(check-run "return an owned long-array" '[^long n]
           '((let [a (long-array n)] (dotimes [i n] (aset a i (* i 3))) a))
           [[0] [4]] :raw true)
(check-run "an owned long-array result past the fixnum range widens" '[^long v]
           '((let [a (long-array 3)] (aset a 1 v) (aset a 2 7) a))
           [[5] [past-fixnum] [9223372036854775807]] :raw true)
(check-run "aclone a borrowed long-array" '[^longs ls]
           '((let [a (aclone ls)] (dotimes [i (alength a)] (aset a i (inc (aget a i)))) a))
           [[(la 1 2 3)] [(la)]] :raw true)

;; --- native helpers (defnative): one instance per call-site types ---------------
(defnative dot [xs ys]
  (loop [i 0 s 0.0] (if (< i (alength xs)) (recur (inc i) (+ s (* (aget xs i) (aget ys i)))) s)))
(defnative scaled [xs k]
  (let [a (aclone xs)] (dotimes [i (alength a)] (aset a i (* k (aget a i)))) a))
(defnative fill! [a v] (dotimes [i (alength a)] (aset a i v)))
(defnative pick-at [xs i] (aget xs i))
(defnative sum-pick [xs i j] (+ (pick-at xs i) (pick-at xs j)))
(defnative ^long fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(defnative fib-nohint [n] (if (< n 2) n (+ (fib-nohint (- n 1)) (fib-nohint (- n 2)))))
(defnative leak-borrowed [xs] xs)

(check-run "one helper instantiated for double and long arrays" '[^doubles a ^longs b]
           '((+ (dot a a) (double (sum-pick b 0 1)) (sum-pick a 0 0)))
           [[(da 1 2 3) (la 1 2 3)] [(da 1.5 2) (la 2 big59)]] :raw true)
(check-run "a helper's owned result returned by the region" '[^doubles xs ^double k]
           '((scaled xs k))
           [[(da 1 2 3) 2.0] [(da) 3.0]] :raw true)
(check-run "a region-owned array lent to a helper that fills it" '[^long n]
           '((let [a (double-array n)] (fill! a 2.5) (scaled a 2.0)))
           [[0] [3]] :raw true)
(check-run "a failure in a nested helper with arrays live on both sides" '[^doubles xs ^long i]
           '((let [a (aclone xs) b (scaled a 2.0)] (+ (sum-pick b 0 i) (aget a 0))))
           [[(da 1 2 3) 2] [(da 1 2 3) 3] [(da 1 2 3) -1]])
(check-run "a recursive helper with a return hint" '[^long n] '((fib n)) [[0] [1] [15]] :raw true)

(check-run "a recursive helper's result type is inferred" '[^long n] '((fib-nohint n)) [[0] [1] [15]] :raw true)
(defnative spins [n] (spins n))
(check-reject "a helper that never returns" '[^long n] '((spins n)) #"never returns")

;; A local shadows a builtin, helper or macro of its name as it does in
;; Clojure, where the call would invoke the local's value; a region has no
;; function values, so it refuses the call rather than run the builtin.
(check-reject "a local named like a builtin is not the builtin" '[^doubles xs]
              '((let [aget 1] (aget xs 0))) #"aget is a local")
(check-reject "a parameter named like a builtin is not the builtin" '[^long inc]
              '((inc 1)) #"inc is a local")
(check-reject "a local named like a macro is not the macro" '[^long when]
              '((when true 1)) #"when is a local")
(check-run "a special form stays one under a local of its name" '[^long n]
           '((let [if 2] (if (pos? n) if 0)))
           [[1] [-1]] :raw true)

;; The region registry is keyed by a hash of the C: a second region that
;; arrives with the name and different C raises instead of running the first.
(check-eq "a region name registered with other C raises"
          [(do (scm/call "jolt-native-register!" "jn_collide_t" "int jn_a;")
               (scm/call "jolt-native-register!" "jn_collide_t" "int jn_a;")
               :same-ok)
           (:throw (outcome (fn [_] (scm/call "jolt-native-register!" "jn_collide_t" "int jn_b;")) nil))]
          [:same-ok "java.lang.IllegalStateException"])
(defnative ^double halve-to [x lim] (if (< x lim) x (halve-to (/ x 2.0) lim)))
(declare odd-steps)
(defnative even-steps [n] (if (zero? n) 0 (odd-steps (dec n))))
(defnative odd-steps [n] (if (zero? n) 1 (even-steps (dec n))))
(check-run "a hinted recursive helper still checks" '[^double x] '((halve-to x 1.0)) [[10.0] [0.5]] :raw true)
(check-run "mutual recursion between helpers is inferred" '[^long n] '((+ (even-steps n) (odd-steps n))) [[0] [7]] :raw true)

;; recursion deeper than the thread's C stack raises StackOverflowError, as the
;; JVM does, instead of crashing the process; what was live is still freed
(defnative ^long down [n] (if (zero? n) 0 (+ 1 (down (dec n)))))
(defnative ^doubles eat [a n] (if (<= n 0) a (let [r (eat a (dec n))] (aset r 0 1.0) r)))
(let [deep-c (compile-region '[^long n] '((down n)))
      deep-arr-c (compile-region '[^long n] '((alength (eat (double-array 3) n))))
      soe {:throw "java.lang.StackOverflowError" :msg nil}]
  ;; cc's C, then the JIT's
  (doseq [[how deep deep-arr] [["" deep-c deep-arr-c] [" (JIT)" (:jit deep-c) (:jit deep-arr-c)]]]
    (check-eq (str "shallow helper recursion" how) (outcome #(apply (:call deep) %) [1000]) {:value 1000})
    (check-eq (str "deep helper recursion raises StackOverflowError" how)
              (outcome #(apply (:call deep) %) [100000000]) soe)
    (check-eq (str "deep helper recursion in a future raises StackOverflowError" how)
              @(future (outcome #(apply (:call deep) %) [100000000])) soe)
    (check-eq (str "the region still runs after a stack overflow" how) (outcome #(apply (:call deep) %) [10]) {:value 10})
    (check-eq (str "deep recursion holding a taken array raises StackOverflowError" how)
              (outcome #(apply (:call deep-arr) %) [100000000]) soe)
    (check-eq (str "the taken array is freed on the overflow" how) ((:live deep-arr)) 0)
    (check-eq (str "no bad free on the overflow" how) ((:bad deep-arr)) 0))
  (check-eq "a region without recursion has no stack guard"
            (str/includes? (:c (compile-region '[^long n] '((sum-pick (long-array 3) 0 n)))) "JN_FRAME() <") false)
  (check-eq "a recursive helper checks the stack" (str/includes? (:c deep-c) "JN_FRAME() <") true))

;; (if c x nil) is a nil-typed if, so the region answers nil in both halves
(check-run "a when beside nil answers nil" '[^long x] '((when (> x 0) (* x 2))) [[3] [-1]])
(check-run "an if beside nil in a let answers nil" '[^long x] '((let [y 1] (if (> x 0) (+ x y) nil))) [[3] [-1]])
(defnative maybe-five [c] (when c 5))
(check-run "a helper's when beside nil answers nil" '[^boolean c] '((maybe-five c)) [[true] [false]])

(check-reject "a helper returning an array the region borrowed" '[^doubles xs] '((alength (leak-borrowed xs)))
              #"only borrowed from its caller.*pass \(aclone x\)")
(check-reject "moving the array a helper borrows in a sibling argument" '[]
              '((let [a (double-array 3)] (pick-at a (let [b a] 0)))) #"after it was given away")
;; the helper may free what it takes before it reads what it borrows, so one
;; array in both a borrowed and a taken position is refused in either order
(defnative borrow-then-take [a b] (let [r (if (> (alength a) 3) b (double-array 1))] (aget a 0) r))
(check-reject "an array both borrowed and taken by one helper call" '[^long n]
              '((let [x (double-array n)] (borrow-then-take x x))) #"after it was given away")

;; an owned array result's C buffer is freed even when the stub cannot make the
;; array to copy it into (an invalid length stands in: a Chez heap that runs out
;; aborts the process): the error propagates and the buffer reaches the region's _free
(doseq [[wrap extra] [["jolt-native-wrap-doubles" ""]
                      ["jolt-native-wrap-longs" " (lambda (p i) 0)"]]]
  (let [freed (scm/eval-string
               (str "(let ((freed -1)) (guard (e (#t freed)) (" wrap " -1 7"
                    " (lambda (v p n) (error 'copy-out \"must not run\"))" extra
                    " (lambda (p) (set! freed p))) -2))"))]
    (when-not (= 7 freed)
      (fail! (str wrap ": a failed result allocation freed " freed ", want the buffer 7")))))

;; --- review follow-ups ----------------------------------------------------------
;; an owned array in a branch of a unit if (a when) is freed in that branch
(check-run "an array in a when inside a loop is freed each iteration" '[^long n ^boolean c]
           '((loop [i 0] (if (< i n) (do (when c (double-array 100)) (recur (inc i))) i)))
           [[3 true] [3 false] [0 true]])
(check-run "an owned array as one branch of a unit if" '[^boolean c]
           '((let [a (double-array 3)] (if c a nil) 1))
           [[true] [false]])
;; a negated negative literal is -(x) in C, not a decrement
(check-run "negating negative literals" '[^long k]
           '((+ (- -5) k (long (- -1.5))))
           [[1]])
;; the JVM's long quot wraps Long/MIN_VALUE / -1; / by zero is the JVM's message
(check-run "quot of Long/MIN_VALUE by -1 wraps" '[^long a ^long b]
           '((quot a b))
           [[Long/MIN_VALUE -1] [7 -1] [7 2] [7 0]])
(check-reject "dotimes over a double count" '[^double x]
              '((dotimes [i x] nil) 0) #"dotimes takes a long count")

;; --- failures: same exception as Jolt, nothing leaked -------------------------

;; the index cast runs before aset's value (as on the JVM); its failure frees what
;; is live THEN, not what the value expression would have allocated or consumed
(check-run "an index overflow before aset's value, inside a loop with temporaries" '[^doubles xs ^long big]
           '((loop [i 0 s 0.0]
               (if (< i 3)
                 (recur (inc i) (+ s (aset xs (if (= i 2) big 0) (aget (double-array 2) 0))))
                 s)))
           [[(da 1 2 3) 4294967296] [(da 1 2 3) 1]])
(check-run "an index overflow before a value that moves an owned array" '[^doubles xs ^long big]
           '((let [a (double-array 2)] (aset xs big (let [b a] (aget b 0)))))
           [[(da 1 2 3) 4294967296] [(da 1 2 3) 1]])

;; --- inferred ownership: a helper takes the arrays it gives away ----------------
(defnative keep-it [a] a)
(defnative maybe-keep [a c] (if c a (double-array 1)))
(defnative pass-on [a] (keep-it a))
(defnative ^doubles drain [a n] (if (<= n 0) a (drain a (dec n))))

(check-run "a helper that returns its argument takes it" '[]
           '((let [a (double-array 3)] (aset a 0 1.5) (keep-it a)))
           [[]] :raw true)
(check-run "a helper that takes on one path frees it on the other" '[^boolean c]
           '((let [a (double-array 2)] (alength (maybe-keep a c))))
           [[true] [false]] :raw true)
(check-run "taken through another helper" '[] '((let [a (double-array 2)] (aset a 1 3.0) (pass-on a))) [[]] :raw true)
(check-run "taken through recursion" '[^long n] '((drain (double-array 3) n)) [[0] [5]] :raw true)
(check-reject "using an array after a helper took it" '[]
              '((let [a (double-array 3) b (keep-it a)] (aget a 0)))
              #"after it was given away \(moved\); native-test/keep-it took ownership of its 1st argument \(it returns it\)")
(check-reject "the explanation follows a transitive take" '[]
              '((let [a (double-array 3) b (pass-on a)] (alength a)))
              #"native-test/pass-on took ownership of its 1st argument \(it passes it to native-test/keep-it, which takes it\)")
(check-reject "a taking helper in a loop iteration that continues" '[]
              '((let [a (double-array 1)] (loop [i 0] (if (< i 3) (let [b (keep-it a)] (recur (inc i))) 0))))
              #"loop iteration that continues")

;; --- failures: same exception as Jolt, nothing leaked -------------------------


(check-run "out of bounds with live arrays" '[^long i]
           '((let [a (double-array 4) b (double-array 2)] (aset a 0 1.0) (+ (aget a i) (aget b 0))))
           [[0] [4] [-1] [4294967296]] :raw true)

(check-run "out of bounds inside a loop with a temporary" '[^long k]
           '((loop [i 0 s 0.0]
               (if (< i 10)
                 (let [t (double-array 3)] (recur (inc i) (+ s (aget t (if (= i k) 3 0)))))
                 s)))
           [[-1] [0] [9]])

(check-run "long overflow" '[^long a ^long b]
           '((+ a b))
           [[1 2] [9223372036854775807 1] [-9223372036854775808 -1]])

(check-run "left fold contagion" '[^long a]
           '((+ a 1 1.5))
           [[1] [9223372036854775807]])

(check-run "double first, no overflow" '[^long a]
           '((+ 1.5 a 1))
           [[9223372036854775807]] :raw true)

(check-run "quot rem mod" '[^long a ^long b]
           '((let [q (quot a b) r (rem a b) m (mod a b)] (+ (* q 1000000) (* r 1000) m)))
           [[7 2] [-7 2] [7 -2] [-7 -2] [5 0] [-9223372036854775808 -1]])

(check-run "array size is Number.intValue" '[^long n] '((alength (double-array n)))
           [[3] [0] [-2] [4294967296] [4294967297] [2147483648] [9223372036854775807] [-4294967295]]
           :raw true)

(check-run "long of double" '[^double d]
           '((long d))
           [[1.5] [-3.7] [9.223372036854775808E18] [-9.223372036854775808E18]
            [1e30] [-1e30] [##NaN] [##Inf] [##-Inf]] :raw true)

(check-run "error after allocations in both branches" '[^boolean c ^long i]
           '((let [a (double-array 2)]
               (if c (let [b (double-array 3)] (aget b i)) (aget a i))))
           [[true 1] [true 3] [false 2]])

(check-run "comparisons and math" '[^long a ^double b]
           '((if (and (< a b) (== a 2) (not= a 3))
               (+ (Math/sqrt b) (Math/pow b 2.0) (Math/floor b))
               (Math/atan2 b (double a))))
           [[2 2.5] [3 2.5] [2 -0.0] [2 ##NaN]] :raw true)
;; Java's pow, not C's: a NaN exponent, or ±1 to an infinite power, is NaN
(when-let [c (check-run "Math/pow takes Java's special cases" '[^double a ^double b]
                        '((Math/pow a b))
                        [[1.0 ##NaN] [-1.0 ##Inf] [1.0 ##-Inf] [2.0 10.0] [##NaN 0.0]] :raw true)]
  (let [f (checked (:region c) (:call c))]
    (check-eq "C Math/pow 1.0 ##NaN" (Double/isNaN (f 1.0 ##NaN)) true)
    (check-eq "C Math/pow -1.0 ##Inf" (Double/isNaN (f -1.0 ##Inf)) true)))
;; quot and rem over longs are Java's / and %, "/ by zero"; mod is a Clojure fn
;; over Objects, "Divide by zero", as the same body outside a region says
(check-run "quot by zero is / by zero" '[^long a ^long b] '((quot a b)) [[7 0] [7 2]] :raw true)
(check-run "rem by zero is / by zero" '[^long a ^long b] '((rem a b)) [[7 0] [7 2]] :raw true)
(check-run "mod by zero is Divide by zero" '[^long a ^long b]
           '((mod a b))
           [[7 0] [-7 2]] :raw true)

;; --- rejected programs --------------------------------------------------------

(check-reject "use after move" '[]
              '((let [a (double-array 1) b a] (aget a 0))) #"after it was given away")
(check-reject "moved in one branch, used after the if" '[^boolean c]
              '((let [a (double-array 1)] (if c (let [b a] 0.0) 0.0) (aget a 0))) #"after it was given away")
(check-reject "outer value moved in a continuing iteration" '[]
              '((let [a (double-array 1)] (loop [i 0] (if (< i 3) (let [b a] (recur (inc i))) 0))))
              #"loop iteration that continues")
(check-reject "outer value passed to recur" '[]
              '((let [a (double-array 1)] (loop [i 0 x (double-array 1)] (if (< i 3) (recur (inc i) a) x))))
              #"loop iteration that continues")
(check-reject "returning a borrowed array" '[^doubles xs] '(xs) #"borrowed")
;; found by the random programs: the index moved the array aset was borrowing,
;; freeing it before the store (a use after free the allocator trapped on)
(check-reject "an argument moves the array the op borrows" '[]
              '((let [a (double-array 4)] (aset a (let [b a] 0) 1.0))) #"after it was given away")
(check-reject "non-boolean test" '[^long n] '((if n 1 2)) #"must be a boolean")
(check-reject "long division" '[^long a] '((/ a 2)) #"ratio")
(check-reject "= across number types" '[^long a] '((= a 1.0)) #"same scalar type")
(check-reject "unhinted parameter" '[a] '(a) #"needs a type hint")
(check-reject "recur type mismatch" '[]
              '((loop [s 0] (if (< s 3) (recur (+ s 0.5)) s))) #"initialize it with a double")
(check-reject "recur not in tail position" '[]
              '((loop [i 0] (do (recur (inc i)) i))) #"tail position")
(check-reject "unknown function" '[^long a] '((println a)) #"not allowed")

;; --- stack arrays: small constant-size arrays that stay in the function -----
;; (jolt.native.stack) live in a frame buffer: no calloc, no free. The tracker
;; would count a free of one as a bad free.

(defn- c-of [params body]
  (emit/emit-region (mem/check-region (ast/analyze-region params body)) "jn_x"))

(defn- stack-arrays [src] (count (re-seq #"_buf\[" src)))

(check-eq "a temporary per iteration is not heap-allocated"
          (boolean (re-find #"\.data = JN_CALLOC\("
                            (c-of '[^long n]
                                  '((loop [i 0 s 0.0]
                                      (if (< i n)
                                        (recur (inc i) (let [t (double-array 3)] (aset t 1 2.0) (+ s (aget t 1))))
                                        s))))))
          false)
(check-eq "the region's result stays on the heap"
          (stack-arrays (c-of '[] '((let [a (double-array 3)] (aset a 0 1.0) a)))) 0)
(check-eq "an array carried by recur stays on the heap"
          (stack-arrays (c-of '[^long n]
                              '((loop [a (double-array 2) i 0]
                                  (if (< i n) (recur (let [b (double-array 2)] (aset b 0 (aget a 0)) b) (inc i))
                                      (aget a 0))))))
          0)
(check-eq "an array a helper takes stays on the heap"
          (stack-arrays (c-of '[] '((let [a (double-array 3)] (alength (keep-it a)))))) 0)
(check-eq "an array a helper only borrows goes on the stack"
          (stack-arrays (c-of '[] '((let [a (double-array 3)] (aset a 0 2.0) (dot a a))))) 1)
(check-eq "a name that may hold a heap array keeps both on the heap"
          (stack-arrays (c-of '[^doubles xs ^boolean c]
                              '((let [a (if c (double-array 3) (aclone xs))] (aget a 0)))))
          0)
(check-eq "a size past the cap stays on the heap"
          (stack-arrays (c-of '[] '((let [a (double-array 300)] (aget a 0))))) 0)

(check-run "a stack temporary is zeroed each time it is made" '[^long n]
           '((loop [i 0 s 0.0]
               (if (< i n)
                 (recur (inc i) (let [t (double-array 2)] (aset t 0 (+ (aget t 0) 1.0)) (+ s (aget t 0))))
                 s)))
           [[0] [3] [10]])

(check-run "an error with stack arrays live frees only the heap ones" '[^doubles xs ^long k]
           '((let [t (double-array 4) h (aclone xs)]
               (aset t 1 (aget h 0))
               (aset t k 1.0)
               (+ (aget t 1) (aget h 0))))
           [[(da 1 2) 2] [(da 1 2) 9] [(da) 0]])

(check-run "stack long-arrays and a borrowing helper" '[^long n]
           '((let [a (long-array 5)]
               (dotimes [i 5] (aset a i (* i n)))
               (let [b (long-array 5)] (dotimes [i 5] (aset b i 1)) (dot a b))))
           [[3] [0] [-4]])

;; --- scalar replacement: a stack array used only at constant indexes ---------

(defn- scalar-locals [src] (count (re-seq #"_e[0-9]+ = 0;" src)))

(check-eq "a constant-index temporary becomes locals"
          (let [src (c-of '[^long n]
                          '((loop [i 0 s 0.0]
                              (if (< i n)
                                (recur (inc i) (let [t (double-array 3)]
                                                 (aset t 0 (double i)) (aset t 2 1.0)
                                                 (+ s (aget t 0) (aget t 2) (double (alength t)))))
                                s))))]
            [(pos? (scalar-locals src)) (stack-arrays src)])
          [true 0])
(check-eq "a variable index keeps the array in memory"
          (stack-arrays (c-of '[^long k] '((let [t (double-array 3)] (aset t 0 1.0) (aget t (rem k 3)))))) 1)
(check-eq "a helper call keeps the array in memory"
          (stack-arrays (c-of '[] '((let [t (double-array 3)] (aset t 0 2.0) (dot t t))))) 1)
(check-eq "a constant index past the end keeps the array in memory"
          (stack-arrays (c-of '[^boolean c] '((let [t (double-array 2)] (if c (aget t 5) (aget t 1)))))) 1)

;; an owned helper parameter has no site of its own: a name that can be it or a
;; stack array stays on the heap, scalarized neither, and is freed as a whole
(defnative pick-own [c xs] (let [x (if c (double-array 2) xs)] (aget x 0)))
(defnative pick-own-at [c xs i] (let [x (if c (double-array 2) xs)] (aget x i)))
(check-run "a stack site and an owned parameter in one name" '[^long n]
           '((let [a (double-array n)] (aset a 0 5.0) (pick-own (< n 0) a)))
           [[3] [1] [0]] :raw true)
(check-run "the same at a variable index" '[^long n ^long i]
           '((let [a (double-array n)] (aset a 0 5.0) (pick-own-at (< n 0) a i)))
           [[3 0] [3 2] [3 5] [1 0]] :raw true)
;; each frame of a recursive helper would push its stack arrays again
(defnative deep-buf [n] (let [b (double-array 256) j (rem n 256)] (aset b j 1.0) (if (<= n 0) (aget b j) (+ (aget b j) (deep-buf (dec n))))))
(check-eq "a recursive helper keeps its arrays on the heap"
          (stack-arrays (c-of '[^long n] '((deep-buf n)))) 0)
(check-run "a recursive helper with an array per frame" '[^long n] '((deep-buf n)) [[0] [50]] :raw true)
(check-eq "a frame keeps at most 8KB of stack arrays"
          (stack-arrays (c-of '[] '((let [a (double-array 256) b (double-array 256) c (double-array 256)
                                          d (double-array 256) e (double-array 256)]
                                      (+ (dot a a) (dot b b) (dot c c) (dot d d) (dot e e))))))
          4)

(check-run "locals are zeroed each time the array is made" '[^long n]
           '((loop [i 0 s 0.0]
               (if (< i n)
                 (recur (inc i) (let [t (double-array 2)] (aset t 0 (+ (aget t 0) 1.0)) (+ s (aget t 0) (aget t 1))))
                 s)))
           [[0] [4]])
(check-run "a past-the-end constant still raises" '[^boolean c]
           '((let [t (double-array 2)] (if c (aget t 5) (aget t 1))))
           [[true] [false]])
(check-run "scalar long arrays through a let alias" '[^long n]
           '((let [a (long-array 2) b a] (aset b 1 n) (aset b 0 (* 2 (aget b 1))) (+ (aget b 0) (aget b 1))))
           [[3] [-4611686018427387904]])

;; --- defextern: C functions a region calls directly ---------------------------
;; A region's C calls the function; outside a region, name calls it through
;; jolt.ffi.

(defextern ^long ext-mix [^long a ^double b] {:c "jn_ext_mix"})
(defextern ^boolean ext-odd [^long a] {:c "jn_ext_odd"})
(defextern ^double ext-half [^double x] {:c "jn_ext_half"})
(defextern ext-tick [] {:c "jn_ext_tick"})
(defextern ^long ext-ticks [] {:c "jn_ext_ticks"})
;; no :c: the symbol's name with - (and anything else C refuses) as _
(defextern ^double jn-ext-half [^double x])

(check-run "a region calls declared C functions" '[^long n ^double d]
           '((loop [i 0 s 0]
               (if (< i n)
                 (recur (inc i) (if (ext-odd i) (+ s (ext-mix i d)) s))
                 (+ (double s) (ext-half d)))))
           [[0 1.5] [7 0.25] [20 -2.0]])

(check-run "a long passed where C takes a double" '[^long n]
           '((ext-half n))
           [[9] [-3]])
(check-run "a C name made from the symbol's" '[^double d] '((jn-ext-half d)) [[3.0] [-1.0]])

(let [before (ext-ticks)
      f (:call (compile-region '[^long n] '((dotimes [_ n] (ext-tick)) (ext-ticks))))]
  (check-eq "a void C function runs for its effect" (- (f 4) before) 4))

(defnative scaled-mix [a] (ext-mix a 0.5))
(check-run "a helper calls a C function" '[^long n] '((scaled-mix n)) [[2] [-5]])

(check-reject "a C function's argument types are checked" '[^double d] '((ext-odd d))
              #"takes :long here, got :double")
(check-reject "a C function's arity is checked" '[] '((ext-half))
              #"takes 1 argument")

(check-eq "a C function without a header is declared from its hints"
          (boolean (re-find #"double jn_ext_half\(double\);" (c-of '[^double x] '((ext-half x)))))
          true)

(defextern ^double ext-cbrt [^double x] {:c "cbrt" :header "<math.h>"})
(check-eq "a C function with a header is included, not declared"
          (let [src (c-of '[^double x] '((ext-cbrt x)))]
            [(boolean (re-find #"#include <math.h>\n" src)) (boolean (re-find #"double cbrt\(double\);" src))])
          [true false])
(check-run "a libm function through its header" '[^double x] '((ext-cbrt x)) [[27.0] [-8.0] [2.0]]
           :cc-only android?)

;; array arguments: a pointer to the elements and their count

(defextern ^double ext-sum [^doubles xs] {:c "jn_ext_sum"})
(defextern ext-scale [^doubles xs ^double k] {:c "jn_ext_scale"})
(defextern ext-fill [^longs xs ^long v] {:c "jn_ext_fill"})
(defextern ^long ext-lsum [^longs xs] {:c "jn_ext_lsum"})

(check-run "a C function reads a borrowed array" '[^doubles xs] '((ext-sum xs))
           [[(da 1 2 3.5)] [(da)]])
(check-run "a C function writes a borrowed array in place" '[^doubles xs ^double k]
           '((ext-scale xs k) (+ (aget xs 0) (ext-sum xs)))
           [[(da 1 2 3) 2.0] [(da 4) -0.5]])
(check-run "owned arrays cross as their storage" '[^long n]
           '((let [a (long-array n) d (double-array 3)]
               (ext-fill a 10)
               (aset d 1 1.5)
               (ext-scale d 4.0)
               (+ (double (ext-lsum a)) (aget d 1))))
           [[0] [5]])
(check-reject "a borrowed long-array must be copied for C" '[^longs xs] '((ext-lsum xs))
              #"pass \(aclone it\)")
(check-run "a copied long-array is fine" '[^longs xs] '((ext-lsum (aclone xs)))
           [[(long-array [1 2 3])] [(long-array [])]])
(check-eq "an array passed to C stays in memory"
          (stack-arrays (c-of '[] '((let [t (double-array 3)] (aset t 0 1.0) (ext-sum t))))) 1)
(check-eq "called outside a region, a C function's changes are written back"
          (let [a (da 1 2)] (ext-scale a 3.0) (vec a))
          [3.0 6.0])
;; outside a region each array is copied into foreign memory; one that cannot be
;; copied (not an array) must not leak the buffers already made for the others
(defextern ^double ext-dot2 [^doubles xs ^doubles ys] {:c "jn_ext_dot_avx2"})
(let [live (atom 0)
      alloc jolt.ffi/alloc
      free jolt.ffi/free]
  (with-redefs [jolt.ffi/alloc (fn [n] (swap! live inc) (alloc n))
                jolt.ffi/free (fn [p] (swap! live dec) (free p))]
    (check-eq "outside a region, a C call frees its buffers when an argument fails"
              [(:throw (outcome (fn [_] (ext-dot2 (da 1 2) nil)) nil)) @live]
              ["java.lang.NullPointerException" 0])
    (check-eq "outside a region, a C call frees its buffers on success"
              [(ext-dot2 (da 1 2) (da 3 4)) @live]
              [11.0 0])))

;; --- platform bodies: defnative's (on guard ...), chosen by the C compiler ------

(defnative which-body [] (on "0" 10) (on "1" 20) (on :default 30))
(let [f (:call (compile-region '[] '((which-body))))]
  (check-eq "C compiles the first body whose guard holds" (f) 20)
  (check-eq "outside a region, name runs the :default body" (which-body) 30))

(defnative no-body [] (on "0" 1) (on "defined(JN_NEVER_DEFINED)" 2))
(check-eq "a platform no body covers is a build error"
          (try (compile-region '[] '((no-body))) :compiled
               (catch Throwable e (boolean (re-find #"Not Supported on This Platform" (.getMessage e)))))
          true)

(defnative vsum [xs]
  (on "defined(__aarch64__) || defined(__x86_64__)"
      (loop [i 0 s 0] (if (< i (alength xs)) (recur (inc i) (+ s (aget xs i))) s)))
  (on :default
      (loop [i (dec (alength xs)) s 0] (if (>= i 0) (recur (dec i) (+ s (aget xs i))) s))))
(check-run "equivalent platform bodies agree" '[^longs xs]
           '((vsum xs))
           [[(long-array [1 2 3 4])] [(long-array [])] [(long-array [-5 9])]])

(defnative mixed-types [x] (on "1" x) (on :default (double x)))
(check-reject "platform bodies must agree on their result type" '[^long n] '((mixed-types n))
              #"platform bodies return different types")

(defnative mixed-takes [a] (on "1" (let [b a] (alength b))) (on :default (alength a)))
(check-reject "platform bodies must take the same arguments" '[]
              '((let [a (double-array 2)] (mixed-takes a)))
              #"platform bodies take different arguments")

(check-eq "a guard is one line of C"
          (try (eval '(jolt.native/defnative bad-guard [] (on "1\n#define X" 1))) :defined
               (catch Throwable e (boolean (re-find #"one-line C condition" (.getMessage e)))))
          true)

(defnative halve-on [x] (on "1" (ext-half x)) (on :default (* 0.5 x)))
(check-run "a C function used only in a platform body is declared" '[^double x]
           '((halve-on x)) [[3.0] [-1.5]])

;; --- ^bytes parameters and bit operations ---------------------------------------
;; A region reads a byte-array in place, each element a signed byte as a long;
;; the bit operations are the JVM's long ones, shift counts masked to 0..63.

(defn ba [& xs] (byte-array xs))

(check-run "sum the bytes of a ^bytes parameter, signed and unsigned" '[^bytes bs]
           '((loop [i 0 s 0 u 0]
               (if (< i (alength bs))
                 (recur (inc i) (+ s (aget bs i)) (+ u (bit-and (aget bs i) 255)))
                 (+ (* s 100000) u))))
           [[(ba)] [(ba 1 2 3)] [(ba -1 -128 127 0)]] :raw true)
(check-run "bit operations" '[^long a ^long b]
           '((bit-xor (bit-and a b) (bit-shift-left (bit-or a b) 1) (bit-shift-left (bit-xor a b) 2)
                      (bit-not a) (bit-and a b 7) (bit-or a b 1)))
           [[0 0] [-1 12345] [Long/MAX_VALUE Long/MIN_VALUE] [6 3]] :raw true)
(check-run "shifts mask their count as the JVM does" '[^long a ^long k]
           '((bit-xor (bit-shift-left a k) (bit-shift-right a k) (unsigned-bit-shift-right a k)))
           [[1 0] [1 63] [1 64] [1 65] [-1 1] [-8 2] [Long/MIN_VALUE 63] [12345 -1] [7 200]] :raw true)
(check-run "little-endian u32 from bytes" '[^bytes bs ^long off]
           '((bit-or (bit-and (aget bs off) 255)
                     (bit-shift-left (bit-and (aget bs (+ off 1)) 255) 8)
                     (bit-shift-left (bit-and (aget bs (+ off 2)) 255) 16)
                     (bit-shift-left (bit-and (aget bs (+ off 3)) 255) 24)))
           [[(ba 1 2 3 4 5) 0] [(ba 1 2 3 4 5) 1] [(ba -1 -1 -1 -1) 0] [(ba 1 2 3) 0] [(ba 1 2 3 4) -1]] :raw true)
(defnative byte-at [bs i] (aget bs i))
(check-run "a helper borrows a ^bytes parameter" '[^bytes bs]
           '((+ (byte-at bs 0) (byte-at bs (dec (alength bs)))))
           [[(ba 5 -7)] [(ba 9)] [(ba)]] :raw true)
(check-reject "a region cannot write a ^bytes parameter" '[^bytes bs] '((aset bs 0 1))
              #"cannot write it")
(check-reject "a region cannot clone a ^bytes parameter" '[^bytes bs] '((alength (aclone bs)))
              #"cannot clone a \^bytes")
(check-reject "a ^bytes parameter is not a C function's array" '[^bytes bs] '((ext-sum bs))
              #"takes \[:array :double\] here")
(check-eq "a ^bytes parameter must be a byte-array"
          (outcome #(apply (checked (:region (compile-region '[^bytes bs] '((alength bs))))
                                    (fn [_] :called)) %)
                   [(double-array 2)])
          {:throw "java.lang.ClassCastException" :msg "native region: a ^bytes parameter got [D"})
;; The call site hands C what the check answers: the backing it tested, read
;; once. Reading it again after the check would race a thread whose store widens
;; a long-array's fxvector to a vector, and C would read that vector as fixnums.
;; An owned array result is only a C pointer until its wrap copies and frees
;; it; the call site wraps it while interrupts are still off, so an
;; asynchronous throw (a fiber kill, an interrupt) cannot land in between.
(let [scm (jolt.backend-scheme/emit-top-form
           (jolt.analyzer/analyze (jolt.host/ctx-for-ns "native-test")
                                  '(fn [n] (jolt.native/native [^long n] (double-array n)))
                                  (@#'jolt.analyzer/empty-env)))
      wrap (str/index-of scm "jolt-native-wrap-doubles")
      on (str/last-index-of scm "(enable-interrupts)")]
  (check-eq "an owned result is taken before interrupts are back on"
            (boolean (and wrap on (< wrap on))) true))
(check-eq "an array argument's check answers the backing it tested"
          (mapv (fn [[check a pred]] (scm/call pred (scm/call check a)))
                [["jolt-native-arg-doubles" (double-array 2) "flvector?"]
                 ["jolt-native-arg-longs" (long-array 2) "fxvector?"]
                 ["jolt-native-arg-bytes" (byte-array 2) "bytevector?"]])
          [true true true])

;; a mask by a non-negative constant bounds an index: the fast copy drops the
;; bounds check once the guard has the array longer than the mask
(let [src (c-of '[^doubles xs ^long i] '((aget xs (bit-and i 3))))
      fast (fast-copy src)]
  (check-eq "a masked index is proven in bounds" (boolean (and fast (str/includes? fast ".len > 3LL"))) true))

;; an unsigned shift by 1 can still answer 2^63 - 1: its range must not drop the
;; bounds check or the overflow check after it
(check-run "an unsigned shift by 1 keeps the index checked" '[^longs xs ^long i]
           '((aget xs (unsigned-bit-shift-right i 1)))
           [[(long-array [1 2 3 4]) 6] [(long-array [1 2 3 4]) -1] [(long-array [1 2 3 4]) -2]] :raw true)
(defn ushr1-inc [^long i] (jolt.native/native [^long i] (inc (unsigned-bit-shift-right i 1))))
(check-eq "an unsigned shift by 1 keeps the overflow check (the JVM's inc throws)"
          [(outcome (fn [_] (ushr1-inc 6)) nil) (outcome (fn [_] (ushr1-inc -1)) nil)]
          [{:value 4} {:throw "java.lang.ArithmeticException" :msg "integer overflow"}])

;; --- native-fn: regions compiled at runtime from data ------------------------------
;; through eval, the backend and the JIT, as a call site in the REPL is

(def rt-sq (jolt.native/native-fn '[^long n] '((* n n))))
(check-eq "native-fn compiles a region from data" (rt-sq 7) 49)
(check-eq "native-fn's region overflows as a region does"
          (outcome (fn [_] (rt-sq 4000000000)) nil)
          {:throw "java.lang.ArithmeticException" :msg "integer overflow"})
(check-eq "native-fn throws the region's compile error"
          (try (jolt.native/native-fn '[^long n] '((aget n 0))) :compiled
               (catch Throwable e (boolean (re-find #"aget takes an array" (.getMessage e)))))
          true)
(check-eq "native-fn resolves helpers in the namespace it is given"
          ((jolt.native/native-fn '[^bytes bs] '((byte-at bs 1)) 'native-test) (ba 3 -4)) -4)
(check-eq "native-fn checks the region in the namespace it is given, not the caller's"
          (binding [*ns* (the-ns 'clojure.core)]
            ((jolt.native/native-fn '[^bytes bs] '((byte-at bs 0)) 'native-test) (ba 3 -4)))
          3)
(check-eq "native-fn runs as JIT'd code" (some #{:jit} (vals (jolt.native/linked-regions))) :jit)

;; --- jolt.native.layout: a record layout lowered into a parser and a scan ---------

(require '[jolt.native.layout :as layout])

(defn- ref-field
  "A field read in plain Clojure: the oracle for the layout's region."
  [^bytes buf base {:keys [type offset]} endian]
  (let [w ({:u8 1 :i8 1 :u16 2 :i16 2 :u32 4 :i32 4 :i64 8} type)
        bs (map #(bit-and (aget buf (+ base offset %)) 255) (range w))
        bs (if (= endian :big) (reverse bs) bs)
        u (reduce (fn [acc [k b]] (bit-or acc (bit-shift-left b (* 8 k)))) 0 (map-indexed vector bs))]
    (case type
      :i8 (aget buf (+ base offset))
      :i16 (if (>= u 0x8000) (- u 0x10000) u)
      :i32 (if (>= u 0x80000000) (- u 0x100000000) u)
      u)))

(defn- ref-fields [lay]
  (first (reduce (fn [[fs off] [n t]]
                   (if (= :pad n) [fs (+ off t)]
                       [(conj fs {:name n :type t :offset off})
                        (+ off ({:u8 1 :i8 1 :u16 2 :i16 2 :u32 4 :i32 4 :i64 8} t))]))
                 [[] 0] lay)))

(def lay [[:magic :u32] [:version :u8] [:pad 1] [:count :u16] [:size :i64] [:delta :i16]
          [:tag :i8] [:big :i32] [:small :u16]])
(def lay-size (layout/record-size lay))
(check-eq "a layout's size counts its padding" lay-size 25)
(def lay-buf (let [r (java.util.Random. 20261125)]
               (byte-array (map (fn [_] (- (.nextInt r 256) 128)) (range (+ (* 40 lay-size) 7))))))

(doseq [endian [:little :big]]
  (let [parse (layout/parser lay :endian endian)
        fs (ref-fields lay)]
    (doseq [base [0 lay-size (* 17 lay-size) 3]]
      (check-eq (str "a " (name endian) "-endian layout parses as plain Clojure reads it, at " base)
                (vec (parse lay-buf base))
                (mapv #(ref-field lay-buf base % endian) fs)))
    (doseq [f fs]
      (check-eq (str "a " (name endian) "-endian column scan of " (:name f))
                (vec ((layout/column lay (:name f) :endian endian) lay-buf))
                (mapv #(ref-field lay-buf (* % lay-size) f endian) (range 40))))))
(check-eq "a layout read past the end raises as aget does"
          (outcome (fn [_] ((layout/parser lay) lay-buf (- (alength lay-buf) 3))) nil)
          {:throw "java.lang.ArrayIndexOutOfBoundsException"
           :msg (str "Index " (alength lay-buf) " out of bounds for length " (alength lay-buf))})
(check-eq "a layout field names a width"
          (try (layout/parser [[:x :u24]]) :compiled
               (catch Throwable e (boolean (re-find #"one of :u8" (.getMessage e)))))
          true)

;; --- specialization: a variant per value of a ^{:specialize [...]} parameter -------
;; The variant has the value as a constant, so the range analysis can prove what
;; it could not for an unknown n; the answers must not change.

(def msum-params '[^long n ^doubles m])
(def msum-body '((loop [i 0 s 0.0]
                   (if (< i n)
                     (recur (inc i) (loop [j 0 s s] (if (< j n) (recur (inc j) (+ s (aget m (+ (* i n) j)))) s)))
                     s))))

(let [generic (c-of msum-params msum-body)
      [keep b] (jolt.native/specialized-body msum-params msum-body '[n] [4])
      fast (fast-copy (c-of keep b))]
  (check-eq "for an unknown n, even the fast copy checks i * n + j for overflow"
            (boolean (re-find #"__builtin_mul_overflow" (or (fast-copy generic) generic))) true)
  (check-eq "specialized on n = 4, a fast copy guards on 16 elements"
            (boolean (and fast (str/includes? fast ".len > 15LL"))) true)
  (check-eq "the specialized fast copy keeps no overflow, cast or bounds check"
            (when fast (re-find #"overflow|INT32_MIN|jn_tls\.code = 1" fast)) nil))

(defn spec-msum [^long n ^doubles m]
  (jolt.native/native ^{:specialize [n] :max-variants 3} [^long n ^doubles m]
    (loop [i 0 s 0.0]
      (if (< i n)
        (recur (inc i) (loop [j 0 s s] (if (< j n) (recur (inc j) (+ s (aget m (+ (* i n) j)))) s)))
        s))))
(defn plain-msum [^long n ^doubles m]
  (loop [i 0 s 0.0]
    (if (< i n)
      (recur (inc i) (loop [j 0 s s] (if (< j n) (recur (inc j) (+ s (aget m (+ (* i n) j)))) s)))
      s)))

(doseq [n [0 1 2 3 4 5 2 3 6]]
  (let [m (double-array (map #(* 0.5 %) (range (* n n))))]
    (check-eq (str "a specialized region answers as written, n = " n) (spec-msum n m) (plain-msum n m))))
(let [site (some (fn [[k v]] (when (some #{[2]} v) k)) (jolt.native/specializations))]
  (check-eq "a variant is compiled per value, up to :max-variants"
            (sort (map first (get (jolt.native/specializations) site))) [0 1 2]))
(check-eq "a value past :max-variants runs the region as written, and fails as it does"
          (outcome (fn [_] (spec-msum 3 (double-array 4))) nil)
          {:throw "java.lang.ArrayIndexOutOfBoundsException" :msg "Index 4 out of bounds for length 4"})
(check-eq "a variant fails as the region does"
          (outcome (fn [_] (spec-msum 2 (double-array 3))) nil)
          {:throw "java.lang.ArrayIndexOutOfBoundsException" :msg "Index 3 out of bounds for length 3"})

(defn spec-scale [^double k ^boolean neg ^doubles xs]
  (jolt.native/native ^{:specialize [k neg]} [^double k ^boolean neg ^doubles xs]
    (dotimes [i (alength xs)] (aset xs i (if neg (- (* k (aget xs i))) (* k (aget xs i)))))
    (aget xs 0)))
(check-eq "double and boolean parameters specialize" [(spec-scale 2.0 false (double-array [1 2]))
                                                     (spec-scale 2 true (double-array [1 2]))
                                                     (spec-scale 2.0 false (double-array [3]))]
          [2.0 -2.0 6.0])
(check-eq "a long argument to a specialized ^double is cast as the region casts it"
          (some #{[2.0 true]} (mapcat val (jolt.native/specializations))) [2.0 true])

;; The key is the cast value, not the argument: an unhinted caller passing 2 and
;; 2.0 gets one variant. A double is keyed by its bits, since 0.0 = -0.0 but a
;; variant with k = 0.0 would answer 0.0 where the region answers -0.0.
(defn spec-k [k ^doubles xs]
  (jolt.native/native ^{:specialize [k]} [^double k ^doubles xs] (* k (aget xs 0))))
(defn plain-k [k ^doubles xs] (* (double k) (aget xs 0)))
(check-eq "0.0 and -0.0 get their own variants, each answering as the region does"
          (mapv #(str (spec-k % (double-array [1.0]))) [0.0 -0.0 2 2.0])
          (mapv #(str (plain-k % (double-array [1.0]))) [0.0 -0.0 2 2.0]))
(check-eq "2 and 2.0 for a ^double share a variant"
          (some (fn [[_ v]] (when (some #{[-0.0]} v) (count v))) (jolt.native/specializations))
          3)
(check-eq "an argument the region's cast refuses raises the region's own error"
          (outcome (fn [_] (spec-k "x" (double-array [1.0]))) nil)
          (outcome (fn [_] (let [k "x"] (jolt.native/native [^double k] k))) nil))

;; Two sites with the same params and body compile to the same generic region,
;; but specialize different params: each needs its own variants, or one site
;; runs the other's constants.
(defn spec-on-x [x y] (jolt.native/native ^{:specialize [x]} [^long x ^long y] (- (* x 10) y)))
(defn spec-on-y [x y] (jolt.native/native ^{:specialize [y]} [^long x ^long y] (- (* x 10) y)))
(check-eq "same-body sites specializing different params keep their own variants"
          [(spec-on-x 5 1) (spec-on-y 1 5) (spec-on-y 2 5) (spec-on-x 2 5)]
          [49 5 15 15])

(doseq [[label params msg] [["an array" '^{:specialize [xs]} [^doubles xs] #"is an array"]
                            ["a non-parameter" '^{:specialize [q]} [^long n] #"not a parameter"]
                            ["not a vector" '^{:specialize n} [^long n] #"vector of parameter names"]]]
  (check-eq (str ":specialize refuses " label)
            (try (macroexpand (list 'jolt.native/native params 1)) :expanded
                 (catch Throwable e (boolean (re-find msg (.getMessage e)))))
            true))

;; --- the machine the JIT compiles for: (on ...) bodies chosen per CPU ------------
;; jolt build (cc here) compiles for whatever its compiler targets, the
;; baseline unless it was configured otherwise; the JIT defines the macro of
;; each feature this CPU has (jolt.native/cpu-features), so its guard picks
;; the body the running machine can run.

;; A region naming a C function this process does not have fails to link. The
;; JIT checks its imports before linking, so the failure leaves its context
;; (and the code of every region compiled in it) in use, not abandoned.
(def mir-contexts
  (try (scm/eval-string "(foreign-procedure \"jolt_mir_contexts\" () integer-64)")
       (catch Throwable _ (constantly :none))))
(let [before (mir-contexts)
      bad (scm/call "jolt-mir-compile-source"
                    "long jn_nope_zz(long); long jn_link_t(long x) { return jn_nope_zz(x); }")
      ok (scm/call "jolt-mir-compile-source" "long jn_link_ok(long x) { return x + 1; }")]
  (check-eq "a region that fails to link names the symbol and keeps the JIT's context"
            [(boolean (re-find #"undefined symbol jn_nope_zz" (str bad))) (string? ok) (and (pos-int? before) (= before (mir-contexts)))]
            [true false true]))

(def feats (jolt.native/cpu-features))
(defnative which-cpu [] (on "defined(__AVX2__)" 2) (on "defined(__SSE2__)" 1) (on :default 0))
(def cc-which ((:call (compile-region '[] '((which-cpu))))))
(let [c (compile-region '[] '((which-cpu)))
      sse2 (if (= 2 cc-which) 1 cc-which)]
  (check-eq "the JIT picks the body for this CPU's features"
            ((:call (:jit c))) (if (:avx2 feats) 2 sse2)))
(check-eq "cpu-features answers keywords" (every? keyword? feats) true)

(defextern ^double ext-dot-avx2 [^doubles xs ^doubles ys] {:c "jn_ext_dot_avx2"})
(defextern ^long ext-dot-calls [] {:c "jn_ext_dot_calls"})
(defnative cpu-dot [xs ys]
  (on "defined(__AVX2__)" (ext-dot-avx2 xs ys))
  (on :default (loop [i 0 s 0.0] (if (< i (alength xs)) (recur (inc i) (+ s (* (aget xs i) (aget ys i)))) s))))
;; whole numbers, so the SIMD kernel's other summation order rounds alike
(let [c (compile-region '[^doubles xs ^doubles ys] '((cpu-dot xs ys)))
      xs (double-array (range 1 12)) ys (double-array (range 12 1 -1))
      want (reduce + (map * (range 1 12) (range 12 1 -1)))
      before (ext-dot-calls)
      cc-r ((:call c) xs ys)
      mid (ext-dot-calls)
      jit-r ((:call (:jit c)) xs ys)
      after (ext-dot-calls)]
  (check-eq "cc's body, the one its compiler's macros pick"
            [cc-r (- mid before)] [(double want) (if (= 2 cc-which) 1 0)])
  (check-eq "the JIT runs the SIMD body where the CPU has AVX2, with the same answer"
            [jit-r (- after mid)] [(double want) (if (:avx2 feats) 1 0)]))

;; --- random programs ----------------------------------------------------------
;; A generator builds well-typed region programs and tracks ownership with its
;; own model of the rules (moves in let bindings, returns and recur; borrows in
;; aget/aset/alength/aclone; no outer value moved in a continuing loop
;; iteration). About one program in twelve gets a planted violation: a use of a
;; value the model knows is gone. The checker must accept every program the
;; model accepts (which then has to pass the cc-vs-JIT run and leave no
;; leak or bad free), and reject every planted one. Two implementations of the
;; ownership rules, checked against each other.
;; NATIVE_FUZZ_SEED and NATIVE_FUZZ_N reproduce or widen a run.

(def fuzz-seed (or (some-> (System/getenv "NATIVE_FUZZ_SEED") parse-long) 20261120))
(def fuzz-n (or (some-> (System/getenv "NATIVE_FUZZ_N") parse-long) 150))

(def ^:dynamic *rng* nil)
(defn rnd
  "A uniform int in [0, n), from a 31-bit LCG (reproducible on every host)."
  [n]
  (let [s (swap! *rng* (fn [s] (mod (+ (* s 1103515245) 12345) 2147483648)))]
    (mod (quot s 65536) n)))
(defn pick [xs] (nth xs (rnd (count xs))))
(defn chance [pct] (< (rnd 100) pct))

;; generator state: owned locals and whether each is still live; borrow-only is
;; the set of owned names a continuing loop iteration may not move
(def ^:dynamic *g* nil)
(def ^:dynamic *planted* nil)
;; plant mode: this program aims for a violation, so moves and reuses of dead
;; values are far likelier
(def ^:dynamic *plant-mode* false)
(defn plant-chance [normal boosted] (chance (if *plant-mode* boosted normal)))
(defn fresh-name [] (symbol (str "v" (:n (swap! *g* update :n inc)))))
(defn live-owned [] (vec (for [[k v] (:owned @*g*) :when (:alive v)] k)))
(defn movable-owned []
  (vec (remove (:borrow-only @*g*) (live-owned))))
(defn dead-owned [] (vec (for [[k v] (:owned @*g*) :when (not (:alive v))] k)))
(defn kill! [name] (swap! *g* assoc-in [:owned name :alive] false))
(defn scalars [t] (vec (for [[k v] (:scalars @*g*) :when (= v t)] k)))

(declare gen gen-let gen-loop)

(defn with-scope
  "Run f with names bound in it; owned names introduced inside go out of scope."
  [f]
  (let [before (set (keys (:owned @*g*)))
        sc (:scalars @*g*)
        r (f)]
    (swap! *g* (fn [g] (-> g
                           (update :owned #(select-keys % (concat before (filter before (keys %)))))
                           (assoc :scalars sc))))
    r))

(defn branch-merge
  "Generate then/else from the same state; a value moved in either is gone after."
  [gen-then gen-else]
  (let [s0 @*g*
        t (gen-then)
        s1 @*g*
        _ (reset! *g* s0)
        e (gen-else)
        s2 @*g*]
    (reset! *g* (assoc s2 :owned
                       (into {} (for [[k v] (:owned s0)]
                                  [k {:alive (and (get-in s1 [:owned k :alive])
                                                  (get-in s2 [:owned k :alive]))}]))
                       :n (max (:n s1) (:n s2))))
    [t e]))

(defn borrowable
  "An array expression to read from: the borrowed param, a live owned local,
  or a fresh temporary. A planted violation borrows a dead one."
  []
  (let [dead (dead-owned)]
    (if (and (seq dead) (plant-chance 8 45))
      (do (reset! *planted* true) (pick dead))
      (let [live (live-owned)]
        (cond
          (and (seq live) (chance 50)) (pick live)
          (chance 70) 'xs
          :else (list 'double-array (inc (rnd 4))))))))

(defn with-borrowed
  "Generate the sibling arguments of an op that borrows arr: arr must stay live
  until the op uses it, so it is borrow-only meanwhile (a planted program may
  still move it, which the checker has to refuse)."
  [arr f]
  (if (and (symbol? arr) (contains? (:owned @*g*) arr))
    (let [bo (:borrow-only @*g*)]
      (swap! *g* update :borrow-only (fnil conj #{}) arr)
      (let [r (f)] (swap! *g* assoc :borrow-only bo) r))
    (f)))

(defn gen-idx [d] (if (chance 75) (rnd 5) (gen :long (dec d))))

(defn gen-long [d]
  (let [ls (scalars :long)]
    (if (<= d 0)
      (pick (concat [(rnd 6) 'n] ls))
      (case (rnd 9)
        0 (list (pick '[+ - *]) (gen :long (dec d)) (gen :long (dec d)))
        1 (list (pick '[quot rem mod]) (gen :long (dec d)) (gen :long (dec d)))
        2 (if (chance 30) (list 'alength 'ls) (list 'alength (borrowable)))
        3 (if (chance 40) (list 'aget 'ls (gen-idx d)) (list 'long (gen :double (dec d))))
        4 (if (chance 30) 9223372036854775807 (rnd 100))
        5 (let [c (gen :bool (dec d))
                [t e] (branch-merge #(gen :long (dec d)) #(gen :long (dec d)))]
            (list 'if c t e))
        6 (gen-let :long d)
        (pick (concat ['n (rnd 6)] ls))))))

(defn gen-double [d]
  (let [ds (scalars :double)]
    (if (<= d 0)
      (pick (concat [(double (rnd 7)) 'dd 0.5] ds))
      (case (rnd 12)
        0 (list (pick '[+ - * /]) (gen :double (dec d)) (gen (pick [:double :long]) (dec d)))
        1 (let [a (borrowable)] (list 'aget a (with-borrowed a #(gen-idx d))))
        ;; a scan guarded by the array's length, read at an offset from the loop
        ;; variable: in bounds (and proven so) at 0, past either end otherwise
        10 (let [a (borrowable) i (fresh-name) acc (fresh-name) off (pick [0 0 1 -1])]
             (list 'loop [i 0 acc 0.0]
                   (list 'if (list '< i (list 'alength a))
                         (list 'recur (list 'inc i) (list '+ acc (list 'aget a (if (zero? off) i (list '+ i off)))))
                         acc)))
        2 (list 'Math/sqrt (gen :double (dec d)))
        3 (list 'double (gen :long (dec d)))
        4 (let [c (gen :bool (dec d))
                [t e] (branch-merge #(gen :double (dec d)) #(gen :double (dec d)))]
            (list 'if c t e))
        5 (gen-let :double d)
        6 (gen-loop :double d)
        8 (list 'do (list 'aset 'ls (gen-idx d) (gen :long (dec d))) (gen :double (dec d)))
        9 (let [a (borrowable)] (list 'pick-at a (with-borrowed a #(gen-idx d))))
        7 (let [a (borrowable)]
            (list 'do (cons 'aset (cons a (with-borrowed a #(vector (gen-idx d) (gen :double (dec d))))))
                  (gen :double (dec d))))
        (pick (concat ['dd 1.5] ds))))))

(defn gen-bool [d]
  (if (<= d 0)
    (pick [true false (list '< 'n 3)])
    (case (rnd 6)
      0 (list (pick '[< <= > >=]) (gen :long (dec d)) (gen :double (dec d)))
      1 (list '= (gen :long (dec d)) (gen :long (dec d)))
      2 (list 'not (gen :bool (dec d)))
      3 (list (pick '[and or]) (gen :bool (dec d)) (gen :bool (dec d)))
      4 (list 'zero? (gen :long (dec d)))
      (list '== (gen :double (dec d)) (gen :long (dec d))))))

(defn gen-array
  "An owned array value: new, a clone, a moved local, or built by a form."
  [d]
  (let [movable (movable-owned)
        dead (dead-owned)]
    (cond
      (and (seq dead) (plant-chance 6 45))
      (do (reset! *planted* true) (pick dead))
      (and (seq movable) (plant-chance 35 75))
      (let [v (pick movable)] (kill! v) v)
      ;; a helper that takes its argument: passing an owned local moves it, at
      ;; the call, so a later argument may still read it (but not move it)
      (and (seq movable) (chance 15))
      (let [v (pick movable)
            call (if (chance 50)
                   (list 'keep-it v)
                   (list 'maybe-keep v (with-borrowed v #(gen :bool (dec d)))))]
        (kill! v)
        call)
      ;; the other violation: moving an outer value in an iteration that continues
      (and *plant-mode* (seq (:borrow-only @*g*)) (chance 30)
           (some (set (live-owned)) (:borrow-only @*g*)))
      (do (reset! *planted* true) (pick (vec (filter (set (live-owned)) (:borrow-only @*g*)))))
      (<= d 0) (list 'double-array (rnd 5))
      :else
      (case (rnd 5)
        0 (list 'double-array (gen-idx d))
        1 (if (chance 40)
            (let [a (borrowable)] (list 'scaled a (with-borrowed a #(gen :double (dec d)))))
            (list 'aclone (borrowable)))
        2 (let [c (gen :bool (dec d))
                [t e] (branch-merge #(gen-array (dec d)) #(gen-array (dec d)))]
            (list 'if c t e))
        3 (gen-let [:array :double] d)
        (gen-loop [:array :double] d)))))

(defn gen-let [t d]
  (with-scope
    (fn []
      (let [k (inc (rnd 2))
            bs (vec (mapcat (fn [_]
                              (let [bt (pick [:long :double [:array :double]])
                                    init (gen bt (dec d))
                                    nm (fresh-name)]
                                (if (vector? bt)
                                  (swap! *g* assoc-in [:owned nm] {:alive true})
                                  (swap! *g* assoc-in [:scalars nm] bt))
                                [nm init]))
                            (range k)))
            body (gen t (dec d))]
        (list 'let bs body)))))

(defn gen-loop
  "(loop [i 0 acc init] (if (< i K) (recur (inc i) step) exit)). The step may
  not move an outer owned value; the exit may."
  [t d]
  (with-scope
    (fn []
      (let [i (fresh-name)
            acc (fresh-name)
            init (gen t (dec d))
            outer (set (live-owned))
            _ (swap! *g* assoc-in [:scalars i] :long)
            _ (if (vector? t)
                (swap! *g* assoc-in [:owned acc] {:alive true})
                (swap! *g* assoc-in [:scalars acc] t))
            [step exit]
            (branch-merge
             (fn []
               (let [bo (:borrow-only @*g*)
                     _ (swap! *g* assoc :borrow-only (into (or bo #{}) outer))
                     s (gen t (dec d))]
                 (swap! *g* assoc :borrow-only bo)
                 s))
             (fn [] (if (and (vector? t) (chance 50))
                      (do (kill! acc) acc)
                      (gen t (dec d)))))]
        (list 'loop [i 0 acc init]
              (list 'if (list '< i (rnd 4)) (list 'recur (list 'inc i) step) exit))))))

(defn gen [t d]
  (case t
    :long (gen-long d)
    :double (gen-double d)
    :bool (gen-bool d)
    (gen-array d)))

(defn gen-program
  "[body planted?] for one random program of a random result type."
  [seed]
  (binding [*rng* (atom seed)
            *g* (atom {:n 0 :owned {} :scalars {} :borrow-only #{}})
            *planted* (atom false)
            *plant-mode* (zero? (mod seed 5))]
    (let [t (pick [:long :double :bool [:array :double] [:array :double]])]
      (if *plant-mode*
        ;; start with a value already given away (v0, moved into v1), so the
        ;; generator has something dead to reach for from the first form
        (do (swap! *g* #(-> % (assoc :n 1)
                            (assoc-in [:owned 'v0] {:alive false})
                            (assoc-in [:owned 'v1] {:alive true})))
            (let [body (gen t 4)]
              [(list 'let ['v0 (list 'double-array 3) 'v1 'v0] body) @*planted*]))
        [(gen t 4) @*planted*]))))

(def fuzz-params '[^doubles xs ^long n ^double dd ^longs ls])
(def fuzz-inputs [[(da 1 2 3 4 5) 2 0.25 (la 1 2 3 4 5)]
                  [(da -1.5 0 7) 0 -3.0 (la big59 -3)]
                  [(da) 4 1e300 (la)]])

(defn fuzz! []
  (let [stats (atom {:accepted 0 :planted 0 :rejected-valid 0})]
    (doseq [k (range (or (some-> (System/getenv "NATIVE_FUZZ_SKIP") parse-long) 0) fuzz-n)]
      (let [seed (+ fuzz-seed (* 7919 k))
            [body planted?] (gen-program seed)
            label (str "fuzz seed=" seed)
            _ (when (System/getenv "NATIVE_FUZZ_TRACE")
                (binding [*out* *err*] (println label (pr-str body)) (flush)))
            verdict (try (mem/check-region (ast/analyze-region fuzz-params [body])) :ok
                         (catch Throwable e e))]
        (cond
          (and planted? (= verdict :ok))
          (fail! (str label ": a planted use of a moved value was accepted: " (pr-str body)))

          planted?
          (if (re-find #"given away|loop iteration" (.getMessage ^Throwable verdict))
            (swap! stats update :planted inc)
            (fail! (str label ": planted violation rejected for another reason: "
                        (.getMessage ^Throwable verdict) "\n  " (pr-str body))))

          (not= verdict :ok)
          (do (swap! stats update :rejected-valid inc)
              (fail! (str label ": the checker rejected a program its model accepts: "
                          (.getMessage ^Throwable verdict) "\n  " (pr-str body))))

          :else
          (do (swap! stats update :accepted inc)
              (let [before (count @failures)]
                (check-run label fuzz-params [body] fuzz-inputs)
                (when (> (count @failures) before)
                  (println "  program:" (pr-str body))))))))
    (println "fuzz:" fuzz-n "programs from seed" fuzz-seed "--" (pr-str @stats))))

(fuzz!)


(def golden-unseen
  ;; only meaningful for the default fuzz run, which is what golden.edn records
  (when-not (some #(System/getenv %) ["NATIVE_FUZZ_SEED" "NATIVE_FUZZ_N" "NATIVE_FUZZ_SKIP"])
    (remove @golden-seen (keys @golden))))
(if golden-write?
  (do (swap! golden #(apply dissoc % golden-unseen))
      (spit golden-path
            (with-out-str
              (println ";; test/chez/native-test.clj's golden oracle: what each row answered when")
              (println ";; regions had a Jolt fallback. Regenerate only with NATIVE_GOLDEN=write.")
              (println "{")
              (doseq [[k v] (sort-by key @golden)] (println (pr-str k) (pr-str v)))
              (println "}")))
      (println "native-test: wrote" (count @golden) "golden outcomes"))
  (do (when (seq golden-unseen)
        (fail! (str (count golden-unseen) " golden.edn entries no row produced (renamed, reordered or"
                    " deleted rows?), e.g. " (pr-str (take 3 golden-unseen)))))
      (println "native-test:" @golden-checked "outcomes matched golden.edn")))
(if (seq @failures)
  (do (println (count @failures) "native-test failure(s)") (System/exit 1))
  (println "NATIVE OK"))
