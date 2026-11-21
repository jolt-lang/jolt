;; jolt.native gate: native regions (Carp-style owned arrays, compiled to C).
;; Each accepted program is compiled with a tracking allocator, run as C and as
;; its Jolt fallback on identical copies of its inputs, and must agree on the
;; result, the exception (class and message), what it did to its input arrays,
;; and leave no live allocation and no bad free behind, on success and on every
;; error exit. Rejected programs must fail to compile with the expected message.
;; Needs cc. Run: bin/jolt run test/chez/native-test.clj
(ns native-test
  (:require [jolt.scheme :as scm]
            [jolt.native.ast :as ast]
            [jolt.native.memory :as mem]
            [jolt.native.emit :as emit]
            [jolt.native.fallback :as fallback]
            [jolt.native.rt :as rt]))

(def failures (atom []))
(defn fail! [msg] (swap! failures conj msg) (println "FAIL" msg))

(def work-dir (str (System/getProperty "user.dir") "/target/native-test"))
(.mkdirs (java.io.File. work-dir))

;; A per-library allocation tracker: every live block in a small registry, a
;; free of anything not in it (double free, wild pointer) raises the bad flag.
(defn tracker [sym]
  (str "#include <stdlib.h>\n#include <stdint.h>\n"
       "static void *jn_live_set[65536]; static int64_t jn_live_n = 0, jn_bad = 0, jn_peak = 0;\n"
       "__attribute__((unused)) static void *jn_track(void *p) { if (p) { if (jn_live_n < 65536) jn_live_set[jn_live_n++] = p; else jn_bad = 1;"
       " if (jn_live_n > jn_peak) jn_peak = jn_live_n; } return p; }\n"
       "__attribute__((unused)) static void jn_untrack(void *p) { if (!p) return; for (int64_t i = 0; i < jn_live_n; i++)"
       " if (jn_live_set[i] == p) { jn_live_set[i] = jn_live_set[--jn_live_n]; free(p); return; } jn_bad = 1; }\n"
       "#define JN_CALLOC(n, s) jn_track(calloc((n), (s)))\n"
       "#define JN_MALLOC(n) jn_track(malloc(n))\n"
       "#define JN_FREE(p) jn_untrack(p)\n"
       "int64_t " sym "_live(void) { return jn_live_n; }\n"
       "int64_t " sym "_bad(void) { return jn_bad; }\n"
       "int64_t " sym "_peak(void) { int64_t p = jn_peak; jn_peak = jn_live_n; return p; }\n"))

(def flv (scm/call "record-accessor" (scm/call "record-rtd" (double-array 1)) 0))
(def mkbv (scm/proc "make-bytevector"))
(def s64 (scm/proc "bytevector-s64-native-ref"))
(def f64 (scm/proc "bytevector-ieee-double-native-ref"))
(def counter (atom 0))

(defn compile-region
  "params + body -> {:call f :live f :bad f :peak f :region r :c src}."
  [params body]
  (let [r (mem/check-region (ast/analyze-region params body))
        sym (str "jn_t" (swap! counter inc))
        src (str (tracker sym) (emit/emit-region r sym))
        c (str work-dir "/" sym ".c")
        so (str work-dir "/" sym ".dylib")
        _ (spit c src)
        cmd ["cc" "-O2" "-ffp-contract=off" "-shared" "-fPIC" "-Wall" "-Werror"
             "-Wno-unused-variable" "-Wno-unused-but-set-variable" "-o" so c
             ;; bionic links no libm into a shared object unasked (atan2 went unresolved)
             "-lm"]
        p (.start (ProcessBuilder. ^java.util.List cmd))
        err (slurp (.getErrorStream p))]
    (when-not (zero? (.waitFor p))
      (throw (ex-info (str "cc failed for " sym ":\n" err) {:src src})))
    ;; process-global, like a statically linked region; ffi/load-library scopes
    ;; a library to its own defcfns
    (scm/call "load-shared-object" so)
    (let [entry (scm/eval-string (emit/foreign-sig r sym))
          fp (fn [suffix args ret]
               (scm/eval-string (str "(foreign-procedure \"" sym suffix "\" " args " " ret ")")))
          copy-out (case (:type r)
                     [:array :double] (fp "_copy_out" "(scheme-object uptr integer-64)" "void")
                     [:array :long] (fp "_copy_out" "(scheme-object uptr integer-64)" "integer-64")
                     nil)
          elem (when (= [:array :long] (:type r)) (fp "_elem" "(uptr integer-64)" "integer-64"))
          free (when (= [:array :long] (:type r)) (fp "_free" "(uptr)" "void"))
          q (fn [n] (scm/eval-string (str "(foreign-procedure \"" sym "_" n "\" () integer-64)")))]
      {:region r :c src
       :live (q "live") :bad (q "bad") :peak (q "peak")
       :call (fn [& args]
               (let [out (mkbv 40 0)
                     cargs (map (fn [p a] (if (ast/borrowed? (:type p)) (flv a) a)) (:params r) args)
                     res (apply entry (concat cargs [out]))
                     code (s64 out 0)]
                 (when-not (zero? code)
                   (rt/raise! code (s64 out 8) (s64 out 16) (f64 out 24)))
                 (cond
                   (= [:array :long] (:type r))
                   (scm/call "jolt-native-wrap-longs" (s64 out 32) res copy-out elem free)
                   (ast/owned? (:type r))
                   (let [n (s64 out 32) arr (double-array n)]
                     (copy-out (flv arr) res n)
                     arr)
                   (#{:unit :never} (:type r)) nil
                   :else res)))})))

(defn fallback-fn [r]
  (eval (list 'fn (mapv :name (:params r)) (fallback/region-form r))))

;; the backend runs each argument through the host check for its kind before
;; either half; the harness does the same
(def arg-checks {[:ref :double] "jolt-native-arg-doubles" [:ref :long] "jolt-native-arg-longs"
                 :long "jolt-native-arg-long" :double "jolt-native-arg-double" :bool "jolt-native-arg-bool"})
(defn checked [r f] (fn [& args] (apply f (map (fn [p a] (scm/call (arg-checks (:type p)) a)) (:params r) args))))

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

(defn copy-args [args]
  (mapv #(if (or (instance? (class (double-array 0)) %) (instance? longs-class %)) (aclone %) %) args))

(defn check-run
  "Run one program as C and as its fallback on each arg vector; compare."
  [label params body arg-vecs & {:keys [raw]}]
  (let [compiled (try (compile-region params body)
                      (catch Throwable e (fail! (str label ": compile failed: " (.getMessage e))) nil))]
    (when compiled
      (let [fb (fallback-fn (:region compiled))
            raw-f (when raw (eval (list* 'fn (vec params) body)))]
        (doseq [args arg-vecs]
          (let [ca (copy-args args) fa (copy-args args) ra (copy-args args)
                c (outcome #(apply (checked (:region compiled) (:call compiled)) %) ca)
                f (outcome #(apply (checked (:region compiled) fb) %) fa)]
            (when-not (if (contains? c :value)
                        (and (contains? f :value) (same? (:value c) (:value f)))
                        (= c f))
              (fail! (str label " " (pr-str args) ": C " (pr-str c) " vs fallback " (pr-str f))))
            (when-not (every? true? (map same? ca fa))
              (fail! (str label " " (pr-str args) ": inputs after C and fallback differ")))
            (when raw-f
              (let [r (outcome #(apply raw-f %) ra)]
                (when-not (if (contains? r :value)
                            (and (contains? f :value) (same? (:value r) (:value f)))
                            (= r f))
                  (fail! (str label " " (pr-str args) ": fallback " (pr-str f)
                              " vs body as plain Clojure " (pr-str r))))))
            (let [live ((:live compiled)) bad ((:bad compiled))]
              (when-not (zero? live) (fail! (str label " " (pr-str args) ": leaked " live " block(s)")))
              (when-not (zero? bad) (fail! (str label " " (pr-str args) ": bad free"))))))
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

;; --- random programs ----------------------------------------------------------
;; A generator builds well-typed region programs and tracks ownership with its
;; own model of the rules (moves in let bindings, returns and recur; borrows in
;; aget/aset/alength/aclone; no outer value moved in a continuing loop
;; iteration). About one program in twelve gets a planted violation: a use of a
;; value the model knows is gone. The checker must accept every program the
;; model accepts (which then has to pass the C-vs-fallback run and leave no
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
      (case (rnd 10)
        0 (list (pick '[+ - * /]) (gen :double (dec d)) (gen (pick [:double :long]) (dec d)))
        1 (let [a (borrowable)] (list 'aget a (with-borrowed a #(gen-idx d))))
        2 (list 'Math/sqrt (gen :double (dec d)))
        3 (list 'double (gen :long (dec d)))
        4 (let [c (gen :bool (dec d))
                [t e] (branch-merge #(gen :double (dec d)) #(gen :double (dec d)))]
            (list 'if c t e))
        5 (gen-let :double d)
        6 (gen-loop :double d)
        8 (list 'do (list 'aset 'ls (gen-idx d) (gen :long (dec d))) (gen :double (dec d)))
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
      ;; the other violation: moving an outer value in an iteration that continues
      (and *plant-mode* (seq (:borrow-only @*g*)) (chance 30)
           (some (set (live-owned)) (:borrow-only @*g*)))
      (do (reset! *planted* true) (pick (vec (filter (set (live-owned)) (:borrow-only @*g*)))))
      (<= d 0) (list 'double-array (rnd 5))
      :else
      (case (rnd 5)
        0 (list 'double-array (gen-idx d))
        1 (list 'aclone (borrowable))
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
    (dotimes [k fuzz-n]
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


(if (seq @failures)
  (do (println (count @failures) "native-test failure(s)") (System/exit 1))
  (println "NATIVE OK"))
