;; jolt.native gate: native regions (Carp-style owned arrays, compiled to C).
;; Each accepted program is compiled with a tracking allocator, run as C and as
;; its Jolt fallback on identical copies of its inputs, and must agree on the
;; result, the exception (class and message), what it did to its input arrays,
;; and leave no live allocation and no bad free behind, on success and on every
;; error exit. Rejected programs must fail to compile with the expected message.
;; Needs cc. Run: bin/jolt run test/chez/native-test.clj
(ns native-test
  (:require [clojure.string :as str]
            [jolt.ffi :as ffi]
            [jolt.scheme :as scm]
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
             "-Wno-unused-variable" "-Wno-unused-but-set-variable" "-o" so c]
        p (.start (ProcessBuilder. ^java.util.List cmd))
        err (slurp (.getErrorStream p))]
    (when-not (zero? (.waitFor p))
      (throw (ex-info (str "cc failed for " sym ":\n" err) {:src src})))
    ;; process-global, like a statically linked region; ffi/load-library scopes
    ;; a library to its own defcfns
    (scm/call "load-shared-object" so)
    (let [entry (scm/eval-string (emit/foreign-sig r sym))
          copy-out (when (ast/owned? (:type r))
                     (scm/eval-string (str "(foreign-procedure \"" sym "_copy_out\" (scheme-object uptr integer-64) void)")))
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
                   (ast/owned? (:type r))
                   (let [n (s64 out 32) arr (double-array n)]
                     (copy-out (flv arr) res n)
                     arr)
                   (#{:unit :never} (:type r)) nil
                   :else res)))})))

(defn fallback-fn [r]
  (eval (list 'fn (mapv :name (:params r)) (fallback/region-form r))))

(defn outcome [f args]
  (try {:value (f args)}
       (catch Throwable e {:throw (.getName (class e)) :msg (.getMessage e)})))

(defn same-double? [a b]
  (or (and (Double/isNaN a) (Double/isNaN b))
      (and (== a b) (= (Math/copySign 1.0 a) (Math/copySign 1.0 b)))))

(defn same? [a b]
  (cond
    (and (double? a) (double? b)) (same-double? a b)
    (and (instance? (class (double-array 0)) a) (instance? (class (double-array 0)) b))
    (and (= (alength a) (alength b)) (every? true? (map same-double? a b)))
    :else (= a b)))

(defn copy-args [args] (mapv #(if (instance? (class (double-array 0)) %) (aclone %) %) args))

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
                c (outcome #(apply (:call compiled) %) ca)
                f (outcome #(apply fb %) fa)]
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

(check-run "negative size" '[^long n] '((alength (double-array n))) [[3] [0] [-2]] :raw true)

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
(check-reject "non-boolean test" '[^long n] '((if n 1 2)) #"must be a boolean")
(check-reject "long division" '[^long a] '((/ a 2)) #"ratio")
(check-reject "= across number types" '[^long a] '((= a 1.0)) #"same scalar type")
(check-reject "unhinted parameter" '[a] '(a) #"needs a type hint")
(check-reject "recur type mismatch" '[]
              '((loop [s 0] (if (< s 3) (recur (+ s 0.5)) s))) #"initialize it with a double")
(check-reject "recur not in tail position" '[]
              '((loop [i 0] (do (recur (inc i)) i))) #"tail position")
(check-reject "unknown function" '[^long a] '((println a)) #"not allowed")
(check-reject "^longs parameter" '[^longs xs] '((alength xs)) #"not supported yet")

(if (seq @failures)
  (do (println (count @failures) "native-test failure(s)") (System/exit 1))
  (println "NATIVE OK"))
