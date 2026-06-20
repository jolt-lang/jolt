# Chez Phase 3 inc8 (jolt-bzni) — the self-hosting bootstrap fixpoint.
#
# The zero-Janet spine (spine-test / run-corpus-zero-janet) proves the ON-CHEZ
# analyzer+emitter compile arbitrary Clojure faithfully. This proves the stronger
# property from self-hosting-bootstrap-research §4: the emitted system reproduces
# ITSELF. Two artifacts are re-emitted ON CHEZ by the loaded compiler:
#
#   COMPILER IMAGE  (jolt.ir + jolt.analyzer + jolt.backend-scheme)
#     stage1 = Janet analyzer/emitter cross-compiles the sources (the bootstrap input)
#     stage2 = the on-Chez compiler (from stage1) re-emits them
#     stage3 = the on-Chez compiler (from stage2) re-emits them
#     FIXPOINT: stage2 == stage3 (stage1 differs only in gensym numbering — the
#     Janet build allocates more gensyms before reaching the compiler emit).
#
#   CORE PRELUDE  (clojure.core tiers + clojure.string/walk/template/edn/set/pprint)
#     pstage2 = on-Chez compiler re-emits the prelude with the JANET prelude loaded
#     pstage3 = ... with pstage2 loaded
#     pstage4 = ... with pstage3 loaded
#     FIXPOINT: pstage3 == pstage4. The prelude converges one stage later than the
#     compiler because its MACRO expanders bake an auto-gensym id (foo#) at emit
#     time, so a macro emitted by Janet (pstage2's loaded prelude) carries a
#     different baked id than one emitted by Chez — only once BOTH stages load a
#     Chez-emitted prelude (pstage3 onward) does it stabilize.
#
# Finally we load the FULLY Chez-emitted system (Chez prelude + Chez compiler
# image, NO Janet-emitted artifact in the loop) and run real cases, proving the
# fixpoint is a working compiler, not a degenerate stable one.
#
#   janet test/chez/fixpoint-test.janet
(import ../../host/chez/driver :as d)
(import ../../host/chez/jolt-chez :as jc)

(var total 0) (var fails 0)
(defn ok [name pred &opt extra]
  (++ total)
  (if pred (printf "ok: %s" name)
    (do (++ fails) (printf "FAIL: %s   %s" name (or extra "")))))

(unless (d/chez-available?)
  (print "chez not on PATH — skipping fixpoint-test")
  (os/exit 0))

(def ctx (d/make-ctx))
(def jprelude (jc/ensure-prelude ctx))

# stage1: the Janet cross-compiled compiler image, cached by source fingerprint
# (same scheme as spine-test / run-corpus-zero-janet).
(defn- image-fingerprint []
  (string/slice (string (hash (string/join
    (map slurp ["jolt-core/jolt/ir.clj" "jolt-core/jolt/analyzer.clj"
                "jolt-core/jolt/backend_scheme.clj" "host/chez/host-contract.ss"
                "host/chez/compile-eval.ss"])))) 0))
(def tmp (or (os/getenv "TMPDIR") "/tmp"))
(def stage1 (string tmp "/jolt-compiler-image-" (image-fingerprint) ".ss"))
(d/ensure-compiler-image ctx stage1)
(printf "stage1 compiler image (Janet cross-compile): %d bytes" (length (slurp stage1)))
(flush)

(defn- bytes= [a b] (= (string (slurp a)) (string (slurp b))))
(defn- first-diff [a b]
  (def s (string (slurp a))) (def t (string (slurp b)))
  (def n (min (length s) (length t)))
  (var i 0) (while (and (< i n) (= (s i) (t i))) (++ i))
  (string "sizes " (length s) " vs " (length t) ", first diff at " i))

# ---- compiler-image fixpoint: stage2 == stage3 -------------------------------
(def s2 (string tmp "/jolt-fixpoint-img2-" (os/getpid) ".ss"))
(def s3 (string tmp "/jolt-fixpoint-img3-" (os/getpid) ".ss"))
(def [c2 e2] (d/emit-image-on-chez jprelude stage1 s2))
(ok "compiler image stage2 emits cleanly on Chez" (and (= c2 0) (os/stat s2))
    (string "exit " c2 " " (string/slice e2 0 (min 300 (length e2)))))
(def [c3 e3] (d/emit-image-on-chez jprelude s2 s3))
(ok "compiler image stage3 emits cleanly on Chez" (and (= c3 0) (os/stat s3))
    (string "exit " c3 " " (string/slice e3 0 (min 300 (length e3)))))
(when (and (os/stat s2) (os/stat s3))
  (ok "compiler image: stage2 == stage3 (byte-for-byte fixpoint)" (bytes= s2 s3)
      (first-diff s2 s3))
  (ok "compiler image is substantial (> 80KB)" (> (length (slurp s2)) 80000)))

# ---- prelude fixpoint: pstage3 == pstage4 ------------------------------------
(def p2 (string tmp "/jolt-fixpoint-prelude2-" (os/getpid) ".ss"))
(def p3 (string tmp "/jolt-fixpoint-prelude3-" (os/getpid) ".ss"))
(def p4 (string tmp "/jolt-fixpoint-prelude4-" (os/getpid) ".ss"))
(def [pc2 pe2] (d/emit-image-on-chez jprelude stage1 p2 "jolt-emit-prelude"))
(ok "prelude pstage2 emits cleanly on Chez (from Janet prelude)" (and (= pc2 0) (os/stat p2))
    (string "exit " pc2 " " (string/slice pe2 0 (min 300 (length pe2)))))
(when (os/stat p2)
  (def [pc3 pe3] (d/emit-image-on-chez p2 stage1 p3 "jolt-emit-prelude"))
  (ok "prelude pstage3 emits cleanly on Chez (from pstage2)" (and (= pc3 0) (os/stat p3))
      (string "exit " pc3 " " (string/slice pe3 0 (min 300 (length pe3)))))
  (when (os/stat p3)
    (def [pc4 pe4] (d/emit-image-on-chez p3 stage1 p4 "jolt-emit-prelude"))
    (ok "prelude pstage4 emits cleanly on Chez (from pstage3)" (and (= pc4 0) (os/stat p4))
        (string "exit " pc4 " " (string/slice pe4 0 (min 300 (length pe4)))))
    (when (os/stat p4)
      (ok "prelude: pstage3 == pstage4 (byte-for-byte fixpoint)" (bytes= p3 p4)
          (first-diff p3 p4))
      (ok "prelude is substantial (> 250KB)" (> (length (slurp p3)) 250000)))))

# ---- the fully Chez-emitted system is a working compiler ----------------------
# Chez-emitted prelude (pstage3) + Chez-emitted compiler image (s2): no Janet
# artifact in the loop. Drive real compile+eval through it.
(def verify-cases
  [["(let [x 1 y 2] (+ x y))" "3"]
   ["(when (> 5 3) (-> 10 (- 1) (* 2)))" "18"]
   ["(defn f [a b] (* a b)) (f 6 7)" "42"]
   ["(map inc [1 2 3])" "(2 3 4)"]
   ["(reduce + 0 (range 5))" "10"]
   ["(let [{:keys [a b]} {:a 7 :b 8}] (+ a b))" "15"]
   ["(filter even? (range 10))" "(0 2 4 6 8)"]
   ["(require '[clojure.string :as s]) (s/upper-case \"hi\")" "HI"]
   ["(cond (= 1 2) :a (= 1 1) :b :else :c)" ":b"]])
(when (and (os/stat p3) (os/stat s2))
  (var vpass 0)
  (each [src want] verify-cases
    (def [code out _] (d/eval-zero-janet p3 s2 (string "(do " src ")")))
    (when (and (= code 0) (= out want)) (++ vpass)))
  (ok "fully Chez-emitted system (Chez prelude + Chez image) compiles+runs real cases"
      (= vpass (length verify-cases))
      (string vpass "/" (length verify-cases) " cases passed")))

# cleanup temp stages
(each p [s2 s3 p2 p3 p4] (when (os/stat p) (os/rm p)))

(printf "\nfixpoint-test: %d/%d checks passed" (- total fails) total)
(os/exit (if (zero? fails) 0 1))
