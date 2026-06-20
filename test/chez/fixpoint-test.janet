# Chez Phase 3 inc8 (jolt-bzni) — the stage2==stage3 self-hosting fixpoint.
#
# The zero-Janet spine (spine-test / run-corpus-zero-janet) proves the ON-CHEZ
# analyzer+emitter compile arbitrary Clojure faithfully. This proves the stronger
# property from self-hosting-bootstrap-research §4: the on-Chez compiler reproduces
# ITSELF. The compiler image (jolt.ir + jolt.analyzer + jolt.backend-scheme cross-
# compiled to Scheme def-var! forms) is built three ways:
#
#   stage1  = Janet analyzer/emitter cross-compiles the compiler sources
#             (driver/emit-compiler-image — the current bootstrap input)
#   stage2  = the ON-CHEZ compiler loaded from stage1 re-emits the same sources
#             (driver/emit-image-on-chez, host/chez/emit-image.ss)
#   stage3  = the ON-CHEZ compiler loaded from stage2 re-emits them again
#
# stage1 differs from stage2 only in gensym numbering (the Janet build allocates
# more gensyms before reaching the compiler emit), so the fixpoint is stage2 vs
# stage3: both are produced by Chez from a fresh process, so a byte-for-byte match
# means the compiler has converged — it compiles its own source to itself. We also
# run real compile+eval cases THROUGH stage2 to prove it's a working compiler, not
# a degenerate one that just happens to be stable.
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
(def prelude-path (jc/ensure-prelude ctx))

# stage1: the Janet cross-compiled image, cached by source fingerprint (same scheme
# as spine-test / run-corpus-zero-janet).
(defn- image-fingerprint []
  (string/slice (string (hash (string/join
    (map slurp ["jolt-core/jolt/ir.clj" "jolt-core/jolt/analyzer.clj"
                "jolt-core/jolt/backend_scheme.clj" "host/chez/host-contract.ss"
                "host/chez/compile-eval.ss"])))) 0))
(def tmp (or (os/getenv "TMPDIR") "/tmp"))
(def stage1 (string tmp "/jolt-compiler-image-" (image-fingerprint) ".ss"))
(def t0 (os/clock))
(d/ensure-compiler-image ctx stage1)
(printf "stage1 (Janet cross-compile): %d bytes (%.1fs)" (length (slurp stage1)) (- (os/clock) t0))
(flush)

# stage2 = on-Chez compiler (from stage1) re-emits the compiler. Fresh temp path so
# it always regenerates.
(def stage2 (string tmp "/jolt-fixpoint-stage2-" (os/getpid) ".ss"))
(def stage3 (string tmp "/jolt-fixpoint-stage3-" (os/getpid) ".ss"))
(def t1 (os/clock))
(def [c2 e2] (d/emit-image-on-chez prelude-path stage1 stage2))
(ok "stage2 emits cleanly on Chez" (and (= c2 0) (os/stat stage2))
    (string "exit " c2 " " (string/slice e2 0 (min 300 (length e2)))))
(when (os/stat stage2)
  (printf "stage2 (on-Chez, from stage1): %d bytes (%.1fs)" (length (slurp stage2)) (- (os/clock) t1)))
(flush)

# stage3 = on-Chez compiler (from stage2) re-emits the compiler.
(def t2 (os/clock))
(def [c3 e3] (d/emit-image-on-chez prelude-path stage2 stage3))
(ok "stage3 emits cleanly on Chez" (and (= c3 0) (os/stat stage3))
    (string "exit " c3 " " (string/slice e3 0 (min 300 (length e3)))))
(when (os/stat stage3)
  (printf "stage3 (on-Chez, from stage2): %d bytes (%.1fs)" (length (slurp stage3)) (- (os/clock) t2)))
(flush)

# THE FIXPOINT: stage2 and stage3 must be byte-for-byte identical.
(when (and (os/stat stage2) (os/stat stage3))
  # slurp returns a buffer; Janet = on buffers is identity, so compare as strings.
  (def s2 (string (slurp stage2))) (def s3 (string (slurp stage3)))
  (ok "stage2 == stage3 (byte-for-byte fixpoint)" (= s2 s3)
      (if (= s2 s3) ""
        (string "sizes " (length s2) " vs " (length s3)
                "; first diff at "
                (let [n (min (length s2) (length s3))]
                  (var i 0) (while (and (< i n) (= (s2 i) (s3 i))) (++ i)) i))))
  # A degenerate emitter (emits nothing) would also be "stable" — guard against it.
  (ok "stage2 image is substantial (> 80KB)" (> (length s2) 80000)
      (string "only " (length s2) " bytes")))

# stage2 must be a WORKING compiler: drive real compile+eval through it.
(def verify-cases
  [["(let [x 1 y 2] (+ x y))" "3"]
   ["(when (> 5 3) (-> 10 (- 1) (* 2)))" "18"]
   ["(defn f [a b] (* a b)) (f 6 7)" "42"]
   ["(map inc [1 2 3])" "(2 3 4)"]
   ["(reduce + 0 (range 5))" "10"]
   ["(filter even? (range 10))" "(0 2 4 6 8)"]])
(when (os/stat stage2)
  (var vpass 0)
  (each [src want] verify-cases
    (def [code out _] (d/eval-zero-janet prelude-path stage2 (string "(do " src ")")))
    (when (and (= code 0) (= out want)) (++ vpass)))
  (ok "stage2 is a working compiler (real cases compile+run)"
      (= vpass (length verify-cases))
      (string vpass "/" (length verify-cases) " cases passed")))

# cleanup temp stages
(each p [stage2 stage3] (when (os/stat p) (os/rm p)))

(printf "\nfixpoint-test: %d/%d checks passed" (- total fails) total)
(os/exit (if (zero? fails) 0 1))
