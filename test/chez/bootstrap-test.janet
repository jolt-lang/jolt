# Chez Phase 3 inc9a (jolt-9phg) — the pure-Chez self-build (no Janet in the loop).
#
# inc8 proved the on-Chez compiler reproduces itself. inc9a makes that the actual
# build: host/chez/bootstrap.ss loads the CHECKED-IN seed (host/chez/seed/{prelude,
# image}.ss — the bootstrap compiler, minted once via the fixpoint) and rebuilds the
# clojure.core prelude + compiler image FROM SOURCE on Chez. No Janet is invoked in
# the compile path — this test only spawns `chez`; the read->analyze->emit is 100%
# Chez. So a fresh checkout + Chez (no Janet) yields a working jolt.
#
# The seed is a JOINT byte-fixpoint, so rebuilding from an up-to-date seed
# reproduces it exactly. If the seed SOURCES change (core tiers, the compiler, the
# host contract, the reader, emit-image.ss) the rebuilt artifacts will differ and
# this test fails — re-mint the seed with driver/mint-chez-seed* (see the failure
# message) and commit the refreshed seed.
#
#   janet test/chez/bootstrap-test.janet
(import ../../host/chez/driver :as d)

(var total 0) (var fails 0)
(defn ok [name pred &opt extra]
  (++ total)
  (if pred (printf "ok: %s" name)
    (do (++ fails) (printf "FAIL: %s   %s" name (or extra "")))))

(unless (d/chez-available?)
  (print "chez not on PATH — skipping bootstrap-test")
  (os/exit 0))

(def seed-prelude "host/chez/seed/prelude.ss")
(def seed-image "host/chez/seed/image.ss")
(ok "checked-in seed prelude exists" (os/stat seed-prelude))
(ok "checked-in seed image exists" (os/stat seed-image))

(when (and (os/stat seed-prelude) (os/stat seed-image))
  (def tmp (or (os/getenv "TMPDIR") "/tmp"))
  (def out-prelude (string tmp "/jolt-bootstrap-pre-" (os/getpid) ".ss"))
  (def out-image (string tmp "/jolt-bootstrap-img-" (os/getpid) ".ss"))
  (def t0 (os/clock))
  # PURE CHEZ: spawn `chez --script bootstrap.ss` — no Janet in the compile path.
  (def [code out err] (d/run-bootstrap seed-prelude seed-image out-prelude out-image))
  (printf "pure-Chez bootstrap pass: %.1fs" (- (os/clock) t0))
  (ok "bootstrap.ss runs cleanly on Chez (no Janet)" (= code 0)
      (string "exit " code " " (string/slice err 0 (min 300 (length err)))))

  (defn- bytes= [a b] (and (os/stat a) (os/stat b)
                           (= (string (slurp a)) (string (slurp b)))))
  (def remint "re-mint with driver/mint-chez-seed* and commit host/chez/seed/")
  (when (= code 0)
    (ok "rebuilt prelude == checked-in seed (joint fixpoint)" (bytes= out-prelude seed-prelude)
        (string "seed is stale — " remint))
    (ok "rebuilt image == checked-in seed (joint fixpoint)" (bytes= out-image seed-image)
        (string "seed is stale — " remint))

    # The rebuilt artifacts must be a WORKING compiler.
    (def cases
      [["(let [x 1 y 2] (+ x y))" "3"]
       ["(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)" "55"]
       ["(->> (range 10) (filter even?) (map #(* % %)) (reduce +))" "120"]
       ["(let [{:keys [a b] :or {b 9}} {:a 1}] [a b])" "[1 9]"]
       ["(require '[clojure.string :as s]) (s/join \",\" [1 2 3])" "1,2,3"]
       ["(loop [i 0 acc 0] (if (< i 5) (recur (inc i) (+ acc i)) acc))" "10"]])
    (var pass 0)
    (each [src want] cases
      (def [c o _] (d/eval-zero-janet out-prelude out-image (string "(do " src ")")))
      (when (and (= c 0) (= o want)) (++ pass)))
    (ok "Chez-built artifacts compile+run real cases" (= pass (length cases))
        (string pass "/" (length cases) " cases passed")))

  (each p [out-prelude out-image] (when (os/stat p) (os/rm p))))

(printf "\nbootstrap-test: %d/%d checks passed" (- total fails) total)
(os/exit (if (zero? fails) 0 1))
