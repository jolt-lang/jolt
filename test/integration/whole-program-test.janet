# Whole-program (Stalin) mode (jolt-t34, opt-in JOLT_WHOLE_PROGRAM): one closed-
# world inference fixpoint over ALL user namespaces, so param types propagate
# across ns boundaries (a non-inlined fn's record params get proven from its
# callers in another unit). This must be SOUND — same results as the per-ns
# pass — which is what this test guards, by running a cross-namespace record
# program both ways through the built binary and comparing output. Skips cleanly
# if build/jolt is absent (source-only test run).
(def jolt "build/jolt")

(defn- run [env-extra]
  (def dir (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-wp-test"))
  (os/mkdir dir)
  (spit (string dir "/wputil.clj")
        (string "(ns wputil)\n"
                "(defrecord V [x y z])\n"
                # recursive => never inlined; params proven only whole-program
                "(defn dot [a b n]\n"
                "  (if (<= n 0) 0.0\n"
                "    (+ (* (:x a) (:x b)) (* (:y a) (:y b)) (* (:z a) (:z b)) (dot a b (dec n)))))\n"))
  (spit (string dir "/wpmain.clj")
        (string "(ns wpmain (:require [wputil :as v]))\n"
                "(defn -main []\n"
                "  (loop [i 0 acc 0.0]\n"
                "    (if (< i 1000)\n"
                "      (let [a (v/->V (double i) 2.0 3.0) b (v/->V 1.0 (double i) 0.5)]\n"
                "        (recur (inc i) (+ acc (v/dot a b 2))))\n"
                "      (println \"sum\" acc))))\n"))
  (def out (string dir "/out.txt"))
  (def jbin (string (os/cwd) "/" jolt))
  (def cmd (string env-extra "JOLT_DIRECT_LINK=1 JOLT_PATH=" dir " " jbin
                   " -m wpmain > " out " 2>&1"))
  (os/execute ["sh" "-c" cmd] :p)
  (string/trimr (slurp out)))

(if (not (os/stat jolt))
  (print "whole-program: SKIP (no build/jolt — run from source)")
  (let [per-ns (run "")
        whole  (run "JOLT_WHOLE_PROGRAM=1 ")]
    (printf "  per-ns:        %s" per-ns)
    (printf "  whole-program: %s" whole)
    (if (and (= per-ns whole) (string/has-prefix? "sum" per-ns))
      (print "whole-program: results match — sound")
      (do (printf "whole-program: MISMATCH per-ns=%q whole=%q" per-ns whole)
          (os/exit 1)))))
