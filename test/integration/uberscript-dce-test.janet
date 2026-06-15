# Dead-code elimination in `jolt uberscript` (jolt-atg): a bundle is closed-
# world (everything it needs is inlined, nothing is required later), so a user
# `defn` that is unreachable from the entry point's reference graph can be
# dropped. Sound + conservative: only plain defn/defn- are prunable; a defn is
# kept if its (bare or ns-qualified) name appears anywhere in a kept form, the
# closure iterates to a fixpoint, and any use of dynamic resolution
# (resolve/eval/...) keeps everything. Drives the BUILT binary (uberscript is a
# CLI command); skips cleanly if build/jolt is absent.
(def jolt "build/jolt")

(defn- write-app [dir files]
  (os/mkdir dir)
  (each [name body] (partition 2 files) (spit (string dir "/" name) body)))

(defn- uber [dir main-ns]
  (def out (string dir "/out.clj"))
  (def jbin (string (os/cwd) "/" jolt))
  (os/execute ["sh" "-c" (string "JOLT_PATH=" dir " " jbin " uberscript " out " -m " main-ns " 2>/dev/null")] :p)
  (slurp out))

(defn- run-bundle [dir]
  (def out2 (string dir "/run.txt"))
  (def jbin (string (os/cwd) "/" jolt))
  (os/execute ["sh" "-c" (string jbin " " dir "/out.clj > " out2 " 2>&1")] :p)
  (string/trimr (slurp out2)))

(defn- has? [s needle] (not (nil? (string/find needle s))))

(if (not (os/stat jolt))
  (print "uberscript-dce: SKIP (no build/jolt — run from source)")
  (do
    # --- basic: an unreachable defn is dropped; reachable ones survive --------
    (def d1 (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-dce1"))
    (write-app d1
      ["lib.clj" (string "(ns lib)\n"
                         "(defn used-helper [x] (* x 2))\n"
                         "(defn dead-never-called [x] (+ x 99999))\n"
                         "(defn also-used [x] (used-helper (inc x)))\n")
       "app.clj" (string "(ns app (:require [lib]))\n"
                         "(defn -main [& _] (println \"result\" (lib/also-used 10)))\n")])
    (def b1 (uber d1 "app"))
    (assert (not (has? b1 "dead-never-called")) "unreachable defn dropped from bundle")
    (assert (has? b1 "used-helper") "transitively-reached defn kept")
    (assert (has? b1 "also-used") "directly-reached defn kept")
    (assert (= "result 22" (run-bundle d1)) "pruned bundle runs identically")

    # --- soundness: a fn reached ONLY through a macro template is kept --------
    (def d2 (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-dce2"))
    (write-app d2
      ["mlib.clj" (string "(ns mlib)\n"
                          "(defn via-macro [x] (* x 3))\n"
                          "(defmacro tri [n] (list 'mlib/via-macro n))\n")
       "app2.clj" (string "(ns app2 (:require [mlib]))\n"
                          "(defn -main [& _] (println \"m\" (mlib/tri 7)))\n")])
    (def b2 (uber d2 "app2"))
    (assert (has? b2 "via-macro") "fn used only via a macro template is kept")
    (assert (= "m 21" (run-bundle d2)) "macro-reached bundle runs identically")

    # --- soundness: dynamic resolution disables DCE (keeps everything) --------
    (def d3 (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-dce3"))
    (write-app d3
      ["dlib.clj" (string "(ns dlib)\n"
                          "(defn looks-dead [x] (* x 5))\n")
       "app3.clj" (string "(ns app3 (:require [dlib]))\n"
                          "(defn -main [& _]\n"
                          "  (println \"d\" ((deref (resolve 'dlib/looks-dead)) 4)))\n")])
    (def b3 (uber d3 "app3"))
    (assert (has? b3 "looks-dead") "resolve in bundle disables DCE (keeps all defns)")
    (assert (= "d 20" (run-bundle d3)) "resolve bundle runs identically")

    # --- soundness: a fn reached only through a defmethod body is kept --------
    (def d4 (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-dce4"))
    (write-app d4
      ["mmlib.clj" (string "(ns mmlib)\n"
                           "(defmulti shape-area :kind)\n"
                           "(defn rect-helper [w h] (* w h))\n"
                           "(defmethod shape-area :rect [s] (rect-helper (:w s) (:h s)))\n"
                           "(defn really-dead [x] (+ x 1))\n")
       "app4.clj" (string "(ns app4 (:require [mmlib]))\n"
                          "(defn -main [& _] (println \"area\" (mmlib/shape-area {:kind :rect :w 3 :h 4})))\n")])
    (def b4 (uber d4 "app4"))
    (assert (has? b4 "rect-helper") "fn reached only via a defmethod body is kept")
    (assert (not (has? b4 "really-dead")) "truly-unreachable fn dropped alongside live multimethod code")
    (assert (= "area 12" (run-bundle d4)) "multimethod bundle runs identically")

    (print "uberscript-dce: all cases passed")))
