# The deps-image cache must not drop -main's command-line args under
# whole-program (jolt-4mui). A cache HIT swaps the running ctx for the saved
# image; *command-line-args* must be (re)bound from the CURRENT invocation's
# argv, not the stale value baked into the image. Drives the built binary:
# first run builds the cache (arg "first"), second run (cache hit) passes "second"
# — both must echo their own arg. Skips if build/jolt is absent.
(def repo (os/cwd))
(def jolt (string repo "/build/jolt"))
(if-not (os/stat jolt)
  (do (print "deps-cache-args: SKIP (no build/jolt)") (os/exit 0)))

(def dir (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-dca-" (os/time)))
(os/mkdir dir)
(spit (string dir "/echoargs.clj")
      "(ns echoargs)\n(defn -main [& args] (println \"GOT\" (pr-str (vec args))))\n")
# fresh per-run image cache dir so the first run is a guaranteed miss
(def imgdir (string dir "/imgcache")) (os/mkdir imgdir)

(each [k v] [["JOLT_DIRECT_LINK" "1"] ["JOLT_WHOLE_PROGRAM" "1"]
             ["JOLT_PATH" dir] ["JOLT_APP_PATHS" dir] ["JOLT_IMAGE_CACHE_DIR" imgdir]]
  (os/setenv k v))
(defn- run [arg]
  (def p (os/spawn [jolt "-m" "echoargs" arg] :p {:out :pipe :err :pipe}))
  (def out (:read (p :out) :all))
  (os/proc-wait p)
  (string (or out "")))

(var fails 0)
(defn check [label got want]
  (if (string/find want got) (print "  ok   " label)
    (do (++ fails) (printf "  FAIL %s: want %p in %p" label want got))))

(def r1 (run "first"))   # cache MISS — builds + saves the image
(check "first run passes its arg" r1 `GOT ["first"]`)
(def r2 (run "second"))  # cache HIT — must use the NEW arg, not the baked one
(check "second run (cache hit) passes its arg, not the baked one" r2 `GOT ["second"]`)

(defn rmrf [p] (when (os/stat p) (if (= :directory (os/stat p :mode)) (do (each e (os/dir p) (rmrf (string p "/" e))) (os/rmdir p)) (os/rm p))))
(rmrf dir)
(if (> fails 0) (do (printf "deps-cache-args: %d FAILED" fails) (os/exit 1))
  (print "deps-cache-args (jolt-4mui) passed!"))
