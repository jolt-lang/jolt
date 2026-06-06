# Smoke-test the command-line flags by running main.janet from source (no build
# needed, so it can't go stale).

(defn- run [& args]
  (def p (os/spawn ["janet" "src/jolt/main.janet" ;args] :p {:out :pipe :err :pipe}))
  (def out (:read (p :out) :all))
  (os/proc-wait p)
  (string (or out "")))

(var fails 0)
(defn check [label got pred]
  (if (pred got) (print "  ok   " label)
    (do (++ fails) (printf "  FAIL %s: got %q" label got))))

(defn- has [sub] (fn [s] (string/find sub s)))

(check "--version"  (run "--version")            (has "jolt v"))
(check "version"    (run "version")              (has "jolt v"))
(check "--help"     (run "--help")               (has "Usage"))
(check "help lists nrepl-server" (run "help")    (has "nrepl-server"))
(check "-e"         (run "-e" "(+ 1 2)")         (has "3"))
(check "--eval"     (run "--eval" "(* 6 7)")     (has "42"))

# -m requires a namespace and calls its -main with the remaining args
(def tmp (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-cli-" (os/time)))
(os/mkdir tmp) (os/mkdir (string tmp "/app"))
(spit (string tmp "/app/core.clj")
      "(ns app.core)\n(defn -main [& args] (println \"MAIN\" (count args)))\n")
(os/setenv "JOLT_PATH" tmp)
(check "-m calls -main with args" (run "-m" "app.core" "a" "b") (has "MAIN 2"))
(os/setenv "JOLT_PATH" nil)
(os/rm (string tmp "/app/core.clj")) (os/rmdir (string tmp "/app")) (os/rmdir tmp)

(if (> fails 0)
  (error (string "cli-test: " fails " failing check(s)"))
  (print "\nAll CLI tests passed!"))
