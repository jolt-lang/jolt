# Smoke-test the command-line flags. Prefer the built binary (baked ctx, ~20ms
# startup); from-source is ~8s cold per invocation, and with ~20 invocations this
# file dominated the (parallel) gate's wall clock. Falls back to running
# main.janet from source when there's no binary, so an unbuilt tree still works —
# the gate (`jpm build && janet run-tests.janet`) builds first, so it's fresh.
(def- jolt-cmd
  (if (os/stat "build/jolt") ["build/jolt"] ["janet" "src/jolt/main.janet"]))

(defn- run [& args]
  (def p (os/spawn [;jolt-cmd ;args] :p {:out :pipe :err :pipe}))
  (def out (:read (p :out) :all))
  (os/proc-wait p)
  (string (or out "")))

(defn- run-err [& args]
  (def p (os/spawn [;jolt-cmd ;args] :p {:out :pipe :err :pipe}))
  (def err (:read (p :err) :all))
  (os/proc-wait p)
  (string (or err "")))

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



# --- user-facing error output (jolt-2o7 rounds 1+2) --------------------------
# Messages are Clojure-shaped; traces show the USER'S fns (compiled fn names
# carry ns/fn-name, demangled by report-error) and never jolt internals.
(check "arith error message rewritten"
       (run-err "-e" `(+ 1 "a")`)
       (has `Cannot add 1 and "a"`))
# unary arithmetic (inc/dec) on a non-number: the host error has no "or :r"
# clause, which used to crash the rewriter itself — handle it (jolt audit)
(check "unary arith error does not crash the rewriter"
       (run-err "-e" `(inc "x")`)
       (fn [s] (and (string/find "expects numbers" s)
                    (nil? (string/find "could not find method" s)))))
(check "arity error names the fn"
       (run-err "-e" "(defn afn [x] x) (afn 1 2)")
       (has "Wrong number of args (2) passed to: user/afn"))
(check "nil-call (nil value) keeps the hint"
       (run-err "-e" "(def x nil) (x 1)")
       (has "Cannot call nil as a function"))
# round 3: typos die at resolve time with Clojure's message, not as nil-calls
(check "unresolved symbol named at resolve time"
       (run-err "-e" "(undefined-fn 1)")
       (has "Unable to resolve symbol: undefined-fn in this context"))
(check "typo inside fn body also resolves to the message"
       (run-err "-e" "(defn f [] (no-such 1)) (f)")
       (has "Unable to resolve symbol: no-such"))
(check "trace shows the user's call chain"
       (run-err "-e" "(defn inner [x] (let [r (+ x :k)] r)) (defn outer [x] (let [v (inner x)] v)) (outer 1)")
       (fn [s] (and (string/find "at user/inner" s) (string/find "at user/outer" s))))
(check "no jolt-internal frames in user errors"
       (run-err "-e" `(+ 1 "a")`)
       (fn [s] (nil? (string/find "src/jolt/" s))))
# --- round 4: load errors carry file:line + the require chain ---------------
(def r4 (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-cli-r4-" (os/time)))
(os/mkdir r4) (os/mkdir (string r4 "/app"))
(spit (string r4 "/app/broken.clj") "(ns app.broken)\n\n(def config\n  (+ 1 \"boom\"))\n")
(spit (string r4 "/app/mid.clj") "(ns app.mid (:require [app.broken :as b]))\n")
(spit (string r4 "/app/top.clj") "(ns app.top (:require [app.mid :as m]))\n(defn -main [& a] nil)\n")
(os/setenv "JOLT_PATH" r4)
(def deep-err (run-err "-m" "app.top"))
(check "load error names the failing file:line" deep-err
       (has "at " ))
(check "load error points into broken.clj line 3" deep-err
       (has "/app/broken.clj:3"))
(check "require chain shows the loading path" deep-err
       (fn [s] (and (string/find "while loading" s) (string/find "/app/mid.clj" s) (string/find "/app/top.clj" s))))
(check "script errors name the script file" 
       (do (spit (string r4 "/scr.clj") "(ns scr)\n\n(+ 1 \"x\")\n")
           (run-err (string r4 "/scr.clj")))
       (has "/scr.clj:3"))
(check "no synthetic <eval> position on one-liners"
       (run-err "-e" `(+ 1 "a")`)
       (fn [s] (nil? (string/find "<eval>" s))))

# --- round 5: reader errors carry file:line:col ------------------------------
(check "unterminated string positions in -e"
       (run-err "-e" `(+ 1 "abc`)
       (has "Syntax error reading source at (<eval>:1:10): Unterminated string"))
(check "unterminated list names script file:line:col"
       (do (spit (string r4 "/syn.clj") "(ns syn)\n\n(defn f [x]\n  (+ x 1\n")
           (run-err (string r4 "/syn.clj")))
       (has ":5:1): Unterminated list"))
(check "unmatched delimiter positioned"
       (do (spit (string r4 "/app/synreq.clj") "(ns app.synreq)\n\n(def x ])\n")
           (spit (string r4 "/app/top2.clj") "(ns app.top2 (:require [app.synreq :as q]))\n(defn -main [& a] nil)\n")
           (os/setenv "JOLT_PATH" r4)
           (run-err "-m" "app.top2"))
       (fn [s] (and (string/find "/app/synreq.clj:3:8): Unmatched delimiter: ]" s)
                    (string/find "/app/top2.clj:1" s))))
(check "bad token positioned"
       (run-err "-e" "(def x ##Huh)")
       (has "Invalid symbolic value: ##Huh"))

(check "JOLT_DEBUG restores the raw trace"
       (do (os/setenv "JOLT_DEBUG" "1")
           (def r (run-err "-e" `(+ 1 "a")`))
           (os/setenv "JOLT_DEBUG" nil)
           r)
       (has "could not find method"))

# --- success checker default-on in direct-link, off in plain builds ----------
# A provably-wrong defn (never called, so no runtime error): the checker is the
# only thing that can flag it. Plain build = silent (no dev regression);
# direct-link build = warns by default (free piggyback on inference).
(def tcw (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-tcwarn-" (os/time) ".clj"))
(spit tcw "(ns tcw)\n\n(defn unused [s]\n  (inc \"definitely-not-a-number\"))\n")
(check "plain build does not run the checker (no regression)"
       (run-err tcw)
       (fn [s] (nil? (string/find "type error" s))))
(check "direct-link build warns by default (free checking)"
       (do (os/setenv "JOLT_DIRECT_LINK" "1")
           (def r (run-err tcw))
           (os/setenv "JOLT_DIRECT_LINK" nil)
           r)
       (fn [s] (and (string/find "type error" s)
                    (string/find "requires a number" s))))
(check "JOLT_TYPE_CHECK=off disables it even in direct-link"
       (do (os/setenv "JOLT_DIRECT_LINK" "1")
           (os/setenv "JOLT_TYPE_CHECK" "off")
           (def r (run-err tcw))
           (os/setenv "JOLT_DIRECT_LINK" nil)
           (os/setenv "JOLT_TYPE_CHECK" nil)
           r)
       (fn [s] (nil? (string/find "type error" s))))
# negative/never types (jolt-wwy): calling a non-function is reported by default
# in direct-link; wrong-arity to a user fn under the JOLT_TYPE_CHECK_USER opt-in
(def tcn (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-tcneg-" (os/time) ".clj"))
(spit tcn "(ns tcn)\n\n(defn nope []\n  (let [n 5] (n 1)))\n")
(check "direct-link reports calling a number as a function"
       (do (os/setenv "JOLT_DIRECT_LINK" "1")
           (def r (run-err tcn))
           (os/setenv "JOLT_DIRECT_LINK" nil)
           r)
       (has "cannot call a number as a function"))
(def tca (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-tcarity-" (os/time) ".clj"))
(spit tca "(ns tca)\n\n(defn f [x y] (+ x y))\n(defn g [] (f 1))\n")
(check "JOLT_TYPE_CHECK_USER reports wrong arity to a user fn"
       (do (os/setenv "JOLT_DIRECT_LINK" "1")
           (os/setenv "JOLT_TYPE_CHECK_USER" "1")
           (def r (run-err tca))
           (os/setenv "JOLT_DIRECT_LINK" nil)
           (os/setenv "JOLT_TYPE_CHECK_USER" nil)
           r)
       (has "wrong number of args (1) passed to `f` (expected 2)"))

(if (> fails 0)
  (error (string "cli-test: " fails " failing check(s)"))
  (print "\nAll CLI tests passed!"))
