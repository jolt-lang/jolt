# Integration test: jolt.nrepl server + client over a real TCP/bencode wire.
#
# The server runs in a subprocess (`jolt nrepl PORT`) so the client (this
# process) isn't affected by the server's accept-loop fiber, which leaves the
# shared ctx's current-ns pointing at jolt.nrepl. The client uses the jolt.nrepl
# Clojure API, exercising both halves of the implementation.

(use ../../src/jolt/api)
(use ../../src/jolt/types)

(def port "17888")

# Watchdog: never let a hang stall CI — bail out after 30s.
(ev/spawn (ev/sleep 30) (eprint "nrepl-test: watchdog fired (possible hang)") (os/exit 1))

(print "Starting jolt.nrepl server subprocess on port " port " ...")
(def proc (os/spawn ["janet" "src/jolt/main.janet" "nrepl" port] :p {:out :pipe :err :pipe}))

# Wait until the server accepts connections (poll up to ~5s).
(var ready false)
(var tries 0)
(while (and (not ready) (< tries 50))
  (let [r (protect (net/connect "127.0.0.1" port))]
    (if (r 0) (do (:close (r 1)) (set ready true))
      (do (ev/sleep 0.1) (++ tries)))))
(assert ready "nREPL server did not start")

(def ctx (init))
(ctx-set-current-ns ctx "user")
(load-string ctx "(require '[jolt.nrepl])")
(load-string ctx (string "(def c (jolt.nrepl/connect {:port " port "}))"))

(defn ev [e] (eval-string ctx e))
(var fails 0)
(defn check [label expr expected]
  (let [got (ev expr)]
    (if (= got expected)
      (print "  ok   " label)
      (do (++ fails) (printf "  FAIL %s: want %q, got %q" label expected got)))))

# describe advertises ops
(check "describe has ops"
  "(boolean (get (first (jolt.nrepl/request c {\"op\" \"describe\"})) \"ops\"))" true)

# clone yields a session id
(ev "(def s (jolt.nrepl/client-clone c))")
(check "clone session is string" "(string? s)" true)

# eval returns a value
(check "eval (+ 1 2)" "(some #(get % \"value\") (jolt.nrepl/client-eval c \"(+ 1 2)\" s))" "3")

# a def's value renders as #'ns/name (pr-str loops on a var's cyclic ns refs)
(check "def renders as #'ns/name"
  "(some #(get % \"value\") (jolt.nrepl/client-eval c \"(def yy 21)\" s))" "#'user/yy")

# defs persist across evals in the session
(check "def then use" "(some #(get % \"value\") (jolt.nrepl/client-eval c \"(* yy 2)\" s))" "42")

# stdout is captured and streamed as an out message
(check "println captured as out"
  "(some #(get % \"out\") (jolt.nrepl/client-eval c \"(do (println \\\"hi\\\") 9)\" s))" "hi\n")

# the response carries the current ns
(check "ns field reported"
  "(some #(get % \"ns\") (jolt.nrepl/client-eval c \"(+ 1 1)\" s))" "user")

# in-ns switches the session ns, and it persists to the next eval
(check "in-ns switches ns"
  "(some #(get % \"ns\") (jolt.nrepl/client-eval c \"(in-ns (quote foo.bar))\" s))" "foo.bar")
(check "ns persists across evals"
  "(some #(get % \"ns\") (jolt.nrepl/client-eval c \"(+ 2 2)\" s))" "foo.bar")
# explicit :ns on the message overrides
(ev "(jolt.nrepl/request c {\"op\" \"eval\" \"code\" \"(+ 1 1)\" \"session\" s \"ns\" \"user\"})")

# eval error -> eval-error status, and the connection keeps working afterward
(check "eval error status"
  "(boolean (some #(let [st (get % \"status\")] (and (sequential? st) (some (fn [x] (= \"eval-error\" x)) st))) (jolt.nrepl/client-eval c \"(/ 1 :z)\" s)))"
  true)
(check "still alive after error"
  "(some #(get % \"value\") (jolt.nrepl/client-eval c \"(+ 5 5)\" s))" "10")

# multiple forms in one eval -> a value per form (values arrive as strings)
(check "multiple forms"
  "(= [\"2\" \"4\"] (mapv #(get % \"value\") (filter #(get % \"value\") (jolt.nrepl/client-eval c \"(+ 1 1) (+ 2 2)\" s))))"
  true)

# unknown op -> error/unknown-op/done
(check "unknown op status"
  "(let [st (get (first (jolt.nrepl/request c {\"op\" \"nope\"})) \"status\")] (and (some #(= \"unknown-op\" %) st) true))"
  true)

# clean up
(ev "(jolt.nrepl/client-close c)")
(os/proc-kill proc true)
(when (os/stat ".nrepl-port") (os/rm ".nrepl-port"))

(if (> fails 0)
  (do (eprint "nrepl-test: " fails " failing check(s)") (os/exit 1))
  (do (print "\nAll nREPL tests passed!") (os/exit 0)))
