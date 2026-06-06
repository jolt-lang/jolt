# The .clj stdlib (clojure.string, clojure.set, jolt.interop, …) is baked into the
# image at build time, so it loads even when the files aren't on disk. We simulate
# the shipped-binary-elsewhere case by clearing the filesystem source roots, so a
# require can only be satisfied by the embedded copy.

(use ../../src/jolt/api)
(use ../../src/jolt/types)

(def ctx (init))
(ctx-set-current-ns ctx "user")
(put (ctx :env) :source-paths @[])   # no FS roots — embedded fallback only

(var fails 0)
(defn check [label expr expected]
  (let [r (protect (eval-string ctx expr))
        got (if (r 0) (normalize-pvecs (r 1)) (string "ERR " (r 1)))]
    (if (= got expected) (print "  ok   " label)
      (do (++ fails) (printf "  FAIL %s: want %q, got %q" label expected got)))))

(assert (> (length (get (ctx :env) :embedded-sources)) 0) "embedded-sources should be populated")

(check "clojure.string from embedded"
  "(do (require (quote [clojure.string :as s])) (s/upper-case \"hi\"))" "HI")
(check "clojure.set from embedded"
  "(do (require (quote [clojure.set :as set])) (vec (set/union #{1} #{2})))" [2 1])
(check "clojure.walk from embedded"
  "(do (require (quote [clojure.walk :as w])) (w/keywordize-keys {\"a\" 1}))" {:a 1})
(check "jolt.interop from embedded"
  "(do (require (quote [jolt.interop :as j])) (j/janet-type 1))" :number)

(if (> fails 0)
  (error (string "embedded-stdlib-test: " fails " failing check(s)"))
  (print "\nAll embedded-stdlib tests passed!"))
