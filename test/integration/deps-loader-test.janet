# The loader resolves namespaces against an ordered list of source roots (the
# stdlib first, then deps.edn-resolved dirs), trying .clj then .cljc. This is the
# foundation for loading Clojure libraries via deps.edn — here we point a root at
# a hand-written "library" and require it.

(use ../../src/jolt/api)
(use ../../src/jolt/types)

(def tmp (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-deps-test-" (os/time)))
(os/mkdir tmp)
(os/mkdir (string tmp "/mylib"))
(os/mkdir (string tmp "/other"))

# a .cljc library with its own ns form and a require of a sibling ns
(spit (string tmp "/mylib/core.cljc")
      "(ns mylib.core (:require [other.util :as u]))\n(defn double [x] (* 2 x))\n(defn doubled-inc [x] (u/inc1 (double x)))\n")
# a .clj sibling, with a dash in the name (-> underscore in the path)
(os/mkdir (string tmp "/other"))
(spit (string tmp "/other/util.clj") "(ns other.util)\n(defn inc1 [x] (+ x 1))\n")

(def ctx (init {:paths [tmp]}))
(ctx-set-current-ns ctx "user")

(var fails 0)
(defn check [label expr expected]
  (let [r (protect (eval-string ctx expr))
        got (if (r 0) (normalize-pvecs (r 1)) (string "ERR " (r 1)))]
    (if (= got expected)
      (print "  ok   " label)
      (do (++ fails) (printf "  FAIL %s: want %q, got %q" label expected got)))))

# require a .cljc lib from the added root and call it
(check "require .cljc lib" "(do (require (quote [mylib.core :as m])) (m/double 21))" 42)
# transitive require (mylib.core requires other.util) resolved from the root too
(check "transitive .clj require" "(mylib.core/doubled-inc 10)" 21)
# the stdlib still resolves from its default root
(check "stdlib still loads" "(do (require (quote [jolt.interop :as j])) (j/janet-type 1))" :number)

# clean up
(defn rmrf [p]
  (if (= :directory (os/stat p :mode))
    (do (each e (os/dir p) (rmrf (string p "/" e))) (os/rmdir p))
    (os/rm p)))
(rmrf tmp)

(if (> fails 0)
  (error (string "deps-loader-test: " fails " failing check(s)"))
  (print "\nAll deps-loader tests passed!"))
