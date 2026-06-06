# deps.edn resolution into source roots, then loading a library through them.
# Uses :local/root deps only so it needs no network (the git path is the same
# code, exercised manually against real repos).

(use ../../src/jolt/api)
(use ../../src/jolt/types)
(import ../../src/jolt/deps :as deps)

(def base (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-deps-resolve-" (os/time)))
(defn rmrf [p]
  (when (os/stat p)
    (if (= :directory (os/stat p :mode))
      (do (each e (os/dir p) (rmrf (string p "/" e))) (os/rmdir p))
      (os/rm p))))
(rmrf base)

(defn mkdirs [p]
  (def abs (string/has-prefix? "/" p))
  (var acc nil)
  (each seg (filter |(not= "" $) (string/split "/" p))
    (set acc (cond (nil? acc) (if abs (string "/" seg) seg) (string acc "/" seg)))
    (unless (os/stat acc) (os/mkdir acc))))

# project -> depends on lib-a (local), which depends on lib-b (local, transitive)
(each d ["proj/src" "a/src/liba" "b/src/libb"] (mkdirs (string base "/" d)))
(spit (string base "/proj/deps.edn") `{:paths ["src"] :deps {my/a {:local/root "../a"}}}`)
(spit (string base "/a/deps.edn") `{:paths ["src"] :deps {my/b {:local/root "../b"}}}`)
(spit (string base "/b/deps.edn") `{:paths ["src"]}`)
(spit (string base "/a/src/liba/core.clj") "(ns liba.core (:require [libb.core :as b]))\n(defn val [] (b/n))\n")
(spit (string base "/b/src/libb/core.clj") "(ns libb.core)\n(defn n [] 99)\n")

(var fails 0)
(defn check [label got expected]
  (if (= got expected) (print "  ok   " label)
    (do (++ fails) (printf "  FAIL %s: want %q, got %q" label expected got))))

(os/cd (string base "/proj"))
(def roots (deps/resolve-deps "deps.edn" (string base "/proj/jpm_tree")))
# roots: proj/src, a/src (transitive: b/src) — at least the two dep srcs present
(check "resolves transitive local dep roots"
  (truthy? (and (some |(string/has-suffix? "/a/src" $) roots)
                (some |(string/has-suffix? "/b/src" $) roots)))
  true)

# load through the resolved roots: liba.core requires libb.core transitively
(def ctx (init {:paths roots}))
(ctx-set-current-ns ctx "user")
(let [r (protect (eval-string ctx "(do (require (quote [liba.core :as a])) (a/val))"))]
  (check "require local lib + transitive dep" (if (r 0) (r 1) (string "ERR " (r 1))) 99))

# cached resolution returns the same roots without re-walking
(check "cached resolve matches"
  (deep= roots (deps/resolve-deps-cached "deps.edn" (string base "/proj/jpm_tree")))
  true)

(os/cd "/")
(rmrf base)

(if (> fails 0)
  (error (string "deps-resolve-test: " fails " failing check(s)"))
  (print "\nAll deps-resolve tests passed!"))
