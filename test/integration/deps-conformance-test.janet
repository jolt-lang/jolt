# Conformance pass for deps.edn-loaded libraries: resolve a few real, pure-cljc
# git libraries and check that their namespaces load and a sample call works.
#
# Network-gated: set JOLT_CONFORMANCE=1 to run (it clones from GitHub). Skipped by
# default so CI stays offline. Findings are summarized in docs/tools-deps.md.

(use ../../src/jolt/api)
(use ../../src/jolt/types)
(import ../../src/jolt/deps :as deps)
(use ../../src/jolt/reader)
# deps are clj/cljc libraries by definition (the jolt-dw4 premise): read them
# under clj-compat features so their #?(:clj ...) branches resolve (spec
# 02-reader S18 — features are a property of the loading context).
(reader-features-set! ["jolt" "clj" "default"])

(unless (os/getenv "JOLT_CONFORMANCE")
  (print "deps-conformance: set JOLT_CONFORMANCE=1 to run (needs network) — skipped")
  (os/exit 0))

(def libs
  [{:name "medley" :url "https://github.com/weavejester/medley" :tag "1.4.0"
    :ns "medley.core" :check "(medley.core/find-first odd? [2 4 5])" :expect "5"}
   {:name "cuerdas" :url "https://github.com/funcool/cuerdas" :tag "2022.06.16-403"
    :ns "cuerdas.core" :check "(cuerdas.core/kebab \"helloWorld\")" :expect "hello-world"}
   {:name "dependency" :url "https://github.com/stuartsierra/dependency" :tag "dependency-1.0.0"
    :ns "com.stuartsierra.dependency"
    :check "(boolean (com.stuartsierra.dependency/graph))" :expect "true"}])

(def base (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-conf-" (os/time)))
(os/mkdir base)

(defn- try-lib [lib]
  (def dir (string base "/" (lib :name)))
  (os/mkdir dir)
  (spit (string dir "/deps.edn")
        (string "{:deps {the/lib {:git/url \"" (lib :url) "\" :git/tag \"" (lib :tag) "\"}}}"))
  (def prev (os/cwd))
  (defer (os/cd prev)
    (os/cd dir)
    (def r (protect (deps/resolve-deps "deps.edn")))
    (if (not (r 0))
      [:resolve-failed (string (r 1))]
      (let [ctx (init {:paths (r 1)})]
        (ctx-set-current-ns ctx "user")
        (def lr (protect (eval-string ctx (string "(require (quote [" (lib :ns) "]))"))))
        (if (not (lr 0))
          [:load-failed (string (lr 1))]
          (let [cr (protect (eval-string ctx (lib :check)))]
            (cond
              (not (cr 0)) [:check-error (string (cr 1))]
              (= (string (normalize-pvecs (cr 1))) (lib :expect)) [:ok nil]
              [:check-mismatch (string "got " (string (normalize-pvecs (cr 1))))])))))))

(print "deps conformance — pure-cljc git libs:\n")
(each lib libs
  (def [status detail] (try (try-lib lib) ([err] [:crash (string err)])))
  (printf "  %-12s %-16s %s" (lib :name) status (or detail "")))
(print "")
(os/exit 0)
