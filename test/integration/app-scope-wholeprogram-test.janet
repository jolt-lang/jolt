# Whole-program inference scoped to app namespaces (jolt-87e).
#
# Auto-whole-program (a -m program run under direct-link) used to defer EVERY
# loaded namespace — including every transitive dependency — into one closed-
# world fixpoint, which is prohibitive on dep-heavy apps (hundreds of dep nses;
# a ~2-minute cold start on malli). With app source roots declared (JOLT_APP_PATHS
# / jolt-deps, here :app-paths), only the app's OWN namespaces join the whole-
# program batch; dependency namespaces skip inference (they stay direct-linked
# but generically typed — the open-world default). With NO app roots declared,
# every namespace is treated as app (whole-program over everything, pre-87e).

(use ../../src/jolt/api)

(var failures 0)
(defn- check [label got want]
  (unless (= got want)
    (++ failures)
    (printf "FAIL [%s] got %q want %q" label got want)))

# Lay down an app root and a dep root under a tmp dir.
(def tmp (string (os/getenv "TMPDIR") "jolt-87e-" (os/time)))
(def app-root (string tmp "/app"))
(def dep-root (string tmp "/dep"))
(os/mkdir tmp) (os/mkdir app-root) (os/mkdir dep-root)
(spit (string dep-root "/mydep.clj")
      "(ns mydep)\n(defn helper [x] (+ x 1))\n")
(spit (string app-root "/myapp.clj")
      "(ns myapp (:require [mydep]))\n(defn run [x] (mydep/helper x))\n")

(defn- with-wp [f]
  (def saved (os/getenv "JOLT_WHOLE_PROGRAM"))
  (os/setenv "JOLT_WHOLE_PROGRAM" "1")
  (defer (os/setenv "JOLT_WHOLE_PROGRAM" saved) (f)))

# --- app roots declared: only the app ns defers into the batch ---------------
(with-wp
  (fn []
    (let [ctx (init {:compile? true :direct-linking? true
                     :paths [app-root dep-root] :app-paths [app-root]})]
      (check "whole-program is on" (truthy? (get (ctx :env) :whole-program?)) true)
      (eval-string ctx "(require '[myapp])")
      (let [deferred (or (get (ctx :env) :inferred-nses) @[])]
        (check "app ns deferred to batch" (truthy? (index-of "myapp" deferred)) true)
        (check "dep ns NOT in batch (per-ns inferred)"
               (truthy? (index-of "mydep" deferred)) false))
      # still runs correctly after the (scoped) whole-program pass
      (when-let [ip (get (ctx :env) :infer-program!)] (protect (ip ctx)))
      (check "scoped whole-program program still correct"
             (eval-string ctx "(myapp/run 41)") 42))))

# --- no app roots declared: every ns defers (pre-87e whole-program) ----------
(with-wp
  (fn []
    (let [ctx (init {:compile? true :direct-linking? true
                     :paths [app-root dep-root]})]
      (eval-string ctx "(require '[myapp])")
      (let [deferred (or (get (ctx :env) :inferred-nses) @[])]
        (check "no app roots: app ns deferred" (truthy? (index-of "myapp" deferred)) true)
        (check "no app roots: dep ns ALSO deferred"
               (truthy? (index-of "mydep" deferred)) true))
      (when-let [ip (get (ctx :env) :infer-program!)] (protect (ip ctx)))
      (check "unscoped whole-program still correct"
             (eval-string ctx "(myapp/run 9)") 10))))

(if (pos? failures)
  (do (printf "app-scope-wholeprogram: %d failure(s)" failures) (os/exit 1))
  (print "app-scope-wholeprogram: all cases passed"))
