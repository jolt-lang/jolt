# -e-capable jolt-chez (jolt-9ziu): the Option-2 back end as a runnable CLI.
#
# Analysis runs on Janet (the portable analyzer); EXECUTION runs on Chez with the
# full clojure.core assembled as a Scheme prelude (driver/emit-core-prelude). The
# prelude is assembled once and cached on disk keyed by a fingerprint of the core
# sources + the Chez RT/emitter, so repeated invocations (e.g. the run-corpus.janet
# gate, one subprocess per case) reuse it.
#
# Usage (the run-corpus.janet boundary):  jolt-chez -e "EXPR"
# Run from the repo root (the prelude loads host/chez/rt.ss by relative path).
(import ../../src/jolt/api :as api)
(import ./driver :as d)

(defn- fingerprint []
  # Hash the inputs that shape the prelude: the core tiers + the emitter + the
  # Chez RT shims. Any change invalidates the cached prelude.
  (def parts @[])
  (each tf d/core-tier-files
    (array/push parts (slurp (string "jolt-core/clojure/core/" tf ".clj"))))
  (each f ["host/chez/emit.janet" "host/chez/driver.janet" "host/chez/rt.ss"
           "host/chez/values.ss" "host/chez/collections.ss" "host/chez/seq.ss"
           "host/chez/atoms.ss" "host/chez/predicates.ss" "host/chez/regex.ss"
           "host/chez/ns.ss" "host/chez/post-prelude.ss" "host/chez/natives-meta.ss"
           "host/chez/natives-str.ss" "host/chez/records.ss"
           "host/chez/host-class.ss" "host/chez/io.ss"
           "host/chez/inst-time.ss" "host/chez/reader.ss" "host/chez/math.ss"
           "host/chez/host-static.ss" "host/chez/dot-forms.ss"
           "src/jolt/clojure/string.clj" "src/jolt/clojure/walk.clj"
           "src/jolt/clojure/template.clj" "src/jolt/clojure/edn.clj"
           "src/jolt/clojure/set.clj" "src/jolt/clojure/pprint.clj"]
    (array/push parts (slurp f)))
  (string/slice (string (hash (string/join parts))) 0))

(defn- ensure-prelude [ctx]
  (def dir (or (os/getenv "JOLT_IMAGE_CACHE_DIR") (os/getenv "TMPDIR") "/tmp"))
  (def path (string dir "/jolt-chez-prelude-" (fingerprint) ".ss"))
  (unless (os/stat path)
    (def [scm _ _] (d/emit-core-prelude ctx))
    (spit path scm))
  path)

(defn main [& argv]
  # argv: [script "-e" EXPR]
  (def args (drop 1 argv))
  (unless (and (= (length args) 2) (= (first args) "-e"))
    (eprint "usage: jolt-chez -e EXPR")
    (os/exit 2))
  (def src (in args 1))
  (def ctx (api/init-cached {:compile? true}))
  # late-bind unresolved symbols (no interpreter to punt to) so defmulti/defmethod
  # forward references lower to a var-deref (jolt-9ls5), matching d/make-ctx.
  (put (get ctx :env) :late-bind-unresolved? true)
  (def prelude-path (ensure-prelude ctx))
  (def [code out err] (d/eval-e-with-prelude ctx src prelude-path))
  (when (= code :emit-err)
    (eprint "jolt-chez: cannot compile: " out)
    (os/exit 1))
  (unless (= "" out) (print out))
  (unless (= "" err) (eprint err))
  (os/exit code))
