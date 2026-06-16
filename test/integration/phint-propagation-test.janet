# Declared record param hints propagate THROUGH the whole-program fixpoint
# (jolt-3ko). A ^Record param's field reads type to the field's record type, and
# when such a field-read value is passed to a SHARED helper (no hints, inferred
# from call sites), the helper's params must pick up that record type so its own
# field reads bare-index. Before the fix, ^-hints were applied only at the final
# re-emit (reinfer-def), not during the fixpoint, so a hinted param with no
# callers stayed :any during inference and never propagated to its callees —
# exactly why the ray tracer's vec ops (called with (:origin ray) etc.) stayed
# unproven even under whole-program optimization.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/types :as types)

(print "phint propagation through the fixpoint (jolt-3ko)...")

(def dir (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-phint-prop"))
(os/mkdir dir)
(spit (string dir "/pp.clj")
      (string
       "(ns pp)\n"
       "(defrecord Vec3 [r g b])\n"
       "(defrecord Ray [^Vec3 origin ^Vec3 direction])\n"
       # shared ops, NO hints — must be inferred from the call sites below. A
       # `loop` makes them inline-INELIGIBLE so they stay real calls (a small fn
       # would be spliced into the caller, where the caller's hint already proves
       # it); the point here is propagation to a separate callee's PARAMS.
       "(defn dot [a b]\n"
       "  (loop [i 0 s 0.0]\n"
       "    (if (< i 1) (recur (inc i) (+ (+ (* (:r a) (:r b)) (* (:g a) (:g b))) (* (:b a) (:b b)))) s)))\n"
       "(defn add [a b]\n"
       "  (loop [i 0 s nil]\n"
       "    (if (< i 1) (recur (inc i) (->Vec3 (+ (:r a) (:r b)) (+ (:g a) (:g b)) (+ (:b a) (:b b)))) s)))\n"
       # callers pass field-read Vec3s from a ^Ray param into the shared ops
       "(defn use-dot [^Ray r] (dot (:origin r) (:direction r)))\n"
       "(defn use-add [^Ray r] (add (:origin r) (:direction r)))\n"))

(os/setenv "JOLT_DIRECT_LINK" "1")
(os/setenv "JOLT_WHOLE_PROGRAM" "1")
(os/setenv "JOLT_PATH" dir)
(def ctx (api/init {:compile? true}))
(api/eval-string ctx "(require '[pp])")
(def report (backend/infer-program! ctx))

(def pns (types/ctx-find-ns ctx "jolt.passes"))
(def reinfer (types/var-get (types/ns-find pns "reinfer-def")))
(def ppns (types/ctx-find-ns ctx "pp"))
(defn ptmap-for [fname params]
  (def pts (get report (string "pp/" fname)))
  (def m @{}) (when pts (var i 0) (each p params (put m p (get pts i)) (++ i))) m)
(defn guards [fname params]
  (def cell (get (ppns :mappings) fname))
  (length (string/find-all ":jolt/type"
    (string/format "%p" (backend/emit-ir ctx (reinfer (get cell :infer-ir) (ptmap-for fname params)))))))

(var fails 0)
(defn check [label got expected]
  (if (= got expected) (print "  ok   " label)
    (do (++ fails) (printf "  FAIL %s: want %p got %p" label expected got))))

# the shared ops' params get the Vec3 record type from the field-read call args,
# so all their field reads bare-index (no :jolt/type guard)
(check "dot params typed Vec3 -> reads bare" (guards "dot" ["a" "b"]) 0)
(check "add params typed Vec3 -> reads bare" (guards "add" ["a" "b"]) 0)
# the hinted callers were already fine (re-emit applies the phint)
(check "use-dot hinted reads bare" (guards "use-dot" ["r"]) 0)

# the report shows the shared ops' params as a Vec3 struct (has :shape / :type)
(def dot-a (get (get report "pp/dot") 0))
(check "dot param a is a struct type" (truthy? (and dot-a (or (get dot-a :shape) (get dot-a :struct)))) true)

# correctness: results are unchanged
(check "dot computes" (api/eval-string ctx "(pp/dot (pp/->Vec3 1 2 3) (pp/->Vec3 4 5 6))") 32)
(check "use-dot computes"
       (api/eval-string ctx "(pp/use-dot (pp/->Ray (pp/->Vec3 1 2 3) (pp/->Vec3 4 5 6)))") 32)

(if (> fails 0) (do (printf "phint-propagation: %d FAILED" fails) (os/exit 1))
  (print "phint propagation (jolt-3ko) passed!"))
