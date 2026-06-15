(use ../../src/jolt/config)

# ============================================================
# resolve-run-mode (jolt-q5ql)
# ============================================================

# Save/restore the env vars these tests poke at.
(def touched ["JOLT_DIRECT_LINK" "JOLT_NO_DIRECT_LINK" "JOLT_OPTIMIZE"
              "JOLT_WHOLE_PROGRAM" "JOLT_NO_WHOLE_PROGRAM" "JOLT_SHAPE"
              "JOLT_NO_SHAPE"])
(def saved (table ;(mapcat (fn [k] [k (os/getenv k)]) touched)))
(defn clear-env [] (each k touched (os/setenv k nil)))
(defn restore-env [] (each k touched (os/setenv k (get saved k))))

(clear-env)

# Interactive (open) mode: no direct-linking, no optimization.
(let [m (resolve-run-mode true false)]
  (assert (= false (m :direct-linking?)) "open mode: not direct-linked")
  (assert (= false (m :inline?)) "open mode: not inlined")
  (assert (not (m :whole-program?)) "open mode: no whole-program"))

# Program entry (-m): direct-links by default, but optimization stays OFF unless
# opted in, so whole-program is off too.
(let [m (resolve-run-mode false true)]
  (assert (= true (m :direct-linking?)) "program run: direct-linked")
  (assert (= false (m :inline?)) "program run: optimization off by default")
  (assert (m :direct-link-auto?) "program run: direct-link auto flag set")
  (assert (not (m :whole-program?)) "program run: no whole-program without optimize"))

# JOLT_OPTIMIZE turns on inlining; a -m entry then enables whole-program.
(os/setenv "JOLT_OPTIMIZE" "1")
(let [m (resolve-run-mode false true)]
  (assert (m :inline?) "JOLT_OPTIMIZE: inlining on")
  (assert (m :whole-program?) "JOLT_OPTIMIZE + main entry: whole-program on"))
(os/setenv "JOLT_OPTIMIZE" nil)

# Explicit env wins: JOLT_NO_DIRECT_LINK forces open even for a program entry.
(os/setenv "JOLT_NO_DIRECT_LINK" "1")
(let [m (resolve-run-mode false true)]
  (assert (= false (m :direct-linking?)) "JOLT_NO_DIRECT_LINK forces open")
  (assert (not (m :direct-link-auto?)) "forced-off is not auto"))
(os/setenv "JOLT_NO_DIRECT_LINK" nil)

# JOLT_DIRECT_LINK forces direct-linking + optimization on even in open mode.
(os/setenv "JOLT_DIRECT_LINK" "1")
(let [m (resolve-run-mode true false)]
  (assert (m :direct-linking?) "JOLT_DIRECT_LINK forces direct-link in open mode")
  (assert (m :inline?) "JOLT_DIRECT_LINK implies optimization")
  (assert (not (m :direct-link-auto?)) "explicit direct-link is not auto"))
(os/setenv "JOLT_DIRECT_LINK" nil)

# ============================================================
# ctx-cache-key footgun (jolt-q5ql): the key must change when ANY ctx-shaping
# env var changes, and stay stable otherwise.
# ============================================================

(clear-env)
(def k0 (ctx-cache-key [:v "1" :ns "app"]))
(assert (= k0 (ctx-cache-key [:v "1" :ns "app"])) "same inputs -> same key")
(assert (not= k0 (ctx-cache-key [:v "2" :ns "app"])) "prefix change -> different key")

# Every ctx-shaping env var must participate (this is the regression that the
# old positional key could miss): flipping each one alone changes the key.
(each ev ctx-shaping-env-vars
  (os/setenv ev "X")
  (assert (not= k0 (ctx-cache-key [:v "1" :ns "app"]))
          (string "ctx-cache-key ignores " ev " — cache-key footgun"))
  (os/setenv ev nil))

(restore-env)

(print "config-test: all assertions passed")
