# Build-time collection mode.
#
# Jolt can be built with either immutable (persistent) collections — proper
# Clojure value semantics — or fast Janet-native mutable collections.
#
#   jpm build                 # immutable (default)
#   JOLT_MUTABLE=1 jpm build  # mutable
#
# This reads the environment at module-load time, so for a jpm-compiled
# executable the value is fixed when the binary is built (a true compile flag).
# `mutable?` is a constant, so the type-mode branches throughout core fold away.
(def mutable? (= "1" (os/getenv "JOLT_MUTABLE")))

# Convenience: immutable? is the default.
(def immutable? (not mutable?))

# ---------------------------------------------------------------------------
# Run-mode + cache-key policy (jolt-q5ql). Lifted out of main so it is unit-
# testable without the CLI and so the disk-cache keys share ONE canonical list
# of ctx-shaping env vars (a positional %q key silently misaligned if a knob was
# added in only one place — the footgun this fixes).
# ---------------------------------------------------------------------------

# Every env var that shapes the built/optimized ctx. Both image caches key on
# this exact list, so adding a knob here updates every cache key at once.
(def ctx-shaping-env-vars
  ["JOLT_PATH" "JOLT_MUTABLE" "JOLT_AOT_CORE" "JOLT_FEATURES"
   "JOLT_INTERPRET" "JOLT_INTERPRET_MACROS" "JOLT_DIRECT_LINK"
   "JOLT_NO_DIRECT_LINK" "JOLT_OPTIMIZE" "JOLT_WHOLE_PROGRAM"
   "JOLT_NO_WHOLE_PROGRAM" "JOLT_SHAPE" "JOLT_NO_SHAPE"
   "JOLT_NO_IR_PASSES" "JOLT_CHECK_HINTS"])

(defn ctx-cache-key
  "Build a disk-cache key from labeled prefix pairs plus the value of every
  ctx-shaping env var. Each component is tagged by name (`label=value`), so a
  new knob can't positionally alias two different builds. `prefix` is a flat
  array of label,value,label,value,... (e.g. version + entry ns + opts)."
  [prefix]
  (def parts @[])
  (var i 0)
  (while (< i (length prefix))
    (array/push parts (string/format "%s=%q" (in prefix i) (in prefix (+ i 1))))
    (+= i 2))
  (each ev ctx-shaping-env-vars
    (array/push parts (string/format "%s=%q" ev (os/getenv ev))))
  (string/join parts "|"))

(defn resolve-run-mode
  "Resolve the compile/optimization knobs from argv-derived flags + env.
  `open-mode?` = an interactive/non-program invocation (repl/-e/help/uberscript);
  `main-entry?` = a -m/-M program entry. Returns the ctx env knob map that main
  installs. Explicit env always wins (JOLT_NO_DIRECT_LINK / JOLT_DIRECT_LINK)."
  [open-mode? main-entry?]
  (def dl-forced
    (cond (os/getenv "JOLT_NO_DIRECT_LINK") :off
          (= "1" (os/getenv "JOLT_DIRECT_LINK")) :on
          :none))
  (def dl (case dl-forced :off false :on true (not open-mode?)))
  # Inference/specialization is the expensive part — default OFF, opt in with
  # JOLT_OPTIMIZE (or an explicit JOLT_DIRECT_LINK, which signals a full build).
  (def optimize?
    (and dl (or (not (nil? (os/getenv "JOLT_OPTIMIZE")))
                (= "1" (os/getenv "JOLT_DIRECT_LINK")))))
  {:direct-linking? dl
   :inline? optimize?
   # auto-enabled (vs explicitly requested) — suppresses the checker default-on.
   :direct-link-auto? (and dl (= dl-forced :none))
   :shapes? (and dl (not (os/getenv "JOLT_NO_SHAPE")))
   :map-shapes? (and (os/getenv "JOLT_SHAPE") (not (os/getenv "JOLT_NO_SHAPE")))
   :whole-program? (and optimize?
                        (not (os/getenv "JOLT_NO_WHOLE_PROGRAM"))
                        (or main-entry? (os/getenv "JOLT_WHOLE_PROGRAM")))})
