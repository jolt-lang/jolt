# Ahead-of-time images for compiled namespaces.
#
# Compile-by-default turns each form into Janet bytecode at load time. AOT skips
# that work on subsequent runs by serializing a namespace's compiled vars to a
# bytecode image and loading them back.
#
# The trick is the marshal dictionary. A compiled jolt function closes over core
# fns (core-map, +, …) and var cells; those core fns are Janet cfunctions/closures
# that can't be marshaled by value. But the runtime env that holds them is baked
# into the binary and is byte-for-byte identical at save and load time, so we
# marshal *against* it: core fns are referenced by name, and only the user's
# bytecode plus its var cells are actually serialized.

(use ./compiler)   # jolt-runtime-env
(use ./types)

# Forward dict (key -> value) for unmarshal; reverse (value -> key) for marshal.
# Built from the runtime env, which chains to the Janet boot env, so both core fns
# and Janet builtins resolve by name.
(defn- fwd-dict [] (env-lookup jolt-runtime-env))
(defn- rev-dict [] (invert (env-lookup jolt-runtime-env)))

(defn marshal-ns
  "Marshal namespace `ns-name`'s var mappings to a byte buffer. The whole mappings
  table is marshaled in one call so var cells shared between defs stay shared."
  [ctx ns-name]
  (marshal ((ctx-find-ns ctx ns-name) :mappings) (rev-dict)))

(defn unmarshal-ns!
  "Install mappings produced by marshal-ns into `ns-name` in ctx, overwriting
  same-named vars. Returns ns-name."
  [ctx ns-name bytes]
  (let [mappings (unmarshal bytes (fwd-dict))
        ns (ctx-find-ns ctx ns-name)]
    (each [sym v] (pairs mappings) (put (ns :mappings) sym v))
    ns-name))

(defn save-ns
  "Write an AOT image of compiled namespace `ns-name` to `path`."
  [ctx ns-name path]
  (spit path (marshal-ns ctx ns-name)))

(defn load-ns-image
  "Read an AOT image written by save-ns back into ctx under `ns-name`. Skips
  parse/analyze/emit/compile entirely — the bytecode is already built."
  [ctx ns-name path]
  (unmarshal-ns! ctx ns-name (slurp path)))
