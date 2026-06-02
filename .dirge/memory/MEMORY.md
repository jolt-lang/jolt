Janet bit-shift: `blshift` (left), `brshift` (right), `brushift` (unsigned right). `brushift` on negative numbers errors (out of range for 32-bit unsigned). No `lshift`/`rshift`/`urshift`.
§
Janet `get` does NOT follow table prototypes set by `table/setproto`. Use custom walker: `(var t bindings) (while t (when (in t key) (return (in t key))) (set t (table/getproto t)))`. Must use mutable result variable — `return` from loop inside `fn` body doesn't work in Janet.
§
`instance?` extended for deftype: checks `:jolt/deftype` tag on struct instances. `set!` extended for field mutation: `(set! (. obj -field) val)` → `(put obj (keyword "field") val)`. Both patterns used by persistent vector implementation (`.arr`, `.cnt`, `.tail`, etc. access).
§
Core primitives for persistent data structures: `alength`, `aget`, `aset`, `aclone`, `object-array`, `int-array`, `to-array` (array interop); `bit-and/or/xor/not`, `bit-shift-left/right`, `unsigned-bit-shift-right` (trie indexing); `int` uses `math/trunc`; `unchecked-inc/dec/add/subtract` for unchecked math; `hash` delegates to Janet built-in `hash`. All registered in `core-bindings`.
§
`and`/`or` macros: `(and x y)` → `(let* [and__x x] (if and__x (and y) and__x))`. `(or x y)` → `(let* [or__x x] (if or__x or__x (or y)))`. Registered as macros in core-macro-names. `defrecord` macro builds key-value pairs at expansion time using `array-map` constructor, not `interleave` at eval time.
§
SCI added as git submodule at `vendor/sci` (https://github.com/borkdude/sci.git). Clone with `git submodule update --init`. 317/317 forms load from 9 core files. Internal namespaces (interop, parser, opts) partially loaded — parser.cljc needs `utils/new-var` (calls `sci.lang.Var.` constructor) and `edamame/normalize-opts` stubs.
