---
triggers:
  - "compile jolt"
  - "jolt compiler"
  - "Clojure to Janet compilation"
  - "add new op to compiler"
  - "fix compiler"
---

# Jolt Compiler

Source-to-source Clojure→Janet compiler. Two-phase: analyze-form (classify + macro expand) → emit-ast (generate).

## Architecture

```
Clojure form → analyze-form [form bindings ctx] → AST {:op ...}
                               ↓ (if head = macro var)
                          expand → re-analyze expanded form
                    ↓
              emit-ast (source string) or emit-expr (data structure)
```

Three public entry points:
- `(compile-form form &opt ctx)` → Janet source string (debug/display)
- `(compile-ast form &opt ctx)` → Janet data structure (for eval)
- `(compile-and-eval form ctx)` → compile-ast + eval

## Why data structures, not source strings

Janet's `eval` does NOT have access to `use`-imported symbols from the calling file. `(eval "(core-inc 1)")` fails with "unknown symbol core-inc". The fix: emit Janet tuples where function VALUES are embedded: `[core-inc 1]`.

```
core-fn-values table:  "core-inc" → core-inc (the actual function)
emit-core-symbol-expr → (get core-fn-values janet-name)
```

Source-to-source (`compile-form` + `emit-ast`) still exists for debugging but is NOT used by `compile-and-eval`.

## Macro expansion

`analyze-form` checks whether the head symbol of a list resolves to a macro var before dispatching to special form handling:

1. Look up symbol in current ns → core ns via `resolve-macro`
2. If `var-macro?` is true, call `(var-get macro-var)` to get the fn
3. `(apply macro-fn (tuple/slice form 1))` to expand
4. `(analyze-form expanded ...)` to re-analyze the result

Macros expand at analyze time, before emission. `defn` expands to `(def name (fn* ...))`, `when` to `(if test (do ...) nil)`, etc.

## Symbol classification (in analyze-form)

Order: qualified ns → local binding → core-symbol → bare symbol

```
(if (form :ns) → :qualified-symbol
    (get bindings name) → :local
    (get core-renames name) → :core-symbol
    → :symbol)
```

core-renames MUST match actual fn names: `"-"` → `"core-sub"` (not `"core--"`), `"not"` → `"core-not"`. Verify against `core.janet` bindings.

## core-fn-values

Maps Janet string names to actual function values. Must be kept in sync with core-renames. When adding a new core fn, update BOTH tables.

Functions that need special mapping (name differs):
- `"apply"` → `apply` (Janet built-in)
- `"-"` → `"core-sub"` (not `core--`)
- `"some"` → `core-some?` (shared with `core-some?`)
- `"pr-str"` → `core-str` (alias)
- `"nth"` → `core-nth` (separate function, added in Phase 6)
- `"list"` → `core-list`, `"name"` → `core-name`, `"subs"` → `core-subs`

## Loop/recur compilation

`loop*` emits a self-referential closure:
```janet
(do (var _loop_N nil)
    (set _loop_N (fn [params] body))
    (_loop_N init-vals...))
```

`recur` saves `:loop-name` in the AST (looked up from bindings `:jolt/current-loop`), then `emit-recur-expr` rewrites to `(loop-name arg1 arg2...)`.

In the string emitter, recur similarly emits `(loop-name arg ...)`.

## Throw/try compilation

- `throw` → `(error val)` in Janet
- `try/catch` → `(try body ([err] handler-body))` — NOTE: Janet uses `([sym] handler)` format, NOT `(catch sym handler)`
- `try/finally` → appends do-block after catch clause in the Janet tuple

## Quote in data-structure emitter

Don't re-analyze quoted forms. Use `raw-form->janet` to pass Jolt reader forms through verbatim to Janet's `quote`:
```
(emit-quote-expr expr) → ['quote (raw-form->janet expr)]
```
raw-form->janet converts symbols to Janet symbols, arrays/tuples recursively.

## Remaining ops (interpreter only)

`syntax-quote`, `set!`, `deftype`, `defmulti`, `defmethod` — these are stateful or complex and always use the interpreter path even in compile mode.

## Stateful forms (must use interpreter, NOT compiler)

These forms modify context state and cannot be compiled:
- `defmacro`, `ns`, `deftype`, `defmulti`, `defmethod`, `require`, `in-ns`

Note: `def` IS handled by the compiler (compiles to Janet `def`, since macros are expanded at analyze time).

## eval-string dispatch (compile mode)

When `:compile?` is true, everything goes through the compiler EXCEPT stateful forms. Phase 0 fix: bare symbols and tuples also go through compile path (not just arrays).

Stateful forms (always use interpreter):
`defmacro`, `ns`, `deftype`, `defmulti`, `defmethod`, `require`, `in-ns`, `syntax-quote`, `set!`, `var`, `.`, `new`

Note: `def` is handled by the compiler — macros like `defn` expand to `(def name (fn* ...))` and `def` emits Janet `def`.

## compile-and-eval def/defn interning

`compile-and-eval` interns `def`/`defn`/`defn-` results in the Jolt namespace via `ns-intern` so the interpreter can resolve bare symbols later. Without this, `(defn foo [x] x)` creates the Janet global but `foo` as a bare symbol can't be found by the interpreter.

## Quote in data-structure emitter

Don't re-analyze quoted forms. Use `raw-form->janet` to pass Jolt reader forms through verbatim to Janet's `quote`. raw-form->janet now handles namespace-qualified symbols: `{:ns "foo" :name "bar"}` → `foo/bar` Janet symbol.

## Remaining ops (interpreter only)

`syntax-quote`, `set!`, `deftype`, `defmulti`, `defmethod`, `var`, `.`, `new` — these are stateful or complex and always use the interpreter path even in compile mode.

## Binding macro (Phase 1)

`core-binding` macro generates `(let [frame {var1 var1-obj var2 var2-obj ...}] (push-thread-bindings frame) (try (do body...) (finally (pop-thread-bindings))))`. Call forms inside the macro expansion MUST use `@[...]` arrays, not tuples — the evaluator treats tuples as literal vectors.

## Adding a new op

1. Add `analyze-form` match arm for the special form
2. Add `emit-ast` match arm (source string path)
3. Add `emit-expr` match arm (data structure path)
4. Add `core-renames` entry if it's a core fn (name → Janet string name)
5. Add `core-fn-values` entry (Janet string name → actual fn value)
6. Add tests in `test/compiler-test.janet`

## Phase 1: Var/Namespace system

- `types.janet`: `all-ns`, `remove-ns`, `create-ns`, `the-ns`, `ns-interns`, `ns-aliases`, `ns-imports-fn`
- `core.janet`: `core-binding` macro + `core-push-thread-bindings`/`core-pop-thread-bindings`
- `evaluator.janet`: ns accessor special forms, ns form extended with `:require`/`:refer`, `:use`, `:refer-clojure`/`:exclude`, `:import` clauses

## Test files

- `test/compiler-test.janet` — Phase 2-5 + Phase 0-1 tests (source output + compile-eval + macro + ns + defn tests)
- `test/phase6-final.janet` — Phase 6 comprehensive compile-mode tests (47 assertions)

Run: `janet test/compiler-test.janet` or `janet test/phase6-final.janet` or `jpm test`

- Source output tests: `(assert (= "(expected)" (compile-str "(input)")) "label")`
- Round-trip tests: `(assert (= val (compile-eval-str "(input)")) "label")`
- Compile flag tests: `(eval-string ctx "(input)")` with `{:compile? true}`

Run: `janet test/compiler-test.janet` or `jpm test`
