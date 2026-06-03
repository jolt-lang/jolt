# jolt-compiler

Jolt Compiler — Clojure→Janet source compiler with data-structure emission path.

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

## Symbol classification (in analyze-form)

Order: qualified ns → local binding → core-symbol → bare symbol

```
(if (form :ns) → :qualified-symbol
    (get bindings name) → :local
    (get core-renames name) → :core-symbol
    → :symbol)
```

## core-renames vs core-fn-values

core-renames maps Clojure name strings → Janet function name strings (used by both emitter paths).
core-fn-values maps Janet function name strings → actual function values (used by data-structure emitter only).

MUST keep both in sync. When adding a new core fn, update BOTH.
**Missing entries → symbol treated as unknown global, returns nil.**

Key name mappings:
- `"-"` → `"core-sub"` (NOT `"core--"`)
- `"apply"` → `apply` (Janet built-in)
- `"some"` → `core-some?` (shared with `core-some?`)
- `"pr-str"` → `core-str` (alias)
- `"list"` → `core-list`, `"name"` → `core-name`, `"subs"` → `core-subs`

## Loop/recur compilation

`loop*` emits a self-referential closure:
```janet
(do (var _loop_N nil)
    (set _loop_N (fn [params] body))
    (_loop_N init-vals...))
```

`recur` saves `:loop-name` in the AST (looked up from bindings `:jolt/current-loop`), then `emit-recur-expr` rewrites to `(loop-name arg1 arg2...)`.

## Throw/try compilation

- `throw` → `(error val)` in Janet
- `try/catch` → `(try body ([err] handler-body))` — Janet uses `([sym] handler)` format

## emi-vector-expr critical fix

**Bare tuples in Janet eval are function calls**: `[1 2 3]` tries to call `1` as a function. Always emit `['tuple ...]` or `(tuple ...)`.

## Quote in data-structure emitter

Use `raw-form->janet` to pass Jolt reader forms through verbatim to Janet's `quote`:
```
(emit-quote-expr expr) → ['quote (raw-form->janet expr)]
```
raw-form->janet converts Jolt symbols to Janet symbols, recursively for arrays/tuples.

## make-symbol fix (reader.janet)

`/` at position 0 means unqualified symbol. Use `(if (and slash (> slash 0)) ...)` — only split on `/` when it's not at the start.

## :data-readers key construction

`:data-readers` uses dynamic table construction because `:#inst` is invalid Janet keyword literal syntax (`:#` triggers reader macro):
```janet
:data-readers (let [dr @{}]
                (put dr (keyword "#inst") (fn [s] s))
                (put dr (keyword "#uuid") (fn [s] s))
                dr)
```

## eval-string dispatch (compile mode)

```janet
(if (or (= head-name "defmacro") (= head-name "ns")
        (= head-name "deftype") (= head-name "defmulti") (= head-name "defmethod")
        (= head-name "require") (= head-name "in-ns"))
  (eval-form ctx @{} form)     ; interpret
  (compile-and-eval form ctx)) ; compile
```

## Test files

- `test/compiler-test.janet` — Phases 0-4 tests (source output + compile-eval + macro tests)
- `test/phase6-final.janet` — Phase 6 comprehensive compile-mode tests (47 assertions)
- `test/phase5-test.janet` — Phase 5 multimethod + hierarchy tests
- `test/phase6-test.janet` — Phase 6 reader extension tests
- `test/hash-map-test.janet` — Phase 2 PersistentHashMap tests

Run: `janet test/<file>.janet` or `jpm test`