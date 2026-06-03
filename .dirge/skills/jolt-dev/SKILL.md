---
name: jolt-dev
description: Jolt development workflow — build, test, special form patterns, Janet gotchas
---

# jolt-dev

Jolt development workflow — build, test, special form patterns, Janet gotchas

# Jolt Development

## Build & Test

```bash
cd /Users/yogthos/src/jolt
jpm build           # produces build/jolt
jpm test            # runs all tests
janet test/foo.janet  # run a single test file from project root
```

## Special Form Checklist

To add a new special form to the evaluator:

1. Add the name to `special-symbol?` in `src/jolt/evaluator.janet`
2. Add a match arm in `eval-list` (the match on `name`)
3. Add tests

The match arm receives `ctx`, `bindings`, and `form` (the full list). Use `(in form 1)` for first arg, etc.

**Non-symbol heads** (keywords, etc.): `eval-list` first checks `(and (struct? first-form) (= :symbol (...)))` before extracting `name`. If not a symbol, falls through to default function application.

### Current special forms (37):
`quote`, `syntax-quote`, `unquote`, `unquote-splicing`, `do`, `if`, `def`, `defmacro`, `fn*`, `let*`, `loop*`, `recur`, `throw`, `try`, `set!`, `var`, `locking`, `instance?`, `defmulti`, `defmethod`, `deftype`, `new`, `.`, `var-get`, `var-set`, `var?`, `alter-var-root`, `find-var`, `intern`, `alter-meta!`, `reset-meta!`, `disj`, `set?`, `protocol-dispatch`, `register-method`, `make-reified`, `satisfies?`

## Compiler Architecture

Two-phase: `analyze-form [form bindings ctx]` → `emit-ast` (string) or `emit-expr` (data structures).

**Why data structures:** Janet's `eval` can't see `use`-imported symbols. Embed function VALUES directly via `core-fn-values` table.

**eval-string dispatch** (compile mode): stateful forms → interpreter; everything else → `compile-and-eval`. Macros expand at analyze time.

## Protocol System

Protocols are maps with `:jolt/type :jolt/protocol` and `:methods` map.
Type registry in context env (`:type-registry`) maps `type-tag → proto-name → method-name → fn`.

**Special forms:**
- `protocol-dispatch [proto-sym method-sym obj rest-args]` — resolves method via type registry or reified methods
- `register-method [type-sym proto-sym method-sym fn-form]` — stores impl in type registry
- `make-reified [proto-sym methods-map]` — creates anonymous object with `:jolt/protocol-methods`

**Critical rule:** fn* form inside extend-type/extend-protocol MUST be `@[...]` (array) to trigger eval-list's special form dispatch. Tuples `[...]` hit `(tuple? form)` branch instead. Same for register-method, protocol-dispatch calls.

## REPL Collection Rendering (Buffer-Based)

Use `write-value` + `write-collection` with a StringBuffer (`@""`) rather than `prin`/`print` directly. Build the entire output string in a buffer, then atomically `(print (string buf))`. Prevents Janet's C runtime (in `jpm build` executables) from interleaving its native `<tuple 0x...>` printer between incremental `prin` calls.

```janet
(var write-value nil)  ; forward declaration

(defn- write-collection [v buf]
  (cond (tuple? v) (do (buffer/push-string buf "[") ...)
        (array? v) (do (buffer/push-string buf "(") ...) ...))

(set write-value (fn [v buf]
  (cond (nil? v) (buffer/push-string buf "nil")
        (number? v) (buffer/push-string buf (string v))
        (tuple? v) (write-collection v buf)
        true (buffer/push-string buf (string v)))))  ; true REQUIRED

(defn print-value [v] (def buf @"") (write-value v buf) (print (string buf)))
```

**Critical:** Janet's `cond` treats a bare expression in the last position as a **test** clause, not a catch-all body. Use `true` as the guard.

## PersistentHashMap Gotchas

- `core-map?`: `(if (and (table? x) (get x :jolt/deftype)) true false)` — `and` returns last truthy, not boolean
- `core-count`: subtract 1 for deftype tables (skip `:jolt/deftype` key)
- Equality: convert via `phm-to-struct` before `deep=`

## defrecord / deftype Patterns

- defrecord emits `(deftype TypeName [fields])` + arrow factory
- Records are tables with `:jolt/deftype` = type name string
- `set!` field mutation: `(set! (.-x obj) val)` parses as array with `.-x` symbol head

## Binding Macro

Uses `array-map` (plain Janet struct) not `hash-map` (PHM) to avoid PHM get() incompatibility with `var-get`.

## Tagged Literals (#inst, #uuid)

Use dynamic table construction: `(let [dr @{}] (put dr (keyword "#inst") fn) dr)`

## LazySeq Patterns

- Use `indexed?` not `tuple?` for realized sequences (may be arrays from `cons`/`concat`)
- Avoid `val'` (apostrophe in symbol names) — use `vf` instead

## Janet Gotchas

- `def` creates constants; use `(var x nil)` for mutable locals
- Bare tuples in `eval` are function calls: `[1 2 3]` tries to call `1`
- `try` format: `(try body ([err] handler))` NOT `(try body (catch sym handler))`
- core-renames MUST match actual fn names: `"-"` → `"core-sub"` (not `"core--"`)
- `(break val)` breaks from loop returning val — useful in bucket search patterns
- `boolean` doesn't exist — use `(if x true false)`
- Janet doesn't support Clojure-style multi-arity defn — use `[& args]` with `case (length args)`
- Janet's `cond` treats bare expression in last position as test, not catch-all — use `true` guard