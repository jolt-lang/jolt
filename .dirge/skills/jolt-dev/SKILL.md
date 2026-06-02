---
description: Jolt development workflow — build, test, special form patterns, Janet gotchas
---

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
3. Add tests in `test/evaluator-test.janet`

The match arm receives `ctx`, `bindings`, and `form` (the full list). Use `(in form 1)` for first arg, etc.

**Non-symbol heads** (keywords, etc.): `eval-list` first checks `(and (struct? first-form) (= :symbol (...)))` before extracting `name`. If not a symbol, falls through to default function application. This means `(:ns &env)` works because the head `:ns` is a keyword, not a symbol, so it's evaluated and called as a lookup.

### Current special forms (22):
`quote`, `syntax-quote`, `unquote`, `unquote-splicing`, `do`, `if`, `def`, `defmacro`, `fn*`, `let*`, `loop*`, `recur`, `throw`, `try`, `set!`, `var`, `locking`, `instance?`, `defmulti`, `defmethod`, `deftype`, `new`, `.`

### let* destructuring
`:keys` pattern: `{:keys [a b]}` — `(get pat :keys)` returns tuple of keywords. Use `(string k)` to convert keyword to string name, `(keyword kname)` for lookup.
Sequential: `[a b c]` — indexed pattern, iterate with `(length pat)`, bind each position via `(in pat di)`
Plain symbol: bind name directly with `bind-put`

### let macro (→ let*)
Macro `let` expands to `let*`: `(let [bindings] body)` → `(let* [bindings] body)`
Registered as macro in `core-macro-names` so evaluator expands before `()let*` special-form dispatch.
Needed because SCI/clojure.core use `let` not `let*`.

### Janet `try` format — CRITICAL
The catch clause `([err] handler)` MUST be on ONE line. Multi-line causes "unexpected closing delimiter )" parse errors.
```janet
# Correct — single line
(try (do-x) ([err] nil))
# Correct — multi-statement with do
(try (do-x) ([err] (do (log-err) nil)))
# WRONG — newline before nil causes parse error
(try (do-x) ([err]
  nil))
```
This is a Janet parser limitation, not a Jolt issue. The `([err] ...)` clause is a tuple literal and Janet requires it on one line.

### Macro registration pattern
- Define macro fn in `core.janet` → add to `core-bindings` map → add to `core-macro-names` table
- Order matters: `core-macro-names` is a literal referencing fn names, so fns must be defined BEFORE the def- map

### Namespace capture for closures
`fn*` and `defmacro` capture `defining-ns` from `(ctx-current-ns ctx)` at definition time.
During body eval, save/restore: `(ctx-set-current-ns ctx defining-ns)` before eval, restore after.
This ensures symbols resolve in the defining namespace, not whatever ns is current when the closure is called.

### defmacro details
- Supports optional docstring: `(defmacro name [args] body)` or `(defmacro name "doc" [args] body)`
- Implementation: `(tuple/slice form 2)` → check if first is string → adjust args-form and body start
- Implicit `&env` binding: `(put new-bindings "&env" @{})` — table, not struct (nil-safe)
- Uses `parse-arg-names` for `& rest` arg handling

### defmulti / defmethod
- `defmulti` stores the methods table on the **var** via `(put v :jolt/methods methods)`, NOT on the function
- Janet `put` on a function value fails silently
- `defmethod` retrieves methods via `(get mm-var :jolt/methods)` using `resolve-var` to get the var first

### set!
- If var doesn't exist, auto-creates it (`ns-intern`) rather than erroring
- Needed for sci's `(set! *warn-on-reflection* true)`

### defmethod (auto-create)
- If the multimethod var doesn't exist yet, auto-creates it with a dummy fn and empty methods table
- This allows sci's `defmethod print-method` to work before `defmulti print-method` is defined

### deftype
- Handles `^:meta` on type name via `with-meta` pattern
- Fields vector handles `^:meta` annotations and `^Type` hints — extracts actual symbol name
- Produces a table with `:jolt/deftype "ns.TypeName"` and keyword-keyed fields

## Janet Gotchas

- `var` declares mutable locals that can be `set` later; `def`/`let` are immutable
- `let` cannot bind to `nil` — use `(var x nil)` instead of `(let [x nil] ...)`
- `(get table key)` needs 2 args minimum — for single-arg checks use `(table :key)`
- Functions are not tables — `(put fn :prop val)` fails. Stash properties on vars
- `match` is Janet's pattern matching — no `case` or `cond` needed for simple dispatch
- Janet structs silently **drop entries with nil values**: `(struct ;[:x nil :y 1])` → `{:y 1}`. Use `@{}` mutable tables when nil-valued entries are needed (e.g., `&env` binding `@{}` for macro bodies)
- Janet `(put table key nil)` **silently drops the key** — even on mutable `@{}` tables. Use `bind-put` helper (stores `:jolt/nil` sentinel) for binding tables; `resolve-sym` unwraps `:jolt/nil` back to `nil`. All `put` calls on bindings in `fn*`, `let*`, `loop*`, macro bodies, `deftype` reify MUST use `bind-put`
- `resolve-sym` returns `:jolt/not-found` sentinel to distinguish nil local bindings from absent ones; falls back to `clojure.core` for unqualified symbols
- `match` with string patterns returns **nil** (not error) when no pattern matches. Used in `eval-list` to handle non-symbol heads cleanly — keyword heads like `:ns` fall through to default function application
- `break` in `while` does NOT return a value in Janet. Use `(var done nil)` + `(set done val)` + check pattern instead
- Janet `#{}` set literals can cause parse issues in some contexts — use `@[]` as fallback
- `(first struct)` calls `:jolt/type` method — use `(get struct :key)` instead of positional access
- Janet `(struct ;[:x nil])` silently drops the nil entry — the map becomes `{}`. Use `@{}` tables for nil-safe entries
- `(length struct)` counts key-value pairs, not keys. Use `(length (keys struct))` for key count
- **`(last string)` returns nil** — `last` works only on indexed types (tuple, array). For strings use `(s (- (length s) 1))` or `(string/slice s (- (length s) 1))`
- **`(set [a b] tuple)` doesn't work** — Janet's `set` doesn't support destructuring. Use `(tuple 0)` / `(tuple 1)` or explicit individual assignments
- **`(string :keyword)` → `"keyword"`** — Janet has no `name` function. Use `(string kw)` to convert keyword to string. `(name :kw)` is NOT a Janet built-in.
- **`try` handler must be one line** — `(try body ([err] handler))` — the `([err] ...)` clause is a tuple literal, Janet requires it all on one line. Multi-line handlers cause parse errors.
- **`put` doesn't work on structs** — `(put {:a 1} :b 2)` errors. Use `@{}` tables for mutable maps.
- **`(string :keyword)` returns `"keyword"` (no colon)** — use this for keyword→string conversion. Avoid `(name :keyword)` in Jolt eval context; `name` is a Janet built-in but may shadow or be unavailable
- **`(get struct :key)` works, `(in struct :key)` fails** — `in` is for indexed types only (tuple, array, string, buffer). Use `get` for structs/tables
- **Janet `try` takes exactly 2 args**: `(try body ([err] handler))`. Multi-expression body must wrap in `do`: `(try (do expr1 true) ([err] handler))`

### unwrap-meta-name helper
Recursively unwraps `(with-meta sym meta)` forms to extract the underlying symbol. Used in `def`, `ns`, `deftype`, `defmethod` to handle metadata-wrapped names:
```janet
(defn- unwrap-meta-name [form]
  (if (and (array? form) (> (length form) 0)
           (struct? (in form 0))
           (= :symbol ((in form 0) :jolt/type))
           (= "with-meta" ((in form 0) :name)))
    (unwrap-meta-name (in form 1))
    form))
```

### Reader map k/v handling
The map reader must handle three special value types in both key and value positions:
- `:jolt/skip` — discarded form (comment, `#_`, nil `#?(:cljs ...)`): skip the K/V pair entirely
- `:jolt/splice` — `#?@` splicing: concat items into the kvs array. If splice has 0 items (nil `:cljs` branch), don't push the key

### `#?(:cljs X)` returns nil → `:jolt/skip`
The non-splicing `#?` reader now returns `{:jolt/type :jolt/skip}` for nil results (e.g., `#?(:cljs X)` on CLJ). This prevents orphaned nil keys/values in maps and lists. `parse-next` and `parse-string` skip past skip markers to return the next real form.

### deftype →TypeName constructor
`deftype` now interns both the type name and `->TypeName` arrow constructor (Clojure convention). The dot-suffix constructor check uses `(sym-name (- (length sym-name) 1))` instead of broken `(last sym-name)`.

## Comment Handling

Comments `;` in `read-form` return `{:jolt/type :jolt/skip}` sentinel:
```janet
(= c 59)  # ;
[{:jolt/type :jolt/skip} line-end])
```

`parse-next` and `parse-string` both skip over `:jolt/skip` results:
- `parse-next` uses inner `parse-next-loop` that recurses on skip
- `parse-string` recurses on skip to return next non-comment form

`read-map` checks both key AND value positions for `:jolt/skip` to skip `#_`-discarded entries.

Closing delimiters `)`, `]`, `}` in `read-form` produce explicit errors:
```janet
(= c 41) (error (string "Unmatched closing paren at " pos))
```

This prevents them from falling through to `read-symbol` which gave "Unrecognized character".

## `unwrap-meta-name` Utility

Recursively unwraps `(with-meta sym meta)` forms to extract the underlying symbol:
```janet
(defn- unwrap-meta-name [form]
  (if (and (array? form) (> (length form) 0)
           (struct? (in form 0))
           (= :symbol ((in form 0) :jolt/type))
           (= "with-meta" ((in form 0) :name)))
    (unwrap-meta-name (in form 1))
    form))
```

Used in: `def`, `ns` (ns name), `deftype` (type name + field names), `defmethod` (arg names).
Replaced duplicated `with-meta` unwrapping code in each of these forms.

## Bootstrap Patterns

### Class-name resolution in resolve-sym

When a simple symbol (unqualified) isn't found in current ns or clojure.core, checks for dotted class-name pattern:
`Foo.Bar.Baz` → finds last dot → ns "Foo.Bar", name "Baz" → tries ns-resolve.
This resolves symbols like `IVar` that are interned in `sci.impl.vars` but referred to unqualified from `sci.lang`.

### Reader conditionals
- `#?(:clj expr :cljs expr2)` — resolved at read time by `read-reader-conditional`
- `#?@(:clj expr :cljs expr2)` — splicing variant, wraps resolved items in `{:jolt/type :jolt/splice :items ...}` 
- List/vector/set readers check for splice and flatten items
- `#_` — discard reader macro, reads next form and returns it as position only

### Core macros (in core.janet)
- `core-macro-names` returns `@{"when" true "when-not" true "if-let" true "when-let" true "if-some" true "when-some" true "defn" true "defn-" true "declare" true "fn" true "let" true "defprotocol" true "extend-type" true "extend-protocol" true "extend" true "reify" true "proxy" true "definterface" true "comment" true}` — a table
- `init-core!` calls `(get (core-macro-names) name)` to check, then `(put v :macro true)`
- Order matters: macro functions must be defined BEFORE `core-bindings` map references them

### Core stubs for sci bootstrap
- `core-derive`, `core-isa?`, `core-ancestors`, `core-descendants` — minimal hierarchy
- `core-Object`, `core-Thread`, `core-ThreadLocal`, `core-IllegalStateException` — JVM class stubs
- `core-volatile!`, `core-vswap!`, `core-vreset!` — volatile (atom-like table with :val key)
- `core-defprotocol` emits `(do (def name @{}) (def method fn) ...)` — macro, returns do form
- `core-extend-type`, `core-extend-protocol`, `core-extend`, `core-reify`, `core-satisfies?`, `core-extends?`, `core-implements?`, `core-type->str` — protocol stubs
- `core-copy-core-var`, `core-copy-var`, `core-macrofy`, `core-new-var`, `core-avoid-method-too-large` — sci.impl.copy-vars stubs (needed by namespaces)
- `core-resolve` — stub returning nil (sci symbol resolution check)
- `core-update` — delegates to Janet's `update` with apply
- `core-prefer-method` — multimethod preference ordering stub

### `:jolt/nil-sentinel` pattern for nil bindings
Janet table literals drop entries with nil values: `@{"x" nil}` → `{}`. For core bindings that need nil root values (e.g., `*1`, `*2`, `*3`, `*e`), use the sentinel:
```janet
"*1" :jolt/nil-sentinel
"*2" :jolt/nil-sentinel
```
`init-core!` unwraps sentinels back to nil:
```janet
(def v (ns-intern ns name (if (= fn :jolt/nil-sentinel) nil fn)))
```

### `fn*` namespace capture
Functions close over their defining namespace so symbols resolve correctly regardless of caller's current-ns:
```janet
"fn*" (let [args-form ...
            defining-ns (ctx-current-ns ctx)]  ; capture at definition time
        (set self (fn [& fn-args]
          (def saved-ns (ctx-current-ns ctx))
          (ctx-set-current-ns ctx defining-ns)   ; restore defining ns
          (var result nil)
          (each body-form body
            (set result (eval-form ctx fn-bindings body-form)))
          (ctx-set-current-ns ctx saved-ns)      ; restore caller's ns
          result)))
```
Same pattern in `defmacro` bodies.

### `let*` destructuring
Supports `:keys` destructuring with both symbol keys and keyword keys:
```janet
(if (struct? pat)
  (let [keys-vec (get pat :keys)]
    (if (and keys-vec (indexed? keys-vec))
      (each k keys-vec
        (def kname (if (keyword? k) (string k) (k :name)))  ; keyword→string, symbol→name
        (bind-put new-bindings kname (get val (keyword kname))))
      (bind-put new-bindings (pat :name) val)))  ; plain symbol binding
  ...
```

Also supports sequential destructuring of vector patterns.

### New core macros
- `core-if-let` — expands `(if-let [x expr] then else)` → `(let* [x expr] (if x then else))`
- `core-when-let` — expands to `(let* [x expr] (when x body))`
- `core-if-some` — expands to `(let* [x expr] (if (some? x) then else))`
- `core-when-some` — expands to `(let* [x expr] (when (some? x) body))`
- `core-let` — expands `(let [bindings] body)` → `(let* [bindings] body)` (macro, like `fn` → `fn*`)

### Namespace handling
- `ns` form handles `^:meta` on ns name via `with-meta` pattern
- `def` form also handles `^:meta` on def name (extracts name-sym from `(with-meta Name meta)`)
- `require` clause in `ns` wraps each spec in `(when s ...)` for nil-safety
- `resolve-var` falls back to checking clojure.core namespace if var not found in current ns

### Bootstrap loading order (corrected)
```
sci.impl.macros      (4/4 ok)
sci.impl.protocols   (15/15 ok)
sci.impl.types       (22/22 ok)
sci.impl.unrestrict  (2/2 ok)
sci.impl.vars        (28/28 ok)
sci.lang             (10/10 ok)
sci.impl.utils       (45/46 ok — 1 fail on dynamic-var internals)
sci.impl.namespaces  (124/127 ok — 3 remaining: major destructure, clojure-version, iterable)
sci.core             (64/65 ok — 1 remaining form)
```
All .cljc files. #?(:clj ...) resolved at read time. #?(:cljs ...) returns nil.

**Critical load order**: `macros` → `protocols` → `types` → `unrestrict` → `vars` → `lang` → `utils` → `namespaces` → `core`. `utils` must load after `lang` (needs `lang/->Namespace`, `vars/unqualify-symbol`). `namespaces` needs `utils` (for `clojure-core-ns`, `dynamic-var`). `core` needs `namespaces` (for `*1`, `*2`, `*3`, `*e`, `resolve`).

### parse-arg-names
- Handles `& rest` args AND nested destructuring vectors
- When an arg is a vector (not a symbol), recurses to extract nested symbol names

## Project Structure

```
src/jolt/
  types.janet      — Var, Namespace, Context, symbol helpers
  reader.janet     — recursive descent parser for Clojure syntax
  evaluator.janet  — tree-walking interpreter (eval-form, eval-list, syntax-quote*)
  core.janet       — 95+ clojure.core functions (map, filter, reduce, etc.)
  api.janet        — public API: init, eval-string, eval-string*
  main.janet       — REPL entry point with (defn main [&])

test/
  evaluator-test.janet  — special form tests (22 forms tested)
  reader-test.janet     — parser tests (includes #?, #?@, #_)
  core-test.janet       — core library tests
  macro-test.janet      — syntax-quote and macro tests
  namespace-test.janet  — ns, require, in-ns tests
  types-test.janet      — Var and Namespace tests
  api-test.janet        — public API tests
  bootstrap-test.janet  — loads sci.impl.macros (deftime, usetime, ?)
  test-load-sci.janet   — loads all sci files, counts ok/fail
  test-eval.janet       — end-to-end sci.core/eval-string test
```

## Edamame Shim for SCI eval-string

SCI's `eval-string` depends on `edamame.core` (parser) and `clojure.tools.reader.reader-types`. Instead of loading the real edamame (its parser.cljc has character-literal edge cases), inject a shim in `core.janet`:

```janet
(defn init-edamame-shim! [ctx parse-str read-f]
  ...)
```

Called after SCI sources load: `(init-edamame-shim! ctx parse-string read-form)`. Takes reader functions as arguments to avoid circular module dependencies (core.janet doesn't use ./reader).

Shim creates `edamame.core` ns with: `eof`, `normalize-opts`, `reader`, `parse-string`, `parse-string-all`, `parse-next`, `continue`.
Also creates `clojure.tools.reader.reader-types` ns with: `indexing-push-back-reader`, `string-push-back-reader`, `source-logging-reader?`, `get-line-number`, `get-column-number`.

### Remaining gap

SCI's `eval-string` calls into `sci.impl.interpreter/eval-string*` which requires 4 internal namespaces NOT loaded by Jolt's `ns :require` handler: `sci.impl.interpreter`, `sci.impl.parser`, `sci.impl.analyzer`, `sci.impl.opts`. Their source files must be loaded separately.

## Module Dependency Gotcha

Don't create standalone shim files that `(use ./reader)` if test files also use reader — Janet resolves modules at compile time and circular dependencies cause "Unmatched closing bracket" errors at unrelated file positions. Embed shims in existing modules (e.g. `core.janet`) and pass reader functions as arguments.

## `let*` Destructuring

`:keys` destructuring: `{:keys [a b]}` → `(get pat :keys)` returns `(a b)` tuple where each is a keyword. Bind each using `(get val (keyword kname))`. Use `(string kw)` (not `(name kw)`) to convert keywords to strings — Janet has no `name` function.

Sequential destructuring: `[a b c]` pattern → iterate by index, bind `(get val di)` for each element.
