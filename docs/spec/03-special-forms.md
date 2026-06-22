# §3 Special Forms

**Status**: catalog complete; normative exemplars for `if` and `let*`; the
remaining entries follow the same format (tracked in `coverage.md`).

A *special form* is a form whose head symbol is evaluated by rule rather than
by function application or macroexpansion. The special forms of Clojure are:

> `def` · `if` · `do` · `let*` · `fn*` · `loop*` · `recur` · `quote` · `var`
> · `throw` · `try`/`catch`/`finally` · `set!` · `monitor-enter` ·
> `monitor-exit` (host) · the interop forms `.` and `new` (host)

`let`, `fn`, `loop`, `and`, `or`, `when`, … are **macros** over these (§8);
implementations MUST treat them as redefinable macros, not additional special
forms. `monitor-enter`/`monitor-exit`, `.` and `new` are host forms: their
syntax is specified here, their behavior is host-defined.

Special-form head symbols are not shadowable: a binding named `if` does not
change the meaning of `(if ...)` in operator position. ⚠ This matches the
reference; it differs from Scheme. A local *may* legally be named like a special
form and used in value position (`(let [if 5] if)` ⇒ `5`); only operator
position is reserved. **Macros, unlike special forms, ARE shadowable** by a local
(`(let [when (fn ...)] (when 1 2))` calls the local).

A list form in operator position is resolved in this order (the canonical
read → **macroexpand** → analyze pipeline): a local binding shadows everything;
otherwise a macro head is expanded and the result re-analyzed; otherwise a
special-form head is parsed by rule; otherwise the form is a function
application. Macroexpansion therefore happens *before* special-form dispatch, so
a macro is never mistaken for a special form (and vice versa).

---

### if — since 1.0

```
(if test then)
(if test then else)
```

**Semantics**

- S1. `test` MUST be evaluated first, exactly once.
- S2. Every value other than `nil` and `false` is *logically true*. If the
  value of `test` is logically true, `then` MUST be evaluated and its value
  returned; otherwise `else` (or `nil` when absent) MUST be evaluated and its
  value returned.
- S3. The branch not taken MUST NOT be evaluated.
- S4. `if` MUST be usable in tail position with respect to `recur` (§3
  `recur`): an `if` whose branch is a `recur` form is a valid recur target
  path.

**Edge cases**

- E1. `(if test then)` with a logically false `test` evaluates to `nil`.
- E2. The empty collections (`()`, `[]`, `{}`, `#{}`), the number `0`, and
  the empty string `""` are logically **true** (only `nil`/`false` are
  false). ⚠ This differs from several Lisps and is a frequent divergence
  source in alternative implementations.

**Errors**

- X1. `(if)` and `(if test)` with fewer than two argument forms, or more
  than three, MUST be a compile-time error.

**Examples**

```clojure
(if 0 :t :f)        ;=> :t
(if "" :t :f)       ;=> :t
(if nil :t :f)      ;=> :f
(if false :t)       ;=> nil
```

**Conformance**

S1–S3, E1–E2 → jolt `forms-spec` "if/do/def" group; truthiness group in
`truthiness-spec`; clojure-test-suite `core_test/if.cljc`. S4 → `forms-spec`
fn/loop recur cases. X1 → `forms-spec` "if arity (X1)" (0/1/4-arg forms throw
in both the analyzer and the interpreter).

---

### let* — since 1.0

```
(let* [sym₁ init₁ … symₙ initₙ] body…)
```

`let*` is the primitive sequential-binding form. The user-facing `let` macro
adds destructuring and expands to `let*` (§8); `let*` itself accepts **only
simple symbols** in binding positions.

**Semantics**

- S1. Each `initᵢ` MUST be evaluated in order, exactly once, in an
  environment where `sym₁…symᵢ₋₁` are bound to their values (sequential
  scope, as Scheme `let*`).
- S2. The body forms MUST be evaluated in order with all bindings in scope;
  the value of the last body form is the value of the `let*` form. An empty
  body evaluates to `nil`.
- S3. A later binding MAY rebind the same symbol; each binding creates a new
  lexical binding visible from the next init onward (no mutation of the
  earlier binding is implied).
- S4. Bindings are lexical and immutable: there is no form that assigns to a
  `let*`-bound local. (Closures capture bindings by value; see §3 `fn*`.)
- S5. The binding vector MUST be a vector literal with an even number of
  forms.

**Edge cases**

- E1. `(let* [] body)` is valid and equivalent to `(do body…)`.
- E2. Binding a symbol that names a var shadows the var for the lexical
  extent of the body; `(var sym)` within that extent still denotes the var.

**Errors**

- X1. An odd number of binding forms MUST be a compile-time error.
- X2. A non-symbol in a binding position (e.g. a destructuring pattern) MUST
  be a compile-time error for `let*` — destructuring belongs to the `let`
  macro. ("Bad binding form, expected symbol" in the reference.)

**Examples**

```clojure
(let* [a 1 b (+ a 1)] (* a b))   ;=> 2
(let* [x 1 x (inc x)] x)         ;=> 2
(let* [] 42)                     ;=> 42
```

**Conformance**

S1–S3, E1 → jolt `forms-spec` let group; clojure-test-suite
`core_test/let.cljc`; jank corpus `form/let/*`. X2 → jolt
`destructuring-spec` "primitives reject patterns". S4, X1 → UNVERIFIED
(cases to add).

---

## Remaining entries (format above; status in coverage.md)

| Form | Notes for the entry author |
|---|---|
| `def` | var creation vs re-binding; metadata on the name; `(def x)` unbound; return value is the var |
| `do` | empty `(do)` → nil; top-level `do` splices for compilation units (important and under-documented) |
| `fn*` | arities, variadic `&`, closure capture, self-name, simple-symbol params only, recur target |
| `loop*` | recur arity must match bindings; recur rebinds in place |
| `letfn` | mutually-recursive local fns (`letrec*` semantics — a fn body sees every binding, not only earlier ones). jolt treats `letfn` as a primitive special, not the reference's `letfn` macro → `letfn*` indirection; behavior is identical |
| `recur` | tail-position rule (normative definition of tail position needed), across `if`/`do`/`let*`/`try` interactions |
| `quote` | self-evaluation table: which literals are self-evaluating unquoted |
| `var` | `#'` reader sugar; resolution at compile time |
| `throw` | any value vs Throwable — host question; jolt/cljs allow data, reference requires Throwable → classification needed |
| `try/catch/finally` | catch dispatch order, `:default`-style catch-all is a dialect extension (⚠ divergence note), finally evaluation guarantees, value of try |
| `set!` | `(set! *var* val)` sets the var's innermost thread binding, else its root, and returns val (implemented); a local (deftype mutable field) or host `(.-field obj)` target is host-dependent |
| `.` / `new` | syntax only; behavior host-defined |
