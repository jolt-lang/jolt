# RFC 0006 — Compile-time detection of provably-wrong code (success typing)

- **Status**: Implemented. Core-fn error domains (arithmetic on non-numbers,
  count/first/rest/next/seq/nth on non-seqable scalars), `JOLT_TYPE_CHECK=
  off|warn|error`. Follow-ups landed: bounded scalar **unions** so a
  use is reported only when every member is in the error domain; **user-fn
  error domains** behind `JOLT_TYPE_CHECK_USER` (closed-world);
  precise **file:line:col** locations. The checker is now one
  inference walk (folded into `infer`), and is **on by default in direct-link
  builds** — where it piggybacks on the specialization inference for ~free —
  and opt-in (`JOLT_TYPE_CHECK`) in plain builds.
- **Champions**: jolt maintainers
- **Created**: 2026-06-13
- **Depends on**: RFC 0005 (structural collection-type inference)

## Summary

Reuse the structural type inference of RFC 0005 as a **loose type checker**: at
compile time, flag code that is *provably* wrong, accept everything that is
merely ambiguous, and never produce a false positive. Concretely, when an
expression's inferred type is concrete and the operation applied to it would
throw at runtime for that type (for example passing a string where a function
only ever operates on numbers), report a clear compile-time error pointing at
the offending form, with the inferred type and what was expected. When the type
is `:any`, a union that includes a valid case, or beyond the inference's depth
cap, accept it silently. This is **success typing** (the discipline behind
Erlang's Dialyzer), applied to jolt for free on top of the inference we already
need for optimization.

## Motivation

Once the compiler tracks concrete types for many values (RFC 0005), it can see
some programs that cannot possibly be correct: `(inc "x")`, `(first 5)`,
`(count :k)`, `(/ 1 "two")`. Today these compile and fail at runtime, often far
from the cause. Reporting them at compile time, with a precise location and
message, turns a class of runtime crashes into immediate, actionable feedback,
at no extra inference cost.

The design constraint the user set is the right one and is exactly success
typing's contract: **accept ambiguous cases, reject only provably-wrong ones.**
A checker that never lies about errors is one developers trust and that does not
get in the way of correct-but-untypeable dynamic code.

## Principle: success typing, never a false positive

Success typing (Lindahl and Sagonas, "Practical Type Inference Based on Success
Typings", 2006; the basis of Dialyzer) inverts the usual type-checker stance.
A normal checker accepts only what it can prove correct and rejects the rest
(false positives on dynamic code). A success typer accepts everything that
*could* be correct and rejects only what *cannot* be correct under any
execution. It is sound for **rejection**: if it reports an error, the code is
genuinely wrong. It is intentionally incomplete: it misses errors it cannot
prove. That is the correct trade for a dynamic language, and it matches the
user's "accept ambiguous, reject provably wrong".

Mapped onto jolt:

- The inference assigns a value a concrete type only when it can prove it
  (RFC 0005). Unprovable is `:any`.
- A use site is reported **iff** the argument's inferred type is concrete and
  lies entirely outside the operation's accepted domain, where the operation
  *throws* on that domain (not merely returns a benign default).
- `:any`, a depth-capped child, or a union that includes an accepted type is
  **never** reported.

## What "provably wrong" means

The checker needs, per operation it understands, an **error domain**: the set
of argument types for which the operation throws at runtime. This is narrower
than "the types it is documented to accept", because Clojure is lenient in many
places and flagging a benign case would be a false positive:

- `(get 5 :k)` returns `nil`, it does not throw. NOT reported.
- `(:k 5)` returns `nil`. NOT reported.
- `(count 5)` throws ("count not supported on number"). Reported when the
  argument is provably a non-countable scalar.
- `(first 5)` throws (not seqable). Reported for a provably non-seqable scalar.
- `(inc "x")`, `(+ 1 "x")` throw. Reported when an argument is provably a
  non-number (`:str`, `:kw`, `:struct`, `:vec`, ...).
- `(nth 5 0)` throws. Reported for a provably non-indexable scalar.

So the checker ships a curated table of the clearest throwing operations with
their error domains. It starts small (arithmetic on non-numbers, seq/`count`/
`nth`/`first` on non-seqables) and grows conservatively. Anything not in the
table is not checked, which is safe (no false positive).

A use site is reported only when:

1. the argument's inferred type `T` is concrete (not `:any`, not a union that
   includes an accepted type, not truncated by the depth cap), and
2. `T` is in the operation's error domain (the operation provably throws on `T`).

## Examples

```clojure
(inc "x")                 ; ERROR: inc expects a number, got a string
(let [n "x"] (inc n))     ; ERROR: same, n inferred :str
(count :foo)              ; ERROR: count not supported on :kw
(first 42)                ; ERROR: 42 is not seqable
(:k 5)                    ; accepted (returns nil, not an error)
(inc (rand-nth coll))     ; accepted if the element type is :any/unknown
(inc (if c 1 "x"))        ; accepted: union {:num, :str} includes :num (ambiguous)
(defn f [n] (inc n)) ...  ; if f is ALWAYS called with strings in-unit, ERROR at the call;
                          ;   if its callers are unknown/varied, accepted
```

## Error reporting

A reported error includes:

- the source location (`file:line:col`) of the offending form;
- the operation and the parameter position;
- the inferred type of the argument, rendered readably (`:str`,
  `{:struct {:r :num}}`, `{:vec :any}`);
- what the operation requires (`a number`, `a seqable`).

Example:

```
type error scene.clj:42:18: `inc` requires a number, but argument 1 is a string
```

Errors are attributed to the form the user wrote. For macro-expanded code, the
checker reports at the original form's recorded position (the loader already
tracks `:error-pos`), never at synthesized internals.

## Strictness levels

`JOLT_TYPE_CHECK` controls behavior:

- **off** — no checking.
- **warn** — report to stderr, do not fail compilation. **The default in
  direct-link builds**, where checking rides the specialization inference for
  ~free; opt-in elsewhere.
- **error** — fail compilation on a provable type error. Opt-in for CI / strict
  builds.

When `JOLT_TYPE_CHECK` is unset, checking is **on (`warn`) in direct-link
builds** and **off in plain REPL/dev builds** (where it would cost a standalone
inference pass, ~2.6× compile). `JOLT_TYPE_CHECK_USER` additionally enables
reporting against inferred user-function domains (closed-world; see below).

Because the checker only fires on provable errors, even `error` mode cannot
break a correct program: a correct program has no provable type errors to
report. (A correct-but-untypeable program is simply not reported, since its
types degrade to `:any`.)

## Soundness of rejection (no false positives)

The whole value of this feature is that a reported error is real. The
guarantees:

- The inference assigns concrete types only when provable (RFC 0005). So a
  concrete `T` at a use site is a genuine lower bound on what flows there in the
  analyzed world.
- The error-domain table lists only operations that genuinely throw on the
  listed types, verified against the runtime.
- Ambiguity is always accepted: `:any`, unions containing an accepted type, and
  depth-capped children are never reported.

Two boundaries need care and bound where the checker is allowed to fire:

- **Closed-world / redefinition.** Inter-procedural argument types assume the
  compiled unit is the whole program (inherited from RFC 0005). For the checker,
  this means a reported error on a *user* function's parameter is only as sound
  as that assumption. The conservative initial policy: only report against
  **core-function** error domains (stable, not redefinable) and against types
  derived without crossing an open boundary. Reporting against inferred user-fn
  signatures is a later, opt-in escalation.
- **Macros / generated code.** Check post-expansion IR but report at the user's
  source location, and suppress reports inside expansions the user did not
  write (or attribute them to the macro call site).

## Relationship to other systems

- **Dialyzer / success typing** (Erlang): the direct model — sound for
  rejection, no false positives, accepts the ambiguous.
- **Typed Clojure / core.typed**: opt-in *sound* gradual typing that rejects
  what it cannot prove correct; the opposite trade (false positives on dynamic
  code), which is why we do not follow it.
- **clj-kondo**: a popular Clojure linter that flags some obvious type misuses
  syntactically; this RFC subsumes the type-driven subset with inference-backed
  precision and no false positives.

## Implementation

The checker is a thin pass over the same inference results:

1. After (or during) inference, walk the IR. At each call to an operation in
   the error-domain table, look at the inferred type of each checked argument.
2. If concrete and in the error domain, record a diagnostic with location, the
   inferred type, and the expected domain.
3. Emit diagnostics per the strictness level.

It adds no new inference; it consumes RFC 0005's types and a small curated
table. It can ship after RFC 0005 lands, starting in `warn` mode with the
smallest high-confidence table (arithmetic and seq/count/nth/first), and grow.

## Design problems and open questions

- **Curating the error domain.** The table must list only genuinely-throwing
  cases. Getting it wrong (listing a lenient op) yields false positives, which
  destroys trust. Mitigation: start tiny, test each entry against the runtime,
  grow slowly. Open question: derive the table from the same machinery the
  runtime uses, to avoid drift?
- **Unions.** *Resolved.* The lattice has a bounded scalar union
  `{:union #{T...}}` (cap 4); differing if-branches form a union instead of
  collapsing to `:any`, and a use is reported only when *every* member is in the
  error domain. Unions are opaque to structural specialization, so codegen is
  unchanged.
- **User-function signatures.** *Resolved, opt-in.* Behind
  `JOLT_TYPE_CHECK_USER`: the checker re-checks a registered non-redefinable
  user fn's body with one parameter bound to its concrete argument type; a
  diagnostic the all-`:any` body did not have means that argument is provably
  wrong. Monotonic, so still no false positives; closed-world, hence opt-in.
- **Negative/never types.** *Resolved.* Calling a provably
  non-callable value (`:num`/`:str` — keywords/maps/vectors/sets are IFn) is
  reported at the default level; wrong-arity to a registered single-fixed-arity
  user fn is reported under the `JOLT_TYPE_CHECK_USER` opt-in. A union callee is
  flagged only when every member is non-callable.
- **Position vs intent.** *Resolved.* The reader records each list
  form's absolute offset (identity-keyed, so positions survive macroexpansion
  exactly when the user's sub-form is spliced through); the analyzer stamps it
  onto `:invoke` nodes, the checker carries it into each diagnostic, and the
  back end renders `file:line:col`. Inlining/scalar-replace preserve it via
  `assoc`.
- **Interaction with the optimization gate.** *Resolved (jolt audit).* The
  checker is one inference walk folded into `infer`. In direct-link builds it
  piggybacks on the specialization inference that already runs (~free, default
  on); in plain builds it runs as a standalone pass only when `JOLT_TYPE_CHECK`
  is set. "Run inference for checking" and "specialize from inference" are the
  same walk now, gated by a `checking?` flag.
