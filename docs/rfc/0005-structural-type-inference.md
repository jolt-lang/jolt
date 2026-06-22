# RFC 0005 — Structural collection-type inference

- **Status**: Implemented (jolt-5uj). Ray tracer 12.8s to 11.0s hint-free,
  matching the explicit `^:struct` version; render checksum unchanged.
- **Champions**: jolt maintainers
- **Created**: 2026-06-13

## Summary

Replace jolt's ad-hoc inference lattice with a single recursive **structural
type**, so that the type of a value mirrors the tree shape of the data it
describes. A struct-map carries its field types, a vector its element type, a
function its parameter and return types, recursively. A keyword lookup returns
the looked-up field's type, so nested access like `(:r (:direction ray))` is
typed end to end. This unifies the two facts the current inference tracks
inconsistently (a vector's element type, but not a map's field types), subsumes
the existing inference phases (jolt-99x Phases 0 to 3) as special cases, and
closes the remaining ray-tracer gap without a hint. The system is a
soft-typing-style inference: it never rejects a program, it assigns a concrete
type only when it can prove one, and it falls back to `:any` (and the existing
runtime guard) everywhere else.

## Motivation

The inference added in jolt-99x specializes a collection access (drops the
`:jolt/type` guard, emits `pv-count`, and so on) when it can prove the
collection's type. It works, it is sound, and it is fully dynamic-fallback
safe. But its type lattice grew ad hoc:

- `:struct-map` means "a raw-get-safe map" but carries **no field types**.
- `{:vec ELEM}` carries its **element type**.

These are the same idea applied to two kinds of child in the data tree, but
only one is tracked. The cost is concrete: in the ray tracer a lookup result
like `(:direction ray)` is typed `:any`, so `(:r (:direction ray))` keeps its
guard, and the `vec3` functions (called all day with such values) cannot be
typed, so the inference reaches only about 3% where the explicit `^:struct`
hint reaches 22%. The hint wins precisely because it asserts the field/param
shape the inference fails to derive.

The fix is to make the type a structural tree, tagged as precisely as provable.
Then `:struct` tracking and field tracking are one mechanism, the special cases
collapse into one signature table, and nested access is typed by construction.

## The type lattice

A type `T` is one of:

- A scalar tag: `:num`, `:str`, `:kw`, `:bool`, `:char`. (Optionally a coarser
  `:nonnil` for "provably not nil and not false", which is what the struct-vs-phm
  decision needs; see below.)
- `:nil`.
- `{:struct {field -> T}}` — a raw-get-safe map (a record) whose
  field `k` has type `(fields k)` or `:any` if absent. The degenerate
  `{:struct {}}` is "a struct, fields unknown" and replaces today's
  `:struct-map`.
- `{:vec T}` — a vector whose elements have type `T`.
- `{:set T}` — a set of `T`.
- `:phm` — a persistent hash map (NOT raw-get-safe; distinct from `:struct`).
- `{:fn {:params [T...] :ret T}}` — a function (optional precision; the current
  flat param/return inference is the zero-arity-detail version of this).
- `:any` — the top. Anything not provably more specific.
- `:bottom` (represented as the absence of a type / `nil` internally) — the
  identity for join, used to seed the fixpoint.

Types are immutable values comparable by structural equality, exactly like the
current `{:vec ELEM}` representation, so they flow across the portable
inference and the host unchanged.

### Join (least upper bound)

```
join(T, T)                         = T
join(bottom, T)                    = T
join({:struct a}, {:struct b})     = {:struct {k -> join(a[k]?:any, b[k]?:any) for k in keys(a) ∪ keys(b)}}
join({:vec a}, {:vec b})           = {:vec join(a, b)}
join({:set a}, {:set b})           = {:set join(a, b)}
join(_, _)                         = :any        ; different constructors
```

Two struct types join field-wise; a field present in only one side becomes
`:any` in the result (it might be absent, so a lookup of it is not provably
typed). This is the standard record lattice.

### Termination: depth cap

Structural types of recursive data (a tree node that contains a tree node, a
cons cell) would be infinite. To keep types finite and the inter-procedural
fixpoint terminating, structural types are **depth-capped**: beyond a small
depth `D` (proposed `D = 4`) a child type is `:any`. Construction and join both
truncate at `D`. With the cap the lattice has finite height, so the monotone
fixpoint converges. The ray tracer's shapes (vec3 inside ray inside hit-info)
are depth 2 to 3, well inside the cap.

## Inference rules

Inference is a forward pass producing `[type node']` for each IR node (the
existing shape), threaded with a local type environment and the
inter-procedural state from Phase 1. The rules are uniform over the structural
type:

- **Literals.** `{:k v ...}` with constant scalar keys and struct-safe values
  builds `{:struct {:k type(v) ...}}`; otherwise `:phm`. `[a b ...]` builds
  `{:vec (join type(a) type(b) ...)}`. `#{...}` builds `{:set ...}`. Scalars
  build their scalar tag. (The struct-vs-phm condition is the same as the back
  end's: scalar keys, and every value provably non-nil and non-false.)
- **Lookup returns the field type.** `(:k m)` / `(get m :k)` where
  `m : {:struct fs}` returns `(fs :k)` or `:any`. This is the single rule that
  makes nesting work and that unifies field tracking with `:struct` tracking.
- **Indexing returns the element type.** `(nth v i)` / `(v i)` where
  `v : {:vec T}` returns `T`. `(first v)` / `(peek v)` likewise.
- **Flow.** `let`/`loop` bind init types; `if` joins the branch types; `do`
  takes the tail type. (As today.)
- **Calls use signatures.** Every call result type comes from the callee's
  signature: core fns from a fixed signature table (below), user fns from the
  inter-procedural fixpoint's inferred signature.

The Phase 1 inter-procedural fixpoint, recompile, escape gate, and closed-world
assumption (RFC to follow / jolt-767) are unchanged. They now propagate
structural types instead of flat tags.

## Core function signatures

The current special cases (`truthy-ret-fns`, `vector-ret-fns`, `elem-fns`,
`hof-table`, and the `conj`/`range`/`reduce`/`mapv` branches) collapse into one
table of **type schemes**, possibly parametric:

```
inc, dec, +, -, *, /, mod, ...   : (... :num) -> :num
count                            : (Coll) -> :num
nth        : ∀T. ({:vec T}, :num) -> T          (3-arg adds a default: -> join(T, default))
get        : ∀T. ({:struct fs}, :k) -> (fs :k)  ; const key
first,peek : ∀T. ({:vec T}) -> T
conj       : ∀T. ({:vec T}, x) -> {:vec join(T, type(x))}
assoc      : ({:struct fs}, :k, v) -> {:struct (assoc fs :k type(v))}   ; const key
vec, mapv  : ... -> {:vec ...}
range      : (...) -> {:vec :num}
rand-nth   : ∀T. ({:vec T}) -> T
map, filter, mapv, filterv, reduce, ...        ; see HOFs
```

Parametric schemes (the `∀T`) are where polymorphism actually matters, and they
give the element/field propagation for free. **Higher-order functions are just
schemes whose parameter is itself a function type**: `reduce`'s scheme says its
function argument is `(Acc, Elem) -> Acc` applied to the collection's element
type, so the closure's element parameter is typed by applying the scheme,
replacing the hand-written `hof-table`.

## Hints as seeds

`^:struct x` seeds `x : {:struct {}}` (a struct, fields unknown) at a unit
boundary the inference cannot see across. A future extension could allow a shape
hint `^{:r :num :g :num :b :num}` to seed field types, but once inference is
structural this is rarely needed; the hint stays the escape hatch for genuinely
dynamic boundaries, exactly as today.

## Soundness

Unchanged in spirit from the current system: a concrete type is assigned only
when proven (a literal genuinely has those fields; a fn provably returns that
shape), and everything unprovable is `:any`, which keeps the dynamic guard. A
wrong specialization is therefore impossible. The inter-procedural part keeps
the closed-world (optimization-mode) assumption already adopted, which is sound
under whole-program / source-distribution compilation.

## Compilation modes and defaults

Direct-linking — and the inference and specialization it enables — is the
**default for running a program** and stays **off for interactive work**, chosen
by the CLI run mode rather than a global opt-in flag:

| mode | linking | whole-program |
|---|---|---|
| `-m` / `-M NS` (program entry) | direct (default) | **auto** (closed world) |
| `FILE` / `-f` / stdin (`-`) | direct (default) | no (per-namespace) |
| `repl`, `-e`, `nrepl-server` | indirect / open | no |

A program run is a closed world — every namespace is required, then the code
runs to completion — so it direct-links: user code gets inlining, record shapes,
and the inference's specialization. A `-m` / `-M` entry is the exact point where
all requires are done and `-main` is about to run, so the whole-program
cross-namespace pass (below) runs there automatically. Interactive modes stay
open: a REPL, `-e`, and the nREPL server must let you redefine vars — which
direct-linking seals against — so they keep the indirect, live-deref path.

Env overrides, all winning over the mode default:

- `JOLT_NO_DIRECT_LINK=1` — force the open/indirect path even for a program run
  (runtime redefinition, hot-reload, self-modifying code).
- `JOLT_NO_WHOLE_PROGRAM=1` — keep direct-linking but skip the whole-program
  pass (per-namespace inference only).
- `JOLT_DIRECT_LINK=1` — force direct-linking on even in an interactive mode.
- `JOLT_WHOLE_PROGRAM=1` — force the whole-program pass on in any direct-linked
  mode.
- `JOLT_NO_SHAPE=1` — disable the record/shape representation under direct-linking.

What direct-linking gives up is what Clojure's `:direct-linking` and jank's
`-Odirect-call` give up: a direct call embeds its callee, so redefining the
callee is not seen by already-compiled callers. Whole-program additionally
const-links stable vars (data defs, record types, `^:redef`), extending the same
trade. That is why the interactive modes stay open and the opt-outs exist.

### Cross-namespace inference

Per-namespace inference (a `FILE` run, or any namespace under
`JOLT_NO_WHOLE_PROGRAM`) types a function's parameters from the call sites it can
see **within that namespace**. A function whose record parameter is supplied by a
caller in *another* namespace is left `:any`, its field reads keep the guard, and
the values derived from it widen — so a decomposed program is markedly slower
than the same code in one namespace (measured at ~3.7× on the ray tracer split
across five namespaces). The information exists in the program; per-namespace
compilation just can't see a caller in a not-yet-loaded namespace. Two ways to
supply it:

1. **Whole-program** (auto for `-m` / `-M`) runs one closed-world inference
   fixpoint over every loaded namespace before `-main`, typing each parameter
   from its call sites wherever they live. Namespaces required later (inside
   `-main`) fall back to per-namespace inference.
2. **Parameter type hints** (`^RecordType`, RFC 0004) declare the type directly,
   so it also works in the open world — REPL, library code that must be fast for
   any caller, and hot-reloading servers — where the world cannot be closed.

## Relationship to Hindley-Milner and soft typing

This is HM-shaped with two deliberate departures, which is the textbook
definition of **soft typing** (Wright and Cartwright, "A Practical Soft Type
System for Scheme", 1997 — HM extended with union types and a dynamic type).

Taken from HM:

- The **structural type language** (records, vectors, functions as type
  constructors). This is the "tree of types".
- **Constraint propagation** and **type schemes** for the core library (the
  `∀T` signatures). That parametric polymorphism is exactly what HM provides,
  and it is where it matters (generic collection functions like `nth`,
  `reduce`, `map`).

Changed, on purpose:

- Replace "unify or **fail**" with "**join over a lattice whose top is `:any`**".
  The inference never rejects a program; an unprovable spot becomes `:any` and
  keeps the runtime guard. This is the "fall back to dynamic when in doubt"
  policy made principled.
- **Monovariant** for user functions (the inter-procedural fixpoint plus
  inlining cover the practical polymorphism); parametric schemes are kept only
  for core functions.

So: HM structural types and constraint propagation and core-fn schemes, solved
by lattice join with a dynamic top instead of unification-or-fail. Other AOT
inferencers for dynamic languages do the whole-program version of the same
thing (RPython's annotator, Crystal's global inference, Shed Skin), all with a
union/dynamic fallback.

## Implementation and migration

This is a refactor that **simplifies** the current code: it deletes the ad-hoc
tag soup and the per-op special cases and replaces them with one recursive type
plus a signature table.

1. Define the structural type, `join`, the depth cap, and the predicates
   (`struct-safe?`, `field-type`, `elem-type`) in `jolt.passes`.
2. Rewrite `infer` so each op produces/consumes structural types: literals
   build shapes; `(:k m)` returns the field type; calls consult the signature
   table.
3. Move the core-fn knowledge into a signature table (subsumes the existing
   tables and HOF handling).
4. The back end keeps reading the use-site type to specialize (guard drop for
   `{:struct}`, `pv-count`/`pv-nth` for `{:vec}`), now uniformly.
5. Keep the Phase 1 fixpoint, recompile, escape gate, and triggering as is; they
   propagate structural types.

The phases land incrementally behind the same optimization-mode gate, each
verified against conformance (three modes), the full test gate, and the
ray-tracer benchmark, exactly as the current phases were.

## Design problems and open questions

- **Recursion / termination.** Handled by the depth cap (`D = 4`). Open
  question: is a fixed cap better than proper recursive (mu) types? A cap is
  simpler and sound; mu-types are more precise but add complexity. Proposed:
  start with the cap.
- **Compile-time cost.** Structural types are larger and the fixpoint does more
  work. Mitigations: the depth cap bounds type size; inference runs only in
  optimization mode; the fixpoint iteration count stays bounded. Needs
  measurement on a large namespace (clojure.core itself) to confirm acceptable.
- **Heterogeneous data.** `[1 "a"]` joins to `{:vec :any}`; a map whose field
  varies across branches joins that field to `:any`. Correct degradation, not a
  problem, but worth stating.
- **Non-constant keys.** `(assoc m k v)` / `(:k m)` with a non-constant `k`
  cannot track a specific field; the result degrades to `{:struct {}}` or
  `:phm` as appropriate. Field tracking only applies to constant scalar keys.
- **`false`/`nil` field values.** A map literal is `{:struct ...}` only when
  every value is provably non-nil and non-false (the back end stores such maps
  as a phm). The `:nonnil` tag (or a per-type "provably truthy" predicate) is
  what the literal rule needs; this must be carried correctly or struct
  inference is unsound.
- **Function-type precision.** `{:fn ...}` is optional. The current flat
  param/return inference is enough for the collection-specialization goal;
  full function types matter more for the type-checker (RFC 0006) and could be
  deferred.
- **Closed-world boundary.** Inherited from Phase 1: param/return inference
  assumes the compiled unit is the whole program. Documented there; unchanged.
