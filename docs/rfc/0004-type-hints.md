# RFC 0004: Type hints and keyword-lookup specialization

Status: accepted (design note)

This note describes how Jolt treats Clojure type hints, and the one place it
uses them: a `^:struct` or `^Record` hint on a local lets a constant-keyword
lookup skip its runtime representation guard. It records the rationale, the
soundness contract, the checked mode for catching inaccurate hints, and the
measured effect, so later work does not relitigate it.

## Background: why the lookup carries a guard

A Jolt map value has several runtime representations (see RFC on collections and
`src/jolt/core.janet`): a Janet struct for a small all-scalar-key literal map, a
persistent hash map (a table tagged `:jolt/type :jolt/phm`) when a key is a
collection or a value is nil, plus sorted maps, transients, and record/deftype
instances. A record instance is a Janet table tagged `:jolt/deftype` but, like a
struct, it carries no `:jolt/type`, so a raw Janet `(get inst :field)` reads its
fields directly.

A constant-keyword lookup `(:k m)` compiles to a guarded form:

```janet
(if (get m :jolt/type) (core-get m k) (get m k))
```

The guard is one opcode. A non-nil `:jolt/type` routes phm/sorted/transient/
lazy-seq values to `core-get`'s full semantics; everything else (structs,
records, nil, scalars) takes the bare Janet `get`, which matches `core-get` for
keyword keys. The guard is correct and cheap, but on a struct it is a second
`get`: profiling the ray tracer (a naive all-maps program) found keyword lookups
are about half of a render, and the guard is the only avoidable part of each
one. A bare get is roughly 20ns where the guarded form is roughly 36ns.

Dropping the guard is only safe when the subject is known to be a plain
struct/record rather than a tagged collection. Jolt does not infer that
inter-procedurally (it would be unsound across a dynamic language's call
boundaries). A type hint supplies the same fact soundly, as a programmer
assertion.

## What the hints mean

Two hints on a local resolve to the "plain struct/record" assertion, which we
call the `:struct` hint internally:

- `^:struct` — the value is a plain struct or record map. There is no Clojure
  keyword with this meaning (Clojure's type hints are class names), so this is a
  Jolt-specific metadata flag, analogous to `^:dynamic`.
- `^Name` where `Name` is a `defrecord`/`deftype`. Both forms define a `->Name`
  positional constructor, so the analyzer treats a tag whose `->Name` resolves
  as a record type. Record instances are raw-get-safe, so the lookup drops the
  guard. A `^String`, `^long`, or any other non-record tag is not a record and
  is ignored, exactly as before.

Every other hint parses and is inert, matching Clojure (S12b in the reader
spec). A hint never changes a program's result; it only permits an
optimization.

## How it flows

The reader already keeps `^hint` metadata on the binding symbol and is otherwise
transparent (`reader.janet`, `meta-form->map`). The change threads that fact to
the lookup site:

1. The analyzer (`jolt-core/jolt/analyzer.clj`) records a `:struct` hint per
   local in its env when a param or `let` binding carries `^:struct` or a
   record-type tag, and attaches `:hint :struct` to that local's `:local` IR
   node. Resolving a record-type tag uses a new host contract function
   `record-type?` (`src/jolt/host_iface.janet`), which checks for the `->Name`
   constructor.
2. The back end (`emit-kw-lookup` in `src/jolt/backend.janet`) emits the bare get
   when the lookup subject is a `:local` carrying the hint, and the guarded form
   otherwise. The unhinted path is byte-identical to before.
3. The inline pass (`jolt-core/jolt/passes.clj`) propagates the hint: when it
   binds a non-trivial call argument to a fresh local, it carries the called
   function's parameter hint onto that local, so lookups inside the spliced body
   keep the bare path. Without this, inlining a hinted function would erase the
   benefit, because the hinted parameter is replaced by an unhinted temporary.

The same machinery covers both `(:k m)` and `(get m :k [default])` when the key
is a constant keyword. A `get` with a variable, numeric, or string key falls
through to `core-get` unchanged.

## Record hints across namespaces, and as inference seeds

A `^RecordType` hint does two things beyond dropping the lookup guard.

**It carries the specific type, not just "a struct".** The guard-skip only needs
to know the value is raw-get-safe (`:struct`), but the structural inference (RFC
0005) wants the actual record type so a field read gets the field's type —
`(:origin ray)` on a `^Ray ray` is a `Vec3`, not `:any`. A record hint on a
parameter is resolved to the record's constructor key and used to **seed the
inference's parameter type**. That is what keeps a record parameter's reads typed
across a namespace boundary *without* whole-program inference (RFC 0005,
"Cross-namespace inference") — the open-world counterpart to the whole-program
pass. Hinting only the public entry point is not enough; the hint has to be on
the function where the hot reads actually happen.

**It resolves across namespaces.** A hint may name a record defined in another
namespace, in either spelling — `^Vec3` where the type is `:refer`-ed, or
`^v/Vec3` where the namespace is `:as`-aliased. Resolution (`record-ctor-key` in
`src/jolt/host_iface.janet`, backed by `record-hint-ctor-key` in
`src/jolt/evaluator.janet`) runs against the *compile* namespace and maps the
type to its home constructor key through a constructor-value index — keyed by the
constructor value, not a var's namespace, so a `:refer`-interned var (whose
namespace is the referring one) still resolves home. The reader keeps a tag's
namespace qualifier (`^v/Vec3` → `"v/Vec3"`, not `"Vec3"`) so the aliased
spelling has something to resolve. Both `defrecord` field hints and function
parameter hints use this resolution.

## Soundness and the checked mode

An accurate hint is correctness-preserving by construction: for a struct or
record the bare get equals the guarded result. An inaccurate hint (asserting
`^:struct` for a value that is actually a phm) makes the raw get return the wrong
thing. This is the same contract as a wrong Clojure `^String`, except that a
wrong Jolt hint fails silently rather than throwing.

To make a lie visible without taxing the fast path, `JOLT_CHECK_HINTS=1` keeps
the guard but throws on the tagged arm with a message naming the local and key:

```
type hint violated on `m`: (:a m) — value carries :jolt/type
(a phm/sorted/transient/lazy-seq), not the plain struct/record the
^:struct/^Record hint asserts
```

This is a development aid, off by default, with zero cost to normal builds (the
flag is read when the lookup is compiled, and the bare get is emitted when it is
off). The flag is part of the image-cache fingerprint.

## Coverage

Type hints parse in every position Clojure accepts them and are inert except for
the optimization above. This matches Clojure's "parse and otherwise do nothing"
model, with the difference that Clojure additionally uses hints to avoid
reflection and select primitive arithmetic, which do not apply to a Janet host.

## Measured effect

On the ray tracer (`~/src/examples/ray-tracer`, all values are `{:r :g :b}`-style
maps), with inlining on and the hot parameters hinted, a render goes from 13.3s
to 10.9s, about 1.22x, taking it to roughly 7.8x the JVM from 9.4x after the
inline pass. A seeded render produces an identical pixel checksum hinted and
unhinted, confirming the hints are correctness-preserving on the full pipeline.

## Status and non-goals

Implemented. Not pursued: inter-procedural shape inference (unsound in a dynamic
language without a guard, which costs as much as the one being removed) and a
shape-based "hidden class" representation (profiling showed allocation is about
1% of the workload, so a cheaper allocation would not help, and an escaping-map
lookup through a runtime shape check costs about the same as the guard it would
replace). The hint is the sound, opt-in lever on the part of the cost that can
move.
