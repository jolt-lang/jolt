# Image dump / restore — research + design plan

Goal: an API that writes the state of a running jolt program to a file, and
restores it in a fresh process, possibly on another machine.

Status: design only. Nothing implemented. Branch `feat/image-dump-restore`.

## 1. Prior art

### Common Lisp

Every major CL does the same thing: dump the whole heap, restore into the same
VM.

- SBCL `sb-ext:save-lisp-and-die`. Documented constraints: it corrupts the
  running image, so the process must die afterwards (the usual workaround is to
  fork a child that saves); only one thread may remain live after
  `*save-hooks*` run, so multi-threaded apps register a save-hook that stops
  threads and an init-hook that restarts them; open streams cannot be preserved
  across save/load because the OS won't allow it; `load-shared-object` needs
  re-resolution on startup.
- The portability line from the SBCL manual is the important one: *"There is
  absolutely no binary compatibility of core images between different runtime
  support programs. Additionally, even runtimes built from the same sources at
  different times are treated as incompatible."* Shipping a core to another
  machine means shipping the runtime with it.
- CCL `save-application`, LispWorks `save-image`/`deliver`, CLISP
  `saveinitmem`, Allegro `dumplisp` — same shape, same constraints.
- When people actually need cross-machine or cross-version transfer they drop
  to a data serializer (`cl-store`, `conspack`). Those write data; functions
  survive only as symbol references, closures not at all.

### Scheme

- **Chez had this and removed it.** v6/v7 had `save-heap` / `load-heap` and the
  `-s`/`-h` command-line options. Current Chez recognizes `-h`/`--saveheap` but
  prints an error and exits: saved heaps are not supported. There is no heap
  dump in Chez 10.
- Gambit is the outlier that genuinely serializes code: `object->u8vector` /
  `u8vector->object` handle procedures *and* continuations, with an optional
  transform procedure invoked on every sub-object so the program can supply its
  own encoding for otherwise-unserializable things like ports and threads.
  This is what makes Termite's process migration possible, and Mobit exists to
  give Termite unrestricted serialization of procedures and continuations. It
  costs compiler/runtime cooperation: code has to be addressable by a stable
  global identifier instead of a raw pointer.
- Racket: no image dump. `racket/serialize` + `serializable-struct` covers
  data; code must be named.
- Guile, Chicken: no image dump.

### Outside the Lisp family

- **Smalltalk / Pharo** is the system everyone means when they say "image".
  It works because activation records are first-class objects, so a suspended
  process is just more object memory. Resuming a running program is real there,
  not emulated.
- **Emacs** replaced `unexec` (rewrite the executable in place) with the
  **portable dumper** in Emacs 27: walk the Lisp heap, write the objects plus
  relocation information to `emacs.pdmp`, load and relocate at startup. It is
  PIE/ASLR-clean, malloc-implementation independent, and matches unexec startup
  time. It is still same-build-only, and it only knows about the Lisp heap —
  C-side state has to be registered with the dumper explicitly.
- CRIU / CRaC checkpoint the OS process instead. Different tradeoff: captures
  in-flight execution, but is Linux-specific and requires the same kernel and
  filesystem surface on restore.

### What the prior art says for us

Two viable families, and Chez picks for us:

1. *Heap image* (SBCL, Emacs, Pharo). Needs the runtime to be able to walk and
   relocate its own heap. Chez does not expose this and removed the feature.
   Off the table without patching Chez.
2. *Logical image* (cl-store, Racket serialize, Termite). Serialize the state
   graph; code travels by name, not by pointer. This is what we can build.

So: **jolt images are state images, not process images.** No in-flight
computation, no continuations, no thread stacks. That limit needs to be in the
docs from day one, because "dump a running program" invites the Pharo reading.

## 2. What Chez 10.4.1 actually gives us

`fasl-write` / `fasl-read` is the only serializer. Probed empirically on the
local Chez 10.4.1 (`--script` mode and via `compile-file`):

**Round-trips cleanly:** vectors, lists, strings, bytevectors, numbers
(fixnum/flonum/bignum/ratnum/complex), records, conditions, mutexes, condition
variables, weak pairs, gensym identity, cyclic structure, shared structure
(two references to one object stay `eq?` after read), **eq-hashtables**.

**Rejects with `invalid fasl object`:**

- any procedure or closure — including one produced by `(compile '(lambda ...))`
- code objects
- continuations
- ports
- threads
- parameters (they are procedures)
- **eqv-, equal-, and string-hash hashtables** — they carry hash/equiv
  procedures. Only `eq` hashtables survive.

**The escape hatch works**, though the cross-arch requirement (§3) means we end
up not using it. `fasl-write` takes an `externals-pred`; every object it flags
is written as a placeholder, and `fasl-read` takes a vector of replacements.
Probed:

- the predicate is consulted on sub-objects nested inside records and vectors,
  not just the root
- structural sharing across externals is preserved — one procedure referenced
  twice yields one external and stays `eq?` after read
- cycles through records still round-trip with externals in play
- a wrong-length externals vector fails loudly:
  `incompatible fasl graph external vector length 0 (expected 1)`

**Record type identity is the sharp edge.** Chez matched the `rtd` to the
already-loaded one in every probe, so records restore into live types — but
only for *nongenerative* record types. Tested with two records in one
compiled file, written by process A and read by process B:

| | same `.so` | after recompiling the `.so` |
|---|---|---|
| `(nongenerative my-v1)` | predicate `#t`, accessors work | predicate `#t`, accessors work |
| generative (no clause) | predicate `#t`, accessors work | **predicate `#f`, accessor raises** |

The recompile case did **not** fail at `fasl-read`. It produced a record of a
stale type that silently fails its own predicate. That is a corruption mode, and
it is the single strongest argument for a hard build-identity check in the
header.

This bites us today: a large share of the runtime's record types have no
`nongenerative` clause, including value types an image must carry —
`pvec` (collections.ss:56), `jolt-atom` (atoms.ss:22), `jolt-lazyseq`
(lazy-bridge.ss:19), `jolt-multifn` (multimethods.ss:42), `jolt-ex-info-record`
(rt.ss:176), `jolt-ref` (refs.ss:13), and the `jrec` substrate behind
user-defined records (records.ss:69-77). `pmap`, `pset`, `hnode`, `hcoll`, and
`var-cell` already carry one.

**Closures carry a recoverable identity.** This is the result the whole design
turns on. With `generate-inspector-information` on, `inspect/object` yields a
closure's free-variable *names and values* in a stable order, and its code
object yields a *name* — provided the lambda is bound rather than bare:

```
(lambda (x) (+ x n))                        name=#f          free=((n . 5))
(letrec ((myfn (lambda (x) (+ x n)))) myfn) name="myfn"      free=((n . 7))
(define (inner-fn x) (+ x n)) inner-fn      name="inner-fn"  free=((n . 9))
case-lambda, letrec-bound                   name="ca"        free=((n . 3))
```

A `letrec` self-binding is exactly what jolt's `emit-fn` already emits (see the
note in source-registry.ss). Verified to survive `compile-file` at
`optimize-level 2` with `generate-inspector-information #t` — jolt's `release`
profile (build.ss:1238); only `optimized` turns inspector info off
(build.ss:1234):

```
compiled O2 +inspector -> name="cf-handler" nfree=2 free=((tag . "T") (n . 11))
```

**But the names collide.** Two fn literals in different scopes that share a
short name both report it:

```
(a1 1) -> name="handler" free=((n . 1))
(a2 2) -> name="handler" free=((n . 2))
```

This is the same ambiguity source-registry.ss already documents and defends
against with its `'ambiguous` marker. Short names are not code identities. The
back end has to emit *globally unique* fn-literal names before they can be.

## 3. Design

Decisions taken: cross-architecture restore, fail-with-path on unserializable
objects, closures first-class from the start. Those three together rule out
Chez `fasl` for the image body and push the design onto Gambit's model.

### Closures are the architecture, not an add-on

Gambit/Termite serializes procedures by addressing code with a stable global
identifier instead of a raw pointer. jolt is a compiler, so it can do the same
thing at its own seam — and the probe above says the runtime half already
almost works.

For each fn literal the back end emits:

1. a **globally unique** munged name — the code-id — instead of today's short
   name, and
2. a **reconstructor**: a top-level procedure abstracted over that literal's
   free-variable set, returning the closure. The analyzer already computes free
   variables for closure conversion, so the set is known at emit time.
3. a registration `code-id → reconstructor`, emitted once at definition time
   like the existing `jolt-register-source!` — zero per-call cost.

Then:

- **dump** a closure → `inspect/object` gives `(code-id, ordered free values)`;
  the values recurse through the normal encoder
- **restore** → `(apply reconstructor decoded-free-values)`

Nothing changes about how fns are represented at runtime. They stay raw Chez
procedures, so the 1.1x regression ceiling is not in play — the cost is emit-time
and dump-time only. The code-id is a string and the captured values are data, so
this is architecture-independent by construction, which is exactly what the
cross-arch decision requires.

The unique-naming change is the prerequisite, and it pays a second dividend:
source-registry.ss's `'ambiguous` fallback exists only because short names
collide, so unique names also un-break native backtraces for the colliding case.

Unresolved and needing a probe in R0: **shared mutable captures.** When two
closures capture the same `set!`-mutated binding, Chez boxes it. The inspector
surfaces a value; whether it surfaces the shared box such that both closures
still share it after restore is unverified. If it doesn't, the encoder must
detect co-captured mutable bindings and rebuild the sharing explicitly.

### Encoding

A jolt-owned tagged binary format with an explicit object table (index-addressed),
which is what gives us cycles and structural sharing. Not `fasl` — it is
arch-tied and cannot carry code refs.

Cross-arch obligations, all in the format rather than left to Chez: explicit
little-endian byte order; IEEE-754 for flonums with negative zero and the NaN
payload pinned; bignums as sign + magnitude bytes; ratnums as a numerator/
denominator pair; records keyed by the jrec type's `ns/name`, not by a Chez rtd.

Keying records by name is what makes the §2 generative-rtd hazard irrelevant on
this path — nothing depends on Chez rtd identity. The `nongenerative` gap is
still a latent bug for anything else that fasls jolt values, so it stays in the
plan, just off the critical path.

Encoded object kinds:

- **value** — persistent collections, records, keywords, symbols, strings,
  numbers, metadata
- **fn-ref** — a procedure that is some var's root, or is in the direct-link
  registry → `"ns/name"`, resolved through the var table on restore
- **closure** — `(code-id, free values)` per above
- **host resource** — port, socket, process, thread → handler-supplied
  descriptor, re-acquired on restore
- **anything else** — refuse, and report the path through the graph

Path reporting is the usability make-or-break. SBCL and cl-store both fail
without saying *which* object and it is miserable. `scan` reports
`#'myapp.core/state → :handlers → "GET /x" → #<procedure>` and the feature is
usable; it reports "unserializable object" and it isn't.

### Root set

1. `var-table` (rt.ss:468) — every var cell's root
2. `ns-registry`, `ns-alias-table`, `ns-refer-table`, `ns-refer-all-table` and
   the exclude table (ns.ss:20-62)
3. atoms, refs, agents reachable from those roots, plus their watches and
   validators — procedures, so they take the fn-ref or closure path
4. multimethod tables (multimethods.ss), protocol dispatch tables (records.ss),
   `*data-readers*`
5. **not** dynamic per-thread bindings (dyn-binding.ss) — thread-local, and
   there are no threads on restore
6. **not** open ports, sockets, child processes, threads — handler registry

### Header

jolt version, image format version, source arch, target-independence flag, app
build hash (reuse the FNV-1a content hash the AOT cache computes), and the
ordered list of loaded namespaces. Restore refuses on a namespace or
format-version mismatch unless `--force`; arch mismatch is expected and allowed.

Code-ids are content-addressed by fn identity, so a restore into a *different
build* fails closed: an unknown code-id is an error, not a silent misbind. That
is the §2 stale-rtd lesson applied to code.

### API — `jolt.image`

```clojure
(dump! path)
(dump! path {:on-unserializable :fail  ; | :omit | :call
             :handlers {...}})
(restore! path)
(scan)          ; dry run: report every unserializable object with its path
(dumpable? x)
(register-handler! pred dump-fn restore-fn)
(add-before-dump-hook! f)   ; quiesce: stop agent pools, app threads
(add-after-restore-hook! f) ; restart them
```

`:fail` is the default. Hooks are SBCL's `*save-hooks*` / `*init-hooks*` and
exist for the same reason. CLI: `jolt run --restore img.jimg`, plus a dump
trigger over nREPL.

Note the manifest gate — new `jolt.host` `def-var!`s need a
`jolt-host-manifest.txt` line, and `make manifestcheck` enforces it.

## 4. Rounds (TDD, one PR each)

- **R0 — probe harness.** Pin §2's Chez behavior as tests so an upgrade fails a
  test rather than an image. Must settle the shared-mutable-capture question
  before R3 is designed.
- **R1 — unique fn-literal names.** Back end emits globally unique munged names.
  Regression: two same-short-named fn literals in different scopes get distinct
  code-ids; source-registry.ss stops marking them `'ambiguous`. Needs
  `make remint` — back-end changes silently no-op without it.
- **R2 — reconstructors.** Emit + register `code-id → reconstructor` per fn
  literal. Test by reconstructing closures in-process, no serialization yet.
- **R3 — walker + `scan`.** Classify every reachable object, report paths. No
  writing.
- **R4 — encoder/decoder, value core.** Round-trip identity: `=`, structural
  sharing, cycles, metadata, record types, sorted/array maps. Cross-arch cases
  (bignum, ratnum, -0.0, NaN) covered here.
- **R5 — code refs and closures.** fn-refs via the var table; closures via
  code-id + free values. Round-trip a var holding a fn, a multimethod, a
  protocol impl, an atom with a watch, a closure over a mutable binding.
- **R6 — root-set capture, end-to-end.** `dump!` / `restore!` on a nontrivial
  app; restore in a fresh process and assert behavior, not just structure.
- **R7 — header + refusal.** Build identity, `--force`, and tests that a
  mismatched image and an unknown code-id are *rejected* rather than silently
  restored.
- **R8 — handlers + hooks.** Registry, plus stdlib handlers for files, sockets,
  and child processes. Agent/thread quiescing.
- **R9 — cross-arch proof.** Dump on arm64, restore on x86-64 in CI. This is the
  round that actually validates the headline requirement; everything before it
  is same-arch.
- **R10 — CLI, nREPL, docs.** Including an explicit "state image, not process
  image" section.
- **Off critical path** — add `nongenerative` to the record types listed in §2.
  Real latent bug, but the name-keyed encoding means images no longer depend on
  it. Track separately.

Perf gate: dumping must add no per-operation cost. Unique fn names and
reconstructor registration are emit-time; verify `emit-diff` shows no change to
generated call sequences, and hold the standing 1.1x ceiling.

## 5. Risks

1. **Shared mutable captures** (§3). Unverified; R0 settles it. If the inspector
   flattens boxes, the encoder needs explicit co-capture detection.
2. **`generate-inspector-information` dependency.** Closure dumping does not
   work in the `optimized` profile, which turns it off. Either document that
   images require `release`, or have the back end record free-variable layout
   itself rather than reading it back from Chez. The latter is more work and
   removes the dependency — worth deciding at R2.
3. **Reconstructor emission bloats binaries.** One extra top-level per fn
   literal. Needs measuring at R2; may want to gate behind a build flag, though
   that would make images a build-time opt-in.
4. **Code-id stability across recompiles.** Content-addressing makes an
   unrelated edit invalidate unrelated code-ids only if the hash inputs are too
   broad. Getting the granularity right decides whether images survive a
   no-op rebuild.
