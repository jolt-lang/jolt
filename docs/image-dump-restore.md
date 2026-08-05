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

**The escape hatch works and is the design's foundation.** `fasl-write` takes an
`externals-pred`; every object it flags is written as a placeholder, and
`fasl-read` takes a vector of replacements. Probed:

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

**Closures are partially recoverable after all.** With
`generate-inspector-information` on, `(inspect/object p)` yields the closure's
free-variable *names* and *values*, and `((o 'code) 'source)` yields the source
expression:

```
(mk 5 "hi") => free vars ((msg . "hi") (n . 5)), source (lambda (x) (list (+ x n) msg))
```

With inspector info off, source and name go to `#f` (free-variable count
survives). jolt's `release` profile sets `generate-inspector-information #t`
(build.ss:1238) — it's what makes native backtraces work — and only the
`optimized` profile turns it off (build.ss:1234). So the introspection path is
available in default release builds. It is slow, it recovers *Scheme* source
rather than jolt source, and it needs an `eval` on restore, so it belongs behind
a flag, not on the default path.

## 3. Design

Portable-dumper shape, cl-store constraints: dump the mutable state graph rooted
at the var table, pair it with a build identity, and restore by starting the
same binary, replaying the requires, and re-installing state.

### Encoding

Chez `fasl` + `externals-pred` for the image body. This buys cycles, structural
sharing, and record identity for free, and leaves us responsible only for
describing the leaves Chez refuses.

The obvious objection is that it welds the format to a Chez version. It doesn't
add a constraint: the generative-rtd result above already restricts an image to
the exact build that wrote it, and a jolt binary embeds its own Chez boot. A
build-independent EDN-ish encoding is a later phase, not the first one.

Every external gets a descriptor:

- **named code** — a procedure that is some var's root, or is in the direct-link
  registry → `(fn-ref "ns/name")`. Resolved through the var table on restore.
- **host resource** — port, socket, process, thread → handler-supplied
  descriptor, re-acquired on restore.
- **reconstructible closure** (opt-in) — source sexp + named free values from
  the inspector, re-`eval`'d on restore.
- **anything else** — refuse, and report the path through the graph to it.

That last point is the usability make-or-break. SBCL and cl-store both fail
without telling you *which* object, and it's miserable. `scan` reports
`#'myapp.core/state → :handlers → "GET /x" → #<procedure>` and the feature is
usable; it reports "unserializable object" and it isn't.

### Root set

1. `var-table` (rt.ss:468) — every var cell's root
2. `ns-registry`, `ns-alias-table`, `ns-refer-table`, `ns-refer-all-table` and
   the exclude table (ns.ss:20-62)
3. atoms, refs, agents reachable from those roots, plus their watches and
   validators — which are procedures, so they go through the code-ref path
4. multimethod tables (multimethods.ss), protocol dispatch tables (records.ss),
   `*data-readers*`
5. **not** dynamic per-thread bindings (dyn-binding.ss) — thread-local, and
   there are no threads on restore
6. **not** open ports, sockets, child processes, threads — handler registry

### Header

jolt version, Chez version, machine-type, app build hash (reuse the FNV-1a
content hash the AOT cache already computes), and the ordered list of loaded
namespaces. Restore refuses on any mismatch unless `--force`. Given the silent
stale-rtd corruption above, this check is load-bearing, not hygiene.

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

Hooks are SBCL's `*save-hooks*` / `*init-hooks*` and exist for the same reason.
CLI: `jolt run --restore img.jimg`, plus a dump trigger over nREPL.

Note the manifest gate — new `jolt.host` `def-var!`s need a
`jolt-host-manifest.txt` line, and `make manifestcheck` enforces it.

## 4. Rounds (TDD, one PR each)

- **R0 — encoding spike.** Harness in `test/chez/` pinning the probe results
  above against jolt's own value types, so a Chez upgrade that changes fasl
  behavior fails a test rather than an image.
- **R1 — rtd stability.** Add `nongenerative` to the image-relevant record
  types listed in §2. Regression: write in process A, recompile, read in B, and
  assert predicates still hold. Pure runtime change, no re-mint.
- **R2 — walker + `scan`.** Classify every reachable object; no writing yet.
  Tests: each jolt value type classifies correctly; every unserializable object
  reports a readable path.
- **R3 — encoder/decoder, value core.** Round-trip identity: `=`, structural
  sharing, cycles, metadata, record types, sorted/array maps.
- **R4 — code refs.** Build the procedure→fqn map from the var table plus the
  direct-link registry. Round-trip a var holding a fn, a multimethod, a protocol
  impl, an atom with a watch.
- **R5 — root-set capture, end-to-end.** `dump!` / `restore!` on a nontrivial
  app; restore in a fresh process and assert behavior, not just structure.
- **R6 — header + refusal.** Build identity, `--force`, and a test that a
  mismatched image is *rejected* rather than silently restored.
- **R7 — handlers + hooks.** Registry, plus stdlib handlers for files, sockets,
  and child processes. Agent/thread quiescing.
- **R8 — CLI, nREPL, docs.** Including an explicit "state image, not process
  image" section.
- **R9 (optional) — closure reconstruction** via the inspector, behind a flag;
  and `--portable` EDN encoding for cross-build restore.

Perf gate: dumping must add no per-operation cost. `emit-diff` should be
byte-identical for builds that never call the API, and the standing 1.1x
regression ceiling applies.

## 5. Open questions

1. **Cross-build restore — required, or is same-build enough?** Same-build is
   R0-R8 as scoped. Cross-build means a name-keyed portable encoding, versioned
   record layouts, and a migration story. Materially bigger.
2. **Scope of "different machine"** — same OS/arch, or also cross-arch? The
   fasl route is arch-tied; cross-arch forces the portable encoding.
3. **Default on unserializable** — `:fail` (safe, noisy) vs `:omit` (convenient,
   silently lossy). Recommend `:fail`.
4. **Is R9 wanted at all**, or do we tell users to store `[fn-name args]`
   descriptors instead of closures, the way Clojure users already do?
