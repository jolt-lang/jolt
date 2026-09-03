# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **The class rows typed.clojure's annotation corpus names.** `java.lang.ref.Reference`
  (abstract, over the `SoftReference` / `WeakReference` shims, which now report
  their own classes instead of `:object`, plus `ReferenceQueue` and
  `.refersTo`), `java.util.RandomAccess` (on `APersistentVector` and
  `ArrayList`), `java.util.Comparator` (on `AFunction`, so a fn is one and
  answers `.compare`), `clojure.lang.MultiFn` (an `AFn`), the whole transient
  lattice (`ITransientCollection` … `ITransientSet`, `ATransientMap` /
  `ATransientSet`, and the four concrete transient classes, so `bases`, `isa?`
  and `instance?` on a transient answer the JVM's ancestry instead of
  `AFunction`'s), and `java.util.SequencedCollection` (JDK 21) between
  `Collection` and `List` / `Deque`, with `getFirst` / `getLast` / `reversed`
  on vectors, lists and seqs and `getFirst` / `getLast` / `addFirst` / `addLast`
  / `removeFirst` / `removeLast` on `ArrayList`. `typed.ann.clojure.base` and
  `typed.ann.clojure` load; `override-classes` stopped at
  `java.lang.ref.Reference` before.
- **`String/CASE_INSENSITIVE_ORDER`.** String's one public static field: a
  `Comparator` of `String$CaseInsensitiveComparator` that compares like
  `compareToIgnoreCase` (a char difference, length last), usable by `sort`,
  `sort-by`, `sorted-set-by` and `.compare`, and findable by `.getField`.
- **`Character/getType` and the general-category constants.** The Unicode
  general category of a char or int codepoint as the JVM's constant, with
  `Character/UNASSIGNED` through `Character/FINAL_QUOTE_PUNCTUATION` (all 30;
  17 is unused, as on the JVM). An int that is not a Unicode scalar value
  answers `UNASSIGNED`, a surrogate `SURROGATE`, as on the JVM.

### Changed

- **`-M` with nothing to run starts a REPL.** `jolt -M:test` where no selected
  alias declares `:main-opts` and the command line adds none used to exit 1
  with `alias(es) [:test] have no :main-opts`. `-M` is `clojure.main`, and
  `clojure.main` with no arguments is a REPL — `clj -M:dev` is how a REPL over
  an alias's extra deps is started — so jolt does the same now, over the
  project resolved with those aliases. `:main-opts ["-r"]` (or `--repl`) asks
  for one explicitly, as on the JVM. A `:main-opts` form jolt does not take
  says which it does: `-m NS`, `-e EXPR`, `-r`, or a script file.

### Fixed

- **A host fault a catch binds is a typed throwable.** A primitive handed the
  wrong value — `(.concat "a" nil)`, `(subs "abc" 5)`, `(clojure.string/trim
  nil)` — raised a raw Chez condition, and a catch bound it as it was:
  `(class e)` answered `:object`, `ex-message` nil, `(instance? Exception e)`
  false, `pr-str` printed `#object[:object]`, and every `RuntimeException`
  clause matched it, so `(catch ArithmeticException e …)` caught a nil
  argument. A raw condition becomes a typed throwable at the catch boundary
  now, classified by what Chez reported: a nil argument is a
  `NullPointerException`, a wrong-typed one a `ClassCastException`, a bad
  index an `IndexOutOfBoundsException` (`StringIndexOutOfBoundsException`
  from a string primitive), a wrong argument count an `ArityException`,
  division by zero an `ArithmeticException`, an i/o failure an `IOException`,
  anything else a `RuntimeException`. Catch clauses dispatch on that class, a
  rethrow keeps the same object, a future's `.getCause` is it,
  `.printStackTrace` prints the frames that led to the fault, and the message
  names the primitive with the offending value printed as a jolt value:
  `string-append: nil is not a string`, not `#[jolt-nil-v1] is not a string`.
  typed.clojure's `check-form*` rethrows such a fault and reported
  `#object[:object]`.
- **A `.method` call in tail position is a trace site.** `(defn f [s] (.concat
  s nil))` erased `f` from the trace: the tail call dropped its frame and,
  unlike a fn call, the interop call stored no site pair, so the report and
  `.printStackTrace` began at the caller. The host call carries its form's
  line now and stores the site the way a tail fn call does; a fault caught by a
  `catch` snapshots the site at the catch boundary too, since the guard's own
  handler is nearer than the uncaught-path capture.
- **The uncaught report names the class.** `Unhandled exception
  (NullPointerException): …` for a typed throwable. An `ExceptionInfo` and a
  bare `Exception` keep the message-only line, as the reference's report does.
- **Throws whose class the JVM answers differently.** `(name nil)`,
  `(namespace nil)`, `(deref nil)`, `(var-get nil)` and `(key nil)` are
  `NullPointerException`s (were `ClassCastException` or `ExceptionInfo`);
  `(key 1)`, `(val 1)` and `(rseq 1)` are `ClassCastException`s (were
  `ExceptionInfo`); `(sorted-map 1)` is an `IllegalArgumentException` "No
  value supplied for key: 1"; `(nth "abc" 5)` is a
  `StringIndexOutOfBoundsException`; `(.get an-array-list 3)` past the end is
  an `IndexOutOfBoundsException` (it answered nil); `(into {} [[1]])` is an
  `IllegalArgumentException` "Vector arg to map conj must be a pair" (it
  answered `{1 nil}`, though `conj` already refused the pair); `(spit nil "x")`
  is an `IllegalArgumentException` "Cannot open <nil> as a Writer." (it wrote
  a temp file into the working directory, then failed to rename it); and
  `(+ nil)` / `(* nil)` are nil, as the reference's single-operand cast makes
  them (were `NullPointerException`).
- **`(into nil coll)` is a list.** `into` folded through `conj` on the nil
  target directly instead of starting a list the way `(conj nil x)` does, and
  died inside the host (`string-append: nil is not a string`) for any
  non-empty source, in both the plain and the transducer arity; `(into nil
  [1 2])` is `(2 1)` and `(into nil [])` nil, as on the JVM. typed.clojure's
  pass scheduler does `(into affects (filter passes after))` with `affects`
  nil, which is where `(t/cf (inc 1))` stopped.
- **A record class constructs from its fields plus `__meta` and `__extmap`.**
  `(R. f1 … fn meta ext)` is the JVM record class's second constructor, and
  what a macro building records without the positional factory emits
  (typed.clojure's `create-expr` expands to `new` with all of them); jolt's
  constructor took exactly the fields and raised an `ArityException`. The
  extra two attach the metadata and carry the map as extension fields.
- **`(ns-resolve ns env sym)`.** The three-argument form, which answers nil for
  a symbol the local environment binds, was missing (`resolve` had its env
  arity); typed.clojure's analyzer resolves through it with `&env`.
- **A record's `__meta` and `__extmap` read as fields.** The JVM record class
  has both as public fields, and typed.clojure's `update-expr` reads
  `(.-__extmap e)` to carry an expression's extra keys; jolt raised "No
  matching field found". `__extmap` is the map of extension keys (nil when
  there are none), `__meta` the metadata.
- **`satisfies?` on a class or interface answers `instance?`.** jolt takes
  `:bb` reader branches, and code written for babashka asks `(satisfies?
  clojure.lang.IObj x)` where its JVM branch asks `instance?` (typed.clojure's
  `obj?`); it raised "satisfies? expects a protocol". The JVM raises on the
  class form and babashka answers false for everything; jolt answers the
  question the code means.
- **`clojure.lang.RT/classForName` and `classForNameNonLoading`.** The same
  answer as `Class/forName`, including `ClassNotFoundException`; typed.clojure's
  analyzer resolves class symbols through the RT statics.
- **`bases` answers Class objects.** It handed back name strings where `supers`
  handed back classes, so `(.getName (first (bases c)))` failed on every class
  (typed.clojure builds its RClass ancestry exactly that way). Superclass first,
  as on the JVM, with `Object` leading a class whose row names no concrete
  super.
- **`Class.getModifiers` on a nested class.** Every nested class jolt models is
  a static nested class on the JVM, so the STATIC bit is set for a `$` name the
  graph knows; the four transient classes and `PersistentArrayMap$Seq`,
  `PersistentHashMap$NodeSeq` and `PersistentList$EmptyList` are
  package-private, `String$CaseInsensitiveComparator` private, and
  `Thread$State` an enum — all probed. `(.getModifiers (class (transient #{})))`
  is 24, `java.util.Map$Entry` 1545, as on the JVM (1 and 1537 before).
- **`(str an-interface-class)` says `interface`.** `java.util.List` rendered as
  `class java.util.List`.
- **A persistent collection refuses the `java.util` mutators with
  `UnsupportedOperationException`.** `.add`, `.set`, `.remove`, `.clear`,
  `.sort` and the rest on a vector, list, seq or set raised an
  `IllegalArgumentException` "no matching method", which a
  `(catch UnsupportedOperationException …)` never saw.
- **A named fn carries the same ancestry as an anonymous one.** Its protocol
  dispatch tags were a hand-copied list with no `Comparator`, `Runnable` or
  `Callable`, so `(instance? java.util.Comparator inc)` was false while the
  same question on `(fn [a b] 0)` was true; the list derives from the class
  graph now.
- **A native iOS build no longer reports itself as Linux.** #796 fixed the
  portable-bytecode half of `sa-os-family` — a `pb` tag names no OS, so the
  `else` branch called every bytecode build Linux, and it probes the filesystem
  now. The native half still fell through: Chez's four iOS tags (`a6ios`,
  `arm64ios`, `ta6ios` and `tarm64ios`, the last documented in `BUILDING` as
  the iOS cross-target) contain none of `osx`, `macos`, `nt` or `pb`, so all
  four reached that same `else` and called a Darwin system Linux. iOS is Darwin
  and `ios` joins the macOS branch, which is the whole fix: that one function is
  where the host asks what OS this is, so the wrong answer was `SIGCHLD` 17
  instead of 20, `EAGAIN` 11 instead of 35, `O_NONBLOCK`, `LC_TIME`, the
  `struct stat` offsets, the chmod and entropy fallbacks and the link libraries,
  all at once — and, as in #796, `jolt.nrepl` handing Darwin's `socket()` the
  Linux `SOCK_CLOEXEC`, which cannot bind. `jolt build --target tarm64ios
  --library` also linked the output with ELF's `-shared` rather than
  `-dynamiclib -install_name`, because the build's target-side Darwin predicate
  matched only `osx`; it takes both spellings now. Android needs no such branch
  and gets none: it has no Chez tag of its own, cross-builds as `tarm64le`, and
  Bionic is Linux for every constant these select — its divergence is the link
  libraries, which no tag can express (`tarm64le` is glibc arm64 Linux too) and
  the target pack owns. The gate pins the iOS and `tarm64le` rows through
  `sa-os-family-for-tag`, from hosts that are neither.
- **A child process no longer inherits `JOLT_PWD`.** `bin/jolt` exports the
  user's directory in `JOLT_PWD` before changing into its checkout, and a
  spawned child's environment was seeded from the parent's, so a child jolt
  started with `:dir` at another project took its user.dir from the variable
  rather than from its own working directory: `(slurp "README.md")` under
  `:dir` answered the parent's README. The variable is the launcher's message
  to the process it started, and the child's directory is whatever the spawn
  set, so `ProcessBuilder`, `jolt.process` and `clojure.java.shell` now start
  every child without it — a child jolt, directly under `:dir` or behind a
  shell's `cd`, reads its own project. A caller that puts `JOLT_PWD` in the
  child's environment map asked for it and keeps it. An installed binary never
  exports the variable, which is why the same program passed under one and
  failed under a source checkout.
- **`clojure.lang.Agent`, `AMapEntry`, `ChunkBuffer`, `IAtom`, `IAtom2`,
  `IBlockingDeref`, `IChunk`, `IMapEntry`, `Reduced` and `Volatile` resolve.**
  typed.clojure's core annotations name all ten, and its `override-classes`
  stopped at the first: `Could not resolve class: clojure.lang.ChunkBuffer`.
  Each is in the class graph under its JVM supers now, so `resolve`, `import`,
  `Class/forName`, `bases`, `supers`, `.isInterface`, `.getModifiers` and
  `instance?` agree: an atom is an `IAtom2`, a promise and a future are
  `IBlockingDeref`, a reduced box is an `IDeref` and reports
  `clojure.lang.Reduced` (it was `:object`), `MapEntry` extends `AMapEntry`
  which is an `IMapEntry`, and a chunk buffer is `Counted` and answers `count`.
  Three general faults came out with them. Protocol dispatch never consulted
  the class a value reports through a class arm, so `extend-protocol` on
  `clojure.lang.Volatile`, `Agent`, `Delay`, `Ref` or a promise's class — or on
  `IDeref` for any of them — threw `No method`; dispatch now reads the same
  class `instance?` does, and the hand-kept list of derefable values that
  answered `instance?` for `IDeref`/`IRef`/`IPending` is gone in favour of the
  graph. A class registered with no supers was read as unregistered, and a `$`
  in its name then made it a fn, so `(supers java.util.Map$Entry)` and the
  promise and future reify classes reported the `AFunction` chain, `Fn`
  included. And `Class/forName` rejected every interface `resolve` accepts
  (`"clojure.lang.IDeref"`) because it consulted a narrower table; it answers
  for every modeled class now, under its full name only. `chunk` still seals
  into a vector rather than an `ArrayChunk`, so nothing is an `IChunk`
  (known-divergences).
- **`Character/isLetter`, `isDigit`, `isLetterOrDigit`, `isUpperCase` and
  `isLowerCase` classify all of Unicode.** They were ASCII range checks, so
  `(Character/isLetter \é)` and `(Character/isUpperCase \É)` were false where
  the JVM says true. They now apply the JVM's category rules over the same
  Unicode general category `getType` reports: `isLetter` is L*, `isDigit` is Nd
  (so `\½` is not a digit), and the case predicates are the Uppercase/Lowercase
  properties (so `\Ⅰ` is upper case, `\ª` lower case, `\ǅ` neither). An int
  codepoint that is not a scalar value is still false throughout.
- **A caught load error no longer misplaces every later error.** The
  `at file:line:col` line under an uncaught error is the top-level form that
  was evaluating, and a file load deliberately left that position on its
  failing form when it threw, so the report named the file that failed. When
  the throw was *caught* — a `data_readers.clj` namespace the loader tolerates,
  a `require` inside a `try` — the position stayed put anyway, and every later,
  unrelated error was reported at it: a CLI argument error came out `at
  …/clj_time/core.clj:254:1`, the form whose Joda class the clj-time data-reader
  load had stumbled on. A file load now binds the position around the whole
  load, and the failing form's position travels with the throw itself (recorded
  by the innermost load it crosses, before the stack unwinds), which the report
  reads back — so a propagating load error is placed exactly as before, and a
  caught one leaves nothing behind. `JOLT_DIAG=edn` diagnostics read the same
  position.
- **The data-reader load warning says where and what.** `data-reader namespace
  clj-time.coerce failed to load: Unknown class DateTimeZone` now carries the
  position the load failed at and the tags that consequently have no reader
  (`tags #clj-time/date-time will not read`), instead of leaving the reader to
  find the form and to meet the missing tag later as an unrelated
  unresolved-var error.
- **A returned call no longer haunts a top-level throw.** The trace under an
  error thrown at the top level of a file being loaded — or of a `-e`, a
  `load-string`, a REPL input — opened with a frame from a fn that had returned
  long before: `jolt.main/drop-end-of-options` under a `run -m` whose namespace
  failed to load, a helper's tail call from the fn that called `require`, a
  macro's helper when the expansion threw. The tail-site slot the reporter reads
  holds the last tail call made, and nothing tail-calls a top-level form, so
  the slot is cleared when one starts to compile and again when its compiled
  code starts to run.
- **Every CLI entry starts in `user`.** The built image bakes `jolt.main` and
  loading a namespace leaves it current, so a bare `jolt -e '(str *ns*)'`
  printed `jolt.main`, a REPL `(defn h …)` landed as `#'jolt.main/h` under a
  prompt that said `user`, and `-main` under `run -m` ran in `jolt.main`.
  `clojure.main` starts every entry in `user`; jolt does too now, and the REPL
  prompt names whatever namespace is current, as `clojure.main`'s does.

## [0.8.1] - 2026-09-02

Host classes are provided by declaration now. The runtime no longer carries the
list of which library implements `java.time.ZoneOffset` or `javax.crypto.Mac`:
a library declares the classes it provides in its own `deps.edn`
(`:jolt/provides`, RFC 0014), and `jolt.deps` installs the mapping before any
user code compiles. **This is a breaking change for a project pinning
jolt-lang/time or jolt-lang/crypto at a sha older than their declarations** —
time before v0.0.8, crypto before v0.0.5 — which raises `No dependency provides
java.time.ZoneOffset` on the first use of such a class unless the project
requires the library itself first. Bump the pin; details under Changed.

The rest is additive. `main` is built every night and published as the rolling
`vnightly` prerelease. A small map is one flat slot vector, and the five general
defects behind the worst scorecard rows are fixed. `clojure.core.async.flow`
ships in the stdlib with its `:io` processes on fibers, and the executors under
it have the JVM's shape: `newCachedThreadPool` grows on demand, an enqueue wakes
one worker rather than all of them, and the three spellings of shutdown do what
they say. `jolt.ffi` gained babashka.ffi's bare `:&`, `[:union …]` layouts and a
direct `ByteBuffer` over foreign memory. `clojure.lang.RT/REQUIRE_LOCK`, a
`munge` over the whole `CHAR_MAP` and reflection that reads the statics registry
are what typedclojure's runtime asks of a host.

### Added

- **Nightly prerelease.** `main` is built daily (17:00 UTC) through the same
  platform matrix and fleet gates as a release and published as the rolling
  `vnightly` prerelease, so a downstream project can test against unreleased
  jolt without building it:

  ```bash
  curl -sSL https://raw.githubusercontent.com/jolt-lang/jolt/main/install | bash -s -- --version nightly
  ```

  The install script also takes `--repo <owner/name>` (or `JOLT_REPO`) so a
  fork's own nightly is installable. A nightly's binary reports its
  `git describe` version (`v0.8.0-56-g63374117`), it never displaces
  `releases/latest` (a bare `install` and the Homebrew tap keep reading the
  newest real release), and a build that fails a gate leaves the previous
  nightly in place: the release is replaced only after every gate has passed.
- **`tools/version.sh`** is now the one definition of a checkout's version,
  read by `bin/jolt`, `build-jolt.ss` and the release workflow. It describes
  against release tags only (`v` + digit): the rolling `vnightly` tag is the
  nearest tag from `main`, so a plain `git describe --tags` would have every
  clone that fetched it report `vnightly`. A checkout with no release tag
  reachable (a shallow clone) reports `dev-g<sha>` rather than the bare sha,
  which `:jolt/min-version` read as a version whenever the sha began with a
  digit — a sha beginning with `0` failed every floor.

- **`clojure.lang.RT/REQUIRE_LOCK`.** The JVM's `RT.REQUIRE_LOCK` is the agreed
  object a caller holds around a `require` so two threads do not load one
  namespace at once. jolt had no such field, so
  `io.github.frenchy64.fully-satisfies.requiring-resolve` — which typedclojure's
  runtime requires — fell back to locking `#'clojure.core/require` instead, and
  `(locking clojure.lang.RT/REQUIRE_LOCK …)` raised outright. The field is now a
  plain `(Object.)`, the same value the JVM holds, and it is reachable both as a
  static reference and reflectively — the two spellings library code uses:

  ```clojure
  (locking clojure.lang.RT/REQUIRE_LOCK (require 'my.ns))
  (.get (.getField clojure.lang.RT "REQUIRE_LOCK") clojure.lang.RT)
  ```

  `clojure.core/requiring-resolve` does NOT hold it, deliberately. Clojure's
  does because its loader has no other guard against two threads loading one
  namespace; jolt's loader blocks the second thread until the first one's load
  of that namespace finishes, so the process-wide lock would add nothing — and
  it adds a lock edge the loader's deadlock detector cannot see. A thread holding
  it while waiting on a namespace whose top level reaches for it hangs both for
  good, and did. `clojure.core/serialized-require` exists, private as upstream,
  for code that reaches for the var.

- **`clojure.lang.Compiler/munge`, and `clojure.core/munge` over the whole
  `CHAR_MAP`.** `munge` rewrote dashes and nothing else, so `(munge 'a?)` answered
  `a?` — not a legal identifier on any host, and not what any caller building a
  class name from it can use. It also made `(= (munge "a?") (munge "a_QMARK_"))`
  **false** for two names that do compile to one class, which is exactly the
  question typedclojure's datatype collision check asks. `Compiler/munge` is now
  registered as the forward direction of the `demunge` that was already there, and
  both ends derive from ONE table, so they cannot name different escapes for a
  character:

  ```clojure
  (munge "a-b?")                          ;=> "a_b_QMARK_"
  (clojure.lang.Compiler/munge "+'")      ;=> "_PLUS__SINGLEQUOTE_"
  ```

  That table was also what named a fn's class, where the nine missing entries
  were visible directly: `(class clojure.core/+')` reported
  `clojure.core$_PLUS_'`, a name no JVM emits and `demunge` cannot reverse. It
  now reads `clojure.core$_PLUS__SINGLEQUOTE_`.

- **`clojure.core.async.flow`, with `:io` processes on fibers.** core.async's
  flow library — a flow is a directed graph of processes communicating over
  channels, with the topology, thread execution, lifecycle, monitoring and error
  handling factored out of your step functions and centralized in the graph — now
  ships in the stdlib. The four namespaces (`clojure.core.async.flow`, `.flow.spi`,
  `.flow.impl`, `.flow.impl.graph`) are upstream's sources unmodified, so they
  track the library rather than reimplementing it, and upstream's own
  `flow_test.clj` passes.

  ```clojure
  (require '[clojure.core.async.flow :as flow])
  (def g (flow/create-flow
          {:procs {:src {:proc (flow/process #'source) :args {:n 5}}
                   :dbl {:proc (flow/process #'doubler)}}
           :conns [[[:src :out] [:dbl :in]]]}))
  (flow/start g)
  (flow/resume g)
  ```

  What jolt supplies underneath is `clojure.core.async.impl.dispatch/executor-for`,
  the workload -> Executor mapping flow runs its processes on, and it maps the
  tags the way `clojure.core.async/thread-call` already does:

  - **`:io` is a fiber.** A flow process's loop is mostly channel ops, which park
    and free their carrier, so an `:io` process costs a stack rather than an OS
    thread. 200 of them run on four carriers; against a bounded pool, everything
    past the worker count never started at all — silently, with no error and no
    message ever reaching those processes. `clojure.core.async/fiber-execute` is
    the spawn behind it, deliberately leaner than `fiber-spawn`: an
    `Executor.execute` returns void, so there is nobody to hand a result channel
    to and none is allocated.
  - **`:mixed` is a thread per task**, because a `:mixed` step may block somewhere
    the runtime cannot see (`Thread/sleep`, a raw fd, a blocking FFI call) and
    that would pin a carrier. One per task and not a pool because a flow process
    runs for the life of the flow, so pooling buys no reuse.
  - **`:compute` is a pooled executor**, the one case that runs a short task per
    message rather than one per process.

- **`jolt.ffi`: babashka.ffi's bare `:&`, one binding for every tail shape
  (#803).** `:&` already declared a variadic C function, but only in the
  *declared-tail* form — `[:int :int :& :int]`, the tail fixed when the binding
  is made. The BARE marker, where no types follow and each call reads its own
  tail off the values it is given, is the more useful half for exactly the libc
  functions the marker exists for, and it now works:

  ```clojure
  (ffi/defcfn c-open "open" [:string :int :&] :int)
  (c-open path O_RDONLY)          ; empty tail
  (c-open path flags 0644)        ; one-int tail, same binding
  ```

  A Chez `foreign-procedure` has its argument and result types fixed when it is
  *compiled*, so a call has nothing to compile a new one from — which is why the
  bare form used to raise saying so. It now lowers to a dispatcher over a
  per-binding cache: the first call of each distinct tail shape compiles a
  `foreign-procedure` for that shape and every call after finds it. jolt carries
  its own compiler, in a `jolt build --release` binary too, so this works in a
  standalone app and not only under the REPL.

  Three carriers keep the shape space small, the same three babashka.ffi
  collapses to: an integer, pointer, boolean or `nil` travels as a 64-bit
  integer, a floating-point number or ratio as a `double`, and a string as a
  NUL-terminated C string. A tail value in none of them raises at the call
  naming the three, rather than reaching C as whatever it happened to be.

  The cost is a compile — about 0.8ms — on the first call of each shape, and a
  cache lookup on every call after. Measured end to end from jolt against a
  do-nothing C variadic on Chez 10.4.1, where a fixed binding to the same callee
  is 20ns and a declared tail 20.4ns:

  | tail | per call |
  |---|---|
  | none | 26.5 ns |
  | one value | 34 ns |
  | two values | 41.5 ns |

  So roughly +13ns and 1.6x a declared tail, against the "roughly twice as fast"
  babashka.ffi's own guide gives for declaring one. Declare the tail where it
  does not vary. Three things got it there, each measured rather than assumed:
  the form is COMPILED, not interpreted (interpreting builds 2.5x faster and
  then calls 3x slower forever); the emitted binding is a `case-lambda` with
  arity-specialized arms for tails of nought to three, so a short tail allocates
  no rest list, is not walked twice and is not spread back with `apply` (12ns);
  and the carrier, the marshaller and the cache scan all test for a fixnum first
  (10ns). The bare form does not combine with `:blocking` (neither form does) or
  with a by-value aggregate argument, whose ftype the runtime path has no way to
  declare; `:capture-native-error` composes with both forms.

- **A variadic binding reaches a scoped library's symbols (#803).** A declared
  `:jolt/native` is `dlopen`'d `RTLD_LOCAL`, so its symbols are not in the
  process-global table that a bare symbol name resolves through. Every binding
  therefore tries the library's own handle first — except a scalar-variadic one,
  which skipped that branch and resolved by name only. The effect was that a
  variadic function in a library loaded with `load-library` could not be bound
  **at all**: the name found nothing and the first call raised
  `no entry for "..."`. `curl_easy_setopt` is that case, and so is every
  `ioctl`-alike a wrapped library exports. Both spellings of `:&` now resolve the
  way the rest of the FFI does. libc is unaffected — nothing declares those
  symbols, so they still come from the global table.

- **`[:union …]` in a layout (#803).** A union goes anywhere a nested struct
  does, is as large as its largest member and aligned to its strictest, and gets
  its offsets from the platform ABI — checked against a C witness, not against
  Chez:

  ```clojure
  (def curl-msg
    (ffi/layout [:struct [[:msg :int]
                          [:easy :pointer]
                          [:data [:union [[:whatever :pointer] [:result :int]]]]]]))
  ```

  A union carries no tag of its own — in C the program knows which member is
  live, from a sibling field (`CURLMsg`) or from what it stored
  (`epoll_data_t`). So `read` answers a union as a POINTER to its bytes and the
  caller reads the member that applies, through a `place` at its path or with a
  type of its own, every member starting at offset 0. `write` names the member
  as a pair, `{:data [:result 0]}`, since the members overlap and writing them
  all would leave only the last. A union is not passed BY VALUE in a signature,
  alone or inside a struct — which member is live is the program's knowledge
  rather than the type's — and that raises saying so.

- **`ffi/byte-buffer`: a `java.nio.ByteBuffer` over foreign memory (#803).**
  `(ffi/byte-buffer p 4096)` answers a buffer that SHARES the pointer's bytes
  rather than copying them, so a decoder that speaks `ByteBuffer` reads foreign
  memory in place and a `put` through the buffer is a write to the pointer. The
  length may also be a type keyword or a compiled layout, as it may for `slice`
  and `reinterpret`. `read-bytes`, `read-array` and `read-into!` remain the block
  moves that copy *out* into a jolt value.

  This is a real java.nio DIRECT buffer: `hasArray` answers false and `array`
  raises, exactly as they do on the JVM, and `slice` shares the bytes instead of
  copying them because there is a real address to offset. `ByteBuffer/wrap` and
  `ByteBuffer/allocate` are unchanged — they keep their jolt byte-array backing
  and every answer they gave before. The buffer keeps nothing alive: using one
  after the memory is released reads freed memory, the same rule babashka.ffi
  states for its own.

### Changed

- **Host classes are provided by declaration, not by a list in the runtime
  (#813, RFC 0014).** The runtime carried the mapping from host class names to
  the library that installs them — `java.time.ZoneOffset` to jolt-lang/time,
  `javax.crypto.Mac` to jolt-lang/crypto — as a hand-synced table, so a library
  growing a class needed a jolt release, and core decided what a class meant
  rather than the library implementing it. A library now declares what it
  provides in its own `deps.edn`, and `jolt.deps` collects the declarations
  through the walk it already does for `:jolt/native` and installs them before
  any user code compiles:

  ```clojure
  :jolt/provides {jolt.time ["java.time.ZoneOffset" "java.time.ZoneId" "java.util.Locale"]}
  ```

  Fully-qualified names only; the runtime derives the simple spelling, which is
  what the old table got wrong by hand. Core keeps only the classes it
  implements itself — `jolt.time.base`'s java.time value types and
  `jolt.socket`'s java.net surface. Two dependencies claiming one class is an
  error at resolve time naming both, since whichever won would decide what the
  class means program-wide; a claim on a class the runtime already implements
  is refused. A miss no longer names a library, because which library supplies
  a class is not the runtime's to say and a caller may write the shim
  themselves:

  ```
  No dependency provides java.time.ZoneOffset — a concrete implementation of
  the JDK classes must be provided. A library supplies one by declaring
  :jolt/provides in its deps.edn (RFC 0014).
  ```

  **Migration.** A project pinning jolt-lang/time or jolt-lang/crypto at a sha
  older than their declarations sees that message on the first
  `DateTimeFormatter`, `ZoneId`, `Locale` or `MessageDigest` reference that
  used to autoload the library; a project that requires `jolt.time` or
  `jolt.crypto` itself before that first use is unaffected, because
  registration at load is unchanged. jolt-lang/time v0.0.8 and jolt-lang/crypto
  v0.0.5 declare theirs, and both run on 0.8.0 as well, which ignores the key —
  so the pin can move before the toolchain does.

- **`reduce-kv` is native, and `assoc` of the value a map already holds is the
  map itself.** `reduce-kv` folds a map's entries or a vector's index/element
  pairs in place — no entry objects, no seq — and stops at a `reduced`; a
  deftype or reify declaring `clojure.lang.IKVReduce` still drives its own
  `kvreduce`, and any other map-like value (a record, a sorted map) folds over
  its keys, the reference's `IPersistentMap` arm. A non-collection now raises
  `IllegalArgumentException` naming the protocol, as on the JVM, instead of
  throwing a string. `(assoc m k v)` where `m` already maps `k` to that very
  object answers `m`, and `(dissoc m k)` of an absent key answers `m` — what
  `PersistentArrayMap` and `PersistentHashMap` both do — so `identical?`
  agrees with the JVM there and no copy is made.
- **State images are format 7.** A map's record is `chez-pmap-v5` (the flat
  slot representation above); images of formats 2 to 6 still restore, an
  old-format map re-minted from its own record's fields on the way in. An
  image written by this build does not read on 0.8.0 and earlier.
- **A `jolt build` direct-links calls to seed vars.** A call from app code to a
  var of a namespace the runtime image boots with (clojure.core, clojure.string,
  …) binds the var's root procedure once, when the def holding the site loads,
  and calls it directly — the same closed-world rule an app def already gets
  under direct-link, applied only where the root is a procedure whose arity
  admits the call and the var is not `^:dynamic`, `^:redef` or redefined by
  the app. What changes is what changes under the JVM's own direct linking: an
  `alter-var-root` or `with-redefs` of such a var is not seen by call sites
  compiled into the binary. `jolt run`, the REPL and `--no-direct-link` never
  direct-link, so tests that redefine core fns keep working there.
- **`case` compares keyword, `nil` and boolean constants with `identical?`.**
  Keywords are interned, and this is what the reference does for those
  constants; symbol, string and number constants still compare with `=`.

- **The reflection member model now reads jolt's static and shim registries, and
  member objects are real `java.lang.reflect.*` values.** `Class.getDeclaredFields`
  / `getMethods` used to consult only the protocol/type registry, so every host
  class jolt models reported no members at all — and `(.getField Long "MAX_VALUE")`
  raised `NoSuchFieldException`, which a caller with a handler for that cannot
  tell apart from a field that really is absent. Three registries now answer, on
  the one rule the statics table already used (a procedure is a static method,
  anything else a static field's value):

  ```clojure
  (.get (.getField Long "MAX_VALUE") nil)             ;=> 9223372036854775807
  (map #(.getName %) (.getMethods String))            ;=> ("format" "join" "valueOf")
  (:flags (first (:members (clojure.reflect/type-reflect 'java.lang.Long))))
  ;;=> #{:public :static :final}
  ```

  `getMethod` / `getDeclaredMethod` / `getConstructor` / `getDeclaredConstructor`
  are new; they select by parameter COUNT (jolt carries no signatures) and raise
  `NoSuchMethodException` for a name no registry has. And the member values —
  `Method`, `Field`, `Constructor` — now report their JVM class and print like the
  JVM instead of as an opaque `#object[:object]`:

  ```clojure
  (str (.getField Long "MAX_VALUE"))
  ;;=> "public static final java.lang.Object java.lang.Long.MAX_VALUE"
  ```

  A member that lives in none of the registries is still absent rather than
  guessed at: String's instance methods are a `cond` in `natives-str.ss`, not data
  anything can enumerate, so String reports its three statics and nothing else.

### Performance

- **A small map is one flat key/value slot vector.** An array-mode map — a
  literal of up to 8 entries, up to 64 keyword-keyed, and everything `assoc`
  keeps below those limits — used to be a hash trie carrying an insertion-order
  list on the side: a record, a node, a leaf pair per entry and an order pair
  per entry, with every lookup hashing the key and descending the trie. It is
  now what `PersistentArrayMap` is: a record over `#(k0 v0 k1 v1 …)`. A lookup
  scans the slots with a compare chosen once from the probe key's kind (a
  keyword by identity, a fixnum, string or symbol by its own equality, the rest
  by `=`); `assoc`, `dissoc` and the transient's `assoc!`/`dissoc!` copy or
  write the slots; `reduce-kv`, `seq`, `keys`, `vals`, `=` and `into` read them
  in place. Measured per operation in one `--opt` binary, before → after:
  `(:k m)` on a 3-entry map 34 → 12 ns, on an 8-entry map 25 → 15,
  `(get m 20)` 43 → 16, `contains?` 31 → 11, `assoc` replacing a value 116 →
  64, `dissoc` 111 → 59, `{:a x :b y}` built at run time 100 → 17, `(= m m')`
  on 5 entries 230 → 84, `(into {} m)` 1045 → 205, `zipmap` of three 400 →
  201, `(reduce-kv f 0 m)` on 8 entries 583 → 50 and on a 64-keyword map
  4259 → 324. The one shape that got slower is the far end of a large
  keyword array map, where the scan is linear as it is on the JVM: the 64th
  key of a 64-keyword map answers in 53 ns where the trie took 38 (the 30th
  is 29), and a symbol probed against symbol keys and missing costs 38 where
  hashing it took 20. A hash-mode map (`hash-map`, or grown past the limits)
  is unchanged.

  Under it, three general things. The runtime's hot loops over a slot vector
  run on a new adapter tier of unchecked fixnum/vector primitives
  (`sa-ufx+`, `sa-uvector-ref` …, see `host/scheme-adapter/CONTRACT.txt`):
  the runtime compiles at Chez's safe optimize-level 2, where a checked scan
  of a 64-keyword map costs three times the unchecked one. Every vector copy
  under a trie node update, a tail append or a transient's growth is a bulk
  move rather than a checked element loop (a 64-slot copy: 390 → 62 ns).
  And a map's `seq`, `keys` and `vals` views are vector-backed cells — one
  entries vector walked by index, so `count` is O(1) and `reduce` runs the
  chunk loop — where they were a cons chain built up front, a cell, a pair
  and an entry per element before the first was read.
- **The worst scorecard rows shared five general defects, now fixed.** Measured
  per operation inside one `--opt` binary (see `bench/README.md`): a compare of
  two base scalars of different kinds, or of anything against nil, walked every
  registered eq arm before it was answered (145-240 ns per miss; `case` lowers
  to that chain) — answered ahead of the walk now, and the registry refuses an
  arm claiming such a pair. The `unchecked-*` family was not lowered at all
  (`unchecked-add` 29 ns against 3.6 for `+`): it now emits its helpers
  directly, `unchecked-long`/`unchecked-int` are casts that type their result
  `:long`, and `jolt-wrap64` tests `fixnum?` before comparing against bignum
  bounds. An interop call on an unproven receiver (`(.length s)` with no hint)
  cost 60-135 ns in the arm walk and a rest-args vector-to-list conversion; a
  method with a string or keyword direct form now tests the receiver at the
  site and takes it, with the generic dispatch as the slow arm. A call to a
  clojure.core fn from a built binary paid ~12 ns of var-cell-deref +
  jolt-invoke dispatch; under direct-link such a site now binds the var's root
  procedure once at load and calls it (see Changed). `nth` on a small vector,
  `first` and `str` had their own dispatch overheads trimmed. `char-scan`,
  `literals`, `string-ops`, `keyed-lookup` and `arrays` all move; the refreshed
  scorecard is in `bench/README.md`.
- **`aget`/`aset` on a `^doubles` parameter index the backing flvector bound
  once at the arity's entry** instead of re-reading the checked record
  accessor per access. A `^doubles` argument that is not a double array now
  raises `ClassCastException` on entry, as a JVM checkcast does, whether or not
  the body reads it. The README's earlier note that the remaining `arrays`
  cost was flonum boxing of the loop accumulator was wrong — a Chez probe of
  the emitted loop allocates nothing per iteration; the accessor was the cost.

- **An executor enqueue wakes one worker, not all of them (#819).** The pool's
  workers and its `awaitTermination` waited on ONE condition, so every enqueue
  broadcast to every waiter: with 130 idle workers, one task woke all 130, and
  129 of them took the queue mutex, found the task already claimed, and parked
  again. Fire-and-forget cost **152µs per no-op task** that way, against 3.2µs on
  reference Clojure, and the herd fed back into the growth rule — workers that
  slow to return look like workers that are not coming, so a 200k-task burst grew
  the pool to 134 threads and 131MB, which made the herd bigger still.

  Task and termination now have a condition each. An enqueue signals exactly one
  parked worker (`jolt-cv-signal-one!`, locks.ss, where the two preconditions it
  needs are written out), and none at all when none is parked — a worker re-reads
  the queue before it parks, under the same mutex, so there is no wake to miss.
  A worker retiring on keep-alive wakes nobody unless the pool is shutting down or
  it is the last one out. Shutdown still broadcasts, on both conditions: that news
  is for every worker.

  The broadcast is older than the growing pool and cost something all along — a
  fixed 32-worker pool woke 32 — so this is a **3x improvement on 0.8.0** as well
  as a 17x one on the growing pool before the split. Dispatching a no-op task
  through `.execute`, same box:

  | | µs/task | threads |
  |---|---|---|
  | 0.8.0: fixed 32-worker pool, one condition | 26.5 | 32 |
  | growing pool, one condition | 152 | 134 |
  | growing pool, one worker signalled | **8.9** | 7 |
  | reference JVM Clojure | 5.2 | 8 |

  and the rest of the shapes, jolt against reference Clojure on that box:

  | | jolt | JVM |
  |---|---|---|
  | 4 producers, one pool | 11 µs/task (was 80) | 2.2 µs |
  | single-thread pool | 4.3 µs | 9.1 µs |
  | submit + get round trip | 12.0 µs | 7.2 µs |
  | 256 threads created | 21 ms | 22 ms |

  The pool now grows to the same handful of threads the JVM's does for the same
  work. What is left between the two is one mutex and one park/unpark per handoff
  where a `SynchronousQueue` transfers without either: it costs jolt 1.7x on a
  single producer, and it is most of the gap with four producers on one pool,
  where the JVM's queue stripes and jolt's one mutex convoys.

### Fixed

- **`(unchecked-long \a)` raises `ClassCastException`.** `RT.uncheckedLongCast(Object)`
  is `((Number) x).longValue()`, so a Character is rejected on the JVM; only
  the int cast has a char overload, and `(unchecked-long (unchecked-int c))`
  still works. jolt answered 97. Found by the JVM certification of new corpus
  rows.

- **A bad namespace designator says what was wrong.** `(ns-name nil)`,
  `(the-ns nil)` and `(find-ns nil)` reached a bare Chez record accessor and
  escaped as a condition with no jolt class and no message — printing as
  `#object[:object]`, with `(ex-message e)` **nil**. A `nil` designator is now the
  JVM's `NullPointerException` and any other non-symbol its failed cast to
  `clojure.lang.Symbol`. `(ns-name nil)` is a plausible slip in any macro reading
  a `:ns` out of metadata (typedclojure's `update-expr` does), and it used to say
  nothing whatsoever about where the failure was.

- **`Executors/newCachedThreadPool` grows on demand (#819).** It was a fixed pool
  of 32 workers. A burst of more than 32 concurrent tasks queued behind the ones
  already running — invisible while tasks are short, and a deadlock when they are
  not: 32 tasks that block waiting for the 33rd (a fan-out whose children hand
  their results back through a channel, say) never let it start, with no error and
  nothing in the code to suggest a ceiling. The pool now has the JVM's shape —
  core 0, max `Integer/MAX_VALUE`, 60s keep-alive — so nothing is forked until a
  task arrives, a task that no idle worker is waiting to take starts one, and a
  worker retires after 60 seconds idle.

  It is still a POOL, which is what keeps that unbounded maximum from meaning a
  thread per task: 2000 trivial tasks submitted back to back peak at **4** workers
  on an idle 8-core box (5000 of them, 3), because the handful that keep up with
  them are idle again by the time the next one lands. How far it grows is a
  property of how far the producer runs ahead of the workers — the same 2000-task
  burst pinned to a single core peaks at 136 — and of the tasks themselves: 1000
  tasks that each sleep 50ms do reach 558 workers. Which is the JVM's own answer to
  the same bursts, measured on the same box against reference Clojure: 13 workers
  for the 2000 trivial tasks, 697 for the 1000 sleepers, and the same 64 for the
  64 blocking ones. The unbounded growth is concurrency, not churn.

  `newVirtualThreadPerTaskExecutor` maps to the same pool, being equally unbounded
  on the JVM; a pooled thread stands in for a fresh virtual one, which nothing
  here can tell apart. `newWorkStealingPool` stays a fixed pool, because a
  `ForkJoinPool` is sized at `availableProcessors` on the JVM too. `newFixedThreadPool`
  and `newSingleThreadExecutor` are unchanged: every worker eager, none retired,
  and for the single-thread pool the submission ORDER that depends on there being
  exactly one worker forever.

  `ThreadPoolExecutor.`'s `corePoolSize`, `maximumPoolSize` and `keepAliveTime`
  now all reach the pool it builds — the core workers eager, the rest on demand up
  to max, and the above-core ones retired after keepAlive. Before, it was a fixed
  pool of `maximumPoolSize` and the keep-alive was discarded. One divergence
  remains, in the direction of the caller's stated maximum: the JVM grows past
  core only once the work queue is FULL, so a `ThreadPoolExecutor` with an
  unbounded queue never passes core there, while jolt (which has one unbounded
  queue per pool) grows on the same no-idle-worker test as a cached pool.

  `.getPoolSize`, `.getActiveCount`, `.getCorePoolSize` and `.getMaximumPoolSize`
  are new, so a caller can see the pool grow and retire — and so the tests can
  assert it without counting threads by hand.

- **The three spellings of executor shutdown do what they say (#819).** Found
  while measuring the pool above, all three verified against reference JVM Clojure
  on Java 21:

  - **A `submit` or `execute` after shutdown is REJECTED** with
    `RejectedExecutionException` (a `RuntimeException`, catchable as either), which
    is what the JVM's default `AbortPolicy` does. jolt used to accept the task and
    queue it — a promise the pool cannot keep, because its workers leave as soon as
    the queue they are draining runs dry. The task then ran only if a worker
    happened to still be there, and otherwise sat in the queue for good, with a
    `Future` whose `.get` waits forever and an `isTerminated` answering false about
    a pool that has nothing left to run it. The check is made under the queue mutex,
    so a task that gets in ahead of the flag is enqueued while a worker is still
    live, and a worker drains the queue before it leaves.
  - **`shutdownNow` drops the queued tasks and hands them back**, which is the
    whole difference between it and `shutdown`. It used to answer an empty vector
    and leave the queue to drain, so the method that exists to say "do not run the
    rest" ran the rest: a caller shutting a pool down hard because its work had
    become irrelevant got every queued task executed anyway, and an empty list
    claiming nothing had been pending. The returned procedures are callable, so a
    caller can still run one itself, as `.run` lets them on the JVM. What jolt still
    cannot do is the other half — interrupting the tasks already RUNNING, which
    needs the workers to carry an interrupt flag a shutdown can set.
  - **`close` blocks until the pool has terminated**, as the JVM's has since 19.
    It used to return the moment the flag was set, so the one spelling of shutdown
    that promises the work is finished when it returns was the one that did not
    wait, and `(with-open [ex …] …)` left its tasks running behind it.

  A worker that dies for any reason other than its task throwing (nothing does
  that today) now hands its slot back before it goes, instead of leaving
  `live-workers` counting a thread that is gone — which would have left
  `isTerminated` false forever and `awaitTermination` waiting out its deadline on a
  finished pool. The next enqueue starts a replacement, because a live count below
  max with a task waiting is what the growth rule spawns on.

- **`deref` of a `java.util.concurrent.Future`.** `@fut` raised "deref:
  unsupported reference type" for a `FutureTask` and for what an
  `ExecutorService.submit` hands back. Neither is `IDeref` on the JVM either —
  `clojure.core/deref` falls *through* to `deref-future` for anything that is not,
  which is `.get`, and for the timed arity `.get(ms, MILLISECONDS)` with a
  `TimeoutException` answered by the timeout value rather than thrown. Both
  arities now work, so `@(.submit pool f)` and `(deref fut 100 :timeout)` do what
  they do on the JVM.

- **`Future.get` reports a task's throw as an `ExecutionException`.** The raw
  throw used to come straight back out, so a caller catching
  `ExecutionException` — which is what the JVM makes them catch — caught nothing,
  and `.getCause` had nothing to read. The original is now the cause, so
  `ex-cause` and `.getCause` both reach it. This is the same wrap a clojure
  `future` already did on deref; the two now agree.

- **`instance?` on the executor and future shims.** `java.util.concurrent`'s
  executor/future interfaces had no rows in the class graph at all, so an
  `ExecutorService` reported `(class x)` as `:object` and answered **false** to
  `(instance? java.util.concurrent.Executor x)`. That is the exact seam
  core.async.flow tests a user-supplied `:io-exec`/`:mixed-exec`/`:compute-exec`
  through, so a real jolt executor was rejected as "not an Executor". `Executor`,
  `ExecutorService`, `ThreadPoolExecutor`, `Future`, `RunnableFuture` and
  `FutureTask` are now in the graph and the shims report their real classes. The
  graph is consulted only by `instance?`/`class`, so naming it costs a shim value
  nothing to construct or call.

- **`Thread(Runnable)` accepts a `FutureTask`.** `.start` invoked its target
  directly, and a `FutureTask` is a shim value rather than a procedure, so
  `(.start (Thread. a-future-task))` hung instead of running the task. It goes
  through the same `Runnable` conversion the executors use now — which is what
  the class graph above already claims, a `FutureTask` being a `Runnable`.

- **Socket error handling could lose `errno` between a syscall and its accessor,**
  causing a clobbered `EAGAIN` to be treated as a terminal socket failure.
  `connect`, `accept`, `recv`, and `send` now capture result and `errno` atomically.

- **Modeled atomic classes used Clojure value equality and unbounded
  arithmetic.** `AtomicReference.compareAndSet` now compares object identity,
  while `AtomicInteger` and `AtomicLong` validate primitive arguments and wrap
  arithmetic at their signed 32- and 64-bit widths, and `AtomicLong.intValue`
  narrows to a signed 32-bit result, matching the JVM.

- **`File.getParentFile` answered the filesystem root as its own parent, so a
  walk-to-root loop never terminated (#809).** The parent was the text before
  the last separator, which for `"/"` is `"/"` — the path back again, where the
  JVM answers `nil`. Every `(recur (.getParentFile d) …)` is written against
  that `nil`, and because the recur is a tail call the non-terminating loop had
  no stack to overflow and nothing to raise: it presented as a hang. Both
  `.getParent` and `.getParentFile` now answer `nil` whenever the computed
  parent equals the path it came from, which is the JVM's invariant and needs no
  special case for `"/"`.

  In the same dispatch table, **`.listFiles` raised where the JVM answers
  `nil`** — for a path that does not exist, for a plain file, and for a
  directory the process may not read — so the ordinary
  `(map str (.listFiles f))` died instead of yielding `()`. Its neighbour
  `.list` already guarded the first two cases and not the third; both spellings
  now share one guard covering all three.

- **Namespaced-map prefixes could absorb separator text into a silent wrong
  namespace or accept an invalid namespace.** The reader now requires the
  namespace — auto-resolved (`#::a`) or explicit (`#:a`) alike — to be a simple
  unqualified symbol, stops at the token boundary, allows whitespace and commas
  before `{`, and rejects other intervening input including comments to match
  JVM behavior. A missing map is reported before the namespace token is judged,
  and an unregistered alias now says `Unknown auto-resolved namespace alias`
  rather than `Invalid token`, so the message matches the JVM's for every
  spelling that names a namespace.

## [0.8.0] - 2026-08-31

The build keeps one toolchain end to end now. When make provisions the pinned
Chez + xPack GCC, the standalone-binary link runs through that same provisioned
GCC — `JOLT_CC`, honored by `build.ss`'s `bld-cc` — instead of a bare `cc`,
which can pair a distro gcc 16 driver with the bundle's pre-`.base64` binutils
and die on `unknown pseudo-op: .base64` (#788). And with nothing explicit
selected, a Chez on `PATH` at or above the pinned version is used as-is rather
than triggering a provisioning download; the system toolchain then builds and
links everything. Older, broken, or absent Chez falls back to the pinned
provision. `JOLT_SYSTEM_CHEZ=` (empty) forces provisioning; an explicit `CHEZ=`
stays authoritative.

`jolt.ffi` gained arenas and is now compatible with `babashka.ffi` name for name
(#799). **Two breaking changes come with that**, both detailed under Changed
below: `ffi/write` takes the value *before* the offset, `(write p t v [offset])`,
which no runtime check can distinguish from the old order; and a fixed array in a
layout is now `[:array element-type count]`, which does raise at compile time
when transposed.

### Added

- **`jolt.ffi` arenas: a group of allocations with one lifetime (#799).** Every
  foreign allocation used to be released one pointer at a time — `ffi/free` per
  block, `free-callable` per callback, a `try`/`finally` per scope — so a
  function holding half a dozen of them spent more lines releasing memory than
  using it, and the failure for getting it wrong is a leak or a fault rather
  than an error. An arena owns a group instead, and closing it releases the
  whole group:

  ```clojure
  (with-open [a (ffi/confined-arena)]
    (let [buf  (ffi/alloc a 4096)
          name (ffi/string->ptr a "config.toml")
          on-ev (ffi/callback a handle-event [:pointer :int] :void)]
      ...))                              ; all three released here
  ```

  Four kinds, differing in who may use them and who closes them:
  `confined-arena` for one thread (using it from another raises, rather than
  two threads racing on one block list), `shared-arena` for any thread,
  `global-arena` for the process, and `auto-arena` for memory released when the
  collector reclaims the arena — the one a callback that C may invoke from a
  thread jolt never started needs, since no scope can be its lifetime.
  `with-open` closes an arena, `with-arena` is the confined arena with its
  constructor spelled in, and `close-arena`, `arena?`, `arena-open?` and
  `drain-auto-arenas!` are the rest of the vocabulary.

  `alloc`, `string->ptr`, `clone`, `reinterpret`, `segment`, `slice` and
  `callback` all take an arena in the same first position; `(alloc n)` with no
  arena is still the caller-owned form, and the `with-alloc`/`with-out`/
  `with-layout`/`with-c-string` helpers are unchanged.

  An arena also scopes a POINTER SIZE. A jolt pointer is a bare address and
  carries no length, so `size` answers what jolt was told — by an arena
  allocation, or by `segment`/`slice`/`reinterpret` — and that record is keyed
  by address. `free` forgets the size of the pointer it releases and an arena
  forgets its group's, because an allocator hands the same address out again and
  a record that outlived its memory would answer a size the new block never had,
  which `copy` and `clone` would then use as a byte count. Called WITHOUT an
  arena, `segment`, `slice` and `reinterpret` record for the life of the
  process: that is the form for a size declared once at startup, and the wrong
  one in a loop, where the record both grows without bound and outlives the
  memory it describes.

- **`jolt.ffi` is now name-for-name and semantics-for-semantics compatible with
  `babashka.ffi`.** The two FFIs had the same job, the same type vocabulary and
  largely the same shape, and disagreed in exactly the places that make a shim
  in either direction hand-written rather than a namespace alias. New here,
  matching babashka.ffi: `cfn`, `alignof`, `place`, `copy`, `clone`, `size`,
  `address`, `segment`, `slice`, `reinterpret`, `pointer?`, `find-symbol`,
  `load-system-library`, `callback`, `arena?`; a `:bool` type; docstrings, an
  attribute map and the wrapper form on `defcfn`; a byte limit on
  `ptr->string`; the typed `read-array`/`write-array` forms; `read` and `write`
  over a whole layout, which decode a struct as a map and an array as a vector;
  a `load-library` that takes an ordered list of candidates, accepts `:mac`
  beside `:darwin`, and answers a `{:path ...}` library map naming the candidate
  that loaded.

- **`:jolt/min-version` in `deps.edn`.** A project, or a library, declares the
  oldest jolt it works on, and a runtime below that refuses to load it rather
  than run it:

  ```clojure
  {:paths ["src"]
   :jolt/min-version "0.8.0"}
  ```

  The key is honoured by the jolt that **reads** it, so it protects from this
  release onward and not before — an older jolt ignores it, as it ignores every
  key it does not know.

  That cuts one way worth stating plainly: **the floor cannot cover this
  release's own breaking changes.** `ffi/write`'s argument order moved here, and
  the old and new spellings are both integers, so an older runtime writes to the
  wrong place and reports nothing — the shape of failure a floor exists for, and
  the one it cannot catch, because the jolt that reads the key arrived in the
  commit after the one that moved the arguments. Every runtime old enough to take
  the offset first is too old to parse `:jolt/min-version`, and skips it.
  Declaring `"0.8.0"` does not turn that break into a message; on such a runtime
  you get the untreated failure — a misdirected write, or a namespace that will
  not load — and not a refusal. Pin the toolchain for that one. What the floor
  catches is the *next* break of that shape, where the runtime on both sides of
  the change can read the key. That is the reason it arrives now rather than at
  the next break.

  A **library** is the natural declarer: it knows which jolt its FFI bindings or
  host shims need, and the app pulling it in does not. An unmet floor names what
  is needed, what is running, and which dependency asked; several unmet floors
  report the newest one. A runtime that names no version (a source build answers
  `dev`) is never refused, since it reads as the oldest possible while in
  practice being the newest; `JOLT_SKIP_MIN_VERSION=1` runs anyway for a released
  runtime whose version string understates what it carries.

- **`:&` declares a variadic C function**, as it does in babashka.ffi.
  `:varargs` is jolt's older spelling of the same marker and still works; the
  two are one code path, so every rule already gated for `:varargs` holds under
  `:&`:

  ```clojure
  (ffi/defcfn c-fcntl "fcntl" [:int :int :& :int] :int)
  ```

  What jolt does *not* have is babashka.ffi's **bare** `:&` —
  `[:string :int :&]`, where each call infers its own tail from the values it is
  given. A `foreign-procedure`'s types are fixed when it is compiled, and a call
  has nothing to compile a new one from, so the tail belongs to the binding: bind
  one signature per tail shape. A bare marker raises saying exactly that, rather
  than falling through to `unknown foreign type :&` — which is what a babashka
  signature pasted in would otherwise have hit.

  Where the two still differ, the substrate is why, and the namespace docstring
  lists them: a jolt pointer is a raw address rather than a sized segment, so
  `read`/`write` do not bounds-check and `size` answers what jolt was *told* (by
  an arena allocation, `segment` or `reinterpret`) and 0 otherwise; `cfn` and
  `callback` are macros, because Chez's `foreign-procedure` needs its types at
  compile time; layouts have no `:union`; and the library-scoped 4-argument
  `cfn` raises rather than quietly searching every loaded library, since a
  declared `:jolt/native` already resolves its own symbols through its own
  handle.

- **`:bool`, a one-byte C boolean.** `_Bool` is one byte and jolt had no type
  for it, so a `bool` parameter or result had to be bound as `:uint8` and
  converted by hand — and a C predicate then answered the truthy number `0`.
  `:bool` reads as `true`/`false` and writes on jolt truthiness, so `nil` and
  `false` send 0 and every other value sends 1. It travels as `unsigned-8`
  rather than through Chez's own int-sized `boolean` type, which is the wrong
  width for `_Bool` and reads three bytes of whatever the callee left above the
  result. Covered against a real `stdbool.h` witness in both directions,
  including a C-invoked callback.

### Changed

- **BREAKING: `ffi/write` takes the value before the offset.** `(write p t v)`
  and `(write p t v offset)`, which is `babashka.ffi`'s order; it was
  `(write p t offset v)`. The two spellings cannot be told apart at runtime —
  an offset and a value are both integers — so this is a silent behaviour change
  for out-of-tree code, and the fix is mechanical: move the last argument of a
  four-argument `write` into third place, and drop it when it is `0`. `read` is
  unchanged, `write-field` is unchanged, and a three-argument `write` is new
  rather than changed. Every call site in this repository was rewritten.

- **BREAKING: a fixed array in a layout is `[:array element-type count]`.**
  babashka.ffi's order; it was `[:array count element-type]`. Unlike `write`,
  this one cannot pass silently — a count in the element position is not a type
  and a type in the count position is not a positive integer, so the transposed
  spelling raises at compile time, naming the descriptor.

- **`ffi/alloc` answers ZEROED memory.** Both forms, arena and caller-owned. A
  struct a caller only partly fills is the ordinary case, and malloc's leftovers
  in the rest of it are a C-visible bug that reproduces only under load — the
  three hand-written zero loops this replaced in `jolt.socket`, `jolt.nrepl` and
  `jolt.mvn-http` are what the guarantee is worth. The fill is one block move,
  not a per-byte loop.

### Performance

- **`ffi/string->ptr` copies the string in one block move.** It was a per-byte
  `foreign-set!` loop — about 30ns a byte across the boundary, the cost the
  buffer-I/O helpers in the same file exist to avoid, on the one path that had
  kept it. The NUL terminator rides along in the same move, since a fresh
  bytevector one byte longer is already zero there.

### Fixed

- **The hash engine computed the wrong hashes on a 32-bit Chez (#801).**
  `hasheq.ss` and the HAMT in `collections.ss` work in the Java int window —
  `[0, 2^32-1]` unsigned — and applied Chez's `#3%fx` UNSAFE fixnum primitives
  to it, on the strength of "every unsigned 32-bit value is a fixnum". That
  holds on a 64-bit target and not on a 32-bit one, where the positive fixnum
  ceiling is 2^29-1: an ordinary hash with bit 30 set is a bignum there, and an
  unsafe fx op applied to a bignum does not raise — it answers nonsense. So a
  `tpb32l` build, the threaded portable-bytecode target a WASM build goes
  through, hashed wrongly rather than failing.

  Each operator now names its wide and narrow twin through one `define-width-op`
  form and the arm is chosen at EXPAND time, so a 64-bit build generates the
  same `#3%fx` chain it always did — no runtime width test, no duplicated arms —
  and a 32-bit build gets exact-integer arithmetic. `JOLT_NARROW_HASH=1` selects
  the narrow arm on a wide machine, which is what lets `make narrowhash` run the
  JVM-pinned hash goldens, the value-model suite and the transient HAMT suite
  over the generic operators without 32-bit hardware.

  This is the hash engine, which is the only place in the host that uses unsafe
  fixnum primitives at all; it is not full `tpb32l` support. Other host code
  still applies checked `fx` ops to int-width values, which raise rather than
  corrupt. Reported and first patched by @jasalt, found bringing up a
  WASM/Emscripten build.

- **`getPosixFilePermissions` and `getOwner` refused to run on hosts whose
  layout jolt already knew.** `nio-file` reads `st_mode` and `st_uid` at offsets
  that are a per-platform ABI, and it chose them from the host's *identity*:
  Darwin, or x86-64 Linux, or else throw `UnsupportedOperationException:
  unverified struct stat layout`. Two kinds of host fell through that were not
  actually unknown. A portable-bytecode build has no identity to read at all —
  its machine tag names neither OS nor architecture (#796, #798) — so every pb
  build refused. And native **aarch64 Linux** refused too, on a machine whose
  offsets were written in the file's own comment and never turned into a branch.

  The layout is measured now instead of deduced. `stat("/")` says which
  candidate row really is `st_mode`, because `S_IFDIR` appears in the format
  bits of the true field and in none of the competing ones — at those offsets a
  real host has `st_dev`'s high half, `st_nlink`, or `st_uid`, none of which
  reach `0x4000` for a root directory. Identity still gets the first word, so a
  host that worked before reads exactly as it did; measurement gets the last, so
  a proposal it contradicts is discarded rather than used. That is also what
  makes a new row cheap to add: get the offsets wrong and nothing verifies, so
  the host refuses exactly as it refuses today rather than quietly answering
  nonsense.

  Both Linux rows are `offsetof`-measured — x86-64 natively, aarch64 under
  `qemu-aarch64` against the arm64 cross headers — and on each ABI `stat("/")`
  matches exactly one row, the others landing on `st_nlink` and `st_uid`.

- **`sa-arch` and `sa-endian` could not answer for a portable-bytecode build.**
  The follow-up half of #796. `sa-arch` matched `arm64`/`a6`/`i3` in the machine
  tag and `sa-endian` read its last two characters for `le`/`be`; a pb tag
  matches neither vocabulary even though `pb64l` does name a 64-bit
  little-endian build, in fields those derivations do not parse. Both now probe
  past a tag that declines to say: `uname(2)`'s `machine` field for the
  architecture (`PROCESSOR_ARCHITECTURE` on Windows, which has no `uname`), and
  `native-endianness` for the byte order, which is exact everywhere and needs no
  probe at all. `os.arch` on a bytecode build answers `amd64`/`aarch64` rather
  than the raw tag.

  `sa-endian` no longer has a `#f` answer — a runtime always knows its own byte
  order. That mattered beyond the tag: `nio-x86-64-linux?` tested the
  architecture and the byte order but never the OS, so it was correct only by
  the accident that the Windows tag has no `le` suffix. Answering endianness
  honestly would have made a Windows x86-64 build claim the Linux `struct stat`
  layout, and the rewrite above removes that predicate entirely.

- **A portable-bytecode build reported itself as Linux, wherever it was running.**
  `sa-os-family` derived the OS by substring-matching Chez's machine tag, which
  answers for a native tag and cannot answer for a bytecode one: `pb`, `pb64l`
  and `tpb64l` name the threading, word size and endianness and deliberately
  name no OS, because the same bytecode is meant to run on any of them. So the
  `else` branch fired and every bytecode build called itself Linux, macOS
  included (#796).

  That function is the single place the rest of the host asks what OS this is,
  so one wrong answer was wrong everywhere at once: `SIGCHLD` (20 vs 17),
  `EAGAIN` (35 vs 11), `O_NONBLOCK`, `LC_TIME`, the `struct stat` offsets, the
  chmod and entropy fallbacks, and the link libraries. The visible route in was
  a socket — `jolt.nrepl` handed Darwin's `socket()` the Linux `SOCK_CLOEXEC`,
  so a bytecode build on a Mac could not bind an nREPL server at all.

  A tag containing `pb` now probes the running host instead of being parsed:
  `/System/Library/CoreServices/SystemVersion.plist` for Darwin (chosen over
  `libSystem.B.dylib` because it is readable from inside a sandbox, which
  anything in the dyld shared cache is not), `/proc/self/status` for Linux,
  `SystemRoot` for Windows, cached for the life of the process. Native tags
  never reach the branch, so `tarm64osx`, `ta6nt` and `ta6le` resolve exactly as
  before, and an unrecognized non-`pb` tag still falls through to `linux` rather
  than newly probing.

  Still open, and pinned by the gate so it is not mistaken for done: a pb tag
  *does* carry its word size and endianness, and neither `sa-arch` nor
  `sa-endian` parses that shape, so `pb64l` answers `other` and `#f`. Both
  stat-layout arms in `nio-file` therefore stay false and a pb build on x86-64
  Linux still refuses `getPosixFilePermissions`; the Darwin case is fixed only
  because that guard does not consult the arch. Closing it needs probes of their
  own.

- **A `File`'s path was kept exactly as given, not normalized.** Every JVM `File`
  constructor runs its path through `FileSystem.normalize()`, so runs of `/`
  collapse to one and a trailing `/` is dropped: `new File("/a/b//c").getPath()`
  is `/a/b/c`. jolt answered `/a/b//c`. The visible route in was
  `createTempFile`, since `$TMPDIR` ends in `/` on macOS and every temp file came
  back carrying a doubled separator.

  `clojure.java.io/file` had the same gap and a second one under it. It is
  registered as the bare file constructor, whose multi-arg loop appends `/`
  unconditionally, so `(io/file "/a/b/" "c")` gave `/a/b//c` where the JVM gives
  `/a/b/c` — Clojure's own docstring says the multi-arg versions are equivalent
  to `(File. parent child)`. That is the half more likely to bite, since
  `(io/file dir name)` is everywhere and a directory read from config or the
  environment often carries a trailing separator.

  `jolt-file-join` already normalized, but only the JOIN SEAM — a trailing
  separator off the parent, leading ones off the child, and it never looked
  inside either. So `(File. "/a//b" "c")` still gave `/a//b/c`. Normalization now
  lives in the `jfile` record's protocol instead: there are nine construction
  sites and only one is the constructor entry point, so `as-file`, the `file:`
  URL coercion, `createTempFile`, `getParentFile` and `listRoots` were each
  building unnormalized paths of their own.

  `.` and `..` are still not resolved. The JVM constructor does not resolve them
  either; that is `getCanonicalPath`.

  The two-arg constructor is `resolve(normalize(parent), normalize(child))` — it
  normalizes each argument, and resolve is not a plain join. An empty parent
  resolves against `getDefaultParent()`, so `new File("", "")` is `/`, not `""`,
  which jolt got wrong in both directions before. (If you go measuring this
  yourself: resolve grew its `child == "/"` case in JDK 21, so through JDK 20
  `new File("/a/b", "/")` answered `/a/b/`, a path with a trailing separator no
  one-arg constructor can produce. 21 onward it is `/a/b`.)

- **`clojure.java.io/file` joined an absolute child instead of rejecting it.**
  `io/file` is not the `File` constructor: Clojure puts every child through
  `as-relative-path`, so `(io/file "/a/b" "/c")` raises `IllegalArgumentException`
  while `(File. "/a/b" "/c")` answers `/a/b/c`. jolt joined it either way.

  Worth knowing that normalization alone would have hidden this rather than
  fixed it — `/a/b` joined to `/c` gives `/a/b//c`, which collapses to a
  plausible-looking `/a/b/c` answer to a call the JVM refuses. A call site that
  newly raises here was already broken for JVM Clojure.

  Review follow-ups, all JVM-measured: `as-relative-path` goes through `as-file`
  first, so the thrown message names the normalized path — `(io/file "/a/b"
  "//c")` says `/c is not a relative path`, not `//c`. `io/as-relative-path`
  itself is now registered as a var (public API in `clojure.java.io` on the
  JVM, missing here entirely). And `io/make-parents` builds `(apply io/file f
  more)` on the JVM, so it carries the same contract: an absolute child raises
  instead of quietly joining.

- **`(io/file nil)` and `(io/as-file nil)` answered an empty `File`, not `nil`.**
  Clojure extends `Coercions` to `nil`, so both are `nil` on the JVM. A `File`
  whose path is `""` is not a harmless stand-in for that: it names the process's
  working directory, so a nil that should have blown up at the coercion instead
  went on to read or write the wrong file. `as-relative-path` goes through
  `as-file`, so a nil child now raises there too, the way `(io/file "/a" nil)`
  does on the JVM, rather than joining as an empty segment.

  The `File` constructor null-checks for the same reason: `(File. nil)` and
  `(File. "/a" nil)` raise `NullPointerException` instead of reading the nil as
  `""`. `(File. nil "c")` still answers `c` — a null *parent* is the one the
  two-arg constructor accepts, and it means the child alone.

- **`jolt run` could not write any closure `clojure.core` makes.** `cycle`,
  `repeat`, `partial`, `comp` and the rest refused with "captured local … was
  optimized into the compiled code" — while a default `jolt build` wrote them
  fine, which is what made it look like an optimizer problem. It was not.
  Recovering a closure's captures needs to know which slot holds which name;
  Chez hands the slots back by position, and the names live in inspector
  information, which a release build does not generate. Generating it would cost
  +117% on the compiled prelude — a debugging model of every procedure in core to
  name a few hundred captures — and measured +53% on the binary with +70% on
  startup.

  Each registered literal is now built by a small per-site maker, so the image
  can call it once with distinct sentinels and learn the layout: every instance
  of a site shares one code object, so what it learns holds for all of them. No
  inspector information, +0.9% on the binary, and no benchmark moved more than
  1.01x. Literals that capture nothing skip it entirely — there is nothing to
  recover — which is what keeps dispatch-heavy code at 1.00x.

- **A `letfn` binding read as free in its own initialiser.** `:free-names` walked
  a `letrec`'s inits with only the *earlier* bindings in scope, which is right
  for `let*` and wrong for `letrec`, where every name is in scope in every init.
  A closure there listed a name it binds itself as a capture, and dumping it
  refused for a variable that was never captured.

- **A project could not build a `:jolt/native` library it declares.** Applying a
  project loads its native libraries before anything runs, so a project whose
  `native/` holds C sources and whose `:jolt/native` names the `.so`/`.dylib`
  built from them could not run its own build step: the task needed its own
  output to exist first. A **task** run now warns and carries on — it may be the
  thing that produces the library — while every other command still refuses to
  start without it. With this, the compile step can live in `deps.edn` `:tasks`
  instead of a makefile or a shell script beside the project.

- **A relative `:jolt/native` path was resolved against the wrong directory.**
  Candidates split the way `dlopen` splits them: a name with no separator is
  searched for on the loader path, a name with one is a path. A path is now
  resolved relative to the **project**, not to the current directory — so
  `native/libfoo.so` loads whatever the working directory happens to be. It used
  to work only when jolt was started from the project root, which meant it did
  not work at all under `bin/jolt`, which exports `JOLT_PWD` and then changes to
  its own tree. A "not found" error names the resolved path now.

- **A build through `bin/jolt` dropped stdlib Clojure code the CLI had already
  loaded.** `jolt.ffi` and `jolt.mvn-http` come into the driver process on the
  way to a build, and the build read its own loaded set as the set the new image
  inherits — so their Clojure half was never emitted, and calling
  `jolt.ffi/layout-size` from the built executable or library died with
  "Attempting to call unbound fn". `jolt run` masked it by compiling the source
  at require time. The preloaded set now comes from the runtime image
  (snapshotted in `loader.ss`, before the CLI loads anything) instead of from the
  build process. Builds through a release binary were already correct — baking
  the CLI closure into the image records the same distinction — so this only hit
  builds driven from a checkout. Diagnosed and fixed by @jasalt in #787, found
  while getting an aggregate-ABI raylib binding to run on Android.

## [0.7.29] - 2026-08-30

Two threads. Splicing a `defn` body at a call site follows **linkage** now
rather than a build flag, so every non-dev build inlines — and the round that
made that true shook four bugs out of the inline pass, every one of which an
`--opt` build could already hit.

The other is state images. `jolt.image` wrote maps, vectors and records, and
quietly could not write most of the rest of the language: a `promise` or an
`agent` came back holding dead OS primitives that worked until something waited
on them, a lazy sequence from `map` refused outright, a multimethod had its
dispatch tables walked and refused at a raw hashtable, and every closure
`clojure.core` made — `partial`, `comp`, `memoize`, `cycle` — refused because
core's fn literals were never recorded. That last one was written up as a limit
of the *format*; it was a limit of the build. Images round-trip every value kind
the language has now, or refuse by name and say what to do about it.

And one fix that belongs to neither: under load a socket read could report
end-of-stream on a connection that was merely not ready yet, because `errno` was
read after the syscall rather than at it.

### Added

- **Every non-dev build inlines, and the seed's var references are hoisted.**
  Splicing a `defn` body at a call site is sound exactly when the callee's var
  cannot be redefined under the copy, which is what direct-linking commits to —
  so that is now the whole condition. It used to also require `--opt`, which
  made the default release build emit a real call everywhere the optimized build
  emitted a spliced body, and Chez cannot make that up: it does not inline
  across top-level forms in a compiled file. `--opt` still selects the Chez
  compile parameters (no inspector or procedure-source information) and no
  longer changes what the compiler emits.

  Alongside it, `clojure.core` stopped re-resolving var names. Core ships in the
  seed image, which was minted with var-cell caching off on the theory that
  gensym-numbered cell names would break the mint's byte-fixpoint; they do not.
  Each late-bound reference was a `string-append` plus a hashtable probe —
  ~102ns against ~1ns hoisted — and 258 of core's 357 emitted vars carried at
  least one. `jolt-truthy?` and five other hot predicates became macros with
  `-fn` twins for value position, and the splicer learned `:fn`, `:loop` and
  `:recur` (alpha-renaming their binders) plus regex/interop literals, which
  were missing from its allowed set rather than refused by it.

  Measured against the previous release, min of 3 timed runs per side on one
  machine: sorted-access 0.28x, hash-eq 0.80x, loop-recur and char-scan 0.92x,
  nothing above 1.02x; app-to-app calls 1.70x in the default release build.
  Costs: +297KB of binary and ~3% of startup. `ci/bench-gate.sh` runs the
  benchmark suite against the previous release on every tag and `publish` waits
  on it, because `make test` and `make libconformance` both stayed green through
  a 5.4x regression in `bench/arrays` during this work — every answer correct,
  just much slower.

- **Cross-compiled managed-runtime libraries.** `jolt build --library` now
  composes with `--target` and `--target-pack`, retargeting the Scheme compile
  through the pack's xpatch and linking the shared object with the target C
  compiler, architecture flags, CSV files, and platform libraries. The target
  pack's Chez kernel and static dependencies must be position-independent.
  Cross `:jolt/native` static archives remain unsupported: build-time emission
  must load them into the host process as well as link target code, so they need
  separate host and target artifacts rather than a target linker flag alone.

- **Reader macros: the `#` dispatch table is open for punctuation.** Clojure's
  is closed on principle, so `#$`, `#%`, `#|` and every other unclaimed
  `#<punct>` is "No dispatch macro for: …" there. `jolt.reader/set-dispatch-macro!`
  puts a reader function on one character and `#<char>` reads through it from
  then on; `remove-dispatch-macro!` takes it back off and `dispatch-macros`
  lists what is registered. Two tiers: the default reads the next form normally
  and the function rewrites it, and `{:raw true}` hands the function the source
  string and an index and takes back `[form end-index]`, which is what a literal
  whose body is not Clojure data (a raw string, a heredoc) needs.

  Registration is a runtime call and jolt reads a file one top-level form at a
  time, so a file can register a macro and use it below — and `jolt build` loads
  the app from source before it scans it, so a built binary reads what a run
  reads. A character the reader already claims, and any letter or digit (which
  begins a `#tag` — a `#s` reader would swallow every `#some/tag`), are refused
  at registration rather than shadowed. `clojure.edn` never consults the table.
  Additive: nothing that reads on the JVM reads differently here.

- **String interpolation: `#$"…"` and `clojure.core.strint/<<`.** `#$` is the
  one reader macro jolt ships on the seam above, applying core.incubator's
  `~{form}` / `~(form)` grammar to a string literal at read time —
  `#$"a ~{x} b ~(inc x)"` reads as `(str "a " x " b " (inc x))`. A string with
  no marker reads as itself, so `#$"plain"` *is* `"plain"` and costs nothing at
  runtime. `clojure.core.strint` ships the `<<` macro under its original name
  and expands through the same grammar, so code written against core.incubator
  runs unchanged.

### Fixed

- **A socket read could answer EOF on a live connection, under load.**
  `jolt.socket`'s syscall wrapper read `errno` *after* the call, and twice — once
  to ask about `EINTR`, once about `EAGAIN`. `errno` survives only until the next
  thing that can set it, and reading it is itself a foreign call, so the two
  questions were not about the same value: under CPU load `recv`'s `EAGAIN` (35)
  read back as `ENOMEM` (12) often enough to matter. The retry branch was missed,
  the `-1` fell through, and the read reported end-of-stream on a connection that
  was simply not ready yet. Callers saw a socket close for no reason — inside a
  `go` block, an exception and a channel that closed with no value.

  `errno` is captured once now, at the syscall, and the branch reads that value;
  the same for `connect`. Measured on the poller stress case: 13 of 60 runs under
  load before, 0 of 100 after. `make errnocheck` fails if a syscall wrapper asks
  twice again — the two spellings are indistinguishable in review, and this one
  presented as a lost poller registration, which sent the search to the wrong
  subsystem entirely.

- **A closure `clojure.core` made could not be written to a state image.**
  `partial`, `comp`, `memoize`, `juxt`, `fnil`, `complement`, and every lazy
  sequence from an overlay fn — `cycle`, `repeat`, `repeatedly`, `map-indexed`,
  `distinct`, `dedupe`, `partition-by` — all refused. RFC 0009 documented that as
  a limit of the format. It was not: it was a limit of the build. Fn literals in
  the language's own namespaces were left unregistered so the seed prelude would
  stay byte-identical across a mint, and the consequence was an image feature
  that carried the part of your program state the compiler found convenient.

  Core's literals register now. A named inner literal registers too — it is bound
  under a unique `<name>$jf<n>` alias so the registry has a key that cannot
  collide, with its short name aliasing that, and the backtrace reader strips the
  suffix the way it already strips the splicer's `__ilN`. That is what the five
  lazy fns above needed: each closes a `lazy-seq` thunk over a `letfn`-bound fn,
  and the captured fn had no source however well the thunk itself travelled.

  Costs: the seed prelude grows 1.30MB to 1.75MB and the compiler image 833KB to
  1.04MB, so a built binary is about 1MB larger. Startup is unchanged (0.1806s
  against 0.1819s, min of 5) — the registrations are hashtable inserts.

  Still unwritable: a procedure nothing can name — one the runtime built rather
  than analyzed, and `seq`, `get` and `nth` in value position, which are
  `set!`-extended after their `def-var!` so the procedure you get was never the
  one recorded.

- **Ten `clojure.core` fns could not be written to a state image when used as a
  VALUE.** `seq`, `get`, `nth`, `peek`, `pop`, `min`, `max`, `mod`, `rem` and
  `quot`, plus `bit-and`/`bit-or`/`bit-xor`/`some?` — so `(tree-seq branch? seq
  root)` refused, and so would `(map seq colls)`. A core fn in value position
  compiles to the runtime's own procedure rather than the var's root, and an
  image writes a procedure as its var name; `def-var!` records that name for the
  procedure it was handed, but these are `set!`-extended afterwards (lazy
  sequences taught to `seq`, arrays to `nth`, the checked numeric layer taking
  over `min` and `quot`), and the extension was a new procedure nothing named.

  The names are re-registered after every extension has run. `make coreproc`
  sweeps the var table and fails if any core fn is unnameable in value position,
  so the next extension that forgets is caught here rather than surfacing as an
  image that will not write. It checks 719 fns.

- **`jolt.image/scan` disagreed with `dump!` about captured values.** The scan
  walked a registered closure's free values with a stub that inspected nothing,
  so a value whose capture was unwritable scanned clean and then refused at dump
  — the one thing the shared write/report verdict exists to prevent. It walks
  them for real now.

- **A lazy sequence built by `clojure.core` could not go into a state image.** A
  lazy seq written with `lazy-seq` already travelled — that thunk is an ordinary
  fn literal with a recorded source, so an infinite generator came back still
  generating and an unrealized side effect still had not run. `map`, `filter`,
  `range`, `take` and friends build their thunks as Scheme closures inside the
  runtime instead, and a closure carries its captured values where nothing can
  read them, so those refused. One core call anywhere in a chain was enough:
  `(rest user-lazy)`, `(take 3 user-lazy)` and `(map inc user-lazy)` all refused
  even though the seq they were built from travelled.

  Those producers record what they are — the producer and its arguments — rather
  than closing over them, so the image writes the producer's *name* and the
  arguments as data and restore re-applies it. Nothing is forced: an infinite
  seq still comes back generating, and a side effect still runs on the restoring
  side rather than at dump. The arguments walk as ordinary data, so a producer
  over another lazy seq nests and a self-referential one (`(def fib (lazy-cat
  [0 1] (map + (rest fib) fib)))`) closes on itself.

  Free: the forcer is a direct call where invoking the closure went through
  `jolt-invoke`, so the benchmark suite is unchanged (`bench/seqs` 1.00x).

  A chain already walked part-way travels too: every seq cell carries the same
  descriptor, not just the producer that made it. What still refuses is a lazy
  seq from a `clojure.core` *overlay* fn — `cycle`, `repeatedly`, `map-indexed`
  and the rest are fn literals in `clojure.core`, and the language's own
  namespaces are not registered, the same limit that stops a `partial` or `comp`
  closure travelling. That refusal names itself now instead of reporting an
  anonymous `#<procedure>`, and says to realize the seq first.

  Cost: `bench/seqs` 1.03x, the only benchmark of 22 outside 0.98–1.02x. The
  per-element cell carries a two-slot descriptor where it used to carry a
  closure. Measured against the previous release on one machine, min of 5
  alternating runs.

- **A var-rooted multimethod or `reify` was walked instead of named.** A named
  fn travels as its var's name and comes back as the live fn. A multimethod is
  code too, but it is a *record*, so `procedure?` missed it, nothing recorded its
  var name, and the image descended into its dispatch tables and refused at a raw
  hashtable naming nothing the user could act on. Both travel as the var's name
  now and restore as the live object — `identical?`, not a copy.

- **A namespace came back as a second namespace.** `find-ns` is identity-stable
  and a var round-trips to the identical var, so a restored namespace that merely
  `=` the live one was out of line with both. Namespaces are interned by name
  now, like keywords.

- **A transient was written silently, or half-written.** A transient vector
  travelled while a transient map refused on its backing hashtable. A transient
  belongs to the thread that made it, which a restore does not have; both refuse
  now, saying to call `persistent!` first.

- **`jolt.image/scan` hung on an infinite unwritable sequence.** A finding
  describes its object by printing it, and printing a lazy seq realizes it, so
  scanning `(repeat :z)` never returned. Seqs are described by kind now.

- **Synchronisation primitives travelled into state images as dead objects.** A
  record carrying a mutex or condition variable had no image walker of its own,
  so the generic record copy serialised the primitive itself. What came back was
  a fasl copy, not a live kernel object — and an *uncontended* acquire on one
  succeeds, so a promise, future or agent read out of an image looked perfectly
  healthy right up until something actually waited on it, and then it was
  `Exception in mutex-acquire: failed: Invalid argument` from whichever thread
  got there first, with no path and no name. `jolt.image/scan` reported nothing.

  Affected `promise`, `future`, `agent`, the per-node lock a lazy sequence and a
  seq cell take once the process is multi-threaded, `core.async` channels, and
  the tap and fibers queues — `atom` and `ref` were already right, because they
  rebuild through their own constructors, which is the shape this generalises. A
  primitive is now written as an inert marker and a fresh live one is minted on
  read. Done at the walk rather than per bearing type, so a record that gains a
  mutex later is carried correctly without anyone rediscovering this; restoring
  an image written before the fix mints a live primitive over the raw one, so
  old files are healed rather than merely readable.

  Image format version 6. Versions 2 through 6 still read; a version 6 image is
  refused by an older build with the version in the message, as before.

- **A restored `future` or `agent` could come back silently wedged.** Both carry
  state that says a thread is mid-flight, and both used to travel with it. A
  future written while still running restored as one that nothing would ever
  complete, so `deref` hung forever; an agent written while an action was in
  flight restored believing it had a busy worker, so every later `send` queued
  behind nothing and its state never moved again. Neither said anything.

  A state image carries state, not execution. A running future is refused now,
  with a message that says so and points at `deref`-ing it first (and stubs
  under `{:unwritable :stub}` like any other unwritable object); a completed one
  is just its value and still travels. An agent's state, validator and error
  handling travel, while its queue and in-flight flag do not, so a restored
  agent accepts work immediately.

- **A var defined twice froze the first definition into callers compiled between
  the two.** With splicing on by default, this legal Clojure gave one binary two
  answers:

  ```clojure
  (defn greet [] "first")
  (defn call-it [] (greet))       ; spliced "first" — frozen
  (defn greet [] "second")
  ```

  `(apply call-it nil)` answered `"first"` while `(call-it)` answered
  `"second"`, and neither matched `jolt run`, `--no-direct-link`, or the JVM
  (all `"second"`). Direct-linking alone was never the problem — both defs
  assign the same binding and the second wins — so a var the program defines
  more than once is no longer stashed for inlining at all, which converges on
  the direct-linked call and restores last-def-wins. A `(declare x)` ahead of
  the real `defn` is one definition, not two, and still inlines.

- **`--tree-shake` dropped every inlined frame from a backtrace.** A callee
  whose call sites were all spliced has no reference left in the graph, so the
  shake pruned its def — and with it the source registration that maps an
  inlined frame back to `ns/name (file:line)`. A shaken binary printed one frame
  where the same build unshaken printed three. Spliced callees are graph roots
  now, so a `--tree-shake` trace reads like every other trace.

- **A named inner fn in a spliced body was reported twice, under a mangled
  name.** The inline chain was stamped through the nested `fn`, but a nested fn
  is emitted as its own lambda and has its own runtime frame, so the reporter
  expanded that frame as spliced code as well and printed the enclosing callee a
  second time. The chain now stops at the `fn` boundary, and the `__ilN` suffix
  the splicer's alpha-rename adds is stripped from a frame name rather than
  shown.

- **A closure returned by an inlined fn would not go into a state image.** An
  anonymous fn travels as its source form plus the values it captured, recovered
  from the live closure by name; a spliced copy matched neither half — its
  binders are renamed, its captures may be the caller's locals, and a constant
  argument folded into the body leaves no capture at all — so the pass dropped
  the registration and `jolt.image/dump!` refused with `cannot write
  #<procedure>`. The same program wrote the closure fine under `jolt run`, which
  does not splice, so this was a divergence between running a program and
  building it, in every default release build.

  A spliced copy now carries its own capture list: the source names still build
  the wrapper the restore compiles, and alongside them the copy records what
  each one became — the live variable's new name, or the constant value itself,
  which then travels as data. It records the namespace the form was written in
  too, so a callee's private and aliased names still resolve when the copy lives
  in another namespace. `clojure.*` and `jolt.*` literals stay unregistered
  whether or not they were spliced, which is what they are when they run
  un-spliced. The refusal message for a fn that genuinely has no source no
  longer blames inlining.

  A spliced literal registers per call site, so a real app emits more of them —
  1082 to 1397 building metosin/malli, +6.7% of emitted Scheme and ~5% of build
  wall time (7.50s to 7.88s, min of three alternating runs). Runtime is
  unaffected: the benchmark suite is 0.96x-1.02x across all 22 rows and the
  built binary starts in the same time, while the record-hint fix below takes
  1.4% off its size.

- **A `^Record` param hint was dropped by a splice.** `:phints` are what types a
  record parameter when nothing about the caller could be inferred, which is the
  open-world case the hint exists for — and the inline stash never carried them,
  so a callee that read its own field by static slot fell back to the generic
  lookup the moment its body was copied into a caller. A declaration the user
  wrote should not depend on whether the compiler happened to inline the fn, so
  the splicer moves it onto the local that replaced the param, where it survives
  the later passes rearranging the bindings around it.

- **A `*data-readers*` entry holding the reader function itself.** The load path
  accepted only a symbol naming the reader var, so a table entry added as
  `(alter-var-root #'*data-readers* assoc 'my/tag (fn …))` — the shape the JVM's
  own table uses — reached the analyzer as `(#<procedure> 'form)` and died there
  as "unsupported form". A function value is now applied at load time like a
  resolved symbol reader (a form result is spliced as code, a value result as a
  value), and a var value resolves through its root, matching what `read-string`
  already accepted. A table entry that is not a reader at all now says so
  instead of emitting a form the analyzer can only call unsupported. `jolt build`
  bakes the table into the binary and assumed every value was a symbol, so it
  died in `symbol-t-ns` on a function; a function entry is skipped there now —
  a closure has no literal form, and the top-level code that installed it is in
  the binary and re-runs at startup, which puts the entry back.

- **An unregistered `#tag` names the tag.** A `#foo/bar` literal with no reader
  function reached the analyzer as a form it had no leaf for and failed with
  `jolt/uncompilable: unsupported form`, which names nothing to fix. It reports
  `No reader function for tag foo/bar` now, as the JVM's reader does.

- **A built binary runs its app's top-level forms past `Sbuild_heap`.** Chez does
  not schedule a forked thread until the boot file has finished loading, and
  `jolt build` put the app's namespace top-level forms in that window — so a
  top-level form that spawned a thread and waited for it never got an answer in a
  built binary, while working under `jolt -m` and in the REPL. Measured at
  namespace top level in a binary: `@(future …)` hung forever, an `agent` `send`
  + `await-for` never ran the action, a `promise` delivered from a `Thread` timed
  out, and `.join` returned with the thread still alive. The shape that found it
  was a top-level `(clojure.java.shell/sh …)`, which drains the child through two
  futures and derefs them with no timeout — so it hung the process with no
  diagnostic at all.

  The app's forms now run from the `scheme-start` launcher instead, before
  `-main` and in the same order relative to optional natives and the source-root
  reset that they had at boot. They also run inside the launcher's guard now, so
  a throw from an app top-level form reports like any other error instead of
  escaping as Chez's opaque dump. Startup is unchanged (the work moved, it did
  not grow): 224/220/225 ms against 225/224/228 ms before, on the same app.

- **`var-set` writes a thread binding, or throws.** `(var-set #'v x)` with no
  thread binding for `v` wrote `v`'s root and returned `x`; the JVM raises
  `IllegalStateException` "Can't change/establish root binding of: v with set",
  because `var-set` is `Var.set` — the same entry point `set!` on a var lowers
  to, and `set!` already got this right. A root write is process-wide and visible
  to every other thread, so code reference Clojure refuses to run was quietly
  mutating shared state. The validator now runs ahead of the binding lookup too,
  as `Var.set` does, so a rejected value reports the validator rather than the
  missing binding.

  Two core forms leaned on the old fallback and are now written the way Clojure
  writes them. `with-redefs-fn` rebinds the **root** (`alter-var-root`) and saves
  it through `.getRawRoot`, so a redef under an enclosing `binding` no longer
  writes the thread-local value over the var's real root on the way out, and a
  redef reaches threads that did not inherit this one's bindings.
  `with-local-vars` gives each local a thread binding for the extent of the form
  instead of a root, so `thread-bound?` on one answers `true` and the cell reads
  back unbound once the form is left. `bound?` follows `Var.isBound` — a root
  **or** a binding in scope.

- **`ex-info` with nil data.** `(ex-data (ex-info "m" nil))` answered `nil` where
  the JVM answers `{}` — `ExceptionInfo`'s constructor rejects a null map, so an
  ExceptionInfo whose data is nil cannot exist. It flipped a
  `(when (ex-data e) ...)` branch silently, and `Throwable->map` and `str` were
  wrong with it. The coercion is in the constructor, which is what the emitter
  lowers the `ex-info` native op to, so compiled call sites are covered too.
  Fixes #771.

- **Binding a non-dynamic var reports its real class.** `push-thread-bindings`
  on a var that is not `^:dynamic`, and `set!` on a var with no thread binding,
  raised an `ExceptionInfo`; the JVM raises `java.lang.IllegalStateException`
  with the same message. Both are typed throwables now, so `ex-data` on them is
  `nil` the way it is on the JVM.

- **`resolve` no longer answers for classes that do not exist.**
  `(resolve 'fake.pkg.String)` handed back a class token because the class-graph
  lookup fell back to the last dotted segment, so any `a.b.String` matched
  `java.lang.String`'s registration. Feature detection — which is what `resolve`
  on a class name is for — read that as "this class is present" and took the
  branch. Answers `nil` now, like the JVM.

### Performance

- **`bench/seqs` is 1.03x slower**, the only benchmark of 22 outside 0.98–1.02x.
  Every lazy sequence cell carries a two-slot descriptor of the producer that
  made it where it used to carry a closure — which is what lets a lazy sequence
  travel in a state image unforced, walked or not. Measured against the previous
  release on one machine, min of 5 alternating runs.

- **A built binary is about 1MB larger** (roughly 4%). `clojure.core`'s fn
  literals now record their source so core's closures can travel, which grows
  the seed prelude from 1.30MB to 1.75MB and the compiler image from 833KB to
  1.04MB. Startup is unchanged — 0.1806s against 0.1819s, min of 5 — because
  the registrations are hashtable inserts, not work.

- App-to-app calls are about 1.70x faster in a default release build, and
  `clojure.core` no longer re-resolves var names at each reference. See the
  inlining entry under Added for the per-benchmark figures.

### Internal

- The `:documented` half of `test/conformance/known-divergences.edn` is gated.
  Those entries are prose about divergences that are not corpus rows, and
  nothing ever ran them: an audit found entries claiming the JVM throws on
  `(ex-info "m" nil)` and on `(resolve 'java.lang.module.ModuleFinder)`, neither
  of which any JVM run reproduces, and two entries whose divergence had been
  fixed years apart with no one noticing. Every entry now carries a `:check` —
  either an expression with its recorded value on each runtime, or `:prose` with
  a reason it cannot be one. `certify.clj` verifies the JVM side, `make
  documented` the jolt side, and both reject an entry whose two sides agree.


## [0.7.28] - 2026-08-27

Two things a namespace does constantly — name a class and read a form — were
answering from the wrong place, and this release is mostly the fallout of
fixing that. `resolve` was reading jolt's internal class tokens as if they were
namespace imports, so it answered classes the namespace never imported; a
nested class was mapped under its innermost name, minting `clojure.core`
mappings the JVM has none of; and 38 of the 96 java.lang auto-imports had no
class-model row, so they resolved to nil. On the reader side, a backtick and a
set literal both survived into macro arguments as jolt's own shapes rather than
the language's, so a macro inspecting its argument saw
`(clojure.core/syntax-quote …)` where the JVM has a quoted symbol, and saw a set
as a map — `map?` answering **true** for `#{}` is what took typedclojure and
reitit down. Both are lowered up front now, over the whole form.

The io shims got the same treatment. `URL/openStream` handed back a Reader where
the JVM hands back an InputStream, so the documented
`(InputStreamReader. (.openStream u))` composition could not work and failed
several layers away with a message about a character not being a number.

New in this release: bb.edn `:tasks`, one-shot escape continuations, class
reflection and `clojure.reflect`, `jolt.host/extend-class!` for extending a shim,
digit separators in number literals, and FFI fixed-array layouts with atomic
native-error capture.

### Fixed

- **`URL/openStream` on a `file:` URL answered a Reader, not an InputStream.**
  It handed back a `StringReader` — content-correct on its own, but the wrong
  half of the io hierarchy, so the documented composition
  `(InputStreamReader. (.openStream u))` could not work: an `InputStreamReader`
  drives its argument's `read(byte[], int, int)`, and a Reader answers that by
  writing *characters* into the byte array. The failure surfaced far from the
  call, as `#\{ is not a number` out of tools.reader, which is how
  typedclojure's config load broke. `openStream` is a `FileInputStream` now,
  like the JVM. Wrapping a Reader in an `InputStreamReader` (or a Writer in an
  `OutputStreamWriter`) raises `IllegalArgumentException` where the mistake is,
  rather than letting the byte/char confusion surface from the eventual read.
  Reported by @burinc.

- **A relative `file:` URL resolved against the process directory.** The JVM
  resolves one against `user.dir`; jolt read it from the current working
  directory, which under the launcher is the jolt installation root — so
  `(slurp (java.net.URL. "file:config.clj"))` raised `FileNotFoundException`
  for a file sitting in the project.

- **The file constructors leaked a raw Chez condition instead of
  `FileNotFoundException`.** `FileInputStream`, `FileReader`, `FileOutputStream`
  and `FileWriter` raised something uncatchable as `java.io.FileNotFoundException`
  when the path could not be opened, so a caller branching on that class — the
  reason `slurp` already raised it — never ran its fallback. A directory is
  refused at construction now, like the JVM, rather than at the first read.

- **A nested set literal reached a macro as jolt's reader form.** `#{...}` reads
  as `{:jolt/type :jolt/set :value [...]}` for the analyzer, and only a set that
  *was* the whole macro argument got turned back into a set. A nested one arrived
  as the raw map, so `set?` answered false and `map?` answered **true** — the
  obvious `cond` over `vector?`/`set?`/`map?` routed a set into the map branch and
  died on the key `:jolt/type`. Clojure's `#{...}` is a reader macro, so a real
  set exists before any macro runs; the whole call form is normalized now, at any
  depth — including `&form`, so a macro reading `(nth &form 1)` rather than its
  parameter sees the same shapes its arguments do. Reported by @burinc (#762).

- **`getSimpleName` and `getCanonicalName` ignored class nesting.**
  `(.getSimpleName java.util.Map$Entry)` answered `"Map$Entry"` where the JVM
  answers `"Entry"`, and `getCanonicalName` kept the `$` where the JVM spells
  nesting with a dot. Splitting on `$` is not the rule: the JVM reads nesting
  off the class file, and a Clojure fn class merely contains one and is
  top-level, so `(.getSimpleName (class inc))` is `"core$inc"`, not `"inc"`.
  Both now ask the class model whether the name before a `$` is itself a class,
  which is the same distinction. Surfaced by the auto-import work above, which
  made `Thread$State` resolvable for the first time.

- **A built binary could intern a stdlib var and leave it unbound.** `jolt build`
  skips any namespace already loaded in the build process as "already in the
  image", which is right for the runtime image every app shares and wrong for
  the CLI's own AOT closure — an app image is a different image and carries none
  of it. `jolt.ffi` and `jolt.mvn-http` are in that closure and in neither the
  runtime image nor the fasl manifest, so `layout-size`, `field-offset`,
  `read-field`, `write-field`, `errno`, `errno-message`, `fetch` and `fetch*`
  were interned but unbound in a built binary and failed at the call rather than
  the build. `jolt run` compiles the source at require time and masked it
  entirely. The loader records why a namespace is preloaded, not merely that it
  is. Thanks to @burinc.

- **38 of the 96 java.lang auto-imports did not resolve.**
  `(resolve 'ExceptionInInitializerError)`, `'StringBuffer`, `'Process`,
  `'ThreadLocal` and 34 others answered nil where the JVM answers the class —
  they had no row in the class model, so no class token. They have one now:
  a name and an ancestry, not an implementation, so `(StringBuffer. "a")` still
  has no constructor. Four of the 96 are not java.lang, and `ns-imports` named
  them wrongly: `BigDecimal` and `BigInteger` are `java.math`, `Callable` is
  `java.util.concurrent`, `Compiler` is `clojure.lang`.

- **A nested class was mapped under its innermost name.** The name a namespace
  maps a class under is the part after the last dot, `$` and all —
  `java.util.Map$Entry` is `Map$Entry`. jolt read the innermost segment instead,
  which minted `clojure.core` mappings for `Entry`, `Seq`, `RSeq`, `SubVector`
  and friends that the JVM has no mapping for, and left `Thread$State` and
  `Thread$UncaughtExceptionHandler` with no mapping at all.

- **A backtick was still there when a macro read its own argument.** Clojure's
  `` ` `` is a reader macro, so a form is past its backticks before anything can
  look at it. jolt reads one to a `(clojure.core/syntax-quote FORM)` marker and
  lowered it only when the marker itself was analyzed, which left it visible to a
  macro reading its argument forms and in quoted data: `(pr-str '`foo)` gave
  `"(clojure.core/syntax-quote foo)"` where Clojure gives `"(quote ns/foo)"`.
  typedclojure's `(f/sub-f sb `call-abstract-many* opts)` asserts its argument is
  `(quote qualified-sym)` and got the marker, so `typed.clj.checker` failed to
  load. The analyzer lowers every marker in a top-level form up front now — same
  lowering, at the reader's moment. A form with no backtick in it is returned
  unchanged and unallocated. tools.reader's three syntax-quote failures go with
  it.

- **`resolve` answered a bare class name the namespace never imported.** A class
  name is a per-namespace mapping on the JVM — the java.lang auto-imports
  everywhere, an `(:import ...)` in the namespace that asked, a
  `deftype`/`defrecord` in the namespace that defines it — and `resolve` answers
  only what the namespace maps. jolt keeps a `clojure.core` class token for every
  class it models so a bare `Pattern` self-evaluates, and `resolve` was reading
  those as mappings: `(resolve 'Path)` answered `java.nio.file.Path` in a
  namespace with no import of it. A macro that guards on `(resolve Name)` before
  defining a type therefore skipped the definition — typedclojure's
  `(u/def-object Path ...)` emitted neither the deftype nor the `Path-maker` fn
  it also emits, and the checker failed to load.

- **`ns-imports` was the same 96 auto-imports for every namespace.** An
  `(:import ...)` and a `deftype`'s own class are namespace mappings and now
  show up in it, mapped to classes rather than class-name strings. The other half
  of that line: a class mapping is an import, never an intern, so `ns-interns`,
  `ns-publics` and the refers built from `clojure.core`'s publics no longer
  report one — which is what made `(ns-map 'user)` answer a var for `String`
  where the JVM answers the class.

- **`compare` ignored a deftype's declared `Comparable`.** `(.compareTo a b)`
  reached the type's own method, but `compare` did not, so it raised "cannot be
  compared to" for values that carry an ordering — and `sort`, `sorted-set` and
  `sorted-map-by`, which all route through `compare`, raised with it. The JVM's
  `Util.compare` calls `((Comparable) o1).compareTo(o2)` for anything
  implementing the interface. A record that does NOT declare `Comparable` still
  refuses to compare, as before.

- **`bases` on a deftype/defrecord type token walked the constructor procedure,
  not the class.** `(bases Rec)` answered `clojure.lang.AFunction` where
  `(bases (class inst))` gave the record's real interfaces. `supers` and
  `ancestors` already routed both spellings through one question; `bases` was
  the one that did not.

- **A type token and `(class inst)` were not `=`.** They are the same Class
  object on the JVM, so `(= Rec (class (->Rec 1)))` should be true and the two
  spellings should be one key in a map. Both now hold: the token's identity hash
  is seeded from its class name where it is registered, so `=` and `hash` agree
  without taxing the procedure-hash fast path.

- **`Class.getDeclaredField` could never find a record's field.** It matched the
  name against `(str :x)` — `":x"` — while `getDeclaredFields`' `getName`
  reported `"x"`, so a lookup by the name jolt had just handed you raised
  `NoSuchFieldException`. Both spellings go through one helper now.

- **Six interfaces were modeled as concrete classes.** `java.util.Queue`,
  `Deque`, `Map$Entry`, `java.nio.file.Path`, `PathMatcher` and `Watchable`
  answered `false` to `.isInterface` and named a superclass where the JVM
  returns null — `Map$Entry` reported `clojure.lang.AFunction`. Found by probing
  the reference JVM for every `java.*` name the class graph models rather than
  by eye.

- **The dev boot cache broke every project-aware `-e`.** `make devboot` emits
  jolt.main AOT'd into `target/dev/flat.so` without loading `cli-core.ss` into
  the build image first, so `jolt.host/run-expr-string` wasn't a var at
  emission and compiled as a host-static class reference — `jolt -M -e`,
  `-A`, `-Sdeps`, and `-e` in any directory with a deps.edn all died with
  "No such var: jolt.host/run-expr-string" whenever bin/jolt took the cache.
  `build-jolt.ss` has loaded cli-core.ss for exactly this reason all along;
  make-devboot.ss now does the same.

- **`set!` on the `(. obj field)` / `(. obj -field)` instance spelling
  compiled.** The analyzer accepted only the `(.field obj)` sugar, but a
  deftype method conventionally writes its mutable fields as
  `(set! (. this -field) v)` — typedclojure's `def-type` does. And the deftype
  macro's mutable-field rewrite treated the member position of a dot form as a
  value, so `(. this v)` with a mutable field named `v` rewrote its member
  symbol into a field read and produced an uncompilable form. Both spellings
  now compile, and the rewrite leaves member positions alone.

- **Loading a namespace from compiled code did not bind the compiler-flag vars,
  so every namespace with `(set! *warn-on-reflection* true)` at its top level
  recompiled from source on every load.** The set! wrote the root binding and
  raised; each caller read that raise as a broken artifact and quietly rebuilt.
  A cached `.so` was deleted and recompiled on every run without ever being
  served, and the embedded fasls for `babashka.fs` and `babashka.process` — both
  of which use the idiom — had never once been used. `RT.load` brackets a
  compiled class's init with those vars just as `Compiler.load` brackets a
  source load, and `jolt build` already did it for the namespaces it AOTs into a
  binary; the loader's own three compiled paths were the ones left out.

  It only looked fine because an enclosing file load leaves the same frame up,
  so a `require` from a script worked and the same `require` from `-e`, a REPL,
  or an nREPL eval did not. `jolt -e "(require 'babashka.process)"` was 0.68s
  and is 0.14s; a `bb.edn` task calling `(shell …)` was 0.69s and is 0.15s.
  The idiom is common in ported libraries — 25 of the conformance-suite
  libraries use it, rewrite-clj and next-jdbc in dozens of namespaces each — and
  none of them could hit their cache.

  Two members of the same family, fixed alongside: `jolt -e` (and the `-` stdin
  script) now binds the compiler-flag vars the way `clojure.main` wraps every
  entry point, so a top-level `(set! *warn-on-reflection* true)` works under
  `-e` instead of raising; and `load-string` listed two of the three vars by
  hand, so a `(set! *unchecked-math* true)` inside one escaped into the caller
  and changed what its later arithmetic compiled to. All the sites now share
  one definition of the frame.

- **A failing `:tasks` shell command exited 0.** `jolt <task>` for a string task
  called the shell and dropped its status on the floor, so a task that failed
  reported success and a CI step built on one could not fail. It now exits with
  the command's status, like every other task failure.

- **The resolution cache keyed on the deps.edn FILES, not on the deps it was
  about to expand.** `-Sdeps '{:deps …}'` merges a map that is in no file, so
  two runs differing only in `-Sdeps` shared a `.jolt/cpcache` entry and the
  second got the first's roots. The effective dep map (with `:override-deps` /
  `:default-deps`) is part of the key now.

- **`File.canRead`/`canWrite`/`canExecute` and `Files.isReadable`/`isWritable`/
  `isExecutable` answer permissions, not existence.** All six reported whether
  the file exists, so a read-only file came back writable and every regular file
  came back executable — code testing writability before a write took the wrong
  branch and found out at the open. `babashka.fs/writable?` and its siblings
  route to `Files/is*able` and inherited it. They now ask `access(2)` about the
  effective user, through one shared predicate so the `java.io` and
  `java.nio.file` spellings cannot drift apart.

### Added

- **`(clojure.lang.Delay. thunk)` and `Var.getRawRoot`.** The class `(delay …)`
  already reported had no constructor, so a library spelling its own `delay`
  macro as the constructor could not build one — fully-satisfies'
  `safe-locals-clearing` does, to control when locals clear. `getRawRoot` reads
  a var's root past any thread binding, which is how its `requiring-resolve`
  reads the global `clojure.core/*loaded-libs*` rather than whatever a load has
  bound over it.

- **Class reflection: `getModifiers`, `java.lang.reflect.Modifier`, and the
  method/field surface.** `Class.getModifiers` derives the JVM bitmask from the
  class graph (jolt has no bytecode to read one out of), with the final,
  abstract and enum marks taken from a probe of the reference JVM for every
  `java.*` class the graph models. `Modifier` ships its constants, its
  predicates and `toString`. `getMethods`/`getDeclaredMethods` return real named
  `Method` objects — `getName`, `getDeclaringClass`, `getParameterCount`,
  `invoke` — and `getFields`/`getField` join the declared pair. A type token
  answers every `java.lang.Class` method by delegating to the same table
  `(class inst)` uses, rather than the four names that were listed by hand.

  The member sets are what jolt's registries know a class declares — a
  deftype/defrecord's fields, and every method registered against the type by
  whichever protocol declares it. A host class jolt models by other means
  (String's methods are a `cond`, not data) reports none rather than guessing at
  the JVM's set. `:bases` and `:flags` are faithful.

- **`clojure.reflect`**, ported from the reference: the `Reflector` and
  `TypeReference` protocols, `flag-descriptors`, the `Constructor`/`Method`/
  `Field` records, `type-reflect` (`:ancestors` included) and `reflect`. The
  reference `JavaReflector` is a thin layer over the Class methods above, so the
  port is the reference apart from the member-set model. There is no
  `AsmReflector` — it reads `.class` bytes, which jolt has none of.

- **`clojure.core/assert-args`**, the reference implementation verbatim.
  Private on the JVM, but macro-heavy libraries reach it as
  `@#'clojure.core/assert-args` — typedclojure consumes it from four
  namespaces and could not load without it.

- **`clojure.repl/demunge`**, the reference implementation over
  `clojure.lang.Compiler/demunge`. typedclojure's `gen-datatype*` resolves it
  lazily via `requiring-resolve`, which returned nil and surfaced later as an
  opaque cast error.

- **bb.edn tasks (#578).** jolt reads a project's `bb.edn` and runs its `:tasks`
  with babashka's semantics, so a bb.edn written for `bb` runs under `jolt`:

  ```clojure
  ;; bb.edn
  {:paths ["script"]
   :tasks {:requires ([babashka.fs :as fs])
           clean {:doc "remove build output" :task (fs/delete-tree "target")}
           build {:doc "build" :depends [clean] :task (shell "make")}}}
  ```

  ```
  $ jolt tasks          # list them
  $ jolt build          # or `jolt run build`, or `jolt run --parallel build`
  ```

  `:init`, `:requires`, `:enter`/`:leave`, `:depends` (each dependency runs once
  per invocation, a cycle is an error rather than a hang), `:doc`, `:private`,
  `:extra-paths`/`:extra-deps` and `:override-builtin` all work. Bodies run in
  the `user` namespace with `babashka.tasks` referred, so `shell`, `jolt`,
  `clojure`, `run` and `current-task` need no require, and the arguments after
  the task name are `*command-line-args*`. A failed `shell` exits with the
  child's status and no jolt stack trace over it. `:pods` are not supported and
  say so.

  Two things differ from babashka deliberately. `clojure` runs the jolt CLI
  rather than the JVM Clojure CLI — on this host jolt is the Clojure, and the
  point is not to need a JVM; `(shell "clojure" "-M:test")` still reaches the
  real one. `jolt` is the same function under the name that says what it does,
  and the spelling to prefer in new task maps. And a STRING task body is a
  shell command line, which is what jolt's own `deps.edn` `:tasks` have always
  meant; babashka evaluates it as an expression, where it does nothing.

  With no `deps.edn`, `bb.edn` is the project config for every command — its
  `:paths` and `:deps` drive `run`, `repl` and `build` too. With both files,
  `deps.edn` is the project and `bb.edn` contributes its `:tasks`; its
  `:paths`/`:deps` join a task run only, so a `bb.edn` `:paths ["script"]`
  cannot displace the app's own source roots on every other command. A task
  name declared in both files is babashka's.

- **`jolt.host/extend-class!`: add to or replace the shim jolt already has for a
  Java class (#575).** jolt models the `java.*` surface with hand-written shims,
  and each one covers the methods jolt and the ported libraries have needed so
  far. A library that needed a method a shim did not have had one way out:
  `__register-class-ctor!` its own replacement for the whole class, which every
  other namespace in the process then silently inherits — the shape that made
  `(.readAllBytes body)` unresolvable for code that never asked for a
  `ByteArrayInputStream` shim.

  ```clojure
  (jolt.host/extend-class! "java.io.File"
    {:methods {"toPath" (fn [self] ...)}})
  ```

  Two tiers. The default is consulted at the end of the dispatch chain, where
  the call would otherwise raise "No matching method", so it can only fill gaps
  — nothing jolt already answers changes behaviour. `:override true` is
  consulted before every built-in arm and replaces jolt's method for every
  caller in the process; it is reported under `JOLT_DEBUG` the way a replaced
  constructor is. Both are keyed by class name and resolved through the modeled
  class graph, so a registration on `java.io.Reader` answers for a
  `StringReader` and either spelling of the name matches. `:statics` and
  `:ctor` in the same spec cover `Class/member` and `(Class. ...)`.

  Overriding a method on `String`, `Keyword` or `StringBuilder` is refused: the
  compiler lowers those receivers directly at proven call sites, so an override
  would apply at some call sites and not others. Adding a method jolt does not
  have on them is allowed.

- **`jolt.continuations`: one-shot escape continuations (#736).** `call-cc` and
  `letcc` expose the capability jolt's runtime already runs on — the fiber
  park/resume switch, the throw site a backtrace is walked from — to jolt
  programs. `(letcc [return] ...)` unwinds out of any depth, including out of a
  callback a library invoked, where `reduced` cannot reach. JVM Clojure
  cannot have this (the JVM cannot capture its stack), so
  code using it is jolt-only by design, like `jolt.scheme`; it is purely
  additive and no Clojure program is affected.

  An escape is a real exit, so a `finally` between the capture and the escape
  runs and a `binding` is restored — the opposite of a fiber park, which drops
  those winders because a park is not an exit. A park between the capture and
  the escape is fine within one fiber: the scheduler captures and restores the
  fiber's whole stack segment.

  Re-entrancy is not supported and never half-works. Invoking an escape twice,
  invoking one after its `call-cc` returned, or invoking one from a thread or
  fiber other than the one that captured it each raise
  `IllegalStateException` naming the rule. The last of those is not pedantry:
  the raw host primitive HANGS the process there, with no error at all.
- **Digit separators in number literals (#389).** `1_000_000` reads as
  `1000000`. The rule is Java's, which is the one someone writing a grouped
  literal expects: an underscore must sit between two digits, never against the
  sign, the `0x` / `NrDDD` radix marker, the decimal point, the exponent
  marker, the ratio slash, or the `N`/`M` suffix. So `0xFF_FF`, `0_52`,
  `36rR_Z`, `1_0.5_5`, `1_0e1_0` and `3_000N` all read, while `1_`, `0x_52` and
  `1e_5` raise `Invalid number` exactly as before. A run of underscores counts
  as one separator (`5_______2` is `52`), and a leading underscore is still an
  ordinary symbol — `_1` is the symbol `_1`, unchanged.

  A jolt superset: the JVM raises `Invalid number` on every literal this adds,
  so nothing that reads today changes meaning. `clojure.edn` deliberately does
  NOT accept separators — edn is an interchange format whose integer grammar
  has none, and jolt's printer never emits one, so the only thing accepting
  them there would add is a hand-written config that reads on jolt and fails in
  every other edn reader.
- **Atomic native-error capture for `jolt.ffi`.** `foreign-fn` and `defcfn`
  accept `{:capture-native-error true}` and return `[native-result error-code]`,
  capturing POSIX `errno` or Windows `GetLastError` in the foreign-call return
  path before cleanup or collector reactivation can overwrite it. It composes
  with `{:blocking true}`; omitted/false capture keeps the existing scalar
  result, and unsupported targets or malformed options fail closed.
- **Fixed arrays in `jolt.ffi` layouts and by-value structs.** A field type may
  be `[:array positive-count element-type]`; element types may themselves be
  fixed arrays or structs. Array indices are integer components in field paths,
  so `[:params 3]`, `[:events 1 :frame]`, and `[:matrix 1 2]` work with
  `field-offset`, `read-field`, and `write-field`. Array container paths still
  expose their base offset. Layout and aggregate ABI gates compare scalar,
  nested-struct, and multidimensional arrays against compiled C witnesses.
  Metadata retains one entry per declared array shape and resolves indexed
  offsets from ABI-derived element strides, so even million-element flat and
  nested arrays remain compact.

### Fixed

- **A backtick nested inside a backtick is lowered inside-out.** Defining a
  macro that defines a macro — `` `(defmacro f [x#] `(g ~x#)) `` — raised
  "Unable to resolve symbol: x#" at the point the OUTER macro was defined,
  where `x#` names a parameter of an inner macro that does not exist yet. It
  was never gensym-specific: `` `(defmacro f [a b] `(vector ~a ~b)) `` failed
  the same way with no `#` anywhere. jolt lowers syntax-quote in the analyzer
  rather than the reader, and the compile path did not recognise a nested one
  at all, so the outer walk claimed the inner template's `~unquotes` as its
  own. It now lowers the inner backtick first, with its own auto-gensym scope,
  and walks the construction code that produces — the order the JVM's reader
  gets for free, and what makes the `x#` in the parameter vector and the `x#`
  in the body the same gensym. `~'~x` carries the outer macro's argument into
  the inner expansion again for the same reason. The reader's data path
  (`read-string`) already did this; only the compile path was missing it.

  One thing this makes visible for the first time: a nested backtick is the
  only place a syntax-quote's construction code becomes a value rather than
  being compiled, so it is the only place jolt's `__sqcat`/`__sq1` shows where
  the JVM has `seq`/`concat`. Both evaluate to the same expansion; only a
  program that prints or structurally walks the inner template can tell.
  Recorded in `known-divergences.edn` as `:impl-detail`.

- **A native library carrying its own static copy of another one is reported
  instead of going inert (#731).** A declared `:jolt/native` linked against
  another's static archive gets a private copy of that library's globals, so
  writes through one are invisible to the other — raygui built against
  `libraylib.a` reads a mouse that never moves and every control goes dead with
  no error anywhere. jolt cannot merge the copies (the duplicate is baked into
  the `.so`; the fix is to rebuild the dependent against the shared library),
  but it no longer stays quiet: binding such a symbol prints a duplicate-native-
  symbol report naming the symbol and the libraries, and
  `jolt.ffi/defining-libraries` answers which libraries supply distinct
  definitions of a symbol.

  The check keys on the resolved ADDRESS, not on how many handles answer:
  `dlsym` searches a handle's dependency chain, so a dependent linked correctly
  against the shared base also resolves the base's symbols through its own
  handle. Counting handles would have flagged exactly the build that got it
  right.

- **A jolt local could capture the ftype heads `jolt.ffi/layout` lowers to.**
  The layout lowering emits `define-ftype`, `make-ftype-pointer`,
  `ftype-sizeof`, `ftype-&ref`, and `ftype-pointer-address` into the scope a
  jolt local lives in, but none were in the back end's emitted-name set. A local
  named `ftype-pointer-address` answered its own value as the layout's
  `:alignment`; the other four failed to compile.
- **`make ffi` reported a green layout and aggregate gate over a red one.** Both
  wrappers run the Scheme-level C-ABI witness and then the public-API test
  without `set -e`, so only the second one's status survived — a failing ABI
  witness exited 0.

## [0.7.27] - 2026-08-25

`nth`'s three-argument form used to answer its not-found value for any receiver
that has no `nth` at all — a set, a map, a keyword, a number, a function — rather
than raising the way `RT.nth` does. That turned "you cannot index this" into
"there is nothing at index 0", silently, and the clearest casualty was
`(distinct #{1 2 3})`, which answered `(nil 3 2)`: `distinct` reads its head
through a `[[f :as xs]]` destructure, which lowers to `(nth coll 0 nil)`.

Note this is a behaviour change, not only a bug fix. Code that fed a map or a
set to `nth`'s not-found arity, or sequentially destructured one, used to get
nil and now raises — which is what the same code does on Clojure.

`distinct` itself comes out the other side accepting any seqable, and answering
a hash set roughly fourteen times faster than it did.

### Fixed

- **`nth`'s not-found arity raises on a type that has no `nth`.** The three-arity
  ended in a bare fallthrough that returned the not-found value for every
  receiver it did not recognize, so an unindexable type was indistinguishable
  from an absent index:

  ```clojure
  (nth #{1 2} 0 :nf)   ; was :nf   — now UnsupportedOperationException
  (nth {:a 1} 0 :nf)   ; was :nf   — now UnsupportedOperationException
  (nth :k 0 :nf)       ; was :nf   — now UnsupportedOperationException
  (let [[f] #{1 2}] f) ; was nil   — now UnsupportedOperationException
  ```

  A genuinely out-of-range index on a type that does have `nth` is untouched, so
  `(nth [1 2] 5 :nf)`, `(nth (range 5) 9 :nf)`, `(nth "ab" 9 :nf)` and
  `(nth nil 3 :nf)` all still answer `:nf`.

- **`nth` and `count` name the simple class when they refuse.** Both wrote the
  canonical name where `RT.nth` and `RT.count` use `getClass().getSimpleName()`,
  so a message read `nth not supported on this type: clojure.lang.Keyword`
  against Clojure's `… : Keyword`. A function class keeps its package-stripped
  form, `core$inc` rather than `clojure.core$inc`.

- **`(class (Object.))` reports `java.lang.Object`.** A bare `Object` fell off
  the end of the class-name chain and answered jolt's internal `:object` type
  keyword, which also appeared inside `count`'s refusal message for one.

### Changed

- **`distinct` accepts any seqable.** It takes its head off the seq it already
  holds rather than through `nth`, so a set, a map, or any other seqable works:

  ```clojure
  (distinct #{3 1 2})     ; => (1 3 2)
  (distinct {:a 1 :b 2})  ; => ([:a 1] [:b 2])
  ```

  This is a superset — Clojure raises on both, in every version back to 1.9 —
  and it is tracked in `test/conformance/known-divergences.edn` as `:permissive`.
  Nothing that runs on Clojure behaves differently here: for every collection
  `nth` accepts, the head it read and the head `seq` yields are the same value.
  The refusal is incidental rather than designed, in that Clojure's own
  `distinct` already accepts a set through its transducer and `sequence` arities
  and every sibling seq function accepts one.

### Performance

- **`distinct` over a hash set answers its seq.** A hash set holds no two
  elements that are `=`, so walking it against a `seen` set it can never hit is
  pure cost — 141.8ms against 10.5ms over 100k elements, where a bare `seq` is
  8.3ms. A sorted set is deliberately excluded: one built with a comparator that
  never reports `0` really does hold `=` duplicates, so it still takes the dedup
  walk. Vector and seq receivers are unmeasurably affected, paying one type test.

## [0.7.26] - 2026-08-25

Interruption, second half. 0.7.25 made a `java.lang.Thread` handle answer about
the thread it stands for, so an `.interrupt` from outside and that thread's own
view of its flag became one flag rather than two. This release gives that flag
the half the JVM has and jolt did not: an `.interrupt` now reaches a thread that
is already *parked*, not merely one that will look at the flag the next time it
asks. Every blocking operation jolt models that is interruptible on the JVM
throws `InterruptedException` and leaves the flag cleared, the way the JVM's do
— which is what makes the ordinary shutdown idiom, interrupt the worker and join
it, terminate rather than wait out a `promise` nobody is going to deliver.
`future-cancel` is `cancel(true)` for the same reason, so cancelling a future
now ends the work rather than only marking it.

The check that buys this runs on waits that never wait — a deref of a promise
already delivered — so the first cut's per-call allocation was measurable
against a release binary. The interrupt box is threaded through the wait loop
instead, and the settled path falls through it.

And one linking fix carried over from the same window: a built binary no
longer exports the Chez kernel's own ncurses, which was enough to stop any
FFI binding of a real ncurses from opening a terminal.

The rest of the window is a run of smaller fixes, most of them found by piping
a generated program into `jolt -`: `slurp` accepts the reader behind `*in*`,
and `refer`'s `:rename` is implemented rather than silently ignored. Separately,
a Maven artifact that packages resources and no Clojure source is now a leaf
rather than a nobody — its extraction stays on the roots so `io/resource` can
read what it ships, without its publisher's JVM dependency tree being walked.

### Fixed

- **A blocking wait is interrupted, not merely flagged.** `.interrupt` set the
  target's flag and stopped there, so a thread already parked ran its wait to
  completion and the JVM's second half of interruption — being thrown out of the
  wait — never happened. Code that shuts a worker down by interrupting it hung
  until whatever it was waiting for arrived, which for a `promise` nobody
  delivers is forever. These now throw
  `java.lang.InterruptedException` and leave the interrupted status **cleared**,
  each certified against JVM Clojure 1.12: `@a-promise` and `@a-future` and
  their timed `deref` forms, `await` and `await-for`, `Thread/sleep`,
  `TimeUnit.sleep`, `Thread.join`, `CountDownLatch.await`, an executor
  `Future.get`, `FutureTask.get`, `ArrayBlockingQueue` `take`/`put`,
  `ExecutorService.awaitTermination` and `Process.waitFor`. A flag that is
  already set makes the next one of them throw immediately, without waiting at
  all.

  The mechanism is one seam. Every thread and fiber wait in the runtime already
  funnels through `jolt-cv-wait`, whose decision is **retaken** on each wake
  rather than resumed into, so an interrupt check at the top of that decision
  re-runs for free on every wake and needs no second wait protocol. What had to
  be added is a way for the interrupter to wake a waiter sitting on a condition
  variable it has never heard of: a waiter registers its `(mutex . condition)`
  against its own interrupt box for as long as it is willing to wait, and
  `.interrupt` sets the flag and then pokes what is registered. The race closes
  because the registration and the check both happen with the waiter's mutex
  held and the interrupter must take that mutex to wake — so an interrupt cannot
  land in the gap between deciding to wait and actually parking.

  It is **opt-in**, and a wait that asks for no interrupt box behaves exactly as
  it did: the runtime's own plumbing waits through the same seam — the carrier
  idle wait, the load barrier, the main-thread queue pump, the tap queue, the
  channel internals — and interrupting any of those breaks the runtime rather
  than the caller's code. Two things that look adjacent are already right and
  were left alone: `monitor-enter` / `locking` is not interruptible on the JVM
  either (a thread blocked entering a monitor stays blocked and its flag stays
  *set*), and `ReentrantLock.lockInterruptibly` already threw.

  What stays divergent is recorded in `test/conformance/known-divergences.edn`,
  because jolt's interrupt identity is the thread's interrupt box and a fiber has
  no flag of its own. N fibers share a carrier, so an interrupt aimed at a
  carrier while several fibers are parked wakes all of them and one consumes it —
  one interrupt still produces exactly one `InterruptedException`, but which
  fiber gets it is not determined, where on the JVM a parked go block occupies no
  thread to interrupt at all. A fiber inside `Thread/sleep` sleeps its carrier
  and there is nothing to poke, so only the already-set half of the rule applies
  there; park on a promise or a channel instead if the wait must be
  interruptible mid-flight. And `PipedInputStream.read` is left out of the set
  because it signals with `InterruptedIOException`, a different contract from the
  ops this covers.

- **`future-cancel` interrupts the worker, and deref of a cancelled future
  raises `CancellationException`.** `clojure.core/future-cancel` is
  `cancel(true)` on the JVM: the worker gets a real `InterruptedException`.
  jolt only flipped the future's own flags, so a cancelled `(future
  (Thread/sleep 60000) ...)` kept its thread for the full minute. The worker now
  carries the future's interrupt box, so a cancel throws it out of any
  interruptible wait. Deref of a cancelled future raised a bare `ExceptionInfo`
  where the JVM raises `java.util.concurrent.CancellationException`, so
  `(catch java.util.concurrent.CancellationException _ ...)` around a deref did
  not catch; it now does.

  `jolt-future`'s record tag moves from `jolt-future-v1` to `jolt-future-v2`,
  because a `jolt-future` is image-format surface: `jolt.image` round-trips
  record values by nongenerative tag, and adding a field under the old tag would
  let a new image read as an old record.

- **The kernel's ncurses is no longer exported from the executable.** Chez's
  expression editor links ncurses and terminfo, and `-rdynamic` — which a built
  binary needs, so a statically linked native resolves through
  `(load-shared-object #f)` — put all of it in the dynamic symbol table: 77
  `_nc_*` internals plus `setupterm`, `tigetnum`, `raw`, `cbreak`, `curs_set`,
  `keypad`, `notimeout`, `wtimeout`, and globals including `cur_term`. The
  executable is searched before any `dlopen`'d library, so a program that bound
  a real ncurses through the FFI had that library's own internal calls bound
  back into the kernel's copy:

  ```
  binding file /lib/x86_64-linux-gnu/libncursesw.so.6 [0] to jolt [0]:
      normal symbol `_nc_setupterm' [NCURSES6_TINFO_5.5.20051010]
  ```

  The kernel's copy predates ncurses 6.1, so it reads a terminfo entry with a
  4096-byte buffer and cannot parse the 32-bit number format that any entry
  carrying a value above 32767 is stored in — `xterm-256color`, whose
  `max_pairs` is 65536, for one. `initscr` then failed with "Error opening
  terminal" on a terminal that works everywhere else; where the entry was in the
  legacy format the old code parsed it instead and filled `cur_term` with a
  pre-6.1 `TERMINAL` layout that the newer library then read with its own, which
  segfaults. Linking with `--exclude-libs` marks those archives' symbols local,
  which is enough: Chez calls them internally, and internal linkage does not go
  through the dynamic symbol table, so the expression editor is unaffected.

- **`slurp` reads the `IReader` behind `*in*`.** `*in*` is a reified `IReader`
  — `-read-line` / `-read-form`, with no host reader object underneath — so
  `slurp` fell past every arm of its cond and reported the value as unopenable:

  ```
  (with-in-str "one\ntwo" (slurp *in*))
  ;; 0.7.25: Cannot open <#object[clojure.lang.IObj$reify__0]> as a Reader.
  ;; now:    "one\ntwo"
  ```

  This is what `jolt -` needs to read a program off a pipe, so
  `ys --to=star file.ys | jolt -` works. The drain is line-based, because
  `-read-line` is the only read `IReader` offers and it drops the delimiter, so
  a trailing newline does not survive — `"a\nb"` and `"a\nb\n"` both drain to
  `"a\nb"`. Reading source text off a pipe does not care.

- **`refer`'s `:rename` installs the var under the name you asked for.** The
  option was parsed by nothing and the refer table keyed on the plain name, so
  the local name simply never existed:

  ```
  (refer 'clojure.string :only '[upper-case] :rename '{upper-case up})
  (up "ok")
  ;; 0.7.25: Unable to resolve symbol: up in this context
  ;; now:    "OK"
  ```

  The table now records `(target-ns . source-name)` against the local name, so
  everything that reads it — `resolve`, `ns-refers`, `ns-map`, syntax-quote
  resolution, the runtime macro lookup — reports the local name and reaches the
  var it was renamed from. `defmethod` is included: a `defmethod` on a renamed
  multifn extends the multifn it was renamed from, where 0.7.25 auto-created a
  dead shadow under the alias and printed the default form.

- **A dialect's own `require`/`use` macro owns its arguments.** The CLI
  auto-quotes the vector/list arguments of a top-level `require` or `use`, so
  `(require [my.lib :as m])` works from `-e` or a stdin program without an
  explicit quote. It decided by the head symbol's *spelling*, so a program that
  defined its own macro of that name — a dialect compiled to jolt, which is how
  `ys --to=star file.ys | jolt -` arrives — had its module spec rewritten to
  `(quote …)` before the macro ever saw it:

  ```
  (defmacro use [& specs] `(println (quote ~specs)))
  (use (demo :as d))
  ;; 0.7.25: ((quote (demo :as d)))
  ;; now:    ((demo :as d))
  ```

  The head is resolved the way the compiler resolves it — a locally defined var
  shadows, then a `:refer`, else the implicit `clojure.core` one — and the
  convenience applies only when that lands on `clojure.core/require` or
  `clojure.core/use`. So an unshadowed `(use [clojure.set :only [union]])` keeps
  its auto-quote, and a qualified `(clojure.core/use …)` or a `:rename`d core
  `require` reaches it too.

- **A Maven JAR carrying only resources stays on the roots.** An artifact with
  no `.clj`/`.cljc` was dropped from the resolution outright, on the reasoning
  that nothing in it can be required. But a jar can package RESOURCES and
  nothing else: `com.cognitect.aws/endpoints` is a jar of endpoint data and the
  aws api client reads it through `io/resource`, which could not find it once
  the root was gone. The extraction is kept.

  What such an artifact still does not get is a **walk**. With no source of ours
  to load, the deps it declares are its publisher's own JVM/cljs toolchain and
  jolt has no JVM to run them on — a wrapper around a large Java SDK would
  otherwise pull that SDK's whole transitive tree, and since a dep that cannot
  be *obtained* is fatal, a subtree that used to be pruned could abort a
  resolution over an artifact jolt has no use for. So it resolves as
  `{:root root :manifest :mvn}` with no `:deps` and no `:pom`, which is a leaf:
  on the roots for its resources, and childless.

- **A Linux build links Chez's own `liblz4`/`libz`.** The link line asked for
  `-llz4 -lz` with no `-L`, so it resolved them only on a machine that had lz4
  and zlib development packages installed. Chez ships both archives in the same
  directory the kernel comes from (`libkernel.a`, `petite.boot`), and the link
  now names that directory — the target pack's for a cross build, not this
  host's, so a cross link cannot pick up the wrong copies.

### Performance

- **The interrupt check is paid by the waits that wait, not by every call.**
  The check above runs on every interruptible wait, including the ones that
  never park: a deref of an already-delivered promise or a settled future
  decides on its first pass and returns. The first cut wrapped the caller's
  decision in a fresh closure and consed a registration entry before deciding,
  so that path allocated twice per call and measured 1.15×–1.18× against the
  same operations before any of this landed.

  The interrupt box is threaded through the wait loop instead:
  `jolt-cv-wait/ibox` is the old loop with one more argument, and the settled
  path is now an `(if ibox ...)` that falls through. `jolt-cv-wait` passes `#f`
  and is exactly as uninterruptible as it was, so the plumbing that must never
  be interrupted is untouched. Release binary, A/B/A in one sitting, ms per 500k
  ops, medians of 5:

  | | before | first cut | now |
  |---|---:|---:|---:|
  | `@delivered-promise` | 27.4 | 32.3 | 30.3 |
  | `@settled-future` | 29.0 | 33.3 | 31.3 |
  | `ArrayBlockingQueue` put+take | 175.0 | 185.8 | 182.9 |

  That is 1.11× / 1.08× / 1.05× over the pre-interrupt figures, down from
  1.18× / 1.15× / 1.06×. It is at the ceiling rather than comfortably under it —
  a second sitting put the two derefs at 1.08× and 1.09× — and 1.04× is the
  measured floor for reading a flag before deciding at all, so what remains is
  close to the cost of the feature itself. The queue column swings ~5% run to
  run.

  `current-interrupt-box` also moves from a thread parameter to a virtual
  register. It had stored `(thread-id . box)` and compared the running thread's
  id on every read, for one reason: a Chez thread parameter is inherited by a
  forked thread, so a plain one would hand a child its parent's box. A virtual
  register starts at fixnum 0 in a fresh thread, which is exactly the property
  that workaround was buying, so the comparison is deleted rather than
  optimized. `.isInterrupted`, `Thread/interrupted`, `monitor-enter`/`exit` and
  `ReentrantLock`'s owner check all read it too.

### Internal

- **Gist/GitHub dependency acquisition reaches the filesystem directly.** Its
  host map was built by requiring `jolt.fs` at runtime and `resolve`-ing three
  vars out of it; it now goes through the same `jolt.host` helpers the rest of
  `jolt.deps` already uses, so there is no runtime require left. A cache move
  that fails raises `Unable to move dependency cache file to …` rather than
  returning nil and leaving the caller to discover the file is not there.

- **`make realclean` clears the `.jolt` caches.** The root and test-fixture
  `.jolt` cache directories are in the `realclean` set now; plain `clean` stays
  limited to build artifacts.

## [0.7.25] - 2026-08-24

Three boundaries where an absent thing was quietly turned into a present one.
A `nil` resource name became the classpath root, so a missing config key
answered with a directory. A `nil` or `false` crossing the FFI's `:string`
boundary became `""` or NULL, so C acted on a value nobody passed. And
`setlocale`'s NULL failure answer arrived as the address 0 — truthy in Scheme —
so every failed locale lookup read as a success.

The rest is reach on the same seams. `clojure.java.io/resource` takes the
ClassLoader argument libraries pass it, and the loader's resource methods
resolve through the same resolver `io/resource` does — which is what makes a
resource baked into a `jolt build` binary visible to a library that reaches the
classpath through `RT/baseLoader`. A `require` fired under someone else's
`(binding [*ns* ..])` interns each loaded file's defs into that file's own
namespace instead of the bound one. And a nested `run-interruptible` extent
leaves the enclosing one still watching its token.

The last one came out of reviewing the others: a `java.lang.Thread` handle now
answers about the thread it stands for rather than the thread doing the asking,
which is what makes `.interrupt` from outside a thread and that thread's own
view of its flag one flag instead of two.

Thanks to @casselc for the interrupt fix and the compatibility surface SCI
reaches for.

### Added

- **The `clojure.lang.Numbers` statics a hosted analyzer emits.** SCI's
  optimized analyzer emits direct `Numbers` calls rather than core var
  references, so 19 of them now route to jolt's existing numeric tower:
  `inc`/`dec` and their `unchecked_` forms, `isZero`/`isPos`/`isNeg`,
  `add`/`minus`/`multiply` checked and unchecked, `remainder`, the four
  comparisons, and `equiv`. Only the ones it emits — this is not a
  claim of complete `Numbers` parity.

- **`Thread.getId`**, answering jolt's stable numeric identity for the running
  thread: repeatable for one thread, distinct across simultaneously live ones.
  It does not claim JVM `Thread` semantics beyond jolt's thread model.

- **`clojure.core/imap-cons` and `clojure.core/system-newline`.** Both are
  private on the JVM, and both are reached by ordinary hosted library code —
  `imap-cons` is the map arm of `conj`, `system-newline` the newline `println`
  writes. jolt's `system-newline` is `"\n"`, its portable convention, on every
  platform.

### Fixed

- **`clojure.java.io/resource` accepts a ClassLoader.** `(io/resource n
  loader)` raised `Wrong number of args (2) passed to:
  clojure.java.io/resource` — jolt only defined the 1-arity. Libraries pass a
  loader at namespace load to pin resource resolution across threads, so the
  missing arity was a load failure rather than a degraded call: cognitect
  aws-api's `cognitect.aws.resources/resource` is `(io/resource n
  (RT/baseLoader))`, which made every `cognitect.aws.*` namespace unloadable.
  jolt has a single classloader, so the argument is accepted and ignored, the
  way `.setDaemon` is on the Thread shim. Three arguments is an arity error
  here as on the JVM; the rest argument was quietly accepting it.

- **Every ClassLoader resource method resolves through `io/resource`.**
  `getResource`, `getResources`, `getResourceAsStream` and the two `Class`
  arms walked the source roots with their own copy of the resolver, which was
  missing two things the real one has. It had no embedded-resources branch, so
  in a `jolt build` binary with `:jolt/build :embed` a baked-in resource
  answered `nil` through `RT/baseLoader` while `(io/resource n)` served it —
  libraries that reach the classpath through a loader therefore found nothing
  in the built artifact and everything in the source tree they were developed
  against. And it never announced its lookup to the AOT cache, so an added
  resource kept serving the "not there" answer, the staleness #576 fixed for
  `io/resource` still live on this path. `getResourceAsStream` now dispatches
  `openStream` on what the resolver returned rather than stripping the scheme
  and slurping a path, which is what makes an embedded hit readable at all.

- **A `nil` resource name is a `NullPointerException`, not the classpath
  root.** The name went through the `str` coercion, which renders `nil` as
  `""` — and `""` is a name with a real answer, since the empty name *is* the
  classpath root. So `(io/resource nil)`, and `.getResource` / `.getResources`
  / `.getResourceAsStream` on `RT/baseLoader`, and `Class.getResource`, handed
  back a URL for the first source root. A name that came from a missing config
  key or an absent optional path became a directory, and the caller only found
  out somewhere far away when something tried to read it. All four throw on
  the JVM, probed directly; `""` keeps answering the root on both, so only
  `nil` changes.

- **`false` is not a `jolt.ffi` `:string`.** 0.7.24 taught the `:string`
  boundary to carry NULL as `nil`. Chez's own `string` foreign type spells NULL
  as `#f`, and the boundary passed anything non-nil straight through, so jolt's
  `false` went out as NULL too — an undocumented second spelling, and the one
  that arrives by accident: a `when` that did not fire, a predicate result, a
  `boolean` of a missing key. `string` is the only foreign type that takes a
  non-string quietly, and what falls through that gap lands on precisely the
  value C reads as "absent". Worse in the callable return direction, where the
  callback hands C a null `char*` and nothing in jolt ever said so. The
  boundary now validates, which also upgrades the message for the non-strings
  that were already rejected:

      before   :object | invalid foreign-procedure argument #[keyword-v1 ...]
      after    IllegalArgumentException | jolt.ffi: :string got :kw — NULL is spelled nil

- **`nil` round-trips through `ffi/string->ptr` and `ffi/ptr->string` as
  NULL.** `ptr->string` has always read NULL back as `nil`, but `string->ptr`
  went out through the `str` coercion and rendered `nil` as `""`, so the pair
  lost the distinction in one direction — an absent string and a present empty
  one were the same thing after a round trip, which for a path, a name or an
  optional argument is the difference C cares about. `nil` now answers NULL and
  allocates nothing; `""` still allocates its NUL byte and still reads back as
  `""`. `with-c-string` binds NULL for a `nil` value, which is how an optional
  C string argument is passed, and `with-c-string-array` writes NULL into the
  slot — argv/envp shape. Every other value keeps the coercion, so `42` is
  still `"42"`.

- **A `nil` that cannot mean NULL is an error, not an empty string.** Three
  more sites took the `str` coercion where the string *names* something, so an
  absent name silently became an empty one and the boundary acted on it:
  `ffi/write-bytes` wrote 0 octets and answered 0, indistinguishable from
  writing `""` on purpose; `ffi/read`, `ffi/write` and `ffi/sizeof` resolved a
  `nil` type and raised `unknown foreign type :`, loud but naming nothing; and
  `ffi/loaded?` `dlopen`'d `""` and answered `false`, so a nonsense query got a
  definite-looking no. All three reject `nil` now. `(load-library nil)` is
  unchanged — it is documented to mean "no name at all, the process's own
  symbols are already resolvable", which is an answer rather than a missing
  one.

- **The libc locale probes no longer clobber `LC_TIME` process-wide.**
  `tzp-locale-available?`, the boot capability probe, set `en_US.UTF-8` and
  left it — and it runs unconditionally, so *every* jolt process ran in
  `en_US.UTF-8` where a C program starts in `"C"`, and any later `strftime`,
  `localtime` or `nl_langinfo`, jolt's own or a user's through `jolt.ffi`,
  answered from a locale nobody selected. An embedder that had chosen its own
  locale before calling in lost it. `tzp-locale-name` restored to a hardcoded
  `"C"` rather than to the value it displaced, which is its own clobber; it
  just looks tidy. Both now save with `setlocale(cat, NULL)` and restore under
  `dynamic-wind`, so a throw between the two cannot leave the category changed
  either — the shape the TZ probe already had.

- **A failed `setlocale` reads as a failure.** Its result was read as `void*`,
  so the NULL it answers for a locale the OS does not have installed arrived as
  the address 0 — truthy in Scheme — and every failure read as a success. That
  is the common case, not a rare path: most images carry only `C` and
  `en_US`. `jolt.host/locale-name` then ran `strftime` under whatever locale
  was current and returned the answer as if it were the requested locale's, so
  `"fr"`, `"ja"`, `"de"` and `"ru"` all answered `January` on a stock box — and
  genuinely the wrong language wherever some other locale was current. It
  answers `nil` now, which is what the caller wants: `jolt.time.fmt` falls
  through to its bundled tables, which are right everywhere.

- **A nested `run-interruptible` leaves the enclosing extent watching.** Each
  extent installs a polling timer handler and restores the previous one on the
  way out, but the restore did not re-arm the timer — so off a fiber, once an
  inner extent had returned, the outer one never polled its token again and an
  interruption after that point was simply never noticed. Active extents are
  now an owner-tagged per-thread stack, tagged because Chez child threads
  inherit thread-parameter values and an inherited stack must not let a child
  poll its parent's token. Covered by a new `interruptnest` gate: normal inner
  return, inner exception, inner interruption, outer interruption after the
  inner exit, concurrent tokens, and child-thread ownership.

- **A `java.lang.Thread` handle answers about the thread it stands for.** Three
  questions asked through one answered about the caller instead. `.getId` read
  the *asking* thread's id, so every handle `Thread/getAllStackTraces` hands
  back — and they are all handles for other threads — reported the caller's id:
  four live threads, four entries, one id. `.getName` was the constant
  `"main"` for every thread. And a thread jolt had forked allocated its own
  interrupt flag on first use rather than adopting the one its `Thread` object
  hands `.interrupt`, so the two were unrelated boxes and the ordinary
  interruption idiom never reached the worker:

      (let [t (Thread. body)] (.start t) ... (.interrupt t))
      before   the caller sees true, the worker polling .isInterrupted sees false
      after    one flag

  A handle now carries the id of the thread it stands for, names are kept in an
  id-keyed table — a thread nobody named answers the JVM's shape, `"main"` for
  the boot thread and `"Thread-<id>"` otherwise — and a forked thread adopts its
  `Thread` object's flag before running the body. `Thread(runnable, name)` also
  accepted the name and dropped it, and a constructed `Thread` had neither
  `.getName` nor `.setName`; all three work, and a rename after `.start` reaches
  the running thread. What has not changed is that no jolt park is interruptible:
  a thread already blocked in `CountDownLatch.await`, `Thread/sleep` or
  `.join` is not thrown out of it with `InterruptedException` the way the JVM
  does, so interruption on jolt is the polling idiom. That half is tracked and
  now written down in `known-divergences.edn`.

- **A `require` under a `(binding [*ns* ..])` interns each file's defs into its
  own namespace.** `*ns*` reads prefer a live thread binding over the
  thread-local current-ns parameter, but the write half only ever set the
  parameter — so the loader's restore of the current namespace on the way out
  of a load did nothing under such a binding, and `*ns*` stayed pointing at the
  last file the require finished. Every def after the `:require` in the
  requiring file then interned into the wrong namespace, and nothing said so
  until a call somewhere else could not resolve. Macroexpansion is where this
  is reached in practice — typedclojure's `ann*` expands under exactly such a
  binding, and its whole dependency tree loaded wrong. The write now targets
  whatever the read would consult, which is also what `(set! *ns* ...)` does on
  the JVM: `Var.set` writes the innermost thread binding and leaves the root
  alone, so the binding frame popping still restores the namespace that was
  current before it.

## [0.7.24] - 2026-08-24

NULL now round-trips through a `:string` FFI position in both directions —
`foreign-callable` joins `foreign-fn`, so a nullable C string argument or
return value has a home on either side of the boundary.

The other half (#716, thanks to @burinc) is a fix for a performance
regression: restoring `TZ` after every timezone probe call — itself a real
leak fix in the previous release — meant libc reloaded zone data twice per
call. Memoizing the probe by `(zone, instant)` undoes it: a repeated
`jolt.host/tz-offset-seconds` lookup drops from ~1.7ms to ~100ns, and
`jolt-lang/time`'s zone-aware `now` speeds up by roughly 100x.

### Fixed

- **`jolt.ffi` `:string` carries NULL in both directions.** Chez's `string`
  foreign type already spells NULL as `#f`, but jolt's own nil is a separate
  sentinel, so the boundary leaked Scheme: passing `nil` to a `:string`
  parameter raised `invalid foreign-procedure argument #[jolt-nil-v1]`, and a C
  function returning NULL answered `false` rather than `nil`. Both directions
  now translate, so `(c-setlocale 0 nil)` queries the locale the way the C API
  intends and `(c-getenv "UNSET")` reads as `nil`.

  This matters for the large amount of C where NULL is a real argument rather
  than an error — `setlocale` queries with it, raylib's `rlLoadShaderCode` takes
  it to mean "use the default vertex shader". Binding such a parameter as
  `:pointer` to get `ffi/null` through worked, but gave up string marshaling on
  that argument and left two parameters of the same C type carrying different
  jolt types for no visible reason. `ffi/null` is unchanged and remains the
  `:pointer` spelling of NULL.

  The same translation now covers `foreign-callable`, where C is the caller and
  so the two directions swap: a null `char*` C passes into a `:string` argument
  arrives as `nil` instead of `false`, and a callback returning `nil` from a
  `:string` hands C a null `char*` instead of raising `invalid return value`.
  Until this a callback could neither model a nullable string argument nor
  decline to answer one, which is ordinary in any C API that hands a callback an
  optional path, name or error.

### Performance

- **The libc timezone probe is memoized.** Restoring `TZ` after every lookup
  fixed a real leak, but it meant libc reloaded zone data twice on every
  call instead of reusing what the leaked `TZ` had already loaded, making
  `jolt.host/tz-offset-seconds` on a repeated `(zone, instant)` pair about
  1000x slower than the leaky version it replaced. The offset of a zone at
  an instant is a pure function of the two, so it is now cached on that
  pair: a repeated lookup answers in low microseconds instead of over a
  millisecond, and `jolt-lang/time`'s `ZonedDateTime/now` is roughly 100x
  faster for it. A live instant, whose epoch is new on every call, still
  pays the full probe.

## [0.7.23] - 2026-08-22

Interop reach. The FFI can describe C structs instead of counting byte offsets:
`ffi/layout` derives size, alignment and field offsets from a literal
descriptor, `[:by-value ...]` passes and returns structs by value, and the
`with-alloc` family scopes foreign allocations so they are released on every
path out. On the Java side, `java.net` answers host identity —
`InetAddress/getLocalHost` and a real `NetworkInterface` — and loads on demand;
`java.util.Properties` is modelled, so `System/getProperties` answers
`.getProperty` rather than reading as a plain map.

The rest is conformance: static fields resolve through every access path, the
reducible family dispatches to `IReduce`/`IReduceInit`/`IKVReduce`, protocol
dispatch reaches a deftype or reify through its declared interfaces, `#=`
read-eval works, and the vector spelling of a require prefix list expands.
Between them these let clj-uuid, mulog and the malli time suites run.

### Added

- **`java.net` host identity.** `InetAddress/getLocalHost`, `getAllByName`
  (an array, as on the JVM), `.getCanonicalHostName`, `.getAddress`, and a new
  `java.net.NetworkInterface` — `getNetworkInterfaces`, `getByName`,
  `getByInetAddress`, `.getInetAddresses`, `.getHardwareAddress`,
  `.getDisplayName` — read from `getifaddrs(3)`. IPv4, matching the rest of
  `jolt.socket`. Touching any `java.net` class now loads `jolt.socket` on
  demand, the courtesy `java.time` and `MessageDigest` already got, so a JVM
  library that reaches for `InetAddress` works with nothing to require.

- **`java.util.Properties`**, with the `defaults` chain the JVM's has, and the
  same split over which operations span it: `getProperty`, `propertyNames` and
  `stringPropertyNames` do, the inherited `Hashtable` surface (`get`,
  `containsKey`, `keySet`, `size`) does not. `System/getProperties` returns one
  whose entries are the computed values, recomputed per call so `user.dir` and
  `java.class.path` stay current, and writes through it reach the store
  `System/getProperty` reads. It used to return a plain map, which answered
  `.getProperty` as a key lookup and so read `nil` for every system property.

- **`java.nio.charset.StandardCharsets` constants.** The constants are the
  charset name strings, which every jolt charset seam takes, so they compose
  with `.getBytes`, `String` ctors and stream readers alike.

- **`X/from` for the core `java.time` value types.** `LocalDate/from`,
  `LocalTime/from` and `LocalDateTime/from` answer the JVM TemporalAccessor
  query over the core value types, throwing `DateTimeException` when the
  fields aren't there; the jolt-lang/time library re-registers them with the
  zoned/offset types added.

- **`#=` read-eval.** The clojure reader's EvalReader: `#=form` evaluates its
  form at read time, refused under `(binding [*read-eval* false] …)` with the
  JVM's message. EDN still has no `=` dispatch. clj-uuid computes its bit
  masks with `#=` and now reads.

- **`java.util.concurrent` construction.** `ArrayBlockingQueue` is a real
  bounded blocking queue (offer/put/take/poll and the timed variants,
  fiber-aware), `FutureTask` a run-once task with a blocking get, and the
  `ThreadPoolExecutor` constructor builds on the existing executor machinery —
  sized by maximumPoolSize, with `.getQueue` answering a live view of the
  internal queue's depth. One documented divergence: the JVM rejects a submit
  when its bounded queue fills; jolt's internal queue is unbounded and accepts
  it.

- **The `java.util.UUID` instance surface.** `.getMostSignificantBits` /
  `.getLeastSignificantBits` (signed longs), `.version`, `.variant`,
  `.timestamp` / `.clockSequence` / `.node` (v1-only, like the JVM),
  `.compareTo` (signed msb-then-lsb — a high-bit uuid sorts first), and a
  JVM-exact `.hashCode`. Uuids are Comparable, so `sort`, `compare` and sorted
  collections accept them. `UUID/fromString` and the `(UUID. s)` ctor now
  implement the JVM's lenient 5-component parse — short groups zero-pad,
  `(UUID/fromString "1-1-1-1-1")` is legal — and throw
  `IllegalArgumentException` on anything else instead of returning nil
  (`parse-uuid` keeps its nil-returning Clojure contract).

- **Lexically scoped `jolt.ffi` allocation helpers:** `with-alloc`, `with-out`,
  `with-layout`, `with-c-string`, and `with-c-string-array` release helper-owned
  native allocations exactly once on normal return or exception. Partially
  constructed C string arrays are cleaned up safely. Pointers created by these
  helpers are valid only within the lexical body and must not escape it.

- **Declarative `jolt.ffi` struct layouts:** `(ffi/layout [:struct ...])`
  compiles a literal, data-only descriptor into immutable ABI metadata derived
  by Chez. `layout-size`, `layout-alignment`, and `field-offset` expose the
  native layout, while `read-field` and `write-field` access scalar fields by
  keyword path. Layouts support fixed-size scalar fields and nested structs;
  arrays, unions, bitfields, packing, and recursive descriptors are not yet
  supported.

- **Structs passed and returned by C value:** `foreign-fn` and `defcfn` accept
  `[:by-value [:struct ...]]` signature types. Arguments are non-null pointers
  to caller-owned struct storage. Aggregate-returning callables take a non-null
  caller-owned destination pointer first, write the returned C value there, and
  return that pointer. Nested structs, multiple aggregate arguments, fixed
  aggregates before `:varargs`, and `:blocking` calls are supported; aggregate
  callbacks, variadic aggregate arguments, and aggregate returns combined with
  `:varargs` remain unsupported.

### Fixed

- **Static fields resolve through every access path.** Three related bugs in
  the shared field/method registry. A field holding a falsy value —
  `Boolean/FALSE` is registered as `#f` — read as absent, so every spelling of
  it threw "No matching field or method"; the registry now detects a miss with
  a sentinel instead of the value's own truthiness. `(. Token -FIELD)` reached
  through a Class-valued local or a cold import looked up the literal `-FIELD`;
  the class-token arm now strips the field spelling. And a field VALUE reached
  through that arm or `host-static-call` was applied as a procedure, surfacing
  Chez's bare "attempt to apply non-procedure" with no message; both now follow
  `static-member`'s rule — a procedure is a method to call, anything else
  answers a zero-argument access and reports itself if given arguments. The
  visible consequence: `malli.experimental.time` loads cold with only the time
  library on deps — its `(. LocalDate -MIN)` reads through the imported simple
  name used to throw unless something had already touched `java.time` by the
  slash spelling first.

- **The reducible family dispatches to `IReduce`/`IReduceInit`/`IKVReduce`
  like the JVM.** 3-arity `reduce` already drove a deftype/reify's own `reduce`
  method; now the rest of the family does too: 2-arity `reduce` (scoped to
  values that DECLARE `clojure.lang.IReduce`, mirroring `instanceof` — an
  `IReduceInit`-only source such as an eduction still seqs), `into` in both
  arities, and `vec`. `reduce-kv` consults a declared `clojure.lang.IKVReduce`
  (`kvreduce`, lowercase) before throwing. `sequence` still requires a seqable
  source, as on the JVM.

- **Protocol dispatch reaches a deftype or reify through its declared
  interfaces.** `value-host-tags` now reports a reify's declared interfaces
  (with their modeled ancestry) and a bare deftype's declared interfaces from
  the class graph, so an `extend-protocol` filed under an interface name —
  `clojure.lang.IReduceInit`, `java.lang.Iterable`, an ancestor like
  `Associative` for a type declaring `IPersistentVector` — dispatches on such
  a value the way `instanceof` answers on the JVM. A defrecord's EXTRA declared
  interfaces join its automatic map set the same way. `satisfies?` agrees: it
  falls through to the same interface walk when the type's own registry has no
  entry (it used to answer false while dispatch succeeded). An extension on
  the type's own tag still wins, and `Object` extensions still catch types
  declaring nothing.

- **A deftype/reify can name a protocol by its dotted class spelling.**
  `(deftype T [] my.ns.PThing (m [_] …))` resolves `my.ns.PThing` to the
  protocol like the JVM does; the methods used to file as interface methods
  and protocol dispatch answered "No method …" (found running mulog).

- **The vector spelling of a require prefix list expands.**
  `[clj-uuid [bitmop :as bitmop] [clock :as clock]]` is a prefix list on the
  JVM; the old single-libspec read required the PREFIX itself — a silent
  self-require mid-load with every sub-spec and alias dropped. The expansion
  is shared by the loader, the build driver, and the compile-env alias
  pre-scan, in both the list and vector spellings.

- **The `java.util` map shims answer `count`, `seq` and `get` over one key
  set.** The three spellings of "is this a hashmap-backed host object" had
  drifted into separate predicates; they are one now, so a shim of that shape
  cannot be countable through one and opaque through another.

## [0.7.22] - 2026-08-22

A patch release of robustness fixes: the AOT cache detects and heals partial
artifacts instead of serving them, `System/gc` never spuriously throws, and
the `var` special form reports real errors.

### Fixed

- **The AOT cache detects a fasl cut at a form boundary.** `load` of a cached
  `.so` truncated exactly at a compiled-form boundary succeeds and silently
  runs only a prefix of the namespace, so a damaged artifact could be served on
  every run — with missing or misplaced defs — while a plain source load
  worked, until the cache directory was deleted by hand. Every published
  artifact now ends with a completion marker; a cache hit that loads without
  reaching it is treated like a corrupt fasl: the artifact is dropped and the
  namespace recompiles from source. Existing caches migrate themselves (one
  recompile per artifact on first use).

- **`System/gc` never throws.** `jolt.host/gc-full!` (behind `System/gc` and
  `Runtime.gc`) called Chez's `collect` directly, which raises when any other
  thread is active at that instant — and jolt always has service threads (the
  io-poller, fiber carriers, the timer) that are active for the microseconds
  between their blocking waits, so an explicit GC could spuriously throw
  "cannot collect when multiple threads are active". The collect now retries
  over a short window and degrades to a no-op if a thread stays active
  throughout, matching the JVM contract that `System.gc` is a hint. (#696)

- **`(var ...)` error messages.** A non-symbol argument — `(var 5)`,
  `(var)` — used to surface an opaque `#object[:object]` raise; it is now a
  clear analysis error. An unresolvable symbol reports the reference wording,
  `Unable to resolve var: nosuchns/foo in this context`, keeping the full
  namespaced symbol (the namespace part used to be dropped). `(var String)`
  still succeeds and returns the class-holding var — jolt keeps imported
  classes in vars, a deliberate superset of the JVM, which refuses.
## [0.7.21] - 2026-08-22

A patch release that gives core vars their documentation: `(meta #'map)` used
to come back `{:ns clojure.core, :name map}` — no arglists, no docstring — so
editor hover and signature help on any core name had nothing to show, and
`(doc ...)` did not exist. Now every var in the image-baked namespaces carries
`:doc` and `:arglists`, clojure.repl grew the fns that read them, and jolt.ffi
gained the exact-width scalar types (#692).

### Added

- **Exact-width `jolt.ffi` scalar types:** `:int8`/`:i8`,
  `:int16`/`:short`, `:uint16`/`:ushort`, `:int32`, and `:uint32`. Each type
  works in native-memory access (`sizeof`/`read`/`write`) and typed native
  signatures (`foreign-fn`/`defcfn`/`foreign-callable`), with signed and
  unsigned boundaries preserved. The existing `:uint8`/`:u8`/`:byte` aliases
  remain unsigned C octets. Exact-width values use native byte order; wire byte
  order remains explicit through a codec or conversions such as
  `htons`/`ntohs`. C default argument promotions still apply after `:varargs`,
  so narrow integers must be declared as `:int` there (and `float` as
  `:double`). Contributed by @casselc in #692.

- **Core vars carry `:doc` and `:arglists`.** The image-baked namespaces
  (`clojure.core`, `clojure.string`, `clojure.walk`, `clojure.set`,
  `clojure.edn`, `clojure.pprint`, `clojure.repl`, `clojure.template`) are
  finished off by a generated shard that fills reference docstrings and
  arglists from Clojure 1.12.5's own metadata (EPL-1.0, (c) Rich Hickey and
  contributors) — 857 vars. Native primitives (`map`, `reduce`, `conj`, `+`,
  ...) had no metadata at all and now report their full arglists; a `:doc` or
  `:arglists` the jolt source itself declares always wins. Regenerate with
  `tools/gen-core-docs.sh` after adding core vars.

- **`clojure.repl/doc`, `find-doc`, `apropos`, and `dir`.** Ported from
  Clojure 1.12 now that var metadata makes them meaningful: `(doc map)`
  prints the arglists and docstring, `(doc when)` says Macro, `(doc if)`
  documents the special form. `source` and `pst` remain unprovided —
  image-baked vars carry no `:file` to point at.

### Fixed

- **Image-baked defmacros no longer lose their metadata.** A `defmacro`
  compiled at runtime kept its derived `:arglists`/`:doc` on the var, but the
  seed mint and `jolt build` lowered macros through a path that dropped the
  metadata wholesale — which is why every core macro (`when`, `cond`, `defn`,
  `->`) answered `(meta #'when)` with just `{:ns :name :macro true}`. The
  image path now derives the same metadata the runtime path does and emits it
  with the expander. App macros in built binaries keep theirs too.

- **`make gambitseed` mints again.** The chez-only-emission safety check
  refused any `(foreign-procedure` occurrence, including the ones inside the
  compiler's OWN emitter strings (data, not code) that the scoped-FFI change
  introduced — the gambit seed had been unmintable since then. The check now
  classifies occurrences with a string-literal scanner and fails only on real
  op uses.

## [0.7.20] - 2026-08-21

A patch release about where jolt runs and what it links against: Nix flake
support lands (thanks to @jasalt, #694), `JOLT_OPENSSL_LIBDIR` reaches an
OpenSSL outside the built-in search paths, and `File.getCanonicalPath`
resolves symlinks — which turns the containment check every static file
server writes from decorative into real.

### Added

- **Nix flake** (`x86_64-linux` and `aarch64-darwin`): `nix build`, `nix run`,
  and a reproducible `nix develop` shell with the pinned toolchain. The
  packaged binary is wrapped with Git, unzip, OpenSSL, and a CA bundle from
  the Nix closure, so `deps.edn` resolution — Maven HTTPS fetches and
  SHA-pinned Git deps — works in a scrubbed environment with nothing
  installed on the host. Contributed by @jasalt in #694. A new `flake`
  workflow builds and smoke-tests it in CI on both platforms.

- **`JOLT_OPENSSL_LIBDIR` overrides the OpenSSL search.** A directory named
  by the variable is tried before the platform candidates when the Maven
  HTTPS transport loads OpenSSL, so a libcrypto outside the built-in paths —
  Nix, MacPorts, Guix, a nonstandard Homebrew prefix — is reachable without
  loader-path tricks (which cannot work on macOS anyway, where the candidates
  are absolute Homebrew paths). Read at fetch time, so baked app binaries and
  restored images honor the environment they run in.

### Fixed

- **`File.getCanonicalPath` resolves symlinks.** It answered with the
  absolute path — never resolving a symlink, a `.`, or a `..` — and the
  difference is load-bearing: the containment check every Java program
  writes,

  ```clojure
  (.startsWith (.getCanonicalPath child) (.getCanonicalPath root))
  ```

  passed for a symlink inside `root` pointing anywhere on the filesystem, so
  a static file server built on it (`ring.middleware.file` is one) served
  whatever the link named. It is `realpath(3)` now, with nonexistent tails
  canonicalized rather than thrown on — the JVM's behavior — and
  `java.nio`'s `toRealPath` delegates to the same binding. (#693)

- **`jolt.ffi/load-library` raises again when a named load fails.** Since
  the scoped-native rewrite in 0.7.10 it dropped the dlopen result and
  returned `nil` either way, turning every candidate-list fallback into dead
  code: `jolt.mvn-http` accepted its first candidate loaded-or-not, and
  http-client's TLS probe could never see a failure. A failed load raises
  `ex-info` naming the path, matching the pre-0.7.10 contract.

## [0.7.19] - 2026-08-20

A patch release: the `java.io.InputStream` surface — `readNBytes`,
`transferTo`, and a `mark`/`reset` that actually marks — which is the half of
#681 that missed the 0.7.18 tag, plus review follow-ups to it. With this,
jolt-lang/http-client stops shimming `java.io.ByteArrayInputStream`
process-wide (~3300x faster stream drains for every app that requires it).

### Added

- **`InputStream` gained `readNBytes` and `transferTo`, and `mark`/`reset`
  now mark.** `readNBytes` (both arities) and `transferTo` were absent, and
  `markSupported` answered `false` for every stream while `reset` silently
  seeked to 0 — so a caller that marked mid-stream and reset believed it had
  gone back to the mark and was actually at the start. `markSupported` is now
  `true` for `ByteArrayInputStream` and `false` for `FileInputStream` (the JVM's
  answers), `mark` records the position, `reset` returns to it, and `reset` on a
  stream that does not support marking throws `IOException` rather than
  pretending. Every value checked against JVM Clojure, edges included.

- **`BufferedInputStream` provides `mark`/`reset` where the port can seek.**
  On the JVM, wrapping any stream in `BufferedInputStream` makes
  `markSupported` true. jolt now honours that over a seekable source (a file
  or byte-array stream); over a socket, pipe, or stdin — where the JVM would
  replay from its buffer — the wrapper stays the wrapped stream and answers
  `false` honestly (documented in known-divergences.edn, with `System/in`
  called out: `true` on the JVM, `false` here).

### Fixed

- **The `mark` surface crashed on `System/in`.** `#690`'s mark state was added
  to streams built through the constructors but not to the `System/in`
  singleton, so `(.markSupported System/in)` (and `.mark`/`.reset`) raised a
  raw host error instead of answering. It answers `false` now, `.mark` is
  accepted and ignored, and `.reset` throws `IOException`.

## [0.7.18] - 2026-08-20

A patch release. The headline is the `--opt` fix for issue #682 — the inliner
unrolled mutually recursive fn clusters exponentially, which cost a one-route
Ruuter server ~76 MB of idle RSS — plus the tail of the bit-op/numeric-hint
performance and conformance work, and the bulk FFI byte moves.

### Added

- **`jolt.ffi/read-into!`** — read a foreign buffer into a slice of an
  existing byte-array (`(read-into! ptr arr off n)`, the
  `InputStream.read(b, off, len)` argument order). A caller reading a stream
  whose total length it already knows now fills one buffer instead of
  allocating and regrowing an accumulator per chunk; a read outside the
  array's bounds throws rather than truncating. `write-array` gained the
  matching slice arity `(write-array ptr arr off n)`.

- **`ProcessBuilder.inheritIO()` and the redirect getters.** `inheritIO` is
  the JDK's `redirectInput(INHERIT).redirectOutput(INHERIT).redirectError(INHERIT)`
  returning `this`; each `redirect*` method also gained its 0-arg getter
  arity, with an unset stream reading back as `PIPE` (the documented default).
  `Redirect.INHERIT`/`PIPE` are singletons, so
  `(= (.redirectInput pb) Redirect/INHERIT)` compares true after `.inheritIO()`,
  as on the JVM.

- **URL-safe Base64** ([#686](https://github.com/jolt-lang/jolt/issues/686)):
  `java.util.Base64/getUrlEncoder`, `getUrlDecoder`, and `.withoutPadding` on
  either encoder. The URL alphabet swaps `-_` for `+/`, the URL decoder takes
  padded and unpadded input, and each decoder rejects the other's alphabet,
  as on the JVM.

- **Four benchmarks covering axes the suite couldn't see** — `hash-eq`
  (composite hashing/equality), `literals` (constant collection literals),
  `transients` (bulk build), `string-ops` (host string interop) — plus
  `vecops`/`nth-access` restored to the scorecard.

### Changed

- **Bit ops and numeric-hint coercions are open-coded again.** Moving
  `bit-and`/`or`/`xor`/`not` and the `^long`/`^double` hint coercions onto
  classed-exception helpers (v0.5.13) turned Chez-inlined primitives into
  cross-unit calls cp0 cannot reach — a 1.7x `loop-recur` regression the old
  scorecard hid. `->int` gained a fixnum fast path, the emitter open-codes the
  fixnum arm of the four bit ops (`fxand`/`fxior`/`fxxor`/`fxnot`) behind a
  runtime guard, and the no-op case of `jolt->fl`/`jolt->fx` is hoisted to the
  call site. 1.28M `bit-xor`s: 26 → 4ms, back at the v0.5.12 raw-primitive
  time; every operand that could raise still reaches the helper, which owns
  the classed error (now naming the operand's class, as the reference does).

- **FFI buffer moves copy the whole block instead of one byte at a time.**
  `read-array`, `write-array`, `read-bytes` and `write-bytes` crossed the
  foreign boundary once per octet, which cost ~30ns each — so a 64K socket
  read spent ~2ms in the copy alone, dwarfing the work around it. They now
  move the block in a single copy and do the signed-byte fold or the UTF-8
  decode on the Scheme side. Measured on the same 64K buffer: `read-array`
  29.9 → 2.9 ns/byte, `write-array` 31.1 → 4.9, `read-bytes` 29.8 → 1.1,
  `write-bytes` 31.4 → 1.6. Two adapter capability entry points carry it
  (`sa-foreign-bytes-ref!` / `sa-foreign-bytes-set!`); a target with no block
  move may implement them as the old per-byte loop, which is what the Chez
  side falls back to if `memcpy` does not resolve.

### Fixed

- **`--opt` no longer unrolls recursive fn clusters into exponential code**
  ([#682](https://github.com/jolt-lang/jolt/issues/682)). The inline pass
  refused direct self-recursion but not *mutual* recursion, and its fixpoint
  pasted one more layer of a cycle per round — so Ruuter's 4-way-branching
  route matcher unrolled to ~4^5 copies: a 3.4 MB `match-trie`, a 71x bigger
  `flat.ss`, 32s builds, and +76 MB idle RSS for a one-route program. A fn
  that can reach itself through the stashed-inline call graph is now never an
  inline candidate; its calls stay real, exactly as they compile without
  `--opt`. Measured on the minimal repro: idle RSS 116 → 40 MB (the no-Ruuter
  control is 38.5), build 32.5 → 3.9s, route throughput unchanged (~2%
  faster). Acyclic helpers called from cycle code still inline.

- **Five numeric-hint and bit-op divergences from the reference**, found by
  diffing 136 hint/bit cases against JVM Clojure after the open-coding work:
  the `^long` parameter coercion is now exactly `RT.longCast` (an out-of-range
  exact integer raises "Value out of range for long", `NaN`/`Infinity` answer
  as the reference does, a `Character` is a `ClassCastException`); the `^long`
  *return* coercion is `Number.longValue` (2^64 wraps to 0, `Inf` saturates,
  `NaN` → 0 — a different operation from the parameter one, and now modelled
  as one); a `:long` operand beside a proven double no longer assumes fixnum
  range (a 2^60..2^63 bignum widened through `fixnum->flonum` raised where the
  reference answers); `bit-clear` wraps to 64 bits like `bit-set`/`bit-flip`
  (`(bit-clear -1 63)` is `Long/MAX_VALUE`, not -2^63-1); and `(double \a)` /
  `(float \a)` is documented as a deliberate permissive divergence (jolt reads
  the code point at every width; the reference throws for the float widths
  only) in known-divergences.edn.

- **A `^long`/`^double` return hint on the arglist vector is honored**, not
  just on the defn name: `(fn ^long [x] x)` and `(defn f ^long [x] x)` both
  declare the primitive return now, as in the reference. An explicit name-tag
  still wins, and a non-numeric arglist tag stays ignored.

- **`#()` no longer strips reader metadata from collection literals in its
  body.** The `%`-substitution walk rebuilds every collection it visits, and
  the rebuilt object lost its `^meta` at any depth — which is how
  `#((fn ^long [x] x) v)` lost its return hint (the hint lives on the arglist
  vector) and `(#(meta ^{:a 1} [1 2]))` read nil.

- **A load-sensitive assertion in the fibers process-io gate**: check 8
  sampled the pipe writer's `'parked` state at the instant its sibling fiber
  completed, but a writer oscillates park/unpark while the child drains its
  kernel buffers, so the sample raced under full-suite load. It now asserts
  the load-independent property (the sibling finished while the 2MB write
  could not yet have).

- **The gambit adapter binds the new bulk foreign-bytes contract entries**, so
  its contract-name gate can again tell "target with no ffi" from "target that
  forgot to port an entry".

- **A library replacing a host class constructor now says so under
  `JOLT_DEBUG`.** `clojure.core/__register-class-ctor!` exists so a shim can
  register a class jolt does not model, but nothing stopped it from replacing
  one jolt *does* — process-wide, for every namespace, silently.
  jolt-lang/http-client substitutes its own tagged-table shim for
  `java.io.ByteArrayInputStream`; any library loaded alongside it then finds
  `(.readAllBytes body)` unresolvable and `io/copy` over that stream ~3600x
  slower (0.3 → 1092 ns/byte measured), with nothing in the error pointing at
  the override. Registering a class the host does not model — the intended
  use — stays silent.

## [0.7.17] - 2026-08-19

A performance and conformance release, out of profiling the honeysql test
suite against the JVM. The theme is hashing: the JVM caches a value's hash
almost everywhere — `__hasheq` on records, the hash field on collections,
`Object.hashCode` identity for functions and plain deftypes — and jolt now
does the same, end to end. The same profiling turned up two places where the
value model itself diverged, and both are conformance fixes with visible
behavior changes, so **read `### Changed` before upgrading**. The honeysql
suite that started this runs warm in 16.8s against 0.7.16's 22.1s (the JVM
runs it in 5.1s, so 4.4x → 3.3x), and its cold first run — compile included —
drops from 37.3s to 19.2s.

### Changed

- **A plain `deftype` compares and hashes by identity.** A deftype without
  declared `equals`/`hashCode` is `Object` semantics on the JVM: two
  equal-field instances are not `=`, do not collide as map or set keys, and
  hash by identity. jolt hashed and compared every deftype structurally, which
  both diverged and went quietly stale next to `^:volatile-mutable` fields.
  defrecords keep structural equality, and a declared `equals`/`hashCode`/
  `hasheq` still governs — and is consulted on every call, never cached, since
  the JVM does not cache custom methods and one may read mutable state.

- **Array-map thresholds follow the 1.13 reference rules exactly.** jolt's
  64-entry keyword array-map limit is Clojure 1.13's `KW_HASHTABLE_THRESHOLD`;
  the rules around it now match `PersistentArrayMap` in the three places they
  drifted:

  - `assoc` consults only the **added** key's type, so a keyword assoc'd onto
    an 8-entry string-keyed map keeps it an insertion-ordered array map, and a
    non-keyword key promotes at 9 entries.
  - A map **literal** stays an array map to 64 entries when every entry past
    the eighth is keyword-keyed; the first 8 may be anything (`canBePAM`).
  - **Transients promote at capacity** — `max(8 entries, the source map's
    size)` — whatever the key type. `(into {} kw-pairs)` and `zipmap` of 9 or
    more pairs now return a `PersistentHashMap` exactly as they do on the JVM,
    where they used to stay insertion-ordered to 64 here. Transient `dissoc!`
    also compacts the way `TransientArrayMap.doWithout` does: the last entry
    moves into the removed slot, which is visible in iteration order.

  Hash-mode iteration order is byte-identical to the JVM's, so code that was
  (incorrectly) relying on insertion order from `into`/`zipmap` results sees
  the same reordering it would see on the JVM.

- **Functions hash by identity.** Every procedure used to hash to the same
  constant, so a function-keyed map degraded to a linear scan as it grew —
  loading a library that keys dispatch tables by function was quadratic.
  `hash` of a function is now a stable per-process id, like
  `Object.hashCode`; as on the JVM, it does not survive serialization, which
  images handle (see below).

- **The image format is version 5.** Records and deftypes carry the new
  hasheq slot, so every record layout tag bumped and older runtimes refuse
  images written by this one, cleanly. This build reads formats 2 through 5:
  a pre-5 image's records rebuild into the current layout through a legacy
  arm on restore, and the checked-in v0.6.8 fixture pins that. Hash caches
  never travel — the dump zeroes them, the way the JVM marks `__hasheq`
  transient — and a map or set keyed by a function or plain deftype is
  re-keyed on restore, since an identity hash is meaningless in the next
  process.

### Fixed

- **A function-keyed map or set restored from an image answered every lookup
  with nil.** The container's trie placement baked in the writing process's
  function identities; restore now rebuilds such containers from their
  entries. Deftype-keyed containers get the same treatment, which is also the
  JVM's behavior for a deserialized identity-keyed map: only the restored key
  object can answer.

### Performance

All figures A/B on the same machine, medians of repeated runs.

- **Records:** the hasheq slot answers a repeat `hash` of a 4-field record in
  35 ns, from 540 ns on 0.7.16 (the JVM's second call is ~10 ns, a field
  read). A record-keyed map `get` goes 580 → 61 ns. Construction is
  unchanged at 40 ns.
- **Collections:** a `pvec`'s cached hash field is finally read — `hash` of
  the same 1000-element vector repeats in 191 ns, from 131 µs — and seq/lazy
  heads cache theirs in a side table. `=` on maps, sets and vectors
  fast-rejects on unequal cached hashes before walking.
- **Quoted literals:** a quoted symbol or quote-containing literal in a fn
  body is hoisted to a per-site constant instead of being rebuilt per call.
- **Cheap predicates:** `true?`/`false?`/`boolean?` compile to identity
  checks (801 → 194 ns for `boolean?`), `identical?` lowers to `eq`, and the
  keyword→symbol conversion feeding map lookups rides a small interning
  front cache. Loading test.chuck's namespace graph goes 3.6 → 1.6 s.
- **`zipmap`** is a native and ~2x faster at both small and large sizes,
  with the reference's transient semantics.

## [0.7.16] - 2026-08-18

A dependencies release. Most of it is one contribution (#645, thanks to
@ingydotnet): code can now acquire a library *while it runs* — from Maven, a
Gist or a single GitHub source file — with no deps.edn and no JVM anywhere in
the picture. The same work settles what it means for a coordinate jolt already
supplies to appear in a deps.edn: jolt IS Clojure, so `org.clojure/clojure` has
always contributed no artifact, and as of this release it contributes no
children either.

**Read `### Changed` before upgrading.** A project whose only dependency is
Clojure no longer gets `clojure.spec.alpha` transitively, and the Maven and
gitlibs cache environment variables have new names with no fallback to the old
ones.

The other half is a collection fast path. `=` and `hash` on a vector, map or
set were falling through the eq and hash arm registries before reaching their
base cases, and those registries grow as libraries load — so requiring an
unrelated library made core operations slower, by 8.4x on a `hash` of a
two-entry map. Nothing about the call site changes, which is what makes that
kind of slowdown so hard to attribute.

### Added

- **`clojurestar.deps/require-deps` acquires a dependency and imports it at
  runtime.**

  ```clojure
  (require '[clojurestar.deps :refer [require-deps]])

  (require-deps
   ["mvn:dev.weavejester/medley@1.10.0/medley.core" :as medley])
  ```

  Literal libspec vectors need no quote — the macro quotes them — and a quoted
  vector or an expression evaluating to one still works, so existing calls are
  unaffected. Libspecs take `:as` and an explicit `:refer` vector, and are
  prepared left to right. An optional leading map accepts `:mvn/local-repo` and
  `:gitlibs/dir`, with `:cache-dir` retained as an alias for the source-file
  cache root. The coordinate forms are:

  ```text
  mvn:<group>/<artifact>@<version>/<namespace>

  gist:<owner>/<id>/<file>
  gist:<owner>/<id>/<file>@<revision>
  gist:<owner>/<id>/<revision>/<file>

  github:<owner>/<repo>/<ref>/<path.clj|cljc>
  github:<owner>/<repo>/blob/<ref>/<path.clj|cljc>
  ```

  Equivalent pinned spellings of the same file normalize to one identity and
  one cache entry, so writing a Gist both ways does not fetch it twice. A
  pinned coordinate reuses the persistent cache — a Gist revision has to be a
  full 40-character commit SHA, as does a GitHub ref for the same treatment;
  an unpinned Gist file, or a GitHub named ref, is re-fetched by the first
  acquisition in each process. A source file must be self-contained and begin
  with an `ns` form; loading one restores the caller's namespace afterwards,
  and a second coordinate claiming a namespace another already owns raises a
  structured error rather than quietly reloading over it. Downloads land
  through a temporary file and an atomic move.

  The namespace is portable rather than jolt-specific: the facade binds to
  `jolt.deps` here and `babashka.deps` on bb.

- **`java.nio.ByteBuffer` reads and writes ints, shorts, longs and chars, and
  `Integer/signum` exists.** The buffer shim had absolute `get(int)`, bulk
  get/put and a single-byte relative `get`, but no width above a byte — so
  framing a length prefix through a buffer, which is the ordinary reason to
  reach for one, got `No matching method putInt found taking 1 args`. Both JVM
  overloads are covered for each width: the relative form starts at `position`
  and advances it, the absolute form takes an index and leaves `position`
  alone. Everything is big-endian, the JVM's default byte order, and `.order`
  stays unshimmed on purpose so a caller asking for little-endian gets a
  missing-method error rather than big-endian bytes with no warning.
  `getShort`, `getInt` and `getLong` read back signed, so a length with the
  high bit set decodes negative the way it does on the JVM; `getChar` is a
  UTF-16 code unit and so reads unsigned, as a character. `Integer/signum` was
  missing for a related reason — the `Integer` statics had `compare`, `max` and
  `min`, and `Math/signum` returns a double, so the int-valued one had nowhere
  to land.

### Changed

- **`org.clojure/clojure` and `org.clojure/clojurescript` are terminal during
  deps.edn expansion: no artifact, and now no children.** jolt used to
  substitute Clojure's two spec children when it dropped the coordinate,
  because on the JVM they arrive with the artifact and libraries lean on that,
  requiring spec without ever naming it. #645 replaced the substitution with
  one uniform rule over host-supplied libraries, so the coordinates have to be
  declared:

  ```clojure
  {:deps {org.clojure/clojure {:mvn/version "1.12.0"}
          org.clojure/spec.alpha {:mvn/version "0.5.238"}
          org.clojure/core.specs.alpha {:mvn/version "0.4.74"}}}
  ```

  Declared that way they resolve as ordinary Maven dependencies. The failure
  when they are absent misleads, which is the reason for the warning above: the
  require reports `Could not locate clojure/spec/alpha.jolt (or .clj/.cljc) on
  the source roots`, and that reads like a jolt coverage gap rather than a
  deps.edn one line short. It is tracked in
  [known-divergences.edn](test/conformance/known-divergences.edn) under a new
  `:deps-model` category, and in the README's divergences list.

- **The Maven and gitlibs cache environment variables have new names, and the
  old names no longer work.** `JOLT_LOCAL_REPO` is now `JOLT_MAVEN_REPOSITORY`,
  `GRENADINE_LOCAL_REPOSITORY` is now `GRENADINE_MAVEN_REPOSITORY`, and
  `JOLT_GITLIBS` is now `JOLT_GITLIBS_DIR`; `GRENADINE_GITLIBS_DIR` joins
  `GITLIBS` as a shared setting. Precedence for the Maven repository is
  `:mvn/local-repo`, `JOLT_MAVEN_REPOSITORY`, `GRENADINE_MAVEN_REPOSITORY`,
  then `~/.m2/repository`; for git, Gist and GitHub source caches it is
  `:gitlibs/dir`, `JOLT_GITLIBS_DIR`, `GRENADINE_GITLIBS_DIR`, `GITLIBS`, then
  `~/.jolt/gitlibs`. A **relative** `JOLT_MAVEN_REPOSITORY` or
  `GRENADINE_MAVEN_REPOSITORY` now resolves against the invoking project
  directory rather than the process's working directory, matching
  `:mvn/local-repo` and every other project-relative path. `JOLT_MVNLIBS`, the
  opt-out from sharing a repository with the JVM toolchain, is unchanged. A
  cache configured under an old name is not lost, only unused — either rename
  the variable or point the new one at the same directory.

### Fixed

- **`range` picks its class from the argument types, the way
  `clojure.core/range` does.** `(range 0 1.0 0.1)` answered
  `clojure.lang.LongRange` in 0.7.15 where the JVM says `clojure.lang.Range`:
  the specialized seq classes added there had one range flavor and used it for
  every bounded range, so a float-stepped range wore the integer-range class.
  The reference sends every argument through `int?` and takes `LongRange` only
  when they all pass, `Range` otherwise — decided once, from what the caller
  handed over, and not following the values, which is why `(range 0 1.0 0.1)`
  is still a `Range` even though its first element is the long `0`. A zero step
  was wrong in the same place: the JVM answers `Repeat.create(start)` there, so
  `(range 0 5 0)` is a `clojure.lang.Repeat` and not a range class at all.
  `Range` joins the chunked run, since the JVM's implements `IChunkedSeq`
  exactly as `LongRange` does — `(chunked-seq? (range 0 1.0 0.1))` is true on
  both. Values, laziness, chunking and `instance?` are unchanged; only the
  class name moved. `(range 0 (bigint 5))` remains a `LongRange` where the JVM
  says `Range`, because `(class (bigint 5))` is `java.lang.Long` here — the
  integer-box model, already documented, rather than this tagging.

- **`bin/jolt` finds a threaded Chez Scheme 10.x rather than the first `chez`
  on `PATH`.** Running from source, a checkout built against a Chez that `make`
  provisioned itself could silently fall back to an older system Chez, which
  then died on whichever primitive that release predates (`variable flvector?
  is not bound`) or, after `make devboot`, on `incompatible fasl-object
  version`. The launcher now honors `$JOLT_CHEZ`, then reuses a threaded 10.x
  already provisioned under `.cache/local`, and only then searches `PATH` —
  checking the version and the threading of each candidate rather than taking
  the first name that resolves, so an unusable Chez is rejected up front
  instead of failing later somewhere stranger. Relative script arguments now
  resolve against the caller's directory too: `bin/jolt` changes into the jolt
  checkout and carries the original directory in `JOLT_PWD`, so `bin/jolt
  ../app/x.clj` used to be read relative to the checkout. This is the source
  launcher only — an installed binary was never affected.

### Performance

- **`=` and `hash` on a vector, map or set answer before the arm registry
  walk.** `jolt=2`, `jolt-hash` and `jolt-hasheq` hoist a few scalar types and
  then fall through a linear walk of the eq and hash arm registries before
  reaching their base cases. Collections were not hoisted, so every compare or
  hash of one paid a predicate call per registered arm — and the registries
  grow as libraries load, with the Clojure-facing `__register-eq!` /
  `__register-hash!` seams registering arms whose predicate is a Clojure fn
  called through `jolt-invoke`. The cost is therefore invisible on a bare
  runtime and shows up as an unrelated dependency slowing down core operations:
  loading jolt-lang/time made `(hash {:a 1 :b 2})` 8.4x slower and `(= v20
  v20)` 44% slower. Keywords were already immune because they are hoisted,
  which is what identified the mechanism.

  A second problem hid behind the same walk. `jolt-coll=?` already had a
  chunked leaf-run compare for two vectors, but it was unreachable:
  `jolt=2-base` tests sequential before collection, and a vector is
  sequential, so two vectors always took the generic seq walk — which
  allocates a seq cell per element and called the variadic `jolt=`, consing a
  rest list per element on top of that.

  Measured on one machine, with JVM Clojure for reference:

  ```text
  (= v20 v20)              1534ms -> 63.6ms   (JVM 156ms)
  (hash {:a 1 :b 2})        162ms -> 10.9ms, and no longer affected
                                              by which libraries are loaded
  honeysql cached format   4573ms -> 2864ms  (JVM 555ms)
  ```

  Hash routing is copied from the existing fallbacks, so hash values do not
  change, and equality answers are identical to the JVM's including nested
  collections, map entries, sorted maps and sets, and maps keyed by
  collections. The hoist is deliberately narrow — jolt's own vector, map and
  set only, never `jolt-map?` (whose arms let host types masquerade as maps)
  and never records, which still route through the walk. All three join the
  fast-path probe sets, so the registry now refuses an arm that would claim
  them rather than registering it and silently never consulting it, which is
  what makes answering ahead of the walk safe.

### Internal

- **The read and print scaling gates measure what they claim to.** Both judge a
  ratio — quadruple the input, and the cost must not quadruple per element —
  and both were deciding it from measurements too imprecise to support it,
  which is how printscaling failed twice on trees with no local changes at all.
  The timer was `currentTimeMillis` against a 1x arm that renders in 2.3ms, so
  whole-millisecond quantization consumed a fifth of the headroom to the
  ceiling as a systematic bias, not noise. The read gate additionally took a
  single sample per arm and straddled a step in per-form cost at 16000 forms,
  so its window measured the step rather than the reader. Both now use
  `nanoTime` and best-of several runs, the read gate sits on the flat side of
  the step at 2000 -> 8000, and a failing ratio is re-measured up to three
  times before the gate fails. None of it costs detection power: against a
  deliberately quadratic record render the ratios stay at 15.7-16.3 over a
  ceiling of 8.0 on every attempt, and an unambiguous read regression at or
  above 12 skips the retries entirely. The gates cost what they cost before
  (read 0.41s, print 0.44s), which matters because CI runs the suite in
  parallel and a gate that holds a core longer fails the timing gates beside
  it.

## [0.7.15] - 2026-08-18

An interop-dispatch release. The theme: a `.method` call on a string —
`indexOf`, `toUpperCase`, `startsWith` — is the innermost loop of
:clj-fast-path libraries like honeysql, and every one of those calls was paying
the full generic record-dispatch walk plus a rest-args vector it didn't need.
The entries below remove that cost where the target provably is a string, and
take the loose ends the campaign surfaced in `clojure.string` with it.

Profiling honeysql the rest of the way turned the theme into a broader one: the
same library's remaining gap was not interop at all but scalar-key hashing and
the numeric casts, each of which had one outlier hiding behind an otherwise
uniform per-call overhead. Those are here too.

The other half of the release is the class model. A seq used to report
`PersistentList` whatever produced it, and `cons` built one whatever it was
handed; both now answer with the class the reference gives them. **Read
`### Changed` before upgrading** — code that dispatches on a seq's class, or
that calls `list?`, `counted?` or `peek` on the result of `cons`, sees
different answers, and they are the reference's answers.

### Added

- **Fibers R8, extended to `jolt.process`: a subprocess pipe read or write
  parks the fiber instead of pinning its carrier.** The trick `jolt.socket`
  already applies to sockets now applies to `proc-fd-input-port` /
  `proc-fd-output-port` (`host/chez/java/process.ss`): the pipe fd the
  parent retains is set `O_NONBLOCK` at creation (never poller-gated), and
  on `EAGAIN` registers readiness with `jolt.io-poller` and parks the
  current fiber — the carrier runs other fibers meanwhile, and blocks on a
  private `kevent`/`epoll_wait` exactly as before when there is no fiber
  (jolt.process's own no-poller fallback keeps working standalone). A fiber
  that is about to park autoloads `jolt.io-poller` if nothing else has
  required it, which is what makes this reach the programs that motivated
  it: `jolt.socket` was the only namespace in the tree that pulls the
  poller in, so a build-tool wrapper or shell pipeline driving subprocesses
  from `go` blocks without ever opening a socket would otherwise have found
  the poller absent and taken the blocking fallback — pinning its carrier,
  the exact starvation this removes. A plain thread does not autoload it and
  keeps the cheaper blocking read. A working `fcntl` binding for Apple arm64
  is what makes the non-blocking fd viable at all: `F_SETFL` is C-variadic,
  and without the `(__varargs_after 2)` calling-convention marker it reports
  success but silently never applies the flags. Missing `fcntl` or `errno`
  bindings cost only the parking, not the `posix_spawn` path itself, whose
  fallback to Chez's `fork` leaves `SIGINT` ignored in the child. Closing a
  pipe port now also tells
  `jolt.io-poller` to forget the fd, mirroring the socket fix below: a
  closed fd is auto-dropped from the kernel's kqueue/epoll set, so a fiber
  still parked on it would otherwise wait forever for an event that is
  never coming. Gated by `test/chez/fibers-process-io-test.ss`, wired into
  `make fibers` right after the socket half's own gate.

### Performance

- **Proven-string interop emits the native call inline.** When the analyzer or
  the whole-program type pass can prove a `.method` target is a string — a
  `^String` hint, a string literal, or inference through `str`/`name`/`subs`/
  `clojure.string` returns — the compiler emits the Chez string native directly
  for the 16 hottest method/arity combinations (`indexOf`, `charAt`,
  `toUpperCase`, `startsWith`, `substring`, …): no `record-method-dispatch`
  walk, no argument vector. Anything unproven, on any other target type,
  dispatches exactly as before. A hinted `.indexOf` drops 145ns → 26ns; an
  inference-proven one with no hint in sight, 139ns → 19ns. On honeysql's
  entity formatting (two `.indexOf` per entity) the 1M-call bench falls
  4.6s → 3.2s.
- **`clojure.string` predicates are single-procedure natives.**
  `starts-with?`/`ends-with?`/`includes?`/`index-of` and `upper-case`/
  `lower-case` were prelude wrappers that coerced, counted and substringed
  before comparing; each is now one native procedure. `upper-case` on a
  keyword argument: 383ns → 43ns.
- **Keyword/fixnum/string map lookups resolve in one procedure.** The
  persistent-map fast path for the three common key types no longer re-enters
  the generic equality machinery per probe.
- **`clojure.string/replace` with a string pattern lowers to a literal scan.**
  No regex machinery is built for a plain-string match (honeysql's per-entity
  `"-"` → `"_"` rename), and `index-of` accepts a char or code point without
  materializing a one-character string. `str/join` walks the collection once
  and skips the intermediate list for 0/1-element collections.
- **`sequential?` is a native**, not two overlay calls deep.
- **Proven-keyword and proven-`StringBuilder` targets emit inline too.** The
  same proof the string work introduced now covers `.getName`/`.getNamespace`
  on a keyword and `append`/`toString`/`length`/`isEmpty`/`charAt` on a
  `StringBuilder` a `let` binding provably holds. `StringBuilder.append` was
  the most expensive interop shape in the language at ~500ns, because a jhost
  method call hashes the tag string to find the method table, hashes the method
  name to find the handler, and passes rest args as a vector it then converts
  back to a list for `apply`.
- **Symbols carry their hash and have fast paths in `jolt-hasheq` and
  `jolt=2`.** Symbols were second-class in both: hashing one walked two arm
  registries and four `cond` clauses (82.5ns against 24ns for the equivalent
  keyword), and the hash itself lived in a per-thread weak table that, for the
  case that matters most — a symbol built for one lookup and dropped — MISSED
  and then INSERTED on every call, growing the table once per lookup.
- **A symbol's name is hashed once per process, not once per symbol.** Caching
  in the record only helps a symbol hashed twice, and honeysql's `format-dsl`
  walks 92 clause keys doing `(get leftover (kw->sym k))`, building a fresh
  symbol each time. The name string is what repeats, so the murmur is memoized
  on the interning pool's entry for it and each symbol points at that entry.
  Constructing and hashing a fresh symbol: 86.9ns → 42.3ns; `(get m sym)` on
  one: 136.9ns → 91.8ns.
- **The hash engine's 32-bit leaves are fixnum-pure.** `rotl32` was the most
  expensive leaf by a factor of three, at 6.3ns against `mul32`'s 2.2ns, and it
  runs twice per murmur round: `(remainder n 32)` was a generic procedure call
  on what is a literal 13 or 15 at every call site. `rotl32` 6.3ns → 2.1ns,
  `hash-combine` 8.2ns → 3.9ns, `murmur3-hash-unencoded-chars` on a 6-char
  string 67.5ns → 34.4ns.
- **The numeric casts check for a fixnum first.** `int`, `long`,
  `unchecked-int` and `unchecked-long` all fell through a generic `cond` to a
  `truncate` call and generic bitwise masking, and `long` additionally
  range-checked against ±2^63, which are BIGNUMS on Chez's 61-bit fixnum tower,
  so every `(long x)` on an ordinary integer paid two fixnum-vs-bignum
  compares. One `(unchecked-int i)` cost 44.7ns against the JVM's 2.7ns, which
  is roughly 100ns of pure coercion per character in a hinted `.charAt` loop.
  honeysql's `alphanumeric?`, a regex rewritten as a character state machine,
  was 30x the reference almost entirely because of this.
- **`jolt-invoke1`/`jolt-invoke2` answer lookup-shaped callables.** A map, set,
  vector or symbol in head position — `(m k)`, `(k m)` — no longer falls
  through to the variadic `jolt-invoke`.

### Changed

- **A seq reports the concrete `clojure.lang` class of whatever produced it.**
  Every seq answered `clojure.lang.PersistentList` regardless of where it came
  from, so `(class (range 3))`, `(class (seq [1 2]))` and `(class (keys m))` all
  read the same and none of them read what the reference says. They now carry
  the class the reference gives them, and `list?`/`counted?`/`chunked-seq?`
  follow from it rather than from one blanket answer:

  | expression | before | now (and on the JVM) |
  |---|---|---|
  | `(range 3)` | `PersistentList` | `LongRange` |
  | `(seq [1 2])` | `PersistentList` | `PersistentVector$ChunkedSeq` |
  | `(keys {:a 1})` | `PersistentList` | `APersistentMap$KeySeq` |
  | `(vals {:a 1})` | `PersistentList` | `APersistentMap$ValSeq` |
  | `(seq "ab")` | `PersistentList` | `StringSeq` |
  | `(seq {:a 1})` | `PersistentList` | `PersistentArrayMap$Seq` |
  | `(sort [2 1])` | `PersistentList` | `ArraySeq` |
  | `(iterate inc 0)` | `PersistentList` | `Iterate` |

  Code that dispatched on a seq's class — a `defmulti` on `class`, an
  `extend-protocol` arm, an `instance?` test — sees the real class now. Code
  that relied on everything being a `PersistentList` has to change, and that is
  the point: it was relying on jolt disagreeing with Clojure.

- **`cons` builds a `Cons`, not a `PersistentList`.** `RT.cons` has two
  outcomes — onto `nil` a `PersistentList`, onto anything else a `Cons` — and a
  `Cons` is an `ASeq` but not an `IPersistentList`, so it is not `list?`, not
  `counted?`, and not a stack. jolt built a list cell either way:

  ```clojure
  (class (cons 1 (list 2)))    ; was PersistentList   -> now Cons
  (list? (cons 1 (list 2)))    ; was true             -> now false
  (counted? (cons 1 (list 2))) ; was true             -> now false
  (peek (cons 1 (list 2)))     ; was 1  -> now throws ClassCastException, as on the JVM
  ```

  The split is on the ARGUMENT being `nil`, not on its seq being empty, so
  `(cons 1 [])` is a `Cons` while `(cons 1 nil)` is a one-element
  `PersistentList` — which is what `RT.cons` tests.

- **`time` reports like the reference macro.** It measured with the epoch wall
  clock at whole-millisecond resolution, so anything faster than 1 ms printed
  `"Elapsed time: 0 msecs"` and a clock adjustment mid-expression could print a
  negative elapsed. It now reads the monotonic nanosecond clock and divides to
  fractional milliseconds, as `(System/nanoTime)` does there. It also prints
  through `prn`, not `println`, so the line is the QUOTED string the reference
  emits — output that was `Elapsed time: 0 msecs` is now
  `"Elapsed time: 0.001 msecs"`, which is what a caller diffing jolt's output
  against Clojure's sees.

### Fixed

- **Fiber socket I/O works on ARM Linux.** `struct epoll_event`'s layout is
  architecture-dependent — the kernel UAPI marks it `EPOLL_PACKED` only on
  x86_64, where it is 12 bytes with the `u64` data at offset 4, while aarch64
  aligns that `u64` naturally: 16 bytes, data at offset 8. `jolt.io-poller`
  hardcoded the x86_64 numbers, so on aarch64 it read every event's fd out of
  the struct padding, matched no waiter, and dropped the event — and epoll being
  level-triggered, the same readiness was handed back immediately, so the poller
  span at 100% of a core while reporting nothing and every fiber parked on a
  socket hung. The layout now comes off `os.arch`; the registration gate loses 8
  of 8 on aarch64 before and passes 320 of 320 after. No prebuilt binary ships
  for that target, but jolt cross-builds it and it is what a source build on ARM
  Linux produces.

- **The dependency resolver does its filesystem work on Windows.** Every
  `mkdir -p`, `mv`, `rm`, `touch`, `test -nt` and `find` in `jolt.deps` went
  through `jolt.host/sh`, which is `cmd.exe` on Windows: `mkdir` there has no
  `-p` and takes a list of paths, so every run created a directory literally
  named `-p` in the working directory, and mv/rm/touch/test/find are not
  commands at all. Running `jolt` in any directory left that `-p` behind plus a
  `.jolt/cpcache` full of `.part-` files the failed `mv` never published, and
  the classpath cache could never hit, so a warm run re-resolved from scratch
  and dropped one more file. Maven jar extraction and git checkout publishing
  failed the same way. All of it is filesystem calls now (new `jolt.host`
  seams: `mkdirs!`, `rename-file!`, `delete-file!`, `delete-tree!`,
  `file-mtime`, `list-dir`, `symlink?`), the captured-output temp files follow
  `%TEMP%` where there is no `/tmp`, and the `shelloutcheck` gate holds the
  line: only `git` and `unzip`, which are real external programs, may reach the
  shell.
- **`jolt build` finds host classes a dependency provides.** The
  require-graph scan never looked at class references inside the closure, so a
  program using a lib-provided host class (`java.util.Locale`,
  `ZonedDateTime`, …) built a binary whose runtime class-miss autoload had
  nothing to load, and hit the RFC 0008 error at startup with the dep
  correctly declared.
- **`(int x)` on an out-of-range integer throws `ArithmeticException`.**
  Clojure routes the integer case through `Numbers.throwIntOverflow`, so
  `(int 2147483648)` is an `ArithmeticException` "integer overflow" while
  `(int 2.5e9)` keeps the `IllegalArgumentException` that `RT.intCast(double)`
  raises for itself. jolt threw the IAE for both. `byte`/`short`/`long` use the
  IAE for either kind of input, so `int` is the only cast that differs.

- **`(str/replace "aaa" "" "b")` is `"bababab"`.** An empty string pattern now
  inserts the replacement at every position, as on the JVM, instead of
  returning the input unchanged.
- **`starts-with?`/`ends-with?` with a `nil` or non-string prefix** throw the
  JVM's `NullPointerException`/`ClassCastException` rather than a Scheme type
  error, so `ex-data`-driven handling sees the expected shape.

## [0.7.14] - 2026-08-16

A collections release. The theme running through it is operations that were
answering a question the data structure could already answer, by walking
instead — `count` and `drop` on a vector-backed seq, `rseq`, `first` on a
sorted collection, and, underneath the whole transient surface, a `persistent!`
that rebuilt the trie it had just been handed. None of it was visible to a
value test: the answers were correct all along, just derived the long way.
Every fix below is what the reference implementation already does. The ones
that changed a complexity class ship with a gate that measures the shape in
one process, so they cannot quietly come back; the constant-factor ones are
tracked in the benchmark suite instead, because a constant has no shape to
assert and a gate calibrating one operation against another flakes on shared
CI machines rather than catching anything.

### Added

- **`jolt.scheme`: a `(scheme ...)` macro for inline Scheme.** The body is
  written as Scheme but read by jolt, rendered to Scheme source at
  macroexpansion and evaluated through the existing eval-string seam; multiple
  forms wrap in `(begin ...)` so `define` splices and the last form's value
  returns. The renderer owns the spellings jolt's reader owns (`true`/`false`,
  chars, vectors, `nil`); keywords, maps and sets have no Scheme reading and
  are refused at macroexpansion rather than mistranslated.

### Performance

- **Transients build the map instead of deferring it.** A transient map or set
  was a Chez hashtable, so `persistent!` folded every entry back through the
  HAMT insert and rebuilt the trie from scratch — the transient did not avoid
  the path-copying build, it deferred it and added a hashtable on top. Writes
  now claim each node on their path into an editable copy and mutate in place,
  and `persistent!` freezes only the claimed spine, keeping untouched subtrees
  by pointer. Over 200k entries: `(into {} m)` 324 → 85 ms, `(into #{} v)`
  270 → 72 ms, an `assoc!` loop 306 → 77 ms; over 20k, `(frequencies v)`
  27 → 11 ms.

  `PersistentHashMap` does this with a per-node edit token. That is not
  available here — the node record's layout is image-format surface, since a
  dumped map's node tree is written raw, so adding a field to it would stop
  released images restoring. A separate editable node type answers the same
  question with a type test and gives a stronger guarantee: such a node is
  only ever reachable from the transient that created it, so the immutable
  node functions are untouched.

  Reading a live transient is about 1.25x slower as a result, because a
  hashtable probe beats a HAMT walk — which was equally true of the persistent
  map all along. The editable walk measures within 1.4% of the existing
  persistent one.

- **`count`, `drop`, `rseq` and `first` answer from the shape.** On a 200k
  collection: `(count (seq v))` 18.8 ms → below timer resolution, `(drop k
  (seq v))` 18.5 ms → 1 µs, `(rseq v)` 19.8 ms → 0, `(first sorted-map)`
  190 ms → 4 µs and flat in n, `(first sorted-set)` 94 ms → 4 µs. Each has a
  structural counterpart in the reference: `PersistentVector$ChunkedSeq`
  implements `Counted` and `IDrop`, `RT.countFrom` short-circuits on the first
  `Counted` cell, `rseq` documents constant time, and `PersistentTreeMap.min()`
  walks one spine.

- **`nth` tests the vector case first.** `jolt-nth` was wrapped four times with
  the array shim outermost, so a plain vector read paid four type probes and
  three chained calls before reaching the vector arm; `RT.nth` tests `Indexed`
  first and returns. 34.3 → 16.0 ns on a small vector, 27.2 → 9.7 ns with a
  default. Small vectors are the shape that matters — tree nodes and tuples
  are five elements, and `nth` was 20% of the time in red-black tree code.

- **`keep`, `map-indexed` and `keep-indexed` preserve chunks**, like their
  siblings already did, so they realize a chunk at a time rather than an
  element at a time.

- **`set` builds through a transient and returns an existing set unchanged.**
  It was `(apply hash-set (seq coll))`: it rebuilt an existing set element by
  element and never used a transient. 200k: `(set v)` 204 → 73 ms,
  `(set already-a-set)` 137 → 0 ms.

- **Record collection ops stop recomputing a per-type constant per instance.**
  Every one asked "does this type declare an impl?" by snapshotting the
  protocol keys under a mutex and probing each, concluding "none" every time
  for a plain defrecord and getting slower the more protocols the type had. A
  by-method index answers it with one unlocked ref, and three further per-type
  answers cache on the descriptor. On a 3-protocol record: `count` 82 → 37 ms,
  `contains?` 144 → 44 ms, `seq` 373 → 142 ms, `instance?` on a protocol
  362 → 234 ms.

- **`clojure.string` skips method dispatch for an argument that is already a
  string.** `to-str`, which every fn in the namespace coerces through, called
  `.toString` unconditionally. The wrapper cost 552 → 348 ns; `(upper-case
  "sel")` was spending over 90% of its time deciding how to dispatch.

- **Vector-backed and chunked seq cells no longer allocate a per-element tail
  closure.**

### Fixed

- **A wrong-arity record constructor names its class.** It raised through a
  Scheme binding that is unbound in that layer, so it died as "variable str is
  not bound" with no class rather than the JVM's `ArityException`. The existing
  tests only asserted "some exception", and a classless host error satisfies
  `(catch Exception e ...)`, which is why it survived; they assert the class now.

- **Printing a record is no longer quadratic in its extension-map size.** Each
  entry was appended to a growing accumulator. Comparing 4000 against 16000
  entries: 12.67x before, 4.50x after.

- **`ref-min-history` / `ref-max-history` argument handling.** They took a
  rest-arg, so three or more arguments silently read instead of raising, a
  missing target died in the Scheme layer with a classless error, and a
  non-Ref target returned the default. They now raise `ArityException`,
  `ClassCastException` and `NullPointerException` to match.

- **Transient map promotion is one-way.** A transient that grew past the
  array-map threshold and was then shrunk back below it came down an array
  map, because the mode was decided at `persistent!` from the final count. The
  reference promotes on the way up and never returns.

- **`with-meta` returns the value itself when the metadata is unchanged.** The
  JVM compares metadata by identity, not by `=`, and returns `this` on a match,
  so `(identical? v (with-meta v nil))` is true for a value with no metadata;
  jolt always allocated. This is what made the reference's `set` fast path
  unusable here.

## [0.7.13] - 2026-08-15

A performance patch. A structural sweep hunted the runtime, stdlib, and
interop layer for super-linear algorithms on user-reachable paths; every
confirmed one is fixed below, and each fix ships with a CI gate that
measures the complexity shape in-process, so none of them can quietly
come back. The browser REPL also boots again.

### Fixed

- **The Gambit web REPL boots again.** Runtime work in recent releases
  had broken every freshly built JS bundle at boot: the keyword interner
  started taking its table lock through `jolt-with-mutex`, a Chez macro
  the Gambit shims never defined, so the bundle died at the first keyword
  intern (and the failure looked like a silent hang in the worker). The
  shim exists now and is reentrant — Chez mutexes are recursive,
  SRFI-18's are not — and the Gambit var-cell record caught up with the
  Chez layout it had drifted two versions behind (`^:dynamic` seed vars
  bound instead of throwing non-dynamic). A new `gambitboot` gate runs
  the full-profile boot on gsi so a Chez-only name reaching shared code
  fails CI instead of shipping a hung REPL.

### Performance

- **Printing a collection is linear in the output.** The join under both
  printers was a right-fold of `string-append`, so every element's copy
  cost was the whole remaining suffix — `pr-str` of a 16k-element vector
  went from 104ms to 10ms, and the same fix covers records, sorted
  collections, and the Gambit host.

- **Piped streams write in O(1) per chunk.** The pipe buffer was a plain
  list re-copied on every write, under the pipe mutex — the
  `ring.util.io/piped-input-stream` path. 8k chunks: 122ms to 15ms, and
  `.available` is a running counter now.

- **`chunk-buffer` appends in O(1)** (a vector sized by cap plus a fill
  count; filling one was O(n²) with the cap argument ignored). 16k
  appends: 500ms to 1ms. Appending past cap grows — the JVM throws
  there; growth is the documented jolt superset.

- **Reader drains are linear.** `read-line` and `read` under
  `with-in-str` (and `*in*`) re-copied — and `read` re-parsed — the whole
  remaining input per item. The readers keep a cursor now; draining 8k
  lines went from 888ms to 9ms.

- **`clojure.test` starts running bodies immediately.** The run loop
  re-filtered the whole registered-test list once per namespace, and
  `run-all-tests` re-scanned the registry per loaded namespace —
  O(namespaces × tests) before a single assertion ran. Grouped once now,
  registration order preserved.

- **`refer`, `:use`, `ns-publics`, `ns-map`, and `all-ns` stopped
  scanning the image.** Each call walked every interned var in the
  process, so one `(:use …)` cost O(total vars) and a program load paid
  it per namespace. The var table now keeps a per-namespace index in
  lockstep; a three-var namespace's `ns-publics` costs the same whether
  the image holds a hundred vars or a hundred thousand.

- **`clojure.set/intersection` walks its smaller argument** (the
  reference's swap): intersecting a 100k-element set with a 3-element
  one cost the big side — measured 25930ms vs 1ms across 200 calls —
  and now costs the small one.

- **Deque interop is O(1) at the front.** `ArrayDeque`/`LinkedList`
  `poll`/`pop`/`removeFirst`/`addFirst`/`push` shifted the whole backing
  array, so the standard worklist idiom (and tools.reader's
  `.remove(0)`) was quadratic; the backing carries a head offset now.
  `StringTokenizer` iterates in O(1) per token instead of O(tokens).

- **core.async `timeout` arming is O(log pending)** — a binary min-heap
  replaces the sorted-list insert that made arming a burst of k timers
  O(k²). `.split` with a positive limit no longer recounts its output
  per part, `ReferenceQueue` and the main-thread job queue enqueue by
  cons, and `cl-format` reuses its compiled format string across calls
  instead of recompiling per call (`pprint`'s per-character buffer
  measure is O(1) too).

### Internal

- Six new scaling gates in CI — `pipescaling`, `chunkscaling`,
  `printscaling`, `ioscaling`, `hotscaling`, `gambitboot` — plus a
  linearity pin on the tree-shaker's reachability walk. All measure a
  shape inside one process (1x-vs-4x ratios or shape independence),
  never an absolute time, so they judge the algorithm and not the
  machine.

## [0.7.12] - 2026-08-15

Fibers are a public API now, Scheme is one require away, and the class
model tells the truth in both directions: `subvec` and bignums answer
their JVM classes, reflection walks a real hierarchy, and a fistful of
field reports — fn parameter lists, `-M` scripts, timed process derefs,
errno — closed along the way.

### Added

- **`jolt -M <file.clj> [args]` runs a script**, the way `clojure -M` does:
  a bare non-option argument is a script path and the remaining arguments
  become `*command-line-args*`. Works for an alias whose `:main-opts` name
  a script too. No more `-M -e` quoting gymnastics to run a file in the
  project's context.

- **`jolt.scheme`: the Scheme escape hatch.** Call a host Scheme procedure
  by name (`call`, `proc`, `defsfn`) or evaluate Scheme text
  (`eval-string`) from any jolt program. The contract is raw — numbers,
  strings, booleans and chars are shared representations; everything else
  crosses as whatever it is on the other side, and host values round-trip
  opaquely. Host-specific by design, and resolution happens at run time,
  so a tree-shaken binary reports a shaken-out binding as a catchable
  "no top-level Scheme binding" error rather than a silent nil.

- **System properties libraries actually sniff.** `os.arch` answers in the
  JVM's spelling (`aarch64`/`amd64`), `user.name` comes from the
  environment, and `os.version` reports the macOS product version
  (`sw_vers`, what the JVM says there) or the kernel release on Linux —
  each also present in `(System/getProperties)`. `java.version` stays nil
  deliberately: jolt has no JDK to report, and claiming one would activate
  JVM-only code paths in libraries that parse it.

- **Minimal Class reflection.** `.getSuperclass`, `.getInterfaces`,
  `.isAssignableFrom`, and `.isInterface` answer from the one modeled class
  graph — so the exception chain walks to `Object`, an interface's
  superclass is nil, and a defrecord reflects like any modeled class. A dot
  form on a class token whose static member doesn't exist now falls back to
  the `java.lang.Class` instance methods on the class object, the way JVM
  Clojure resolves `(.getName String)` — which also makes
  `(.isAssignableFrom Object …)` reachable. `getSuperclass` on a
  statics-only shim (`Math`) answers nil where the JVM answers `Object`;
  tracked.

- **`jolt.ffi/errno` and `errno-message`: a public, thread-correct errno.**
  errno is a per-thread slot behind a libc function (`__error` on macOS,
  `__errno_location` on Linux, `_errno` on Windows); the accessor reads the
  calling thread's slot, so it is right under threads and under fibers
  (whose syscall and read share a carrier thread). `jolt.io-poller` now
  reads through it instead of binding the platform pair privately —
  downstream FFI code no longer has to rediscover that dance. Read it
  immediately after the failing call; the docstring says why.

- **`jolt.fibers`: the fiber primitive as a public API**, side by side with
  core.async. `spawn` runs a body on the carrier pool and returns the fiber
  handle; `join` waits for its value (parking on a fiber, blocking on a
  thread; optional timeout) and rethrows the body's error; `monitor!`
  observes completion race-free (a finished fiber fires the callback
  inline); `state`, `fiber?`, `current-fiber`, `in-fiber?`, `yield`, and the
  pool knobs (`carrier-count`/`set-carrier-count!`,
  `preempt-ticks`/`set-preempt-ticks!`) round it out. Channel ops and
  `deref` inside a spawned body park exactly as they do in a `:fiber` go
  block. The gate (`test/chez/jolt-fibers-test.clj`, in `make smoke`) covers
  spawn/join/monitor semantics, error and binding conveyance, parked-state
  observation, and the knob floors.

### Fixed

- **`subvec` answers the JVM's `SubVector` class (#629).** A non-empty
  `subvec` is `clojure.lang.APersistentVector$SubVector` — extending
  `APersistentVector` in the modeled hierarchy, so every vector check
  holds while concrete-class dispatch distinguishes it, exactly as on the
  JVM — and it stays one under `conj`/`assoc`/`pop`, while rebuilds, an
  empty range, and popping a plain vector answer `PersistentVector`. The
  representation is unchanged: still the RRB structural slice, which
  (unlike the JVM's view) does not retain the backing vector. One shared
  vector representation carries a kind, the same seam map entries already
  used.

- **Integer classes are value-sensitive (#627).** `(instance?
  clojure.lang.BigInt 21)` and `(instance? java.math.BigInteger 21)` were
  true for every integer; now a fixnum is a `Long` (and an `Integer` —
  the documented breadth) and neither big class, while a bignum answers
  `clojure.lang.BigInt` from `class` and `instance?` — the class the
  JVM's promotion produces — and is no longer a `Long`. A bignum still
  answers `BigInteger` (one big representation serves both classes), the
  remaining documented superset.

- **`_` as both a positional and the rest parameter compiles.** `(fn [_ x
  & _] …)` — legal Clojure, the later binder wins — was rejected with
  "invalid parameter list": the duplicate-parameter rename ran over the
  fixed slots only, so a positional duplicating the rest name survived
  into the one Scheme formal list. Renaming now covers fixed and rest
  together; the rest binder is the last occurrence and keeps its name, so
  `((fn [_ _ & _] _) 1 2 3 4)` answers `(3 4)` as on the JVM.

- **A timed `deref` on a process honours the timeout.** `(deref proc 300
  ::timed-out)` threw a cast error: jolt satisfies `:bb` reader
  conditionals, and upstream babashka.process guards its
  `IBlockingDeref` arm behind `#?@(:bb [] :clj […])` — so jolt took the
  same empty arm real babashka takes. The vendored submodule now points
  at the jolt-lang/process fork, whose `:jolt` arm implements the timed
  wait over the host's `waitFor`; a live process answers the timeout
  value, a finished one its result map.

## [0.7.11] - 2026-08-14

Vectors grew a tree. Concatenating and slicing persistent vectors is
structural now — `(into vec vec)` and `subvec` are O(log n) instead of
rebuilding element by element — and built binaries can explain their own
startup. A dependency resolution that failed halfway can no longer be cached
as if it had succeeded.

### Added

- **RRB vectors.** The persistent vector supports O(log n) structural
  concatenation and slicing (an RRB tree behind the existing vector), with a
  `clojure.core.rrb-vector` overlay (`catvec`/`subvec`) for code using the
  library API. `(into vec vec)` and `clojure.core/subvec` route through the
  structural ops, so both are logarithmic in the vector size. Complexity
  gates pin the class at the raw-op level (`make rrbscaling`) and through
  core (`make vecscaling`) — an element-by-element rebuild coming back fails
  the build, not a benchmark eyeball.

- **Startup profiler for built binaries.** `JOLT_STARTUP_PROFILE=1 ./myapp`
  writes per-stage wall time — native boot, runtime files, each namespace,
  `-main` — with no measurable cost when disabled (the default).

- **Load watchdog.** A namespace-load claim wait past its limit (default
  120s; `JOLT_LOAD_WAIT_LIMIT_SECS` overrides, 0 disables) raises with the
  full claim table — which namespaces are loading, which threads are waiting
  on them — instead of hanging silently.

### Fixed

- **A degraded resolution is never cached.** A Maven or local jar that could
  not be extracted (no `unzip` in the environment, full disk) only warned,
  resolved the project *without* that dependency's source, and wrote the
  result to `.jolt/cpcache` — where it kept being served after the
  environment was fixed, until the project's `.jolt` was deleted by hand.
  Extraction failures now collect into the same "artifacts could not be
  resolved" report as fetch failures and abort the resolution, so nothing
  degraded is ever written. The cache key also folds `JOLT_MVNLIBS` /
  `JOLT_LOCAL_REPO` / `GRENADINE_LOCAL_REPOSITORY`: they move where
  extractions live, and the old location usually still exists, so validating
  that cached paths exist could not catch the switch.

## [0.7.10] - 2026-08-13

Startup, twice over. A binary that took 1.7 seconds to start a two-dependency
project now takes 0.17s, and a regex-heavy application binary no longer pays
for patterns it never matches — one profiled CLI spent 372ms of its 660ms
launch compiling two HTTP Link-header patterns its command path never used.
Native libraries now load scoped, closing a symbol-shadowing regression that
flipped crypto to the wrong SSL library. `resolve` returns classes, reader
conditionals match `:bb`, and `destroy-tree` actually destroys the tree.

### Performance

- **Stdlib namespaces load precompiled.** Install-owned namespaces (clojure.test,
  jolt.time, jolt.nrepl, jolt.fs, …) were excluded from the AOT cache, so every
  require recompiled them from source on every process start — +0.35s for
  clojure.test, +1.3s for the jolt.time base namespaces in any project using the
  time library. The binary now carries one compiled fasl per namespace in a
  linked blob; a require memcpy's the slice out and loads it. Requires that cost
  0.3–1.4s are now at the floor. The embedded set is pinned in
  `host/chez/stdlib-fasl-manifest.txt`, and drift fails the build. Binary +1MB;
  startup floor and memory unchanged.

- **Project dependency resolution is cached.** A warm run re-expanded the whole
  dependency graph — POM parsing, version selection, gitlib probing — on every
  invocation. The final resolution is now cached under the project's
  `.jolt/cpcache`, keyed on the content of everything the expansion reads: the
  project and user deps.edn, the active alias set, every `:local/root` dep's
  deps.edn, and the artifact-root environment (`JOLT_GITLIBS` and friends). A
  hit validates that every cached path still exists, so a pruned gitlib
  re-expands instead of erroring; `-Stree`/`-Strace`/`-Scp` never touch it.
  Warm `jolt -e nil` in a project: 0.38s → 0.14s.

- **Regex engines compile lazily.** `re-pattern` built two engines per pattern
  at construction: a throwaway DFA compile used only to count capture groups —
  the expensive one on alternation-heavy patterns — and then the real
  backtracking engine. Construction now only parses (a malformed pattern still
  throws `PatternSyntaxException` right there, like the JVM), and the engine
  builds at first match, chosen from the parsed pattern, so a capturing pattern
  builds exactly one engine and a pattern that is never matched never compiles.
  A namespace of `def`'d patterns loads for the price of parsing: the profiled
  Link-header pair went from 1.25ms to 23µs at construction, and to nothing at
  all when the middleware holding them isn't exercised.

- **The dev boot cache no longer recompiles the CLI on every invocation**
  (contributors only): `make devboot`'s image AOTs jolt.main + jolt.deps the
  same way the release build does. Dev `bin/jolt` 1.65s → 0.25s.

### Changed

- **Reader conditionals match `:bb`.** The feature set is now
  `:jolt :bb :clj :default`, like babashka's own `:bb :clj`: a library's `:bb`
  branch solves the same non-JVM problems jolt has — no reflection, no JVM-only
  classes — and is listed ahead of `:clj` precisely so a bb-like host takes it.
  Previously every such branch was unreachable. A `:jolt` branch placed before
  `:bb`/`:clj` still overrides both. Where babashka's own model differs from
  jolt's (bb represents classes as symbols in hierarchies and is lenient where
  jolt throws like the JVM), jolt keeps the JVM shape; the divergence entry in
  known-divergences spells this out. `jolt.fs` now defines `list-dir` itself —
  the vendored babashka.fs leaves it to bb's built-in on a `:bb` host.

### Fixed

- **A `:blocking` foreign call resolved through a scoped handle keeps
  collect-safety.** The scoped-resolution path introduced in this release built
  the foreign procedure from the dlsym address without `__collect_safe`, so in
  a process with any registered native handle every `:blocking` socket call
  lost the convention on Linux (dlsym through a handle also resolves libc
  symbols via its dependency scope). A garbage collection while one of them
  blocked then waited forever, and every other thread parked behind it — a
  silent, zero-CPU freeze of the whole VM, most likely on a first run with
  cold caches, where compilation keeps the collector busy. Both resolution
  branches now carry the convention, and the FFI gate asserts it in the
  emitted code.
- **Native libraries load scoped, and FFI symbol resolution is deterministic.**
  Chez resolves a foreign name against shared objects most-recently-loaded
  first, and re-loading the process-global handle re-promoted it — on macOS
  that flipped every `EVP_*` to Apple's BoringSSL, breaking jolt-crypto against
  the OpenSSL it had verified against. `jolt.ffi/load-library` now dlopens
  `RTLD_LOCAL` and resolves symbols per handle, declared `:jolt/native`
  libraries before the global namespace, so a library gets the symbols of the
  object it named. The per-OS map form of `load-library`
  (`{:mac "libx.dylib" :linux "libx.so"}`) works as documented and a missing
  platform key raises naming it; both are gated in the CLI smoke. Site FFI docs
  describe the resolution order.

- **A poller readiness registration can no longer be lost.** The io-poller
  drained pending registrations in two critical sections, so a registration
  landing between them was erased before reaching the kernel set — one fiber in
  a thousand parked forever. The drain is one critical section now, a parking
  fiber commits under the poller lock with interrupts disabled, closing a
  socket forgets its fd (a reused descriptor inherited stale ready-flags), and
  `jolt.io-poller/debug-state` classifies any future loss at the stage that
  dropped it. Gated by a registration-storm smoke that fails on the first lost
  wakeup.

- **`resolve` and `ns-resolve` return the class for a class mapping**, like the
  JVM: `(resolve 'String)` is the class `java.lang.String`, `(= (resolve 'X) X)`
  holds for dotted names, imported short names, and deftype names, and a var
  that merely holds a class — `(def MyCls String)` — still resolves to the var.
  Tooling that classifies resolution results with `class?` (typed.cljc.analyzer
  does) now works unpatched. A macro that splices the result —
  `` `(instance? ~(resolve sym) x) `` — compiles in call position and inside
  quoted structure, the way spliced vars always did.

- **`destroy-tree` destroys the tree.** `ProcessHandle.descendants` was
  hardcoded empty, so babashka.process's `destroy-tree` quietly reduced to
  `destroy`: killing a wrapper (`lake env repl`, a shell around the real work)
  orphaned whatever the wrapper had spawned — observed in the wild as a repl
  holding 4.8GB, reparented to init, unreachable but alive. descendants now asks
  the OS for the live tree: `proc_listchildpids` on macOS, one `/proc/*/stat`
  pass on Linux. Windows still answers empty.

### Internal

- A `vecops` benchmark suite records the vector concat/slice axis (pairwise
  `into`, `subvec` windows, split-rejoin) ahead of the RRB work.

## [0.7.9] - 2026-08-13

A dependency fetch that hits a network blip now tries again instead of failing
the build.

### Fixed

- **The git dependency steps retry a transient failure.** `git ls-remote`,
  `git clone` and `git submodule update` talk to a remote, and a remote fails
  transiently more often than not — a reset, a rate limit, a DNS blip. Any one of
  those failed the whole resolution, and with it the build. They now retry up to
  three times with a short backoff, the way the HTTPS fetch already did, and the
  error names the attempts when it gives up:

  ```
  git dep: could not list <url> (git ls-remote exited 128 after 3 attempts: …)
  ```

  `git checkout` and the publish rename are unchanged: they are local, so
  retrying them would hide a real failure rather than survive a flake.

### Internal

- The nREPL listen socket's close-on-exec is now checked by the CLI smoke,
  reading `fcntl(F_GETFD)` on an ephemeral port. It asserts the property rather
  than one mechanism, so each platform covers the path it relies on.

## [0.7.8] - 2026-08-12

Two things that only show up when something else goes wrong. A dependency fetch
that failed reported the dependency as missing, which is the wrong place to look;
and an nREPL's listening socket was inherited by every subprocess, so the port
outlived the server holding it.

### Fixed

- **A failed `git ls-remote` is no longer reported as a missing tag.** Every
  non-zero exit collapsed into "not found", so a reset, a rate limit or an
  unreachable host read as *this tag does not exist* — sending you to the
  repository and the pin instead of to the fetch. It now says which happened, and
  carries git's own message:

  ```
  git dep: could not list <url> (git ls-remote exited 128: fatal: '<path>' does
  not appear to be a git repository)
  ```

  A tag the repository genuinely lacks still reports "tag not found", unchanged.
  This is the distinction the Maven path already made, for the same reason.

- **The nREPL listen socket no longer survives into child processes.** Without
  close-on-exec, every subprocess spawned from a process running an nREPL
  inherited a duplicate of the listening socket, and the port stayed bound for as
  long as any of them lived — so killing the server left the next start unable to
  bind its nREPL, against a server that was already gone. `SOCK_CLOEXEC` on Linux
  so the descriptor is never briefly inheritable, `fcntl` `F_SETFD` elsewhere on
  POSIX, and neither on Windows, which controls inheritance with
  `HANDLE_FLAG_INHERIT`.

## [0.7.7] - 2026-08-12

Reading. Running [edamame](https://github.com/borkdude/edamame)'s own test suite
turned up a name jolt had quietly reserved — `syntax-quote`, which is not a
Clojure special form, and which edamame happens to call its own resolver — plus
three gaps in `read`/`read-line` over a `java.io` reader and a `conj` that
rejected a record it agreed was a map. Fixing the `read` arity then exposed what
had been hiding behind it: reading a source file form by form was quadratic, at
674x the JVM's time on `clojure/core.clj`.

### Fixed

- **`syntax-quote` is not a reserved name.** jolt's reader lowers `` ` `` to a
  marker form that the analyzer expands, and that marker used to be spelled with
  the bare symbol `syntax-quote` — which the analyzer dispatched as a special
  form, and special forms are deliberately not shadowable. So a var or a local of
  that name was compiled into a syntax-quote of its first argument instead of a
  call, silently producing a wrong value rather than an error:

  ```clojure
  (defn syntax-quote [a b c] [:fn a b c])
  (syntax-quote 1 2 3)
  ;; was => 1          (the arguments were dropped)
  ;; now => [:fn 1 2 3]
  ```

  The marker is now `clojure.core`-qualified, the way `~` and `~@` already read as
  `clojure.core/unquote` and `clojure.core/unquote-splicing`. `` `syntax-quote ``
  also qualifies to the current namespace now, like any other symbol.

- **`read` takes an options map.** The `(read opts stream)` and
  `(read stream eof-error? eof-value recursive?)` arities were missing, and so
  were the matching two on `read+string`. `{:eof v}` is the value returned at end
  of input, and it is the key's *absence* — not a nil value under it — that
  throws.

  ```clojure
  (read {:eof :done} rdr)   ; was: Wrong number of args (2) passed to: clojure.core/read
  ```

- **`read-line` reads from whatever `*in*` holds.** It went through a protocol
  only jolt's own `*in*` implements, so binding `*in*` to a `java.io` reader —
  which is exactly what `clojure.tools.reader`'s `read-line` does for a
  `LineNumberingPushbackReader` — threw `No method -read-line`. That reader also
  had no `.readLine` of its own.

- **`conj` and `merge` accept a record on the right.** A record *is* a map, and
  `(map? a-record)` said so, but `(conj {:a 1} a-record)` raised "conj on a map
  expects a [k v] pair or a map". `conj` now asks whether the right-hand side seqs
  into entries — the question the JVM asks — so records, sorted maps and a bare
  seq of map entries all work, and a non-pair vector and a non-seqable both report
  what the JVM reports.

- **A `PushbackReader` is not a `BufferedReader`.** `instance?` answered true for
  every reader, so library code branching on the difference took the wrong path.

- **Each form read from a reader carries its own line.** Every form read through
  `(read rdr)` used to be stamped `{:line 1 :column 1}`, because each read reparsed
  a fresh copy of the remaining source. A `LineNumberingPushbackReader` now reports
  the same positions the JVM does.

### Performance

- **Reading a source file form by form is no longer quadratic.** Each `read` off a
  `java.io` reader drained the *entire* remaining reader — one method dispatch per
  character, a quarter-million of them for a 250KB file — parsed one form, and
  pushed the tail back as a new reader. A string-backed reader now parses at its
  own index and advances. Reading `clojure/core.clj` (263KB, 697 top-level forms),
  with JVM Clojure 1.12.5 for scale:

  | reader | 0.7.6 | 0.7.7 | JVM |
  | --- | --- | --- | --- |
  | `java.io.PushbackReader` | 37764 ms | 27 ms | 31 ms |
  | `clojure.lang.LineNumberingPushbackReader` | 41953 ms | 32 ms | 23 ms |

  `slurp`, `line-seq` and `clojure.edn/read` over a reader take the same bulk path
  and get the same relief.

### Library conformance

| library | before | after |
| --- | --- | --- |
| edamame | did not load | **50 tests, 351 pass**, 0 fail, 0 error |

## [0.7.6] - 2026-08-12

Standard input, and what a stream is willing to say about itself. `System/in` did
not exist, so a namespace that read stdin the way ported code does failed to
load; adding it exposed that jolt was reading fd 0 through two ports that
buffered independently, which is not how the JVM stacks them. Following that
through the io layer turned up the rest: line endings `readLine` handles and jolt
did not, a reader that closed the stream it was built over, a read that waited for
a buffer to fill rather than for input to arrive, and `available()` answering 0 for
everything — which java.io permits, and which quietly makes every loop written
around it a loop that never runs. Text accumulation was also quadratic, in a way
that showed up as seconds on a large JSON value.

### Fixed

- **`System/in` exists.** A namespace that read standard input the ordinary
  ported way — `(slurp System/in)`, `(io/reader System/in)`,
  `(io/copy System/in System/out)` — failed to load outright with "No matching
  field or method: System/in". `System/setIn` replaces it, and unlike the JVM
  that also redirects `read-line`, which is otherwise only reachable through
  `with-in-str`.

- **`read-line` and `System/in` are the same stream.** They were two ports on
  fd 0, each buffering ahead on its own, so whichever read first ate input the
  other never saw. `*in*` is a Reader over `System/in` now, the way the JVM
  builds it, and nothing is read past the line returned:

  ```clojure
  ;; stdin: "alpha\nbeta\n"
  [(read-line) (.read System/in) (read-line)]
  ;; before  ["alpha" 98 "eta"] or ["alpha" -1 nil], depending on what buffered first
  ;; after   ["alpha" 98 "eta"]  ; the JVM answers -1 here, from its reader's own 8K buffer
  ```

- **`read-line` ends a line the way `readLine` does** — on `\r` and on `\r\n`,
  not on `\n` alone. A CRLF file came back with the `\r` inside each string, and
  a CR-only file read as one enormous line.

- **`(io/reader System/in)` no longer takes standard input away.** R6RS
  `transcoded-port` takes ownership of the port it wraps, so building the reader
  closed fd 0 underneath both it and `read-line`.

- **`InputStream.read(buf off len)` returns as soon as a byte is there**, which
  is the JVM's contract, rather than waiting for the buffer to fill. A read over
  a pipe or a terminal hung whenever the rest of the buffer was only going to
  arrive after the program did something with these bytes.

- **`available()` answers a real byte count.** Every stream reported 0. That is a
  legal JVM answer — java.io documents it as an estimate — but it leaves
  `(pos? (.available in))` false forever, so the loop written to drain what has
  arrived never runs an iteration. A file, a byte array and a piped stream now
  answer their exact remainder; a pipe, a terminal and a socket answer the
  kernel's count, from the same `ioctl(FIONREAD)` the JVM asks. This covers
  `jolt.socket`'s streams as well, whose entry in the known-divergences list is
  gone rather than rewritten.

- **A file read while a macro expands invalidates the AOT cache.** (#576) The key
  covered a namespace's own source and the namespaces it requires, so a macro
  that slurped a resource — a SQL migration, a template — kept its old expansion
  after that file changed, and the binary shipped the stale text. Resources a
  compile reads are part of the key now, including ones that were missing and
  have since appeared.

- **A closed stream raises `java.io.IOException`** from `read`, `readAllBytes`,
  `skip` and `available`, rather than a classless host error that no
  `(catch java.io.IOException …)` could see. A closed socket raises
  `java.net.SocketException`, as Java's does.

### Changed

- **`System/out` and `System/err` are the `java.io.PrintStream`s they are on the
  JVM.** They reported `java.io.PrintWriter` — `*out*`'s class, not theirs — and
  answered false to `(instance? java.io.OutputStream System/out)`, which is what
  ported code branches on before handing them to anything taking a stream.

- **A `with-out-str` no longer captures what is written to `System/out`.** The
  two are different objects on the JVM and a program writing deliberately past a
  capture — a prompt, a progress bar, a logger aimed at the real stderr — had its
  output swallowed into the string. `*out*` is unaffected; that is what
  `with-out-str` captures.

- **`(io/copy System/in System/out)` is byte for byte.** A `PrintStream` is an
  `OutputStream`, and routing the copy through the text sink decoded the bytes as
  UTF-8 first, replacing every byte that was not text with U+FFFD.

### Performance

- **Accumulating text a piece at a time is linear.** `StringBuilder.append` did
  `(string-append (whole buffer) piece)`, so building an n-character string cost
  O(n²); `StringWriter.write` and `PrintWriter`'s write-through to either had the
  same shape. It showed up wherever text is built up incrementally: data.json
  reads a quoted string one character at a time, and an 88KB JSON string value
  took 623ms to parse against 30ms for the same bytes spread over many short
  values. That case now reads in 200ms and writes in 89ms, from 623ms and 672ms.

- **`read-line` over piped stdin is 3.6x faster**, 3372 → 940 ns/line over 300k
  lines. `(current-input-port)` is Chez's console port and reads a character at a
  time; the port `System/in` already had reads the descriptor in blocks.

### Internal

- The runtime can bind a variadic C function. `jolt-foreign-proc-safe` takes a
  calling convention, the same `(__varargs_after n)` `jolt.ffi`'s `:varargs`
  marker emits for a library binding. Binding `ioctl` fixed-arity is what makes
  Apple arm64 return success with the out-parameter untouched, and that is a
  property of the binding rather than a limit of the FFI.

- Two gates were fixed rather than worked around: a Chez port over a socket
  answers `#t` to `port-has-port-length?` and then raises ESPIPE, so `available`
  asks by trying; and `state-image-test` named its temporary image with
  `(random 100000)`, which is the same number on every run, so two concurrent
  runs deleted each other's file.

## [0.7.5] - 2026-08-12

Everything here is one subject: what a fiber may do with the thread it is running
on. The 0.7.4 deadlock turned out to have a sibling one lock over, in the loader,
and both were instances of a rule the runtime stated in two contradictory ways.
Looking for the rest of that family found the opposite failure as well, and a
larger one: a `go` block that waited for almost anything — a promise, a future, an
agent, a thread, a latch, a subprocess — stopped every other fiber sharing its
carrier, and could deadlock outright. Two rules now, both checked at build time and
at run time rather than described in a comment: a fiber never leaves the CPU while
holding one of the runtime's locks, and a fiber never blocks the thread it runs on.

### Fixed

- **A `require` that has to wait no longer risks wedging the process.** A
  namespace whose top level blocks, required at the same time by several fibers
  and by a thread, could stop everything: a waiting fiber parked inside the short
  critical section that guards the loader's per-namespace claims, which leaves a
  blocking re-acquire of that lock attached to the fiber's resume. That re-acquire
  runs inside the scheduler, on the carrier thread, and the carrier can do nothing
  else until it succeeds, so every fiber on it waits behind a lock that every
  other load also passes through. Same shape as the object monitor fixed in 0.7.4,
  found by looking for it rather than by hitting it. (jolt-04ee)

- **`restart-agent` no longer runs the agent's validator while holding the
  agent's lock.** A validator is user code and may block, which would have
  released the lock half way through the restart. It now runs inside the agent's
  object monitor, which is what `synchronized` is on the JVM, so two concurrent
  restarts still serialize and the error a healthy agent reports is unchanged.

- **Blocking from a `go` block on the fiber backend no longer stops the other
  fibers sharing its carrier, and no longer deadlocks.** `@a-promise`, `@a-future`
  and their timed forms, `await` on an agent, `.join` on a thread,
  `CountDownLatch.await`, `.get` and `.awaitTermination` on an executor, reading a
  piped stream, and waiting on a subprocess all waited by blocking the carrier
  thread. A carrier runs many fibers and a fiber cannot move to another one, so
  such a wait also stopped every fiber placed behind it — including, frequently,
  the one that would have ended the wait. Two fibers on one carrier, one deref'ing
  a promise the other delivers, hung forever. With more carriers the same code
  merely stalled, which is harder to see and no more correct.

  Each of those waits now parks the fiber and is resumed by whatever ends the
  wait; a real thread still blocks, which is what a thread should do. Timed waits
  park with a deadline registered against the runtime's one timer thread. The
  loader and `locking` were already doing this, which is why they were not
  affected, and that is now the only way the runtime knows how to wait: a bare
  `condition-wait` outside the file that defines the two is a build failure.
  (jolt-x1no)

  The same went for the two waits that poll rather than wait on a condition,
  because there is nothing for them to wait on: `.waitFor` on a subprocess and the
  stdin readiness check behind `read-line`. Both slept the carrier between probes,
  for as long as the child ran or until input arrived. `.waitFor` was the worst
  case in the runtime, because it also held the per-process lock for that whole
  time, and a carrier holding one of the runtime's locks is not preemptible either
  — so a `.waitFor` in a `go` block froze every fiber on its carrier beyond the
  reach of even the preemption that is supposed to be the backstop. It now holds
  that lock for one `waitpid` attempt and parks between attempts.

  One related case is deliberately unchanged: `Thread/sleep` in a `go` block still
  occupies its carrier for the duration, matching what it does in a JVM
  `core.async` go block. It cannot deadlock, since it always makes progress, and
  jolt has a gate asserting that behaviour on purpose.

### Changed

- **A fiber can no longer leave the CPU while its carrier holds one of the
  runtime's locks, and that is now enforced rather than described.** The scheduler
  already refused to *preempt* a fiber holding a lock, but a voluntary park was
  allowed subject to a condition about every other user of that lock on the same
  carrier, which no individual site can check and no reviewer can see. Three bugs
  arrived through that gap, the last two of them process-wide deadlocks. There is
  one rule now, it covers both kinds of switch, and it is checked in two places:
  at the switch itself, where breaking it raises an error naming the rule instead
  of stopping the process with no output, and over the whole runtime as a build
  gate that reads the code and rejects the shape before it can run. Waiting for
  state a lock guards, which is what the exception existed for, is a single
  primitive that the object monitor, `ReentrantLock`, the loader, the channel
  waiters and the IO poller all share. (jolt-h9nq)

  For anyone who hit the 0.7.4 class of failure: the symptom to expect from a
  regression now is an error mentioning that a fiber cannot leave the CPU while a
  lock is held, rather than a silent hang.

## [0.7.4] - 2026-08-12

One fix, and it is a deadlock: a monitor contended by a real thread and by fibers
at the same time could stop the whole process. It is what wedged a `make test` for
ninety minutes, and it is not the timer the 0.7.3 notes guessed at, so that note
is corrected as well.

### Fixed

- **`locking` on an object contended by a thread and by fibers no longer
  deadlocks.** One OS thread and eight fibers taking the same monitor in a loop
  wedged the process in one run of twelve. Every thread ended up parked, with
  nothing left in a poller or a timer and nothing runnable, so there was no error
  and no output: the process simply stopped. `dosync`, a `delay` being forced and
  `ReentrantLock` all take the same monitor and were exposed the same way, and
  nothing about the program has to mention fibers beyond running a `go` block on
  them.

  A waiting fiber parked *inside* the short critical section that guards the
  monitor's own bookkeeping, relying on that section being a `dynamic-wind` to
  release the lock on the way out and re-acquire it on the resume. The rule for
  parking inside one of those sections is stronger than that: no fiber on the
  carrier may be holding the lock while this one is off the CPU, because the
  re-acquire runs from Chez's rewind, on the carrier thread, at the interrupt
  depth the fiber parked at, where the carrier can do nothing else until it
  succeeds. A monitor's bookkeeping lock cannot satisfy that, since every fiber on
  the carrier and every thread passes through the section for the same monitor.

  The fiber now registers itself and commits to parked under that lock, and the
  switch happens outside it, after which the decision is retaken from the start,
  because a resume only ever means that something changed. That is what the
  channel waiters and the IO poller already did. The thread side is unchanged.
  (#586, jolt-dfuo)

- **The 0.7.3 note on the ninety-minute `make test` wedge was wrong** about the
  cause and is corrected: the `(timeout ms)` timer fixed in that release cannot
  produce it. The case it wedged on creates its timeouts in increasing deadline
  order, so every insert lands at the tail of the timer's pending list and neither
  timer bug can fire there. The wedge was the monitor above, which reproduces with
  no sockets, no poller and no timeouts at all. (jolt-8tma)

## [0.7.3] - 2026-08-11

`core.async` names three carriers and jolt has all three now: `io-thread` runs
its body on a fiber. Three fixes are for things that failed quietly rather than
loudly, which is what took them so long to find: `System/exit` off the main
thread did nothing, a stray close paren truncated a file and reported success,
and the one timer behind every `(timeout ms)` could miss its own deadline by the
length of the farthest deadline pending. Three more come from making the real
`clojure.jdbc` run: `with-open` on a `reify`, `(Class/FIELD)` in call position,
and a protocol extended to a class a library declared.

### Added

- **`io-thread` runs its body on a fiber.** It was an alias for `thread`, so
  asking for the cheap blocking-shaped carrier got you an OS thread and the
  `workload` argument to `thread-call` was accepted and ignored. The three names
  now pick three different things: `thread` is a real OS thread, `go` is the CPS
  pass on whatever `clojure.core.async/*go-backend*` says, and `io-thread`, which
  is `(thread-call f :io)`, is a fiber. `:mixed` (the default, as on the JVM) and
  `:compute` stay OS threads, and any other workload throws rather than quietly
  running on one of them.

  Neither `thread` nor `io-thread` consults `*go-backend*`, because both name
  their carrier at the call site. A body that parks releases its carrier, so
  thousands of `io-thread` bodies cost thousands of stacks rather than thousands
  of OS threads, and a park works anywhere in the body, including inside a called
  function, a `try` or a loop, because a fiber parks by capturing its
  continuation rather than by being rewritten. That is the reason to have
  `io-thread` as well as `go`.

  What a fiber does not get is an OS thread of its own. Channel operations park,
  and so does `jolt.socket`, but work that blocks somewhere the runtime cannot
  see (`Thread/sleep`, a raw fd read, a blocking FFI call) holds its carrier for
  the duration. `thread` is still the escape for that, and the docstrings say
  which is which. `go`'s default backend is unchanged. (#583, jolt-579)

### Fixed

- **`System/exit` ends the process from any thread.** It was Chez's `exit`, which
  unwinds only the calling thread when that thread is not the main one, so off
  the main thread the call did nothing the process could notice: a worker that
  decided the program should stop could not stop it, and nothing said it had
  tried. On the JVM `System.exit` halts the VM from wherever it is called. The
  boot thread keeps the normal path with its exit handlers; any other thread
  flushes stdout and stderr by hand and calls libc `_exit`. Not unwinding is
  right on its own terms too, since the JVM does not run `finally` blocks on
  `System.exit`. (#582, jolt-7xls)

- **One paren too many is a read error, not a silent truncation.** The two loops
  that read a file form by form treated a stray close delimiter as end of input,
  so everything after it was dropped: `jolt run` printed whatever came before the
  paren and exited 0, and a build emitted an image of those forms and reported
  success. The JVM raises "Unmatched delimiter: )" and exits 1, and so does jolt
  now, naming the file and the line and column of the delimiter rather than the
  start of the file. Found the way you would want to find it, from a test file
  whose entire body silently did not exist. (#581, jolt-3amm)

- **A `(timeout ms)` closes on its own deadline.** One timer thread serves every
  `(timeout ms)` in the process, and it slept to the nearest deadline with its
  mutex released, which left it off its condition variable exactly when a nearer
  deadline arrived. The wake was dropped and the new timeout did not close until
  the deadline the timer was already sleeping to, so a `(timeout 100)` created
  while a `(timeout 3000)` was pending took 3000ms. Every `alts!` timeout guard,
  every operator built on one, and every fiber parked on one inherited that.

  The same three lines cleared the flag that guards the fork before an idle wait,
  but a timer that finds nothing pending waits rather than exiting, so the next
  timeout both signalled the live thread and forked another one that also never
  exits. 100 sequential `(timeout 1)` calls left 100 live timer threads.
  (#584, jolt-pe84)

- **`with-open` closes a `reify`.** It had an arm for a `deftype` or `defrecord`
  that declares `close` and fell through to "no .close method on value" for a
  `reify` that declares one, even though `(.close x)` on the same value worked.
  It now goes through the shared interface-method lookup that already handles
  both. A connection handed out as a `reify` implementing `java.io.Closeable`,
  which is what `clojure.jdbc` does, could not be used with `with-open` before.
  (#585)

- **`(Class/FIELD)` in call position reads the field** instead of trying to apply
  its value, so `(Math/PI)` and `(Integer/MAX_VALUE)` evaluate to the field as
  they do on Clojure, which reads a parenthesised static member as a field when
  one exists. It holds for a class a library declares too, which is what
  `clojure.jdbc` needs for its `(Locale/US)`. The dot form already resolved this
  ambiguity at runtime; the slash form now gets the same treatment, for a
  zero-argument call whose head is a qualified non-var symbol. A no-arg static
  method is still called, and a zero-arg var call is untouched. (#585)

- **`extend-protocol` on a class a library declared now dispatches.** Registering
  a class made `(class x)` and `instance?` answer for the library's own host
  values, which was half the point; the other half, dispatching a protocol
  extended to that class, never worked. Resolving the extended type name to a
  dispatch tag only recognised classes the runtime models, so a name it did not
  know was filed under the extending namespace, a tag no value can ever carry,
  and the extension silently never fired. A dotted name that names no `deftype`
  is now taken verbatim, which is the tag such a value reports. Simple names are
  untouched. (#585)

### Internal

- **A hung gate case reports itself instead of wedging the run.** `make test` sat
  on the socket poller case for over ninety minutes in one local run and said
  nothing, because the per-case cap needed GNU `timeout` and stock macOS has
  none. Every host has a cap now (`host/chez/cap.sh`, POSIX sh), and the case
  itself runs its workload on a spawned thread while the main thread watches a
  deadline, so a wedge exits 1 naming the round and phase it stopped in. Its old
  claim that an `alts!!` timeout bounded every read was not a bound at all, since
  the bound depended on the same machinery the case exists to stress. (jolt-8tma)

  What made that run wedge is now known, and it is not the timer above: a monitor
  contended by a real thread and by fibers at the same time can deadlock the whole
  process, because `monitor-wait!` parks a fiber inside the critical section that
  guards the monitor's own book-keeping. Filed as jolt-dfuo with a twenty-line
  repro that needs no sockets. The watchdog added here cannot report that one,
  since it takes every thread including the watchdog.

- **`make certify` names the rows it could not finish**, rather than counting
  them, and staleness now ignores rows the JVM oracle had no opinion on, so a
  row that timed out is not evidence that a divergence went away. The
  `(reduce + (eduction (filter odd?) [1 2 3 4 5]))` row is recorded as flaky,
  because the JVM's own answer is unspecified: `CollReduce` is extended to both
  `IReduceInit` and `Iterable`, an Eduction implements both, and which one wins
  falls out of a hash set ordered by identity hash codes. Twelve consecutive
  local runs raise; CI returns 9. (jolt-owjl)

## [0.7.2] - 2026-08-11

A Path answers for the file system it came from, and the java.nio.file shim
values report their classes through the registry every other shim uses.

### Added

- **`Path.getFileSystem` and `FileSystem.getPath`.** `FileSystems/getDefault`
  hands back one file-system object rather than a fresh one per call, so
  `(identical? (FileSystems/getDefault) (.getFileSystem p))` holds the way it
  does on the JVM, and a file system reached through a path can construct paths
  itself. That is the shape migratus uses to filter migration files: it takes
  the file system off a path, asks it for both the candidate path and the glob
  matcher, and matches the two. Thanks to @sundbp (#574).

### Fixed

- **`extend-protocol` on `java.nio.file.Path` now dispatches.** A Path answered
  `instance?` and `(class p)` through arms local to the nio shim rather than
  through the jhost tag registry, and protocol dispatch reads that registry, so
  `(extend-protocol P java.nio.file.Path …)` threw "No method" on a value whose
  own `(class …)` said `java.nio.file.Path`. The registry now carries a row for
  each of the three shims and the tag-local arms are gone, which is also what
  fixes `(class (FileSystems/getDefault))` answering `:object` and
  `(instance? java.nio.file.FileSystem fs)` answering false where the JVM
  answers true. A Path is a Comparable, an Iterable and a Watchable now, as it
  is on the JVM. (#577, jolt-o9xv)

## [0.7.1] - 2026-08-11

Dependency resolution tells the truth about a Maven fetch that failed, retries a
transient one, and stops silently continuing without the dependency.

### Changed

- **Grenadine updated to v0.1.6.** No behaviour change for jolt: the only two
  namespaces jolt consumes, `grenadine.version` and `grenadine.pom`, differ from
  v0.1.5 by license and provenance comment headers alone.

  It is not a plain submodule bump, though. From v0.1.6 Grenadine *generates*
  `basis`, `coordinate`, `expander` and `gitlibs` from pinned upstream sources
  rather than committing them — its own `.gitignore` lists all four — so the git
  tree is an incomplete source tree, and jolt loads Clojure straight off
  `vendor/grenadine/src`. Those four now come from the release's
  checksum-verified `-src.tar.gz` and live in `vendor/grenadine-generated`; the
  alternative was running Grenadine's `yq`-and-`git clone` staging step on every
  build and CI job. `make grenadinecheck` fails if the vendored sources and the
  pinned submodule name different versions.

- **A Maven dependency that cannot be obtained is now a hard error**, matching
  the reference implementation: tools.deps aborts with "Error building
  classpath. The following artifacts could not be resolved:" and exits 1 for a
  missing artifact and an unreachable repository alike (checked against Clojure
  CLI 1.12.5.1654), and jolt already did this for a git dep whose tag does not
  exist. Maven was the outlier — it warned and carried on with the dependency
  missing from the classpath. Every unresolvable dep is named in one message,
  as tools.deps does, rather than one build at a time.

  Unaffected: a jar that downloads fine and carries no jolt-loadable source
  still contributes nothing, quietly. That is a different condition — the
  artifact resolved — and JVM-only jars turn up as transitive deps routinely.
  Only failing to *obtain* an artifact is fatal.

### Fixed

- **"not found" no longer means "something went wrong".** `jolt.mvn-http/fetch`
  answered one bit, so a 404 was indistinguishable from a connection reset, a
  TLS failure, a timeout, a truncated body, a 429 or a 503 — and `jolt.deps`
  reported all of them as `maven dep X V not found`. A fetch now classifies its
  outcome (`:ok` / `:not-found` / `:retryable` / `:failed`), and the message
  distinguishes "not found in any repository" from "could not be fetched:
  <repo> — <reason>", naming the underlying error instead of discarding it.

  This is not hypothetical: the v0.7.0 release run reported
  `org.clojure/spec.alpha 0.5.238 not found` for an artifact that returns HTTP
  200 from Central. The dependency vanished from the classpath, orchard failed
  to load, and the nREPL suite reported it as 55 identical "connect refused"
  errors that never mentioned a dependency.

- **A transient Maven fetch is retried**, three attempts with a short backoff,
  and only for conditions a retry can fix — a 408, a 429, a 5xx, a truncated
  body, or a connect/TLS error. A 404 and a 403 are final and cost exactly one
  attempt. There was no retry at all before, so a single packet-loss event
  failed a whole resolution.

Finishes the fiber backend for `core.async`. A parked `go` process is roughly
5.5x smaller when the compiler can see where it parks, a compute-bound body no
longer starves the fibers queued behind it, and every lock a program can hold —
`locking`, `dosync`, a `delay` being forced, `ReentrantLock` — survives a park
with exclusion intact. Fibers stay opt-in
(`clojure.core.async/*go-backend* :fiber`); the default is still `:thread`.

### Added

- **The cheap park: a `go` body pays for a continuation only where it needs
  one.** A fiber parks by capturing a continuation, which Chez represents as a
  stack segment that stays live for as long as the process is parked. A CPS pass
  in `clojure.core.async` now rewrites the rest of the body into a closure where
  it can see the park, and the channel op stores that closure and switches to the
  scheduler with no capture at all. Measured in one run, same process shape and
  retention: 864 B live / 1,244 B peak RSS rewritten, against 4,762 B / 7,368 B
  captured — 5.5x and 5.9x. Round-trip time is unchanged (903 ns against 858 ns,
  inside the spread).

  The choice is per **park site**, not per body, with the continuation park as
  the runtime fallback. A park the pass cannot see or cannot rewrite — inside a
  called function, inside a `try`, inside a nested `fn`, in an `alts!`, reached
  through `eval` — is left exactly as written and parks the way every park did
  before. So the pass is opportunistic: it can cost a park its cheap
  representation, never its correctness, and nothing about a `go` body needs
  annotating. Jolt keeps the property the JVM's transform gives up: a parking op
  does not have to appear lexically in the body.

- **Preemption is the scheduler.** A `go` body that computes without reaching a
  channel op used to hold its carrier for as long as it ran, and every fiber
  queued behind it waited — fibers cannot migrate, so nothing could rescue them.
  Chez polls an engine timer at procedure calls and loop back edges, so a tight
  arithmetic loop yields like anything else; the quantum is ~0.45 ms.
  `clojure.core.async/*fiber-preempt-ticks*` sets it, read once when the carrier
  pool starts. There is no value that turns preemption off — cooperative-only is
  an unbounded starvation window, so code that wants it asks for a very long
  quantum instead. A fiber inside a blocking foreign call is not running Scheme
  and no timer fires for it.

- **`clojure.core.async/go-monitor`** yields the throwable when a `go` body
  died, and closes when it did not. A body that threw and a body that returned
  nil were otherwise indistinguishable: both close the result channel and hand
  the reader nil, with the condition reported to stderr at best. It answers for
  either backend and for both fiber spawn paths — which backend ran a body
  follows from the caller's `*go-backend*` binding and which spawn path from
  whether the CPS pass could rewrite the body, so a monitor that reported on some
  of those and said "clean" for the rest would be worse than none.

  ```clojure
  (let [g (a/go (throw (ex-info "boom" {})))]
    (a/<!! g)                    ;=> nil, same as a body that returned nil
    (a/<!! (a/go-monitor g)))    ;=> the throwable
  ```

  A `thread` block's channel answers too — it has the same nil ambiguity and
  comes from the same spawn. A channel that is not one of those monitors as nil
  rather than raising.

### Fixed

- **A lock is a lock across a park.** Holding an object monitor, a transaction
  or a `delay` across a `<!` broke exclusion, in each case because ownership sat
  in an OS mutex: `locking` was a `dynamic-wind`, so a park released the monitor
  mid-body and re-took it on resume, and two fibers on one carrier then ran the
  same body at once — undetected, because same-carrier fibers share the carrier's
  identity and the owner check took the reentrant arm. `dosync` lost isolation
  the same way, and `(delay (<!! gate))` ran its body once per forcer that got
  in, so one delay answered two forcers with different values. Those four locks —
  object monitors, `dosync`, `delay`, `java.util.concurrent.locks.ReentrantLock`
  — now carry ownership in a field keyed on the fiber, which survives a context
  switch. You can hold a monitor across a `<!`, park in the middle of a
  transaction, and force a `delay` whose body waits on a channel.

  Underneath, every lock in the runtime routes through one counting wrapper and
  the scheduler refuses to switch a fiber that holds one, re-arming on a short
  retry so the preemption lands just after the region. A gate checks the routing
  rather than documenting it.

- **`require` is safe to call from more than one thread.** Nothing serialized a
  namespace load, so two threads requiring the same namespace both passed the
  loaded check and both ran its top-level forms — every `def` and every
  side effect twice. The mark-before-load that terminates a require cycle made it
  worse across threads: the namespace reads as loaded before its forms run, so
  the second thread's `require` returned having defined nothing. Loads are
  serialized per namespace now, following the JVM's class-initialization
  procedure (JLS 12.4.2) — the same thread re-entering its own in-progress load
  still proceeds, which is what makes a cycle terminate, and unrelated namespaces
  still load in parallel.

  The compiler had to be made safe for that first: the emit session's scratch,
  the per-def cache cells and the hoisted constant pool lived on a process-global
  unit that two emitting threads traded, so a namespace that compiled cleanly
  alone died with `variable _kc$81 is not bound`. They are thread-bound vars now,
  which also unwinds them on a throw, where the old save/restore pair left the
  unit pointing at an abandoned def's collector.

- **`compare-and-set!`, `swap-vals!` and `reset-vals!` name the class the JVM
  names.** They skipped the atom check `swap!`/`reset!` run and reached a record
  accessor, so a non-atom receiver reported jolt's own record type and nil
  stopped reporting as a `NullPointerException` — which a `catch` selecting on it
  stopped matching.

- **A dynamic binding the runtime establishes is not doubled by a park.** Three
  places pushed a binding frame in a `dynamic-wind`'s before thunk — `*agent*`,
  `*compile-files*`, and the loader's file and source-path frame. A park saves
  the fiber's slice with the frame already in it, the escape's unwind pops it,
  and the resume both restores the slice and re-runs the before thunk, so the var
  ended up bound twice and one frame leaked past the end of its extent. Visible
  with an ordinary park; no preemption needed.

## [0.6.9] - 2026-08-10

### Changed

- **`(.-field obj)` raises when the field is absent**, where it used to answer
  nil — so `(.-zz record)` and `(.-nope map)` no longer read as a field that is
  present and set to nil, and a caller testing one no longer silently takes the
  wrong branch. The JVM raises `IllegalArgumentException` there and so does
  jolt now. What still answers is unchanged: a declared `deftype`/`defrecord`
  slot, and the documented map-as-object read where a key the map HAS reads as
  a field. Code that relied on the nil needs a `contains?`/`get` instead.

### Fixed

- **A lookup answers with the element the collection HOLDS**, not the key it was
  probed with, so metadata on the stored element survives. `(#{^{:x 1} [a b]}
  [a b])` handed back the bare probe and lost the metadata; `assoc` replaced a
  stored key with the equal one it was given, `find` minted its entry from the
  probe, and `select-keys` rebuilt through `assoc`. All of them read the
  collection's own entry now.

- **`count`/`seq`/`nth` work on a `CharSequence`**, which `RT.count`/`RT.seq`/
  `RT.nth` reach for on the JVM: a `deftype` presenting a window over a string
  answered its field count rather than its length. `re-matches`/`re-find`/
  `re-seq` accept one too (only `re-matcher` did), and `StringBuilder` is a
  `CharSequence` at all now — it had no supers row and no `count`/`seq`/`nth`/
  `subSequence`.

- **`deref`'s timed arity is a real `IBlockingDeref` cast.** `(deref (delay 7)
  50 :timeout)` returned 7 with the timeout silently dropped, where the JVM
  throws `ClassCastException` because `Delay` is `IDeref` but not
  `IBlockingDeref`; same for an agent. `atom`/`ref`/`var` did throw, but
  reported the class as an opaque `:object` sentinel.

- **A core.async transducer's `:ex-handler` gets the original throwable.** It
  was handed the raw raised condition, so `ex-data`, `ex-message` and the class
  all read nil and the thrown `ex-info`'s data was lost. core.async also joins
  the library conformance manifest, giving `async.ss` a standing regression gate.

- **The runtime's shared side-tables are serialized.** A Chez hashtable is not
  thread-safe, and the metadata table (every `with-meta`) and the variadic
  fixed-arity registry (every variadic closure creation) were written from
  whatever thread the program ran on. The corruption surfaced later as an
  invalid memory reference inside the collector, or a hang — reproducibly, as a
  crash of core.async's own pipeline test, and in builds predating the fibers
  work.

- **Shutdown hooks run** (#571). `Runtime.addShutdownHook` kept a list nothing
  ever read, so `jolt.process`'s `:shutdown` option — `destroy-tree` and
  friends — cleaned up nothing on any exit path, and a `SIGTERM` to a jolt
  program left its child processes running. Hooks now run once per process from
  a single registry, on normal return, on `System/exit`, on an uncaught throw,
  and on `SIGTERM`/`SIGHUP`.

  The signals are taken over only once a hook is actually registered, by a
  thread parked in `sigwait` — Chez runs a `register-signal-handler` handler on
  the main thread at its next safe point, and the two ways a program usually
  waits (a `deref` of a promise, a blocking foreign call) never reach one, so
  that route would have made the process ignore the signal instead of handling
  it. A program with no hooks is untouched and dies on `SIGTERM` as before. Exit
  status is 128+signal, as on the JVM.

- **A blocking stdin read no longer stops the rest of the program.** `read-line`
  and the REPL waited inside Chez's own blocking read, which holds the whole
  Scheme world: a `future` stopped ticking and an agent stopped draining the
  moment the main thread reached a prompt, where a JVM keeps both running. The
  wait now happens in `sleep` and the port is read once it has something. A
  buffered line costs one `char-ready?` and no sleep, so a piped `read-line` loop
  keeps its throughput (1326 -> 1386 ns/line, 1.045x).

- **A subprocess can be interrupted with `^C` again.** Children spawned the
  default way came up with SIGINT set to `SIG_IGN`, so `^C` in a terminal killed
  your program and left the subprocess running. That is the `system(3)` leak the
  convention exists to avoid rather than the convention itself — a child's
  dispositions should match what a plain shell would give it, as they do on the
  JVM. `posix_spawn` now drives every spawn rather than only the ones redirecting
  a stream to `:inherit` (it already piped every other stream); Chez's fork,
  which is where the ignore was set, stays as the fallback for platforms without
  the FFI surface.

- **A child process no longer inherits jolt's signal mask.** jolt blocks SIGINT
  in its own threads so `^C` reaches the one parked for it, and now blocks
  SIGTERM/SIGHUP for the shutdown watcher; both travelled to every child through
  `posix_spawn`'s null attributes and through Chez's fork. A child that starts
  with SIGTERM blocked survives the `destroy` a shutdown hook calls, and one with
  SIGINT blocked ignores `^C`. The mask is emptied for the length of a spawn and
  put back, so a child gets the default mask the JVM would give it.

### Added

- **Fibers R8: a socket read parks the fiber instead of pinning its carrier.**
  `jolt.socket` runs its syscalls with the fd in `O_NONBLOCK`; on `EAGAIN` it
  registers readiness with the new `jolt.io-poller` and parks the fiber, so the
  carrier runs other fibers meanwhile, and blocks on a private
  `kevent`/`epoll_wait` exactly as before when there is no fiber. User code keeps
  its blocking shape either way. File offload is not in this round.

- **Fibers R5: a carrier pool, and `<!!` parks on a fiber.** The scheduler state
  R4 kept in globals moved into a per-carrier record — run queue, mutex,
  condition, resume continuation, dynamic slice, thread, stop flag — since R4's
  single global resume continuation stops being safe the moment there is more
  than one carrier. Carriers default to the processor count and are overridable;
  placement is round-robin at spawn and never revisited, because a fiber cannot
  migrate.

- **Fibers R4: `go` on fibers, opt-in.** A `go` body can run on a fiber.
  `clojure.core.async/*go-backend*` selects it (`:thread` default, `:fiber` to
  opt in), read at spawn time off the dynamic binding, so a `binding` covers
  every `go` in its scope including ones inside functions it calls.
  `thread`/`thread-call` stay real OS threads regardless — that is the documented
  escape for blocking work, which would otherwise pin a carrier. The default is
  unchanged.

- **Fibers R3 (jolt-nvpr.4): one waiter protocol for threads and fibers.** A
  pending channel operation is a handler, never a fork. The alt-taker/alt-putter
  records in `host/chez/java/async.ss` — the list machinery `ac-notify!` already
  drains under the channel mutex — gain a `wake` field, and `alt-deliver!`
  dispatches on it: a thread-waiter is woken by its condvar exactly as before, a
  fiber-waiter by enqueuing the fiber on its carrier's run queue
  (`sa-fiber-resume`, safe cross-thread now that the run queue in
  `host/chez/fibers.ss` has a mutex). The channel core never learns what a fiber
  is: a fiber's `< !` registers as an alt-taker, its `>!` as an alt-putter. The
  new bridge `host/chez/java/fibers-async.ss` defines `jolt-fiber-<!` /
  `jolt-fiber->!`, the Scheme-level primitives R4's `go` will drive: the
  immediate-completion path takes a buffered value or drains a waiting putter
  under the channel mutex with no continuation capture, and the park path
  registers the handler, releases the channel mutex, then suspends — the commit
  decision (park vs. already-delivered) happens inside the handler mutex, so a
  delivery racing the release can never be lost. A parked fiber taker counts as
  a waiting taker for the unbuffered readiness check in `ac-try-give!/locked`,
  so `offer!`/`put!` complete against it instead of forking a thread. Lock order
  stays channel mu → handler mu → run-queue mu; "never yield while holding the
  channel mutex" is now stated in async.ss's lock-order comment. The new `make
  fibers` gate (`fibers-chan-test.ss`, 47 checks) covers thread/fiber handoff in
  both directions, buffered and unbuffered, a thread-put waking a parked fiber
  and a fiber-put waking a parked thread, no-capture immediate completion (the
  park counter stays at zero; 81ns vs 1775ns per take), N×M exactly-once
  delivery, close! waking both waiter kinds, a fiber parking while a sibling on
  the same carrier puts, and a pumping-thread stress drain. The `thread-sleep`
  helpers in async.ss moved to Chez `sleep`.

- **Fibers R2 (jolt-nvpr.3): per-fiber dynamic state.** A fiber's `binding`
  frames, current namespace, and STM `*txn*` now travel with the fiber instead
  of the carrier. R0 pinned the bug: dyn-binding.ss pushes a binding frame by
  calling a thread parameter as a setter, and a setter write survives a
  continuation escape — a fiber parked inside a `binding` leaked its frames
  onto the carrier, visible to the scheduler and every other fiber, and a
  second fiber's pop could pop the parked fiber's. The fix lives in the
  scheduler: each fiber carries a dynamic slice (`host/chez/fibers.ss`,
  `jolt-dslice`), saved on switch-out and restored on switch-in, with the
  carrier reverting to the caller's state between fibers. Writes are diffed
  with eq? (a thread-parameter write is 33ns vs 2ns to read), so identical
  slices cost reads only; the switch ratio in `make fibers` moved from ~22x to
  ~27x a bare procedure call (ceiling 60x). `sa-fiber-spawn` conveys the
  parent's bindings and namespace but never its `*txn*` (async-go-spawn
  parity), so a child spawned inside a dosync cannot join the parent's
  transaction. `dyn-binding.ss` is untouched. The new `make fibers` gate
  (`fibers-state-test.ss`) asserts the six R2 scenarios: binding invisibility
  between siblings, the parked-frame leak regression, bindings intact on
  resume, transaction isolation across two fibers on one carrier, spawn
  conveyance, and namespace-follows-fiber.

- **Fibers R1 (jolt-nvpr.2): the fiber primitive and a single-carrier
  scheduler** behind the new `coroutines` CONTRACT.txt tier (`sa-fiber-spawn`,
  `sa-fiber-yield`, `sa-fiber-resume`, `sa-fiber-run-all`), in
  `host/chez/fibers.ss`. Per R0's measurements: the per-fiber slice rides in
  one virtual register (2ns vs 33ns per thread-parameter write), the run queue
  is intrusive (the next link lives in the fiber record), and exceptions are
  isolated per fiber (a raise kills the fiber, never the scheduler loop).
  The Chez gate (`make fibers`, in CI) asserts correctness (round trip,
  completion, raise isolation, 8-fiber round-robin, yield from 40 frames deep)
  and the pinned numbers: spawn 0.04us (< 5us), switch 50ns (< 100ns),
  3.7KB live per parked fiber (< 8KB, by the R0-corrected absolute-live-bytes
  measurement). Gambit satisfies the tier with a call/cc-based scheduler
  (verified under native gsi in gambitcheck); `go` stays on OS threads there
  per the plan's documented degradation. No jolt-level `go` or channel code
  yet — those are later rounds.

- **Gambit build profiles**, so a bundle carries only the language a build
  needs. The Gambit runtime plus jolt's kernel is two thirds of the bundle and
  cannot be dropped; what is separable is regex, the compiler, and
  `clojure.core` itself, so the profiles trade features for the last third —
  `repl` ships 27.7MB / 3.1MB gzipped against `full`'s 32.6MB / 3.5MB.
  `make gambitweb` builds the browser bundle, and `host/gambit/host-vars.ss`
  binds the 40 `clojure.core` names the `java/` tree owns on Chez (`(time 1)`
  reached an unbound global and failed as a bare Gambit exception); each is
  either implemented or raises a catchable `UnsupportedOperationException` that
  names itself.

- **Gambit is a second Scheme backend** — the first target ported through the
  portable Scheme layer (#446). Jolt reads, compiles, and evaluates source on
  native Gambit, and the whole stack compiles to a single JavaScript file via
  `gsc -target js` that boots in about a second in a browser (the live REPL on
  jolt-lang.github.io). `host/gambit/` holds the adapter, prelude shims, hash
  kernel, and a target-owned runtime kernel; the seed and the expansion-hostile
  macros are generated on Chez (`make gambitseed`) rather than hand-written.
  Three detection-gated targets hold the boundary: `make gambitcheck` (adapter
  and shims), `make gambitkernel` (the booted kernel, 113 checks), and
  `make gambiteval` (jolt source through the compiler, renders pinned to Chez
  captures). Nothing on the Chez path changed.

  The backend is demo-grade: FFI, AOT compilation, and program images raise,
  the concurrency tier is stubbed, and only checked primitives are emitted.
  `jolt build` binaries remain Chez-only. See "Scheme Backends" in the docs for
  the contract a new target must satisfy.

## [0.6.8] - 2026-08-07

### Added

- **Variadic FFI** (#551): a `:varargs` marker in a `defcfn` argtype vector
  declares the binding variadic and marks the fixed/variadic boundary — types
  before it are the named parameters, types after it the concrete variadic
  arguments. Calls emit Chez's `(__varargs_after n)` convention so variadic
  arguments travel where the callee's `va_list` reads them; on Apple arm64
  they ride the caller's stack, and a fixed-arity binding silently corrupts
  them (fcntl's F_SETFL flags never land). Marker-first, marker-last, and
  `:varargs` combined with `:blocking` reject at compile time with messages
  naming the rule.

## [0.6.7] - 2026-08-06

### Added

- **Portable Scheme layer completed** (#446, rounds R7–R10). The FFI tier,
  eval/compile/AOT, and the backend's unsafe-primitive and FFI emission all
  route through the scheme-adapter now; the lint allowlist is structurally
  confined to the two target-owned files. `host/scheme-adapter/` gains
  `TARGET-CONTRACT.md` (the porting document) and `guile.ss` (a structural
  stub a port starts from); design notes are RFC 0010 on the site. No
  behavior change on Chez — the seed prelude is byte-identical and the hot
  FFI paths measured within noise.

### Changed

- **State image format version 3**: refs now travel in `jolt.image` dumps by
  value (a descriptor is written; restore re-mints live refs — identity,
  cycles, metadata, and STM liveness all preserved). This build still reads
  version-2 images, including ones holding refs; runtimes at 0.6.6 or older
  refuse version-3 images with a clean version error. Dropping the ref
  record's dead lock field also makes `(ref x)` cheaper: the STM microbench
  runs ~1.17x faster and a ref-heavy image round-trip ~1.2x faster.

### Fixed

- `jolt.image/resolve-stub!` now replaces a stub held inside a ref's value;
  the substitution walk previously passed refs through untouched.

## [0.6.6] - 2026-08-06

### Added

- **Portability groundwork for #446** — a general Scheme layer with the
  Chez-specific bits isolated. Every Chez-only identifier the host uses is
  now either in the documented adapter contract (`host/scheme-adapter/`) or
  routed through `sa-*` capability entry points
  (`host/chez/scheme-adapter-runtime.ss`), enforced by new ci gates
  (`portcheck`, `adaptercheck`, `degradedbacktrace`). Capability tiers
  (threads / ffi / introspect / native-compile) make a WASM-class target
  definable; the threads-tier audit behind this produced the concurrency
  fixes above. No user-facing behavior change otherwise.

- **`jolt.socket`: `java.net.Socket` / `ServerSocket` / `InetSocketAddress` /
  `InetAddress` over POSIX sockets** (`(require 'jolt.socket)` registers the
  classes; IPv4, blocking I/O). Contributed by @allen-munsch in #542,
  hardened in #543: writes to a peer-closed socket throw `IOException`
  instead of silently dropping (SIGPIPE guarded via
  `MSG_NOSIGNAL`/`SO_NOSIGPIPE`), `ServerSocket` binds the wildcard address
  like Java (port 0 + `getLocalPort` report the kernel-assigned port via
  `getsockname`), accepted sockets know their peer,
  `class`/`instance?`/`str` answer as the mirrored classes, and
  `InetAddress/getByName` resolves. Deliberate gaps are in
  `known-divergences.edn`: `available()` is 0, a recv error reads as EOF,
  connect timeouts are ignored.

### Fixed

- The install script honors `PREFIX` (`PREFIX=~/.local bash install` puts the
  binary in `~/.local/bin`) and, when neither `PREFIX` nor `--dir` is given,
  defaults to `~/.local/bin` for non-root users instead of failing on
  `/usr/local/bin` with permission denied. Root keeps `/usr/local/bin`.
- **Monitors are reentrant per thread, like JVM intrinsic locks.** A nested
  `(locking x ...)` on the same object from the same thread deadlocked, and
  a non-owner `monitor-exit` silently released someone else's lock instead
  of throwing `IllegalMonitorStateException`. (#446 threads audit)
- **Executor pool workers no longer inherit an in-flight transaction.** A
  pool created inside a `dosync` handed its workers the live transaction,
  so a job's `ref-set` outside any transaction silently wrote into the dead
  transaction log instead of throwing `IllegalStateException`.
- **Atomic `updateAndGet`/`getAndUpdate` run the update fn lock-free** in a
  CAS retry loop like the JVM; a fn that touched its own atomic deadlocked.
- **`await`/`await-for` on a failed agent throw** `"Agent is failed, needs
  restart"` instead of returning normally. Entering the wait on an
  already-failed agent matches the JVM exactly; an agent failing mid-wait
  throws where the JVM blocks forever (deliberate; `known-divergences.edn`).

- **`ProcessBuilder$Redirect/INHERIT` inherits the real file descriptors.**
  A spawn with any INHERIT stream goes through `posix_spawn`: the child sees
  the actual fd (`isatty` answers truthfully, stdin's read offset is shared
  between children), pipes are built only for the streams that ask for one,
  and the API returns null streams for the inherited ends like the JVM. Pump
  emulation remains as the fallback where that FFI surface is unavailable.
  (#541)
- **A top-level macro call expanding to `do` unrolls into top-level forms**,
  per Clojure's compilation-unit rule: each child compiles and evaluates
  before the next, so a macro emitting `(do (require ...) (deftest ...))`
  has the require in force when the `deftest` compiles. Previously only a
  literal `(do ...)` unrolled; babashka.process's own suite loads exactly
  this way.

## [0.6.5] - 2026-08-05

Images now carry the two things 0.6.3 refused: anonymous functions and
sorted collections. Open resources stub instead of refusing. Image format
is now v2 — older runtimes refuse a v2 image with the reason named. (#539)

### Added

- **Anonymous functions travel in images.** A `fn` literal is compiled under
  a stable name with its source form and free names registered at load time;
  the dump records the source plus the captured values (recovered live from
  the closure), and restore rebuilds a callable closure in the defining
  namespace. Captures optimized into the compiled code refuse at dump,
  naming the local — never a closure that silently computes with `nil`.
- **Sorted maps and sets travel in images**, restored through the public
  constructors with the original comparator (natural or user-supplied).
- **Open resources dump as resolvable stubs.** `dump-world!` stubs
  unwritable values (ports, threads, sockets) by default and reports them
  under `:stubbed`; `dump!` stays strict unless `{:unwritable :stub}`.
  After restore, `jolt.image/stubs` lists what needs rebuilding, inert until
  `resolve-stub!` replaces one; `register-stub-describer!` /
  `register-stub-resolver!` extend both ends. `scan` reports each value's
  `:disposition` so the split is visible before writing.

### Fixed

- Timed `(deref ref timeout-ms timeout-val)` on a record or reify
  implementing `IBlockingDeref` reaches the 3-arity method instead of
  silently blocking on the one-arity; a `deref` method existing only at the
  other arity throws the JVM's `ClassCastException` naming the interface.
  (#537, #540, fixes #538)
- `realized?` dispatches to a record or reify `isRealized` method
  (`clojure.lang.IPending`). (#540)

### Conformance

- mount (application state lifecycle) passes its full suite — 21 tests,
  131 assertions, matching JVM Clojure exactly — and joins the
  libconformance fleet. (#540)

## [0.6.4] - 2026-08-05

### Changed

- Vendored grenadine updated to v0.1.5. The effective-POM reader now handles
  Unicode in Maven XML and legacy Maven dependency forms, widening the set of
  POMs `jolt.deps` can resolve transitively. (#536)

## [0.6.3] - 2026-08-05

Adds `jolt.image` — write a running program's state to a file and restore it
in a fresh process, on another machine or another CPU architecture. State
travels, execution does not: no thread stacks, no continuations. Design in
RFC 0009; a working GUI example lives in jolt-lang/examples
(`image-dump-example`). (#533, #534)

### Added

- **`jolt.image/dump-world!` / `restore-world!` — save the program, not a
  variable.** Walks the var table and writes every var's root, so nothing in
  an application lists what its state consists of; a new `def` is in the
  image without touching the saving code. Code does not travel — a var whose
  root is a function is skipped, since the restoring process is the same
  build and already has it. `clojure.*`/`jolt.*` vars stay with the process
  being restored into; `user` is kept. `add-before-dump-hook!` /
  `add-after-restore-hook!` bracket the pair (quiesce on the way out, rebuild
  derived state on the way in), and `restore-world!` returns the number of
  vars rebound.
- **`jolt.image/dump!` / `read-image`** — the same machinery for a single
  value you name.
- **`scan` / `scan-world` / `dumpable?`** — dry runs that name the route
  through the graph to anything unwritable
  (`#'app.core/state -> :handlers -> "GET /x" -> #<procedure>`) instead of
  writing a subtly incomplete image. `dump!` refuses with the same path.
- **`register-handler!`** — teach the encoder a resource: a predicate, a
  fn that renders it as plain data, and a fn that re-acquires it on restore.
  Claimed at var roots, so a handler payload is ordinary state (functions
  become names, keywords re-intern).
- **Cross-architecture images.** The body is a machine-independent stream by
  construction — code travels by name, never as code objects — so an image
  written on arm64 reads on x86-64. Structural sharing, cycles, records,
  metadata, and every numeric type round-trip; interned keywords are
  re-interned on the way in, so restored maps look up correctly. The header
  pins the runtime: an image survives a machine or architecture change but
  not a Chez upgrade, and a mismatch is refused with the reason named.
- Not writable, by design: anonymous closures (store a named fn, or data to
  rebuild one from) and sorted maps/sets. Both are refused with a clear
  message, never silently dropped.

### Internal

- New `stateimage` gate (in `make ci`): pins the value/world round-trips and
  the Chez fasl behaviour the format assumes — what fasls, what is refused,
  machine-independence of data-only streams — so a Chez upgrade fails a test
  rather than someone's image.

## [0.6.2] - 2026-08-04

Rebuilds tracing on compile-time metadata, removing the per-entry ring push
that 0.6.1 left in place — tracing is now effectively free and on by default
everywhere, including built binaries. (#531, #532)

### Changed

- **Tracing is ~free, in dev mode and in built binaries.** The tail-frame
  ring, the per-entry push, the per-call line store, and the tail mark are all
  gone. The only runtime instrumentation left is one virtual-register store of
  a static `(fn . line)` pair at tail call sites; every call site additionally
  registers its static callee in load-time tables, and the reporter
  RECONSTRUCTS TCO-erased chains from those tables at throw time — backward
  from the throw-site pair through unambiguous tail edges, forward between
  live frames through unique tail-exit paths. Reconstruction can be
  incomplete (a fork in the static call graph stops it; a mutual-recursion
  cycle renders once, not per iteration) but never invented. Dev mode against
  `JOLT_TRACE=0` on the same binary: `fib` 7.1 → 0.9 ms (floor 0.7; was ~10×),
  `tak` at the floor (was ~9×), `binary-trees` 70.1 vs floor 68.0 (was 1.6×),
  `arrays`/`mathfns`/`loop-recur`/`mandelbrot` at floor parity. A zipper-heavy
  library test suite (rewrite-clj, ~10⁸ delegation tail calls) runs traced at
  untraced speed. An intermediate continuation-marks design was measured and
  rejected: Chez marks ballooned the heap to tens of GB at that call volume.
- **`jolt build` bakes tracing in by default.** A deployed binary's uncaught
  error names TCO-erased frames with exact lines — the site literals and
  tables carry everything; no source or marker files needed at run time.
  `JOLT_TRACE=0 jolt build` produces an untraced binary (build-time axis; the
  baked binary has no runtime knob). Built benches, traced vs untraced
  builds: `tak`/`binary-trees`/`arrays`/`mathfns`/`loop-recur`/`mandelbrot`
  within noise; `fib:30` 9.5 vs 7.7 ms (~0.6 ns/call — the tail-site store on
  tail-position arithmetic, which can throw and so must be recorded).

### Added

- **Host faults get real traces.** A raw Chez condition (a bad
  primitive-array index) has its site captured pre-unwind — in the cli run
  path and in built binaries' launchers — so the trace names the faulting fn
  and line. These previously fell back to a bounded history window, or
  nothing.
- **Erased frames recover at every spine level.** Forward gap-filling names
  the TCO-erased fns between any two live frames when the static call graph
  is unambiguous there — the old ring only ever recovered the innermost
  chain.

### Fixed

- An explicit `throw` in tail position reported its fn's definition line
  instead of the throw line.
- A trace could show a caller above its callee when the throw left live
  frames below a recovered chain.

## [0.6.1] - 2026-08-04

Undoes a dev-mode performance regression that shipped in 0.5.20 and 0.6.0, and
takes backtraces off the tail-frame history ring and onto the live continuation,
which is what made the regression unnecessary in the first place. Built binaries
were never affected in any release — they compile with tracing off — so this is
entirely about `jolt run` / `-M:alias`.

### Fixed

- **Up to 19× slower numeric code on the `jolt run` path (0.5.20, 0.6.0).** The
  tail-frame backtrace fix in 0.5.20 paired a history-ring save/restore around
  every non-tail call. It was applied downstream of every branch of the call
  emitter, so a call the inference had already reduced to a single machine
  instruction got wrapped too — a proven `(aget ^doubles a ^long i)` became

  ```scheme
  (let ((_tu$ (jolt-trace-save)))
    (let ((_tr$ (flvector-ref (jolt-array-vec a) i))) (jolt-trace-unwind! _tu$) _tr$))
  ```

  The `let` is the costly half: the restore runs after the call, so an unboxed
  flonum is held across it, lands on the heap, and takes the surrounding `fl+`
  chain with it. The cost tracked how well the inference had done — 19× on a
  flonum array loop, 1.9× on `vector conj`, 1.05× on lazy seqs. A proven
  `aget`/`aset` loop measured 10.4 ms on 0.5.19, 206.1 ms on 0.6.0, and 10.4 ms
  here; every phase of a real image pipeline paid 4.5–8.6× and is likewise back to
  its 0.5.19 timing. Tracing itself still costs (~10× on a `fib` microbenchmark
  against `JOLT_TRACE=0`) — that is the per-entry ring push, is unchanged by this
  release, and is why `JOLT_TRACE=0` exists.

- **Deep recursion lost the outermost frames of a backtrace.** The history ring
  holds 64 subproblems, so a 90-deep non-tail recursion reported `down (x64)` and
  dropped `-main` entirely. The spine now comes from the live continuation, which
  has no such bound, and the trace reports the true depth with its caller. This is
  the same failure the 0.5.20 fix set out to address; that fix covered calls that
  had already *returned*, never depth.

- **The AOT cache served artifacts across trace modes.** Whether tracing is on
  changes the emitted code, so a cached fasl is only valid for the mode that
  produced it, and the cache generation did not record which. A traced run that
  reused untraced artifacts silently reported **no** backtrace frames at all — the
  feature switched off by a cache hit. The generation is now keyed on trace mode.

- **Cached namespaces recorded a source path that no longer existed.** The cache
  compiled the emitted Scheme from a pid-unique temp and renamed it away, so
  `compile-file` baked the temp's name into every frame's source object. Nothing
  read that path before this release; the continuation-based backtrace does. The
  `.scm` is now published at its final name — still via temp + atomic rename, so a
  concurrent compiler never sees a half-written file — and compiled from there.
  `clojure.core/compile`'s artifact writer had the identical shape and is fixed
  alongside.

### Changed

- **Eval-path frames carry source objects.** Runtime-compiled code (an AOT cache
  miss) was a transient string: by the time anything threw, the text was gone, so
  a frame from that code could not point back into it. The eval path now reads
  each emitted form with `get-datum/annotations` under a synthetic source name and
  registers that text's `#|L<line>|#` markers under the same name first, so a
  frame's `(source-name . offset)` resolves to the original clj line even though
  the text itself is gone. Gated on tracing — with tracing off the path is the old
  plain `read`+`eval`, verbatim. Traces are unchanged; `JOLT_DEBUG_FRAMES=1` shows
  the resolved line per frame (`source=jolt-eval-src-1@278 -> clj:L4`).

- **Backtraces are rendered from the live continuation.** The reporter read the
  whole trace off the tail-frame history ring, with the continuation only as a
  fallback. That is backwards: the continuation *is* the stack, it is exact, and
  reading it costs nothing per call (the `call/cc` is paid at the throw). Each
  frame's clj line now comes from a `#|L<line>|#` marker the emitter leaves beside
  every traced call site, resolved through the frame's source object. The ring is
  consulted only for frames tail-call optimisation erased, which is the one thing
  the stack genuinely cannot hold. Trace output is unchanged apart from the
  deep-recursion fix above.

## [0.6.0] - 2026-08-03

Running third-party suites through each library's *own* runner, instead of a
bespoke harness that hand-listed namespaces and supplied its own deps, reached
code paths that had never executed. Most of this release is what that surfaced.

The largest single find was protocol identity. Dispatch keyed a method table by
the protocol's SIMPLE name, so two protocols named alike in different namespaces
shared one table and the later `extend` silently replaced the earlier one — a
wrong answer, not a crash.

### Added

- **`proxy` extends a concrete class by delegation.** It used to desugar to
  `reify`, which has no base, so a proxy over a concrete class answered only the
  methods its body declared and `proxy-super` threw unconditionally. jolt
  generates no classes, so a proxy now constructs a real base instance, answers
  what the body declares, forwards the rest, and `proxy-super` calls the base's
  own implementation. A proxy is an instance of its base and reports its class
  and host tags. A super naming an interface has nothing to construct and stays
  the reify it was.

  Delegation is not subclassing in one respect, recorded in
  `known-divergences.edn`: the base holds no reference back, so a base method
  calling an overridden method runs the base's version where the JVM re-enters
  the override. Overriding a leaf method — the common case — is identical.

- **`java.io.PrintStream`, and `System/setOut` / `System/setErr`.** Redirecting
  the process streams was previously withheld for want of the proxy support
  above.

- **A `URLStreamHandler` decides what its URL reads.** `openConnection` is the
  handler's, `openStream` is that connection's `getInputStream`, and `slurp` and
  `io/reader` route the same way. The constructors are now told apart by
  argument type the way the JVM's overloads are, so `(URL. context spec handler)`
  works.

- **A `deftype` or `reify` declaring `Iterable` or `Iterator` is seqable**, as on
  the JVM, walked lazily. `Seqable` wins over `Iterable` for a type declaring
  both, matching `RT.seqFrom`.

- **`clojure.test` reports like reference Clojure** — the blank line,
  `FAIL in (test-name) (file:line)`, the testing context and message, then
  `expected:` / `  actual:`. Every editor integration, CI parser and third-party
  reporter keys off that shape, and test.check's own suite asserts on it.
  Position comes from the reader rather than a stack walk. `*report-counters*`
  is incremented too.

- **`clojure.stacktrace`**, which Clojure ships and jolt did not. Frame lists are
  empty here — tail calls, already a recorded divergence — but the throwable
  line, `ex-data` and cause chain match exactly.

- `java.lang.reflect.Array` (`newInstance`/`getLength`/`get`/`set`),
  `Void/TYPE`, `System/arraycopy` with its overlapping-copy semantics,
  `Character/codePointAt`, `Murmur3/hashCombine`, `clojure.datafy`,
  `clojure.java.javadoc`, and Runtime's memory API.

### Fixed

- **Protocol dispatch keys by defining namespace**, not simple name. Resolved
  through `:refer` and `:as`; a symbol naming no protocol keeps its bare name,
  because that is how `value-host-tags` spells the tags a value reports.
- **`(. Class MEMBER)` reads a static field** when one is registered. It always
  emitted a static call, so a field's value was applied as a zero-arg procedure
  and came back **nil** — a silent wrong answer. `(. Class -MEMBER)` was rejected
  outright.
- **`java.io.File` compares by pathname under `=`.** `.equals` and `hash` already
  agreed, so two Files built from one path were unequal only through `=`. The
  two-arg constructor also joins with exactly one separator.
- **`println` and `prn` flush when `*flush-on-newline*`**, and
  `clojure.core/flush` dispatches to the writer `*out*` holds rather than only
  flushing the Chez port. The var existed and defaulted true with nothing
  reading it, so text sat in a writer's buffer.
- **`OutputStreamWriter` leaves the stream it wraps open**, and its flush reaches
  it. Transcoding the stream's port closes that port under R6RS, so
  `(.toString baos)` after wrapping one failed.
- `getPath` / `getFile` on a URL are the path component, not the spec minus a
  `file:` prefix; `io/reader` on a URL no longer reads the spec as a local path.
- A `reify` declaring `IFn` is `ifn?` and an instance of it.
- iconv issues the POSIX reset call, so a stateful charset emits its closing
  escape.
- `keys` / `vals` throw `ClassCastException` on a non-entry element.
- Accepts the sha an annotated tag carries as well as the commit it peels to,
  and the legacy `:sha` / `:tag` spellings.

### Changed

- **`(char n)` spans the Unicode scalar values.** jolt's strings are code-point
  indexed, so it could not rebuild a char `(first s)` had just handed out. This
  is a deliberate superset of the JVM's 16-bit char; the clojure-test-suite
  already treats that assertion as host-dependent, and jank and basilisp behave
  as jolt now does. Recorded in `known-divergences.edn`. Surrogates stay
  rejected.
- **An incomplete `make test` run can no longer read as a pass.** The gate runs
  as a sub-make behind a wrapper: a verdict line is printed either way, so the
  log can never end on a passing target — which also covers `make test | tail`.
  A complete pass writes a receipt naming the tree it covers, and `make
  gate-status` answers whether the working tree is gated. `-i` and `-k` are
  refused for gate targets.

### Library conformance

| library | before | after |
| --- | --- | --- |
| malli | 121 pass, 20 load-fail | **12,059** pass, 8 load-fail |
| honeysql | 832 pass | **2,842** pass |
| ring-core | 405 pass, 15 fail, 18 error | **446**, **5**, **5** |
| tick | 620 pass, 1 load-fail | **723**, 0 |
| Selmer | 525 pass, 1 error | **526**, **0** |
| tools.logging | 219 pass, 4 error | **226**, **0** |
| test.check | 230 pass, 15 fail | **236**, **10** |
| test.chuck | 98 pass, 22 fail | **110**, **17** |
| data.json | 320 pass, 2 error | **322**, **0** |
| markdown-clj | 180 pass, 2 error | **182**, **0** |
| data.codec | 1 pass, 11 error | **12**, **0** |
| hiccup | 385 pass, 1 fail | 386, **0** |
| rewrite-clj | 3,380 pass | 3,381 pass |

malli's fail and error counts also rise, because seven namespaces that used to
load-fail now run at all. Its residue is characterised in the manifest: 72
errors are sci, which does not fully load here (it reaches private
`clojure.core` internals by var), and 104 of the failures are one test that
round-trips values drawn with a fixed seed — a seed draws different values here
than on the JVM.

ring-core's multipart suite runs against jolt-lang/multipart, an RFC 7578
parser, through a shim registering the commons-fileupload2 class surface ring
reaches for. The shim is glue: it decides nothing about multipart syntax, which
is what keeps the suite worth running.


## [0.5.20] - 2026-08-02

A backtrace could show frames from calls that had already finished, and in the
common shape it pushed the frame you needed out of the trace entirely to make
room for them.

### Fixed

- **A backtrace no longer lists calls that already returned.** The tail-frame
  history keeps one entry per call reached, and its ring only ever moved forward
  — so a call that returned kept its place, and its frames printed under the ones
  that actually threw. Worse, once the ring filled they evicted the real caller.
  A `-main` that prints a value and then throws looked like this:

  ```
  Unhandled exception: Divide by zero
    trace:
      app/inner (src/app.clj:3)
      app/outer (src/app.clj:4)
      jolt.time.impl/type-of (jolt/time/impl.clj:7)     <- from the println,
      jolt.time.impl/jt? (jolt/time/impl.clj:78)           which had already
      ... 12 more of the same                              returned
  ```

  and now names the caller it was dropping:

  ```
  Unhandled exception: Divide by zero
    trace:
      app/inner (src/app.clj:3)
      app/outer (src/app.clj:4)
      app/-main (src/app.clj:7)
  ```

  The compiler now pairs a save/restore of the ring position around every
  non-tail call, so a returned call's entries are reused by whatever the caller
  does next. Tail calls are deliberately left alone: consuming their result to
  run the restore would make them non-tail and defeat tail-call elimination,
  which is the reason the history exists at all. This costs 1.9–3.1x on
  `jolt run` and **nothing** in a built binary, whose frame prologues are baked
  at build time with tracing off.

  One case still leaves residue: frames pushed by library code that the runtime
  itself called into (a type registering per-value `str`/`compare` arms, say)
  have no wrapped call site to unwind them. Those now sit below a complete
  spine rather than on top of a truncated one.

### Changed

- **Registering an arm whose predicate claims a runtime-owned type now fails
  loudly.** `=`, `hash`, `get`, `count`, `contains?`, `empty?`, `seq` and
  `compare` each answer their commonest types before consulting the registries a
  host type extends them through, so an arm matching one of those was silently
  skipped and its handler never ran. Registration now probes the candidate
  predicate and rejects it, naming the type. A shim that registered, say,
  `string?` for `count` fails at registration instead of being quietly ignored
  at every later call. Each registry checks only its **own** fast path — `get`
  answers records but not strings, `count` the reverse — so an arm that is legal
  for one and not another is still accepted where it belongs.

### Performance

- **`print`, `println` and `pr` resolve `*out*`, `*print-readably*` and
  `*print-namespace-maps*` through cached var cells** instead of rebuilding
  `"clojure.core/<name>"` and hashing it once per value printed. A/B/A in one
  session over a print-saturated workload, two runs: 3.5% and 5.5% faster, with
  drift of 0.15% and 0.33% between the A columns. Real programs print far less
  than that benchmark, so expect less. Reads still go through the binding stack,
  so `with-out-str` and `(binding [*print-readably* nil] …)` are unaffected.

### Internal

- Dependency-tree expansion moved out of `jolt.deps` into the vendored
  [Grenadine](https://github.com/clojurestar/grenadine) library, which now owns
  the tools.deps-compatible traversal, exclusion handling and version selection
  alongside the effective-POM builder and version comparator it already
  provided. Behaviour is unchanged — `-Stree`, `-Spath` and the resolution
  warnings all render identically.

## [0.5.19] - 2026-08-02

`print` was implemented as string conversion rather than as printing, so it
disagreed with `pr` on six types: a BigDecimal lost its `M`, a regex printed as
bare source, a UUID as bare hex, the infinities as `Infinity`/`NaN`, a bigint
lost its `N`, and a char nested inside a collection kept the backslash that
`print` is supposed to drop. On the JVM `print` is `pr` with `*print-readably*`
off, and that flag changes only strings and chars.

### Changed

- **`print`/`println`/`print-str` now render like `pr` with quoting off, not like
  `str`.** Previously `print` of a `2M` printed `2`, a regex printed `re`, and a
  UUID printed the bare hex string. It now shares the readable renderer with `pr`
  and only drops the string/char quoting, so those print as `2M`, `#"re"` and
  `#uuid "…"`, and a mixed coll prints `[s a 2M]` instead of `[s \a 2M]` — the
  JVM's `(binding [*print-readably* nil] (pr …))`. `str` is untouched (its
  contract is `.toString`). The quoting-off flag rides a virtual register rather
  than a per-value dynamic binding, which would have cost about 770ns a value.

### Performance

- **`pr`, `pr-str` and `print` skip the printer's arm registries for the value
  types the runtime owns** — numbers, strings, chars, keywords, symbols, booleans
  and nil now go straight to the renderer's base case instead of testing ~40
  registered host-type predicates that cannot match them, and `print-method` is
  resolved through a cached var cell instead of being looked up by name per
  value. Measured over 200k values, A/B/A in one session: `pr-str` 220 ms → 162 ms
  (1.36x), `print` 235 ms → 224 ms (parity — the delta is inside run-to-run
  drift). A registered arm that could match one of those types is rejected at
  registration, so the fast path cannot silently bypass a host shim's rendering.

## [0.5.18] - 2026-08-02

Two things a program cannot work without: knowing where it broke, and getting
randomness that is actually random. An uncaught error printed no location at all
for the ordinary shape — a `-main` that tail-calls the function that throws — and
`.printStackTrace` did not exist on the value a `catch` binds. Separately, every
jolt process replayed one identical stream of "random" values, so a fleet minted
colliding UUIDs, and the UUIDs were guessable even once they were unique.

### Changed

- **A runtime error names the fn, file and line it came from, without setting
  anything.** The tail-frame history that survives tail-call elimination is on by
  default when running from source (`jolt run`, `-m`, `-M`, `-e`, the REPL); it was
  behind `JOLT_TRACE` before. This is what a plain uncaught error looked like:

  ```
  Unhandled exception: Divide by zero
  ```

  and now:

  ```
  Unhandled exception: Divide by zero
    trace:
      app.core/boom (src/app/core.clj:4)
      app.core/-main (src/app/core.clj:8)
  ```

  Each line is the one reached **inside that frame** — where the innermost
  function threw, and where every frame above it made its call — the same thing a
  JVM stack trace reports per frame, rather than the line each function happened
  to be defined on. The compiler sets the current line before each call and a
  function's entry records its caller's, so a frame's own line is the one recorded
  by the frame below it. `.printStackTrace` snapshots the throwing line on entry to
  the `catch`, so it reports the fault rather than the handler.

  The reporter walked Chez's live continuation, which TCO erases a tail-called
  frame from — so for the ordinary shape, `-main` tail-calling a fn that throws,
  every frame was gone and the error printed with no location at all. A *non*-tail
  call always worked, which is why this kept looking fixed when it was checked.

  Set `JOLT_TRACE=0` to opt out. The cost is a ring push per entry in code compiled
  at runtime; core is unaffected (the seed prelude is already compiled), so a
  seq/string/map workload measures the same either way, and it is only visible in
  code that is almost entirely user-level calls — a fib microbenchmark pays about
  7x. A `jolt build` binary is unchanged: its prologues are decided at build time,
  so it carries no tracing and no per-call cost unless built with it on.

  Tracing itself also got ~2.5x cheaper even with the per-frame lines added
  (per-thread state moved from thread parameters, whose writes cost ~32ns each, to
  Chez virtual registers at ~1ns, and the ring sizes are powers of two so a wrap is
  a mask rather than a division).

### Added

- **`java.security.SecureRandom`**, implemented natively over the same OS
  entropy: `nextBytes`, `nextInt` (both arities), `nextLong`, `nextDouble`,
  `nextFloat`, `nextBoolean`, `generateSeed`, `setSeed`, and the `getInstance` /
  `getInstanceStrong` statics. `nextInt(bound)` rejection samples rather than
  taking a bare modulo, which would bias toward the low residues. It no longer
  auto-loads `jolt-crypto`: it is a JDK class on the JVM, and reaching for one
  should not make a program declare a dependency.

### Fixed

- **`rand`, `rand-int`, `Math/random` and `random-uuid` now differ between
  processes and between threads.** Chez starts its PRNG from a fixed seed and
  keeps the state per thread, so every jolt process replayed one identical
  stream and every forked thread restarted it from the top. Two unrelated
  processes agreed on every "random" value, and eight threads in one process
  drew eight identical UUIDs. Clojure runs these off a process-global
  `java.util.Random` seeded from the clock; jolt now seeds lazily on first draw
  per thread, from the clock mixed with pid, thread id and a counter.

- **`random-uuid` and `UUID/randomUUID` now draw from the OS CSPRNG.** They were
  built out of `random`, which is seeded from the clock — unique per process
  after the fix above, but still guessable by anyone who knows roughly when the
  process started. Clojure backs `random-uuid` with `SecureRandom` because v4
  UUIDs get used as session ids, CSRF nonces and reset tokens, where guessable
  means forgeable. Bytes now come from `/dev/urandom`, or `BCryptGenRandom` /
  `RtlGenRandom` on Windows. If a host offers no entropy source at all the
  fallback says so on stderr rather than degrading quietly.


- **`(java.util.Random.)` with no seed never worked.** It seeded from
  `(truncate (current-time))`, and `current-time` answers a time object rather
  than a number, so the no-arg constructor always threw. Seeded instances were
  unaffected and still reproduce the JVM's exact LCG stream.

- **`.waitFor` on a subprocess can no longer hang forever.** It issued a blocking
  `waitpid`, which parks in the kernel — and when `SIGCHLD` is `SIG_IGN`, POSIX has
  wait block until *every* child has terminated before failing with `ECHILD`, so a
  child that became a zombie beforehand left it parked for good, with the whole
  process at 0% CPU. A program that sets `SIG_IGN` itself or inherits it could hang
  on any `.waitFor`. It polls with `WNOHANG` now (0.2ms backing off to 10ms), the
  way the timed `waitFor` already did, so no signal disposition can park it.

  Found because tracing shifts timing enough to make the process suite hit this
  every run instead of rarely; the hang predates that and is not caused by it.

- **`.printStackTrace` exists on every exception, and prints the trace.** It was
  reachable only on a raw Chez condition, so on the value a `catch` actually binds
  — an ex-info, or any typed host throwable — it answered `No matching method
  printStackTrace found for java.lang.ArithmeticException`. It now prints
  `class: message` followed by the same backtrace an uncaught error reports, to
  stderr or to a `PrintWriter`/`PrintStream` passed as its argument.

  The cause was that `java.lang.Throwable`'s methods were restated in two places
  that drifted. They are one table now (`throwable-method`), inherited by every
  exception class the way the JVM inherits them from `Throwable`, which also filled
  in `.getLocalizedMessage`, `.getSuppressed` and `.fillInStackTrace` — each of
  which existed on one kind of throwable and not the other.

## [0.5.17] - 2026-08-01

Gaps and wrong answers on the `java.lang.String` surface, found by probing it after
`.toCharArray` turned out to be missing.

### Added

- **`.toCharArray`, `.strip` / `.stripLeading` / `.stripTrailing`,
  `.compareToIgnoreCase`, `.contentEquals`, `.regionMatches` (both overloads), and
  `String/join`.** `.toCharArray` answers a real `char[]` — the value `(char-array s)`
  builds and `(String. ca)` reads back. The strip family is `Character.isWhitespace`,
  where `.trim` cuts at `<= U+0020`, so strip removes the Unicode separators trim
  leaves (U+3000) but not the non-breaking spaces Java deliberately excludes from
  `isWhitespace` — the same predicate `clojure.string/trim` already used here, now
  also covering U+001C..U+001F, which are whitespace to Java but carry no Unicode
  White_Space property.

### Fixed

- **`.compareTo` answers an int.** It returned `-1.0` / `1.0` / `0.0`, so it read
  correctly through `neg?` / `pos?` but printed as a double and was never `= -1`.

- **`String/valueOf(char[])` is the characters.** It rendered the array itself, so
  `(String/valueOf (char-array "hi"))` came back `"#object[[C]"` where
  `(String. (char-array "hi"))` already gave `"hi"`.

## [0.5.16] - 2026-08-01

Chez's clocks and collector counters are readable from Clojure now, as plain
integers off `jolt.host`, which is enough for a profiler, a health endpoint or an
OpenTelemetry exporter to work from. `System/nanoTime` and `Thread.join` were
both wrong in ways that surfaced on the way there, and `.split` turned out to be
discarding its limit argument.

The release workflow's own fleet gates were testing the wrong binary, which is
why this is the first 0.5.16 to ship.

### Added

- **Telemetry primitives on `jolt.host`.** Two clocks (`wall-nanos`,
  `mono-nanos`), the collector's counters (`cpu-nanos`, `real-nanos`,
  `gc-count`, `gc-cpu-nanos`, `gc-real-nanos`, `gc-bytes`), the allocator's
  (`bytes-allocated`, `current-memory-bytes`, `maximum-memory-bytes`),
  `thread-id`, and the runtime's own identity (`scheme-version`,
  `machine-type`). Chez tracked all of it already, but only behind record types
  — time objects and sstats — that Clojure code can't do arithmetic on. The two
  clocks stay distinct on purpose: `wall-nanos` is the only one a remote
  collector can interpret and ntp can step it, `mono-nanos` never steps but has
  an arbitrary origin, so anything reporting both an absolute timestamp and a
  duration needs both and derives the timestamp from the pair.

- **`java.util.concurrent.TimeUnit`.** The seven constants, the `to*` conversions
  (truncating toward zero, like the JVM) and `sleep`. Nothing modeled it before,
  which meant every method taking a `(timeout, unit)` pair could not be called
  with one — and that is how each of them went unnoticed with its timeout argument
  discarded. `CountDownLatch.await`, `Future.get`, `ExecutorService.awaitTermination`
  and `ReentrantLock.tryLock` all read it now: the bounded overloads gave up
  immediately, waited forever, or (awaitTermination) read the amount as
  milliseconds outright, so `5 SECONDS` was five milliseconds.

### Fixed

- **The release's own fleet gates ran the wrong binary.** Every library gate and
  the examples gate unpacked the release tarball into the workspace root and then
  picked the jolt to test with `find . -name jolt -perm -u+x`, in a workspace that
  also holds the jolt checkout — whose `bin/jolt` is an executable file by that
  name. Which one won came down to directory traversal order, and the extracted
  directory carries the version in its name, so the order flipped between one
  release and the next: v0.5.16's gates all died on "No valid Chez Scheme
  executable found" against a binary that was fine. The tarball now unpacks into a
  directory of its own and is searched only there.

- **A rebuilt binary was killed on macOS.** `jolt build` wrote its output over the
  existing file, and macOS caches a code-signature verdict per vnode — so the
  rewritten binary kept the stale verdict and the kernel `SIGKILL`ed the next run
  with no output at all (`Killed: 9`). A rebuild-and-run loop over one output path
  worked a handful of times and then started dying for no visible reason, on a
  binary that ran fine the moment it was built somewhere else. The output is
  removed before the new one is written, so it lands on a fresh inode; a failed
  link now also leaves no binary rather than a half-overwritten one.

- **`.split` honors its limit, on both `String` and `Pattern`.** The limit argument
  was discarded outright, so `(.split "user:pass:word" ":" 2)` split three ways and
  anything separating a key from a value that may itself contain the separator — a
  URL header, a password, a status line's description — got the value truncated at
  its first separator. Positive limits cap the parts and leave the last unsplit,
  a negative limit keeps trailing empty strings, and 0 (the one-argument form)
  drops them. Interior empty fields survive too: `(.split "a::b" ":")` was
  `["a" "b"]` where the JVM gives three parts. Both methods answer a `String[]`
  now; `String.split` used to return a vector and `Pattern.split` a seq, so the
  same split printed two different ways.

- **A compile error names the expression that failed.** Only the unresolved-symbol
  diagnostic carried a position, so everything else raised while analyzing — an
  uncompilable form, a destructuring pattern the desugarer rejects, a macro that
  threw expanding — was reported at the enclosing top-level form's opening line,
  over thirty lines of the analyzer's own frames (`analyze-list`, `map-seq`,
  `seq->list`) naming nothing the reader could act on. Analysis failures carry the
  innermost positioned form's `file:line:column` now, and the internals trace is
  dropped, which is what the reporter already did for the one diagnostic that had
  a position.

- **A `proxy` method with several arities.** `proxy` accepts both
  `(name [params*] body*)` and `(name ([params*] body*) ([params*] body*) ...)`;
  only the flat form was handled, so the grouped one handed `reify` a body
  expression where an arglist belonged and died in destructuring — "unsupported
  destructuring pattern: (.read src buf off (min 1 len))" for a throttled
  `InputStream`.

- **`System/nanoTime` is monotonic.** It was `(* 1000000 (now-millis))` —
  wall-clock derived, so a clock step ran it backwards, and millisecond-granular,
  so any interval shorter than a millisecond timed as zero. It reads Chez's
  `'time-monotonic` clock now, which is the JVM's contract for it.

- **`Thread.join` honors its timeout and returns on an unstarted thread.** The
  timeout argument was discarded outright, so every bounded join was an unbounded
  one — a caller that joined a long-lived worker with a timeout deadlocked
  instead of giving up. Both forms also waited on the "thread finished" flag
  rather than on liveness, and that flag is never set on a thread that was never
  started, so `join()` on one blocked forever and `join(ms)` burned the whole
  timeout where the JVM returns at once. A negative timeout throws
  `IllegalArgumentException` instead of waiting indefinitely.

## [0.5.15] - 2026-08-01

Dependency resolution reads the POM Maven would. jolt used to take a jar's
transitive deps from whatever `pom.xml` the jar happened to package, which meant
a jar that ships without one — `metosin/malli` — contributed none at all, and
projects worked around it by listing them by hand. jolt now vendors Grenadine
and builds the effective POM from the repository, so inheritance, properties,
dependency management and exclusions all count. The CLI also grew the rest of
the `clj` `-S` option surface, which is how editors ask for a classpath.

### Added

- **Maven transitive deps come from the real effective POM.** jolt used to scrape
  the `pom.xml` a jar happened to package under `META-INF/maven/`, so a jar that
  ships without one — `metosin/malli` is one — contributed no transitive deps at
  all, and a project had to list them by hand. jolt now vendors
  [Grenadine](https://github.com/clojurestar/grenadine) as a git submodule under
  `vendor/`, alongside irregex and babashka fs/process, and builds the effective
  POM from the repository: parent inheritance, properties, dependency
  management, BOM imports, and `<exclusions>`, which the expander already
  honored but never received. Resolving malli picks up its five deps and their
  transitives instead of nothing. The cost is one small `.pom` fetch per
  dependency, cached in the local repository beside the jar.

  A POM jolt can't model no longer sinks the resolution: an unfetchable `.pom`
  (a hand-installed jar, an offline machine) or a version left `${unresolved}`
  because it comes from a profile now warns and falls back to whatever the jar
  packages. Grenadine checks every declared dependency before jolt filters by
  scope, so a test-scoped dependency jolt discards anyway was enough to abort.

- **`-Spath` prints the classpath and runs nothing**, like the clj CLI. It can
  come on either side of the alias options — `jolt -A:test:dev -Spath` and
  `jolt -Spath -M:test` both print the source roots that run would use, and
  `-X`/`-T`/`-Sdeps` compose with it the same way (`-T` replacing the project's
  own basis, as it does when running). Editors ask for the classpath this way
  before connecting; jolt only had `jolt path`, which took no aliases from the
  argv it was dispatched with, so Calva's `-A:test:dev -Spath` died on "unknown
  command or task: -Spath".

- **The rest of the clj option surface: `-Stree`, `-Strace`, `-Sdescribe`,
  `-Scp`, `-P`, `-Srepro`, `-Sverbose`.**
  They compose with the aliases the same way `-Spath` does.
  `-Stree` prints the dependency tree in the tools.deps format — a top dep
  unprefixed, what it pulled in indented under it with `.`, and a node that lost
  marked `X` with the reason (`:use-top`, `:older-version`, `:excluded`,
  `:superseded`, …); the expansion already made those decisions, it just wasn't
  recording them, and `-Strace` writes the same log to `trace.edn` in the
  tools.deps shape (`{:log [...] :vmap {...}}`). `-Sdescribe` prints the
  version, the deps.edn chain, and the cache locations as an edn map, without
  resolving a single dependency, so an editor can ask cheaply and offline. `-P`
  fetches every dependency and stops — the prepare step for a CI job or a
  container layer. `-Srepro` ignores `~/.clojure/deps.edn` for one run (the
  per-run form of `JOLT_NO_USER_DEPS`), and `-Sverbose` says which files the
  resolution reads and which caches it fetches into, on stderr rather than clj's
  stdout so `-Sverbose -Spath` still pipes.

- **`-Scp` runs against source roots given on the command line**, expanding no
  dependencies — `jolt -Scp "$(jolt -Spath)" -M:test` runs offline with nothing
  fetched. The deps.edn chain is still read, so aliases, `:main-opts` and
  `:tasks` work; tools.deps' `--skip-cp` draws the line in the same place. What
  goes with the expansion is a *dependency's* `:jolt/native` shared libraries —
  the project's own still load.

- **`-Sforce`, `-Sthreads N` and `-Jopt` are accepted and ignored**, so a tool
  that always passes them isn't rejected: jolt resolves its roots on every run
  (no classpath cache to force), fetches serially, and has no JVM. An `-S`
  option jolt genuinely doesn't have (`-Sman`, `-Spom`) now names itself as an
  unsupported option instead of being reported as an unknown deps.edn task.

### Fixed

- **An undeclared alias is a warning, not an error.** tools.deps skips an alias
  the deps chain doesn't declare and says so ("Specified aliases are undeclared
  and are not being used"); jolt threw instead, so an editor sending a fixed
  alias set got no classpath at all from a project that happens to declare only
  some of them. It now warns on stderr and carries on with the aliases that do
  exist.

- **`-A` no longer resolves the project twice.** It resolved and applied the
  deps before re-dispatching the rest of the argv, and every command it
  re-dispatches to (`run`, `build`, `repl`, `nrepl-server`, a task, `-e`,
  `-M`/`-X`/`-T`) resolves for itself — so each `-A` invocation walked the whole
  dependency tree twice and printed any resolution warning twice with it.

- **`bin/jolt` runs the same Chez as the rest of the build.** `make` provisions
  its own Chez when the one on `PATH` is a different version, but `bin/jolt`
  searched `PATH` itself, so a build could straddle two installs — the targets
  calling `$(CHEZ)` getting one and the targets shelling out to `bin/jolt` the
  other. That surfaced as whichever primitive the older Chez predates
  (`variable flvector? is not bound` on a 9.x), or, once `make devboot` had run,
  as `incompatible fasl-object version`, since a fasl only loads in the Chez
  that wrote it. `bin/jolt` now takes `$JOLT_CHEZ`, which the Makefile exports.

  Separately, the devboot image now records which Chez built it, and `bin/jolt`
  falls back to source mode when that isn't the one about to run it or when it
  has since been upgraded in place. Neither shows up in the image's input list,
  so the cache read as current while nothing in jolt explained the failure.

- **A built binary serves its own source for a shadowed namespace.** Install
  roots are first-wins, but the binary baked them last-wins, so a namespace
  present on two roots — a vendored library shipping a facade under a jolt name
  — reached the binary as the wrong file while `bin/jolt` kept the right one.
  The namespace itself is compiled in and kept working; what broke was
  `io/resource`, which is how orchard maps a namespace back to a file, so an
  editor could jump to the wrong source.

### Changed

- **A deps.edn `:mvn/local-repo` now outranks the `JOLT_LOCAL_REPO`
  environment variable**, matching how tools.deps treats explicit configuration.
  `GRENADINE_LOCAL_REPOSITORY` sits between them as the shared environment
  default; `JOLT_LOCAL_REPO` still works.

## [0.5.14] - 2026-08-01

Editor tooling works: jolt publishes its source roots as `java.class.path`,
`jolt.nrepl` gained the version and error-history seams an nREPL middleware needs,
and `clojure.test` report maps carry `:expected`/`:actual` again, so a custom
reporter can say what it compared. Together these let jolt-lang/nrepl serve the
cider-nrepl op set to CIDER and Calva. A stale `PWD` no longer wins over the real
working directory, and `keys`/`vals` throw on an element that isn't an entry
instead of returning nonsense.

### Added

- **`java.class.path` answers with the resolved source roots.** jolt's classpath
  is its source roots — the project's `:paths`, every dependency's root, and the
  roots jolt ships — and the loader now publishes them through the system-property
  table. Editor tooling ported from the JVM discovers project sources through that
  property, so with it unset orchard's namespace scan, compliment's classpath
  completion sources and cider-nrepl's `classpath` op all quietly returned nothing.

- **`jolt.nrepl`: `register-version!`, REPL history vars, and the last error's
  backtrace.** Middleware could register ops but not versions, so an editor had no
  way to learn what dialect the server speaks (CIDER refuses the cider-nrepl ops
  without a version entry); `register-version!` is the seam, beside
  `register-ops!`. `evaluate` now sets `*1`/`*2`/`*3` and `*e` like every other
  nREPL server, and records the backtrace of the exception in `*e`
  (`last-error-backtrace`) — a jolt exception carries no stack of its own, and the
  backtrace is only readable where it was caught, so tooling that presents the
  error afterwards had nothing but the message. `*capturing-thread*` names the
  thread whose output an eval is capturing, so middleware that forwards server
  output doesn't send an eval's own output twice.

### Fixed

- **`clojure.test` report maps carry `:expected` and `:actual`.** Assertions
  folded both into a rendered `:message`, leaving every custom reporter — CIDER's
  test op, test.check, matcher-combinators, any TAP/JUnit reporter — with nothing
  to report: a failure said what it printed but not what it compared. `:expected`
  is now the form as written and `:actual` the form with its arguments evaluated
  (or, for an error, the throwable itself rather than its message text), matching
  clojure.test. `(is (instance? C x))` reports the class of `x` on both branches,
  and `is` yields the value it tested, both like the reference.

- **`clojure.core/hash-combine` hashes its second argument.** It is
  `(Util/hashCombine x (Util/hash y))` — a seed and a VALUE — but jolt passed `y`
  straight to the integer combiner, so `(hash-combine 0 "a")` threw out of
  `bitwise-and` instead of hashing. Any ported library that folds `hash-combine`
  over values died on the first non-number.

- **A keyword's `.hashCode` is the Java hash, not its hasheq.**
  `Keyword.hashCode()` is `sym.hashCode() + 0x9e3779b9`; jolt answered with the
  murmur-based hasheq, so a keyword's `.hashCode` disagreed with the JVM while a
  symbol's agreed.

- **`.listFiles` / `file-seq` keep the form of the path they were given.** Like
  `new File(this, name)`, listing a relative directory yields relative children;
  jolt resolved the base to an absolute path first, so every child came back
  absolute and a caller relativizing the results against the directory it passed
  in (classpath scanning) got `../..`-prefixed garbage.

- **`java.util.Map`'s default methods on the HashMap shim.** `putIfAbsent`,
  `computeIfAbsent`, `computeIfPresent`, `compute`, `merge`, `replace` and
  `forEach`, with the JVM's return values and its treatment of a nil mapping as
  absent.

- **A stale `PWD` no longer decides `user.dir`.** `PWD` is exported by the shell
  and is not updated by a process that changes directory itself, so any tool that
  ran jolt after a `chdir` resolved every relative path against the directory it
  started in. `user.dir` now prefers `JOLT_PWD`, then the real working directory,
  and honors `PWD` only where it agrees with it.

- **`keys` and `vals` throw on an element that isn't an entry.** Both indexed
  every element blindly, so `(keys ["ab" "cd"])` handed back `(\a \c)` and
  `(keys [1 2])` gave `(nil nil)` where the JVM throws — silent nonsense in place
  of an error. Anything that is not a two-element vector now raises the same
  `ClassCastException`, naming the same class. A vector of pairs still walks as a
  seq of entries, which is a documented superset.

## [0.5.13] - 2026-08-01

Locale-sensitive formatting works: `NumberFormat/getCurrencyInstance`,
`SimpleDateFormat`'s month and day names, and `String/format`'s decimal separator
all honor a `Locale`, with the per-locale data supplied by jolt-lang/time through
a new extension-point seam. `io/resource` returns a `java.net.URL` like the JVM,
`:use` honors `:exclude`, and a mismatched delimiter reports its position instead
of hanging the reader.

### Fixed

- **`:use` honors `:exclude`, and `(:refer-clojure :exclude …)` lands in the ns
  being defined.** Two host namespace bugs, one visible failure surface: a library
  test ns that `:use`s two namespaces exporting the same name got the WRONG one.
  `use` registered its refer-all with no record of the spec's `:exclude`, so with
  `(:use [a] [b :exclude [f]])` the bare `f` resolved to `b/f` — the later use
  shadowed the earlier, exclusion or not — instead of falling through to `a/f`.
  Excluded names are now recorded per (ns, target) and skipped in the refer-all
  walk, so resolution falls through exactly like load-lib's filtered refer.
  Separately, the `refer-clojure` macro registered its exclusions at macroexpansion
  time under the analysis-time ns, so a `(:refer-clojure :exclude [==])` clause in
  an ns form excluded nothing (the ns it named didn't exist yet); the expander now
  emits a runtime registration call that runs after the form's `in-ns`, and unwraps
  the ns macro's quoted args the way the JVM's splice-into-`refer` does. This is
  what was behind core.logic's nominal suite residue (64/36/6 → 106/0/0): the test
  ns's plain `fresh` was silently nominal's `fresh`, and fd.clj's `-rator`
  syntax-quotes qualified to `clojure.core/==` instead of
  `clojure.core.logic.fd/==`.

### Changed

- **`byte-array` elements are signed bytes, −128..127, like the JVM's `byte[]`.**
  They were unsigned 0..255, so `(vec (byte-array [255 128]))` was `[255 128]` where
  the JVM gives `[-1 -128]`, and every numeric look at a high byte disagreed:
  `(neg? b)` never fired, `(= b -1)` was never true, `(reduce + bytes)` summed to a
  different number, and a `(bit-and b 0xff)` mask that is load-bearing on the JVM
  was a no-op here. Nothing errored — the answers were just quietly different.

  This is one representation change across every producer and consumer, not a
  per-call-site patch: `na-byte-of` is the one place a value entering a byte array is
  narrowed (`Byte.byteValue()` semantics — truncate toward zero, low 8 bits, fold the
  sign), `na-bv->bytearray` the one place raw bytes become a byte array, and
  `na-bytearray->bv` the one place they go back. `byte-array` / `.getBytes` /
  `String.` / stream reads / `Files/readAllBytes` / `jolt.fs` / `io/copy` /
  `ByteBuffer` / Base64 / `Random/nextBytes` / `jolt.ffi` read-array/write-array all
  route through those three, so bytes survive any round-trip byte-exactly. `aset` on
  a byte array narrows rather than storing raw — the JVM rejects `(aset bytes 0 200)`
  because it can see 200 is an Integer, and jolt has one integer type, so narrowing
  is what keeps the array's range invariant true for `aget`/`seq`/`String.`.
  `InputStream.read()` stays unsigned 0..255, which is its contract and the only way
  a caller can tell `0xff` from end-of-stream.

  `(byte-array [-1.9])` is now `-1`, not `-2` — the JVM's `d2i` truncates toward
  zero where this floored.

- **`format`'s `%x` / `%X` / `%o` are unsigned conversions.** They printed a signed
  magnitude, so `(format "%x" -1)` was `"-1"` — wrong under any width. The JVM takes
  the width from the argument's type (`Byte` 8 bits, `Short` 16, `Integer` 32, `Long`
  64); jolt unifies every integer as one type and so takes the narrowest width that
  holds the value. That is the JVM's answer whenever the value's origin type is the
  narrowest that holds it, so an unmasked byte out of a `byte[]` prints two digits
  and an int-sized value eight — which is what the common hex-dump and
  percent-encoder idioms need, and it keeps ring-codec and cognitect aws-api's signer
  correct unmodified. A long whose value fits narrower is the one case that
  diverges (`(format "%x" (long -1))` is `"ff"` here, 16 digits on the JVM); it is
  entered in `known-divergences.edn` under `:integer-box-model`. `(bit-and b 0xff)`
  pins the width explicitly and is identical on both.

- **`clojure.java.io/resource` returns a `java.net.URL`, like the JVM.** It returned a
  `java.io.File`, which `io.ss` papered over by answering `getProtocol`/`getFile` on a
  File. Callers that branch on the real type broke: Selmer's `render-file` does
  `(instance? java.net.URL path)`, took the `:else` branch on a File, and died on
  `(.startsWith ^File …)`. Both branches now return a URL — a `file:` URL for a hit on
  a source root, a `jar:`-classed embedded resource (class `java.net.URL`) for a file
  baked into a built binary — so the "same surface whichever branch served it"
  invariant holds in `make test`, not only inside a built binary. The File URL-compat
  kludge is gone; a File no longer answers URL methods, as on the JVM.

  A URL is now a first-class source too: `slurp` / `io/reader` / `io/input-stream` /
  `.openStream` read a `file:` URL's target (a non-file protocol raises, as the JVM
  does, rather than returning empty content) and `io/file` strips the scheme. These
  had to land before the return-type flip or the common `(slurp (io/resource …))`
  idiom would throw.

### Added

- **`jolt build` compiles the runtime half of its flat source once and keeps the
  fasl.** A build emits one flat Scheme file — jolt's runtime (`rt.ss`, the
  `clojure.core` prelude, the compiler image, the loader) followed by the app — and
  handed the whole thing to Chez every time. The runtime half is byte-identical for
  every app a given jolt builds in a given mode (verified: two unrelated apps
  produce the same 3.0 MB to the byte), and compiling it is ~2.6s. It is now emitted
  to its own `runtime.ss`, compiled once per (content, mode), and the fasl reused;
  the two units are loaded into the boot in order, so the runtime's defines still
  precede the app's reads.

  A small app's build is mostly that one compile, so this is most of its build time:
  `examples/hiccup-app` goes from 3.13s to 0.50s. A large app amortizes it against
  its own work (`examples/ring-app` loses ~2.6s of ~42s). Cached under
  `~/.jolt/runtime-cache` (`JOLT_RUNTIME_CACHE_DIR`), newest 8 entries kept;
  `JOLT_RUNTIME_CACHE=0` opts out and `JOLT_NO_FLAT_SPLIT=1` restores the one-file
  build. Skipped for `--tree-shake` (which rewrites the prelude per app), for
  `--library`, and for cross builds.

- **`JOLT_BUILD_PROFILE=1` reports each build phase's wall-clock time**, including a
  breakdown inside the two expensive ones (the whole-program fixpoint and the emit
  walk). `bench/build-phases.sh` drives it across build modes and prints the split
  between jolt's own passes and Chez's compile. A build's cost divides between those
  two and they want unrelated fixes, so which one dominates is worth being able to
  see rather than assume — it is not the same for a small app as for a large one.

### Fixed

- **`.toAbsolutePath` was a no-op on a relative path in a built binary, so every
  `fs/glob` under a relative root came back unusable.** A user-facing relative path
  resolves against user.dir, which is `JOLT_PWD` → `PWD` → `"."` — the chain
  `System/getProperty "user.dir"` answers with. `project-relative` implemented only
  the first link, and `JOLT_PWD` is exported by `bin/jolt` but by nothing in a built
  binary, which never cd's away and so needs none. There the path came back
  unresolved, `jfile-abs` returned a relative string in defiance of its own
  contract, and babashka.fs's `match` — which relativizes each result against
  `(absolutize "")` whenever the glob root is relative — subtracted an absolute cwd
  from a relative entry: `(fs/glob "examples" "**")` yielded
  `../../../../examples/sample.clj`. Nothing threw; every subsequent open just
  missed. `project-relative` and `jfile-abs` now share one `jolt-user-dir` helper,
  so neither can implement half the chain again.

  The dev launcher masked this end to end — it always exports `JOLT_PWD`, so no
  `bin/jolt` run could reproduce it — and `test/chez/fs-test.clj` rooted every case
  at an absolute temp dir, so the one gate that runs a built binary never exercised
  a relative path. It now asserts that a relative path absolutizes under user.dir
  and that `relativize` undoes `absolutize`, the round-trip `match` performs.

- **Java regex translation now matches the JVM on 17 long-tail `Pattern` edge cases.**
  The `Pattern`→irregex translator (`host/chez/java/regex-translate.ss`) now agrees
  with `java.util.regex.Pattern` on accept/reject for flag groups, character-class
  intersection, inverted ranges, malformed quantifiers, and class-only escapes:
  - empty and unknown flag groups (`(?)`, `((?){0,0})`, `(?c:Z)`) now compile, while
    a dangling `*`/`+`/`?` after a body-less flag group (`(?)?`) is rejected;
  - `&&` intersection with an empty side means "everything" (`[&&x]`, `[x&&]`,
    `[%-&&&]`, `[x&&&&]`), and a leading `&&` with no left operand (`[&&&]`) still
    rejects;
  - inverted character ranges (`[b-a]`, `[]-X]`, `[x-\cx]`, `[{\x{10000}-}]`, and the
    nested `[[[[{-\c}]]]]`) now reject;
  - a quantifier with min greater than max (`{1,0}`) now rejects even with no
    preceding atom;
  - `\Q..\E` (empty) and `\R` are rejected inside a character class;
  - `\e` is now ESCAPE (U+001B), not a literal `e`.
  20 JVM-certified rows added to `test/chez/corpus.edn`.

- **`conj` of a map into a `defrecord` merges the map's entries, as on the JVM.**
  The default record `conj` handler treated its argument as a single `[k v]` pair and
  `nth`'d it, so `(conj (map->R {:a 1}) {:b 2})` threw
  "nth not supported on this type: clojure.lang.PersistentArrayMap" instead of
  merging; `merge`-ing a map into a record hits the same `conj` path. This surfaced
  through test.chuck: `instaparse.failure/augment-failure` does
  `(merge <Failure> {:line … :column …})`, so a regex that fails to parse raised an
  uncatchable `UnsupportedOperationException` where the library (and the JVM) raise a
  catchable `ExceptionInfo {:type ::regexes/parse-error}`. A map argument now folds
  its entries; a `[k v]` pair or `MapEntry` is unchanged.

- **`clojure.pprint`'s `simple-dispatch` and `code-dispatch` are multimethods, so
  libraries can extend them.** They were plain functions that `case`d on a type
  keyword, so `(. clojure.pprint/simple-dispatch addMethod Tie pprint-tie)` — which
  core.logic's nominal namespace does — failed with "No matching method addMethod",
  and `clojure.core.logic.nominal.tests` could not load. Both are now `(defmulti …
  class)` with methods for the same interfaces the reference uses
  (`IPersistentVector`/`-Map`/`-Set`, `PersistentQueue`, `ISeq`, `IDeref`, `nil`,
  `:default`; `code-dispatch` adds `Symbol`), each arm still calling the per-type
  printer it always did, so built-in output is unchanged — a defrecord still prints
  as a bare map, as it does on the JVM.

- **A sub-process that could not be waited on hung the caller forever.** The reap
  loop retried on any `waitpid` failure, including `ECHILD` — the child already
  reaped by something else, which no number of retries changes. The loop holds the
  process's mutex, so it did not merely spin: every other method on that process
  deadlocked behind it, silently and indefinitely. This is what sat on a CI gate for
  3h42m. `EINTR` is now the only retried failure; an unwaitable child resolves to a
  status (128+signal when jolt signalled it, else 0 — the JVM always reaps its own
  children and so always knows, jolt cannot recover a status the kernel consumed).

  The condition is reachable through no fault of the program: with `SIGCHLD` set to
  `SIG_IGN` the kernel reaps every child itself, and that disposition survives
  `exec`, so jolt can inherit it from any parent. The first spawn now restores
  `SIG_DFL` when it finds `SIG_IGN`, leaving a real inherited handler alone.

- **`(.availableProcessors (Runtime/getRuntime))` always answered 1.** It was
  hardcoded, so nothing sized to the machine — `pmap` in particular ran a fixed
  4-wide window regardless of how many cores were available. It now reports the
  processors this process may actually use: `sched_getaffinity` on Linux, so a
  process confined by `taskset` or a cpuset sees its real limit rather than the whole
  machine (what the JVM and `nproc` report); `hw.logicalcpu` on Darwin;
  `NUMBER_OF_PROCESSORS` on Windows. `pmap` sizes its look-ahead from it, as Clojure
  does. A cgroup CPU quota is not yet reflected (jolt-j4sd).

- **`{n}` in a regex meant `{n,}`.** The translator handed irregex an unbounded
  upper bound for an exact count, so every exact repetition matched greedily past
  it: `(re-find #"\d{4}" "20260729")` returned the whole string instead of `"2026"`,
  `(re-matches #"\d{4}" "20260")` matched where the JVM returns nil,
  `(re-seq #"\d{2}" "123456")` gave one element instead of three, and
  `#"(?:%[0-9a-f]{2})+"` ran past the last percent-escape — which is how
  ring-codec's `percent-decode` came apart. `{n,}` and `{n,m}` were always right;
  only the comma-less form was wrong.

- **`jolt <task>` lost to a same-named directory.** A bare argv token is dispatched
  as a file to run before a `:tasks` lookup, and the "is this a file" test was
  `file-exists?`, which is true for a directory too. `test` is a `:tasks` entry AND a
  `test/` directory in every jolt project, so `jolt test` was dispatched as a path
  and died in `load-file`'s decoder — `failed on #<binary input port test>: is a
  directory` — instead of running the task. That is why every library's CI spells out
  `jolt -M:test`.

- **`Base64` handed back an opaque host buffer instead of a `byte[]`.**
  `.decode` / `.encode` return `byte[]` on the JVM, so `(vec (.decode dec s))` is
  ordinary Clojure; here they returned a raw Chez bytevector that no collection
  dispatcher knows, so that threw `Don't know how to create ISeq from` and callers
  had to route every result through `String.` first. Both return a byte array now,
  and `bytes?` is true of them.

- **`Random/nextBytes` produced a different stream from the JVM for the same seed.**
  It drew 8 bits per byte, consuming the LCG four times as fast as
  `java.util.Random`, which draws one `nextInt()` per four bytes and takes them
  low-to-high. Every other `Random` method already matched; this one did not, so a
  seeded byte fill was not reproducible against the JVM.

- **`io/copy` from a `File` to anything but another file went through text.** A File
  source was only treated as a byte source when the destination was also a file;
  everything else fell through to slurping it as UTF-8, which replaced each
  non-UTF-8 byte with U+FFFD. `(io/copy f stream)` on a binary file returned
  corrupt bytes.

- **`Arrays/toString` printed a jolt vector.** `"[1 2]"` rather than the JVM's
  `"[1, 2]"` — no commas, a `nil` element rendered empty instead of `null`, and a
  string element came out `pr`-quoted.

- **`Integer/toString` with a radix uppercased its digits** (`"-FF"` for
  `(Integer/toString -255 16)`, where the JVM gives `"-ff"`), and `Long` was missing
  `toHexString` / `toOctalString` / `toBinaryString` / `toString` / `compare`
  altogether. `Character/isLetter` and `isLetterOrDigit` were missing too — aws-api's
  request signer classifies each UTF-8 byte of a URI through them.

- **A compile-time error pointed at the top of the enclosing form, not at the
  expression that failed.** The only position available to the reporter was the one
  the loader records per TOP-LEVEL form, so an unresolved symbol partway into a
  long `defn` was reported at the `defn`'s opening line — 280 lines above the
  offending name in the case that prompted this. The analyzer now tracks the
  innermost enclosing form that carries reader metadata and attaches its
  `:line`/`:column`/`:file` to the diagnostic, which the human report and the
  `JOLT_DIAG=edn` map both prefer over the coarser one. The reference compiler
  reports the same position for the same file, down to the column.

  The form is tracked, not its position map, because building the map allocates and
  this runs for every list form the analyzer walks; the map is built once, on the
  error path. `analyze-list` saves and restores the cell around its children, so a
  finished sibling subtree cannot leave a deeper position behind for a later
  sibling's diagnostic — the same thing `Compiler.analyzeSeq` does by pushing
  thread bindings of `LINE`/`COLUMN`, for the same reason. A release build of
  `examples/ring-app` takes the same 43s it did before.

- **A compile-time error printed the analyzer's own call stack as its "trace".**
  Around thirty frames of `analyze-list` / `analyze-seq` / `map-seq` / `seq->list`,
  which are jolt compiling the form rather than anything from the program, and they
  pushed the message and the location off the top of the report. Such an error is
  raised while ANALYZING, so there is no user call stack to show, and on the
  open-world path a user frame would carry no location anyway. A diagnostic
  carrying `:jolt/error` now reports message and location only. A runtime error's
  trace is unchanged.

- **`make devboot`'s cache turned source-map registration off**, so the dev CLI's
  traces printed bare procedure names where the released binary prints
  `ns/name (file:line)`. `emit-image.ss` disables both var-cell caching and source
  registration at load time — a build must emit byte-identical output carrying no
  absolute paths — and the image loads the build subsystem eagerly, so it baked the
  build settings. The var-cache half of this was fixed in 0.5.11; source
  registration was missed. Both are restored now. The symptom was that the
  source-mapped-trace smoke checks passed in script mode and failed only against a
  fresh cache.

## [0.5.12] - 2026-07-29

Stack traces from a build, and from a binary built without direct-linking, now
carry the file and line they came from. `#{1 1}` is a read error, as on the JVM,
and `:as-alias` aliases without loading — both at the REPL and through a build.

### Fixed

- **An open-world build's stack traces had no namespace, file or line.** A
  direct-linked build registers each fn def's source, so an uncaught error maps its
  frames to `app.util/deep-boom (…/util.clj:24)`. The registration was gated on
  direct-link, so `jolt build --no-direct-link` printed a bare `deep-boom` with
  nothing to locate it. The build turns `source-reg` on when it is not
  direct-linking — the same switch the runtime eval path already sets, so an
  open-world binary now reads like a `jolt run` trace. A direct-link build is
  unchanged and nothing registers twice.

- **`#{1 1}` compiled to `#{1}` instead of being a read error.** The JVM rejects a
  duplicate element when the reader builds a set literal
  (`PersistentHashSet/createWithCheck`); jolt only checked on the DATA path, so
  `(read-string "#{1 1}")` threw while the same literal in a source file quietly
  became a one-element set. Map literals were already checked. Elements compare as
  read, like the JVM: two symbols are distinct, two equal lists are not, and the
  runtime builders (`set`, `hash-set`) still dedupe silently.

  Neither check runs in the build's scan mode, which is a fix in its own right: an
  auto keyword whose alias is not registered yet keeps the ALIAS TEXT as its
  namespace there, so `#{::o/x :o/x}` read as two copies of `:o/x` and the
  duplicate check rejected it — failing the build of a namespace that loaded fine.

- **`:as-alias` loaded the namespace, and did not alias it.** Clojure 1.11's
  `:as-alias` establishes the alias without loading the target, for a namespace that
  may not exist yet or exists only to qualify keywords; `load-lib` picks its loader
  with `need-ns (or as use)` and falls to `(create-ns lib)`. jolt loaded the target
  and then dropped the alias on the floor, so the namespace's side effects ran and
  `::o/x` was still an invalid token. A spec that also carries `:as`, or that
  arrives through `use`, still loads.

  `jolt build` got both halves wrong too, independently of the loader: its require
  scan counted an alias-only spec as a dependency, so the target was emitted into
  the binary and its top level ran there, and the emitted `ns` prelude replayed
  `:as` but not `:as-alias`, so the alias was missing at runtime. Combining
  `:refer` with `:as-alias` throws on both jolt and the JVM — the target is not
  loaded, so there is nothing to refer — with different message text.

- **A `jolt build` failure reported no location.** The build has three walks that
  process a source file without evaluating its forms — the require scan, the
  whole-program inference walk, the emit walk — and none reaches
  `jolt-enter-form!`, which is what records where we are. So a failure in any of
  them printed `Unhandled exception: …` over a trace of runtime procedure names
  (`rdr-form->data`, `bld-ns-requires`, `dfs`) and said nothing about which file it
  was reading. They record it with `jolt-enter-file!` now.

  That is a bare set rather than a `parameterize` on purpose: the uncaught reporter
  runs from the CLI's guard, outside every dynamic binding the failing walk held,
  so a parameterized value has already unwound by the time it is read. It is the
  same reason `jolt-enter-form!` sets rather than binds, and why `load-jolt-file*`
  restores on normal return only. Entering a file also clears any leftover
  line/column, which belonged to whatever was last evaluated somewhere else — a
  build error pointing into `jolt/main.clj` would be worse than one pointing
  nowhere.

  The frame names themselves are unchanged. Only a direct-link or AOT build
  registers procedure sources, so on the open-world path a frame maps to a bare
  name; that is the documented trade-off in `source-registry.ss`, and the location
  line is what carries the actionable part.

## [0.5.11] - 2026-07-29

A warm AOT cache no longer replays a second copy of the stdlib namespaces a
library requires, which had been silently undoing whatever that library
registered over them, and `apply` no longer realizes an infinite seq before
handing it to a variadic fn. `examples/ring-app` builds. `*allow-unresolved-vars*`
and `*compile-path*` now do what they do on the JVM, and a batch of conformance
work from the compliment and orchard suites lands with them.

### Added

- **`clojure.core/compile` and a working `*compile-path*`.** `*compile-path*` was
  a var jolt exposed with the JVM's default and nothing behind it, and `compile`
  did not exist at all. Both now work the way core.clj and `Compiler.compile`
  describe: `(compile 'my.lib)` binds `*compile-files*`, loads the lib, and writes
  its compiled form under `*compile-path*`; a nil `*compile-path*` raises
  `*compile-path* not set`. The artifact is a Chez fasl of the emitted Scheme —
  the same thing the AOT cache produces — beside the `.scm` it was built from and
  a `.meta` describing what it was built against, in place of the JVM's `.class`
  files. Like the JVM, the compile carries through the lib's whole load closure,
  and the output directory has to be a source root (jolt's classpath) before a
  later load will pick it up, so `(compile 'app)` into a directory you then put on
  the roots gives you a project that runs with no source present.

  `RT.load` prefers a `.class` to its `.clj` on mtime. jolt compares a content
  hash instead — the rule the AOT cache already decides by, and immune to a bare
  `touch` — and refuses an artifact outright unless the jolt version and runtime
  fingerprint in `.meta` match, since a fasl from another build calls runtime
  helpers that may be gone. `.meta` also records the direct requires and their
  hashes, so editing a namespace this one requires invalidates it; a change
  further down the graph does not, the same discipline JVM AOT needs.

- **`areduce` and `amap`.** Index loops over `alength`/`aget`/`aset`/`aclone`,
  all of which jolt already had, so they were never JVM-only — they had been
  parked in the JVM-only macro suite beside `gen-class`. `orchard.profile` needs
  them.

- **`java.util.Arrays/sort`.** Sorts in place and returns void, so it writes back
  through the array's own backing rather than building a new one, and its
  comparator argument routes through the same seam `sort` uses. `list-sort` is a
  stable merge sort, matching `Arrays.sort` over objects.

- **`clojure.repl`, and a `java.lang.Thread` model.** `run`/`start`/`join`/
  `isAlive` with `instance?` against `Runnable`, `Integer/compare` as a 3-way int
  comparison, `Keyword/table` reflectively visible, and `NoSuchFieldException` in
  the throwable hierarchy.

### Fixed

- **A cached namespace swallowed the install-owned namespaces it required, and
  replaying them undid what its siblings registered.** The AOT capture teed every
  form compiled while a namespace loaded. A cacheable require redirected that — the
  nested `aot-capture-load` opens its own port — but an install-owned require
  bypasses the cache entirely and so never did, and its forms landed in the
  *requiring* namespace's artifact. `jolt.time` is a 14-line `ns` form; it cached as
  412 KB carrying whole second copies of eight `jolt/time/*` stdlib namespaces.

  On a cache hit those copies replay after the require that already loaded them
  properly, so any top-level registration a sibling namespace layered on top gets
  undone. `jolt.time.local` registers an ISO-only `java.time.LocalDate/parse` that
  ignores a formatter; `jolt.time.fmt` overrides it with the pattern-aware one. Cold,
  the override held. Warm, `local`'s baked copy landed back on top of it, and
  `(t/parse-date "2020/02/02" (t/formatter "yyyy/MM/dd"))` silently parsed as ISO —
  `substring: 11 and 11 are not valid start/end indices`. Deterministic: cold green,
  warm red, `JOLT_AOT_CACHE=0` green.

  The capture is now bound to the file it was opened for, so no nested load can
  append to it, whether the cache handles that namespace or bypasses it. `jolt.time`
  caches as 582 bytes — its `ns` form, which is all it has. `clojure.core/compile`
  shares the capture and had inherited the same bug.

- **`*allow-unresolved-vars*` affects resolution.** `clojure.core/*allow-unresolved-vars*`
  read `false` and did nothing; the analyzer consulted a separate
  `jolt.analyzer/*allow-unresolved-vars*` that only jolt's own nREPL knew to bind.
  There is now one var, clojure.core's, read through `jolt.host/allow-unresolved-vars?`
  the way `Compiler.resolveIn` reads `RT.ALLOW_UNRESOLVED_VARS` — and like the JVM
  only for an unqualified symbol with no mapping, so `resolve`/`ns-resolve` still
  answer `nil` and a qualified symbol still throws. Bound true, jolt emits a
  late-bound var-ref in the compiling namespace, so a name defined by a later eval
  resolves; the JVM's `UnresolvedVarExpr` emits no bytecode and fails later with a
  `VerifyError`, which is tracked in `known-divergences.edn`.

- **`apply` realized a lazy rest before every variadic call.** `(apply (fn [& xs]
  (take 3 xs)) (range))` returns on the JVM, where `RestFn.applyTo` hands the seq
  to the variadic arity unrealized; here it allocated until the process died.
  `orchard.profile-test` reaches it through `(apply baz (range))` and took a
  machine down through swap exhaustion. The cause was the calling convention, not
  `apply`: an emitted variadic arity is a plain Chez `(lambda (a b . rest) …)` and
  a Chez rest parameter must be a proper list, so the tail had to be realized to
  cross that boundary — `jolt-apply` had a hard-coded exception for `concat` and
  nothing else. Variadic arities now bind their rest through `jolt-rest-seq`, each
  emitted variadic closure registers its variadic arity's fixed-param count, and
  `jolt-apply` peels that many plus one and passes the remainder as a box. Only
  registered callees ever see a box, which keeps the ~115 hand-written Scheme
  variadics that read their rest list directly working. Peak RSS on the repro goes
  from unbounded past 2.6 GB to 69 MB.

- **The reader took any qualified `->x`/`map->x` call for a record literal's
  factory form.** Ordinary code calls those functions too — `(u/->long n)`
  throughout jolt.time — and the data path applies a factory form, so reading such
  a file as data applied an unbound var. A build reads every source file as data
  to scan its requires, so nothing depending on jolt-lang/time could be built.
  Reader-built factory forms are marked by identity in a weak side table now, like
  `rdr-map-order`, and the name test is gone.

- **`jolt build` emitted the app section with the loader's order appended last.**
  The static require closure drops install-owned files, so a stdlib namespace an
  app namespace requires arrived only through the loader hook and landed behind
  its callers — the binary died at startup with `Attempting to call unbound fn:
  #'jolt.time.impl/register-type!`. The hook's order is dependency-correct by
  construction, so it leads now, with closure entries the hook never saw in front.

- **`proxy-super` was a function**, so its method-name argument was analyzed as an
  expression and `(proxy-super reset)` in `clojure.tools.logging/log-stream`
  failed to resolve `reset`. It is a macro now, as on the JVM, still throwing when
  the body runs since a proxy desugars to a reify with no superclass.

- **`jolt -Sdeps '{…}' build` reached `cmd-build` with `jolt.host/build-binary`
  unbound.** The launcher loads the build driver on demand and looked for `build`
  at argv[0] only, but `-Sdeps` and `-A` re-dispatch the rest of the argv.

- **An unresolved symbol in a nested body was late-bound instead of reported.**
  The analyzer only raised "Unable to resolve symbol" for a symbol at the top
  level of a compilation unit; inside any fn, loop or let body it bound the name
  to a var in the compile ns whose root is an unbound sentinel, so a typo surfaced
  much later as a type error on whichever unbound reference was used
  arithmetically first — usually not the symbol that was misspelled. A missing
  `areduce` presented as `'#[jolt-var-unbound-v1 "orchard.profile" "i"] is not a
  number'. The check applies wherever the symbol appears now, matching the JVM.
  Legitimate forward references are unaffected: `declare` and `(def name)` intern
  a resolvable cell first. Four latent forward references fell out, all of which
  JVM Clojure would reject too.

- **`io/resource` returned two incompatible types.** A path on a source root came
  back as a jfile, carrying a URL-compatibility surface; a path in the embedded-
  resources table came back as a bare record with no methods and no class arm, so
  `.getPath` threw and `(class r)` read `:object`. Which branch served a stdlib
  path depended on whether `target/dev/flat.so` was fresh, which is why
  `orchard.meta-test` kept moving between 63 and 67 passing assertions with no
  relevant code change.

- **`-e` printed its result with the `str`-style printer**, so a nested string
  lost its quotes and a char its reader syntax: `["hi" \c 1]` printed as
  `[hi \c 1]`. Clojure's REPL prints with `pr`, which is what makes a printed
  result read back as the value it names. `nil` still prints as nothing, matching
  `clojure -M -e nil`. `str`, `print` and `println` are untouched.

- **Comparators were coerced in three unrelated places and only one knew about
  Comparator objects.** `sort` tested how the value was built rather than what it
  can do, so a `deftype` Comparator threw ClassCastException, and `sort-by` and
  `sorted-map-by`/`sorted-set-by` invoked the value directly and threw for `reify`
  and `deftype` alike. All four go through one `__comparator-fn` seam now, which
  asks whether the value has a `compare` method. Plain fns, 3-way or boolean, are
  unchanged.

- **An explicit `{:arglists '([x])}` in a `defn`/`defmacro` attr-map was
  discarded**, because both assembled the derived parameter vectors last. The
  derived value is a default the attr-map overrides now, matching the JVM's order:
  name metadata, then derived, then attr-map, then docstring. `^{:arglists …}` on
  the *name* still does not win, because the JVM ignores it there.

- **Conformance fixes from the compliment and orchard suites.** syntax-quote
  lowers metadata as templates, qualifies class tags to FQNs and strips reader
  position keys; `defmacro`/`def` land docstring, attr-map, arglists and
  class-typed `:tag` metadata on the var; `get-method` dispatches through `isa?`;
  jrec gains its collection methods; `jolt-write` goes through a rebound `*out*`
  writer like the JVM; `empty?`/`with-out-str` regressions fixed and an unknown
  object renders as `#object[…]`.

### Changed

- **Static fields are a registry keyed on class + field**, rather than a string
  pair hard-coded inside `Class.getDeclaredField`'s cond. `Keyword/table` is its
  first row.

- **Dropped the unused `__register-seq!` seam** (`get!`/`empty!`/`count!`/
  `contains!`). It had no caller anywhere — not the stdlib, jolt-core, the tests,
  the vendored libraries or the conformance libraries — and never shipped in a
  release, so nothing external can depend on it.

## [0.5.10] - 2026-07-28

An optimized build no longer discards a `throw` written in a map value that the
map's only reader never looks at.

### Fixed

- **An optimized build could swallow a `throw`.** The inline pass admitted
  `:throw` to `safe-op?`, the predicate that marks an IR node as safe to move.
  `pure?` and `total?` both fall through to `safe-op?` for anything that isn't an
  `:invoke`, so both accepted a throw, and `total?` is what `elim-let-structs`
  consults before dropping a scalar-replaced map binding whose values are never
  read. The result was that `(let [m {:a 1 :b (throw "boom")}] (:a m))` folded to
  `1` under `--opt --direct-link`: the binding disappeared and took the throw with
  it, so a release binary returned a value where the interpreter and the JVM both
  raise. `:throw` stays in `safe-op?`, because an inlined body may contain one and
  splicing it preserves it, but `pure?` and `total?` now reject it explicitly. A
  throw is relocatable and never discardable. `test/chez/inline-throw-app` covers
  it end to end through a `--opt --direct-link` build, next to the existing case
  for a throwing sibling behind a non-pure operator like `/`.

### Changed

- **A numeric shim extends the tower through `register-num-arm!` instead of
  assigning core vars.** `java/bigdec.ss` reached into nineteen runtime vars with
  `set!` at load time, which put the core's arithmetic, predicate, cast and
  comparison entry points in a shim's hands and left `predicates.ss` holding the
  jbigdec representation for `==`. `seq.ss` now owns the extension point, in the
  same shape as `register-eq-arm!` and `register-compare-arm!`: a shim hands over
  `(lambda (prev) handler)` and a handler declines what it cannot take by calling
  `prev`. An op name is the runtime var it extends with the `jolt-` prefix
  stripped, so nothing has to be memorized and a name that doesn't follow the rule
  raises when the shim loads. bigdec registers its arithmetic, predicate, cast and
  `==` arms this way and its ordering through the existing `register-compare-arm!`,
  and the core no longer names jbigdec in `==`.
- **The `seqable?` shim check lives with the class table.** `post-prelude.ss`
  carried its own list of `jhost` tags that are `Iterable` on the JVM, duplicating
  knowledge that belongs to `java/host-static-classes.ss`. It now calls
  `jhost-seqable-shim?`, defined next to the `ArrayList`/`HashSet`/`HashMap` arms
  that own it.
- **The Chez compatibility preamble moved to the top of `rt.ss`.** The global
  `error` shadow and the expression-position `cond-expand` shim were defined in
  `regex.ss`, partway through runtime bring-up, so everything loaded before it saw
  an un-normalized `error`. They now sit at the top of `rt.ss`, which every load
  path enters first.

## [0.5.9] - 2026-07-28

An interrupted git fetch no longer leaves a dependency unresolvable on every run
that follows.

### Fixed

- **An interrupted git fetch no longer poisons the dependency cache.**
  `ensure-git` created the cache directory with `mkdir -p` and then cloned into
  it, so the directory existed before the clone had produced anything. Interrupt
  the fetch (a `^C` while `jolt serve` resolves deps is enough, since the
  `SIGINT` reaches the child `git`) and the empty directory stayed behind: `git`
  cleans up a clone directory only when it created that directory itself. Every
  later run found the path and took it for a finished checkout, so the dependency
  contributed no source root and the run failed far from the cause, with `Could
  not locate ring_chez/adapter.jolt (or .clj/.cljc) on the source roots` for a
  dep deps.edn plainly declared. Deleting the directory by hand was the only way
  out. A fetch now clones, checks out, and updates submodules in a staging
  directory beside its destination, and moves it into place only once all three
  succeed, so the cache holds nothing but finished checkouts and a failure at any
  step removes the staging directory instead of leaving a trap. Completeness is
  recorded by a `.jolt-git-ok` marker, with `.git` accepted for a checkout an
  earlier jolt cloned in place, so an already-poisoned cache also heals itself on
  the next run.

## [0.5.8] - 2026-07-27

The AOT cache no longer serves stale code after a length-preserving source edit,
and the gate runs about 2.7x faster.

### Changed

- **The gate runs about 2.7x faster.** `make -j8 ci` drops from 338s to 124s.
  The build-driving gates (`buildsmoke`, `shakelocal`, `depssmoke`,
  `staticnativesmoke`, `buildlibsmoke`) shelled out to source-mode `bin/jolt`,
  where a `jolt build` costs ~12.5s against ~2.5s through a prebuilt binary, and
  `buildsmoke` alone drives 26 of them. They now take `testbin` and default
  `JOLT_BIN` to `target/release/jolt`, the way `smoke` and `cts` already did:
  those five together go from 713s to 161s. `buildsmoke` keeps one explicit
  `bin/jolt` build at the end so the source-mode driver stays gated, and
  `JOLT_BIN=bin/jolt` puts any of them back in script mode. `testbin` itself is
  now rebuilt only when something it bakes in is newer than the binary —
  unconditional rebuilds were free under `make -j ci` but charged every
  single-gate run 18s, enough to make `make buildlibsmoke` slower with the new
  prerequisite than without it.
- **`make gateboot` precompiles the gate boot preamble, taking a pass gate from
  ~1.5s to ~0.14s.** The two dozen gate scripts spent nearly all of their runtime
  loading the same six runtime files from Chez source. `make gateboot` compiles
  exactly that preamble to `target/dev/gate.so`, and the gates use it when it is
  present and newer than everything that went into it, falling back to the source
  loads otherwise. Opt-in and unreferenced by any other target, the way `devboot`
  is, so CI is unaffected unless someone builds it; the win is iterating on a
  single gate. It needs its own image rather than reusing `target/dev/flat.so`
  because that one also loads `loader.ss`, which the gates deliberately skip.
  `JOLT_GATEBOOT=1` reports which path was taken.
- **The runtime boot preamble lives in one file.** Eight gate scripts each
  carried their own copy of the same eight `load` lines; they now load
  `host/chez/gate-boot.ss`. `make-gateboot.ss` generates the image from
  `bld-runtime-manifest`'s prefix rather than a hand-written list, and
  `manifestcheck` pins `gate-boot.ss`'s literal fallback against that same
  prefix, so the runtime, the fallback, and the image cannot drift apart.

### Fixed

- **The AOT cache served stale code after a source edit that did not change the
  file's length.** The cache keys a namespace's compiled fasl on the source's
  byte length plus `equal-hash` of its content, on the assumption that a false
  share needs a collision in both. But Chez's `equal-hash` on a string is not a
  content hash: it samples about 26 characters no matter how long the string is
  (the first 6, roughly 15 strided, the last 5), so for any real source ~99% of
  the bytes never reached the key. Length was doing all the invalidation work,
  and any length-preserving edit — `42` to `99`, `<` to `>`, `inc` to `dec`, a
  rename to an equal-length name — produced a byte-identical key and quietly
  loaded the previous compile. Nothing else catches it: there is no mtime or
  size check behind the filename, so an existing `.so` is treated as valid.
  The cache now keys on a full-content FNV-1a hash, which reads every byte, at
  a cost of one linear pass over source that is about to be compiled anyway
  (6.4ms for all 3MB of runtime source, once per process). The length prefix
  stays on as a second factor.
- **Two jolt builds reporting the same version no longer share a cache
  generation.** The generation directory folds in a fingerprint of the runtime
  itself, precisely because `git describe` reports the same `…-dirty` for every
  build out of one working tree. That fingerprint used `equal-hash` too, so a
  length-preserving change to the runtime left both builds in one generation,
  each loading the other's fasls — the exact failure the fingerprint exists to
  prevent. Both the source-tree fingerprint and the one a binary bakes in now
  use the content hash.
- **`make aotfingerprint`** (added to `ci`) — pins that every byte of a source
  affects the hash, that the hash is reproducible across processes, and that a
  one-character length-preserving edit moves the namespace key, the source-tree
  fingerprint, and the fingerprint a built binary bakes. `aot-cache-smoke`'s
  existing invalidation case did this same `42`→`99` edit and passed throughout,
  because its 36-byte fixture put the change inside the sampled window; it now
  also drives a multi-kilobyte source with the value mid-file.

## [0.5.7] - 2026-07-27

A `.jolt` source extension, `-e` and `:main-opts` fixes across the CLI entry
points, and the arity gate that the 0.5.6 regression would not have survived.

### Added

- **`.jolt` is a source extension alongside `.clj` and `.cljc`.** A namespace can
  live in `foo.jolt`, and it is the same language: the reader, analyzer, and
  emitter never look at the extension. The point is to mark intent, so a reader
  can tell at a glance that a file uses jolt-specific interop and is not portable
  Clojure. It resolves first, ahead of `.clj` and `.cljc`, so a library can ship a
  portable `foo.cljc` next to a `foo.jolt` that wins on jolt, the way `.clj` wins
  over `.cljc` on the JVM. Everything a `.clj` works with works here: `require`,
  a bare `jolt foo.jolt` script argument, `clojure.core/load`, `data_readers.jolt`,
  the AOT cache, and `jolt build`.
- **`make oparity`** (added to `ci`) — every numeric fast-path op at every arity
  it admits, derived from `op-registry` rather than hand-listed, so a
  specialization added later is covered the moment it lands. Each case asserts
  that the specialized form compiles, that it agrees with the generic `apply`
  path, and that the emitted code actually contains the specialization — the last
  one being what the hand-written probes kept missing, since a case that silently
  fails to specialize otherwise passes for the wrong reason. Reverting 0.5.6's
  n-ary fold turns it red on the original symptom.

### Fixed

- **An alias's `:extra-paths` now precede the project's `:paths` on the source
  roots.** jolt appended them instead, which is the opposite of the clj CLI:
  `clojure -A:t -Spath` prints `test src`, jolt's `path` printed `src test`. The
  order decides which copy of a namespace loads, since the loader takes the first
  root that has it, so a `:extra-paths` directory that deliberately shadows one of
  the project's own files was silently ignored. `:extra-paths` also precede an
  alias's `:replace-paths`, and combine in alias-selection order, both matching
  clj. Dependency roots still come last.
- **`-e` composes with `-Sdeps`, `-A`, `-M`, and a project's `deps.edn`.** `-e` was
  handled entirely in the launcher, before `jolt.main` was loaded, so it never
  resolved a project and anything that re-dispatched into `jolt.main` first —
  `jolt -Sdeps '{...}' -e EXPR`, `jolt -A:test -e EXPR` — died with `unknown
  command or task: -e`. `jolt.main` now has its own `-e` arm that resolves the
  project (paths, deps, native libraries) and then evaluates through the same
  launcher primitive, so the two paths cannot drift. A bare `jolt -e EXPR` still
  takes the launcher's fast path when the project directory has no `deps.edn`
  (nothing to resolve, and it skips loading `jolt.main`); with a `deps.edn`
  present it resolves the project, so `jolt -e "(require 'my.app)"` now works
  from a project directory. The stdin forms (`-e -` and a bare `-`) follow the
  same rule.
- **`:main-opts` may be an `-e` expression.** `apply-main-opts` understood only
  `["-m" NS]`, so a `deps.edn` alias or task declaring `:main-opts ["-e" "..."]`
  failed with `unsupported :main-opts`.
- **`-M` takes main options from the command line.** `jolt -M -e EXPR` and `jolt
  -M -m NS` threw `alias(es) [] have no :main-opts`. The selected aliases'
  `:main-opts` now precede the ones given on the command line, matching the clj
  CLI, and when no alias declares any the command line supplies them on its own.
  Only an empty command line with no `:main-opts` is still an error.
- **`(unchecked-add x)`, `(unchecked-multiply x)` and `(unchecked-subtract x)` on
  a `^long` operand no longer crash.** They raised a runtime arity error against
  the two-operand primitive. jolt's own overlay has always taken one operand —
  `(apply unchecked-add [8])` is `8` and `unchecked-subtract` is `-8` — so the
  inline path contradicted the rest of jolt rather than only the JVM, which
  rejects these at one operand outright. Found by the new arity gate.
- **`op-registry` named a bigdec primitive that does not exist.** `"mod"` carried
  `:bd "jbd-mod"`, and nothing anywhere defines `jbd-mod`. It never reached
  emission, since only `quot`/`rem` are assigned the bigdec kind — but the `^long`
  set beside it does list `mod`, so making the two symmetric would have emitted an
  unbound identifier and broken every bigdec `mod` at load. It also reserved the
  name against user shadowing for nothing. Dropped; bigdec `mod` goes through the
  generic path, which was already correct and now has a corpus row.

## [0.5.6] - 2026-07-27

Fixes a 0.5.2 regression: long arithmetic with more than two operands did not
compile at all.

### Fixed

- **`+`, `-`, `*`, `min` and `max` take any number of long operands again.**
  `(+ (long a) (long b) (long c))` failed to compile with `invalid syntax
  (jolt-l+ ...)`. 0.5.2 moved `^long` arithmetic off Chez's variadic `fx+` onto
  jolt's own overflow-checking `jolt-l+`, which is binary, but the back end kept
  splicing every operand into one call, so the expander rejected the form. Any
  3-or-more-operand arithmetic over long-typed operands was affected — an integer
  literal counts as one, so `(+ (long a) (long b) 5)` was enough — while a single
  double operand hid it, the flonum ops being variadic. Under `*unchecked-math*`
  the same splice produced a runtime arity error instead of a compile error.
  N operands now lower as a left fold of the binary op, which is also the
  reference semantics: `(+ a b c)` is `(+ (+ a b) c)`, so each step is
  overflow-checked separately rather than the sum being checked once at the end.
- **`(+ x)`, `(* x)`, `(min x)` and `(max x)` compile over a long operand.** The
  other end of the same gap: a binary op has no one-operand form either, so these
  hit the same syntax error. They emit the operand, which is already coerced and
  needs no further check. `(- x)` and `(/ x)` are not identities and keep their
  call.

## [0.5.5] - 2026-07-27

Two diagnostic fixes: a stack frame now carries its own source location even when
another namespace defines the same function name, and `satisfies?` says what went
wrong instead of throwing an empty message.

### Fixed

- **A trace frame resolves to its own `ns/name (file:line)` when another namespace
  defines the same short name.** The host names a frame after the function's short
  name, so two namespaces defining one name collided in the source registry, which
  dropped the location rather than risk attributing the frame to the wrong file.
  The fallback was right but the collision was constant: every project defines
  `-main` and so does jolt, so the outermost frame of a typical trace never had a
  location. A function that registers a source map now gets a per-var frame name.
  Core keeps its short names — the seed and `jolt build` are unaffected.
- **`satisfies?` on something that is not a protocol names what it was given.** It
  threw with an empty message, so a caller had nothing to go on. Passing a host
  interface still throws, as it does on the JVM; the message now reads
  `satisfies? expects a protocol, got: java.lang.CharSequence`.

## [0.5.4] - 2026-07-26

Ten host-interop fixes, found by running a real library's test suite end to
end. The Cognitect test-runner works against jolt now; before this it reported
`Ran 0 tests` on any project.

### Fixed

- **A list built by `clojure.lang.PersistentList/create` answers `list?`.** It
  reported class `PersistentList` and satisfied `instance?
  clojure.lang.IPersistentList` but `list?` was false, because jolt marks the
  head cell of a real list and that constructor built unmarked cells.
  `clojure.tools.reader` reads every list through it and
  `clojure.tools.namespace`'s `ns-decl?` asks `list?`, so namespace discovery
  found nothing and the Cognitect test-runner ran no tests at all.
- **`clojure.test` deselects a test by its `:test` metadata.** `clojure.test`
  finds tests by scanning vars for that key, so tooling deselects one by
  removing it — the test-runner's `-v`/`-i`/`-e` do exactly that and restore it
  afterwards. `run-tests` ran straight from the registry `deftest` populates and
  never re-read the metadata, so all three options silently selected everything.
- **`.lookingAt` and `.matches` anchor at the region start.** Both anchor there
  on the JVM, not at the cursor `.find` advances, so a `.lookingAt` after a
  `.find` re-anchors at the beginning instead of resuming. A successful match
  now also moves the cursor past itself, so a following `.find` continues after
  it rather than re-finding what was just matched.
- **`unchecked-add-int` and its family wrap at 32 bits.** They were aliased to
  the long ops and wrapped at 64: `(unchecked-multiply-int 100000 100000)` gave
  `10000000000` instead of `1410065408`. Any 32-bit hash mixing was silently
  wrong.
- **`(str x)` uses a deftype's declared `toString`.** It is `x.toString()` on the
  JVM; jolt printed the field map instead. `pr-str` is unchanged, which is the
  same split the JVM makes.
- **The regex functions take a `CharSequence`, not only a `String`.** A library
  matching over a window of a larger string passes its own implementation;
  jolt now realizes one through the type's `toString`.
- **`slurp` of a missing path throws `java.io.FileNotFoundException`.** It threw
  a raw host condition, so a caller catching that class never saw it — a common
  way to test whether an argument is a path or content.

### Added

- **`clojure.core.protocols`**, with the `CollReduce`, `InternalReduce`,
  `IKVReduce`, `Datafiable` and `Navigable` protocols, for libraries that extend
  them to their own types.
- **`clojure.lang.LispReader$StringReader`**, which libraries instantiate to read
  a string literal with Clojure's own escape rules; the literal is parsed by the
  same code jolt's reader uses, so the escapes agree.
- **`java.util.regex.Matcher.lookingAt`** and **`Pattern.flags`**.

## [0.5.3] - 2026-07-26

Stack traces from `jolt run` name their frames the way an AOT build's do.

### Fixed

- **A trace off the runtime path shows `ns/name (file:line)`, not a bare frame
  name.** The renderer could already print the mapped form; nothing populated the
  source map outside an AOT or `JOLT_TRACE` build, so an uncaught error under
  `jolt run` listed bare Chez procedure names with no namespace and no location.
  A fn def now registers its source on the runtime eval path too — one hashtable
  insert per def at definition time, no per-call cost. The tail-frame history
  that recovers TCO-erased frames still costs a push per call and stays opt-in
  behind `JOLT_TRACE`. A frame whose short name is shared across namespaces
  (every project's `-main` and jolt's own, say) keeps printing bare rather than
  risk attributing it to the wrong source.
- **A `def` evaluates to its var when a source registration follows it.** The
  registration was spliced as the last form of a `begin`, so the `def` took its
  `nil` as the form's value and `(pr-str (defn f [] 1))` gave `nil` instead of
  `#'user/f`. Latent since the tail-frame history landed — it only reached code
  compiled with `JOLT_TRACE` set, which the corpus gate never exercised.

## [0.5.2] - 2026-07-26

Compiler-flag and `^long` arithmetic fixes that let clojure/test.check load,
a dropped-argument bug in optimized builds of multi-collection `map`, and three
correctness fixes in the per-namespace compile cache.

### Fixed

- **`map`, `mapv` and `mapcat` over more than one collection work in an
  optimized build.** The inference pass rebuilt such a call as the function plus
  a single collection, so `(map f c1 c2)` compiled to `(map f c1)` and the
  two-argument function was then applied to one element. The runtime compile
  path does not run that pass, so the same source worked under `jolt run` and
  failed only once built. Sibling patterns for `get` and `reduce` truncated an
  over-arity call the same way instead of leaving it for the runtime to report.
- **The compile cache distinguishes the runtime that filled it.** Cached
  namespaces were keyed on the jolt version, which does not identify a build:
  `git describe` reports the same `…-dirty` for every edit in a working tree, so
  successive builds out of one checkout shared a key and each loaded the
  previous runtime's compiled output. Application binaries carry no version at
  all and so shared one key across unrelated programs. Each build now carries a
  fingerprint of its own runtime, and a runtime that cannot be identified does
  not use the cache.
- **Editing a namespace invalidates the ones that depend on it.** A cached
  namespace was keyed on its own source alone, but what it compiled to also
  depends on what its dependencies contributed — macro expansions above all — so
  editing a macro left every consumer running expansions of a definition that no
  longer existed. The key now folds in the key of each namespace required, which
  makes it transitive: a change three namespaces down invalidates the whole
  path.
- **The dev boot cache no longer halves the speed of the code it runs.** The
  image `make devboot` builds loads the build subsystem eagerly, and that turns
  per-site var-cell caching off so the seed mint and `jolt build` stay
  byte-deterministic. Since the image saved that setting, every namespace
  compiled at runtime under the cache resolved each var by name on every access
  — about half speed on var-reference-heavy code, in a cache whose purpose is
  faster iteration. It restores the runtime setting after loading the build
  driver. Released binaries were never affected; they load that subsystem
  lazily.
- **A file's top-level `(set! *unchecked-math* true)` works.** It threw "Can't
  change/establish root binding"; the reference binds the var around every file
  load, so the form is legal and its effect ends with the file. The loader bound
  `*warn-on-reflection*` and `*assert*` but not this one, and the AOT path lost the
  effect separately — an optimized build decides whether `+`/`-`/`*` lower to their
  wrapping forms while it emits, which happens before the boot-time `set!` runs, so
  the flag is now applied as emission walks past it. clojure/test.check sets the
  flag at the top of `random.clj` and so could not be loaded at all.
- **`^long` arithmetic covers all 64 bits.** `+`, `-`, `*`, `inc` and `dec` on
  `^long` values raised once a value passed the Chez fixnum boundary at 2^60 —
  `(- x 1)` on an ordinary long threw instead of computing. They now compute
  generically and throw `ArithmeticException` at the 2^63 edge the hint actually
  promises, matching the reference on both the value and the message. The other
  long ops already did this; `+ - *` were left on the raw fixnum ops on the
  assumption that `*unchecked-math*` rewrote them first, which only holds when the
  flag is on.

### Changed

- **Superseded compile-cache generations are collected.** Keying on the runtime
  means the cache moves whenever jolt is rebuilt, so a build loop would leave a
  full generation behind each time. A run now keeps the three most recently used
  and drops the rest. `JOLT_CACHE_DIR` still selects the location, and
  `JOLT_AOT_CACHE=0` still opts out.

## [0.5.1] - 2026-07-26

Optimized builds type recursive walks over record trees, and a
devirtualization bug that could return a wrong value is fixed.

### Fixed

- **A protocol call on a record-or-nil receiver no longer devirtualizes.** In an
  optimized closed-world build the inference took the devirtualization target
  straight off the receiver's type without checking whether it could be nil, and
  because such a site caches its first resolution, a receiver that was a record
  on one call and `nil` on the next got the cached implementation instead of a
  dispatch. With a record that has no fields that returned a wrong value where
  Clojure raises `IllegalArgumentException`; with fields it surfaced as an
  untyped host error. Only a receiver proven non-nil devirtualizes now — a
  `some?`/`nil?` guard narrows one back, so the fast path is kept wherever it is
  sound.

### Changed

- **Recursive walks over record trees are typed and read fields by slot.** The
  whole-program pass could not follow a record through a nilable recursive
  position, so a tree walker's parameter stayed untyped and every field read
  went through the generic keyword lookup. Four things blocked it: a `defn`'s
  self-recursive call resolves through the function's own name rather than its
  var and so never picked up the function's inferred return type (which meant a
  recursive constructor poisoned its own record's field types); the parameter
  fixpoint had no priming phase, so a recursion whose argument is computed from
  the parameter pinned it at the top type; joining two views of the same record
  widened any field only one side carried; and a field read off a record-or-nil
  discarded the field's type entirely. `binary-trees` runs 2.5× faster
  (165ms→67ms, about 1.8× JVM Clojure). The first two apply to any
  self-recursive `defn` in an optimized build.

## [0.5.0] - 2026-07-25

The CLI is `jolt` now, not `joltc` — a rename worth a minor version even
though `bin/joltc` still works as a shim. Alongside it: `deps.edn` handling
that follows tools.deps rather than approximating it, Linux binaries that run
on distributions back to CentOS 7, and another round of numeric performance
work.

### Added

- **Dependency resolution matches tools.deps.** The expansion engine is now the
  one from `clojure.tools.deps` — version map, exclusion tree, and orphan
  cutting ported directly — so `:exclusions` are honored, a library pulled at
  two versions resolves to the newest (a top-level coordinate still pins), and
  children orphaned by that choice are dropped. Maven versions order by
  ComparableVersion semantics without a JVM; git coordinates compare by commit
  ancestry. New coordinate handling: `:local/root` may point at a `.jar` (it is
  extracted and its pom supplies transitive deps), and `:git/tag` with a short
  `:git/sha` resolves the tag to its commit and verifies the prefix. Custom
  `:mvn/repos` are consulted after Clojars and Central.
- **The deps.edn chain and the tools.deps CLI surface.** A user `deps.edn`
  ($CLJ_CONFIG, else $XDG_CONFIG_HOME/clojure, else ~/.clojure) merges under
  the project's; `-Sdeps '<edn>'` merges an extra map last; `JOLT_NO_USER_DEPS`
  opts out of the user file. `-X:alias [ns/fn] [k v …]` invokes `:exec-fn` with
  `:exec-args` (k v pairs and a trailing map merge over it, `:ns-default` /
  `:ns-aliases` qualify the symbol), and `-T:alias` does the same with the
  project's own paths and deps replaced by the tool's. Libraries declaring
  `:deps/prep-lib` are named in a warning, since jolt runs no prep step.
- **deps.edn aliases follow tools.deps semantics** (#453). Alias maps combine
  with the reference merge rules — lifted directly from
  `clojure.tools.deps.edn` into `jolt.deps.edn` — and the full args-map key set
  applies: `:extra-deps`, `:override-deps`, `:default-deps`, `:replace-deps`
  (legacy `:deps`), `:extra-paths`, `:replace-paths` (legacy `:paths`), and
  last-wins `:main-opts`. A leading `-A:…` now carries its aliases into the
  dispatched command, so `jolt -A:jolt path`, `-A:x -M:y`, and `-A:… build`
  all resolve with them. An undeclared alias is an error, as in tools.deps.
- **The jolt-lang/time library autoloads from the source roots.** Referencing a
  `java.time` formatting/zone class (`ZoneId`, `ZonedDateTime`,
  `DateTimeFormatter`, …) with the library on the deps loads its `jolt.time`
  install namespace automatically — `(require '[tick.core :as t])` works
  directly in a project that declares the dependency, e.g. via a `:jolt` alias.

### Changed

- **The CLI is named `jolt` now, not `joltc`.** The dev launcher is `bin/jolt`,
  the built binary is `target/<profile>/jolt` (`jolt.exe` on Windows), release
  archives are `jolt-<version>-<target>.tar.gz` containing a `jolt` binary, and
  the Makefile targets are `jolt` / `jolt-release` / `jolt-debug` / `joltsmoke`.
  `bin/joltc` remains as a compatibility shim that execs `bin/jolt`. The
  `install` script installs `jolt` and falls back to the `joltc`-named assets
  for releases up to 0.4.15; the Homebrew formula switches to the new name
  automatically on the next release bump. The cross-compile variable
  `JOLTC_TARGET` is now `JOLT_CROSS_TARGET`.
- **Linux release binaries run on much older systems** (#455, fixes #452). The
  Linux build now happens inside manylinux2014 with ncurses/tinfo/zlib linked
  statically, dropping the glibc requirement from 2.35 to 2.17 — so the
  published binary runs on CentOS 7+, Ubuntu 14.04+, Debian 8+, and Amazon
  Linux 2+ instead of demanding a 2023-or-newer distribution.
- **Out-of-range `aget`/`aset` on a primitive array throws
  `ArrayIndexOutOfBoundsException`** with the JVM's message (#458). It
  previously surfaced as an untyped host condition that `(class e)` reported as
  `:object` and that a `catch` of an unrelated exception class could swallow.

### Performance

- **Mixed `long`/`double` arithmetic no longer falls off the fast path** (#454).
  An integer operand in an otherwise-flonum expression now coerces instead of
  forcing generic dispatch, and integer-literal loop counters take JVM `long`
  semantics. `mathfns` went from ~22.7× the JVM to ~1.5×, `loop-recur` from
  ~8.3× to ~1.6×, `mandelbrot` to ~1.6×.
- **Primitive `double` array access is roughly 3× faster** (#457, #461). The
  index takes a fixnum-first path, and — where the pass has proven the array
  and index types — the backend now emits the flvector read/write inline
  instead of calling a wrapper whose flonum return had to be re-boxed on every
  element. The `arrays` benchmark went from ~18.6× the JVM to ~6×.
- **Generic `inc`/`dec` open-code their numeric fast path** (#458) rather than
  calling through a procedure, matching how `+`/`-`/`*` already worked.

## [0.4.15] - 2026-07-22

Two numeric fast paths for hot array and math code, both hint- and
inference-driven.

### Changed

- **Primitive `double`/`float` arrays are unboxed.** A double/float `jolt-array`
  is now backed by a Chez flvector (unboxed flonums) rather than a boxed vector;
  the collection dispatchers (`count`/`seq`/`nth`/`ref-put!`/`aclone`) and
  `java.util.Arrays` go through backing-agnostic helpers, so behavior is
  unchanged. A `^doubles`/`^floats` array hint — on a param **or** a `let`-binding
  — lets `(aget a i)` lower to a direct `flvector-ref` typed `:double` and
  `(aset a i v)` to a direct `flvector-set!`, so a read/fill loop over a primitive
  array stays unboxed on both ends and the surrounding arithmetic unboxes to
  `fl+`/`fl*` instead of the generic `jolt-nth` + numeric-tower path.
- **`java.lang.Math` over proven flonum operands lowers to the native op.** When
  every operand is a `:double` (or an int literal coerced to one, with at least
  one genuine double), a Math call emits the native Chez flonum op
  (`flsqrt`/`flatan`/`flexpt`/…) typed `:double`, so it keeps flonum contagion in
  the enclosing expression. Untyped args and all-integer forms like `(Math/abs 5)`
  stay the generic host-static call.
- **Hot 1-dim `aget`/`aset`/`alength` lower to the array-aware native ops**
  (`jolt-nth`/`jolt-aset3`/`jolt-count`), skipping the clojure.core overlay's
  var-deref + reduce/seq alloc. Multi-dim forms fall back to the overlay.

## [0.4.14] - 2026-07-21

### Fixed

- A GUI app started from an nREPL session no longer aborts on macOS with "API
  misuse: setting the main menu on a non-main thread." The nREPL server parks in
  `park-until-interrupt` (for clean `^C` shutdown), which did not activate the
  main-thread pump, so `jolt.host/call-on-main-thread` fell through and ran
  `g_application_run` inline on the nREPL worker thread — GTK quartz aborted when
  it set the main menu off the main thread. `park-until-interrupt` now doubles as
  the pump: it drains queued jobs and runs each on the primordial thread, idling
  via an interrupt-checked `sleep` poll so `^C` is still delivered and the shutdown
  hooks still run.

### Added

- **`jolt.host/call-on-main-thread-async`** — a fire-and-forget hop onto the main
  thread, so a GUI framework's `run` can schedule the boot and return immediately,
  leaving the nREPL session live for reactive edits. The blocking
  `call-on-main-thread` and the external `run-main-pump` pump API are unchanged.

## [0.4.13] - 2026-07-21

### Changed

- Closed the reader-side half of the lazy-realization race. The tail force
  (`seq-more`) and lazy-node force (`force-lazyseq`) kept an unlocked fast-path
  read of the realized flag and value as their first case, taken even in
  multi-threaded programs. Because the writer stores the value and the flag with no
  barrier between them, on a weak memory model like ARM64 a lock-free reader could
  observe the flag set while the value was still the thunk, the same leaked-closure
  crash the earlier writer-side fix described. Once a second thread exists, every
  access now goes through the per-node and per-cell mutex, reads included, so the
  reader synchronizes against the writer's release. Single-threaded programs keep
  the fully lock-free fast path. Host-runtime change only; the minted seed is
  unaffected.

- Fixed a latent concurrency bug in lazy sequence realization. A `cseq` seq cell
  memoizes its tail under mutable `tail`/`forced?` fields with no synchronization,
  so once a lazy-seq node was realized its underlying cell chain was shared across
  threads (every future/agent walking the same seq reads the *same* cells) and two
  threads could both force a cell's tail and publish the fields non-atomically — a
  third reader could then observe `forced?` set with `tail` still the thunk,
  leaking a closure out as a seq and crashing. The cell's tail force (`seq-more`)
  is now guarded by a lazily-created per-cell mutex under the same `jolt-mt?` flag
  the lazy-seq node uses, so it stays lock-free in single-threaded programs and
  takes double-checked locking once a second thread is spawned. This was exposed
  by the new concurrent-realization unit guard and is otherwise unchanged in
  behavior; the minted seed is unaffected (host-runtime change only).

- Lazy sequences are noticeably faster in single-threaded programs, which is the
  common case. Every lazy-seq node used to allocate an OS mutex when it was created
  and acquire it on first realization, so that concurrent futures or agents could
  not run the body twice. But iterate, repeat, cycle, and every map/filter chunk
  tail is a lazy node, so idiomatic pipelines paid a mutex allocation and lock per
  node, and per element for iterate. A node now carries no mutex and realization
  takes no lock until the process actually spawns a second thread. A global flag
  flips the first time `fork-thread` runs (via future, agent, core.async, or a
  subprocess), after which realization falls back to the original double-checked
  locking on a mutex created lazily per node. This is race-free because a single
  thread is either forking or forcing, never both, and `fork-thread` establishes
  happens-before so the spawned child sees the flag. The lazy-seq/HOF benchmark
  (`bench/seqs.clj`) drops about 26% (roughly 1200ms to 890ms on an M-series
  machine); tight-loop and persistent-collection benchmarks are unchanged.
- `every?`, `some`, and the `not-every?`/`not-any?` derived from them are faster
  over chunked collections. They walked a sequence cell by cell with
  `seq`/`first`/`next`, which re-coerces to a seq and allocates a step cell per
  element and threw away the tight index loop a chunked source like `range` or a
  vector already supports. They are now expressed over `reduce` and `reduced`, so a
  chunked source drives `reduce`'s index loop while a lazy seq is still stepped one
  cell at a time, and `reduced` preserves the short-circuit and laziness (an
  infinite seq with an early counterexample still terminates). A scan-heavy probe
  (`every?` over many small ranges) drops about 30%. The `bench/seqs.clj` aggregate
  is unchanged because its `every?` component is a small slice of the total; the win
  shows up in code that leans on these predicates. `every?` lives in the kernel tier
  so it is expressed with `fn*`, and both functions are part of the minted seed.
- Chunked `map` and `filter` allocate less per chunk. Both realized a chunk by
  copying the source vector's 32-element trie leaf into an intermediate pvec and
  then re-reading that copy to build the output, and `filter` additionally built a
  Scheme list, reversed it, and converted it to a vector. They now read the source
  leaf directly into the output chunk, skipping the intermediate copy (and, for
  `filter`, the list round trip). The leaf resolution is identical to the previous
  code, so chunk boundaries and the rare non-leaf-aligned window behave exactly as
  before. On its own this is in the noise on `bench/seqs.clj` because the per-chunk
  lazy-seq node cost dominated; stacked with the lazy-seq mutex change it accounts
  for roughly another 4% (about 30ms) on that benchmark.
- Transducers are faster and no longer slower than the equivalent lazy pipeline.
  Each transducer stage's reducing fn was a variadic `(lambda a (case (length a)
  …))`, so calling it with two arguments allocated a rest list and walked
  `(length a)` on every element, then forwarded through the general variadic
  invoke. Each stage is now a `case-lambda` with the exact transducer arities
  (init, complete, step), so there is no rest list and no length check, and the
  step calls the downstream reducing fn and the mapping/predicate fn through the
  fixed-arity fast paths. The map transducer keeps a trailing variadic clause for
  its multi-input arity, so the single-input hot path stays allocation-free. On a
  2,000,000-element pipeline `transduce` drops about 23% and `into` with an xform
  about 22%; a transducer pipeline is now faster than the lazy-seq equivalent, as
  it should be, where before it was slower.
- The `jolt build` subsystem no longer runs at every startup. Following #433,
  `build.ss` (with the `emit-image.ss` and `dce.ss` it inlines) was still emitted
  as eager top-level forms in the boot image, so its ~100 defines ran on every
  process start, and the runtime `.ss` source it reads during a build was
  registered into the resource table each start — about 20ms on every invocation,
  none of it touched by `run`, `-e`, `repl`, `version`, or any non-build command.
  The fully-inlined build subsystem is now baked as a source string and `eval`ed
  into the top-level environment on the first `jolt build`, with the `.ss` embed
  registration moved into a thunk fired at the same point. Non-build invocations
  pay nothing.
- Iterating a record's keyword map is about 1.8x faster. The persistent array
  map's order list now carries `(key . value)` pairs instead of bare keys, so the
  `pmap-fold` / `pmap-fold-fwd` iteration scans entries directly instead of doing
  one HAMT lookup per key — the hot path for folding the 64-entry keyword maps
  defrecord ext maps produce (a full `vals` fold drops from ~5.9us to ~3.2us). A
  new `order-replace` updates a replaced key's value in place so `assoc` on an
  existing key keeps both its position and value; no behavioral change.

## [0.4.12] - 2026-07-21

### Changed

- joltc startup is about 4x faster. The CLI entry namespaces (`jolt.main`,
  `jolt.deps`, and their on-demand Clojure closure) were baked into the binary as
  `(load-namespace …)` boot forms, which re-analyzed and re-emitted them from
  Clojure source on every process start because a Chez boot file re-runs its
  top-level forms each `Sbuild_heap`. That was roughly 380ms, about 70% of the
  startup floor, paid by every invocation. They are now emitted to Scheme at build
  time via the same path an app build uses and marked loaded, so at boot the vars
  are installed by running compiled code like the rest of the runtime image. A
  trivial `joltc prog.clj` drops from ~0.51s to ~0.12s. This is the base floor the
  per-namespace AOT cache (0.4.10) could not touch, since install-owned namespaces
  are never cached.
- joltc startup drops a further ~15% (~130ms to ~110ms on an M-series machine).
  The build subsystem (`build.ss` plus the `emit-image.ss`/`dce.ss` it inlines) and
  the runtime `.ss` source embeds it reads are used only by `jolt build`, but they
  ran their defines and registered their bytes at every startup. They are now baked
  as source and loaded lazily on the first `jolt build`, so `run`, `-e`, and every
  other command skip them. No behavior change to `jolt build`; the standalone binary
  is also slightly smaller.

## [0.4.11] - 2026-07-20

A base java.time API in core that works with no dependency, as a single
implementation rather than two (RFC 0008). Core previously registered a partial
java.time surface in Scheme (Instant, LocalDateTime, ZoneId, DateTimeFormatter,
FormatStyle) that was both incomplete — `Instant/now` worked, `LocalDate/now` did
not — and a second copy of logic the jolt-lang/time library already implements in
Clojure. The Scheme shim is gone; the boundary now sits at the java.time value
types.

### Added

- **java.time value types in core, no dependency.** Instant, LocalDate /
  LocalTime / LocalDateTime, Duration, Period, Year / YearMonth / MonthDay, and
  the Month / DayOfWeek / ChronoUnit / ChronoField enums live in core under
  `stdlib/jolt/time` as portable Clojure, aggregated by a core-owned
  `jolt.time.base` that autoloads the first time interop resolves one of them. A
  date-free program never triggers the load, so it pays nothing.

### Changed

- **Formatting and zones are the jolt-lang/time library, as the single
  implementation.** DateTimeFormatter, FormatStyle, ZoneOffset, ZoneId,
  ZonedDateTime / OffsetDateTime, localized formatting, and java.util.Locale are
  the library — core carries no second copy. Referencing one of those without the
  library now gives an error that names the dependency instead of a bare "Unknown
  class", on both the static and constructor paths.
- **`.toInstant` bridges through the base.** `inst-time.ss` keeps the
  always-available java.util / java.text layer (Date, sql.Date/Timestamp,
  Calendar, TimeZone, SimpleDateFormat) and the `#inst` literal; its `.toInstant`
  routes a Date, `#inst`, or FileTime to the base Instant, autoloading the base so
  the bridge needs no dependency. `now()` fixes on UTC in the base; the library
  refines it to the system zone.

### Removed

- **The Scheme java.time shim.** The partial Instant/LocalDateTime/ZoneId/
  DateTimeFormatter/FormatStyle surface previously implemented in Scheme is
  removed in favor of the Clojure base.

## [0.4.10] - 2026-07-20

Per-namespace AOT/compile cache for required libraries: a disk-backed cache that
fasls a required namespace's emitted Scheme on first load and loads the `.so` on
subsequent runs, recovering most of the per-run recompile cost for library
requires. Keyed by source content hash + jolt version, so any source edit or
compiler change misses automatically. Default ON for a built `joltc`; the dev
`bin/joltc` opts out (volatile dev compiler). Measured ~28% startup speedup on a
4-library require (cold 2.81s → warm 2.03s).

### Added

- **Per-namespace compile cache.** When a disk-backed namespace is required, the
  emitted Scheme is teed off the existing load path (preserving the interleaved
  analyze→eval semantics — forward macro refs, `defrecord`/`defprotocol`, data
  readers, and transitive requires all reproduce on cache hit) and fasled to
  `~/.jolt/aot-cache/<jolt-version>/v1/<ns>-<content-hash>.so`. The cache filename
  embeds a content hash of the source, so editing a namespace invalidates it
  automatically (no mtime tracking). On the next run the `.so` is loaded directly
  instead of recompiled.
- **`JOLT_AOT_CACHE` env var** — `0`/`false`/`no`/`off` opt out. Default ON for a
  built `joltc`; the dev `bin/joltc` script sets it to `0` (a volatile dev compiler
  whose "dev" version tag wouldn't invalidate across edits, and whose startup is
  already covered by the devboot cache).
- **`JOLT_CACHE_DIR` env var** — override the cache root (default
  `~/.jolt/aot-cache`). Useful for tests and CI isolation.
- **Safety gates.** Install-owned namespaces (embedded in the binary) are never
  cached; `:reload` / `:reload-all` bypass the cache so live editing always wins.
- **`make aotcachesmoke`** (added to `ci`) — deterministic correctness gate:
  miss/hit/invalidate, macro def-then-use, `defrecord`, data readers, transitive
  require, `:reload` bypass, install-owned never cached.
- **`make aotcacheperf`** — cold-vs-warm wall-clock measurement (needs Maven jars
  locally; not in the default gate).
- **Git deps can omit `:git/url`.** A git coordinate whose lib name encodes a
  known host resolves its clone URL the way tools.deps does — `io.github.OWNER/REPO`
  clones from GitHub, and likewise for `com.github.*`, `io.gitlab.*`/`com.gitlab.*`,
  `io.bitbucket.*`/`org.bitbucket.*`, and `ht.sr.*` (Sourcehut). An explicit
  `:git/url` still wins; a git coordinate with neither a URL nor an inferable host
  now reports an actionable error naming the fix instead of being silently skipped.

## [0.4.9] - 2026-07-20

Better compile diagnostics, borrowing a few ideas from Carp: near-miss name
suggestions, machine-readable error output, and an opt-in success-type lint.

### Added

- **"Did you mean?" suggestions for an unresolved symbol.** When a bare symbol
  doesn't resolve at the top level, the compile error now lists the closest
  in-scope names by edit distance, drawn from the current namespace's vars,
  `clojure.core`'s publics, and the lexical locals. `(prinltn 1)` reports
  `Unable to resolve symbol: prinltn in this context (did you mean print, printf,
  println?)`. A symbol with no near match still gets the bare message.
- **Machine-readable diagnostics (`JOLT_DIAG=edn`).** With `JOLT_DIAG=edn`, an
  uncaught error is emitted as a single line of valid EDN to stderr instead of
  the human report, so an editor or tool can read it back. The map carries the
  human `:message` and the source `:line`/`:column`/`:file`; an unresolved-symbol
  error also carries structured `:type`/`:symbol`/`:suggestions`/`:ns`. The
  underlying analyzer error now attaches that data as ex-data on the thrown
  `ex-info`, so it is available programmatically even in the default human mode.
- **Opt-in success-type lint (`JOLT_CHECK`).** With `JOLT_CHECK` set to a truthy
  value, each runtime-compiled top-level form is run through the existing
  success-type checker (RFC 0006) and any findings print to stderr as located
  warnings, e.g. `1:10: warning: \`+\` requires a number, but argument 2 is a
  keyword`. Off by default (zero cost, no behavior change); a checker error never
  breaks a compile. Only runtime-compiled code is linted — `clojure.core` and the
  prelude are baked into the seed at build time and are not re-checked.

## [0.4.8] - 2026-07-20

Dependency downloads no longer shell out to `curl` — jolt fetches Maven jars over
its own cert-verifying HTTPS.

### Changed

- Maven dependencies download over HTTPS through jolt itself instead of shelling
  out to `curl`. A new `jolt.mvn-http` does a minimal cert-verifying HTTPS GET
  (a raw socket via `jolt.ffi`, TLS over the system OpenSSL with peer verification
  + SNI + hostname check), so a dependency jar is never fetched over an
  unauthenticated transport. It loads the real OpenSSL lazily — on macOS the
  Homebrew `openssl@3` (the protected `/usr/lib` copy can't be loaded); on Linux
  the distro `libssl`/`libcrypto`; on Windows the `libssl-3-x64.dll` DLLs via
  Winsock (the Windows path is implemented but not yet validated on a Windows
  host). A repo that can't be reached is skipped, non-fatal. `git` (git deps) and
  `unzip` (jar extraction) are still shelled out; `curl` is gone.

## [0.4.7] - 2026-07-19

Library conformance (flatland/ordered; babashka.http-client via jolt-lang/http-client),
a proxy-over-interface fix, Maven cache invalidation, and quieter default output.

### Added

- **`pr`/`pr-str` honor a user `(defmethod print-method SomeType …)`** for a
  deftype/defrecord, rendering through it into a `StringWriter` instead of the
  default `#ns.Name{…}` form. An unqualified `(defmethod print-method …)` in a
  library loaded via `require` now extends `clojure.core/print-method` (implicit
  `refer-clojure`) instead of building a dead per-namespace shadow.
- **Java collection interop on native collections.** `.cons`/`.assoc`/`.without`/
  `.assocN`/`.disjoin`/`.pop`/`.asTransient`/`.hashCode` and the transient
  `.conj`/`.persistent`/… now dispatch on a vector/map/set (and its transient), so
  a deftype built on the `clojure.lang.*` interfaces (flatland/ordered) works.
  `conj`/`contains?`/`disj`/`get`/`transient`/`persistent!`/`hash` route to a
  deftype's own `cons`/`containsKey`/`disjoin`/`get`/`asTransient`/`persistent`/
  `hasheq` methods. `instance?` on a deftype now walks its declared interfaces'
  ancestry (a `IPersistentMap` is a `Counted`/`Associative`/`IPersistentCollection`).
- A collection's `.hashCode` is now the `java.util.Map`/`Set`/`List` hashCode
  (previously the Murmur3 hasheq), so a jolt collection hashes equal to a library
  type computing the Java hashCode. `clojure.lang.APersistentMap/mapHash` is shimmed.

### Changed

- Progress/informational output is quiet by default; set `JOLT_DEBUG` to surface
  it. `jolt.deps` no longer prints its fetching / using-cache / skipping /
  added-natives lines on a routine run (a program pulling a native-declaring
  library used to barf a `[jolt.deps] … not auto-loaded` line every time), and the
  "static member registered twice" drift warning — which also fires when two
  libraries legitimately shim the same class — is likewise gated. Genuine
  problems (an unresolvable dependency, a failed extraction, a malformed
  `deps.edn`) still print unconditionally. `JOLT_DEBUG` is the knob to re-enable
  the diagnostics when debugging dependency resolution or static-shim drift.

### Fixed

- `(proxy [SomeInterface] [] (method [args] body))` now works — it desugars to a
  `reify` over the same interfaces (`this` is bound in the method body), instead of
  throwing "proxy is unsupported". Only `(proxy [ThreadLocal] …)` keeps its
  dedicated form. This unbreaks clj-http-lite (its trust-all `HostnameVerifier`)
  and any library that proxies an interface. `proxy-super` / calling an inherited
  concrete superclass method is still unsupported (jolt has no superclass).
- A bare imported deftype/defrecord class name resolves to its class value, equal
  to `(type instance)` — `(= SomeType (type inst))` holds, and a flat
  `(:import a.b.Type)` binds the name.
- Maven jar extraction re-extracts when the jar is newer than the last extraction
  (`.jolt-ok` was trusted forever, so a rebuilt/refetched jar — a SNAPSHOT, or a
  coord reinstalled into `~/.m2` — was never re-read), and the `.jolt-ok` marker
  is written only after a successful `unzip`, so a failed/partial extraction is no
  longer cached as complete.

## [0.4.6] - 2026-07-19

Cross-compilation, and a stdin namespace-switch fix.

### Added

- **`jolt build --target <machine> --target-pack <dir>`** cross-compiles an app
  for another Chez machine, and `build-joltc.ss` cross-builds joltc itself — which
  restores the x86_64-macos release artifact (cross-built on the arm64 runner).
  `build.ss` already split at the machine boundary (steps 1-3 emit machine-neutral
  `flat.ss`); a cross build retargets only step 4 under the target pack's Chez
  `xpatch` and links the target kernel with the target cc. A "target pack"
  (assembled by `tools/cross-compile/make-pack.sh` from a ChezScheme cross
  checkout) supplies the boots/kernel/`scheme.h` + xpatch + link-libs + static
  lz4/zlib. `--library` and `:jolt/native` cross builds are not supported yet.

### Fixed

- **`jolt -` / `-e` honor an `(ns …)` switch.** Stdin and `-e` compiled every
  top-level form in a hardcoded `user` namespace, so an `(ns …)` form's switch was
  ignored — a later `(refer …)`/`def` into the switched-to namespace wasn't visible
  to a following form's analysis. They now compile each form in the current
  namespace, re-read per form, like loading a file. Fixes
  `ys -T jolt prog.ys | jolt -` (a compiled YAMLScript program switches ns then
  refers its stdlib in). Process substitution `jolt <(ys …)` already worked.

## [0.4.5] - 2026-07-19

Sub-process working-directory fixes: a spawned child now runs in the user's cwd,
and an unresolvable program fails like the JVM.

### Fixed

- **Spawned children inherit the user's cwd.** A child process ran in jolt's OS
  cwd — the repo root its launcher `cd`s to — instead of the user's cwd. A JVM
  child inherits `user.dir`; jolt preserves the user's cwd in `JOLT_PWD`, so a
  child now defaults to `JOLT_PWD` and a relative `:dir` resolves against it (like
  `ProcessBuilder.directory`). Before, `(sh ["ls"])` listed the jolt repo.
- **`System/getProperty "user.dir"` is the user's cwd.** It returned `PWD` (the
  repo root after the launcher's `cd`); it now prefers `JOLT_PWD`, so `user.dir`
  and spawned-child cwds agree.
- **Missing program throws like the JVM.** `ProcessBuilder.start` resolves the
  program before spawning and throws `IOException("…No such file or directory")`
  when it can't be found (absolute / slash-relative file must exist; a bare name
  must be on `PATH`), instead of letting the shell fail at `exec`.

## [0.4.4] - 2026-07-19

Sub-process support: `jolt.process` (vendored babashka.process) runs over a new
host `ProcessBuilder` / `Process` layer, with `clojure.java.shell` alongside it.
Also a build-scanner fix so `joltc build` no longer chokes on `::alias/kw`
keywords.

### Added

- **`jolt.process` — sub-process spawning.** `jolt.process` is now the
  [babashka.process](https://github.com/babashka/process) API. Jolt vendors
  babashka.process over a new `java.lang.ProcessBuilder` / `java.lang.Process`
  host shim (`host/chez/java/process.ss`) built on Chez `open-process-ports` for
  stdin/stdout/stderr pipes plus the pid, and libc `waitpid` / `kill` (via FFI)
  for exit codes, liveness and signals. `process`, `sh`, `shell`, the `$` macro,
  `check`, `pipeline`, `destroy`/`alive?` and stream/file/inherit redirects all
  work; `:dir`, `:env`/`:extra-env`, and stdin feeding are supported. `exec`
  (GraalVM-only) is not re-exported. `jolt.process` is the curated public surface
  (require `babashka.process` directly if you prefer).
- **`clojure.java.shell`.** The standard `clojure.java.shell/sh` (and
  `with-sh-dir` / `with-sh-env`) now run, over a new `Runtime.exec` host method
  and the ProcessBuilder shim.
- **Shim class identity is registry-derived.** The new `ProcessBuilder` /
  `Process` / `ProcessBuilder$Redirect` shims register one tag→FQN row each in the
  central `jhost-tag->fqn` registry, so `instance?` and `class` derive from the
  class graph with no per-class arm — the same seam the java-value shims use.
- **AOT namespace pre-registration.** A `jolt build` binary now registers every
  namespace in its closure before any app form runs, so a load-time `(require 'x)`
  of an AOT'd namespace no-ops instead of hunting for absent source — needed by a
  namespace that conditionally requires a later, var-less one (babashka.process
  requires babashka.process.pprint, which only carries a `defmethod`).

### Fixed

- **`joltc build` require scanner accepts `::alias/kw` keywords.** The build's
  namespace scanner reads source before any namespace is loaded, so an
  alias-resolved auto keyword can't resolve yet; it is now read leniently in a
  scanner-scoped mode instead of failing the build with `Invalid token: ::alias/kw`.
  (#416)

## [0.4.3] - 2026-07-18

Compiler architecture consolidation: the per-op fact tables, the IR schema, the
`jolt.host` surface, and the class model each get a single source of truth, and all
~25 module-level pass/inference/emit atoms fold into one per-compilation context so
the compiler is reentrant. Plus var-metadata parity (`:ns` is a `Namespace`, defs
carry source position) and a fix for declaration-only vars in built binaries.

### Added

- **Var metadata carries source position.** `def`/`defn` attach `:line`/`:column`/
  `:file` to the var, like the JVM Compiler — what `spec` instrument and `expound`
  read. User code and loaded files carry it; a `clojure.core` var minted without a
  reader position does not (documented).
- **`:ns` in var metadata is a `Namespace`.** `(class (:ns (meta #'x)))` is now
  `clojure.lang.Namespace` instead of a string; it still prints as the namespace name.

### Changed

- **One compilation-unit context.** The pass/inference/emit state that lived in ~25
  module-level atoms across the analyzer passes and the back end now lives on a single
  per-compilation `unit`, threaded explicitly. A compilation is reentrant — two units
  never see each other's state — and the whole-program fixpoint no longer mutates
  shared config while a checker reads mid-estimates. The three record-shape registries
  collapse into one install.
- **Per-op facts derive from one registry.** The `native-ops`, fast-path, arity, and
  classifier tables the back end and the passes need are derived from a single
  `jolt.op-registry`, so a per-op fact is edited in one place and the mirrors can't
  drift.
- **The IR schema is written down and validated.** `jolt.ir` documents every op and its
  required keys; `JOLT_IR_VALIDATE` checks each form entering and leaving the pass
  pipeline in dev.
- **The `jolt.host` surface is pinned.** A manifest lists every `jolt.host` name, checked
  against the `def-var!` sites and the `jolt-core` references, so the host contract can't
  silently drift.
- **Java-layer dedup.** Class-model arms (count, str-render, instance checks) route
  through named registries with priority instead of last-writer-wins `set!` chains;
  `regex-translate.ss` moved into the java layer.

### Fixed

- **Declaration-only vars are discoverable in built binaries.** A no-init `def` now
  carries position metadata, so it emits `set-var-meta!` then `declare-var!` — and
  `declare-var!` must mark the already-interned cell resolvable. Without it, a
  `(declare x)` or a no-root `(def ^:dynamic *x*)` was missed by `find-var`/`resolve`/
  `ns-interns` in an AOT binary (only there — interactive use masked it).
- **A no-init `def` with metadata evaluates to its var**, not `nil` (`(var? (def x))`).
- **The seed mint fails loudly on a dropped overlay form.** A form that fails to compile
  in the converged fixpoint pass now aborts the re-mint instead of silently deleting the
  var while the byte-fixpoint still converges.

## [0.4.2] - 2026-07-18

The bulk of a focused compiler review: identifier hygiene, the numeric tower,
macro fidelity, reader/namespace behavior, typed throws, laziness/sorted
collections, syntax-quote, `eval`-embedded constants, and multimethod dispatch —
each change regression-tested and gated (`--opt` soundness pinned by a build
assertion). Deliberate divergences from the JVM are now documented in
`known-divergences.edn`.

### Fixed

- **Locals named after emitted runtime heads no longer miscompile.** A local
  bound to a name the back end emits as a bare Scheme head — `keyword`,
  `integer->char`, `case-lambda`, `dynamic-wind`, `host-new`,
  `record-method-dispatch`, and others — shadowed the emitted form and crashed
  (`(let [keyword 5] :a)` errored instead of returning `:a`). The muncher's
  shadow set now covers every such head.
- **`munge-name` is injective.** Distinct locals like `a'` and `a_PRIME_` munged
  to the same Scheme identifier, so one silently captured the other's value.
  Symbol characters are now escaped reversibly.
- **Subnormal doubles print correctly.** `(pr-str 4.9E-324)` produced a corrupt
  `"5.0E-256.0"` (a mangled Chez precision suffix); subnormals now round-trip.
- **`bigdec` is exact for ratios and scientific notation.** `(bigdec 1/4)` was a
  bigdec with a *ratio* in its unscaled field (so `(+ (bigdec 1/4) 1M)` gave
  `5/4M`); it now yields `0.25M`, `(bigdec 1/3)` throws like the JVM, and Inf/NaN/
  garbage strings throw `NumberFormatException`.
- **`==` handles BigDecimal operands** (`(== 3M 3)` threw) and, on a mixed long/
  double set, the `apply`/HOF path now agrees with the call-position result
  instead of comparing exactly (`(apply == [9007199254740993 9007199254740992.0])`).
- **`Math/round` / `clojure.math/round`** no longer crash on `##NaN`/`##Inf` or
  overflow to a bignum — they follow Java semantics (0, saturate, half-up).
- **`hash` of BigDecimals and negative/large ratios** matches the JVM (every
  bigdec previously hashed to one constant; ratios used the wrong integer hash).
- **`clojure.math` expm1/log1p keep precision near zero and hypot doesn't
  overflow**; the `Math/*` statics route to the same implementations.
- **Bit operations require a long operand** — a double/ratio (or `bit-not` on a
  non-integer) throws `IllegalArgumentException` instead of truncating or leaking
  a raw error; `bit-set`/`bit-flip` wrap to 64 bits.
- **`clojure.math/floor-div`/`floor-mod` return a long**, and `parse-double`
  trims whitespace and accepts a trailing `f`/`d` type suffix.
- **`case` treats composite constants as literals.** A vector/map/set test
  constant was evaluated (so `(case [1 2] [a b] …)` matched on the values of
  `a`/`b`); it is now quoted like a symbol or list constant.
- **`case`/`condp` throw `IllegalArgumentException`** (not `ex-info`) when no
  clause matches, so `(catch IllegalArgumentException …)` works like the JVM.
- **`for`/`doseq` nest `:let`/`:when`/`:while` in source order.** A `:while`
  after a `:when` no longer sees elements the `:when` filtered out, multiple
  `:while` clauses all apply, and `doseq` runs in constant space instead of
  realizing a `for` comprehension. `(for [x [2 4 3 6] :while (even? x) :when (> x 3)] x)`
  is now `(4)`.
- **`if-let`/`if-some`/`when-some` reject a malformed binding vector**, and
  `assert` expands to nothing when `*assert*` is false at compile time.
- **Taking the value of a macro is a compile error** (`(partial when …)` no longer
  silently yields the macro's expansion as data), and `get-method`, `methods`,
  `prefer-method`, `remove-method`, `remove-all-methods`, and `prefers` are
  functions, so they work under `map`/`apply`/`partial` like the JVM. (`instance?`
  stays a macro — jolt's class model precludes the fn form.)
- **`lazy-seq` forces its body exactly once and caches a thrown failure.** Two
  threads racing to realize the same cell could run the thunk twice; a thunk that
  threw left the cell unrealized so the next access re-ran it. Forcing is now
  guarded per cell, and a thrown condition is cached and re-raised.
- **`rsubseq` with two bounds** no longer returns `()` — it seeks from the end
  bound and walks predecessors (`(rsubseq (sorted-set 1 2 3 4 5) >= 2 <= 4)` is
  `(4 3 2)`).
- **`subseq`/`rsubseq` require a sorted collection** and throw `ClassCastException`
  on a plain map/vector instead of silently returning `nil`.
- **`with-meta`/`meta` work on sorted maps and sets** (previously threw).
- **`(take 0 coll)` doesn't realize the source** (it was forcing the first chunk).
- **`contains?` throws on a lazy seq or fn** like the JVM, instead of returning a
  silent `false` (it already threw for eager seqs).
- **`empty?` works on a transient collection.**
- **Kernel and collection errors throw typed exceptions.** `peek`/`subvec` and the
  `conj`/`nth`/`count`/odd-map-literal error paths threw bare strings or untyped
  conditions that a `(catch SomeException …)` wrongly caught; they now throw
  `ClassCastException` / `IllegalArgumentException` / `IndexOutOfBoundsException` /
  `UnsupportedOperationException`, and an odd-length map literal (`{1}`) raises
  `IllegalArgumentException` instead of crashing.
- **`throw-jvm` resolves any simple exception name through the class hierarchy**, so
  `(class e)` reports the full name (e.g. `java.lang.RuntimeException`) rather than a
  bare `RuntimeException` for names outside its short table.
- **Misordered `try` clauses and wrong-arity `recur` are compile errors.** A body
  expression after a `catch`, a second `finally` (or one that isn't last), and a
  `recur` whose argument count doesn't match the enclosing `loop`/`fn` are rejected
  at compile time instead of silently miscompiling or failing only at runtime.
- **`read-string` of a syntax-quote matches the JVM in more cases.** `` `() ``
  read as data was `(clojure.core/list ())` (evaluating to `(())`); it is now
  `(clojure.core/list)` = `()`. An interop head or fully-qualified class name in a
  read-time backquote (`` `.foo ``, `` `foo. ``) stays bare instead of being
  qualified to the current namespace, matching the compile path.
- **`eval` accepts an embedded BigDecimal, `#inst`, or `#uuid` value.** A form
  containing one of these values (read via `read-string`, or spliced by a macro)
  failed with `unsupported form`; the analyzer now emits it as the same constant a
  source literal produces. (Long / BigInt / Ratio already worked.)
- **Multimethods: preference conflicts, transitive preference, printing, and
  errors match the JVM.** `prefer-method` now throws `IllegalStateException` on a
  contradictory preference; a preference resolves an ambiguity transitively through
  the hierarchy (a preferred parent settles a child); a multifn prints as
  `#object[clojure.lang.MultiFn 0x0 "name"]` instead of dumping its record; the
  ambiguous-dispatch error names the dispatch value and the conflicting methods; and
  `defmulti` returns the var (not the multifn).
- **`*data-readers*` entries may be a fn or var**, not only a symbol — a tag bound
  to `inc` now applies it (`#t/tag 5` → `6`).
- **`--opt` no longer swallows an exception from dead code.** Scalar-replacement
  treated arithmetic (`+`/`min`/`zero?`/…) as never-throwing, so a throwing but
  unread initializer — `(let [m {:a (+ x "s")}] (:b m))` — was dropped and returned
  `nil` in an optimized build where dev/release threw. A discarded expression must
  now be *total* (never throws); merely-pure expressions are still relocated and
  duplicated (record scalar-replacement is unaffected).

## [0.4.1] - 2026-07-17

Correctness patch: the first round of a focused compiler review (correctness and
architecture), plus two loader fixes surfaced while running a real dependency
tree. Every behavioral change is regression-tested and, where it only shows in an
optimized build, pinned by a `--opt` build-smoke assertion.

### Fixed

- **`nil?`/`some?` folded to the wrong constant in optimized builds.** When
  inference proved a value nil, `jolt build --opt` folded `(nil? x)` to `false`
  and `(some? x)` to `true` — inverted — so an `if`/`if-some`/`when-some` gated
  on it took the wrong branch in a release binary (dev/interpreted mode was
  unaffected). The fold now matches: nil? true, some? and every type predicate
  false.
- **A `loop` that rebinds a record-typed outer local crashed under `--opt`.** The
  inference pass left the loop variable with the outer local's record type, so a
  slot read like `(:x p)` devirtualized to a raw record access and blew up when
  the loop actually carried something else — the common
  `(let [x (init)] (loop [x x] … (recur (f x))))` shape. Loop variables (and a
  `(fn f …)` self-reference) now correctly shadow the outer binding during
  inference.
- **`min`/`max` returned a float where they should return the original operand.**
  `(min 2.5 1)` returned `1.0` in optimized/release builds instead of `1` (dev
  gave `1`). Double contagion no longer applies to `min`/`max`, which return an
  argument unchanged.
- **`clojure.math` and `Math/*` leaked complex numbers.** Out-of-domain real
  inputs returned a Chez complex — `(Math/sqrt -1.0)` gave `0.0+1.0i` — instead
  of `##NaN`. `sqrt`, `pow`, `log`, `log10`, `log1p`, `asin`, and `acos` now
  return `##NaN` off their real domain, matching Java; in-domain results and
  `##NaN`/`##Inf` are unchanged.
- **`compare-and-set!`, `swap-vals!`, and `reset-vals!` were not atomic.** The
  overlay redefined them as check-then-act compositions that lost updates under
  real threads (futures/agents), shadowing the atomic mutex/CAS implementations.
  The atomic versions are restored.
- **Any missing namespace crashed with an opaque error.** `require` of a
  namespace with no source file raised `incorrect number of arguments 3 to
  throw-jvm` instead of a catchable `FileNotFoundException` naming the file —
  a stray argument in the loader's not-found path.
- **A failed nested `require` was blamed on the wrong file.** The reported source
  location pointed at the last form of a dependency that had just loaded
  successfully, not the `ns` form that issued the failing require. The loader now
  restores the source position after each nested load.

## [0.4.0] - 2026-07-17

Strict-resolution and default-fast-builds release: a five-dimension audit
(architecture, dead code, duplication, correctness, performance) followed by
two implementation waves (PRs #376–#388), every behavioral change certified
against reference JVM Clojure 1.12.5.

### Changed

- **Unresolved symbols are compile errors.** Top-level and operator-position
  references to undefined symbols throw "Unable to resolve symbol" at analyze
  time in every entry path (`-e`, files, `run`, built binaries) instead of
  silently producing unbound-var values that pattern-matched as truthy. Fn
  bodies still auto-declare (matching JVM-with-`declare` semantics); the nREPL
  path keeps late binding for interactive redefinition.
- **`-e` and file loading evaluate one top-level form at a time**, like the
  JVM: a `require` in one form is visible to the reader and analyzer of the
  next, so `joltc -e "(require '[x :as a]) ::a/k"` resolves. As a CLI
  convenience, `-e` auto-quotes `require`/`use` vector args.
- **Plain `jolt build` now direct-links and runs whole-program inference** —
  measured 2.5x on cross-namespace call loops with no flags. A plain `def` is
  frozen in the binary; `^:redef`/`^:dynamic` defs stay var-routed so runtime
  redefinition and `binding` keep working. Opt out with `--no-direct-link`,
  `--dev`, deps.edn `:jolt/build {:direct-link false}`, or
  `JOLT_NO_WP_INFER=1` for the inference fixpoint alone.
- **Vars are non-dynamic unless marked**, like the JVM: `binding` a var
  without `^:dynamic` metadata throws, `set!` of a dynamic var with no thread
  binding throws instead of mutating the root, and `(def ^:dynamic *x*)`
  declares bindable. All runtime-defined dynamic vars (`*out*`, `*1`,
  print flags, `&form`/`&env`, …) carry the tag.
- **`import` is a macro** (its specs are never evaluated, so bare
  `(import [java.nio.file Path])` works under strict analysis) and binds host
  class short names to class values; built binaries now run their `:import`
  clauses (they previously never did). `defmulti` interns its var at analysis
  like the JVM, so a reference later in the same form resolves.
- Errors across the runtime throw **typed JVM exceptions** — 79 sites that
  raised untyped host conditions are catchable by class:
  `(catch NoSuchElementException …)` for iterator exhaustion,
  `FileNotFoundException` for missing requires, `NumberFormatException`,
  `IndexOutOfBoundsException`, `ClassCastException`, `IllegalStateException`,
  `ArityException`, and friends, all oracle-verified. Broad
  `(catch Exception …)` continues to work everywhere.

### Added

- `jolt.deps/add-deps`: resolve an inline `:deps` map (git / local / Maven
  coordinates) at runtime and add the roots to the loader — the
  `babashka.deps/add-deps` idiom, detection included:
  `(when (System/getProperty "jolt.version") ((requiring-resolve 'jolt.deps/add-deps) '{:deps {…}}))`.
- `*jolt-version*` and `(System/getProperty "jolt.version")`: the release tag
  baked into binaries (else `git describe`, else `"dev"`); never nil under
  jolt, so it doubles as am-I-on-jolt detection.
- **Maven/gitlibs cache sharing with the JVM toolchain**: jars live at their
  standard `~/.m2/repository` paths (bidirectional reuse with clj);
  `:mvn/local-repo` in deps.edn relocates the repository like tools.deps,
  `JOLT_LOCAL_REPO` overrides from the environment; git deps reuse existing
  tools.gitlibs checkouts read-only and honor `$GITLIBS` for cache placement.
- Dev boot cache: `make devboot` precompiles the runtime so source-mode
  `bin/joltc` starts in ~0.3s instead of ~1.5s, with automatic staleness
  fallback. `jolt.main`/`jolt.deps` are AOT'd into the `joltc` binary
  (CLI commands no longer recompile them per invocation).
- Multimethods memoize isa?-resolved dispatch (invalidated by `defmethod`,
  `remove-method`, `prefer-method`, and hierarchy changes) with fixed-arity
  fast paths.

### Fixed

- for/doseq: `:while` can reference a preceding `:let` binding, and modifiers
  nest in written order (`:when`-skipped elements never reach a later
  `:while`).
- `extends?` sees inline `defrecord`/`deftype` protocol implementations
  without polluting `(extenders P)`; `select-keys` preserves metadata.
- Reader: `::alias/kw` with an unknown alias throws Invalid token instead of
  silently minting the wrong keyword; `\backspace`/`\formfeed` round-trip
  through pr; `*print-readably*` and `*print-namespace-maps*` are honored.
- `format`: unknown directives throw instead of emitting literal text while
  consuming the argument; `%s` renders nil as `"null"`.
- Deref of a failed future throws `ExecutionException` wrapping the original
  as its cause. `clojure.string/split` on zero-width matches splits between
  characters. Bit-shift counts mask to 6 bits; unary `bit-and`/`bit-or`/
  `bit-xor` throw `ArityException` (the raw variadic host primitives no
  longer leak through value positions). `(keyword 5)` returns nil. Var meta
  `:name` is a symbol. Transient read ops throw after `persistent!`.
  `System/gc` never throws (a guarded no-op while threads are active).
- Records: `assoc`/`dissoc` build the new record with one allocation and
  direct slot reads (~28% faster); `with-meta` on vectors shares structure
  (O(1), ~173x on kilo-element vectors); string hashing drops a per-hash
  UTF-16 allocation and caches like `String.hashCode` (18x on string-keyed
  lookups); bignum hashes are JVM-exact int32.
- Whole-program inference: the per-namespace IR cache stayed aligned past
  macro forms — under `--opt` every form after the first macro in a namespace
  had been silently compiled from the next form's IR, corrupting macro
  expanders.
- Startup/memory: embedded sources ship as UTF-8 bytevectors (~10MB steady
  RSS); the `joltc` boot GC peak is tunable; `ns-has-vars?` is O(1);
  stdout/stderr flush on every exit path (output no longer lost when the
  process exits while helper threads wind down).

### Internal

- The conformance corpus gate asserts `:expected :throws` rows raise on jolt
  and gates the crash bucket against an exact-label baseline; `jolt build`
  brackets each baked namespace with RT.load-parity compiler-var bindings;
  `jolt-load-string` no longer leaks a binding frame when the loaded source
  throws; the tree-shaker recognizes metadata-carrying defs as prunable and
  roots `global-hierarchy`.

## [0.3.3] - 2026-07-16

Full-codebase audit release: seven review rounds plus follow-ups (PRs
#362-#373), every behavioral fix certified against reference JVM Clojure.

### Fixed

- Build: `--embed` resources are baked into the binary at build time (shipped
  binaries no longer re-read build-machine paths at startup); tree-shaking is
  sound for redefined vars (duplicate-fqn refs union); binary namespace roots
  derive from the require graph, so namespaces loaded before the build hook
  (data-reader helpers and their requires) ship correctly.
- core.async: `alts!`/`alts!!` use handler registration — an alts put and an
  alts take on an unbuffered channel rendezvous instead of livelocking, and
  blocked alts no longer busy-poll. Fixed-buffer channels with transducers get
  real backpressure; pending rendezvous puts park through `close!` until a
  taker consumes their value (JVM-verified); `timeout` channels share one
  timer thread.
- Hashing: record hashes are JVM-exact defrecord hasheq (a bignum overflow
  into unchecked fixnum ops previously made equal values hash differently,
  nondeterministically); collections cache their hasheq lazily; vector/map/
  set hashes are value-identical to the JVM.
- Inference soundness: a `reduce` accumulator seeded `:double` no longer
  forces a coercion crash on nil-returning reducers; same-named records in
  different namespaces resolve exactly instead of by suffix; user `^double`
  hints survive HOF seeding; locals named like runtime identifiers
  (`jolt-nil`, `fl+`, …) are munged instead of shadowing.
- Reader/regex: mid-pattern `(?i)` applies to the remainder and `(?-i)`
  actually removes flags; `\Q…\E` quantifier scope, strict `\p{…}`, Java
  octal escapes, possessive quantifiers as atomic groups; radix-aware `N`
  literals (`042N` ⇒ `34N`); positioned EOF errors in string escapes;
  top-level `#?@` throws; record literals construct records; `#!` is a
  to-EOL comment (clojure reader only — EDN rejects it); syntax-quote
  resolves through the full alias/refer/core chain (`` `map `` ⇒
  `clojure.core/map`); core macros resolve as vars with `:macro` meta.
- clojure.test: `(is (instance? C x))` actually asserts; every assertion
  dispatches through the `report` multimethod; interned `:test`-meta tests
  run inside `:once` fixtures.
- Destructuring `:or` defaults are `get`'s not-found argument (JVM-exact:
  eager, sibling bindings in scope) — `{:or {b (inc a)}}` no longer throws.
- Laziness: `not-empty` uses `seq` (no more hanging on infinite seqs);
  `pmap` is semi-lazy with bounded look-ahead; `pprint` honors
  `*print-length*` by stopping; `when-first` tests the seq.
- Java compat: `io/copy` between files copies bytes (binary files no longer
  corrupted through a UTF-8 round-trip); deleting a non-empty directory
  throws/returns false; parsed timezone offsets apply; FQN and short class
  names share one statics table; bitwise `Math/getExponent`;
  `awaitTermination` actually waits; `ReentrantLock` is reentrant;
  interruptible bodies unwind their timers; `getAbsoluteFile` shares
  `getAbsolutePath`'s base; `(System/getenv)` reads the environment directly
  (multi-line values intact); shared counters and caches are mutex-guarded;
  string `index-of`/`last-index-of` from-args clamp like the JVM; `assert`
  messages evaluate at failure time.
- Memory: caught exceptions no longer root the captured continuation (a
  catch-complete hook clears it after the handler finishes; traces intact).
- On hosts with an unverified `struct stat` layout (e.g. aarch64 Linux),
  `getPosixFilePermissions`/`getOwner` throw a clear
  `UnsupportedOperationException` instead of reading garbage.

### Changed

- The whole-program shake's hand-maintained name lists are gate-verified; the
  17 run-gate scripts share one harness; `--opt` builds reuse the
  whole-program pass's analysis at emission; one mode→Chez-parameters table;
  the layered `Files` registrations collapse to one block per class; dead
  code across the runtime removed (shakesmoke byte-identity verified).
- The long-only integer boxing model is documented as the SPEC feature
  `:numerics/long-only` (`(short x)` range-checks but boxes as Long).

### Performance

- `subseq`/`rsubseq` seek from the comparator bound and walk lazily
  (O(log n) instead of materializing the collection); string scans stop
  allocating per candidate offset (`.indexOf` −20%, `replace` −12%);
  regex literals compile once per source string (~30× on literal-in-loop
  patterns); collection-as-map-key lookups no longer rehash O(n) per probe.

## [0.3.2] - 2026-07-15

### Changed

- Built binaries use roughly a third less memory. The launcher registers the
  appended boot image as a region of the executable (read through a file
  descriptor at startup) instead of holding a resident copy — 7–14 MB less
  depending on the app, on every platform. Tree-shaken binaries with no runtime
  eval now boot from `petite.boot` alone, dropping the bundled Chez compiler:
  another ~5 MB of memory and ~1 MB of binary size (macOS/Linux). A hello world
  goes from ~34 MB to ~22 MB resident; default (REPL-capable) builds keep the
  compiler and still save the boot copy.

## [0.3.1] - 2026-07-14

### Added

- Map destructuring follows Clojure 1.13.0-alpha4: idents after `&` in
  `:keys`/`:syms`/`:strs` (and the `!` variants) are keys, not binding symbols;
  `:or` accepts key→val entries; `:defaults name` binds a map of the resolved
  defaults; `:select name` binds a map of the mentioned keys, filled from `:or`
  and selecting deeply through nested map patterns. Adds `some-vals`.

### Fixed

- `(. Class staticMethod args)` now dispatches statically for the value classes
  (`Long`/`Integer`/`String`) and any registered/fully-qualified class, matching
  the `Class/staticMethod` slash form.
- `load-string` and `eval` handle source containing reader literals (`#inst`,
  `#uuid`, `#"regex"`): `load-string` reads raw forms like file loading, and
  `eval` self-evaluates opaque host values built by `read-string`.

## [0.3.0] - 2026-07-14

### Changed

- **Breaking:** `java.time.*` is no longer built into core — it is the
  [jolt-lang/time](https://github.com/jolt-lang/time) library. The full surface
  (`LocalDate`/`Time`/`DateTime`, `Instant`, `ZonedDateTime`, `OffsetDateTime`,
  `Duration`, `Period`, `Year`/`YearMonth`, zones with DST, `DateTimeFormatter`)
  is now portable Clojure over the value-semantics seams below, with
  [juxt/tick](https://github.com/juxt/tick) on top; tick's full suite passes. A
  program using `java.time.*` must depend on the library. Core keeps the `#inst`
  / `java.util.Date` layer and the libc zone/locale primitives (`tz-primitives`).

### Added

- `jolt.deps` resolves Maven coordinates. A Clojure library's Maven JAR carries
  its `.clj`/`.cljc` source, so a `:mvn/version` dep — including one pulled in
  transitively (tick declares its deps as Maven) — is fetched from Clojars/Central,
  extracted, and its `pom.xml` read for further transitive deps, with no JVM.
  Skips test/provided/optional deps, pure-Java or ClojureScript-only artifacts,
  and the clojurescript toolchain.
- Core value-semantics seams a library uses to give its own host values full
  Clojure semantics: `__register-eq!` / `__register-hash!` / `__register-str!` /
  `__register-pr!` / `__register-compare!`, and `__register-class!` so those
  values answer `class`/`type` and dispatch protocols extended to their class.
- `jolt.host/set-instant-ctor!` — the `#inst`/`Date` layer's `.toInstant` yields
  a library-owned instant, so `Date` and a library `Instant` are one representation.
- `java.util.Date` is now `Comparable` (`compareTo` / `clojure.core/compare`).

## [0.2.8] - 2026-07-13

### Added

- `jolt.fs` is now the [babashka.fs](https://github.com/babashka/fs) API. Jolt
  vendors babashka.fs over a new `java.nio.file` host shim — `Path`, `Files`,
  `FileTime`, file attributes, POSIX permissions, symbolic links, and directory
  walking with symlink-cycle detection. `jolt.fs` re-exports it as the public
  surface (require `babashka.fs` directly if you prefer). Symbolic links,
  creation time, and permissions — which the previous `java.io.File`-based
  `jolt.fs` could not do — now work through the shim's `stat`, `realpath`,
  `symlink`, `chmod`, and `getpwuid` bindings.
- A `java.nio.file` interop surface: `Paths`/`Path`, `Files` (predicates,
  create/delete/copy/move, read/write, temp files, `walkFileTree`,
  `newDirectoryStream`, attributes), `FileTime`, `PosixFilePermissions`,
  `FileVisitor`/`FileVisitResult`, and the `LinkOption`/`CopyOption`/`OpenOption`
  enums.
- `jolt.util/import-vars` — re-export a namespace's public vars as bakeable
  delegating definitions (functions and macros, with an `:exclude` set). The
  pattern for putting a public face on a vendored library; how `jolt.fs` wraps
  babashka.fs. Works in an AOT-built binary, unlike an `intern` over
  `ns-publics`.

### Fixed

- A built binary now includes a namespace's forms that follow a non-matching
  reader conditional. The AOT emission reader stopped at the first `#?(:cljs …)`
  (with no `:clj` branch), silently dropping every later `def` — so an AOT-built
  app crashed on an unbound var when it called one. This surfaced with
  babashka.fs (many cljs-only conditionals); a build-smoke fixture now builds a
  binary that uses the vendored library and checks it runs.

### Changed

- The documentation moved to the site ([jolt-lang.github.io](https://jolt-lang.github.io));
  the repo `docs/` folder is gone and the README links to the live pages.

### Notes

- `zip`/`unzip`/`gzip`/`gunzip` need `java.util.zip`, which Jolt does not shim
  yet, so those babashka.fs functions are excluded from `jolt.fs`.

## [0.2.7] - 2026-07-13

### Fixed

- `read-string`/`read` expand a syntax-quote at read time, like the JVM reader:
  `` (read-string "`(a ~b c)") `` returns the `(seq (concat (list 'ns/a) …))`
  form with symbols namespace-qualified against `*ns*` and auto-gensyms shared
  within a form, instead of a raw `(syntax-quote …)`. (edn and tools.reader are
  unaffected.)
- A qualified or aliased trailing-dot constructor — `(some.ns/Type. args)` or
  `(alias/Type. args)`, as SCI builds `sci.impl.types/Reified.` — now
  constructs the cross-namespace deftype instead of erroring "Unknown class
  \<ns\>"; a namespaced head never reached the constructor path before.

### Added

- The joltc CLI runs a bare file: `joltc FILE` (the `run` subcommand is now
  optional, like bb), and a `FILE` of `-` reads the program from stdin — so
  `joltc run -`, `joltc FILE`, and `joltc -` all work with piped input. A token
  that isn't a file still resolves as a deps.edn `:tasks` entry.

## [0.2.6] - 2026-07-13

### Fixed

- `defmacro` re-heads its generated expander with `clojure.core/fn`, not a bare
  `fn`, so a macro *named* `fn` — like prismatic/schema's `s/fn`, whose namespace
  does `:refer-clojure :exclude [fn]` — no longer resolves `fn` to the
  half-defined macro and fail at load with "Don't know how to create ISeq from:
  :object". Fixed in both the spine and the analyzer.
- `(class x)` returns a real class rather than the `:object` fallback for a few
  values whose class wasn't registered, so using one where a cast or `seq` is
  expected now reports the JVM's message: an unbound var value is
  `clojure.lang.Var$Unbound` (an exact match — the JVM throws the same
  `ClassCastException` for `(def x (+ x 1))`); a `reify` is a stable
  `clojure.lang.IObj$reify__0` placeholder (its JVM name is an unreproducible
  per-eval `ns$eval$reify__N`); `promise`/`future` match the JVM's stable
  enclosing-fn prefix, `clojure.core$promise$reify__0` / `$future_call$reify__0`.

### Added

- `resolve` gets the 2-arg `(resolve &env sym)` arity (nil when `sym` is a local).
- A `deftype`/`defrecord` type token (its constructor closure) is a full class
  value: `class?` is true, it carries `java.lang.Class` dispatch tags, `instance?`
  works when it's passed by value, and `.getName`/`.getSimpleName` answer off its
  tag. A named fn reports its own `ns$name` class plus `AFunction`/`IFn` tags —
  so a protocol extended to a Class value or a specific fn's class dispatches.
  (These, with `clojure.lang.MultiFn` `.addMethod` interop, `with-test`, and an
  `IdentityHashMap` shim, are what let prismatic/schema load, compile, and run.)
- The joltc CLI reads from stdin: `joltc -` runs a program read from stdin as a
  script, `joltc -e -` reads the expression from stdin; both set
  `*command-line-args*` from the trailing argv.

## [0.2.5] - 2026-07-12

Driven by running more libraries: camel-snake-kebab and clj-rss now pass their
suites, claxon passes its byte-parsing tests, and pretty passes four of its six
test namespaces. clj-rss runs over a new `clojure.data.xml` emitter shipped in
[jolt-lang/xml](https://github.com/jolt-lang/xml) v0.0.2.

### Fixed

- A char value reports `java.lang.Character` for protocol dispatch, so a
  protocol extended to `Character` matches a char. It reported nothing, so
  `(extend Character …)` never dispatched (camel-snake-kebab's separator split).
- Record literals `#pkg.Record{…}` read their map/vector values as data, like
  the JVM: `#user.Foo{:content ("a" "b")}` keeps the list instead of evaluating
  it as a call, while a nested record literal is still constructed.
- `(set! (.field obj) v)` compiles, matching `(set! (.-field obj) v)` — an
  instance-field write via the `.name` form was rejected.
- A chained numeric comparison with a `^long`/`^double` operand,
  `(<= 0x21 value 0x7e)`, expands to `(and (op a b) (op b c))` — the fast binary
  op received three arguments and emitted invalid code.
- `(String. bytes offset length charset)` decodes the requested slice; it
  decoded the whole array, ignoring offset/length.

### Added

- Clojure 1.12 qualified instance-method syntax `(ClassName/.method target
  args…)`, lowering to `(.method target args…)`.
- `clojure.lang.Compiler/CHAR_MAP` (the munge map).
- `java.util.WeakHashMap`, `java.util.Collections` (synchronized/unmodifiable/
  empty views), and `java.util.concurrent.atomic.Atomic{Reference,Integer,Long,
  Boolean}`.
- `java.util.concurrent.ExecutorService` / `Executors` backed by a real task
  queue and worker threads — a single-thread executor runs tasks strictly FIFO.
- `java.util.concurrent.locks.ReentrantLock`, `java.net.URI` `getUserInfo`,
  `System/console`/`lineSeparator`, `java.lang.Byte/toUnsignedLong`, and
  `java.nio.ByteBuffer` `slice` plus absolute/relative single-byte `get`.

## [0.2.4] - 2026-07-11

### Fixed

- Destructuring a rest pattern positionally walks the seq like the JVM:
  `(let [[[k v] & ks] a-map] …)` bound `k`/`v` to nil because the positional
  elements read `(nth coll i nil)` even when `&` is present. This silently
  broke `clojure.spec.alpha`'s `keys` conform — `s/valid?` accepted maps whose
  nested key specs failed.
- `empty?` is seq-based like the reference implementation: any seqable value
  answers (including the `java.util` collection shims) and a non-seqable
  raises `IllegalArgumentException` instead of an opaque host error.
- A deftype declaring a `clojure.lang` collection interface now matches the
  JVM at both ends: `instance?`/`map?`/`coll?`/`associative?` answer through
  the declared interface and its ancestry, and calling a declared-but-
  unimplemented method throws `AbstractMethodError` instead of falling back to
  the bare-deftype fields-as-map behavior.
- `inst?` is a real instance check covering `java.util.Date`, its `java.sql`
  subclasses, and `java.time.Instant` — the old tagged-map probe crashed on
  sorted collections and missed `Instant`.
- Throwables and reader conditionals no longer leak their internal map
  representation through `map?`/`coll?`/`ifn?`/`seqable?`/`instance? IObj`.
- Java regex hex and unicode escapes (`\xHH`, `\x{…}`, `\uHHHH`) translate to
  their characters before reaching the regex engine, which mis-parsed them.
- `keys`/`vals` accept any seq of map entries — `(keys (filter pred a-map))`
  works like `RT.keys`.
- A transient carries its source map's representation: an array map round-trips
  through `transient`/`persistent!` as an array map and reports
  `TransientArrayMap`; a hash map stays hash-ordered (previously everything
  came back in array mode).
- The `instance?` macro evaluates a var or local holding a Class value —
  `(def c (class x)) (instance? c y)` works — and `class?` recognizes Class
  values instead of always returning false.
- `clojure.pprint`'s cl-format engine: parametrized directives (`~5A`, `~2{`,
  `~20<`, …) rejected their own parameters, and a forward `~n@*` goto never
  moved. Both fixed, and the missing `~F`, `~$`, `~C`, `~R` (radix/roman), and
  `~(` case-conversion directives are implemented, so `(cl-format nil "~,2f" x)`
  and friends work. A JVM-certified subset of the upstream cl-format suite now
  runs as a standing gate.

### Added

- The JVM class model fills out across the board, driven by running type-
  introspection libraries (lasertag, expound, fireworks all pass or reach
  their documented ceilings): ~20 exception/error constructors with hierarchy
  placement, `java.util.ArrayDeque` and `HashSet`, `(class x)` for the
  `java.time` values, Agent/Volatile/Var/Delay/MultiFn/ReaderConditional/
  MapEntry, sorted and transient collections and hash-mode maps, JVM-shaped
  function class names and the `#object[…]` printed form, `Matcher`
  `.start`/`.end`, `String` `.repeat`/`.isBlank`, `getDeclaredFields`
  reflection over modeled types, a minimal `DateTimeFormatterBuilder`, and a
  `clojure.main` namespace with `demunge`.
- `clojure.test/*testing-contexts*` is a real bindable dynamic var and
  `testing` binds it; `testing-contexts-str` added.

### Changed

- Small sets preserve insertion order through the same array-mode backing that
  small map literals use (past 8 elements they go hash-ordered), so sets and
  maps share one deterministic iteration story. The `java.util` HashMap and
  HashSet shims iterate in insertion order too.
- Record fields fed a mix of integers and floats (`:num`) unbox in protocol-impl
  arithmetic at monomorphic call sites: whole-program builds emit a
  flonum-specialized clone per eligible impl (a `:num` field read beside a
  proven-double operand, where Clojure double contagion already fixes the
  result type), and devirtualized call sites resolve the clone while
  megamorphic dispatch keeps the shared impl. Mono-dispatch ~9% faster;
  results are bit-identical.
- Proven numeric sites and the protocol inline cache's warm-hit scan compile
  to Chez's per-site unsafe primitives (`#3%fl*`, `#3%vector-ref`): the type
  and bounds checks they skip are exactly the ones the compiler already
  proved redundant, so semantics are unchanged while megamorphic protocol
  dispatch gets ~4% faster. Checked `^long` arithmetic keeps its raising
  overflow behavior — fixnum ops are never emitted unsafe.
- `(double x)`, `(long x)`, `(int x)`, and `(float x)` casts feed the typed-
  arithmetic fast path the way `^double`/`^long` hints do: `(* (double x) 2.0)`
  compiles to flonum ops. The casts keep their full checked semantics
  (ClassCastException on a non-number, `(long ##NaN)` is 0, int range
  enforced), so they are a portable escape hatch where inference can't prove
  a type.
- BigDecimal literals follow JVM double contagion in compiled arithmetic:
  `(+ 1.5M 2.0)` is 3.5 (a Double) on the flonum fast path. Mixed
  bigdec/double expressions with non-literal bigdecs keep the generic
  (already correct) path.

- Whole-program builds infer record field types from the constructor
  arguments: a field every `(->Ctor …)` site fills with a flonum reads as a
  double (arithmetic over it unboxes, through protocol-method returns and
  reduce accumulators), and a field holding a record-or-nil narrows guarded
  reads to the direct accessor. No hints needed; conflicting or escaping
  constructors soundly leave fields untyped.

## [0.2.3] - 2026-07-11

### Fixed

- Release and optimized builds compile at Chez optimize-level 2, not 3 — level
  3 is unsafe mode (fx/fl/car operations skip their type checks) and jolt's
  error semantics depend on those raising: an optimized binary returned
  `(take nil coll)` instead of throwing and looped forever on a nil-count
  `repeat`. Costs ~8-13% on dispatch/allocation benchmarks, nothing on
  numeric ones.
- The standalone `joltc` binary's `-e` matches the script driver: trailing
  args bind `*command-line-args*`, the first `--` ends option parsing, and an
  uncaught throw reports its source location. Both entry points now share one
  dispatch (`cli-core.ss`), guarded against re-diverging by the load-manifest
  check.

### Changed

- The smoke and clojure-test-suite gates run against a freshly built joltc
  binary (10x faster boot than script mode): `make test` drops from ~12 to
  ~3 minutes (`make -j` parallelizes the rest), and the gates now exercise
  the shipped artifact — which is how both fixes above were found.

## [0.2.2] - 2026-07-10

### Added

- Refs and STM: `ref` (with `:validator`/`:meta`), `dosync`, `alter`, `commute`,
  `ref-set`, `ensure`, `sync`, `io!`, with serialized transactions on a single
  global lock; refs participate in watches/validators/metadata, and
  `*loaded-libs*` is a real ref over the loader registry (the tools.namespace
  reload pattern works). Transactions buffer writes and commit atomically:
  a thrown `dosync` rolls back, other threads never see uncommitted values,
  watches fire once per changed ref after commit, agent sends inside a
  transaction are held until commit, and transaction state does not leak into
  threads spawned inside a `dosync`. `(class (ref 0))` is `clojure.lang.Ref`,
  and `ref-min-history`/`ref-max-history` take the setter arity.
- `jolt.parser`: a general monadic parser-combinator core (`jolt.parser` +
  `jolt.parser.{basic,combinators,monad,position}`), adapted from rm-hull/jasentaa,
  with added combinators (`eof`, `between`, `sep-by`, an `optional` default-value
  arity, and the `digit`/`letter`/`alpha-num` character classes). Parse failures
  raise a jolt `ex-info`.
- `jolt.infix`: built-in infix math notation via the `infix`/`$=` macros and
  `from-string` (ported from rm-hull/infix), built on `jolt.parser`.
- Rounded out the `java.lang.Math` static surface: `atan2`, `sinh`, `cosh`,
  `tanh`, `cbrt`, `hypot`, `rint`, `floorDiv`, `floorMod`, `copySign`,
  `toRadians`, `toDegrees`, `log1p`, `expm1`.
- `java.text.ParseException` as a constructable/catchable host exception class,
  including `.getErrorOffset`.

### Changed

- `joltc` with no arguments starts a REPL, like `bb` and `clj` (piped stdin
  evaluates and exits). The nREPL server is the bare command
  `joltc nrepl-server [port]` — the flag spelling `--nrepl-server` is removed;
  `help` and `version` work as bare commands; an unknown command points at
  `joltc help`.
- Records store their fields inline (one heap object per record instead of a
  descriptor + separate values vector), and a typed non-nilable field read
  emits the receiver's direct per-arity slot accessor — no dispatch, one load.
  A retention-heavy construction microbenchmark allocates 25% less and runs
  ~44% faster; the mono-dispatch benchmark improves ~2.6x (101 → 39 ms,
  ~2.8x of JVM from ~7.8x). Nilable receivers keep the nil-safe read path
  (gate-pinned), and generic reads dispatch on the descriptor's field count.

### Fixed

- Reading a declared-but-unset var returns the `Var$Unbound` sentinel from
  every surface — a plain read, `@#'x`, and `var-get` all yield the same
  object (printing as `#object[clojure.lang.Var$Unbound …]`) instead of two
  of the three throwing; `bound?` still reports false.
- The self-host byte-fixpoint runs in CI: the seed rebuild is byte-identical
  on the pinned source-built Chez, so a seed source edited without a remint
  fails the gate on every platform.
- A tree-shaken binary crashed at startup when the project registered data
  readers (`data_readers.clj`): the emitted launcher re-scanned the source roots
  and eagerly reloaded each reader namespace through `jolt-compile-eval-form`,
  which a no-eval `--tree-shake` build has dropped. Data readers and reader
  namespaces are now baked once and not re-scanned at runtime, so a
  `(read-string "#my/tag …")` resolves its reader in the binary as it does under
  `joltc run`.
- Tree-shake soundness: a reader fn reached only through the baked
  `*data-readers*` map — including one registered programmatically via
  `alter-var-root`, not just via `data_readers.clj` — is now a DCE root, so the
  shake no longer prunes it and degrades `read-string` to a call error. App-form
  reference collection unions an IR walk (`:var`/`:the-var` nodes) with a text
  scan of the emitted Scheme, so a `(var-deref "ns" "nm")` a macro splices in
  with no IR node still roots its target.
- `jolt build --library`: the launcher guard now wraps the prologue (native
  loads + source-root setup) as well as the export-publish body, so an init
  failure anywhere reports and returns non-zero instead of leaving
  `jolt_lookup` silently returning `NULL` for every name.
- A warmed monomorphic protocol-call site in a direct-linked build now honors a
  runtime `extend-type`: the per-site cache carries the protocol epoch and
  re-resolves when an extension bumps it, so every dispatch path serves the new
  implementation.
- `--opt` builds no longer fold away a throwing operation: `/`, `quot`, `rem`,
  `mod`, `even?`, and `odd?` are not treated as pure, so
  `(:a {:a 1 :b (/ 1 0)})` raises `ArithmeticException` like Clojure instead of
  folding to `1`.
- A var read in a call or collection literal now evaluates in source order
  against a mutating sibling: `(f (do (def y 2) 0) y)` passes `[0 2]` like
  Clojure instead of reading `y` before the mutation.
- List libspecs whose second element is a keyword — `(:require (ns :only [x]))`
  — parse as libspecs everywhere (previously `require`/`use` mis-read them as
  prefix lists); the JVM rejects that shape outright, so this is a documented
  superset.
- A tree-shaken binary that queues agent sends inside a `dosync` no longer
  prunes `send` (the STM commit path resolves it by name at runtime); a new
  gate asserts every such runtime reference is a shake root.

## [0.2.1] - 2026-07-09

### Added

- `Throwable->map` (`:via`/`:cause`/`:data` over the `ex-cause` chain).
- The 11 core dynamic vars the JVM defines that were missing (`*agent*`,
  `*repl*`, `*compile-path*`, `*source-path*`, …), with real context behavior:
  `*agent*` is bound inside agent actions, `*repl*` and the `*1`/`*2`/`*3`/`*e`
  history work in `joltc repl`, `*file*`/`*source-path*` bind during loads, and
  `*command-line-args*` carries app args for `run` and `-m`.
- `clojure.test/test-var` and `test-vars`; `run-tests` discovers tests attached
  via `:test` var metadata, and `deftest` vars carry `:test` metadata.

### Changed

- `ns-map` returns every visible mapping (imports, refers, interns) and
  `ns-refers` includes the implicit `refer-clojure`, matching the JVM.
- Maps print with comma-separated entries (`{:a 1, :b 2}`).
- Double printing follows `Double.toString` (plain decimal only in
  `[1e-3, 1e7)`, otherwise `d.dddE±x`); `pr` of a beyond-long integer carries
  the BigInt `N` suffix.
- `hash-map` results iterate in insertion order up to the array-map threshold,
  like ClojureScript.

### Fixed

- The numeric fast path keeps `=` exactness-aware: `(= ^double-x 0)` is `false`
  like the JVM, and `:long` typing comes only from an explicit `^long` hint —
  an unhinted integer loop keeps arbitrary precision instead of raising a
  fixnum overflow.
- `require` honors `:reload`, `:reload-all`, and `:verbose`; a namespace whose
  load throws can be required again after the file is fixed; a data reader
  that resolves but throws surfaces its error (naming the tag) instead of
  silently degrading.
- `joltc -e EXPR args…` binds the trailing args as `*command-line-args*`
  (nil when empty), and the first standalone `--` is consumed as the POSIX
  end-of-options marker in every arg-taking path (`-e`, `run FILE`, `-m`,
  `-M` aliases, tasks, and `build` flags); later `--` stay literal.
- `(?x)` COMMENTS-mode regexes follow Java: whitespace (including newlines —
  multi-line patterns previously matched nothing) and `#`-comments are
  stripped, even inside character classes, and a mid-pattern cluster works.
- `$` matches before a final newline like Java; `\<`/`\>` are literal escapes;
  regex literals keep the backslash of an escaped quote in their source.
- `clojure.string/split-lines` drops trailing empty strings.
- `clojure.pprint` no longer emits trailing spaces before line breaks.

## [0.2.0] - 2026-07-09

### Added

- `jolt.fs` — file-system utilities in the standard library (predicates, glob,
  recursive copy/delete, move, `which`, temp dirs), shaped after `babashka.fs`.
- Data readers work in ahead-of-time binaries: reader namespaces are compiled in
  and `*data-readers*` is baked, so runtime `read-string` of `#tag` literals
  works in built executables.
- Reader errors report `file:line:column` in the message and carry
  `:file`/`:line`/`:column` in `ex-data`.
- [yamlstar](https://github.com/yaml/yamlstar) and
  [jolt-lang/yaml](https://github.com/jolt-lang/yaml) (libyaml bindings with a
  `clj-yaml.core` compat layer) are listed as supported libraries.

### Changed

- Performance round one: protocol dispatch goes through per-descriptor tables
  with polymorphic inline caches, record constructors inline, dynamic invoke and
  var access are cheaper, and collection equality/hash/reduce walk vector chunks
  directly. Geometric mean on the benchmark suite improved from ~6x to ~2.8x of
  JVM Clojure.
- Release builds run the inference passes (dispatch caches, devirtualization,
  constructor inlining) by default — 3.4x on dispatch-heavy code. Inlining and
  scalar replacement additionally require `--opt` with direct linking; projects
  can opt in via `deps.edn` `:jolt/build {:opt true}`.
- Optimized builds compile at Chez optimize-level 3 with compressed fasl output
  (−37% binary size).
- `defcfn` resolves its foreign symbol lazily on first call, so an optional
  `:jolt/native` library that is missing no longer aborts startup — a missing
  symbol is a catchable error at the call site.
- `spit` writes atomically (temp file + rename), so a crash mid-write can no
  longer truncate the target.
- The host class model (`instance?`, class tokens, type tags, `supers`) derives
  from a single class graph instead of parallel hand-maintained tables.

### Fixed

- Tree shaking soundness: `ns-publics`-family reflection triggers the
  keep-everything bail, a `defonce` no longer silently disables the whole shake,
  and data-reader functions are kept as roots.
- Native build link line: static archives precede system `-l` flags, paths are
  quoted, and Windows builds pass `--export-all-symbols`.
- Exceptions from `go`/`thread`/`Thread` bodies and data-reader load failures
  surface on stderr instead of being swallowed.
- A malformed `deps.edn` fails with a clear error instead of being ignored.
- `instance?` evaluates a local or var operand holding a class value instead of
  quoting it as a literal class name.
- Regex parity with Java: combined inline flag clusters (`(?sx)`, `(?si:…)`),
  scoped dot-all, escaped `]` inside character classes, and
  `Matcher.appendReplacement` escape semantics in replacement strings.
- `intern` and `alter-meta!` carry `:macro` through, and macro vars report
  `:macro` metadata.
- `require` of a namespace defined earlier in the same file is satisfied.
- `File.setLastModified` actually sets the file's mtime.
- `String.codePointAt` and `Character/toChars`; bigint edge-case coercions.

## [0.1.7] - 2026-07-06

### Added

- `jolt build --library` ahead-of-time compiles a project into a managed-runtime
  shared library (C ABI) for embedding Jolt in host applications, with
  Windows-friendly naming, build-time toolchain validation, and robust
  initialization.

### Changed

- The boot script now probes multiple names for the `chez` executable, improving
  discovery across installs.

## [0.1.6] - 2026-07-04

### Changed

- `JOLT_TRACE` tail-frame history now resolves each frame to its `ns/name`
  (`file:line`) source position instead of an opaque call site.

## [0.1.5] - 2026-07-04

### Fixed

- `JOLT_TRACE` is honored at runtime in a built `joltc` binary — it was
  previously baked in at build time and ignored the environment on the target
  machine.

## [0.1.4] - 2026-07-04

### Added

- Tail-call-optimized (elided) frames are recovered and shown in uncaught-error
  stack traces.

### Changed

- Tracing is on by default during REPL-driven development; `JOLT_TRACE` uses a
  single case-insensitive off-check covering both enable paths.

### Fixed

- Ahead-of-time builds run `-main` with `*ns* = user`, matching `clojure.main`.

## [0.1.3] - 2026-07-04

### Added

- Clojure 1.13 parity: `req!`, checked-keys destructuring, and keyword array maps.

### Fixed

- `build` invoked with a no-main entry namespace now runs the namespace as a
  script instead of crashing.

## [0.1.2] - 2026-07-04

### Added

- A `joltc` version string.

### Fixed

- nREPL server runs on Windows.
- `deps.edn` files that omit `org.clojure/clojure` no longer warn.
- Missing vendor submodules now fail with an actionable error.

## [0.1.1] - 2026-07-02

### Added

- Windows release binaries (x86_64) built via MSYS2/MinGW and statically linked
  into a single-file executable.
- The `clojure-test-suite` is vendored as a standing conformance gate
  (`make cts`).
- Every conformance corpus row is tagged with `:portability` (`:common` vs.
  `:jvm`).
- A single `IRef` seam shares watches, validators, and metadata across `atom`,
  `var`, and `agent`.

### Changed

- Binary numeric operators dispatch through a Numbers-style category model.
- Hierarchy functions follow the reference contracts, and `deftype` classes join
  the class graph.
- `clojure.string` performs `toString` coercion; `some-fn`/`ifn?` follow
  reference semantics.
- The reader enforces strict tokens, and EDN mode matches the reference's error
  contracts.
- `rand-nth` follows the reference shape.

### Fixed

- General divergences surfaced by the `clojure-test-suite`.
- `clojure.test/are` substitutes through `clojure.template`.
- Checked narrow casts, and runtime `require` in self-contained-built binaries.

### Removed

- Delisted `next.jdbc` (JVM/JDBC-driver dependent).
- Dropped `x86_64-macos` from releases (GitHub retired the Intel runner).

## [0.1.0] - 2026-07-01

Initial public release. Jolt is a self-hosting Clojure implementation on
[Chez Scheme](https://cisco.github.io/ChezScheme/) — it reads Clojure source,
analyzes it to a host-neutral IR, emits Scheme, and runs it on Chez, shipping a
Clojure-compatible standard library.

### Added

- **Language & runtime**: a self-hosted compiler (reader → analyzer → IR →
  Scheme backend) written in Clojure and driven by a checked-in bootstrap seed;
  `bin/joltc` evaluates expressions, runs a line REPL, and serves an nREPL
  server.
- **Persistent collections**: 32-way-trie vectors, HAMT hash maps and sets, with
  transient variants and linear-time builds.
- **Numeric tower**: exact integers, bignums, ratios, and doubles; category-aware
  `=` (`(= 3 3.0)` ⇒ `false`) and value-equality `==`.
- **Sequences & transducers**: lazy and infinite sequences, plus
  transducer-returning `map`/`filter`/`take`/… and `transduce`, `into`,
  `sequence`, `eduction`, and `reduced`.
- **Types & abstractions**: multimethods with hierarchies;
  `defprotocol`/`deftype`/`defrecord`/`reify`/`extend-protocol`/`extend-type`;
  metadata; and full `ns` forms.
- **Reference & concurrency types**: atoms (per-atom mutex, JVM-style CAS),
  volatiles, delays, `future`/`promise`/`agent`/`pmap`, and `clojure.core.async`
  over native channels.
- **Reader**: `#()` fn literals, `#_`, `#?` reader conditionals, tagged literals
  (`#inst`, `#uuid`), `#"…"` regex via vendored irregex, and a proper char type.
- **Runtime macroexpansion**: `eval`, `load-string`, and `defmacro` at runtime.
- **Standard library**: `clojure.string`, `clojure.set`, `clojure.walk`,
  `clojure.edn`, `clojure.pprint`, and the `jolt.ffi` foreign-function interface
  (foreign-callable callbacks, binary-faithful buffer I/O, `:blocking` calls,
  and `:jolt/native` library declarations).
- **Host interop shim**: a subset of the `java.*` standard library (including
  `java.time` Duration/Period/enums) so portable Clojure loads; class tokens are
  names rather than loaded classes, with no reflection or `gen-class`/`proxy`.
- **Ahead-of-time builds**: `joltc build -m ns -o out` compiles a project into a
  single self-contained executable (runtime + `clojure.core` + stdlib + app +
  `deps.edn` dependencies) with `--opt` inference/inlining passes and opt-in
  `--direct-link` and `--tree-shake` whole-program dead-code elimination.
- **Standalone toolchain binary**: `make joltc-release`/`make joltc-debug` link a
  single `joltc` that runs and `build`s apps without a local Chez or C toolchain.
- **Conformance gates**: a JVM-sourced conformance corpus (`make corpus`/
  `make certify`), a bootstrap self-hosting fixpoint (`make selfhost`), and an
  SCI compatibility stress gate (`make sci`).
- **Distribution**: a self-contained `joltc` binary, a Homebrew tap, and an
  install script.

[Unreleased]: https://github.com/jolt-lang/jolt/compare/v0.8.1...HEAD
[0.8.1]: https://github.com/jolt-lang/jolt/compare/v0.8.0...v0.8.1
[0.7.28]: https://github.com/jolt-lang/jolt/compare/v0.7.27...v0.7.28
[0.7.16]: https://github.com/jolt-lang/jolt/compare/v0.7.15...v0.7.16
[0.7.15]: https://github.com/jolt-lang/jolt/compare/v0.7.14...v0.7.15
[0.7.6]: https://github.com/jolt-lang/jolt/compare/v0.7.5...v0.7.6
[0.7.5]: https://github.com/jolt-lang/jolt/compare/v0.7.4...v0.7.5
[0.7.4]: https://github.com/jolt-lang/jolt/compare/v0.7.3...v0.7.4
[0.7.3]: https://github.com/jolt-lang/jolt/compare/v0.7.2...v0.7.3
[0.7.2]: https://github.com/jolt-lang/jolt/compare/v0.7.1...v0.7.2
[0.7.1]: https://github.com/jolt-lang/jolt/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/jolt-lang/jolt/compare/v0.6.9...v0.7.0
[0.6.9]: https://github.com/jolt-lang/jolt/compare/v0.6.8...v0.6.9
[0.6.8]: https://github.com/jolt-lang/jolt/compare/v0.6.7...v0.6.8
[0.6.7]: https://github.com/jolt-lang/jolt/compare/v0.6.6...v0.6.7
[0.6.6]: https://github.com/jolt-lang/jolt/compare/v0.6.5...v0.6.6
[0.6.5]: https://github.com/jolt-lang/jolt/compare/v0.6.4...v0.6.5
[0.6.4]: https://github.com/jolt-lang/jolt/compare/v0.6.3...v0.6.4
[0.6.3]: https://github.com/jolt-lang/jolt/compare/v0.6.2...v0.6.3
[0.6.2]: https://github.com/jolt-lang/jolt/compare/v0.6.1...v0.6.2
[0.6.1]: https://github.com/jolt-lang/jolt/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/jolt-lang/jolt/compare/v0.5.20...v0.6.0
[0.5.20]: https://github.com/jolt-lang/jolt/compare/v0.5.19...v0.5.20
[0.5.19]: https://github.com/jolt-lang/jolt/compare/v0.5.18...v0.5.19
[0.5.18]: https://github.com/jolt-lang/jolt/compare/v0.5.17...v0.5.18
[0.5.17]: https://github.com/jolt-lang/jolt/compare/v0.5.16...v0.5.17
[0.5.16]: https://github.com/jolt-lang/jolt/compare/v0.5.15...v0.5.16
[0.5.15]: https://github.com/jolt-lang/jolt/compare/v0.5.14...v0.5.15
[0.5.14]: https://github.com/jolt-lang/jolt/compare/v0.5.13...v0.5.14
[0.5.13]: https://github.com/jolt-lang/jolt/compare/v0.5.12...v0.5.13
[0.5.12]: https://github.com/jolt-lang/jolt/compare/v0.5.11...v0.5.12
[0.5.11]: https://github.com/jolt-lang/jolt/compare/v0.5.10...v0.5.11
[0.5.10]: https://github.com/jolt-lang/jolt/compare/v0.5.9...v0.5.10
[0.5.9]: https://github.com/jolt-lang/jolt/compare/v0.5.8...v0.5.9
[0.5.8]: https://github.com/jolt-lang/jolt/compare/v0.5.7...v0.5.8
[0.5.7]: https://github.com/jolt-lang/jolt/compare/v0.5.6...v0.5.7
[0.5.6]: https://github.com/jolt-lang/jolt/compare/v0.5.5...v0.5.6
[0.5.5]: https://github.com/jolt-lang/jolt/compare/v0.5.4...v0.5.5
[0.5.4]: https://github.com/jolt-lang/jolt/compare/v0.5.3...v0.5.4
[0.5.3]: https://github.com/jolt-lang/jolt/compare/v0.5.2...v0.5.3
[0.5.2]: https://github.com/jolt-lang/jolt/compare/v0.5.1...v0.5.2
[0.5.1]: https://github.com/jolt-lang/jolt/compare/v0.5.0...v0.5.1
[0.5.0]: https://github.com/jolt-lang/jolt/compare/v0.4.15...v0.5.0
[0.4.15]: https://github.com/jolt-lang/jolt/compare/v0.4.14...v0.4.15
[0.4.14]: https://github.com/jolt-lang/jolt/compare/v0.4.13...v0.4.14
[0.4.13]: https://github.com/jolt-lang/jolt/compare/v0.4.12...v0.4.13
[0.4.12]: https://github.com/jolt-lang/jolt/compare/v0.4.11...v0.4.12
[0.4.11]: https://github.com/jolt-lang/jolt/compare/v0.4.10...v0.4.11
[0.4.10]: https://github.com/jolt-lang/jolt/compare/v0.4.9...v0.4.10
[0.4.9]: https://github.com/jolt-lang/jolt/compare/v0.4.8...v0.4.9
[0.4.8]: https://github.com/jolt-lang/jolt/compare/v0.4.7...v0.4.8
[0.4.7]: https://github.com/jolt-lang/jolt/compare/v0.4.6...v0.4.7
[0.4.6]: https://github.com/jolt-lang/jolt/compare/v0.4.5...v0.4.6
[0.4.5]: https://github.com/jolt-lang/jolt/compare/v0.4.4...v0.4.5
[0.4.4]: https://github.com/jolt-lang/jolt/compare/v0.4.3...v0.4.4
[0.4.3]: https://github.com/jolt-lang/jolt/compare/v0.4.2...v0.4.3
[0.4.2]: https://github.com/jolt-lang/jolt/compare/v0.4.1...v0.4.2
[0.4.1]: https://github.com/jolt-lang/jolt/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/jolt-lang/jolt/compare/v0.3.3...v0.4.0
[0.3.3]: https://github.com/jolt-lang/jolt/compare/v0.3.2...v0.3.3
[0.3.2]: https://github.com/jolt-lang/jolt/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/jolt-lang/jolt/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/jolt-lang/jolt/compare/v0.2.8...v0.3.0
[0.2.8]: https://github.com/jolt-lang/jolt/compare/v0.2.7...v0.2.8
[0.2.7]: https://github.com/jolt-lang/jolt/compare/v0.2.6...v0.2.7
[0.2.6]: https://github.com/jolt-lang/jolt/compare/v0.2.5...v0.2.6
[0.2.5]: https://github.com/jolt-lang/jolt/compare/v0.2.4...v0.2.5
[0.2.4]: https://github.com/jolt-lang/jolt/compare/v0.2.3...v0.2.4
[0.2.3]: https://github.com/jolt-lang/jolt/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/jolt-lang/jolt/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/jolt-lang/jolt/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/jolt-lang/jolt/compare/v0.1.7...v0.2.0
[0.1.7]: https://github.com/jolt-lang/jolt/compare/v0.1.6...v0.1.7
[0.1.6]: https://github.com/jolt-lang/jolt/compare/v0.1.5...v0.1.6
[0.1.5]: https://github.com/jolt-lang/jolt/compare/v0.1.4...v0.1.5
[0.1.4]: https://github.com/jolt-lang/jolt/compare/v0.1.3...v0.1.4
[0.1.3]: https://github.com/jolt-lang/jolt/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/jolt-lang/jolt/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/jolt-lang/jolt/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/jolt-lang/jolt/releases/tag/v0.1.0
