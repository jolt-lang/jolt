# Host interop and JVM standard-library shims

Jolt runs on Chez Scheme, not the JVM, so there are no real Java classes behind
interop forms. Instead the runtime ships shims for the slice of the JVM standard
library that portable Clojure code reaches for, so libraries written against
`clojure.core` and common `java.*` classes run unchanged. The Clojure interop
syntax works against these shims:

```clojure
(Math/sqrt 2)                  ; static call
Math/PI                        ; static field
(StringBuilder.)               ; constructor
(.append sb "x")               ; instance method
(instance? String "hi")        ; class token
```

A class token (`String`, `java.util.UUID`, …) resolves to a name; there is no
reflection and no class hierarchy. `(class x)` returns the JVM class name for the
scalar/collection types Clojure programs compare against (`"java.lang.Long"`,
`"java.lang.String"`, and so on).

## Source layering: JVM-specific code lives in the java layer

Keep anything JVM-specific in `host/chez/java/`. The rest of the runtime stays
JVM-free, and the compiler in `jolt-core/` is JVM-free by construction.

- `host/chez/java/` holds the JVM model: the `java.*` mirrors, the class tokens
  and class hierarchy, `(class x)`/`(type x)`/`instance?`, exception classes, the
  interop dispatch for `.method`/`Class/static`/`(Class.)`. If a value or name
  only means something because the JVM has it, it belongs here.
- The rest of `host/chez/` is the host-neutral runtime — the value model
  (`values.ss`, `collections.ss`, `seq.ss`), reader, vars, multimethods, meta. It
  speaks jolt's own taxonomy (`:string`, `:vector`, `:jolt/inst`), never JVM class
  names.
- `jolt-core/` (the Clojure compiler + `clojure.core` overlay) emits and reasons
  in that taxonomy only. The JVM mapping happens *after*, in the java layer.

The worked example is `type`. The core layer (`natives-meta.ss`) computes the
keyword taxonomy and binds it as `__type-tag` — that's what `print-method` and the
reader dispatch on, with no JVM in scope. The java layer (`java/host-class.ss`)
then rebinds the public `clojure.core/type` to Clojure's `(or (:type meta) (class
x))`, mapping `:jolt/inst` → `java.util.Date` and so on, right next to `(class
…)`. So the compiler keeps emitting `:jolt/inst`; the java layer remaps it.

When you add interop behaviour, prefer registering it through the generic hooks a
java-layer file already uses — `register-class-arm!` for `(class x)`,
`register-instance-check-arm!` for `instance?`, `register-eq-arm!` for value
equality — rather than threading a JVM concept back into a host-neutral file. A
new `java.*` shim is a new file under `host/chez/java/` loaded from `rt.ss`, not a
branch added to `collections.ss` or `seq.ss`.

## What's shimmed

This is the surface today, not the whole JVM. Methods not listed generally
aren't implemented; a few are accepted but no-ops (noted inline).

### Numbers and language

- **`java.lang.Math`** — `sqrt` `cbrt` `pow` `exp` `log` `log10` `floor` `ceil`
  `round` `abs` `max` `min` `sin` `cos` `tan` `asin` `acos` `atan` `signum`
  `random`; fields `PI`, `E`. (`clojure.math` mirrors these as functions.)
- **`Long` / `Integer`** — `parseLong`/`parseInt`/`valueOf` (optional radix),
  `MAX_VALUE`, `MIN_VALUE`; `(Integer. x)`.
- **`Double` / `Float`** — `parseDouble`, `valueOf`, `toString`, `isNaN`,
  `isInfinite`, the `*_VALUE`/`*_INFINITY`/`NaN` fields; `(Double. s)`.
- **`Boolean`** — `parseBoolean`, `TRUE`, `FALSE`.
- **`Character`** — `isUpperCase` `isLowerCase` `isDigit` `isWhitespace` (ASCII).
- **Boxed-number methods** — every number answers `.intValue` `.longValue`
  `.doubleValue` `.floatValue` `.byteValue` `.shortValue` `.toString`
  `.hashCode` (integer projections wrap modulo their width, as on the JVM).
- **`java.lang.System`** — `currentTimeMillis` `nanoTime` `exit` `getProperty`
  `setProperty` `clearProperty` `getProperties` `getenv` `gc` (a full Chez
  collection — clears weak references and fires their queues).
- **`java.lang.Thread`** — real OS threads over Chez `fork-thread`, sharing the
  one heap (a captured atom/var is shared): `(Thread. thunk)` + `start` / `join` /
  `run` / `isAlive`; plus `sleep` (real), `yield`/`interrupted`/`interrupt`
  (no-ops), `currentThread`.
- **`java.util.concurrent.CountDownLatch`** — `(CountDownLatch. n)` + `countDown`
  / `await` / `getCount`, a real counting barrier (mutex + condition).
- **`java.lang.ref.SoftReference` / `WeakReference` + `ReferenceQueue`** — genuine
  GC reclamation: the referent is held through a Chez weak pair, so the collector
  reclaims it once unreachable (`.get` then returns nil) and a guardian enqueues
  the reference on its `ReferenceQueue` (`poll`). Chez has no reference softer than
  weak, so a `SoftReference` clears on unreachability, not memory pressure — eager,
  but real eviction (core.cache's SoftCache).
- **`java.lang.Object`** — `(Object.)` as a fresh-identity sentinel; `.toString`
  `.hashCode` `.equals` `.getClass` work on any value.
- **`java.lang.Class`** — `forName` (throws a catchable `ClassNotFoundException`
  for a class jolt can't back, so `(try (Class/forName "opt.Dep") (catch …))`
  dependency probes work). There is no reflection, but a few common interfaces
  carry a modeled ancestry so `(supers c)` / `(ancestors c)` answer like the JVM —
  e.g. `(ancestors (class f))` for a function yields `Runnable` and `Callable`,
  the check `core.memoize` uses to validate a memoizable argument.

### Strings and text

- **`java.lang.String`** statics — `valueOf`, `format` (the `clojure.core/format`
  engine; `String/format` with a leading locale is accepted). Instance methods
  go through `clojure.string` / the native string ops.
- **`StringBuilder`** — `append` `toString` `length` `charAt` `setLength`.
- **`java.text.NumberFormat`** — `getInstance` `getNumberInstance`
  `getIntegerInstance`; `.format`, `.setGroupingUsed`,
  `.setMinimum/MaximumFractionDigits`.
- **`java.util.StringTokenizer`** — `hasMoreTokens` `countTokens` `nextToken`.
- **`java.util.regex.Pattern`** — `compile` (with `Pattern/MULTILINE`), `quote`;
  `.split`, `.pattern`. (`#"…"` literals and `clojure.string` regex fns are the
  usual entry points.)

### Collections (mutable)

- **`java.util.ArrayList`** — `add` `get` `set` `size` `isEmpty` `remove` `clear`
  `contains` `toArray` `iterator`.
- **`java.util.HashMap`** / **`java.util.concurrent.ConcurrentHashMap`** — `put`
  `get` `getOrDefault` `containsKey` `containsValue` `size` `isEmpty` `remove`
  `clear` `putAll` `keySet` `values` `entrySet`; `clojure.core`'s `get` / `count` /
  `contains?` also read them. (One shared heap, so the plain mutable map serves the
  concurrent one.)

### I/O

- **`java.io.File`** — `(File. path)` / `(File. parent child)`. A File keeps the
  path as given (`(.getPath (File. "rel"))` is `"rel"`, `.isAbsolute` false); a
  relative path resolves against `JOLT_PWD` only when the filesystem is touched.
  Methods: `getPath` `getName` `getParent` `getParentFile` `getAbsolutePath`
  `getAbsoluteFile` `getCanonicalPath` `getCanonicalFile` `toURI` `toURL`
  `exists` `isDirectory` `isFile` `isAbsolute` `isHidden` `length` `lastModified`
  `canRead` `canWrite` `canExecute` `list` `listFiles` `mkdir` `mkdirs` `delete`
  `createNewFile` `renameTo` `compareTo` `equals` `hashCode`. Statics:
  `File/separator` `File/separatorChar` `File/pathSeparator` `File/createTempFile`
  `File/listRoots`.
- **Byte streams** — `FileInputStream` / `FileOutputStream` (over a path/File,
  `append` arg), `ByteArrayInputStream` / `ByteArrayOutputStream`
  (`toByteArray`/`toString`/`size`/`reset`), `BufferedInputStream` /
  `BufferedOutputStream`. `read`/`read(byte[])`, `write(int)`/`write(byte[])`,
  `flush`, `close`. Each is a Chez binary port underneath.
- **Char streams** — `FileReader` / `InputStreamReader` (read a byte stream as
  UTF-8), `FileWriter` / `OutputStreamWriter`, `BufferedReader` (`readLine`,
  `lines`) / `BufferedWriter` (`newLine`), `StringReader` / `StringWriter` /
  `PushbackReader`.
- **`clojure.java.io`** — `file` `as-file` `reader` `writer` `input-stream`
  `output-stream` `copy` (byte-exact for byte sources) `make-parents`
  `delete-file` `resource` `as-url`. `slurp`/`spit`/`line-seq`/`with-open` work
  over all of the above.
- **`java.lang.ClassLoader`** — `getSystemClassLoader`, `.getResource`,
  `.getResourceAsStream` (resolved against the source roots).

### Time and date

- **`java.util.Date`** — `(Date.)` / `(Date. ms)`; `getTime` `toInstant`
  `toLocalDate(Time)` `before` `after` `equals` `toString` (RFC 3339).
- **`java.time`** — `Instant` (`now`, `ofEpochMilli`, `toEpochMilli`, `atZone`),
  `LocalDateTime`, `ZoneId`, `DateTimeFormatter` (`ofPattern`, `ISO_LOCAL_*`,
  localized styles), `FormatStyle`.
- **`java.text.SimpleDateFormat`** — `(SimpleDateFormat. pattern)`; `parse`
  `format` `toPattern` `applyPattern` (`setTimeZone`/`setLenient` accepted but
  ignored — formatting is UTC).
- **`java.util.TimeZone`** / **`java.util.Locale`** — constructed and passed
  through; only UTC is honored for formatting.

### Net, encoding, misc

- **`java.net.URL`** — `(URL. spec)`; `toString` `toExternalForm` `getProtocol`
  `getPath` `getFile`.
- **`java.net.URI`** — full component accessors (`getScheme` `getHost` `getPort`
  `getPath` `getQuery` `getFragment`, raw variants, `isAbsolute`).
- **`java.util.Base64`** — `getEncoder`/`getDecoder` with `encode`,
  `encodeToString`, `decode`.
- **`java.nio.charset.Charset`** — `forName`.
- **`java.util.UUID`** — `randomUUID`, `fromString`; `(UUID. s)`.
- **Exceptions** — `Throwable` `Exception` `RuntimeException`
  `IllegalArgumentException` `IllegalStateException` `IOException`
  `NumberFormatException` `ArithmeticException` `NullPointerException`
  `ClassCastException` `IndexOutOfBoundsException` `FileNotFoundException`
  `UnsupportedOperationException` `Error` `AssertionError` and the common network
  exceptions, each with the `(E.)` / `(E. msg)` / `(E. msg cause)` / `(E. cause)`
  constructors. `try` dispatches its `catch` clauses by class in order, respecting
  the exception supertype hierarchy (`(catch Exception e …)` catches a
  `RuntimeException` but not an `Error`); a thrown value matching no clause
  re-throws. An untyped host condition (e.g. from `(/ 1 0)`) is caught by a
  `RuntimeException`/`Exception`/`Throwable` clause.

What's deliberately absent: STM (`clojure.lang.LockingTransaction/isRunning`
returns `false`), reflection, `gen-class`/`proxy` of Java classes, and
`BigDecimal`.

## Adding your own shim from a library

The built-in shims above are baked into the seed. A library or project can
register its **own** host classes at load time — no seed re-mint, no host edits.
Put the registration calls at the top level of a namespace your code requires.
Four functions (in `clojure.core`) plus the tagged-table seam (in `jolt.host`)
cover it.

`__register-class-ctor!` makes `(Name. …)` work; `__register-class-statics!`
makes `Name/field` and `(Name/method …)` work; `__register-class-methods!`
attaches instance methods to a tagged value; `__register-instance-check!` teaches
`instance?` about your class. **Method and static names are strings** (they match
the literal name in the interop form).

A stateful object is a *tagged table* — `jolt.host/tagged-table` creates one,
`ref-put!`/`ref-get` set and read its fields. Read the tag back with
`jolt.host/ref-get` (or test it with `jolt.host/table?`); a plain `get` /
keyword lookup deliberately can't see a wrapper's own `:jolt/type`.

```clojure
(ns mylib.greeter
  (:require [jolt.host :as host]))

;; (Greeter. name) -> a tagged value carrying its name
(__register-class-ctor! "Greeter"
  (fn [name] (-> (host/tagged-table :greeter)
                 (host/ref-put! :name name))))

;; (.hello g) -> instance method, keyed by the literal method name
(__register-class-methods! :greeter
  {"hello" (fn [self] (str "hi " (host/ref-get self :name)))})

;; Greeter/VERSION (field) and (Greeter/make x) (static method)
(__register-class-statics! "Greeter"
  {"VERSION" "1.0"
   "make"    (fn [name] (Greeter. name))})

;; (instance? Greeter x)
(__register-instance-check!
  (fn [class-name v]
    (when (= class-name "Greeter")
      (and (host/table? v) (= :greeter (host/ref-get v :jolt/type))))))
```

```clojure
(.hello (Greeter. "ada"))            ;=> "hi ada"
Greeter/VERSION                      ;=> "1.0"
(.hello (Greeter/make "bob"))        ;=> "hi bob"
(instance? Greeter (Greeter. "x"))   ;=> true
```

An instance-check predicate returns `true`/`false` to decide, or `nil` to defer
to the next registered check and the built-ins — so several libraries can
register checks without clobbering each other. This is the mechanism jolt's
HTTP client library uses to emulate `java.net.URL` and `HttpURLConnection` so
`clj-http-lite` runs unchanged.

`__register-instance-check!` answers one `(instance? Foo x)` question. When a
class belongs to a *hierarchy* — a custom exception that should be caught as an
`IOException`, or a value that should match `(instance? SomeInterface x)` across
its whole supertype chain and dispatch a protocol extended to any of those
supertypes — declare its direct supers once with `jolt.host/register-class-supers!`
instead. `instance?`, `isa?`, `supers`/`ancestors`, and `extend-protocol`
dispatch all derive from the one declaration (supers are given by canonical name;
transitivity is computed):

```clojure
;; a library's exception type that catch/instance? should treat as an IOException
(jolt.host/register-class-supers! "com.acme.RetryExhaustedException"
                                  ["java.io.IOException"])

(throw (jolt.host/throwable "com.acme.RetryExhaustedException" "gave up"))
;; (catch java.io.IOException e …) now matches it; (instance? java.lang.Exception e) is true
```

deftype/defrecord classes join the same graph automatically at definition: a
record's ancestry carries the record interfaces (`clojure.lang.IRecord`,
`IPersistentMap`, `Associative`, …), a bare deftype carries
`clojure.lang.IType`, and every protocol the type implements inline appears as
an implemented interface — so `(ancestors MyRecord)`, `(isa? MyRecord
clojure.lang.IPersistentMap)`, and hierarchy relationships `derive`d on a
class's supers all answer like the JVM.

Extending a *built-in* class instead (adding a method to core's `String` shim,
say) means editing the relevant `host/chez/*.ss` file and running `make remint`
— see [building-and-deps.md](building-and-deps.md).

## Calling into Jolt from C

`bin/joltc build --library` (see the README) produces a shared object whose
entry points you reach through a C ABI instead of JVM-style interop. The Jolt
side uses `jolt.ffi/export!`; the C side uses `jolt_library_init` +
`jolt_lookup`. This is the inverse of `foreign-fn`: `foreign-fn` calls *out* of
Jolt into C; `export!` lets C call *in*.

```clojure
(defn add [x y] (+ x y))
(jolt.ffi/export! "add" add [:int :int] :int)
```

The argtype/rettype keywords are the same set `foreign-fn`/`ffi-type->chez`
accepts: `:int :uint :long :ulong :int64 :uint64 :size_t :ssize_t :iptr :uptr
:double :float :pointer` (alias `:void*`) `:string :void :uint8` (aliases
`:u8`/`:byte`) `:char`. A `:pointer`/`:void*` returns an opaque address you pass
back unchanged; `:string` copies a C string in/out.

```c
typedef int (*init_fn)(int, char**);
typedef void* (*lookup_fn)(const char*);
typedef int (*add_fn)(int, int);

void* h = dlopen("./libadd.so", RTLD_NOW | RTLD_LOCAL);
((init_fn)dlsym(h, "jolt_library_init"))(0, NULL);
add_fn add = (add_fn)((lookup_fn)dlsym(h, "jolt_lookup"))("add");
add(2, 3);                          /* => 5 */
```

Two things to keep in mind across the boundary. The library carries its own GC,
so call `jolt_library_init` exactly once on the host thread before any `jolt_lookup`
result, and call `jolt_library_shutdown` to tear it down. A value returned as
`:pointer`/`:void*` is not GC-tracked by the caller — if Jolt hands back a
pointer into managed memory you must keep it alive on the Jolt side (e.g. hold it
in a top-level ref) for as long as C uses it.
