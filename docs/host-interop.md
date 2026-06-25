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
  `setProperty` `clearProperty` `getProperties` `getenv`.
- **`java.lang.Thread`** — `sleep` (real), `yield`/`interrupted` (no-ops),
  `currentThread`.
- **`java.lang.Object`** — `(Object.)` as a fresh-identity sentinel; `.toString`
  `.hashCode` `.equals` `.getClass` work on any value.
- **`java.lang.Class`** — `forName`.

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
- **`java.util.HashMap`** — `put` `get` `getOrDefault` `containsKey`
  `containsValue` `size` `isEmpty` `remove` `clear` `putAll` `keySet` `values`
  `entrySet`.

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
  `UnsupportedOperationException` and the common network exceptions, each with
  the `(E.)` / `(E. msg)` / `(E. msg cause)` / `(E. cause)` constructors.

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

Extending a *built-in* class instead (adding a method to core's `String` shim,
say) means editing the relevant `host/chez/*.ss` file and running `make remint`
— see [building-and-deps.md](building-and-deps.md).
