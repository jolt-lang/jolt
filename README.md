# Jolt

[![tests](https://github.com/jolt-lang/jolt/actions/workflows/tests.yml/badge.svg)](https://github.com/jolt-lang/jolt/actions/workflows/tests.yml)

Jolt is a Clojure implementation on top of Scheme. It works by compiling Clojure source to a host-neutral intermediate representation, which is emitted as Scheme and then compiled to native code. Clojure layer compiles to portable Scheme which, in turn, targets a specific runtime backend. Currently, Chez Scheme is the primary host, and Gamit Scheme is supported as a secondary target.

A Clojure-compatible standard library is included, and [many JVM Clojure libraries](https://jolt-lang.net/docs/libraries.html#jvm_libraries) work out of the box.

For more about Jolt, installation and documentation,
[check the website](https://jolt-lang.github.io).

### Quickstart

Or with the install script, which puts the binary in `~/.local/bin` (or
`/usr/local/bin` when run as root):

```bash
curl -sL https://raw.githubusercontent.com/jolt-lang/jolt/main/install | bash
```

For other installation options check [Install](https://github.com/jolt-lang/jolt/#install) section. 

Once Jolt is installed, try it out with:

```bash
$ jolt -e '(->> (range 10) (filter even?) (map (fn [x] (* x x))) (reduce +))'
120
$ jolt -e '(/ 1 2)'
1/2
```

You can also use `#!/usr/bin/env jolt` to make an executable script:

```bash
$ cat hello
#!/usr/bin/env jolt
(println "hello" (first *command-line-args*))
$ chmod +x hello
$ ./hello world
hello world
```

When the current directory has a `deps.edn`, Jolt resolves it first, so the
program sees the project's namespaces and its dependencies. The familiar `clj`
options work as you would expect: `-Sdeps`, `-A`, `-M:alias`, `-Spath`, `-Stree`,
including git and Maven dependencies.

## Install

Prebuilt binaries are self-contained, with the runtime, compiler, and standard
library packaged in a single file, for Linux x86_64 (glibc 2.17+), macOS arm64 and x86_64
(11+), Windows x86_64, and Android arm64 under Termux.

With Homebrew:

```bash
brew install jolt-lang/jolt/jolt
```

Or download the archive for your platform from the
[releases page](https://github.com/jolt-lang/jolt/releases). The "Source code"
archives GitHub attaches to a release omit the submodules,
so you should clone the repository instead.

Nix users can run a checkout directly:

```sh
nix run 'git+https://github.com/jolt-lang/jolt.git?submodules=1'
```

A source checkout can be run directly without needing a build step, because the bootstrap seed is
checked in:

```bash
git clone --recurse-submodules https://github.com/jolt-lang/jolt.git
cd jolt
bin/jolt -e '(+ 1 2)'        # => 3
```

The submodules are needed because they carry jolt's regex engine, its Maven resolver, and its test
suites. A checkout missing them needs to run:

`git submodule update --init --recursive`.

## Differences from Clojure

Most portable Clojure runs unchanged, and all the standard features behave the same as they do on the JVM. There are a few differences that matter however:

- There is no Java runtime available, so JVM reasoning about features such as garbage collection does not carry over.
  - Interop syntax (`Class.`, `Class/static`, `.method`) resolves against a
  shimmed subset of the `java.*` standard library written in Scheme.
  - There is no reflection and no `gen-class`.
  - For calling C libraries directly, jolt provides the `jolt.ffi` foreign-function interface.
- Strings are indexed by codepoint rather than by UTF-16 unit:
  `(count "😀")` is 1 rather than 2, and `subs` does not split characters.
- Regex Patterns compile through a vendored
  [irregex](https://github.com/ashinn/irregex) engine rather than
  `java.util.regex`, behind a translator that reads Java's grammar. Most common
  patterns behave as on the JVM, but there may be differences.
- The coverage of `clojure.core` is implemented against a
  JVM-sourced conformance corpus. It is broad, but not yet total.
- Clojure is a terminal dependency because Jolt supplies
  its own version of `org.clojure/clojure`.
  JVM Clojure pulls in `spec.alpha` transitively, but on Jolt a project that wants
  `clojure.spec.alpha` has to declare it explicitly.

Jolt also adds a few reader conveniences that do not have a JVM equivalent: a
`.jolt` file extension for jolt-specific code, digit separators such as
`1_000_000`, `#$"a ~{x}"` string interpolation, and an open `#` dispatch table
via `jolt.reader/set-dispatch-macro!`.

The full list of value-level divergences is tracked and gated in
[test/conformance/known-divergences.edn](test/conformance/known-divergences.edn);
the prose version is
[Differences from Clojure](https://jolt-lang.github.io/docs/differences.html)
on the docs site.

## Tools

- `jolt build` compiles a project into a single self-contained executable. The
  runtime, `clojure.core`, the standard library, the app, and its dependencies
  are linked in, and the result is a standalone binrary. `--dev` keeps every app var redefinable, while `--closed-world` prunes everything that cannot be reached from `-main`. See
  [Building & Running](https://jolt-lang.github.io/docs/building-and-deps.html).
- `jolt build --library` produces a shared object with a small C ABI that a
  C, C++, or Rust host can link or `dlopen`. See
  [Native Interop](https://jolt-lang.github.io/docs/native-interop.html).
- `jolt.ffi` binds C libraries directly from Clojure. On Jolt this is treated as the equivalent of Java interop in the JVM Clojure, and is used by the official libraries to wrap things like database drivers.
- `jolt repl` starts a line REPL, and `jolt nrepl-server` starts an nREPL
  server that CIDER, Calva, and Cursive connect to. See
  [REPL-Driven Development](https://jolt-lang.github.io/docs/repl-driven-development.html).
- Projects can define babashka-style tasks in `bb.edn` or `deps.edn` can be run with
  `jolt task-name`.

## Documentation

Full documentation is at [jolt-lang.github.io](https://jolt-lang.github.io):
[Getting Started](https://jolt-lang.github.io/docs/getting-started.html),
[Differences from Clojure](https://jolt-lang.github.io/docs/differences.html),
[Host Interop](https://jolt-lang.github.io/docs/host-interop.html),
[Native Interop (FFI)](https://jolt-lang.github.io/docs/native-interop.html),
[Writing Libraries](https://jolt-lang.github.io/docs/writing-libraries.html),
the [language specification](https://jolt-lang.github.io/docs/spec/README.html),
and the [RFCs](https://jolt-lang.github.io/docs/rfc/README.html). Every page is
listed in [`llms.txt`](llms.txt) as well.

## Contributing

Building from source, the seed and re-minting, the architecture, the Scheme
backends, and the test gates are in [CONTRIBUTING.md](CONTRIBUTING.md).

## License

[Eclipse Public License 2.0](https://www.eclipse.org/legal/epl-2.0/)

Jolt builds on other people's work, among them Chez Scheme, Clojure and
ClojureScript, and babashka with its fs, process, and cli libraries.
[NOTICE](NOTICE) lists each one with its copyright and license, and
[licenses/](licenses/) holds the license texts. Those parts stay under their
own licenses.
