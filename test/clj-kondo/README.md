# clj-kondo export fixture

Static-analysis coverage for `clj-kondo.exports/jolt-lang/jolt/`, the
config and hook that let [clj-kondo](https://github.com/clj-kondo/clj-kondo)
see through `jolt.ffi`'s macros. `kondo_fixture.clj` is only ever linted, never loaded:
its `jolt.ffi` forms are there for clj-kondo to analyze, not to run.

## Run it

```bash
mkdir -p /tmp/kondo-fixture-check/.clj-kondo
cp test/clj-kondo/kondo_fixture.clj /tmp/kondo-fixture-check/
cd /tmp/kondo-fixture-check
clj-kondo --lint /path/to/this/jolt/checkout --dependencies --copy-configs
clj-kondo --lint kondo_fixture.clj
```

Expect exactly these seven errors, one per call in the fixture's "wrong
arity" section, and nothing else: no unresolved-symbol or unresolved-var
finding anywhere in the "right arity" section or the macro definitions
above it:

```
kondo_fixture.clj:88:22: error: kondo-fixture/c-strlen is called with 2 args but expects 1
kondo_fixture.clj:89:24: error: kondo-fixture/safe-strlen is called with 0 args but expects 1
kondo_fixture.clj:90:33: error: kondo-fixture/c-fcntl is called with 2 args but expects 3
kondo_fixture.clj:91:35: error: kondo-fixture/c-fcntl is called with 4 args but expects 3
kondo_fixture.clj:92:29: error: kondo-fixture/c-bare-snprintf is called with 2 args but expects 3 or more
kondo_fixture.clj:93:22: error: kondo-fixture/multi-wrapper is called with 0 args but expects 1 or 2
kondo_fixture.clj:94:24: error: kondo-fixture/multi-wrapper is called with 3 args but expects 1 or 2
linting took Nms, errors: 7, warnings: 0
```

The line numbers hold only while the fixture's own line count does; if the
file changes, re-derive them from a fresh run rather than trusting this
copy of the output.

## What it covers

Every `defcfn` shape `def-cfn-form` accepts (plain, docstring, attribute
map, `:blocking`, an options map, the raw-binding wrapper in both its
single- and multi-arity forms, and both the declared-tail and bare forms
of the `:varargs`/`:&` marker), plus `layout`, `with-arena`, `with-alloc`,
`with-out`, `with-layout`, `with-c-string`, `with-c-string-array`, `cfn`,
`foreign-fn`, `foreign-callable`, `callback` and `export!`, which is every public
macro in `stdlib/jolt/ffi.clj` except the errno accessors, which are plain
`defcfn`s already covered by the shapes above.
