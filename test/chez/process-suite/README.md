# babashka.process upstream suite runner

Runs the vendored [babashka/process](https://github.com/babashka/process) test
suite (`vendor/process/test`) against joltc, to measure conformance of the
`java.lang.ProcessBuilder` / `Process` host shim and `jolt.process`.

The reliable CI gate is `test/chez/process-test.clj` (wired into `smoke.sh`); this
runner is a manual conformance harness, like `cts-app`.

## Running

```
JOLT_PWD="$PWD/test/chez/process-suite" BABASHKA_TEST_ENV=jvm \
  bin/joltc -M:proc babashka.process-test
```

`BABASHKA_TEST_ENV=jvm` makes the many tests that shell out to `babashka` itself
skip gracefully when `bb` is not on the `PATH` (they are neither pass nor fail).

## Baseline (bb absent)

`babashka.process-test`: **19 assertions pass, 0 failures.** The bb-dependent
tests skip. The one remaining error is `resolve-program-macos-linux-test`, which
tests babashka's own PATH/program resolution and needs staged executable fixtures
under `process/test-resources/` plus a writable repo-root `target/` — it is
environment-specific, not a `jolt.process` defect.

If `bb` is installed the bb-gated tests also run and exercise real babashka
sub-process interaction; those results are environment-dependent.
