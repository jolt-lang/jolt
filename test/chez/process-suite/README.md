# babashka.process upstream suite runner

Runs the vendored [babashka/process](https://github.com/babashka/process) test
suite (`vendor/process/test`) against joltc, to measure conformance of the
`java.lang.ProcessBuilder` / `Process` host shim and `jolt.process`.

The reliable CI gate is `test/chez/process-test.clj` (wired into `smoke.sh`); this
runner is a manual conformance harness, like `cts-app`.

## Running

```
host/chez/process-suite.sh
```

The script stages what the suite needs and isolates babashka:

- **Program-resolution fixtures.** The program-resolution tests copy
  `test-resources/print-dirs.sh` into `target/test/{on-path,cwd,workdir}` and
  resolve a bare name via `PATH`, so the script symlinks the fixtures into a temp
  project and puts the `on-path` dir on `PATH`.
- **bb isolation.** Most other tests shell out to `babashka` itself (one can
  hang). The script runs joltc under a minimal tool `PATH` (just `chez` +
  `/usr/bin:/bin`) that excludes the dir holding `bb`, so `find-bb` returns nil
  and those tests skip (`BABASHKA_TEST_ENV=jvm`).

Everything is staged in a temp dir — no writes into the repo.

## Baseline

`babashka.process-test`: **27 assertions pass, 0 fail, 0 errors.** The bb-gated
tests skip.

## Manual run

For a quick run without the fixture staging (the program-resolution test will
error on the missing fixtures, and bb-gated tests run if `bb` is on `PATH`):

```
JOLT_PWD="$PWD/test/chez/process-suite" BABASHKA_TEST_ENV=jvm \
  bin/joltc -M:proc babashka.process-test
```
