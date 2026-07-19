# Cross-compilation POC (issue #375)

Answer to [jolt-lang/jolt#375](https://github.com/jolt-lang/jolt/issues/375):
**yes — cross-compilation works today**, with no changes to jolt itself.
This directory holds the proof-of-concept that built and verified it.

## What was proven

On an ARM64 Mac (`tarm64osx`, Chez 10.4.1), `jolt build` output was
cross-compiled into an **x86_64 macOS** (`ta6osx`) executable and verified
under Rosetta 2:

| host → target | app | result |
|---|---|---|
| tarm64osx → ta6osx | `hello/` (pure fns, loop/recur, reduce) | `Mach-O 64-bit executable x86_64`; output byte-identical to the native arm64 build (Rosetta 2) |
| tarm64osx → ta6osx | `examples/hiccup-app` (real git dep, macros) | same — byte-identical HTML output |
| ta6le → tarm64le | `hello/` (Arch Linux x86_64 host) | `ELF 64-bit LSB pie executable, ARM aarch64`; identical output, exit 0 under `qemu-aarch64-static` |

Additionally verified on **physical Intel hardware**: the cross-built
`hello-x86` was transferred (sha256-matched) to an Intel MacBook Pro
(macOS 26.5.2, x86_64) and ran natively with identical output, exit 0.
For binaries meant for other Macs, pass
`ARCH_FLAG="-arch x86_64 -mmacosx-version-min=11.0"` (and build the target
kernel with the same `-mmacosx-version-min` in `CFLAGS`) — clang otherwise
stamps the build host's OS version as the binary's minimum.

`otool -L` on the produced binaries shows only macOS system libraries
(`libSystem`, `libncurses`, `libiconv`, `Foundation`) — lz4/zlib are linked
statically, so the binary is as self-contained as a native `jolt build` one.

## Why it works

`jolt build` (host/chez/build.ss) splits exactly at the machine boundary:

1. **Steps 1–3 are machine-neutral.** Loading the entry ns, re-emitting each
   namespace to Scheme, and flattening runtime + app + launcher into
   `<out>.build/flat.ss` produces plain Scheme *text*. Audited: no absolute
   host paths, no host machine-type baked as data; every `(machine-type)`
   in the image is a runtime call (so `os.name` etc. are correct on the
   target at runtime).
2. **Step 4 is machine-specific** — `compile-file` → `make-boot-file` →
   cc link — and Chez Scheme ships cross-compilation for precisely those
   two functions. `make bootquick XM=<target>` emits the target's boot
   files **plus** `xc-<target>/s/xpatch`; loading that xpatch into the host
   Chez retargets `compile-file`/`make-boot-file` to emit target-machine
   code (ChezScheme/BUILDING, "CROSS COMPILING SCHEME PROGRAMS").
   The target's C kernel (`libkernel.a`) comes from
   `./configure --cross --force -m=<target>` — for macOS arm64→x86_64 that
   is literally just `CFLAGS="-arch x86_64"`.

`cross-build-poc.sh` is jolt build's step 4, retargeted. Everything else is
stock `jolt build`.

## Reproduction

```sh
# one-time: prepare Chez artifacts (~10 min total on an M-series Mac)
git clone --recurse-submodules https://github.com/cisco/ChezScheme ~/dev/ChezScheme
cd ~/dev/ChezScheme
./configure -m=tarm64osx && make          # host compiler
make bootquick XM=ta6osx                  # target boots + xpatch
./configure --cross --force -m=ta6osx CFLAGS="-arch x86_64" CC_FOR_BUILD=cc
make                                      # target kernel

# per app (~1 min)
cd tools/cross-compile/hello
../../../bin/joltc build -m hello.core -o hello-host        # normal host build
CHEZ_SRC=~/dev/ChezScheme ../cross-build-poc.sh hello-host.build hello-x86
file hello-x86                 # Mach-O 64-bit executable x86_64
arch -x86_64 ./hello-x86       # runs under Rosetta; same output as ./hello-host
```

## Trying it from a Linux host

The same flow works with Linux machine types (`ta6le` = x86_64, `tarm64le` =
aarch64) — **verified 2026-07-19 on Arch Linux** (packages:
`aarch64-linux-gnu-gcc`, `qemu-user-static`):

```sh
cd ~/dev/ChezScheme
./configure -m=ta6le && make
make bootquick XM=tarm64le
# --disable-curses --disable-x11: a cross sysroot has no ncurses or X11
# headers, and the expeditor otherwise needs both (same reason jolt's CI
# builds Chez with X11 off).
./configure --cross --force -m=tarm64le --toolprefix=aarch64-linux-gnu- \
  --disable-curses --disable-x11 CC_FOR_BUILD=cc
make

HOST_M=ta6le TARGET_M=tarm64le ARCH_FLAG="" CC=aarch64-linux-gnu-gcc \
  CHEZ_SRC=~/dev/ChezScheme tools/cross-compile/cross-build-poc.sh app.build app-arm64
qemu-aarch64-static -L /usr/aarch64-linux-gnu ./app-arm64   # or a real ARM box
```

The script picks the matching Linux link libs automatically for `*le`
targets. A machine that only *runs* a cross-built binary needs nothing
installed — no Chez, no jolt.

## What a real `jolt build --target <machine>` needs

The POC hand-drives step 4; productizing it in build.ss is modest:

1. **Spawn-path cross compile.** `build-with-cc` already spawns a fresh Chez
   for `compile-file`/`make-boot-file`; a `--target` flag only needs to
   prepend `(load ".../xc-<target>/s/xpatch")` to the generated compile.ss.
   (The self-contained path compiles in-process and would keep using the
   spawn path when cross-targeting.)
2. **Key platform decisions off the target, not the host.** `bld-osx?` /
   `bld-nt?` / `bld-link-libs` / the `.exe` suffix all read the *host*
   `(machine-type)` today; under `--target` they must read the target
   machine string.
3. **Target packs.** `JOLT_CHEZ_CSV` already overrides where the boots +
   kernel come from. A per-target bundle (petite/scheme.boot, libkernel.a,
   scheme.h, static lz4/zlib, prebuilt launcher stub — ~10 MB, exactly what
   the `csv<ver>/<machine>` install layout already contains, plus the
   xpatch) could be published per release, Zig/Go style. With a prebuilt
   *target* launcher stub, the append-payload path needs **no C compiler at
   all** for pure-Clojure apps: the stub framing
   (`[stub][boot][u64 len]["JOLTBOOT"]`) is arch-independent.
4. **C cross-compilers only where unavoidable** (`:jolt/native` static
   archives, `--library`): macOS↔macOS is `-arch`; Linux targets from any
   host work with `zig cc -target x86_64-linux-gnu`; Windows needs
   mingw-w64 (`--toolprefix=x86_64-w64-mingw32-`, same as Chez's own cross
   build).
5. **Version pinning.** Host compiler, xpatch, target boots, and target
   kernel must come from one Chez tree (jolt already pins 10.4.1 in CI).

Caveats: `:jolt/native` archives must be provided per target arch; app
macros that inspect the build host at compile time would bake host facts
(none of the stdlib does); untested here: Windows targets and cross-OS
(mac→linux) link, which needs the zig/mingw toolchain from item 4.

## Why bother (beyond the issue)

`.github/workflows/release.yml` ships **no x86_64-macos binary** — GitHub
retired the Intel runners ("Intel Macs build from source"). Item 1–3 above
would restore that artifact by cross-building `ta6osx` on the existing
`macos-14` arm64 runner, POC'd here end to end.

## Observed timings (M-series Mac)

- host Chez build: ~6 min (one-time)
- `bootquick XM=ta6osx`: ~2 min (one-time)
- cross kernel: ~1 min (one-time)
- per-app cross step 4 (compile + boot + link): ~30 s
