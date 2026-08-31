# jolt — Clojure on Chez Scheme. Single substrate, no Janet.
#
# bin/jolt runs jolt directly off the checked-in seed (host/chez/seed/).
# `make build` builds the standalone binary, `make test` is the full gate, and
# `make remint` rebuilds the seed after a source change.

R := https://github.com/makeplus/makes
M ?= .cache/makes
# PINNED. Unpinned, this clone is whatever makeplus/makes HEAD is on the day of
# the build — including a release build, whose toolchain would then be decided by
# a repository this one records no version of. Bump deliberately.
V := d14cb578c2f04e6f9d00e01c7c4e416a9baf94e9
# Nix supplies a locked, source-only tree through M. Ordinary checkouts retain
# the Git clone so their pinned revision is verified and provisioned on demand.
$(shell [ -f '$M/init.mk' ] || git clone -q $R '$M')
# Fetch only when the pin is absent, so a normal build stays offline. A
# source-only Nix input has no .git directory, so there is nothing to fetch.
$(shell [ ! -d '$M/.git' ] || { git -C '$M' cat-file -e '$V^{commit}' 2>/dev/null || git -C '$M' fetch -q origin; git -C '$M' rev-parse -q --verify HEAD 2>/dev/null | grep -qx '$V' || git -C '$M' checkout -q '$V'; })

include $M/init.mk

# An explicit caller-selected Chez is authoritative. This preserves CI/release
# toolchains whose threading, libc floor, and native libraries are intentional.
#
# Same-or-newer system Chez: with nothing explicit selected, a Chez on PATH at
# or above JOLT-CHEZ-FLOOR is also used as-is — the system toolchain then
# builds and links everything, one self-consistent toolchain end to end, and
# nothing is downloaded. Older, broken, or absent, fall through to provisioning
# the pinned Chez + xPack GCC below. Set JOLT_SYSTEM_CHEZ= (empty) to always
# provision the pinned versions.
JOLT-CHEZ-FLOOR ?= 10.4.1
JOLT_SYSTEM_CHEZ ?= 1
JOLT-CHEZ := $(or $(CHEZ),$(CHEZSCHEME))
ifeq (,$(JOLT-CHEZ))
ifeq (1,$(JOLT_SYSTEM_CHEZ))
JOLT-CHEZ := $(shell \
  for name in chez chezscheme scheme; do \
    exe=$$(command -v $$name 2>/dev/null) || continue; \
    test -x "$$exe" || continue; \
    exe=$$(cd "$$(dirname "$$exe")" && pwd -P)/$$(basename "$$exe"); \
    v=$$(printf '(display (scheme-version)) (newline)\n' | "$$exe" -q 2>/dev/null | tr -d '\r'); \
    v=$$(printf '%s\n' "$$v" | awk '{print $$NF}'); \
    ok=$$(awk -v v="$$v" -v f="$(JOLT-CHEZ-FLOOR)" 'BEGIN { \
      split(v, V, "."); split(f, F, "."); \
      for (i = 1; i <= 3; i = i + 1) { \
        if (V[i] + 0 > F[i] + 0) { print 1; exit } \
        if (V[i] + 0 < F[i] + 0) { exit } \
      } \
      print 1 }'); \
    if [ "$$ok" = 1 ]; then printf '%s\n' "$$exe"; break; fi; \
  done)
endif
endif
ifneq (,$(JOLT-CHEZ))
SHELL-DEPS += $(JOLT-CHEZ)
else
include $M/chezscheme.mk
JOLT-CHEZ := $(CHEZSCHEME)
endif

include $M/clean.mk
include $M/shell.mk

MAKES-CLEAN := \
  build/ \
  target/ \

MAKES-REALCLEAN := \
  .jolt/ \
  test/chez/*/.jolt/ \
  test/chez/*/*/.jolt/ \

PREFIX ?= $(if $(IS-ROOT),/usr/local,$(HOME)/.local)
CHEZ ?= $(JOLT-CHEZ)

# Hand the selected Chez down to bin/jolt, so the targets that shell out to it
# run the same interpreter as the ones that call $(CHEZ) directly. When make
# provisions its own Chez (the one on PATH being a different version), bin/jolt's
# own PATH search would otherwise pick the other one.
export JOLT_CHEZ := $(CHEZ)

# A locally built Chez links its kernel against the bundled lz4 and zlib. Their
# static archives remain in the Makes build tree rather than the installed Chez
# prefix, so expose them when linking Jolt's standalone launcher.
ifneq (,$(CHEZSCHEME-LOCAL))
CHEZSCHEME-LIB-DIRS := \
  $(LOCAL-TMP)/$(CHEZSCHEME-DIR)/pb/lz4/lib \
  $(LOCAL-TMP)/$(CHEZSCHEME-DIR)/pb/zlib
export LIBRARY_PATH := $(subst $(space),:,$(strip $(CHEZSCHEME-LIB-DIRS)))$(if $(LIBRARY_PATH),:$(LIBRARY_PATH))
# The provisioned GCC built Chez, so the same driver links the standalone
# binary: gcc.mk puts the bundle's bin FIRST on the exported PATH, but the
# bundle ships no `cc`, so a bare cc falls through to the distro driver and
# pairs it with the bundle's older as/ld (#788: gcc 16 .base64 into pre-2.43
# gas). build.ss bld-cc reads JOLT_CC. A command-line JOLT_CC=... still wins.
export JOLT_CC := $(GCC)
endif

JOLT-TARGETS-NEEDING-DEPS := \
  aotcacheperf aotcachesmoke aotfingerprint asynctimer buildlibsmoke buildsmoke \
  aotcachepathsmoke compilepathsmoke contagion corpus cts dcerefs depssmoke depsunit devboot \
  readscaling vecscaling pipescaling chunkscaling printscaling complexity ioscaling hotscaling \
  devbootsmoke devirt directlink ffi fibers fieldjoin fieldnum fieldread flarr fnform coreproc grenadine \
  gateboot gatebootsmoke gosm hasheq httpsfetch infer inline inline-body irvalidate statlayout \
  jolt jolt-debug jolt-release joltsmoke libconformance mandelbrot-num mathfl mvnhttp \
  narrow numeric numwp oparity pic protoret printperf remint sbperf sci selfhost shakelocal \
  traceemit \
  shakesmoke smoke staticnativesmoke stateimage test testbin transient unit unitcontext \
  threadsafety values wp ci

# Only mark PHONY targets for names that have file system conflicts:
.PHONY: build install test ci gate-run-test gate-run-ci gate-status \
        gambitcheck gambitkernel gambiteval gambitseed gambitweb gambitprofile \
        gambitgen gambitgencheck gambitseedcheck grenadinecheck \
        fibersbench dynbench \
        fibersresidue

default:: build

# Honor an explicit Chez or install the pinned toolchain, initialize every
# vendored dependency, and enforce Jolt's threaded-runtime requirement.
deps: submodules $(JOLT-CHEZ)
	@threaded=$$(printf '(write (threaded?)) (newline)\n' | \
	  '$(JOLT-CHEZ)' -q 2>/dev/null | tr -d '\r'); \
	  test "$$threaded" = '#t' || { \
	    echo "Jolt requires a threaded Chez Scheme ($(JOLT-CHEZ) reported $$threaded)" >&2; \
	    exit 1; \
	  }

submodules:
	@if git submodule status --recursive | grep -q '^-'; then \
	  git submodule update --init --recursive; \
	fi

# Every target that runs Chez directly or through bin/jolt waits until deps is
# complete. This keeps direct targets and parallel aggregate gates race-free.
$(JOLT-TARGETS-NEEDING-DEPS): | deps

build: testbin

install: build
	install -d '$(PREFIX)/bin'
	install -m 755 target/release/jolt '$(PREFIX)/bin/jolt'

# --- the gate ---------------------------------------------------------------
#
# A gate is only worth anything if an INCOMPLETE run cannot be read as a pass.
# Three things go wrong otherwise, and all three have happened:
#
#   1. make stops at the first failing target, so every target after it never
#      runs. Re-running a hand-picked subset afterwards prints per-target
#      success that looks exactly like the whole gate's.
#   2. `make test | tail` reports tail's status, and the log then ENDS on some
#      passing target's output — the failure scrolled past.
#   3. `make -i` exits 0 even when targets failed.
#
# So: the gate runs as a sub-make, a verdict line is printed either way (the log
# can never end on a passing target), the exit status is preserved, and a receipt
# naming the covered tree is written ONLY on a complete pass. `make gate-status`
# answers "is this working tree gated?" — which is not something to remember.

CI-GATES := submodules values corpus unit documented grenadine mvnhttp readscaling vecscaling pipescaling chunkscaling printscaling complexity ioscaling hotscaling depssmoke taskssmoke depscpcache depsunit \
  smoke tracesmoke buildsmoke buildlibsmoke staticnativesmoke sci scifunctional cts ffi ffidupsym continuations stdlibfasl \
  transient rrbprop rrbscaling stateimage infer wp devirt fieldread numwp fieldnum fieldjoin contagion \
  hasheq \
  protoret pic narrow directlink unitcontext numeric oparity mathfl flarr \
  fnform coreproc traceemit traceeval degradedbacktrace \
  inline inline-body dcerefs shakelocal manifestcheck readmecheck portcheck adaptercheck hostprops statlayout lockcheck parkcheck shelloutcheck errnocheck irvalidate devbootsmoke \
  gatebootsmoke aotcachesmoke aotcachepathsmoke aotfingerprint compilepathsmoke makefilesmoke \
  systemstreams \
  certify gambitcheck gambitgencheck gambitseedcheck gambitboot grenadinecheck fibers gosm asynctimer interruptnest threadsafety
TEST-GATES := submodules selfhost ci

GATE-RECEIPT := target/gate-receipt

# Every tracked and every untracked-but-not-ignored file, so adding a source file
# invalidates the receipt as surely as editing one does. Hash each file's NAME and
# content, over a SORTED list: `git ls-files -c -o` groups untracked separately, so
# concatenating in its order made the hash change when a file merely went from
# untracked to tracked — committing, which changes nothing, read as "not gated".
# The submodule paths list as directories, which the per-file hash cannot read;
# their pinned SHAs come from submodule status instead, so a submodule bump
# invalidates the receipt too.
GATE-FINGERPRINT = { git ls-files -z -c -o --exclude-standard \
    | LC_ALL=C sort -z | xargs -0 $(GATE-SHA) 2>/dev/null; \
    git submodule status --recursive 2>/dev/null; } \
  | $(GATE-SHA) | cut -d' ' -f1
GATE-SHA = $$(command -v sha256sum >/dev/null 2>&1 && echo sha256sum || echo "shasum -a 256")

# -i ignores failures outright; -k runs on past them and leaves a later target's
# success as the last thing in the log. Both turn the gate into decoration. This
# has to refuse at PARSE time: -i ignores the failure of a recipe line, including
# a guard's own, so a guard inside the recipe is itself ignored and the gate runs.
# make groups SHORT flags into the first word of MAKEFLAGS with no leading dash
# ("ik"); long options stay separate words. Read the first word only when it is
# that group, or --no-print-directory matches a search for "i".
MAKE-SHORT-FLAGS := $(filter-out -%,$(firstword $(MAKEFLAGS)))
ifneq (,$(filter test ci gate-run-test gate-run-ci,$(MAKECMDGOALS)))
ifneq (,$(findstring i,$(MAKE-SHORT-FLAGS)))
$(error make -i ignores failures, so the gate cannot fail — drop -i)
endif
ifneq (,$(findstring k,$(MAKE-SHORT-FLAGS)))
$(error make -k runs past failures, so the log's last line is not the verdict — drop -k)
endif
endif

# $(1) = gate name, $(2) = target list.
# The `+` on the sub-make hands the jobserver down; without it the sub-make runs
# -j1 and a parallel gate silently serializes.
define run-gate
@rm -f '$(GATE-RECEIPT)'
+@if $(MAKE) --no-print-directory gate-run-$(1); then \
  mkdir -p target; \
  { echo "gate: $(1)"; \
    echo "tree: $$($(GATE-FINGERPRINT))"; \
    echo "head: $$(git rev-parse HEAD 2>/dev/null || echo none)"; \
    echo "targets: $(2)"; } > '$(GATE-RECEIPT)'; \
  echo "OK: $(1) gate passed ($(words $(2)) targets)"; \
else \
  st=$$?; \
  echo "FAILED: $(1) gate did not complete — targets after the failure never ran,"; \
  echo "        so nothing here is evidence the rest of the gate passes."; \
  exit $$st; \
fi
endef

# Full gate (dev machine). Includes the self-host byte-fixpoint, which only holds
# on the same Chez that minted the seed.
test:
	$(call run-gate,test,$(TEST-GATES))

# CI gate: behavior only. The checked-in seed is a minted artifact (like a
# lockfile) — it RUNS correctly on any Chez, but `selfhost` rebuilds it and a
# different Chez version may emit byte-different (gensym/order) output, so the
# byte-fixpoint is a dev-machine check, not a CI one (jolt-8479).
ci:
	$(call run-gate,ci,$(CI-GATES))

# The prerequisite-only targets the wrappers drive. Not meant to be run directly:
# they pass silently, which is the thing the wrappers exist to prevent.
gate-run-test: $(TEST-GATES)
gate-run-ci: $(CI-GATES)

# Is THIS working tree covered by a complete gate run? A subset run leaves the
# receipt absent (the wrapper clears it) and any edit since changes the tree hash.
gate-status:
	@if [ ! -f '$(GATE-RECEIPT)' ]; then \
	  echo "NOT GATED: no receipt — run 'make test'"; exit 1; fi
	@recorded=$$(sed -n 's/^tree: //p' '$(GATE-RECEIPT)'); \
	 current=$$($(GATE-FINGERPRINT)); \
	 if [ "$$recorded" != "$$current" ]; then \
	   echo "NOT GATED: tree changed since the last full gate run"; \
	   echo "  receipt: $$(sed -n 's/^gate: //p' '$(GATE-RECEIPT)') at $$(sed -n 's/^head: //p' '$(GATE-RECEIPT)')"; \
	   exit 1; \
	 fi; \
	 echo "GATED: $$(sed -n 's/^gate: //p' '$(GATE-RECEIPT)') gate passed on this exact tree"

# Self-host fixpoint: bootstrap.ss rebuild == checked-in seed.
selfhost:
	@sh host/chez/selfcheck.sh

# Value-model unit tests (nil/truthiness/collections on Chez).
values:
	@$(CHEZ) --script test/chez/values-test.ss

# The hash engine's VALUES, pinned to JVM Clojure. hasheq.ss gets tuned for speed
# (its 32-bit leaf helpers are macros so they inline), and a tuning pass that
# changes a hash VALUE rather than its cost fails nothing until a hash crosses the
# JVM boundary — so the goldens are literal here, plus a flat-vs-layered mixer
# sweep over every length and char class.
hasheq:
	@$(CHEZ) --script test/chez/hasheq-test.ss

# Fibers R1 (epic jolt-nvpr.2): the fiber primitive + single-carrier scheduler
# behind the CONTRACT.txt coroutines tier. Correctness (round trip, completion,
# per-fiber raise isolation, round-robin order, deep-stack yield) plus the
# pinned numbers (spawn < 5us, switch < 100ns, per-fiber live < 8KB by R0's
# corrected absolute-live-bytes measurement). Detection-free, no jolt boot.
# fibers-state-test.ss is the R2 dynamic-slice gate (per-fiber bindings/ns/txn;
# loads rt.ss for the real thread parameters).
# fibers-chan-test.ss is the R3 waiter-protocol gate (fiber <! / >! over the
# existing channel handlers; loads rt.ss for the real async.ss channels).
# fibers-go-test.ss is the R4 gate (epic jolt-nvpr.5): go on fibers via the
# *go-backend* opt-in (sections 1-3 through the compiler), and alts! as a wait
# set (sections 4-7 through the host seam; the :default check loads the
# overlay).
# fibers-lock-test.ss is the object-monitor gate (jolt-3a87): `locking` and the
# bare monitor-enter/monitor-exit halves across a fiber switch. A monitor is the
# one lock in the runtime that wraps user code, so neither half of locks.ss's
# premise — short regions, never spanning a park — holds for it.
# async-io-thread-test.ss is the io-thread gate (jolt-579): core.async's third
# carrier. It runs with the pool pinned to ONE carrier, which is what makes "8
# bodies parked at the same time, all of them resuming" mean that a fiber released
# its carrier rather than held it.
fibers:
	@$(CHEZ) --script test/chez/fibers-test.ss
	@$(CHEZ) --script test/chez/fibers-state-test.ss
	@$(CHEZ) --script test/chez/fibers-chan-test.ss
	@$(CHEZ) --script test/chez/fibers-go-test.ss
	@$(CHEZ) --script test/chez/fibers-pool-test.ss
	@$(CHEZ) --script test/chez/fibers-io-test.ss
	@$(CHEZ) --script test/chez/fibers-process-io-test.ss
	@$(CHEZ) --script test/chez/fibers-sm-test.ss
	@$(CHEZ) --script test/chez/fibers-preempt-test.ss
	@$(CHEZ) --script test/chez/fibers-lock-test.ss
	@$(CHEZ) --script test/chez/fibers-monitor-test.ss
	@$(CHEZ) --script test/chez/async-io-thread-test.ss

# The one (timeout ms) timer thread (jolt-pe84): a timeout closes on its own
# deadline however far away the pending ones are, and the thread is forked once.
# Both were broken by the same three lines — a sleep that left the timer off its
# condition variable dropped the wake for a nearer deadline, and a fork guard
# cleared before an idle wait forked a second immortal timer per call.
asynctimer:
	@$(CHEZ) --script test/chez/async-timer-test.ss

# A nested run-interruptible extent restores the enclosing polling timer after
# normal return, exception, or interruption, without sharing ownership between
# application threads.
interruptnest:
	@$(CHEZ) --script test/chez/interrupt-nesting-test.ss

# The dynamic-var binding stack (jolt-3bo): lookup cost against binding DEPTH and
# against the number of vars in one frame, push/pop throughput, and the two
# workloads the trade-off is judged on — N nested fn literals (deep) and a real
# namespace compile (wide and shallow). Opt-in, NOT part of make ci.
dynbench:
	@sh bench/dyn-binding/run.sh

# Fibers R6 (jolt-nvpr.7): the :thread vs :fiber benchmark harness. Opt-in and
# NOT part of the gate — benchmarks do not belong in CI. Runs each measurement
# phase as a subprocess and prints the comparison table; like aba.sh it is
# blind to dev mode (every phase runs the runtime from source).
fibersbench:
	@sh bench/fibers/run.sh

# R0-residue probes (jolt-nvpr.10): the size of a fiber with REAL frames, and
# memory under fiber churn. Opt-in like fibersbench, NOT part of make ci.
fibersresidue:
	@$(CHEZ) --script bench/fibers/residue.ss
	@$(CHEZ) --script bench/fibers/churn.ss

# Corpus conformance vs JVM-sourced expecteds (allowlist + floor).
corpus:
	@$(CHEZ) --script host/chez/run-corpus.ss

# Host-specific unit cases.
unit:
	@$(CHEZ) --script host/chez/run-unit.ss

# The jolt half of the known-divergences :documented gate: every entry's :check
# must render exactly its recorded :jolt value, its :jvm and :jolt must differ,
# and an entry with no :check fails. certify.clj runs the JVM half against
# reference Clojure; this half needs no JVM, so it lives in `ci`.
# `make documented-record` prints what jolt currently answers, for recording a
# new entry (the JVM side comes from
# `clojure -M test/conformance/certify.clj --record-documented`).
documented:
	@$(CHEZ) --script host/chez/run-documented.ss

documented-record:
	@$(CHEZ) --script host/chez/run-documented.ss --record

# Real-CLI smoke over bin/jolt.
# The CLI and build gates spawn a jolt process per case; a prebuilt binary boots
# ~10x faster than script mode (0.14s vs 1.5s) and builds an app ~5x faster, so
# they take this as a prerequisite. JOLT_BIN=bin/jolt forces script mode.
#
# Rebuilt only when something it bakes in is newer than the binary. It used to
# rebuild unconditionally, which is free under `make -j ci` (one shared node in
# the graph) but charged every single-gate run 18s — enough to make `make
# buildlibsmoke` slower with the prerequisite than without it. The staleness
# check covers the same inputs build-jolt.ss embeds: the runtime .ss files, the
# install roots, and the launcher stub. JOLT_FORCE_TESTBIN=1 rebuilds anyway.
TESTBIN-INPUTS := host/chez jolt-core stdlib vendor/fs/src vendor/process/src vendor/grenadine/src vendor/grenadine-generated vendor/irregex
testbin:
	@if [ -n "$${JOLT_FORCE_TESTBIN:-}" ] || [ ! -x target/release/jolt ] || \
	   [ -n "$$(find $(TESTBIN-INPUTS) -type f -newer target/release/jolt -print -quit 2>/dev/null)" ]; then \
	  $(CHEZ) --script host/chez/build-jolt.ss release target/release/jolt; \
	else \
	  echo "testbin: target/release/jolt up to date"; \
	fi

smoke: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/smoke.sh

# An escaping throw names the Clojure fn, file and line it came from — including
# when TCO erased the frame — and every exception class inherits the Throwable
# method surface.
tracesmoke: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/trace-smoke.sh

# The IR schema validator (JOLT_IR_VALIDATE) reports no problems on real code.
irvalidate:
	@sh host/chez/ir-validate-smoke.sh

# The build-driving gates take testbin for the same reason smoke and cts do,
# only more so: a `jolt build` costs ~2.5s through the prebuilt binary and
# ~12.5s through the source-mode driver, and buildsmoke alone drives 26 of them
# (343s -> 78s measured). Under `make -j ci` the one testbin build is shared
# with smoke/cts/aotcachesmoke. buildsmoke keeps an explicit bin/jolt build at
# the end so the source-mode driver stays gated; JOLT_BIN=bin/jolt forces the
# whole gate back to script mode.

# `jolt build` produces a working standalone binary.
buildsmoke: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/build-smoke.sh

# `jolt build --library` produces a shared object callable from C/C++/Rust.
buildlibsmoke: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/build-lib-smoke.sh

# `jolt build` cc-links a :jolt/native :static archive into the binary (the
# default), and --dynamic keeps the runtime load-shared-object path.
staticnativesmoke: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/static-native-smoke.sh

# Duplicate native symbol detection (issue #731): a declared :jolt/native that
# carries its own static copy of another's code — raygui linked against
# libraylib.a — used to go inert with no error. Pins that the footgun build is
# reported AND that a correctly linked one is not.
ffidupsym:
	@sh host/chez/ffi-duplicate-symbol-smoke.sh

# OPT-IN: jolt.mvn-http cert-verifying HTTPS fetch against Central + Clojars.
# Not in `make test` — needs network + a working system OpenSSL.
httpsfetch:
	@sh host/chez/https-fetch-smoke.sh

# OPT-IN: replay third-party libraries' own clojure.test suites and compare the
# tallies against test/conformance/libs/manifest.edn. Not in `make test` — needs
# the upstream library checkouts ($JOLT_CONFORMANCE_LIBS, default
# ../conformance-libraries), which are not vendored. Skips cleanly without them.
# `make libconformance LIBS="malli honeysql"` runs a subset.
# See test/conformance/libs/README.md.
libconformance: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" \
	 JOLT_NO_USER_DEPS=1 target/release/jolt run test/conformance/libs/run.clj $(LIBS)

# jolt.mvn-http pure-function tests (URL/redirect/header/body parsing). No
# network, no OpenSSL — runs in the default gate.
mvnhttp:
	@bin/jolt run test/mvn_http_test.clj

# Reading a source file form by form off a java.io reader must cost time LINEAR
# in the source. It was quadratic until v0.7.7 (37s to read clojure/core.clj,
# against the JVM's 0.06s) and nothing caught it, because the corpus rows about
# reading are all about the VALUES a read produces. Asserts the 1x-vs-4x ratio
# measured inside ONE process, so it judges the shape and not the machine.
# Takes the built binary: script mode would measure the same ratio far slower.
readscaling: testbin
	@JOLT_NO_USER_DEPS=1 target/release/jolt run test/read_scaling_test.clj

# (into vec vec) and subvec stay O(log n) through core — the raw pvec ops have
# rrbscaling; this catches core falling back to an element-by-element rebuild.
vecscaling: testbin
	@JOLT_NO_USER_DEPS=1 target/release/jolt run test/vec_scaling_test.clj

# A piped-stream write costs O(1), not O(chunks-queued): the jpipe buffer was a
# plain list re-copied per write, so piping N chunks cost O(N^2) cons work under
# the pipe mutex (the ring.util.io/piped-input-stream path). 1x-vs-4x in-process
# ratio, like readscaling.
pipescaling: testbin
	@JOLT_NO_USER_DEPS=1 target/release/jolt run test/pipe_scaling_test.clj

# chunk-append (the clojure.core chunk-builder API) costs O(1) amortized per
# item, not O(items-buffered) — filling a chunk-buffer was O(n^2) via a
# re-copied item list, and the cap argument was ignored.
chunkscaling: testbin
	@JOLT_NO_USER_DEPS=1 target/release/jolt run test/chunk_scaling_test.clj

# Printing a collection costs O(printed length): jolt-str-join was a right-fold
# of string-append (each element re-copied the whole joined suffix), on every
# pr-str/str/prn of any collection, record, or sorted coll.
printscaling: testbin
	@JOLT_NO_USER_DEPS=1 target/release/jolt run test/print_scaling_test.clj

# Operations that must not be linear in the collection's size: count/drop on a
# vector-backed seq, rseq, first on a sorted collection. Every one of these was
# O(n) here while the reference answers from the shape, and none of it is visible
# to a value test. Judged as a shape (n vs 4n), not an absolute time.
complexity: testbin
	@JOLT_NO_USER_DEPS=1 target/release/jolt run test/complexity_test.clj

# Draining *in* / with-in-str by lines or forms costs O(input), not
# O(input x items): the IReader buffer atoms re-copied (and read re-parsed) the
# whole remaining input per item until the [string offset] cursor rework.
ioscaling: testbin
	@JOLT_NO_USER_DEPS=1 target/release/jolt run test/io_scaling_test.clj

# The 2026-08 sweep's remaining hot-path shapes in one gate: split-with-limit,
# core.async timeout arming, ArrayDeque/StringTokenizer draining, ns-publics/
# refer var-table independence, set/intersection smaller-side walk.
hotscaling: testbin
	@JOLT_NO_USER_DEPS=1 target/release/jolt run test/hotpath_scaling_test.clj

# deps.edn alias + CLI semantics (tools.deps args-map keys, -X/-T/-Sdeps, the
# user deps.edn chain, jar/git coordinates) through the real CLI, over local
# fixture projects in test/chez/deps-alias/. Offline.
depssmoke: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/deps-alias-smoke.sh

# bb.edn / deps.edn :tasks through the real CLI: babashka task semantics
# (:depends, :init, :requires, :enter/:leave, :private, :extra-paths/:extra-deps,
# the babashka.tasks API), the `tasks` listing, and exit-code propagation.
# Offline fixture projects in test/chez/tasks/.
taskssmoke: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/tasks-smoke.sh

# The resolved-roots cache (.jolt/cpcache): a warm run reuses a project's final
# dependency resolution instead of re-expanding the graph. Offline throwaway
# project in a temp dir; gates the cache key, invalidation, and dev posture.
depscpcache: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/deps-cpcache-smoke.sh

# Shared Grenadine dependency-expansion integration tests: exclusions, version
# selection, orphan cutting, and the Maven version comparator, driven through
# a fake coordinate type. The cases are ported from tools.deps. Offline.
depsunit:
	@JOLT_NO_USER_DEPS=1 bin/jolt run test/deps_expand_test.clj

# Vendored Grenadine core plus Jolt's effective-POM adapter. Offline.
grenadine:
	@JOLT_NO_USER_DEPS=1 bin/jolt run test/grenadine_test.clj

# Build jolt as a self-contained native binary into target/<profile>/jolt. The
# binary bundles the runtime, compiler, jolt-core + stdlib source, the Chez boots,
# and a launcher stub, so it runs AND compiles jolt apps with no Chez or cc on the
# machine. Built on a dev/CI host that HAS Chez + cc. release = optimize-level 3,
# no inspector info, compressed; debug = optimize-level 0 + inspector + debug info.
# JOLT_CROSS_TARGET (optional) cross-compiles jolt for another Chez machine — it is
# passed as build-jolt.ss's 3rd arg and needs $JOLT_TARGET_PACK (empty = native).
jolt-release:
	@$(CHEZ) --script host/chez/build-jolt.ss release target/release/jolt $(JOLT_CROSS_TARGET)
jolt-debug:
	@$(CHEZ) --script host/chez/build-jolt.ss debug target/debug/jolt
# Re-mint the seed first so the embedded compiler image is current, then both builds.
jolt: selfhost jolt-release jolt-debug
	@echo "OK: target/release/jolt and target/debug/jolt built"

# Self-build smoke: the distributed jolt compiles an app with Chez + cc removed.
joltsmoke:
	@sh host/chez/jolt-selfbuild-smoke.sh

# The embedded per-namespace stdlib fasls (R1): assert that a built jolt serves
# clojure.test (and jolt.time) from the compiled fasl blob rather than
# recompiling from source, that the aot-info line fires (non-vacuous), and that a
# real deftest + LocalDate expression run through the binary. Depends on
# jolt-release having run.
stdlibfasl: testbin
	@sh host/chez/stdlib-fasl-smoke.sh

# SCI conformance: load borkdude/sci's source through jolt (floor-gated).
sci:
	@$(CHEZ) --script host/chez/run-sci.ss

# A complementary functional gate: load SCI through Jolt's ordinary dependency
# path, then initialize and reuse real contexts. run-sci.ss remains the broad,
# intentionally lenient source-loading compatibility gate.
scifunctional: testbin
	@JOLT_NO_USER_DEPS=1 target/release/jolt -Sdeps '{:deps {borkdude/sci {:local/root "vendor/sci"}}}' run test/chez/sci-functional-test.clj

# clojure-test-suite conformance: run the vendored jank-lang/clojure-test-suite
# per-namespace under jolt, gated on the per-namespace baseline
# (test/chez/cts-known-failures.txt).
cts: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" bash host/chez/cts.sh

# FFI: bind native functions (typed foreign-procedure), memory, and that a
# :blocking call is collect-safe (a parked thread doesn't pin the collector).
# The widths gate covers the exact scalar vocabulary across both halves of the
# API: runtime memory access and compiler-emitted procedures/callables, :bool
# included — the one type whose value, not just width, converts at the boundary.
# The layout gate compares declarative struct metadata and field access against
# C; the aggregate gate covers structs passed and returned by C value; the native
# error gate covers atomic errno/GetLastError capture and option composition.
# The arena gate covers the allocation-lifetime API (issue #799) and the rest of
# the babashka.ffi-compatible surface built on it — the four arena kinds and who
# closes each, arena-owned blocks/strings/callbacks/views, the pointer
# vocabulary, layout-shaped read and write, places, and the typed array moves.
ffi:
	@$(CHEZ) --script test/chez/ffi-binding-test.ss
	@sh test/chez/ffi-widths-test.sh "$(CHEZ)"
	@sh test/chez/ffi-layout-test.sh "$(CHEZ)"
	@sh test/chez/ffi-aggregate-test.sh "$(CHEZ)"
	@bin/jolt run test/chez/jolt-ffi-scoped-test.clj
	@bin/jolt run test/chez/jolt-ffi-arena-test.clj
	@sh test/chez/ffi-native-error-test.sh "$(CHEZ)"

# Escape continuations (jolt.continuations, issue #736): the one-shot contract
# call-cc/letcc expose, what unwinds on an escape, that a park inside ONE fiber
# is not an ownership boundary, and the four misuses. The cross-fiber rows are
# why this gate exists — unguarded, invoking an escape captured on another
# fiber hangs the process rather than raising, so each of those rows runs on a
# watchdog thread and FAILS on a deadline instead of wedging the run.
continuations:
	@bin/jolt run test/chez/continuations-test.clj

# Transients: mutable backing, snapshot on persistent!, and linear-time builds.
transient:
	@$(CHEZ) --script test/chez/transient-test.ss

# RRB vector: catvec/slice against a list model with structural invariants
# (seeded sequences, seed printed on failure), and the n-vs-4n complexity
# ratio measured in one process.
rrbprop:
	@$(CHEZ) --script test/chez/rrb-property-test.ss
rrbscaling:
	@$(CHEZ) --script test/chez/rrb-scaling-test.ss

# State images: value-graph round-trip through jolt.image, plus the Chez fasl
# behaviour the format assumes (machine-independence, what fasl refuses).
stateimage:
	@$(CHEZ) --script test/chez/state-image-test.ss

# Inference / success-type checking: drive jolt.passes.types directly and assert
# diagnostic counts + collected calls/escapes (the optimization pass the other
# gates don't exercise).
infer:
	@$(CHEZ) --script host/chez/run-infer.ss

# Whole-program param-type fixpoint: record types flowing across fn boundaries
# (a callee's param picks up its callers' ctor return types), the foundation the
# bare-index field reads + protocol devirtualization build on.
wp:
	@$(CHEZ) --script host/chez/run-wp.ss

# Protocol-call devirtualization: a monomorphic call resolves its impl by the
# inferred record tag (find-protocol-method) instead of routing through the
# protocol var; the result must match ordinary dispatch.
devirt:
	@$(CHEZ) --script host/chez/run-devirt.ss

# Fibers R7 (jolt-nvpr.9): the CPS pass over a go body. Asserts WHICH
# representation each park site got (on the expansion, and on the cheap-park vs
# capture counters), that the fallbacks still work, and that both backends give
# the same values.
gosm:
	@$(CHEZ) --script host/chez/run-gosm.ss

# The runtime's shared side-tables under concurrent access (jolt-3907). Scenario 1
# is a reproducer: with the hasheq caches shared instead of per-thread it faults
# inside the collector on most runs. test/chez/thread-tables.clj (smoke.sh) covers
# the same bug class through a core.async pipeline sweep.
threadsafety:
	@$(CHEZ) --script test/chez/thread-safety-test.ss

# Native record field reads: a keyword lookup on a statically-known record reads
# the field by its declared slot (jrec-field-at) instead of jolt-get; the value
# must match, and a non-field key / default-arg form keeps the generic path.
fieldread:
	@$(CHEZ) --script host/chez/run-fieldread.ss

# Inline method body field-read gate: when the optimize pipeline re-infers
# defrecord/deftype inline method bodies with the receiver typed, field reads
# must emit jrec-field-at (bare index) instead of jolt-get.
inline-body:
	@$(CHEZ) --script host/chez/run-inline-body.ss

# DCE reference collection (dce.ss): an app form's refs must union an IR walk
# (:var/:the-var nodes) with a text scan of the emitted Scheme, so a macro-spliced
# (var-deref "ns" "nm") with no :var node still roots its target. Pins both halves.
dcerefs:
	@$(CHEZ) --script host/chez/run-dce-refs.ss

# Hintless whole-program double inference: a fn whose every call site passes a
# flonum has its param typed :double by the closed-world fixpoint and unboxed to
# fl-ops with no ^double hint; an integer caller leaves it generic, an escaped fn
# keeps :any.
numwp:
	@$(CHEZ) --script host/chez/run-numwp.ss

# Mandelbrot count-point hot loop: whole-program fixpoint must seed cr/ci as
# :double from caller type, and the numeric pass must emit fl-ops with zero
# jolt-n* generic arith in the double arithmetic path.
mandelbrot-num:
	@$(CHEZ) --script host/chez/run-mandelbrot-num.ss

# Double record fields: a ^double-tagged field reads back as a flonum (coerced at
# construction and set!), so hintless arithmetic over those fields unboxes to fl-ops.
fieldnum:
	@$(CHEZ) --script host/chez/run-fieldnum.ss

# Whole-program record field-type inference: wp-infer! joins the ctor-argument types
# across every (->Ctor ...) site to derive each field's type — all-flonum -> :double
# (reads unbox, protocol-method return concretizes, caller accumulator goes fl+);
# record-or-nil -> nilable record (guarded reads narrow to the direct accessor);
# conflicting/escaping/mutable/map-> -> :any. Portable hint-free code now reaches the
# same emission the ^double hint reaches above.
fieldjoin:
	@$(CHEZ) --script host/chez/run-fieldjoin.ss

# Devirt-gated fl* contagion for :num record fields: a :num field read
# beside a proven :double operand contagion-coerces (exact->inexact) and lowers to
# fl* in a specialized clone resolved only at devirtualized call sites — recovering
# the win Option B gave up without touching the megamorphic/PIC regime. Pins the
# types contagion-specialize API (the invariant: contagion fires only beside a
# proven :double; a pure-:num body stays generic) and the runtime clone registry.
contagion:
	@$(CHEZ) --script host/chez/run-contagion.ss

# Protocol-method return inference: a method whose impls all return the same record
# type has a monomorphic return, so a (method recv ..) call types as that record and
# a field read off the result bare-indexes; a disagreeing impl keeps the generic path.
protoret:
	@$(CHEZ) --script host/chez/run-protoret.ss

# Protocol-dispatch polymorphic inline cache: a protocol call the inference tags
# :proto/:method but can't prove monomorphic emits a per-site cache keyed on the
# receiver's descriptor identity (eq? scan + a global epoch guard). Pins the
# emission, megamorphic correctness across record types, and that an extend-type at
# runtime invalidates the cache (the epoch bump) so the new impl is served.
pic:
	@$(CHEZ) --script host/chez/run-pic.ss

# Under tracing, the tail-frame ring save/restore goes around calls that can push a
# rib and nowhere else. A site lowering to inline Chez primitives (a proven aget is
# one flvector-ref) can never reach a fn prologue, and wrapping it let-binds the
# result across the restore — which re-boxes an unboxed flonum and cost 19x on an
# array loop. Gates both directions: no wrapper on the primitive branches, wrapper
# still present on the ones that really apply a fn.
traceemit:
	@$(CHEZ) --script host/chez/run-trace-emit.ss

# trace-r2: an eval-path (cache-miss) frame carries a source object, and its
# (source-name . offset) resolves to the original clj line via the eval marker
# registry; with tracing off the eval path registers nothing.
traceeval:
	@$(CHEZ) --script host/chez/run-traceeval.ss

# PSL R6: with introspection suppressed (sa-introspect-enabled? #f), a throw
# still surfaces type+message while the walker entry points return empty and
# the backtrace renders without continuation frames.
degradedbacktrace:
	@$(CHEZ) --script host/chez/run-degraded-backtrace.ss

# Nilable record types + flow-sensitive narrowing: a record-or-nil types as a nilable
# record (some?/nil? don't fold, so a runtime guard stays); inside (if (some? x) ..)
# the then-branch narrows x to non-nil, so its field reads bare-index and unbox.
narrow:
	@$(CHEZ) --script host/chez/run-narrow.ss

# Direct-linking emission: a closed-world build binds top-level app defs to jv$
# Scheme bindings and routes app->app calls/refs to them, skipping var-deref +
# jolt-invoke; ^:dynamic/^:redef and nested defs opt out.
directlink:
	@$(CHEZ) --script test/chez/directlink-test.ss

# Unique anon-fn letrec names + source-form registration (R1): a user-ns anon
# literal registers jfn$<ns>$<def>$<n> -> {form, ns, free-names} and the live
# closure's inspector name must agree; system-ns closures stay unregistered.
fnform:
	@$(CHEZ) --script test/chez/fnform-test.ss

# Every clojure.core fn must be nameable in value position: the state image
# writes a procedure as its var NAME, and a native that is set!-extended after
# its def-var! leaves a procedure nothing named — so values built from it stop
# being writable, silently. Swept from the var table, so a fn added later is
# covered without anyone remembering.
coreproc:
	@$(CHEZ) --script test/chez/core-proc-name-test.ss

# Compilation-unit context: the emit-session state (mode flags, direct-link
# registries, ctor shapes, gensym, cache cells) is per-unit, so two units are
# isolated (reentrant) and a flag set under one never leaks into another.
unitcontext:
	@$(CHEZ) --script test/chez/unit-context-test.ss

# Every numeric fast-path op at every arity it admits, derived from op-registry:
# the specialized form compiles, agrees with the generic path, and actually emits
# its specialization. op-registry names a proc per kind without saying what arity
# that proc takes, so this is what pins the two together.
oparity:
	@$(CHEZ) --script test/chez/op-arity-test.ss

# Hint-directed fast arithmetic: ^double/^long param hints (and float literals)
# lower arithmetic to Chez fl*/fx* ops; un-hinted integer code stays generic.
numeric:
	@$(CHEZ) --script test/chez/numeric-test.ss

# java.lang.Math over proven flonum operands lowers to the native Chez flonum op
# (flsqrt/flatan/…), result typed :double so flonum contagion holds; an untyped
# arg or an all-integer Math/abs stays the generic string-keyed host-static-call.
mathfl:
	@$(CHEZ) --script host/chez/run-mathfl.ss

# (aget ^doubles a i): a primitive-array param hint lowers aget to an unboxed
# flvector-ref (jolt-flaget) typed :double, so surrounding arithmetic unboxes to
# fl+; an untyped aget stays the native jolt-nth.
flarr:
	@$(CHEZ) --script host/chez/run-flarr.ss

# IR inlining: a small single-arity defn is spliced at call sites (under optimize
# + direct-link, closed-world guarantee), with ^double/^long entry/return
# coercions carried through via :coerce nodes.
inline:
	@$(CHEZ) --script test/chez/inline-test.ss

# Tree-shake soundness: build example apps (incl. deps.edn git-lib apps) default vs
# --tree-shake and require identical output. Slow (two builds per app); not in the
# default gate. Skips without the examples repo / Chez kernel dev files.
shakesmoke: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/tree-shake-smoke.sh

# The no-git-dep tree-shake correctness fixtures only (ns-publics/defonce/
# data-reader apps under test/chez) — build in seconds, no examples repo needed,
# so they run in `make test`/ci. The git-dep apps stay in the manual shakesmoke.
shakelocal: testbin
	@SHAKESMOKE_SCOPE=local JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/tree-shake-smoke.sh

# Runtime load-manifest drift guard: cli.ss (the live entry) and bootstrap.ss
# (the seed rebuilder's reduced set) hand-mirror build.ss's bld-runtime-manifest;
# this diffs them so a load added to one but not the other fails the gate.
manifestcheck:
	@sh host/chez/manifest-check.sh

# README truncation guard. Tools that feed the README to a model cut it at a byte
# budget (15000 is a common default) with no marker, so anything past the cut is
# not "further down the page", it is absent. When "Differences from Clojure" sat
# at the 19KB mark a reader saw the java.* shims, never saw what they are not,
# and reasoned about JVM daemon threads for the rest of a task. So the section
# has to END inside the budget, not begin near it. 12000 leaves room for a 12KB
# cut and for the section to grow.
README-DIFF-CEILING := 12000
readmecheck:
	@end=$$(LC_ALL=C awk '/^## Differences from Clojure$$/ {seen=1; next} \
	                      seen && /^## / {print off; found=1; exit} \
	                      {off += length($$0) + 1} \
	                      END {if (!found) print (seen ? off : -1)}' README.md); \
	 if [ "$$end" -lt 0 ]; then \
	   echo "readmecheck: README.md has no '## Differences from Clojure' section" >&2; exit 1; fi; \
	 if [ "$$end" -gt $(README-DIFF-CEILING) ]; then \
	   echo "readmecheck: 'Differences from Clojure' ends at byte $$end, past the $(README-DIFF-CEILING) ceiling." >&2; \
	   echo "  A truncating fetch would cut it. Move it earlier or shorten what precedes it;" >&2; \
	   echo "  contributor-facing prose belongs in CONTRIBUTING.md." >&2; exit 1; fi; \
	 for f in CONTRIBUTING.md llms.txt; do \
	   [ -f "$$f" ] || { echo "readmecheck: $$f is missing (README links to it)" >&2; exit 1; }; \
	 done; \
	 echo "readmecheck: divergences end at byte $$end (ceiling $(README-DIFF-CEILING))"

# PSL R1 portability lint gate: fails when a blocklisted Chez-only identifier
# appears in a host file that is not allowlisted for it (and on stale allowlist
# lines, and on any line naming a non-target-owned file). Allowlist seeded from
# current reality; the R10 end state is the two target-owned files.
portcheck:
	@sh host/chez/portability-check.sh

census:
	@sh host/chez/portability-check.sh --census

# PSL R2 adapter contract gate: assert every CONTRACT.txt name is bound in
# (chezscheme). The chez adapter defines nothing — this only fails when the
# contract file lists a name Chez does not provide.
adaptercheck:
	@$(CHEZ) --script host/scheme-adapter/chez.ss

# The three derived host properties (sa-os-family / sa-arch / sa-endian) are all
# host logic may ask about the platform, so one wrong row is a wrong SIGCHLD,
# LC_TIME and struct-stat offset at once. The row that broke in #796 — a
# portable-bytecode tag, which names no OS — is only reachable from a host we do
# not build on, so the table is pinned per tag rather than per running machine.
hostprops:
	@$(CHEZ) --script test/chez/host-derived-props-test.ss

# The other half of the same rule: knowing the platform is only useful if the
# struct stat offsets it selects are the ones this machine actually uses. The
# gate measures the layout with no identity to go on — the pb case — and then
# reads a file whose mode it just set, which no offset that merely happens to
# carry S_IFDIR would answer correctly.
statlayout:
	@$(CHEZ) --script test/chez/stat-layout-test.ss

# Every lock in the runtime must route through jolt's wrapper, because
# preemption is refused while one is held and that only works if the runtime can
# tell. A wrapper nobody is obliged to use decays into the hand-marked scheme it
# replaced, which is how the previous mechanism kept missing regions. The
# allowlist records today's unmigrated sites and must only ever shrink.
lockcheck:
	@sh host/chez/lock-check.sh

# The other half of the same rule: a fiber never leaves the CPU while its carrier
# holds a counted lock. lockcheck above proves the runtime can TELL that a lock is
# held; this one proves nothing parks while one is. It reads every host .ss as data,
# closes "can park" over the call graph, and fails on a call to anything in that
# closure from inside a jolt-with-mutex region — which is what the previous lexical
# scan could not see, because the park was one call past the region (jolt-04ee). It
# also fails if a switch point stops calling the runtime assertion, so the two
# halves cannot be removed independently.
parkcheck:
	@sh host/chez/park-lock-check.sh

# jolt.host/sh is Chez's `system`, which is cmd.exe on Windows: `mkdir -p a/b`
# there creates a directory named `-p`, and mv/rm/touch/test/find are not
# commands at all. So the resolver does its filesystem work through filesystem
# calls, and the shell is left for git and unzip, which are real programs. The
# two spellings look alike in the source, so the rule is checked rather than
# remembered.
shelloutcheck:
	@sh host/chez/shellout-check.sh

# errno survives only until the next thing that can set it, and reading it is
# itself a foreign call — so a syscall wrapper must capture it once, at the
# syscall, and branch on the value. See the script for what asking twice cost.
errnocheck:
	@sh host/chez/errno-check.sh

# Makefile dependency selection: explicit Chez overrides must bypass local
# Makes provisioning so release jobs retain their chosen compiler and libc.
makefilesmoke:
	@bash test/makefile-smoke.sh

# JVM oracle: certify the corpus against reference Clojure. Skips if clojure absent.
certify:
	@if command -v clojure >/dev/null 2>&1; then \
		clojure -M test/conformance/certify.clj --self-test && \
		clojure -M test/conformance/certify.clj; \
	else \
		echo "certify: clojure not on PATH — skipped"; \
	fi

# Gambit adapter gate (G1, jolt-mj95.2): loads host/gambit/{prelude-shims,
# scheme-adapter-runtime,hasheq}.ss under native gsi and asserts the
# CONTRACT.txt names + shim behavior. Detection-gated like certify — skips
# cleanly when gambit-scheme is absent. NEVER bare gsc/gsi (gsc on PATH is
# Ghostscript): always the brew-prefix binary.
GAMBIT_GSI := $(shell brew --prefix gambit-scheme 2>/dev/null)/bin/gsi
GAMBIT_GSC := $(shell brew --prefix gambit-scheme 2>/dev/null)/bin/gsc

# host/gambit/records-gambit.ss is GENERATED from the four host/chez records
# files — records.ss, records-coll.ss, protocols.ss, records-dispatch.ss — (the
# define-jrec-family phase wall — see gen-records.ss). Regenerate after every
# change to any of them.
gambitgen:
	$(CHEZ) --script host/gambit/gen-records.ss

# ...and this is what makes that an invariant rather than a note: regenerate into
# a temp file and diff. A records-file change that never reached the generated
# file fails here instead of silently leaving the Gambit host a release behind.
# Runs on Chez alone, so it gates in CI whether or not gambit is installed.
# Grenadine ships four namespaces it generates rather than commits, so half the
# source tree comes from the submodule and half from vendor/grenadine-generated
# (see the README there). Bumping one without refreshing the other mixes two
# Grenadine versions, which breaks far from the cause — so it is checked, not
# documented and hoped for.
# Compares the COMMIT, not the tag: a submodule checkout does not necessarily
# carry tags — CI's does not — so `git describe` fails there and a tag
# comparison would only ever pass on a developer's machine. That is exactly the
# environment-dependent gate that reads as green where it is never really
# checked, so the recorded VERSION carries the sha and this reads that.
grenadinecheck:
	@pinned=$$(git -C vendor/grenadine rev-parse HEAD 2>/dev/null); \
	  vendored=$$(sed -n 's/^[^#].* //p' vendor/grenadine-generated/VERSION 2>/dev/null | tr -d '[:space:]'); \
	  tag=$$(sed -n 's/^\([^# ][^ ]*\) .*/\1/p' vendor/grenadine-generated/VERSION 2>/dev/null); \
	  if [ -z "$$pinned" ]; then \
	    echo "grenadinecheck: vendor/grenadine is not checked out — run 'git submodule update --init'" >&2; \
	    exit 1; \
	  elif [ "$$pinned" = "$$vendored" ]; then \
	    echo "grenadinecheck: generated sources match the pinned submodule ($$tag $$(echo $$pinned | cut -c1-7))"; \
	  else \
	    echo "grenadinecheck: vendor/grenadine is at $$pinned but vendor/grenadine-generated records $$vendored ($$tag)" >&2; \
	    echo "  refresh the generated sources — see vendor/grenadine-generated/README.md" >&2; \
	    exit 1; \
	  fi

gambitgencheck:
	@out=$$(mktemp -d)/records-gambit.ss; \
	  GEN_RECORDS_OUT="$$out" $(CHEZ) --script host/gambit/gen-records.ss >/dev/null; \
	  if diff -q "$$out" host/gambit/records-gambit.ss >/dev/null; then \
	    echo "gambitgencheck: records-gambit.ss is current with the records files"; \
	  else \
	    echo "gambitgencheck: host/gambit/records-gambit.ss is STALE — run 'make gambitgen'" >&2; \
	    diff -u host/gambit/records-gambit.ss "$$out" | head -40 >&2; \
	    rm -f "$$out"; exit 1; \
	  fi; \
	  rm -f "$$out"

# Gambit seed-mint gate: re-mint the gambit seed into a temp dir on Chez and
# require it to match host/gambit/seed/ byte-for-byte. Fails when gen-seed.ss
# itself cannot run (a chez-only construct reached the emission) or when the
# chez seed was re-minted without `make gambitseed`. Runs on Chez alone, so it
# gates in CI whether or not gambit is installed.
gambitseedcheck:
	@out=$$(mktemp -d); \
	  if ! GEN_SEED_OUT_DIR="$$out" $(CHEZ) --script host/gambit/gen-seed.ss > "$$out/mint.log" 2>&1; then \
	    tail -20 "$$out/mint.log" >&2; \
	    echo "gambitseedcheck: gen-seed.ss FAILED — the gambit seed cannot be minted from current sources" >&2; \
	    rm -rf "$$out"; exit 1; \
	  fi; \
	  if diff -q "$$out/prelude.ss" host/gambit/seed/prelude.ss >/dev/null \
	      && diff -q "$$out/image.ss" host/gambit/seed/image.ss >/dev/null; then \
	    echo "gambitseedcheck: gambit seed is current with the sources"; \
	    rm -rf "$$out"; \
	  else \
	    echo "gambitseedcheck: host/gambit/seed is STALE — run 'make gambitseed'" >&2; \
	    rm -rf "$$out"; exit 1; \
	  fi

gambitcheck:
	@if [ -x "$(GAMBIT_GSI)" ]; then \
		"$(GAMBIT_GSI)" host/gambit/gambitcheck.ss; \
	else \
		echo "gambitcheck: gambit-scheme not installed (brew) — skipped"; \
	fi

# G2 kernel-test gate (jolt-mj95.4): the full booted manifest on native gsi,
# driven through the real natives. Same detection-gated shape as gambitcheck;
# NOT in the ci list. Run from the repo root (boot's irregex load is cwd-relative).
gambitkernel:
	@if [ -x "$(GAMBIT_GSI)" ]; then \
		"$(GAMBIT_GSI)" host/gambit/kernel-test.ss; \
	else \
		echo "gambitkernel: gambit-scheme not installed (brew) — skipped"; \
	fi

# G3 eval gate: real jolt source through jolt-compile-eval on the booted
# manifest + cross-minted seed, renders pinned to Chez captures. Detection-
# gated like gambitcheck and NOT in the ci list — it boots the full seed, so
# it takes about a minute on gsi. Run from the repo root.
gambiteval:
	@if [ -x "$(GAMBIT_GSI)" ]; then \
		"$(GAMBIT_GSI)" host/gambit/eval-test.ss; \
	else \
		echo "gambiteval: gambit-scheme not installed (brew) — skipped"; \
	fi

# Build profiles: generate the reduced repl profile and check that the language
# still works while an excluded feature reports itself instead of failing as an
# unbound name. Cheap (a gsi load, no js compile), so it can gate the mechanism.
gambitprofile:
	@if [ -x "$(GAMBIT_GSI)" ]; then \
		$(CHEZ) --script host/gambit/gen-boot.ss repl; \
		"$(GAMBIT_GSI)" host/gambit/profile-test.ss; \
	else \
		echo "gambitprofile: gambit-scheme not installed (brew) — skipped"; \
	fi

# The full-profile boot comes up on gsi: the js and gsi targets share the boot,
# and NOTHING else runs its top level end to end — the shipped web REPL hung at
# boot for a week of commits (unbound jolt-with-mutex at the first keyword
# intern) while every other gambit gate stayed green. Skipped, like the other
# gambit gates, when gambit-scheme is absent.
gambitboot:
	@if [ -x "$(GAMBIT_GSI)" ]; then \
	  $(CHEZ) --script host/gambit/gen-boot.ss full; \
	  out=$$(cd host/gambit && "$(GAMBIT_GSI)" boot-probe.scm < /dev/null 2>&1); \
	  case "$$out" in \
	    *BOOT-OK*) echo "gambitboot: full-profile boot OK on gsi";; \
	    *) printf '%s\n' "$$out" | tail -20; echo "gambitboot: FAILED — full-profile boot did not reach BOOT-OK" >&2; exit 1;; \
	  esac; \
	else \
	  echo "gambitboot: gambit-scheme not installed (brew) — skipped"; \
	fi

# The browser bundle: the whole stack (kernel + seed + compiler + a queue-polling
# REPL loop) compiled to one JavaScript file by the Gambit backend. ~30s, ~32MB
# raw / ~4MB gzipped, which is what a web server ships. Point GAMBIT_WEB_OUT at
# a site checkout to refresh the live demo:
#   make gambitweb GAMBIT_WEB_OUT=../jolt-lang.github.io/resources/static/js/jolt-web.js
# NEVER bare gsc — that is Ghostscript on a brew machine.
# PROFILE selects how much of the language the bundle carries (host/gambit/
# profiles.ss lists them: full, repl, kernel). gen-boot.ss writes the boot for it
# on Chez — Gambit resolves ##include at expansion time, so this cannot be a
# runtime switch — and binds every name an excluded group owned to a raise that
# names the group.
PROFILE ?= full
GAMBIT_WEB_OUT ?= target/gambit/jolt-web.js
gambitweb:
	@if [ -x "$(GAMBIT_GSC)" ]; then \
		$(CHEZ) --script host/gambit/gen-boot.ss $(PROFILE); \
		mkdir -p "$$(dirname "$(GAMBIT_WEB_OUT)")"; \
		out="$$(cd "$$(dirname "$(GAMBIT_WEB_OUT)")" && pwd)/$$(basename "$(GAMBIT_WEB_OUT)")"; \
		(cd host/gambit && "$(GAMBIT_GSC)" -target js -exe -o "$$out" repl-main.ss); \
		echo "gambitweb: $(GAMBIT_WEB_OUT) ($$(wc -c < "$$out" | tr -d ' ') bytes,\
 $$(gzip -c "$$out" | wc -c | tr -d ' ') gzipped)"; \
	else \
		echo "gambitweb: gambit-scheme not installed (brew) — skipped"; \
	fi

# G3 compiler-on-gsi (jolt-mj95.4): cross-mint the Gambit seed from the Chez
# seed ON CHEZ with the backend target at :gambit (R9), grep-verifying the
# emission contains no chez-only spelling before writing host/gambit/seed/.
# Runs on CHEZ (gambitseed), not gsi.
gambitseed:
	$(CHEZ) --script host/gambit/gen-seed.ss

# Re-mint the seed after changing a seed source (reader/analyzer/backend/core).
remint:
	@sh host/chez/remint.sh

# Precompile the runtime to target/dev/flat.so so dev bin/jolt boots ~10x faster
# (loads the .so instead of compiling ~50 .ss files from source every invocation).
devboot: submodules
	@$(CHEZ) --script host/chez/make-devboot.ss

# Precompile the gate boot preamble to target/dev/gate.so so a pass gate boots in
# ~0.2s instead of ~1.5s (it spends nearly all of that loading the same six
# runtime files from Chez source). Opt-in like devboot: gate-boot.ss uses the
# image when it is present and newer than every input, and loads from source
# otherwise, so nothing depends on this target and CI is unaffected. Worth it
# when iterating on one pass gate; `make ci` runs them in parallel anyway.
gateboot: submodules
	@$(CHEZ) --script host/chez/make-gateboot.ss

# Smoke test: the gate boot image's staleness predicate. Drives
# gate-boot-image-fresh? over synthetic input lists, so it boots no runtime,
# touches nothing in the repo, and is safe under parallel make.
# devbootsmoke is a prerequisite for ORDERING, not because this needs anything it
# builds: it touches host/chez/rt.ss and seed/prelude.ss to test its own cache
# invalidation, and both are inputs to the gate boot image whose freshness this
# asserts. Run concurrently under `make -j` the touch lands between this smoke's
# freshness probe and the gate run it guards, and the gate correctly falls back to
# source — failing a check about something else entirely. The two conflict by
# construction, so they are sequenced rather than raced.
gatebootsmoke: gateboot devbootsmoke
	@sh test/chez/gateboot-smoke.sh

# Smoke test: the dev boot cache is used when fresh and invalidated correctly.
# MAKEFLAGS cleared: the script re-invokes make itself, and inheriting the
# jobserver it cannot claim only produces a warning.
devbootsmoke: devboot
	@MAKEFLAGS= sh test/chez/devboot-smoke.sh

# Smoke test: the per-namespace AOT/compile cache (miss/hit/invalidate, edge
# cases, bypass semantics). Drives dev bin/jolt; no Maven jars required. The
# built binary is a second, genuinely different runtime — case (k) needs it to
# check that two runtimes sharing a version string still key separately.
aotcachesmoke: testbin
	@sh test/chez/aot-cache-smoke.sh

# System/in, System/out, System/err: the process streams and the classes the JVM
# reports for them. Needs a real pipe on stdin, so it is a script rather than a
# corpus row.
systemstreams:
	@sh test/chez/system-streams-smoke.sh

# Smoke test: clojure.core/compile writes artifacts under *compile-path* and a
# later PROCESS loads them — including with the source removed, which is the point
# of compiling. Needs the built binary; each phase is its own jolt run.
compilepathsmoke: testbin
	@sh test/chez/compile-path-smoke.sh

# The content hash under the cache, and the two runtime fingerprints built on it
# (source-tree and baked). Covers what the smoke test can't reach without a full
# jolt rebuild: that a ONE-CHARACTER, length-preserving edit moves the namespace
# key, the source-tree fingerprint, and a built binary's baked fingerprint.
# equal-hash saw ~26 bytes of a source and served stale fasls for everything else.
aotfingerprint:
	@$(CHEZ) --script test/chez/aot-fingerprint-test.ss

# A frame from a CACHED namespace must report a source path that EXISTS. The cache
# used to compile a pid-unique temp and rename it away, so compile-file baked a
# path that died with the rename — which the R3 trace would then fail to resolve
# offsets against.
aotcachepathsmoke: testbin
	@sh test/chez/aot-cache-path-smoke.sh

# Perf measurement: cold (recompile) vs warm (cache hit) for a multi-library
# require. Needs Maven jars locally; NOT in the default ci gate (timing budget).
aotcacheperf:
	@sh test/chez/aot-cache-perf.sh

# Perf probe for the print and pr value seams: 200000 values each through
# with-out-str. Guards the per-value *print-readably* override and the printer's
# fast path for runtime-owned types. Manual like aotcacheperf — the repo has no
# wall-clock assertions in its default gates, and a timing floor would be flaky
# on loaded CI. See the header of the script for how to read the numbers.
printperf:
	@$(CHEZ) --script test/chez/print-throughput.ss

# StringBuilder.append must stay amortised O(1). Asserts a SCALING RATIO rather
# than a wall-clock floor — 4x the appends should cost ~4x, not ~16x — so unlike
# the probes above it is meaningful on a loaded machine. Still manual, to keep
# the default gate free of timing. See the script header.
sbperf:
	@$(CHEZ) --script test/chez/string-builder-perf.ss
