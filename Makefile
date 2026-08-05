# jolt — Clojure on Chez Scheme. Single substrate, no Janet.
#
# bin/jolt runs jolt directly off the checked-in seed (host/chez/seed/).
# `make build` builds the standalone binary, `make test` is the full gate, and
# `make remint` rebuilds the seed after a source change.

R := https://github.com/makeplus/makes
M := .cache/makes
# PINNED. Unpinned, this clone is whatever makeplus/makes HEAD is on the day of
# the build — including a release build, whose toolchain would then be decided by
# a repository this one records no version of. Bump deliberately.
V := d14cb578c2f04e6f9d00e01c7c4e416a9baf94e9
$(shell [ -d '$M' ] || git clone -q $R '$M')
# Fetch only when the pin is absent, so a normal build stays offline.
$(shell git -C '$M' cat-file -e '$V^{commit}' 2>/dev/null || git -C '$M' fetch -q origin)
$(shell git -C '$M' rev-parse -q --verify HEAD 2>/dev/null | grep -qx '$V' || git -C '$M' checkout -q '$V')

include $M/init.mk

# An explicit caller-selected Chez is authoritative. This preserves CI/release
# toolchains whose threading, libc floor, and native libraries are intentional.
JOLT-CHEZ := $(or $(CHEZ),$(CHEZSCHEME))
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
endif

JOLT-TARGETS-NEEDING-DEPS := \
  aotcacheperf aotcachesmoke aotfingerprint buildlibsmoke buildsmoke \
  aotcachepathsmoke compilepathsmoke contagion corpus cts dcerefs depssmoke depsunit devboot \
  devbootsmoke devirt directlink ffi fieldjoin fieldnum fieldread flarr grenadine \
  gateboot gatebootsmoke httpsfetch infer inline inline-body irvalidate \
  jolt jolt-debug jolt-release joltsmoke libconformance mandelbrot-num mathfl mvnhttp \
  narrow numeric numwp oparity pic protoret printperf remint sci selfhost shakelocal \
  traceemit \
  shakesmoke smoke staticnativesmoke stateimage test testbin transient unit unitcontext \
  values wp ci

# Only mark PHONY targets for names that have file system conflicts:
.PHONY: build install test ci gate-run-test gate-run-ci gate-status

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

CI-GATES := submodules values corpus unit grenadine mvnhttp depssmoke depsunit \
  smoke tracesmoke buildsmoke buildlibsmoke staticnativesmoke sci cts ffi \
  transient stateimage infer wp devirt fieldread numwp fieldnum fieldjoin contagion \
  protoret pic narrow directlink unitcontext numeric oparity mathfl flarr \
  traceemit traceeval \
  inline inline-body dcerefs shakelocal manifestcheck irvalidate devbootsmoke \
  gatebootsmoke aotcachesmoke aotcachepathsmoke aotfingerprint compilepathsmoke makefilesmoke \
  certify
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

# Corpus conformance vs JVM-sourced expecteds (allowlist + floor).
corpus:
	@$(CHEZ) --script host/chez/run-corpus.ss

# Host-specific unit cases.
unit:
	@$(CHEZ) --script host/chez/run-unit.ss

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
TESTBIN-INPUTS := host/chez jolt-core stdlib vendor/fs/src vendor/process/src vendor/grenadine/src vendor/irregex
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

# deps.edn alias + CLI semantics (tools.deps args-map keys, -X/-T/-Sdeps, the
# user deps.edn chain, jar/git coordinates) through the real CLI, over local
# fixture projects in test/chez/deps-alias/. Offline.
depssmoke: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" sh host/chez/deps-alias-smoke.sh

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

# SCI conformance: load borkdude/sci's source through jolt (floor-gated).
sci:
	@$(CHEZ) --script host/chez/run-sci.ss

# clojure-test-suite conformance: run the vendored jank-lang/clojure-test-suite
# per-namespace under jolt, gated on the per-namespace baseline
# (test/chez/cts-known-failures.txt).
cts: testbin
	@JOLT_BIN="$${JOLT_BIN:-target/release/jolt}" bash host/chez/cts.sh

# FFI: bind native functions (typed foreign-procedure), memory, and that a
# :blocking call is collect-safe (a parked thread doesn't pin the collector).
ffi:
	@$(CHEZ) --script test/chez/ffi-binding-test.ss

# Transients: mutable backing, snapshot on persistent!, and linear-time builds.
transient:
	@$(CHEZ) --script test/chez/transient-test.ss

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

# Makefile dependency selection: explicit Chez overrides must bypass local
# Makes provisioning so release jobs retain their chosen compiler and libc.
makefilesmoke:
	@bash test/makefile-smoke.sh

# JVM oracle: certify the corpus against reference Clojure. Skips if clojure absent.
certify:
	@if command -v clojure >/dev/null 2>&1; then \
		clojure -M test/conformance/certify.clj; \
	else \
		echo "certify: clojure not on PATH — skipped"; \
	fi

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
