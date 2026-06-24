# jolt — Clojure on Chez Scheme. Single substrate, no Janet.
#
# bin/joltc runs jolt directly off the checked-in seed (host/chez/seed/); there is no
# build step. `make test` is the full gate. `make remint` rebuilds the seed after a
# source change.

.PHONY: test ci values corpus unit smoke buildsmoke selfhost sci certify ffi transient infer directlink numeric inline shakesmoke remint

# Full gate (dev machine). Includes the self-host byte-fixpoint, which only holds
# on the same Chez that minted the seed.
test: selfhost ci
	@echo "OK: all gates passed"

# CI gate: behavior only. The checked-in seed is a minted artifact (like a
# lockfile) — it RUNS correctly on any Chez, but `selfhost` rebuilds it and a
# different Chez version may emit byte-different (gensym/order) output, so the
# byte-fixpoint is a dev-machine check, not a CI one (jolt-8479).
ci: values corpus unit smoke buildsmoke sci ffi transient infer directlink numeric inline certify
	@echo "OK: CI gates passed"

# Self-host fixpoint: bootstrap.ss rebuild == checked-in seed.
selfhost:
	@sh host/chez/selfcheck.sh

# Value-model unit tests (nil/truthiness/collections on Chez).
values:
	@chez --script test/chez/values-test.ss

# Corpus conformance vs JVM-sourced expecteds (allowlist + floor).
corpus:
	@chez --script host/chez/run-corpus.ss

# Host-specific unit cases.
unit:
	@chez --script host/chez/run-unit.ss

# Real-CLI smoke over bin/joltc.
smoke:
	@sh host/chez/smoke.sh

# `jolt build` produces a working standalone binary.
buildsmoke:
	@sh host/chez/build-smoke.sh

# SCI conformance: load borkdude/sci's source through joltc (floor-gated).
sci:
	@chez --script host/chez/run-sci.ss

# FFI: bind native functions (typed foreign-procedure), memory, and that a
# :blocking call is collect-safe (a parked thread doesn't pin the collector).
ffi:
	@chez --script test/chez/ffi-binding-test.ss

# Transients: mutable backing, snapshot on persistent!, and linear-time builds.
transient:
	@chez --script test/chez/transient-test.ss

# Inference / success-type checking: drive jolt.passes.types directly and assert
# diagnostic counts + collected calls/escapes (the optimization pass the other
# gates don't exercise).
infer:
	@chez --script host/chez/run-infer.ss

# Direct-linking emission: a closed-world build binds top-level app defs to jv$
# Scheme bindings and routes app->app calls/refs to them, skipping var-deref +
# jolt-invoke; ^:dynamic/^:redef and nested defs opt out.
directlink:
	@chez --script test/chez/directlink-test.ss

# Hint-directed fast arithmetic: ^double/^long param hints (and float literals)
# lower arithmetic to Chez fl*/fx* ops; un-hinted integer code stays generic.
numeric:
	@chez --script test/chez/numeric-test.ss

# IR inlining: a small single-arity defn is spliced at call sites (under optimize),
# with ^double/^long entry/return coercions carried through via :coerce nodes.
inline:
	@chez --script test/chez/inline-test.ss

# Tree-shake soundness: build example apps (incl. deps.edn git-lib apps) default vs
# --tree-shake and require identical output. Slow (two builds per app); not in the
# default gate. Skips without the examples repo / Chez kernel dev files.
shakesmoke:
	@sh host/chez/tree-shake-smoke.sh

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
