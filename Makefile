# jolt — Clojure on Chez Scheme. Single substrate, no Janet.
#
# bin/joltc runs jolt directly off the checked-in seed (host/chez/seed/); there is no
# build step. `make test` is the full gate. `make remint` rebuilds the seed after a
# source change.

.PHONY: test ci values corpus unit smoke selfhost sci certify ffi transient remint

# Full gate (dev machine). Includes the self-host byte-fixpoint, which only holds
# on the same Chez that minted the seed.
test: selfhost ci
	@echo "OK: all gates passed"

# CI gate: behavior only. The checked-in seed is a minted artifact (like a
# lockfile) — it RUNS correctly on any Chez, but `selfhost` rebuilds it and a
# different Chez version may emit byte-different (gensym/order) output, so the
# byte-fixpoint is a dev-machine check, not a CI one (jolt-8479).
ci: values corpus unit smoke sci ffi transient certify
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

# SCI conformance: load borkdude/sci's source through joltc (floor-gated).
sci:
	@chez --script host/chez/run-sci.ss

# FFI + threading: HTTP server GC-safety (blocking calls deactivate the thread)
# and http-client temp-file uniqueness, plus a live request.
ffi:
	@chez --script test/chez/ffi-server-test.ss

# Transients: mutable backing, snapshot on persistent!, and linear-time builds.
transient:
	@chez --script test/chez/transient-test.ss

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
