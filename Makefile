# jolt — Clojure on Chez Scheme. Single substrate, no Janet.
#
# bin/joltc runs jolt directly off the checked-in seed (host/chez/seed/); there is no
# build step. `make test` is the full gate. `make remint` rebuilds the seed after a
# source change.

.PHONY: test values corpus unit smoke selfhost certify remint

# Full gate. Each step exits non-zero on failure, failing the target.
test: selfhost values corpus unit smoke certify
	@echo "OK: all gates passed"

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
