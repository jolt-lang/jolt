#!/bin/sh
# OPT-IN network smoke for jolt.mvn-http: fetch a small real artifact over
# cert-verifying HTTPS from both Maven Central and Clojars, and assert the body
# landed as a non-empty file with the expected content. NOT in `make test` — it
# needs network + a working system OpenSSL. Run with: make httpsfetch
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
JOLTC="${JOLTC:-bin/joltc}"

fails=0

# fetch URL -> tmp; assert the file is non-empty and its first line looks like a
# Maven pom (<?xml ...> or <project ...>). $1 = label, $2 = url.
check_pom () {
  label="$1"; url="$2"; tmp="${TMPDIR:-/tmp}/jolt-http-$$"
  rm -f "$tmp"
  got="$($JOLTC -e "(require '[jolt.mvn-http]) (if (jolt.mvn-http/fetch \"$url\" \"$tmp\") :ok :fail)" 2>&1 | tail -1)"
  if [ "$got" != ":ok" ]; then
    echo "https-fetch: FAIL $label — fetch returned $got"
    fails=$((fails + 1)); rm -f "$tmp"; return
  fi
  if [ ! -s "$tmp" ]; then
    echo "https-fetch: FAIL $label — empty body"
    fails=$((fails + 1)); rm -f "$tmp"; return
  fi
  if ! head -c 200 "$tmp" | grep -Eq '<\?xml|<project'; then
    echo "https-fetch: FAIL $label — body is not a pom (first bytes:)"
    head -c 60 "$tmp"; echo
    fails=$((fails + 1)); rm -f "$tmp"; return
  fi
  echo "https-fetch: PASS $label ($(wc -c < "$tmp" | tr -d ' ') bytes)"
  rm -f "$tmp"
}

# Central: a pom is small and plaintext, ideal for a content check.
check_pom "central" "https://repo1.maven.org/maven2/org/clojure/math.combinatorics/0.2.0/math.combinatorics-0.2.0.pom"
# Clojars-hosted artifact (math.combinatorics is Central-only; clj-http is on Clojars).
check_pom "clojars" "https://repo.clojars.org/clj-http/clj-http/3.12.3/clj-http-3.12.3.pom"

if [ "$fails" -ne 0 ]; then
  echo "https-fetch: FAILED — $fails check(s) failed"
  exit 1
fi
echo "https-fetch: passed"
