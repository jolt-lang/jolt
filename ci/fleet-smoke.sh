#!/bin/sh
# fleet-smoke.sh — run one jolt-lang library's or example's own test suite against
# a given jolt binary. Called once per matrix row by the `libraries` / `examples`
# jobs in release.yml; also runnable by hand:
#
#   ci/fleet-smoke.sh <dir> <jolt-binary> [test-command...]
#
# The library repos each carry the SAME entrypoint — `:tasks {test "jolt -M:test"}`
# plus a `:test` alias — and their own CI runs `JOLT_PWD="$PWD" jolt -M:test`. This
# reproduces that exactly, with the binary under test on PATH so a task that shells
# out to `jolt` reaches it too (a :tasks string is a shell command).
#
# Why a script rather than inline `run:` steps: the same command has to work from a
# release matrix row AND from a developer's shell when a library goes red, and
# JOLT_PWD / PATH / the exit-code handling are easy to get subtly different between
# the two. `make fleetsmoke DIR=… ` is not offered on purpose — the fleet lives in
# other repos, so there is nothing for the Makefile to depend on.
set -eu

dir="${1:?usage: fleet-smoke.sh <dir> <jolt-binary> [test-command...]}"
jolt="${2:?need the jolt binary to test}"
shift 2

# Absolute, because we cd into the project: a :tasks entry that shells out to
# `jolt` resolves it through PATH, and PATH entries must not be relative.
case "$jolt" in
  /*) ;;
   *) jolt="$(cd "$(dirname "$jolt")" && pwd)/$(basename "$jolt")" ;;
esac
[ -x "$jolt" ] || { echo "fleet-smoke: $jolt is not executable"; exit 2; }
# A shim directory holding only a `jolt` symlink. Putting the binary's own
# directory on PATH is not enough: in the release workspace the binary is
# `jolt-bin`, so `jolt` missed it and found the checkout DIRECTORY named jolt,
# which dash reports as "jolt: Permission denied".
binshim="$(mktemp -d)"
trap 'rm -rf "$binshim"' EXIT
ln -s "$jolt" "$binshim/jolt"
PATH="$binshim:$PATH"
export PATH

cd "$dir"
# Every library reads JOLT_PWD to locate its own test fixtures (their CI sets it).
JOLT_PWD="$(pwd)"
export JOLT_PWD

if [ "$#" -gt 0 ]; then
  set -- "$@"
else
  set -- "$jolt" -M:test
fi

echo "fleet-smoke: $(basename "$dir") — $*"
"$@"
