#!/bin/sh
# examples-smoke.sh — build every app in jolt-lang/examples with the jolt under
# test, and run the test task of those that have one.
#
#   ci/examples-smoke.sh <examples-checkout> <jolt-binary>
#
# BUILD is the point of this gate: `jolt build` is the compiler's whole pipeline
# (analyze, tree-shake, direct-link, cc-link) over real third-party code, which is
# more surface than the test suites here reach. So every app is built, including the
# GUI ones whose windows can't open on a headless runner — a build needs no display.
# Tests run only where the app has a test task AND is headless- and network-safe.
#
# The manifest is explicit rather than derived from each deps.edn: the main
# namespace, the test entrypoint and the headless/network constraints differ per app
# in ways no convention captures (:tasks vs :aliases, app.core vs rt.render). It is
# cross-checked against the checkout in both directions, so an example added
# upstream without a row here fails the gate rather than silently skipping it.
#
# Fields: dir | build-main | test-task ("-" for build-only) | os (optional:
# only build on this uname, lowercased — todomvc-uikit's glimmer-uikit backend
# creates its CoreFoundation foreign-procedures at load, so even the BUILD
# needs macOS until the library defers them; see jolt-lang/glimmer-uikit)
#
# Build-only, and why:
#   http-client-app  -main hits real HTTPS endpoints — a release must not depend
#                    on third-party uptime.
#   fps-demo, glimmer-app, glimmer-gl-app, glimmer-tui-example,
#   image-dump-example   GUI/GL/TUI; no headless test task. (image-dump-example's save/load path is
#                    covered by the jolt-side stateimage gate.)
#   todomvc-uikit    darwin-only, build-only: AppKit.
#   hiccup/malli/markdown-app, ray-tracer*  no test task at all.
#   ffi-examples     its three programs call libffi and PortAudio, both declared
#                    `:optional true` because neither is on every runner, and it
#                    has no `test` task — `check` only reports what is present.
#                    Building app.check still compiles all three.
set -eu

root="${1:?usage: examples-smoke.sh <examples-checkout> <jolt-binary>}"
jolt="${2:?need the jolt binary to test}"
case "$jolt" in
  /*) ;;
   *) jolt="$(cd "$(dirname "$jolt")" && pwd)/$(basename "$jolt")" ;;
esac
[ -x "$jolt" ] || { echo "examples-smoke: $jolt is not executable"; exit 2; }
# A task body is a shell command, and the ones here spell the binary `jolt`
# (`:tasks {test "jolt -M:test"}`), so it has to be on PATH under that name.
# Prepending the binary's OWN directory is not enough and is actively wrong in
# CI: the release job unpacks ./jolt-bin next to the repo checkout, which is a
# directory called `jolt`, so `jolt` resolved to the directory. dash reports
# that as "jolt: Permission denied" and stops rather than continuing along PATH
# the way macOS's sh does, which is why it only ever failed on the Linux runner.
# A dedicated bin dir holding one symlink named `jolt` has no such ambiguity.
binshim="$(mktemp -d)"
ln -s "$jolt" "$binshim/jolt"
PATH="$binshim:$PATH"
export PATH
root="$(cd "$root" && pwd)"

manifest="$(mktemp)"
trap 'rm -f "$manifest"; rm -rf "$binshim"' EXIT
cat > "$manifest" <<'EOF'
basic-example    app.core       test
commonmark-app   app.core       test
nrepl-example    app.core       test
ring-app         app.core       test
glimmer-datastar app.core       test
reactive-dashboard app.core     test
hiccup-app       app.core       -
malli-app        app.core       -
markdown-app     app.core       -
http-client-app  app.core       -
ray-tracer       ray-baseline   -
ray-tracer-multi rt.render      -
fps-demo         fps-demo.core  -
glimmer-app      app.core       -
glimmer-gl-app   gl-demo.core   -
glimmer-tui-example tui-demo.core -
todomvc-uikit    app.core       -   darwin
image-dump-example app.core     -
ffi-examples     app.check      -
EOF

fails=0
note_fail() { echo "  FAIL: $*"; fails=$((fails + 1)); }

# Both directions: a row naming no directory, and a directory with no row. The
# second is the one that matters — an example added upstream must not slip past.
while read -r name main test os; do
  [ -n "${name:-}" ] || continue
  [ -d "$root/$name" ] || note_fail "$name is in the manifest but not in the checkout"
done < "$manifest"
for d in "$root"/*/; do
  name="$(basename "$d")"
  [ -f "$d/deps.edn" ] || continue
  awk -v n="$name" '$1 == n { found = 1 } END { exit !found }' "$manifest" \
    || note_fail "$name is in the checkout but has no ci/examples-smoke.sh row"
done
[ "$fails" -eq 0 ] || { echo "examples-smoke: manifest is out of sync with the checkout"; exit 1; }

host_os="$(uname -s | tr '[:upper:]' '[:lower:]')"
while read -r name main test os; do
  [ -n "${name:-}" ] || continue
  if [ -n "${os:-}" ] && [ "$os" != "$host_os" ]; then
    echo "examples-smoke: $name — skipped ($os-only, host is $host_os)"
    continue
  fi
  dir="$root/$name"

  # Build into a throwaway dir: a failed build leaves nothing behind and the
  # checkout stays clean for the test run that follows.
  tmp="$(mktemp -d)"
  echo "examples-smoke: $name — build -m $main"
  if ( cd "$dir" && JOLT_PWD="$(pwd)" "$jolt" build -m "$main" -o "$tmp/$name" ) \
     && [ -x "$tmp/$name" ]; then
    :
  else
    note_fail "$name build"
  fi
  rm -rf "$tmp"

  if [ "$test" != "-" ]; then
    echo "examples-smoke: $name — $test"
    ( cd "$dir" && JOLT_PWD="$(pwd)" "$jolt" "$test" ) || note_fail "$name $test"
  fi
done < "$manifest"

if [ "$fails" -ne 0 ]; then
  echo "examples-smoke: $fails FAILED"
  exit 1
fi
echo "examples-smoke: every example built; every test task passed"
