#!/bin/sh
# The AOT cache's worker leaves nothing behind. A built jolt hands a cache
# miss's compile to a copy of itself (loader.ss aot-worker-start!) that outlives
# the run, finishes the jobs and then removes its manifest, log and locks. A
# worker that died before cleaning up, or one ended with its run, left them in
# the cache for good.
#
#   1. a cold-cache run: once the worker is done, every miss has its .so and no
#      aot-jobs-* file is left
#   2. (POSIX) ^C to the run's process group mid-run: the worker, in a group of
#      its own, still finishes and cleans up
#   3. what a killed worker left is swept by the next run that starts one
#
# JOLT_BIN is the jolt under test (a built binary: source mode has no worker).
jolt="${JOLT_BIN:-target/release/jolt}"
case "$(uname -s)" in MINGW*|MSYS*|CYGWIN*) windows=1 ;; *) windows= ;; esac

fails=0
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/proj/src/mylib"
for i in 1 2 3 4 5 6; do
  printf '(ns mylib.n%s)\n(defn f [] %s)\n' "$i" "$i" > "$tmp/proj/src/mylib/n$i.clj"
done
# The runtime is handed native paths on Windows.
native() { if [ -n "$windows" ]; then cygpath -m "$1"; else printf '%s' "$1"; fi; }
proj="$(native "$tmp/proj")"

run() {   # $1 cache dir, $2 forms
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$(native "$1")" "$jolt" -e "
    (require 'jolt.deps)
    (jolt.deps/add-deps {:deps {'mylib/mylib {:local/root \"$proj\"}}})
    $2"
}
leftovers() { find "$1" -name 'aot-jobs-*' 2>/dev/null; }
sos() { find "$1" -name 'mylib.n*.so' 2>/dev/null | wc -l | tr -d ' '; }
# Wait up to 120s for the worker to finish: no manifest left.
await_clean() {
  i=0
  while [ -n "$(find "$1" -name 'aot-jobs-*.edn' 2>/dev/null)" ] && [ $i -lt 240 ]; do
    sleep 0.5; i=$((i+1))
  done
}
verdict() {   # $1 label, $2 cache dir, $3 wanted .so count
  await_clean "$2"
  sleep 1
  left="$(leftovers "$2")"
  n="$(sos "$2")"
  if [ -z "$left" ] && [ "$n" -ge "$3" ]; then
    echo "PASS: $1 ($n .so, nothing left)"
  else
    echo "FAIL: $1 ($n .so, want >= $3; left: $(echo $left | tr '\n' ' '))"
    for f in $(find "$2" -name 'aot-jobs-*.log'); do echo "  $f:"; sed 's/^/    /' "$f"; done
    fails=$((fails+1))
  fi
}

# --- 1. cold cache ------------------------------------------------------------
c1="$tmp/c1"; mkdir -p "$c1"
# the run counts its own manifest, so a pass says a worker really ran
out="$(run "$c1" "(require 'mylib.n1 'mylib.n2 'mylib.n3)
  (println :sum (+ (mylib.n1/f) (mylib.n2/f) (mylib.n3/f))
           :manifests (count (filter #(re-find #\"aot-jobs-.*[.]edn\$\" (.getName %))
                                     (file-seq (java.io.File. \"$(native "$c1")\")))))" 2>&1 | tr -d '\r')"
case "$out" in *":sum 6 :manifests 1"*) ;; *) echo "FAIL: cold run printed: $out"; fails=$((fails+1)) ;; esac
verdict "a cold-cache run's worker cleans up" "$c1" 3

# --- 2. ^C to the run's process group (POSIX) --------------------------------
if [ -z "$windows" ]; then
  c2="$tmp/c2"; mkdir -p "$c2"
  set -m
  run "$c2" "(require 'mylib.n1 'mylib.n2 'mylib.n3 'mylib.n4) (println :loaded) (flush) (Thread/sleep 30000)" >"$tmp/c2.out" 2>&1 &
  pid=$!
  set +m
  i=0
  until grep -q loaded "$tmp/c2.out" 2>/dev/null || [ $i -ge 600 ]; do sleep 0.1; i=$((i+1)); done
  kill -INT -"$pid" 2>/dev/null
  wait "$pid" 2>/dev/null
  verdict "a worker outlives a ^C to its run and cleans up" "$c2" 4
fi

# --- 3. a killed worker's files are swept by the next run --------------------
c3="$tmp/c3"; mkdir -p "$c3"
run "$c3" "(require 'mylib.n1) (println :warm)" >/dev/null 2>&1
await_clean "$c3"
v1="$(dirname "$(find "$c3" -name 'mylib.n1*.so' | head -1)")"
# what a worker killed mid-job leaves: a manifest naming a dead parent and
# worker, its log, its locks (held by nobody) and a partial fasl
dead=999990
printf '(worker %s)\n("%s/x.scm" "%s/x.so")\n' "$dead" "$(native "$v1")" "$(native "$v1")" > "$v1/aot-jobs-$dead-old-1.edn"
: > "$v1/aot-jobs-$dead-old-1.edn.log"
: > "$v1/aot-jobs-$dead-old-1.edn.plock"
: > "$v1/aot-jobs-$dead-old-1.edn.wlock"
: > "$v1/x.so.part$dead"
run "$c3" "(require 'mylib.n5) (println :n5 (mylib.n5/f))" >/dev/null 2>&1
verdict "the next run sweeps what a killed worker left" "$c3" 2
if [ -e "$v1/x.so.part$dead" ]; then
  echo "FAIL: the killed worker's partial fasl is still there"; fails=$((fails+1))
fi

if [ "$fails" -eq 0 ]; then echo "aot-worker-cleanup smoke: passed"; else echo "aot-worker-cleanup smoke: $fails failed"; exit 1; fi
