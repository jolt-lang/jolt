#!/bin/sh
# FileChannel.lock and tryLock take POSIX record locks, so a lock one process
# holds keeps another process out, as the JDK's do. The unit rows cover what one
# process sees (overlaps, release, close); this needs two processes, so it is a
# script.
#
#   - an exclusive lock held by one process makes another's tryLock answer nil
#     over the same bytes, and grants it bytes outside the range
#   - lock() in a second process waits until the holder lets go, and the holder
#     exiting is letting go
#   - a shared lock admits another shared lock and keeps an exclusive one out
#   - release() hands the bytes over while the holder keeps running
#
# Runs the CLI through the given chez (default `chez`). Windows has no fcntl, so
# the script skips there.
#
#   sh test/chez/file-lock-smoke.sh [chez]
set -u

case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*) echo "SKIP: file-lock smoke (no POSIX record locks on Windows)"; exit 0 ;;
esac

chez="${1:-chez}"
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
dir="$(mktemp -d)"
pids=""
cleanup() {
  for p in $pids; do kill "$p" 2>/dev/null; done
  rm -rf "$dir"
}
trap cleanup EXIT

jolt() { "$chez" --script host/chez/cli.ss -e "$1"; }

fails=0
expect() {
  if [ "$2" = "$3" ]; then
    echo "PASS: $1"
  else
    echo "FAIL: $1"; echo "  got:  $(printf '%s' "$2" | tr '\n' '|')"; echo "  want: $(printf '%s' "$3" | tr '\n' '|')"
    fails=$((fails+1))
  fi
}
# wait-for FILE SECONDS: 0 once FILE exists, 1 if it never appears
wait_for() {
  n=0
  while [ ! -e "$1" ]; do
    n=$((n+1)); [ "$n" -gt $(($2 * 10)) ] && return 1
    sleep 0.1
  done
  return 0
}

lockfile="$dir/the.lock"
pre='(import (quote (java.nio.channels FileChannel)) (quote (java.nio.file Paths StandardOpenOption OpenOption)))
(defn ch [] (FileChannel/open (Paths/get "'"$lockfile"'" (make-array String 0)) (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/READ StandardOpenOption/WRITE])))
(defn await-file [f] (loop [n 0] (when (and (< n 1500) (not (.exists (java.io.File. f)))) (Thread/sleep 20) (recur (inc n)))))
(defn ok [l] (if l (do (.release l) true) l))'

# A holder: takes the lock, says so in a marker file, then waits for a go file.
# MODE release: release the lock, say so, and wait for a second go file before
# exiting. Otherwise it exits still holding the lock.
holder() { # name position size shared mode
  jolt "$pre
(let [c (ch) l (.lock c $2 $3 $4)]
  (spit \"$dir/$1.held\" \"\")
  (await-file \"$dir/$1.go\")
  (when (= \"$5\" \"release\")
    (.release l)
    (spit \"$dir/$1.released\" \"\")
    (await-file \"$dir/$1.bye\")))" >"$dir/$1.out" 2>&1 &
  pids="$pids $!"
}

# --- an exclusive lock keeps another process out of its bytes -----------------
holder ex 0 100 false exit
if wait_for "$dir/ex.held" 60; then
  got="$(jolt "$pre (let [c (ch)] (prn [(.tryLock c) (.tryLock c 0 1 true) (.tryLock c 99 5 false) (ok (.tryLock c 100 10 false))]))" 2>&1)"
  expect "tryLock over an exclusively held range answers nil, outside it a lock" "$got" "[nil nil nil true]"

  # lock() waits while the holder has the bytes, and gets them once it exits.
  # The waiter says it has booted just before it calls lock(), so the check
  # below measures the wait and not the start-up, which takes several seconds.
  jolt "$pre (let [c (ch)] (spit \"$dir/waiter.started\" \"\") (let [l (.lock c)] (spit \"$dir/waiter.got\" (str (.isValid l)))))" >"$dir/waiter.out" 2>&1 &
  waiter=$!; pids="$pids $waiter"
  if wait_for "$dir/waiter.started" 60; then
    sleep 2
    if [ -e "$dir/waiter.got" ]; then got=acquired; else got=waiting; fi
  else
    got="never started: $(cat "$dir/waiter.out")"
  fi
  expect "lock() waits while another process holds the range" "$got" "waiting"
  touch "$dir/ex.go"
  if wait_for "$dir/waiter.got" 60; then got="$(cat "$dir/waiter.got")"; else got="timed out: $(cat "$dir/waiter.out")"; fi
  expect "lock() is granted once the holding process exits" "$got" "true"
  wait "$waiter" 2>/dev/null
else
  expect "exclusive holder took its lock" "$(cat "$dir/ex.out")" ""
fi

# --- a shared lock admits shared and refuses exclusive -------------------------
holder sh 0 100 true release
if wait_for "$dir/sh.held" 60; then
  got="$(jolt "$pre (let [c (ch)] (prn [(ok (.tryLock c 0 10 true)) (.tryLock c 0 10 false) (.tryLock c)]))" 2>&1)"
  expect "a shared lock admits another process's shared lock, not its exclusive one" "$got" "[true nil nil]"
  touch "$dir/sh.go"
  if wait_for "$dir/sh.released" 60; then
    got="$(jolt "$pre (let [c (ch)] (prn (ok (.tryLock c))))" 2>&1)"
    expect "release() lets another process lock while the holder still runs" "$got" "true"
  else
    expect "shared holder released" "$(cat "$dir/sh.out")" ""
  fi
  touch "$dir/sh.bye"
else
  expect "shared holder took its lock" "$(cat "$dir/sh.out")" ""
fi

wait 2>/dev/null
if [ "$fails" -ne 0 ]; then
  echo "file-lock smoke: $fails failed"
  exit 1
fi
echo "file-lock smoke: all passed"
