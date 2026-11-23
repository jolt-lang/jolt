#!/bin/sh
# FileChannel.lock and tryLock take record locks (fcntl on POSIX, LockFileEx on
# Windows), so a lock one process
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
# Runs the CLI through the given chez (default `chez`), or JOLT_BIN when it is
# set (a built jolt; on Windows, jolt.exe from MSYS2's sh, where the locks are
# LockFileEx's).
#
#   sh test/chez/file-lock-smoke.sh [chez]
#   JOLT_BIN=target/release/jolt sh test/chez/file-lock-smoke.sh
#
# Nothing here waits on a clock to decide a verdict: each step waits for a
# marker file the other process writes, and a holder holds until the script
# tells it to let go, however slowly anything starts.
set -u

case "$(uname -s)" in MINGW*|MSYS*|CYGWIN*) windows=1 ;; *) windows= ;; esac

chez="${1:-chez}"
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
tmp="$(mktemp -d)"
pids=""
cleanup() {
  for p in $pids; do kill "$p" 2>/dev/null; done
  rm -rf "$tmp"
}
trap cleanup EXIT
# The runtime is handed native paths on Windows.
native() { if [ -n "$windows" ]; then cygpath -m "$1"; else printf '%s' "$1"; fi; }
dir="$(native "$tmp")"

if [ -n "${JOLT_BIN:-}" ]; then
  jolt() { "$JOLT_BIN" -e "$1"; }
else
  jolt() { "$chez" --script host/chez/cli.ss -e "$1"; }
fi

fails=0
expect() {
  if [ "$2" = "$3" ]; then
    echo "PASS: $1"
  else
    echo "FAIL: $1"; echo "  got:  $(printf '%s' "$2" | tr '\n' '|')"; echo "  want: $(printf '%s' "$3" | tr '\n' '|')"
    [ -s "$tmp/stderr" ] && sed 's/^/  stderr: /' "$tmp/stderr"
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
(defn await-file [f] (loop [] (when-not (.exists (java.io.File. f)) (Thread/sleep 20) (recur))))
(defn ok [l] (if l (do (.release l) true) l))'

# A holder: takes the lock, says so in a marker file, then waits for a go file,
# with no limit: it lets go only when the script says so, and the trap kills it
# if the script ends first. A holder that gave up on its own would release the
# lock early on a slow runner and make the waiting rows pass or fail by timing.
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
    (await-file \"$dir/$1.bye\")))" >"$tmp/$1.out" 2>&1 &
  pids="$pids $!"
}

# --- one process: the open's checks and DELETE_ON_CLOSE -----------------------
# The unit rows hold these on POSIX; this is where a Windows run sees them too.
got="$(jolt "$pre (let [f \"$dir/doc.tmp\" o (fn [& xs] (into-array OpenOption xs)) p (Paths/get f (make-array String 0))] (spit f \"x\") (let [c (FileChannel/open p (o StandardOpenOption/WRITE StandardOpenOption/DELETE_ON_CLOSE)) sz (.size c)] (.close c) (prn [sz (.exists (java.io.File. f)) (try (FileChannel/open p (o StandardOpenOption/APPEND StandardOpenOption/TRUNCATE_EXISTING)) (catch IllegalArgumentException e (.getMessage e))) (.getName (class (ch)))])))" 2>"$tmp/stderr")"
expect "DELETE_ON_CLOSE removes the file at the close; option checks come before the open" "$got" '[1 false "APPEND + TRUNCATE_EXISTING not allowed" "sun.nio.ch.FileChannelImpl"]'

# --- an exclusive lock keeps another process out of its bytes -----------------
holder ex 0 100 false exit
if wait_for "$tmp/ex.held" 120; then
  got="$(jolt "$pre (let [c (ch)] (prn [(.tryLock c) (.tryLock c 0 1 true) (.tryLock c 99 5 false) (ok (.tryLock c 100 10 false))]))" 2>"$tmp/stderr")"
  expect "tryLock over an exclusively held range answers nil, outside it a lock" "$got" "[nil nil nil true]"

  # lock() waits while the holder has the bytes, and gets them once it exits.
  # The waiter says it has booted just before it calls lock(), so the check
  # below measures the wait and not the start-up. The holder cannot let go
  # before ex.go, so a waiter that holds the lock within the window is a real
  # failure, never a slow start.
  jolt "$pre (let [c (ch)] (spit \"$dir/waiter.started\" \"\") (let [l (.lock c)] (spit \"$dir/waiter.got\" (str (.isValid l)))))" >"$tmp/waiter.out" 2>&1 &
  waiter=$!; pids="$pids $waiter"
  if wait_for "$tmp/waiter.started" 120; then
    sleep 2
    if [ -e "$tmp/waiter.got" ]; then got=acquired; else got=waiting; fi
  else
    got="never started: $(cat "$tmp/waiter.out")"
  fi
  expect "lock() waits while another process holds the range" "$got" "waiting"
  touch "$tmp/ex.go"
  if wait_for "$tmp/waiter.got" 120; then got="$(cat "$tmp/waiter.got")"; else got="timed out: $(cat "$tmp/waiter.out")"; fi
  expect "lock() is granted once the holding process exits" "$got" "true"
  wait "$waiter" 2>/dev/null
else
  expect "exclusive holder took its lock" "$(cat "$tmp/ex.out")" ""
fi

# --- a shared lock admits shared and refuses exclusive -------------------------
holder sh 0 100 true release
if wait_for "$tmp/sh.held" 120; then
  got="$(jolt "$pre (let [c (ch)] (prn [(ok (.tryLock c 0 10 true)) (.tryLock c 0 10 false) (.tryLock c)]))" 2>"$tmp/stderr")"
  expect "a shared lock admits another process's shared lock, not its exclusive one" "$got" "[true nil nil]"
  touch "$tmp/sh.go"
  if wait_for "$tmp/sh.released" 120; then
    got="$(jolt "$pre (let [c (ch)] (prn (ok (.tryLock c))))" 2>"$tmp/stderr")"
    expect "release() lets another process lock while the holder still runs" "$got" "true"
  else
    expect "shared holder released" "$(cat "$tmp/sh.out")" ""
  fi
  touch "$tmp/sh.bye"
else
  expect "shared holder took its lock" "$(cat "$tmp/sh.out")" ""
fi

# --- lock() waits in the kernel: interrupt and close end it, a fiber parks ----
# The waits below are against a holder in another process. The short sleeps
# only make it likely the waiter has reached its kernel wait before the
# interrupt or close lands; the answer is the same either way, and no verdict
# reads a clock. A wait that interrupt or close failed to end shows as :hung
# after a minute rather than hanging the script.
holder wt 0 100 false release
if wait_for "$tmp/wt.held" 120; then
  got="$(jolt "$pre (require (quote [jolt.fibers :as fb]))
(let [r1 (let [c (ch) res (promise)
               t (Thread. (fn [] (deliver res (try (.lock c) :granted (catch java.nio.channels.FileLockInterruptionException _ [:interrupted (.isInterrupted (Thread/currentThread))])))))]
           (.start t) (Thread/sleep 200) (.interrupt t) [(deref res 60000 :hung) (.isOpen c)])
      r2 (let [c (ch) res (promise)]
           (.start (Thread. (fn [] (deliver res (try (.lock c) :granted (catch java.nio.channels.AsynchronousCloseException _ :async-close))))))
           (Thread/sleep 200) (.close c) (deref res 60000 :hung))
      _ (fb/set-carrier-count! 1)
      r3 (let [c (ch) th (promise)
               f (fb/spawn (fn [] (deliver th (Thread/currentThread))
                             (try (.lock c) :granted (catch java.nio.channels.FileLockInterruptionException _ [:interrupted (.isInterrupted (Thread/currentThread))]))))]
           (Thread/sleep 200) (.interrupt @th) [(fb/join f 60000 :hung) (.isOpen c)])
      c (ch)
      a (fb/spawn (fn [] (.isValid (.lock c))))
      b (fb/spawn (fn [] (dotimes [i 5] (Thread/sleep 10)) :ran))
      rb (fb/join b 60000 :pinned)]
  (spit \"$dir/wt.go\" \"\")
  (prn [r1 r2 r3 rb (fb/join a 60000 :hung)]))" 2>"$tmp/stderr")"
  # one carrier: fiber b can only finish while fiber a's lock() is parked, and
  # the holder lets go only after b has finished
  expect "lock() waiting in the kernel: interrupt closes and throws (thread and fiber), close throws, a fiber parks" "$got" "[[[:interrupted true] false] :async-close [[:interrupted true] false] :ran true]"
  touch "$tmp/wt.bye"
else
  expect "wait-test holder took its lock" "$(cat "$tmp/wt.out")" ""
fi

# --- a waiter is not starved by a holder that releases and re-locks ----------
# The holder releases and takes the range again in a tight loop until the
# waiter says it got it, up to a count of cycles, not a time. A waiter in the
# kernel is woken by the first release (a JVM waiter needed at most a few
# hundred cycles on macOS and one or two on Linux, jolt's one); one that polled
# could only land in the instant between a release and the next lock, and took
# 74,000 cycles or never got it at all in 100,000. The bound is 10,000.
jolt "$pre
(let [c (ch) l (atom (.lock c))]
  (spit \"$dir/cy.held\" \"\")
  (await-file \"$dir/cy.started\")
  (Thread/sleep 300)
  (let [n (loop [n 0]
            (if (or (.exists (java.io.File. \"$dir/cy.got\")) (>= n 10000))
              n
              (do (.release @l) (reset! l (.lock c)) (recur (inc n)))))]
    (spit \"$dir/cy.cycles\" (str n))))" >"$tmp/cy.out" 2>&1 &
pids="$pids $!"
if wait_for "$tmp/cy.held" 120; then
  jolt "$pre (let [c (ch)] (spit \"$dir/cy.started\" \"\") (.lock c) (spit \"$dir/cy.got\" \"\"))" >"$tmp/cyw.out" 2>&1 &
  pids="$pids $!"
  if wait_for "$tmp/cy.cycles" 300; then
    n="$(cat "$tmp/cy.cycles")"
    if [ -e "$tmp/cy.got" ] && [ "$n" -lt 10000 ]; then got=granted; else got="starved after $n cycles"; fi
  else
    got="holder never finished: $(cat "$tmp/cy.out")"
  fi
  expect "a waiter gets the range from a holder that releases and re-locks in a loop" "$got" "granted"
else
  expect "cycling holder took its lock" "$(cat "$tmp/cy.out")" ""
fi

# Every verdict is in. A holder that never got as far as its marker (a failed
# row above) would wait for ever, so release the rest and end what is left.
touch "$tmp/ex.go" "$tmp/sh.go" "$tmp/sh.bye" "$tmp/wt.go" "$tmp/wt.bye" "$tmp/cy.got"
for p in $pids; do kill "$p" 2>/dev/null; done
wait 2>/dev/null
if [ "$fails" -ne 0 ]; then
  echo "file-lock smoke: $fails failed"
  exit 1
fi
echo "file-lock smoke: all passed"
