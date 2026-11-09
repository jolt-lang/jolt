#!/bin/sh
# exit-wait-smoke.sh — the process ends when the program's main returns only
# once every live non-daemon thread has finished, as a JVM running clojure.main
# does. Every expected outcome here was measured on JVM Clojure 1.12.5 / JDK 21
# with the same program (timing probes, 2026-09): what gets printed, the exit
# status, and whether the process is still up a few seconds later.
#
# The JVM's idle holds are long — a future's pool worker lingers 60s, a send's
# never ends — so those cases do not wait them out: they check the process is
# still alive after LINGER seconds and kill it. Everything else must end on its
# own well inside CAP.
#
# JOLT_BIN overrides the binary under test (defaults to bin/jolt source mode).
set -u
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
JOLT="${JOLT_BIN:-bin/jolt}"
CAP=40      # nothing that should end may take longer than this
LINGER=4    # a held process must still be up this long after its work is done
pass=0; fail=0
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# run SECONDS FILE ARGS... -> sets out, rc (142 = still running at SECONDS, killed)
run() {
  secs="$1"; shift
  out=$(perl -e 'alarm shift; exec @ARGV' "$secs" "$@" 2>&1 < /dev/null)
  rc=$?
}
script() { printf '%s\n' "$2" > "$tmp/$1.clj"; echo "$tmp/$1.clj"; }
ok() { pass=$((pass+1)); }
bad() { fail=$((fail+1)); echo "FAIL: $1 — rc=$rc out=[$out]"; }

# ends: exits by itself with status RC and output containing WANT (or empty WANT)
ends() { # label rc want program
  f=$(script case "$4")
  run "$CAP" "$JOLT" "$f"
  if [ "$rc" = "$2" ] && { [ -z "$3" ] || printf '%s' "$out" | grep -q -- "$3"; }; then ok; else bad "$1"; fi
}
# ends-without: exits with status 0 and output NOT containing WANT
ends_without() { # label unwanted program
  f=$(script case "$3")
  run "$CAP" "$JOLT" "$f"
  if [ "$rc" = 0 ] && ! printf '%s' "$out" | grep -q -- "$2"; then ok; else bad "$1"; fi
}
# held: still running LINGER seconds after the program returned
held() { # label program
  f=$(script case "$2")
  run "$LINGER" "$JOLT" "$f"
  if [ "$rc" = 142 ]; then ok; else bad "$1"; fi
}

# -- pending work on non-daemon threads is waited for --------------------------
ends "pending future is finished" 0 ":done" \
  '(future (Thread/sleep 1000) (println :done)) (shutdown-agents)'
# a future is an agent-pool thread, not a daemon, even when a daemon thread
# (core.async's) starts it
ends "a future started on a daemon thread is finished" 0 ":done" \
  "(require '[clojure.core.async :as a]) (a/<!! (a/thread (future (Thread/sleep 1000) (println :done)))) (future (Thread/sleep 1500) (shutdown-agents))"
ends "pending pmap is finished" 0 "\[2 3\]" '(println (vec (pmap inc [1 2]))) (shutdown-agents)'
ends "pending send-off is finished" 0 ":done" \
  '(send-off (agent 0) (fn [_] (Thread/sleep 1000) (println :done))) (shutdown-agents)'
ends "pending plain Thread is finished" 0 ":done" \
  '(.start (Thread. (fn [] (Thread/sleep 1000) (println :done))))'
ends "shut-down pool drains its queue" 0 ":done" \
  '(let [ex (java.util.concurrent.Executors/newFixedThreadPool 1)] (.submit ex ^Runnable (fn [] (Thread/sleep 1000) (println :done))) (.shutdown ex))'
ends "shutdownNow interrupts the running task" 0 ":interrupted" \
  '(let [ex (java.util.concurrent.Executors/newFixedThreadPool 1)] (.submit ex ^Runnable (fn [] (try (Thread/sleep 60000) (println :slept) (catch InterruptedException e (println :interrupted))))) (Thread/sleep 200) (.shutdownNow ex))'
ends "a delayed task still runs after shutdown" 0 ":done" \
  '(let [ex (java.util.concurrent.Executors/newScheduledThreadPool 1)] (.schedule ex ^Runnable (fn [] (println :done)) 1 java.util.concurrent.TimeUnit/SECONDS) (.shutdown ex))'
ends "allowCoreThreadTimeOut lets an idle pool go" 0 ":x" \
  '(let [ex (java.util.concurrent.ThreadPoolExecutor. 1 1 1 java.util.concurrent.TimeUnit/SECONDS (java.util.concurrent.LinkedBlockingQueue.))] (.allowCoreThreadTimeOut ex true) (.get (.submit ex ^Callable (fn [] 1))) (println :x))'
ends "shutdown hooks run after the wait" 0 ":thread :hook" \
  '(.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (println :hook)))) (.start (Thread. (fn [] (Thread/sleep 500) (print :thread "")))) '

# -- idle holds: the agent pools and a pool never shut down ---------------------
held "a finished future's pool worker lingers" '(deref (future 1)) (println :x)'
held "a finished send holds the process" '(let [a (agent 0)] (send a inc) (await a) (println @a))'
held "a pool never shut down holds the process" \
  '(let [ex (java.util.concurrent.Executors/newFixedThreadPool 1)] (.get (.submit ex ^Callable (fn [] 1))) (println :x))'
held "a cached pool's idle worker lingers" \
  '(let [ex (java.util.concurrent.Executors/newCachedThreadPool)] (.get (.submit ex ^Callable (fn [] 1))) (println :x))'
# ...but a virtual-thread executor never shut down does not: virtual threads are
# daemons (it lingered 60s, as the cached pool it is built on)
ends "a virtual-thread executor never shut down does not hold the process" 0 ":x" \
  '(let [ex (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)] (.get (.submit ex ^Callable (fn [] 1))) (println :x))'
ends_without "a virtual-thread executor's pending task is abandoned" ":done" \
  '(.submit (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor) ^Runnable (fn [] (Thread/sleep 3000) (println :done)))'
held"a non-daemon thread blocked forever hangs" '(.start (Thread. (fn [] @(promise))))'
ends "shutdown-agents ends the linger" 0 ":x" '(deref (future 1)) (send (agent 0) inc) (shutdown-agents) (println :x)'

# -- daemon threads never hold it -----------------------------------------------
ends "a daemon Thread blocked forever does not hold" 0 "" \
  '(.start (doto (Thread. (fn [] @(promise))) (.setDaemon true)))'
ends_without "a/thread's pending work is abandoned" ":done" \
  '(require (quote [clojure.core.async :as a])) (a/thread (Thread/sleep 3000) (println :done))'
ends_without "a take! nobody completes does not hold" ":never" \
  '(require (quote [clojure.core.async :as a])) (a/take! (a/chan) (fn [_] (println :never)))'
ends_without "CompletableFuture's pending work is abandoned" ":done" \
  '(java.util.concurrent.CompletableFuture/supplyAsync (fn [] (Thread/sleep 3000) (println :done)))'
ends_without "a daemon ThreadFactory's pool does not hold" ":done" \
  '(.submit (java.util.concurrent.Executors/newFixedThreadPool 1 (reify java.util.concurrent.ThreadFactory (newThread [_ r] (doto (Thread. ^Runnable r) (.setDaemon true))))) ^Runnable (fn [] (Thread/sleep 3000) (println :done)))'

# -- what ends the process at once ----------------------------------------------
ends "System/exit does not wait" 3 "" '(future (Thread/sleep 60000)) (System/exit 3)'
ends "Runtime.halt does not wait" 4 "" '(future (Thread/sleep 60000)) (.halt (Runtime/getRuntime) 4)'
ends "an uncaught error does not wait" 1 "boom" '(future (Thread/sleep 60000)) (throw (ex-info "boom" {}))'
ends "a future after shutdown-agents is rejected" 0 "RejectedExecutionException" \
  '(shutdown-agents) (println (try @(future 1) (catch Exception e (.getName (class e)))))'
ends "a send after shutdown-agents returns the agent" 0 ":sent" \
  '(shutdown-agents) (println (try (send (agent 0) inc) :sent (catch Exception e (.getName (class e)))))'

# -- the other entry points wait the same way -----------------------------------
run "$CAP" "$JOLT" -e '(future (Thread/sleep 1000) (println :done)) (shutdown-agents)'
if [ "$rc" = 0 ] && printf '%s' "$out" | grep -q ":done"; then ok; else bad "-e waits for a pending future"; fi
out=$(printf '(future (Thread/sleep 1000) (println :done))\n(shutdown-agents)\n' | perl -e 'alarm shift; exec @ARGV' "$CAP" "$JOLT" 2>&1); rc=$?
if [ "$rc" = 0 ] && printf '%s' "$out" | grep -q ":done"; then ok; else bad "the REPL at end of input waits for a pending future"; fi

# `jolt run <task>` is babashka's entry, not clojure.main's, and follows bb's rule
# (bb 1.12: its future and agent threads are daemons): a future the task leaves
# running is abandoned, a plain Thread it starts is still waited for.
case "$JOLT" in /*) J="$JOLT" ;; *) J="$root/$JOLT" ;; esac
mkdir -p "$tmp/bbtask"
printf '%s\n' '{:tasks {fut (do (future (Thread/sleep 3000) (println :done)) (println :returned)) th (do (.start (Thread. (fn [] (Thread/sleep 1000) (println :thread-done)))) (println :returned))}}' > "$tmp/bbtask/bb.edn"
out=$(cd "$tmp/bbtask" && JOLT_NO_USER_DEPS=1 perl -e 'alarm shift; exec @ARGV' "$CAP" "$J" run fut 2>&1); rc=$?
if [ "$rc" = 0 ] && printf '%s' "$out" | grep -q ":returned" && ! printf '%s' "$out" | grep -q ":done"; then ok; else bad "a task does not wait for its future"; fi
out=$(cd "$tmp/bbtask" && JOLT_NO_USER_DEPS=1 perl -e 'alarm shift; exec @ARGV' "$CAP" "$J" run th 2>&1); rc=$?
if [ "$rc" = 0 ] && printf '%s' "$out" | grep -q ":thread-done"; then ok; else bad "a task waits for a Thread it started"; fi

# `jolt build` is the compiler, not the program: it loads the app's namespaces
# to compile them, and build-app's app.core derefs a future at load, which would
# hold a program up for the pool's 60s keep-alive. The build ends when its work
# is done.
t0=$(date +%s)
out=$(JOLT_PWD="$root/test/chez/build-app" JOLT_RUNTIME_CACHE_DIR="$tmp/rtcache" "$J" build -m app.core -o "$tmp/built" 2>&1); rc=$?
t1=$(date +%s)
# the build itself takes well under 30s; waiting on the pool would add 60
if [ "$rc" = 0 ] && [ -x "$tmp/built" ] && [ $((t1 - t0)) -lt 40 ]; then ok; else bad "jolt build does not wait for the app's pools ($((t1 - t0))s)"; fi

echo "exit-wait smoke: $pass passed, $fail failed"
[ "$fail" = 0 ]
