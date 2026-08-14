#!/bin/sh
# A :blocking binding whose symbol resolves through a REGISTERED native handle
# (the scoped-resolution path) must stay __collect_safe. The probe registers a
# handle the way any library feature-probe does; on Linux dlsym through that
# handle also resolves libc symbols via its dependency scope, so the scoped
# branch — not the global fallback — builds the procedure. A read parked on an
# empty pipe must then leave the collector free: when the scoped branch drops
# the convention, the collect below waits on the parked reader forever (the
# v0.7.10 first-cold-run wedge, bisected to the scoped-FFI merge). Runs the
# REAL binary: script-mode gates never initialize the dlsym helper, so the
# scoped path cannot arm there and the check would be vacuous.
set -eu
JOLT="${1:?usage: ffi-collect-safe-smoke.sh <jolt-binary>}"
out=$("$JOLT" -e "
(require (quote [jolt.ffi]))
(jolt.ffi/load-library {:darwin \"libz.dylib\" :linux \"libz.so.1\" :windows \"zlib1.dll\"})
(jolt.ffi/defcfn cs-pipe \"pipe\" [:pointer] :int)
(jolt.ffi/defcfn cs-read \"read\" [:int :pointer :size_t] :ssize_t :blocking)
(jolt.ffi/defcfn cs-write \"write\" [:int :pointer :size_t] :ssize_t)
(jolt.ffi/defcfn cs-close \"close\" [:int] :int)
(let [fds (jolt.ffi/alloc 8)
      _ (cs-pipe fds)
      rfd (jolt.ffi/read fds :int 0)
      wfd (jolt.ffi/read fds :int 4)
      buf (jolt.ffi/alloc 8)
      reader (future (cs-read rfd buf 1))
      _ (Thread/sleep 300)
      gcp (promise)
      _ (future (System/gc) (System/gc) (deliver gcp :ok))
      r (loop [i 0]
          (cond (realized? gcp) :collected
                (>= i 60) :collector-stuck
                :else (do (Thread/sleep 100) (recur (inc i)))))]
  (cs-write wfd buf 1)
  @reader
  (cs-close rfd) (cs-close wfd)
  (jolt.ffi/free fds) (jolt.ffi/free buf)
  (println \"ffi-collect-safe:\" r))" </dev/null 2>&1 | tail -1)
echo "$out"
case "$out" in
  *":collected") exit 0 ;;
  *) echo "ffi-collect-safe smoke: FAILED"; exit 1 ;;
esac
