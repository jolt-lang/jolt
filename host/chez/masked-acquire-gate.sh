#!/bin/sh
# Runs test/chez/masked-acquire-gc-test.ss under a deadline. The failure that
# gate catches is a collection that never finishes, and every Scheme thread of
# the process, its own deadline loop included, waits in that collection, so the
# limit has to come from outside. macOS has no timeout(1).
chez=${1:-chez}
secs=${MASKED_ACQUIRE_DEADLINE:-120}
"$chez" --script test/chez/masked-acquire-gc-test.ss &
step=$!
( sleep "$secs"; kill -9 "$step" 2>/dev/null ) &
watchdog=$!
wait "$step"; status=$?
if kill -0 "$watchdog" 2>/dev/null; then
  kill "$watchdog" 2>/dev/null
  wait "$watchdog" 2>/dev/null
  exit "$status"
fi
wait "$watchdog" 2>/dev/null
echo "masked-acquire: FAIL: did not finish within ${secs}s (a collection stalled on a lock taken with interrupts disabled)"
exit 1
