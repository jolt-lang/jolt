#!/bin/sh
# Gate for tools/ns_audit.clj: its fixtures report what they should, and every
# namespace jolt ships passes. A namespace nothing loads in a gate is only ever
# checked here.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root" || exit 2
jolt="${JOLT_BIN:-bin/jolt}"
fail=0

out=$("$jolt" tools/ns_audit.clj test/chez/ns-audit/good 2>&1); rc=$?
if [ "$rc" != 0 ]; then
  echo "ns-audit: good fixtures: expected exit 0, got $rc"; echo "$out"; fail=1
fi

out=$("$jolt" tools/ns_audit.clj test/chez/ns-audit/bad 2>&1); rc=$?
if [ "$rc" != 1 ]; then
  echo "ns-audit: bad fixtures: expected exit 1, got $rc"; echo "$out"; fail=1
fi
for want in \
  'second_ns.clj:3: ns second-ns-b: unknown reference clause' \
  'split_doc.clj:3: ns split-doc: not a reference clause, got: open' \
  'unbalanced_doc.clj: cannot be read' \
  'unknown_clause.clj:1: ns unknown-clause: unknown reference clause, got: (:requires' \
  '4 files, 4 findings'; do
  case "$out" in
    *"$want"*) ;;
    *) echo "ns-audit: bad fixtures: no \"$want\" in:"; echo "$out"; fail=1 ;;
  esac
done

"$jolt" tools/ns_audit.clj stdlib jolt-core || fail=1

[ "$fail" = 0 ] && echo "ns-audit: ok"
exit "$fail"
