#!/bin/sh
# Files/probeContentType reads ~/.mime.types before the JDK's own table (#1554).
#
# The JDK's default detector asks ~/.mime.types, then (on Linux) /etc/mime.types,
# then content-types.properties. Each expectation below was measured against
# OpenJDK 21 with -Duser.home pointing at the same file. HOME is read once per
# process, so this drives bin/jolt with a scratch HOME rather than being a unit
# row. Windows types files from the registry instead, which jolt does not model,
# so the ~/.mime.types rows do not apply there.

set -e

pass=0
fails=0
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
jolt="$root/bin/jolt"

case "$(uname -s 2>/dev/null)" in
  MINGW*|MSYS*|CYGWIN*|Windows*) echo "mime-types-smoke: skipped on Windows"; exit 0 ;;
esac

home="$(mktemp -d "${TMPDIR:-/tmp}/jolt-mime-types-XXXXXX")"
trap 'rm -rf "$home"' EXIT

cat > "$home/.mime.types" <<'EOF'
# a comment line
text/x-clojure	clj cljc
TEXT/X-Upper  UPX
image/x-mine png
text/x-first dup
text/x-second dup
type=application/x-sol exts=sol,sol2
text/x-cmt cmt # a trailing comment
application/x-tgz tar.gz
text/x-bashrc bashrc
  text/x-lead lead
bogus bad
text/x-param;charset=utf-8 prm
EOF

check() {
  clabel="$1"; cgot="$2"; cwant="$3"
  if [ "$cgot" = "$cwant" ]; then
    echo "PASS: ($clabel) $cgot"; pass=$((pass+1))
  else
    echo "FAIL: ($clabel) got '$cgot', want '$cwant'"; fails=$((fails+1))
  fi
}

# probe <name>...: the types, one call, printed as a vector.
probe() {
  names=""
  for n in "$@"; do names="$names \"$n\""; done
  HOME="$home" JOLT_QUIET=1 "$jolt" -e "(mapv (fn [s] (java.nio.file.Files/probeContentType (java.nio.file.Path/of s (into-array String [])))) [$names])" </dev/null 2>/dev/null | tail -1
}

check "an entry types its extensions" \
  "$(probe x.clj dir/y.cljc)" '["text/x-clojure" "text/x-clojure"]'

check "the type is lower-cased; the extension matches in its own case only" \
  "$(probe x.UPX x.upx x.CLJ)" '["text/x-upper" nil nil]'

check "~/.mime.types comes before the JDK's table" \
  "$(probe x.png)" '["image/x-mine"]'

check "the JDK's table still answers what the file does not, in any case" \
  "$(probe x.PDF)" '["application/pdf"]'

check "the first entry for an extension wins" \
  "$(probe x.dup)" '["text/x-first"]'

check "a Solaris type=/exts= line" \
  "$(probe x.sol x.sol2)" '["application/x-sol" "application/x-sol"]'

check "# starts a comment anywhere on a line" \
  "$(probe x.cmt x.trailing x.comment)" '["text/x-cmt" nil nil]'

check "the extension starts after the first dot, then gives way" \
  "$(probe x.tar.gz .bashrc dir/.bashrc a.bashrc)" \
  '["application/x-tgz" "text/x-bashrc" "text/x-bashrc" "text/x-bashrc"]'

check "leading whitespace on a line" \
  "$(probe x.lead)" '["text/x-lead"]'

check "an entry with no subtype answers nil rather than falling through" \
  "$(probe x.bad)" '[nil]'

check "a line holding = is read as Solaris, so a parameter hides the entry" \
  "$(probe x.prm)" '[nil]'

echo "mime-types-smoke: $pass pass, $fails fail"
[ "$fails" -eq 0 ]
