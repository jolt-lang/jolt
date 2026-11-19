#!/usr/bin/env bash
# The install script under /bin/bash (3.2 on macOS): a token goes only to the
# GitHub API, and with the API unreachable the latest release still resolves,
# from the releases page's redirect. The last check needs the network.

set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
bash_bin=/bin/bash
fail() { echo "install-smoke: $*" >&2; exit 1; }

# fetch with curl stubbed: which requests carry the token. The header comes on
# stdin (--header @-), never in argv, where ps would show it to other users.
fns="$(sed -n '/^has() {/,/^}/p;/^github_api=/p;/^fetch() {/,/^}/p' "$root/install")"
calls="$(GITHUB_TOKEN=tok "$bash_bin" -c "set -euo pipefail
curl() { h=; case \" \$* \" in *\" --header @- \"*) h=\$(cat) ;; esac; echo \"curl \$* hdr=\$h\"; }
$fns
fetch https://api.github.com/repos/x/releases/latest
fetch https://github.com/x/releases/download/a.tgz")"
grep 'https://api.github.com/' <<<"$calls" | grep -q 'hdr=Authorization: Bearer tok$' || fail "no token on the API request"
grep -q 'Bearer tok.*hdr=' <<<"$calls" && fail "the token is on a command line"
grep 'releases/download' <<<"$calls" | grep -q Authorization && fail "token sent off the API"
notoken="$("$bash_bin" -c "set -euo pipefail
curl() { h=; case \" \$* \" in *\" --header @- \"*) h=\$(cat) ;; esac; echo \"curl \$* hdr=\$h\"; }
$fns
fetch https://api.github.com/repos/x")"
grep -q Authorization <<<"$notoken" && fail "a header without a token"

# the API unreachable: the redirect names the release
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
JOLT_GITHUB_API=https://127.0.0.1:9 "$bash_bin" "$root/install" \
    --dir "$tmp/bin" --download-dir "$tmp/dl" >/dev/null
[[ -x "$tmp/bin/jolt" ]] || fail "no jolt installed with the API unreachable"

echo "install-smoke: ok"
