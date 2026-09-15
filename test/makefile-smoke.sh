#!/usr/bin/env bash

set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"

# Test each caller override independently of the Make process that launched us.
# Command-line variables also propagate to nested Makes through these flags.
unset CC JOLT_CC CHEZ CHEZSCHEME MAKEFLAGS MAKEOVERRIDES GAMBIT_PREFIX JOLT_REQUIRE_GAMBIT

tmp="$(mktemp -d)"
# A provision-armed Makefile parse mkdirs .cache/local (local.mk runs at parse;
# no downloads happen there). If a check dies mid-run, don't leave those dirs
# behind on a checkout that never provisioned.
trap 'rm -rf "$tmp"; if [ "${had_provision_dirs:-0}" -eq 0 ] && [ -e "$root/.cache/local" ]; then rm -rf "$root/.cache/local"; fi' EXIT

fake_chez="$tmp/chez"
cat >"$fake_chez" <<'FAKE'
#!/usr/bin/env bash
case ${1-} in
  -q)
    cat >/dev/null
    printf '#t\n'
    ;;
  --version)
    printf '10.4.1\n'
    ;;
  *)
    exit 1
    ;;
esac
FAKE
chmod +x "$fake_chez"

# The probe lives in its own makefile that includes the real one, rather than
# being injected with `make --eval`: --eval arrived in GNU Make 3.82, and macOS
# still ships 3.81, where every check here died with "unrecognized option". Since
# this gate is part of `ci`, that took `make test` down on any Mac using the
# system make. Running with -C keeps every relative path in the included Makefile
# (.cache/makes, host/chez/...) resolving against the repo root.
probe_mk="$tmp/probe.mk"
cat >"$probe_mk" <<'MK'
include Makefile
inspect-chez:
	@printf "%s\n" \
	  "chez=$(CHEZ)" \
	  "jolt-chez=$(JOLT-CHEZ)" \
	  "local-root=$(LOCAL-ROOT)" \
	  "local-origin=$(origin LOCAL-LOADED)" \
	  "gcc-origin=$(origin GCC-LOADED)" \
	  "joltcc-origin=$(origin JOLT_CC)" \
	  "joltcc-env=$${JOLT_CC-}"
inspect-gambit:
	@printf "%s\n" "gsi=$(GAMBIT_GSI)" "gsc=$(GAMBIT_GSC)"
MK

check_override() {
  local name=$1
  local output

  output="$(
    make -C "$root" -f "$probe_mk" --no-print-directory -s \
      "$name=$fake_chez" inspect-chez
  )"

  grep -Fx "chez=$fake_chez" <<<"$output" >/dev/null
  grep -Fx "jolt-chez=$fake_chez" <<<"$output" >/dev/null
  grep -Fx "local-origin=undefined" <<<"$output" >/dev/null
  grep -Fx "gcc-origin=undefined" <<<"$output" >/dev/null
  # No provisioning armed, so JOLT_CC must be unset — the caller's system
  # toolchain links whatever gets built.
  grep -Fx "joltcc-origin=undefined" <<<"$output" >/dev/null
  grep -Fx "joltcc-env=" <<<"$output" >/dev/null

  make --no-print-directory -s "$name=$fake_chez" deps
}

check_override CHEZ
check_override CHEZSCHEME

# A Chez already on PATH is used as-is, with NOTHING set. This is the case the
# release matrix depends on: every non-Linux row builds its own Chez and exposes
# it as `chez` on PATH, then runs a bare `make jolt-release`. If that fell through
# to local provisioning, a release would silently ship a binary built by a Chez
# other than the one the job prepared — and the two differ in exactly the ways
# that matter here (threading, libc floor, cross target).
check_path_discovery() {
  local output real

  # A toolchain already provisioned into .cache/local takes precedence over PATH,
  # so this checkout can no longer answer the question. Legitimate state, and it
  # does not arise on a release runner: those are fresh clones that put their own
  # chez on PATH before any make runs, so nothing provisions.
  if [ -d "$root/.cache/local" ]; then
    echo "makefile smoke: SKIP PATH discovery (.cache/local is provisioned here)"
    return 0
  fi

  # Deliberately the REAL chez, not the stub used above: provisioning validates
  # what it finds on PATH and rejects a stub, so a fake would fall through and
  # provision — testing the opposite of the intended case.
  real="$(command -v chez 2>/dev/null || command -v chezscheme 2>/dev/null || command -v scheme 2>/dev/null || true)"
  if [ -z "$real" ]; then
    echo "makefile smoke: SKIP PATH discovery (no Chez on PATH)"
    return 0
  fi

  output="$(make -C "$root" -f "$probe_mk" --no-print-directory -s inspect-chez)"

  grep -Fx "jolt-chez=$real" <<<"$output" >/dev/null || {
    echo "a Chez on PATH was not selected; expected $real, got:" >&2
    echo "$output" >&2
    exit 1
  }
  grep -Fx "local-origin=undefined" <<<"$output" >/dev/null || {
    echo "provisioned a local toolchain despite a Chez on PATH:" >&2
    echo "$output" >&2
    exit 1
  }
}

check_path_discovery

# A fake Chez answering all three discovery names (chez, chezscheme, scheme)
# with a chosen version, so shadowing PATH controls exactly what discovery
# sees regardless of what else the machine has installed.
fake_chez_bin() {
  local dir=$1 line=$2 bare=$3

  mkdir -p "$dir"
  cat >"$dir/chez" <<FAKE
#!/usr/bin/env bash
case \${1-} in
  -q)
    prog=\$(cat)
    case \$prog in
      *scheme-version*) printf '%s\n' '$line' ;;
      *) printf '#t\n' ;;
    esac
    ;;
  --version)
    printf '%s\n' '$bare'
    ;;
  *)
    exit 1
    ;;
esac
FAKE
  chmod +x "$dir/chez"
  ln -sf chez "$dir/chezscheme"
  ln -sf chez "$dir/scheme"
}

# Nothing explicit selected + a Chez on PATH at or above the floor: used as-is,
# with NOTHING provisioned or pinned — the system toolchain builds and links
# everything, self-consistent end to end. All three discovery names are
# shadowed so a real Chez further down PATH can't answer for the fake.
check_system_chez_preferred() {
  local bin out

  bin="$tmp/newer"
  fake_chez_bin "$bin" 'Chez Scheme Version 10.9.0' '10.9.0'
  bin="$(cd "$bin" && pwd -P)"

  out="$(PATH="$bin:$PATH" make -C "$root" -f "$probe_mk" --no-print-directory -s inspect-chez)"

  grep -Fx "jolt-chez=$bin/chez" <<<"$out" >/dev/null || {
    echo "a same-or-newer Chez on PATH was not selected; expected $bin/chez, got:" >&2
    echo "$out" >&2
    exit 1
  }
  grep -Fx "gcc-origin=undefined" <<<"$out" >/dev/null || {
    echo "provisioning armed despite a same-or-newer system Chez:" >&2
    echo "$out" >&2
    exit 1
  }
  grep -Fx "joltcc-origin=undefined" <<<"$out" >/dev/null || {
    echo "JOLT_CC pinned without any provisioning armed:" >&2
    echo "$out" >&2
    exit 1
  }
}

# Nothing qualifies (only an older-than-floor Chez on PATH): provisioning arms —
# pinned Chez + xPack GCC — and CC is pinned to that same provisioned GCC, so
# the standalone-binary link cannot mix one compiler's driver with another's
# binutils (#788: distro cc 16 driving the provisioned pre-.base64 gas).
check_provision_fallback() {  local bin out

  bin="$tmp/older"
  fake_chez_bin "$bin" 'Chez Scheme Version 10.3.9' '10.3.9'
  bin="$(cd "$bin" && pwd -P)"

  out="$(PATH="$bin:$PATH" make -C "$root" -f "$probe_mk" --no-print-directory -s inspect-chez)"

  grep -Fx "gcc-origin=file" <<<"$out" >/dev/null || {
    echo "provisioning did not arm despite no qualifying system Chez:" >&2
    echo "$out" >&2
    exit 1
  }
  # Against LOCAL-ROOT as the Makefile computes it, not against a hardcoded
  # .cache/local: the nix develop shell puts the provisioned toolchain somewhere
  # else entirely, and a pattern anchored on this checkout asserts the layout
  # rather than the property (which is that the PINNED toolchain won, not the
  # older one on PATH).
  local root_dir
  root_dir="$(sed -n 's/^local-root=//p' <<<"$out")"
  grep -E "^jolt-chez=${root_dir%/}/chezscheme-[0-9.]+/bin/scheme$" <<<"$out" >/dev/null || {
    echo "pinned provisioned Chez not selected as the fallback:" >&2
    echo "$out" >&2
    exit 1
  }
  grep -E "^joltcc-env=${root_dir%/}/gcc-[0-9.-]+/bin/gcc$" <<<"$out" >/dev/null || {
    echo "provisioning armed but JOLT_CC not pinned to the provisioned GCC:" >&2
    echo "$out" >&2
    exit 1
  }
}

# A Chez on PATH that answers NOTHING (broken install, stub, wrong binary) must
# not be selected by the floor probe: an empty version carries no information,
# and an all-equal component comparison would otherwise treat it as at-floor.
# The pinned provision is the safe fallback — this is the "broken" arm of the
# same-or-newer rule.
check_broken_chez_falls_back() {
  local bin out

  bin="$tmp/broken"
  mkdir -p "$bin"
  printf '#!/usr/bin/env bash\nexit 0\n' >"$bin/chez"
  chmod +x "$bin/chez"
  ln -sf chez "$bin/chezscheme"
  ln -sf chez "$bin/scheme"
  bin="$(cd "$bin" && pwd -P)"

  out="$(PATH="$bin:$PATH" make -C "$root" -f "$probe_mk" --no-print-directory -s inspect-chez)"

  grep -Fx "gcc-origin=file" <<<"$out" >/dev/null || {
    echo "a silent Chez on PATH was selected instead of falling back to provisioning:" >&2
    echo "$out" >&2
    exit 1
  }
}

# The gambit gates find gsi/gsc under GAMBIT_PREFIX (brew's prefix when unset;
# tests.yml points it at the Gambit it builds from source) and skip when there
# is none there — unless JOLT_REQUIRE_GAMBIT is set, which makes the skip a
# failure. CI sets it: without that, an install step that quietly stopped
# installing turns every gambit gate into a no-op that reads as green, which is
# exactly how the boot ran dead for two weeks before the gates existed.
# Probed with a prefix holding no binaries at all, so the branch under test is
# the one taken on a machine without gambit.
check_gambit_detection() {
  local out t
  mkdir -p "$tmp/no-gambit/bin"

  # A relative prefix resolves against the repo root: gambitboot and
  # unbound-check.sh cd into host/gambit before running the binary.
  out="$(make -C "$root" -f "$probe_mk" --no-print-directory -s "GAMBIT_PREFIX=test/.no-gambit" inspect-gambit)"
  grep -Fx "gsi=$root/test/.no-gambit/bin/gsi" <<<"$out" >/dev/null || {
    echo "GAMBIT_PREFIX not honored, or not made absolute:" >&2
    echo "$out" >&2
    exit 1
  }
  grep -Fx "gsc=$root/test/.no-gambit/bin/gsc" <<<"$out" >/dev/null || {
    echo "GAMBIT_GSC does not follow GAMBIT_PREFIX:" >&2
    echo "$out" >&2
    exit 1
  }

  # One gsi-gated and one gsc-gated target (the latter also pulls gambitboot).
  for t in gambitcheck gambitunbound; do
    out="$(JOLT_REQUIRE_GAMBIT= make -C "$root" --no-print-directory "GAMBIT_PREFIX=$tmp/no-gambit" "$t" 2>&1)" || {
      echo "$t failed instead of skipping on a machine without gambit:" >&2
      echo "$out" >&2
      exit 1
    }
    grep -q "skipped" <<<"$out" || {
      echo "$t skipped silently (no skip line to read in a log):" >&2
      echo "$out" >&2
      exit 1
    }
    if out="$(JOLT_REQUIRE_GAMBIT=1 make -C "$root" --no-print-directory "GAMBIT_PREFIX=$tmp/no-gambit" "$t" 2>&1)"; then
      echo "$t skipped although JOLT_REQUIRE_GAMBIT is set:" >&2
      echo "$out" >&2
      exit 1
    fi
    grep -q "JOLT_REQUIRE_GAMBIT" <<<"$out" || {
      echo "$t failed under JOLT_REQUIRE_GAMBIT without saying why:" >&2
      echo "$out" >&2
      exit 1
    }
  done
}

had_provision_dirs=0
[ -e "$root/.cache/local" ] && had_provision_dirs=1

check_system_chez_preferred
check_provision_fallback
check_broken_chez_falls_back
check_gambit_detection

# build.ss bld-cc is the seam the JOLT_CC pin lands on: the native link honors
# $JOLT_CC (empty/unset falls back to cc); JOLT_TARGET_CC still rules cross.
bldcc_ss="$tmp/bldcc.ss"
cat >"$bldcc_ss" <<'SS'
;; build-jolt.ss's exact preamble in a fresh Chez: build.ss's top level derefs
;; compiler vars (jolt.passes.types), so it loads inside the booted compiler
;; image — the same environment `make testbin` actually links in. The bld-cc
;; under test comes from the on-disk file via plain load, exactly as in a real
;; build.
(import (chezscheme))
(load "host/chez/scheme-adapter-runtime.ss")
(load "host/chez/rt.ss")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(load "host/chez/post-prelude-str.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/cli-core.ss")
(load "host/chez/png.ss")
(load "host/chez/loader.ss")
(load "host/chez/java/ffi.ss")
(set-source-roots! ldr-install-roots)
(load "host/chez/build.ss")
(define cross-machine (string-append "zz-" bld-machine))
(define (probe-cc var val)
  (putenv var val)
  (let ((r (if (string=? var "JOLT_TARGET_CC")
               (parameterize ((bld-target cross-machine)) (bld-cc))
               (bld-cc))))
    (putenv var "")
    r))
(write (probe-cc "JOLT_CC" "/x/y/cc")) (newline)
(write (probe-cc "JOLT_CC" "")) (newline)
(write (probe-cc "JOLT_TARGET_CC" "/t/arm-gcc")) (newline)
(write (probe-cc "JOLT_TARGET_CC" "")) (newline)
SS
expected_bldcc="$(cat <<'EOF'
"/x/y/cc"
"cc"
"/t/arm-gcc"
"cc"
EOF
)"
actual_bldcc="$("${JOLT_CHEZ:-chez}" --script "$bldcc_ss")" || {
  echo "makefile smoke: bld-cc probe failed to run" >&2
  exit 1
}
[ "$actual_bldcc" = "$expected_bldcc" ] || {
  echo "makefile smoke: bld-cc does not honor CC / JOLT_TARGET_CC; expected:" >&2
  echo "$expected_bldcc" >&2
  echo "got:" >&2
  echo "$actual_bldcc" >&2
  exit 1
}

echo "makefile smoke: explicit Chez overrides bypass local provisioning,"
echo "                a Chez on PATH is used without provisioning,"
echo "                a same-or-newer system Chez is preferred over provisioning,"
echo "                provisioning pins CC to the provisioned GCC,"
echo "                the gambit gates honor GAMBIT_PREFIX and JOLT_REQUIRE_GAMBIT,"
echo "                and bld-cc honors CC / JOLT_TARGET_CC"
