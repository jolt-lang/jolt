#!/bin/sh
# Run from any directory. Each process prints one median per workload.
set -eu
if [ "$#" -ne 4 ]; then
  echo "usage: $0 MAIN_BINARY BEFORE_BINARY FIXED_BINARY OUTPUT_DIRECTORY" >&2
  exit 2
fi
main=$1 before=$2 fixed=$3 out=$4
probe=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)/single-edit.clj
mkdir -p "$out"
tmp=$(mktemp -d "$out/.edits.XXXXXXXX")
trap 'rm -rf "$tmp"' EXIT
csv="$tmp/medians.csv"
echo 'version,launch,size,operation,ns_per_edit,bytes_per_edit' > "$csv"
for launch in 1 2 3 4 5; do
  case $((launch % 3)) in
    1) order='main before fixed' ;;
    2) order='before fixed main' ;;
    0) order='fixed main before' ;;
  esac
  for version in $order; do
    case $version in
      main) binary=$main ;;
      before) binary=$before ;;
      fixed) binary=$fixed ;;
    esac
    JOLT_CACHE_DIR="$out/cache-$version" "$binary" "$probe" > "$tmp/process.txt"
    awk -v v="$version" -v l="$launch" '
      BEGIN { OFS="," }
      NF != 4 { exit 1 }
      { print v,l,$1,$2,$3,$4 }
      END { if (NR != 9) exit 1 }
    ' "$tmp/process.txt" >> "$csv"
  done
done
mv "$csv" "$out/single-edit-medians.csv"
