#!/bin/sh
# deps-no-unzip-smoke.sh — Maven dependencies load from their jars in place,
# with no unzip on PATH and no extraction (jolt issues #988 and #1005). A jar
# on the roots is read through its central directory (host/chez/java/zip-file.ss),
# so:
#   1. a :mvn/version dependency from an offline local Maven repository, whose
#      jar tools/mkjar.clj writes, resolves and loads with an empty PATH; the
#      root IS the jar, nothing is extracted beside it, a resource in the jar
#      resolves to a jar:file: URL that slurps, and *file* names the entry;
#   2. a jar that is not a zip fails resolution loudly and caches nothing;
#   3. a jar cut short inside its central directory fails the same way;
#   4. a jar whose entry fails its CRC-32 resolves (its directory is whole), and
#      the require of that entry fails with a ZipException that names the jar,
#      as reading a damaged entry does;
#   5. a :local/root jar is its own root too, with no extraction cache;
#   6. a jar on :paths with a launcher stub before the archive (an executable
#      jar) loads, and its jar: URLs and *file* are absolute without the root's
#      "./" spelling;
#   7. the pom.xml a jar packages is consulted for its dependencies when no
#      .pom sits beside the jar: one it declares resolves and loads, and one no
#      repository has fails the resolution;
#   8. a jar root's resources answer as the JDK's class loader does: a
#      directory without its slash, openConnection, toURI, relative URLs;
#   9. resource URLs from directory and jar roots are percent-encoded.
#
# JOLT_BIN is a built jolt, which needs no program on PATH; the gate runs
# target/release/jolt (make testbin).
#   JOLT_BIN=target/release/jolt sh host/chez/deps-no-unzip-smoke.sh
set -u
root="$(CDPATH='' cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root" || exit 1
JOLT="${JOLT_BIN:-target/release/jolt}"
case "$JOLT" in /*) ;; *) JOLT="$root/$JOLT" ;; esac
pass=0; fail=0
# Hermetic: no user deps.edn, the default Maven layout, and an AOT cache of its
# own — the rows that read *file* out of a jar-loaded namespace see the source
# path only when the namespace is COMPILED; a fasl hit in ~/.jolt/aot-cache from
# an earlier run reads the requiring file's *file*, as a JVM AOT class does.
export JOLT_NO_USER_DEPS=1
unset JOLT_MVNLIBS GRENADINE_MAVEN_REPOSITORY
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
export JOLT_CACHE_DIR="$tmp/aot-cache"

# label, then a yes/no word
yn() { if [ "$2" = "yes" ]; then pass=$((pass+1)); else fail=$((fail+1)); echo "  FAIL: $1" >&2; fi; }

m2="$tmp/m2"
empty="$tmp/empty-path"
mkdir -p "$empty"

# A POM for org.example/NAME 1.0.0 in the local repository.
pom() {
  mkdir -p "$m2/org/example/$1/1.0.0"
  printf '<project><modelVersion>4.0.0</modelVersion><groupId>org.example</groupId><artifactId>%s</artifactId><version>1.0.0</version></project>\n' "$1" \
    > "$m2/org/example/$1/1.0.0/$1-1.0.0.pom"
}

# 1. a jar from the local repository resolves and loads with an empty PATH
pom nounzip
printf '(ns nounzip.core)\n(def answer 42)\n(def here *file*)\n' > "$tmp/core.clj"
printf '{:from "the jar"}\n' > "$tmp/data.edn"
jar1="$m2/org/example/nounzip/1.0.0/nounzip-1.0.0.jar"
JOLT_PWD="$tmp" JOLT_QUIET=1 "$JOLT" run "$root/tools/mkjar.clj" \
  "$jar1" "nounzip/core.clj=$tmp/core.clj" "nounzip/data.edn=$tmp/data.edn" >/dev/null \
  || { echo "  FAIL: 1: mkjar did not write the jar" >&2; fail=$((fail+1)); }
mkdir -p "$tmp/proj/src/app"
printf '{:paths ["src"] :deps {org.example/nounzip {:mvn/version "1.0.0"}}}\n' > "$tmp/proj/deps.edn"
cat > "$tmp/proj/src/app/core.clj" <<'EOF'
(ns app.core (:require [nounzip.core :as n] [clojure.java.io :as io]))
(defn -main [& _]
  (println "answer" n/answer)
  (println "file" n/here)
  (let [u (io/resource "nounzip/data.edn")]
    (println "url" (str u))
    (println "data" (pr-str (read-string (slurp u))))
    (println "stream" (pr-str (slurp (io/input-stream u))))
    (println "missing" (pr-str (io/resource "nounzip/absent.edn")))))
EOF
out1="$(PATH="$empty" JOLT_PWD="$tmp/proj" JOLT_QUIET=1 JOLT_MAVEN_REPOSITORY="$m2" "$JOLT" run -m app.core 2>&1)"
yn "1: resolves and loads with an empty PATH" \
   "$(printf '%s\n' "$out1" | grep -qx 'answer 42' && echo yes || echo no)"
yn "1: *file* is the entry's classpath name, as on the JVM" \
   "$(printf '%s\n' "$out1" | grep -qx "file nounzip/core.clj" && echo yes || echo no)"
yn "1: a resource in the jar resolves to its jar:file: URL" \
   "$(printf '%s\n' "$out1" | grep -qx "url jar:file:$jar1!/nounzip/data.edn" && echo yes || echo no)"
yn "1: the resource slurps through its URL" \
   "$(printf '%s\n' "$out1" | grep -qx 'data {:from "the jar"}' && echo yes || echo no)"
yn "1: the resource opens as a stream" \
   "$(printf '%s\n' "$out1" | grep -qx 'stream "{:from \\"the jar\\"}\\n"' && echo yes || echo no)"
yn "1: an absent resource is nil" \
   "$(printf '%s\n' "$out1" | grep -qx 'missing nil' && echo yes || echo no)"
yn "1: nothing was extracted beside the jar" \
   "$([ ! -e "$jar1.jolt" ] && [ -z "$(find "$m2" -name '.jolt-ok' -o -name '.jolt-extract-*')" ] && echo yes || echo no)"
out1p="$(PATH="$empty" JOLT_PWD="$tmp/proj" JOLT_QUIET=1 JOLT_MAVEN_REPOSITORY="$m2" "$JOLT" path 2>&1)"
yn "1: the jar itself is on the roots" \
   "$(printf '%s\n' "$out1p" | grep -qF "$jar1" && echo yes || echo no)"
yn "1: a second run hits the classpath cache and still loads" \
   "$(PATH="$empty" JOLT_PWD="$tmp/proj" JOLT_QUIET=1 JOLT_MAVEN_REPOSITORY="$m2" "$JOLT" run -m app.core 2>&1 | grep -qx 'answer 42' && echo yes || echo no)"

# 2. a jar that is not a zip fails loudly and caches nothing
pom broken
printf 'this is not a zip file\n' > "$m2/org/example/broken/1.0.0/broken-1.0.0.jar"
mkdir -p "$tmp/proj2/src/app2"
printf '{:paths ["src"] :deps {org.example/broken {:mvn/version "1.0.0"}}}\n' > "$tmp/proj2/deps.edn"
printf '(ns app2.core)\n(defn -main [& _] (println "ran"))\n' > "$tmp/proj2/src/app2/core.clj"
out2="$(PATH="$empty" JOLT_PWD="$tmp/proj2" JOLT_QUIET=1 JOLT_MAVEN_REPOSITORY="$m2" "$JOLT" run -m app2.core 2>&1)"
yn "2: a jar that is not a zip fails resolution loudly" \
   "$(printf '%s' "$out2" | grep -q 'not a whole zip archive' && echo yes || echo no)"
yn "2: the message names the jar" \
   "$(printf '%s' "$out2" | grep -q 'broken-1.0.0.jar' && echo yes || echo no)"
yn "2: nothing cached" \
   "$([ ! -d "$tmp/proj2/.jolt/cpcache" ] && echo yes || echo no)"

# 3. a jar cut short inside its central directory: the END record is gone
pom cut
printf '(ns cut.core)\n(def answer 1)\n' > "$tmp/cut.clj"
cutjar="$m2/org/example/cut/1.0.0/cut-1.0.0.jar"
JOLT_PWD="$tmp" JOLT_QUIET=1 "$JOLT" run "$root/tools/mkjar.clj" "$cutjar" "cut/core.clj=$tmp/cut.clj" >/dev/null \
  || { echo "  FAIL: 3: mkjar did not write the jar" >&2; fail=$((fail+1)); }
size="$(wc -c < "$cutjar" | tr -d ' ')"
head -c "$((size - 30))" "$cutjar" > "$cutjar.tmp" && mv "$cutjar.tmp" "$cutjar"
mkdir -p "$tmp/proj3/src/app3"
printf '{:paths ["src"] :deps {org.example/cut {:mvn/version "1.0.0"}}}\n' > "$tmp/proj3/deps.edn"
printf '(ns app3.core)\n(defn -main [& _] (println "ran"))\n' > "$tmp/proj3/src/app3/core.clj"
out3="$(PATH="$empty" JOLT_PWD="$tmp/proj3" JOLT_QUIET=1 JOLT_MAVEN_REPOSITORY="$m2" "$JOLT" run -m app3.core 2>&1)"
yn "3: a cut jar fails resolution loudly" \
   "$(printf '%s' "$out3" | grep -q 'not a whole zip archive' && echo yes || echo no)"
yn "3: nothing cached" \
   "$([ ! -d "$tmp/proj3/.jolt/cpcache" ] && echo yes || echo no)"

# 4. a jar whose entry fails its CRC-32: the directory is whole, so it
# resolves; the require reads the entry and refuses it. mkjar writes a stored
# entry: its data follows a 30-byte local header and the entry's name, and one
# byte changed there leaves the header's CRC-32 behind.
pom midfail
entry=midfail/core.clj
printf '(ns midfail.core)\n(def answer 1)\n' > "$tmp/mid.clj"
midjar="$m2/org/example/midfail/1.0.0/midfail-1.0.0.jar"
JOLT_PWD="$tmp" JOLT_QUIET=1 "$JOLT" run "$root/tools/mkjar.clj" "$midjar" "$entry=$tmp/mid.clj" >/dev/null \
  || { echo "  FAIL: 4: mkjar did not write the jar" >&2; fail=$((fail+1)); }
[ -s "$midjar" ] || { echo "  FAIL: 4: no jar to damage" >&2; fail=$((fail+1)); }
printf 'X' | dd of="$midjar" bs=1 seek=$((30 + ${#entry})) conv=notrunc 2>/dev/null
mkdir -p "$tmp/proj4/src/app4"
printf '{:paths ["src"] :deps {org.example/midfail {:mvn/version "1.0.0"}}}\n' > "$tmp/proj4/deps.edn"
printf '(ns app4.core (:require [midfail.core]))\n(defn -main [& _] (println "ran"))\n' > "$tmp/proj4/src/app4/core.clj"
out4="$(PATH="$empty" JOLT_PWD="$tmp/proj4" JOLT_QUIET=1 JOLT_MAVEN_REPOSITORY="$m2" "$JOLT" run -m app4.core 2>&1)"
yn "4: the damaged entry is refused when it is required" \
   "$(printf '%s' "$out4" | grep -q 'invalid entry CRC' && echo yes || echo no)"
yn "4: the refusal names the jar and the entry" \
   "$(printf '%s' "$out4" | grep -q "midfail-1.0.0.jar!/$entry" && echo yes || echo no)"
yn "4: the program did not run" \
   "$(printf '%s\n' "$out4" | grep -qx 'ran' && echo no || echo yes)"

# 5. a :local/root jar is its own root
mkdir -p "$tmp/proj5/src/app5"
printf '(ns locjar.core)\n(def y 7)\n' > "$tmp/loc.clj"
JOLT_PWD="$tmp" JOLT_QUIET=1 "$JOLT" run "$root/tools/mkjar.clj" "$tmp/proj5/libj.jar" "locjar/core.clj=$tmp/loc.clj" >/dev/null \
  || { echo "  FAIL: 5: mkjar did not write the jar" >&2; fail=$((fail+1)); }
printf '{:paths ["src"] :deps {local/jarlib {:local/root "libj.jar"}}}\n' > "$tmp/proj5/deps.edn"
printf '(ns app5.core (:require [locjar.core :as j]))\n(defn -main [& _] (println "jarlib" j/y))\n' > "$tmp/proj5/src/app5/core.clj"
out5="$(PATH="$empty" JOLT_PWD="$tmp/proj5" JOLT_QUIET=1 HOME="$tmp/home5" "$JOLT" run -m app5.core 2>&1)"
yn "5: a :local/root jar loads in place" \
   "$(printf '%s\n' "$out5" | tail -1 | grep -qx 'jarlib 7' && echo yes || echo no)"
yn "5: no extraction cache under HOME" \
   "$([ ! -d "$tmp/home5/.jolt/jarlibs" ] && echo yes || echo no)"

# 6. a jar on :paths — a root spelled relative, "./lib.jar" — with a launcher
# stub before the archive, as an executable jar has: the central directory is
# found from the END record, so it loads, and its jar: URLs and *file* are
# absolute with no "./" segment, as a directory root's file: URL is.
mkdir -p "$tmp/proj6/src/app6"
printf '(ns pathjar.core)\n(def z 9)\n(def here *file*)\n' > "$tmp/pj.clj"
printf 'k: v\n' > "$tmp/pj.edn"
JOLT_PWD="$tmp" JOLT_QUIET=1 "$JOLT" run "$root/tools/mkjar.clj" "$tmp/pj-plain.jar" "pathjar/core.clj=$tmp/pj.clj" "pathjar/res.edn=$tmp/pj.edn" >/dev/null \
  || { echo "  FAIL: 6: mkjar did not write the jar" >&2; fail=$((fail+1)); }
{ printf '#!/bin/sh\nexec java -jar "$0" "$@"\n'; cat "$tmp/pj-plain.jar"; } > "$tmp/proj6/lib.jar"
printf '{:paths ["src" "lib.jar"]}\n' > "$tmp/proj6/deps.edn"
cat > "$tmp/proj6/src/app6/core.clj" <<'EOF2'
(ns app6.core (:require [pathjar.core :as p] [clojure.java.io :as io]))
(defn -main [& _]
  (println "pathjar" p/z)
  (println "file" p/here)
  (println "url" (str (io/resource "pathjar/res.edn"))))
EOF2
out6="$(PATH="$empty" JOLT_PWD="$tmp/proj6" JOLT_QUIET=1 "$JOLT" run -m app6.core 2>&1)"
yn "6: a stub-prefixed jar on :paths loads in place" \
   "$(printf '%s\n' "$out6" | grep -qx 'pathjar 9' && echo yes || echo no)"
yn "6: *file* is the entry's classpath name, whatever the root's spelling" \
   "$(printf '%s\n' "$out6" | grep -qx "file pathjar/core.clj" && echo yes || echo no)"
yn "6: the resource URL is the absolute jar: URL with no ./ segment" \
   "$(printf '%s\n' "$out6" | grep -qx "url jar:file:$tmp/proj6/lib.jar!/pathjar/res.edn" && echo yes || echo no)"

# 7. the pom.xml a jar packages under META-INF is consulted for its
# dependencies when there is no .pom beside it — a :local/root jar, or a Maven
# jar whose .pom could not be fetched. The jar's pom.xml names a second
# artifact in the offline repository, which must resolve and load; a pom.xml
# naming an artifact no repository has must fail the resolution, not drop it.
pom frompom
printf '(ns frompom.core)\n(def w 3)\n' > "$tmp/frompom.clj"
JOLT_PWD="$tmp" JOLT_QUIET=1 "$JOLT" run "$root/tools/mkjar.clj" "$m2/org/example/frompom/1.0.0/frompom-1.0.0.jar" "frompom/core.clj=$tmp/frompom.clj" >/dev/null \
  || { echo "  FAIL: 7: mkjar did not write frompom" >&2; fail=$((fail+1)); }
printf '<project><modelVersion>4.0.0</modelVersion><groupId>local</groupId><artifactId>withpom</artifactId><version>1</version><dependencies><dependency><groupId>org.example</groupId><artifactId>frompom</artifactId><version>1.0.0</version></dependency></dependencies></project>\n' > "$tmp/withpom.xml"
printf '(ns withpom.core (:require [frompom.core :as f]))\n(def total (+ f/w 4))\n' > "$tmp/withpom.clj"
mkdir -p "$tmp/proj7/src/app7"
JOLT_PWD="$tmp" JOLT_QUIET=1 "$JOLT" run "$root/tools/mkjar.clj" "$tmp/proj7/withpom.jar" "withpom/core.clj=$tmp/withpom.clj" "META-INF/maven/local/withpom/pom.xml=$tmp/withpom.xml" >/dev/null \
  || { echo "  FAIL: 7: mkjar did not write withpom" >&2; fail=$((fail+1)); }
printf '{:paths ["src"] :deps {local/withpom {:local/root "withpom.jar"}}}\n' > "$tmp/proj7/deps.edn"
printf '(ns app7.core (:require [withpom.core :as w]))\n(defn -main [& _] (println "withpom" w/total))\n' > "$tmp/proj7/src/app7/core.clj"
out7="$(PATH="$empty" JOLT_PWD="$tmp/proj7" JOLT_QUIET=1 JOLT_MAVEN_REPOSITORY="$m2" "$JOLT" run -m app7.core 2>&1)"
yn "7: a dependency declared in the jar's own pom.xml resolves and loads" \
   "$(printf '%s\n' "$out7" | grep -qx 'withpom 7' && echo yes || echo no)"
printf '<project><modelVersion>4.0.0</modelVersion><groupId>local</groupId><artifactId>nopom</artifactId><version>1</version><dependencies><dependency><groupId>org.example</groupId><artifactId>does-not-exist</artifactId><version>9.9.9</version></dependency></dependencies></project>\n' > "$tmp/nopom.xml"
mkdir -p "$tmp/proj7b/src/app7b"
JOLT_PWD="$tmp" JOLT_QUIET=1 "$JOLT" run "$root/tools/mkjar.clj" "$tmp/proj7b/nopom.jar" "withpom/core.clj=$tmp/frompom.clj" "META-INF/maven/local/nopom/pom.xml=$tmp/nopom.xml" >/dev/null \
  || { echo "  FAIL: 7: mkjar did not write nopom" >&2; fail=$((fail+1)); }
printf '{:paths ["src"] :deps {local/nopom {:local/root "nopom.jar"}}}\n' > "$tmp/proj7b/deps.edn"
printf '(ns app7b.core)\n(defn -main [& _] (println "ran"))\n' > "$tmp/proj7b/src/app7b/core.clj"
out7b="$(PATH="$empty" JOLT_PWD="$tmp/proj7b" JOLT_QUIET=1 JOLT_MAVEN_REPOSITORY="$m2" "$JOLT" run -m app7b.core 2>&1)"
yn "7: a dependency the jar's pom.xml declares that no repository has fails the resolution" \
   "$(printf '%s' "$out7b" | grep -q 'does-not-exist' && echo yes || echo no)"
yn "7: the program did not run without it" \
   "$(printf '%s\n' "$out7b" | grep -qx 'ran' && echo no || echo yes)"

# 8. a jar root's resources answer as the JDK's class loader answers them: a
# directory to its name without the "/" (JarFile.getJarEntry's lookup), a
# JarURLConnection from openConnection, a URI from toURI, and a relative URL
# resolved against the entry with its ".." segments gone.
mkdir -p "$tmp/proj8/src/app8"
: > "$tmp/empty8"
printf 'one\n' > "$tmp/one8.sql"
printf 'top\n' > "$tmp/top8.txt"
JOLT_PWD="$tmp" JOLT_QUIET=1 "$JOLT" run "$root/tools/mkjar.clj" "$tmp/proj8/res8.jar" \
  "mig8/=$tmp/empty8" "mig8/001.sql=$tmp/one8.sql" "top8.txt=$tmp/top8.txt" >/dev/null \
  || { echo "  FAIL: 8: mkjar did not write the jar" >&2; fail=$((fail+1)); }
printf '{:paths ["src"] :deps {local/res8 {:local/root "res8.jar"}}}\n' > "$tmp/proj8/deps.edn"
cat > "$tmp/proj8/src/app8/core.clj" <<'EOF3'
(ns app8.core (:require [clojure.java.io :as io]))
(defn -main [& _]
  (println "dir" (str (io/resource "mig8")))
  (let [c (.openConnection (io/resource "mig8/001.sql"))]
    (println "conn" (instance? java.net.JarURLConnection c) (.getEntryName c)
             (pr-str (slurp (.getInputStream c))) (.getName (.getJarEntry c))))
  (println "dirconn" (.getEntryName (.openConnection (io/resource "mig8/"))))
  (println "uri" (str (.toURI (io/resource "top8.txt"))))
  (println "rel" (pr-str (slurp (java.net.URL. (io/resource "mig8/001.sql") "../top8.txt")))))
EOF3
out8="$(PATH="$empty" JOLT_PWD="$tmp/proj8" JOLT_QUIET=1 "$JOLT" run -m app8.core 2>&1)"
yn "8: a directory resource answers to its name without the slash" \
   "$(printf '%s\n' "$out8" | grep -qx "dir jar:file:$tmp/proj8/res8.jar!/mig8" && echo yes || echo no)"
yn "8: openConnection is a JarURLConnection onto the entry" \
   "$(printf '%s\n' "$out8" | grep -qx 'conn true mig8/001.sql "one\\n" mig8/001.sql' && echo yes || echo no)"
yn "8: a directory's connection names the directory entry" \
   "$(printf '%s\n' "$out8" | grep -qx 'dirconn mig8/' && echo yes || echo no)"
yn "8: toURI is the URL's text as a URI" \
   "$(printf '%s\n' "$out8" | grep -qx "uri jar:file:$tmp/proj8/res8.jar!/top8.txt" && echo yes || echo no)"
yn "8: a relative URL resolves against the entry" \
   "$(printf '%s\n' "$out8" | grep -qx 'rel "top\\n"' && echo yes || echo no)"

# 9. resource URLs are percent-encoded as the JDK's class loader encodes them
# (ParseUtil.encodePath): the root's path and the name, from a directory root and
# from a jar root, and they still read, open as a File, and resolve relative URLs.
mkdir -p "$tmp/proj9/r é#;=/s p"
printf 'in dir\n' > "$tmp/proj9/r é#;=/s p/a b#;=é.txt"
printf 'top\n' > "$tmp/proj9/r é#;=/top.txt"
printf 'in jar\n' > "$tmp/jar9.txt"
JOLT_PWD="$tmp" JOLT_QUIET=1 "$JOLT" run "$root/tools/mkjar.clj" "$tmp/proj9/l b.jar" "j p/x é.txt=$tmp/jar9.txt" >/dev/null \
  || { echo "  FAIL: 9: mkjar did not write the jar" >&2; fail=$((fail+1)); }
printf '{:paths ["r é#;=" "l b.jar"]}\n' > "$tmp/proj9/deps.edn"
cat > "$tmp/run9.clj" <<'EOF4'
(require '[clojure.java.io :as io])
(let [d (io/resource "s p/a b#;=é.txt") j (io/resource "j p/x é.txt")]
  (println "dir" (subs (str d) (.lastIndexOf (str d) "/r%20")))
  (println "jar" (subs (str j) (.lastIndexOf (str j) "/l%20b.jar")))
  (println "read" (pr-str (slurp d)) (pr-str (slurp j)))
  (println "file" (.exists (io/file d)) (.getName (io/file d)))
  (println "rel" (pr-str (slurp (java.net.URL. d "../top.txt")))))
EOF4
out9="$(cd "$tmp/proj9" && PATH="$empty" JOLT_PWD="$tmp/proj9" JOLT_QUIET=1 "$JOLT" "$tmp/run9.clj" 2>&1)"
yn "9: a directory root's resource URL is encoded" \
   "$(printf '%s\n' "$out9" | grep -qx 'dir /r%20%c3%a9%23%3b%3d/s%20p/a%20b%23%3b%3d%c3%a9.txt' && echo yes || echo no)"
yn "9: a jar root's resource URL is encoded" \
   "$(printf '%s\n' "$out9" | grep -qx 'jar /l%20b.jar!/j%20p/x%20%c3%a9.txt' && echo yes || echo no)"
yn "9: both read" \
   "$(printf '%s\n' "$out9" | grep -qx 'read "in dir\\n" "in jar\\n"' && echo yes || echo no)"
yn "9: io/file decodes the URL" \
   "$(printf '%s\n' "$out9" | grep -qx 'file true a b#;=é.txt' && echo yes || echo no)"
yn "9: a relative URL resolves against the encoded one" \
   "$(printf '%s\n' "$out9" | grep -qx 'rel "top\\n"' && echo yes || echo no)"

echo "deps-no-unzip-smoke: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
