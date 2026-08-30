;; java.io.File path normalization gate — the JVM constructor runs its path
;; through FileSystem.normalize: runs of separators collapse and one trailing
;; separator drops (the root "/" alone keeps its slash); "." and ".." are NOT
;; resolved. clojure.java.io/file layers as-relative-path on the multi-arg
;; forms: each child must be relative or IllegalArgumentException, and the
;; join goes through File(parent, child), which normalizes the seam.
;; (jolt-lang/jolt#793)
;;
;; Run: bin/jolt run test/chez/file-normalize-test.clj (smoke.sh greps for
;; "FILE-NORMALIZE OK").
(ns file-normalize-test
  (:import [java.io File])
  (:require [clojure.java.io :as io]))

(def failures (atom []))
(defn check [label got want]
  (when-not (= got want)
    (swap! failures conj (str label ": want " (pr-str want) " got " (pr-str got)))))

(defn path ^String [f] (.getPath f))

;; --- one-arg constructor: normalize, don't resolve ---------------------------

(check "duplicate separators collapse" (path (File. "/a/b//c")) "/a/b/c")
(check "trailing separator drops" (path (File. "/a/b/")) "/a/b")
(check "several runs collapse" (path (File. "/a//b//c")) "/a/b/c")
(check "leading run collapses" (path (File. "//a/b")) "/a/b")
(check "relative run collapses" (path (File. "a//b")) "a/b")
(check "root stays root" (path (File. "/")) "/")
(check "only separators is root" (path (File. "///")) "/")
(check "empty stays empty" (path (File. "")) "")
(check "dot is not resolved" (path (File. "/a/./b")) "/a/./b")
(check "dotdot is not resolved" (path (File. "/a/b/../c")) "/a/b/../c")
(check "str sees the normalized path" (str (File. "/a//b")) "/a/b")
(check "relative stays relative" (path (File. "rel//x")) "rel/x")

;; --- two-arg constructor: resolve(normalize parent, normalize child) ---------

(check "seam with trailing parent" (path (File. "/a/b/" "c")) "/a/b/c")
(check "parent run collapses" (path (File. "/a//b" "c")) "/a/b/c")
(check "absolute child is joined" (path (File. "/a/b" "/c")) "/a/b/c")
(check "empty child yields parent" (path (File. "/a/b" "")) "/a/b")
(check "root parent takes child" (path (File. "/" "c")) "/c")
(check "separator-only child" (path (File. "/a/b" "///")) "/a/b/")
(check "child trailing separator" (path (File. "/a/b/" "c/")) "/a/b/c")
(check "File parent argument" (path (File. (File. "/a//b") "c")) "/a/b/c")

;; --- clojure.java.io/file ----------------------------------------------------

(check "io/file single arg normalizes" (path (io/file "/a/b//c")) "/a/b/c")
(check "io/file seam" (path (io/file "/a/b/" "c")) "/a/b/c")
(check "io/file reduce over more" (path (io/file "/a/b/" "c" "d")) "/a/b/c/d")
(check "io/file parent run" (path (io/file "/a//b" "c")) "/a/b/c")
(check "io/file relative child run" (path (io/file "/a/b" "c//d")) "/a/b/c/d")
(check "io/file File parent" (path (io/file (File. "/a//b") "c")) "/a/b/c")
(check "io/file File passthrough" (path (io/file (io/file "/a/b//c"))) "/a/b/c")

(check "io/file absolute child throws"
       (try (path (io/file "/a/b" "/c")) :no-throw
            (catch IllegalArgumentException e :threw))
       :threw)
(check "io/file absolute child message"
       (try (path (io/file "/a/b" "//c")) nil
            (catch IllegalArgumentException e (.getMessage e)))
       "/c is not a relative path")
(check "io/file absolute File child throws"
       (try (path (io/file "/a/b" (io/file "/c"))) :no-throw
            (catch IllegalArgumentException _ :threw))
       :threw)
(check "io/file absolute child in more throws"
       (try (path (io/file "/a/b" "c" "/d")) :no-throw
            (catch IllegalArgumentException _ :threw))
       :threw)

;; --- the method surface sees normalized paths --------------------------------

(check "getParentFile of collapsed path" (path (.getParentFile (File. "/a//b/c"))) "/a/b")
(check "getAbsolutePath normalizes the tail" (.endsWith (.getAbsolutePath (File. "a//b")) "/a/b") true)
(check "getName of collapsed path" (.getName (File. "/a//b")) "b")

;; --- createTempFile: the temp dir often ends in a separator ------------------

(def tmp (System/getProperty "java.io.tmpdir"))
(def tmp-file (File/createTempFile "joltnorm" ".tmp" (io/file (str tmp "/"))))
(check "createTempFile joins one separator" (re-find #"//" (path tmp-file)) nil)
(check "createTempFile keeps suffix" (.endsWith (path tmp-file) ".tmp") true)
(.delete tmp-file)

;; --- listFiles children carry the normalized parent --------------------------

(def norm-dir (str tmp "/jolt-norm-" (System/currentTimeMillis)))
(.mkdirs (File. (str norm-dir "//sub")))
(spit (str norm-dir "/sub/f.txt") "x")
(def children (map path (.listFiles (File. (str norm-dir "/sub/")))))
(check "listFiles child has no doubled separator" (some #(re-find #"//" %) children) nil)
(check "listFiles finds the file" (some #(.endsWith % "f.txt") children) true)
(doseq [f [(str norm-dir "/sub/f.txt") (str norm-dir "/sub") norm-dir]]
  (try (.delete (File. f)) (catch Throwable _ nil)))

;; --- report ------------------------------------------------------------------

(if (seq @failures)
  (do (println "FILE-NORMALIZE FAILURES:")
      (doseq [f @failures] (println " " f))
      (System/exit 1))
  (println "FILE-NORMALIZE OK"))
