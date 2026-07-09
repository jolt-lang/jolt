;; jolt.fs gate — exercises the file-system API against a scratch temp dir.
;; Run: bin/joltc run test/chez/fs-test.clj (smoke.sh greps for "FS-TEST OK").
(ns fs-test
  (:require [jolt.fs :as fs]
            [clojure.string :as str]))

(def failures (atom []))
(defn check [label got want]
  (when-not (= got want)
    (swap! failures conj (str label ": want " (pr-str want) " got " (pr-str got)))))

(def root (fs/create-temp-dir {:prefix "fs-gate-"}))

;; file coercion + nesting
(check "file nests" (fs/file-name (fs/file root "a" "b.txt")) "b.txt")

;; creation
(fs/create-dirs (fs/file root "d1" "d2"))
(check "create-dirs" (fs/directory? (fs/file root "d1" "d2")) true)
(fs/create-dir (fs/file root "d3"))
(check "create-dir" (fs/directory? (fs/file root "d3")) true)
(fs/create-file (fs/file root "d3" "empty.txt"))
(check "create-file" (fs/regular-file? (fs/file root "d3" "empty.txt")) true)

;; content + size
(spit (str (fs/file root "d1" "one.clj")) "12345")
(spit (str (fs/file root "d1" "d2" "two.clj")) "abc")
(spit (str (fs/file root "d1" "three.txt")) "x")
(check "size" (fs/size (fs/file root "d1" "one.clj")) 5)
(check "exists?" (fs/exists? (fs/file root "d1" "one.clj")) true)
(check "exists? neg" (fs/exists? (fs/file root "nope")) false)

;; path pieces
(check "file-name" (fs/file-name (fs/file root "d1" "one.clj")) "one.clj")
(check "extension" (fs/extension "a/b/one.clj") "clj")
(check "extension none" (fs/extension "a/b/Makefile") nil)
(check "strip-ext" (fs/strip-ext "one.clj") "one")
(check "parent" (fs/file-name (fs/parent (fs/file root "d1" "one.clj"))) "d1")
(check "relativize" (str (fs/relativize root (fs/file root "d1" "one.clj")))
       (str "d1" java.io.File/separator "one.clj"))
(check "absolute?" (fs/absolute? root) true)
(check "relative?" (fs/relative? "a/b") true)

;; listing + glob
(check "list-dir count" (count (fs/list-dir (fs/file root "d1"))) 3)
(check "walk includes root" (first (fs/walk root)) (fs/file root))
(check "glob *" (mapv fs/file-name (sort-by str (fs/glob (fs/file root "d1") "*.clj")))
       ["one.clj"])
(check "glob **" (mapv fs/file-name (sort-by str (fs/glob root "**.clj")))
       ["two.clj" "one.clj"])
(check "glob ?" (mapv fs/file-name (fs/glob (fs/file root "d1") "one.cl?"))
       ["one.clj"])
(check "glob alt" (count (fs/glob (fs/file root "d1") "*.{clj,txt}")) 2)
(check "glob none" (fs/glob root "*.nope") ())

;; copy / copy-tree / move
(fs/copy (fs/file root "d1" "one.clj") (fs/file root "d3" "one-copy.clj"))
(check "copy" (fs/size (fs/file root "d3" "one-copy.clj")) 5)
(check "copy no-replace throws"
       (try (fs/copy (fs/file root "d1" "one.clj") (fs/file root "d3" "one-copy.clj"))
            :no-throw (catch Exception _ :threw))
       :threw)
(fs/copy (fs/file root "d1" "three.txt") (fs/file root "d3" "one-copy.clj")
         {:replace-existing true})
(check "copy replace" (fs/size (fs/file root "d3" "one-copy.clj")) 1)
(fs/copy-tree (fs/file root "d1") (fs/file root "d1-copy"))
(check "copy-tree nested" (slurp (str (fs/file root "d1-copy" "d2" "two.clj"))) "abc")
(fs/move (fs/file root "d1-copy") (fs/file root "d1-moved"))
(check "move" (fs/exists? (fs/file root "d1-moved" "one.clj")) true)
(check "move gone" (fs/exists? (fs/file root "d1-copy")) false)

;; times
(def t0 (java.time.Instant/ofEpochMilli 1600000000000))
(fs/set-last-modified-time (fs/file root "d1" "one.clj") t0)
(check "mtime round-trip" (fs/last-modified-time (fs/file root "d1" "one.clj")) t0)

;; which / cwd
(check "which sh" (some? (fs/which "sh")) true)
(check "which nonsense" (fs/which "no-such-binary-xyz") nil)
(check "cwd is dir" (fs/directory? (fs/cwd)) true)

;; unsupported ops throw cleanly
(check "sym-link? throws"
       (try (fs/sym-link? root) :no-throw (catch Exception _ :threw)) :threw)

;; deletion
(check "delete-if-exists" (fs/delete-if-exists (fs/file root "d3" "empty.txt")) true)
(check "delete-if-exists neg" (fs/delete-if-exists (fs/file root "d3" "empty.txt")) false)
(check "delete missing throws"
       (try (fs/delete (fs/file root "nope")) :no-throw (catch Exception _ :threw)) :threw)
(fs/delete-tree root)
(check "delete-tree" (fs/exists? root) false)

(if (empty? @failures)
  (println "FS-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "FS-TEST FAILED:" (count @failures))))
