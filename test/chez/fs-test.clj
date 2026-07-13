;; jolt.fs gate — exercises the public file-system API against a scratch temp dir.
;; Run: bin/joltc run test/chez/fs-test.clj (smoke.sh greps for "FS-TEST OK").
(ns fs-test
  (:require [jolt.fs :as fs]))

(def failures (atom []))
(defn check [label got want]
  (when-not (= got want)
    (swap! failures conj (str label ": want " (pr-str want) " got " (pr-str got)))))

(def root (fs/create-temp-dir {:prefix "fs-gate-"}))

;; path nesting
(check "path nests" (fs/file-name (fs/path root "a" "b.txt")) "b.txt")

;; creation
(fs/create-dirs (fs/path root "d1" "d2"))
(check "create-dirs" (fs/directory? (fs/path root "d1" "d2")) true)
(fs/create-dir (fs/path root "d3"))
(check "create-dir" (fs/directory? (fs/path root "d3")) true)
(fs/create-file (fs/path root "d3" "empty.txt"))
(check "create-file" (fs/regular-file? (fs/path root "d3" "empty.txt")) true)

;; content + size
(spit (str (fs/path root "d1" "one.clj")) "12345")
(spit (str (fs/path root "d1" "d2" "two.clj")) "abc")
(spit (str (fs/path root "d1" "three.txt")) "x")
(check "size" (fs/size (fs/path root "d1" "one.clj")) 5)
(check "exists?" (fs/exists? (fs/path root "d1" "one.clj")) true)
(check "exists? neg" (fs/exists? (fs/path root "nope")) false)

;; path pieces
(check "file-name" (fs/file-name (fs/path root "d1" "one.clj")) "one.clj")
(check "extension" (fs/extension "a/b/one.clj") "clj")
(check "extension none" (fs/extension "a/b/Makefile") nil)
(check "strip-ext" (fs/strip-ext "one.clj") "one")
(check "parent" (fs/file-name (fs/parent (fs/path root "d1" "one.clj"))) "d1")
(check "relativize" (str (fs/relativize root (fs/path root "d1" "one.clj"))) "d1/one.clj")
(check "absolute?" (fs/absolute? root) true)
(check "relative?" (fs/relative? "a/b") true)

;; listing + glob (Path results)
(check "list-dir count" (count (fs/list-dir (fs/path root "d1"))) 3)
(check "glob *" (mapv fs/file-name (sort-by str (fs/glob (fs/path root "d1") "*.clj")))
       ["one.clj"])
(check "glob **" (sort (mapv fs/file-name (fs/glob root "**.clj")))
       ["one.clj" "two.clj"])
(check "glob ?" (mapv fs/file-name (fs/glob (fs/path root "d1") "one.cl?"))
       ["one.clj"])
(check "glob alt" (count (fs/glob (fs/path root "d1") "*.{clj,txt}")) 2)
(check "glob none" (seq (fs/glob root "*.nope")) nil)

;; copy / copy-tree / move
(fs/copy (fs/path root "d1" "one.clj") (fs/path root "d3" "one-copy.clj"))
(check "copy" (fs/size (fs/path root "d3" "one-copy.clj")) 5)
(check "copy no-replace throws"
       (try (fs/copy (fs/path root "d1" "one.clj") (fs/path root "d3" "one-copy.clj"))
            :no-throw (catch Exception _ :threw))
       :threw)
(fs/copy (fs/path root "d1" "three.txt") (fs/path root "d3" "one-copy.clj")
         {:replace-existing true})
(check "copy replace" (fs/size (fs/path root "d3" "one-copy.clj")) 1)
(fs/copy-tree (fs/path root "d1") (fs/path root "d1-copy"))
(check "copy-tree nested" (slurp (str (fs/path root "d1-copy" "d2" "two.clj"))) "abc")
(fs/move (fs/path root "d1-copy") (fs/path root "d1-moved"))
(check "move" (fs/exists? (fs/path root "d1-moved" "one.clj")) true)
(check "move gone" (fs/exists? (fs/path root "d1-copy")) false)

;; times (FileTime round-trip)
(fs/set-last-modified-time (fs/path root "d1" "one.clj") (fs/millis->file-time 1600000000000))
(check "mtime round-trip"
       (fs/file-time->millis (fs/last-modified-time (fs/path root "d1" "one.clj")))
       1600000000000)

;; symbolic links (supported through the java.nio.file shim)
(fs/create-sym-link (fs/path root "d3" "link.clj") (fs/path root "d1" "one.clj"))
(check "sym-link?" (fs/sym-link? (fs/path root "d3" "link.clj")) true)
(check "sym-link? neg" (fs/sym-link? (fs/path root "d1" "one.clj")) false)
(check "read-link" (fs/file-name (fs/read-link (fs/path root "d3" "link.clj"))) "one.clj")

;; POSIX permissions
(fs/set-posix-file-permissions (fs/path root "d1" "one.clj") "rw-r--r--")
(check "posix perms" (fs/posix->str (fs/posix-file-permissions (fs/path root "d1" "one.clj")))
       "rw-r--r--")

;; which / cwd
(check "which sh" (some? (fs/which "sh")) true)
(check "which nonsense" (fs/which "no-such-binary-xyz") nil)
(check "cwd is dir" (fs/directory? (fs/cwd)) true)

;; deletion
(check "delete-if-exists" (fs/delete-if-exists (fs/path root "d3" "empty.txt")) true)
(check "delete-if-exists neg" (fs/delete-if-exists (fs/path root "d3" "empty.txt")) false)
(check "delete missing throws"
       (try (fs/delete (fs/path root "nope")) :no-throw (catch Exception _ :threw)) :threw)
(fs/delete-tree root)
(check "delete-tree" (fs/exists? root) false)

(if (empty? @failures)
  (println "FS-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "FS-TEST FAILED:" (count @failures))))
