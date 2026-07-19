;; jolt.process gate — exercises the public sub-process API against real programs.
;; Run: bin/joltc run test/chez/process-test.clj (smoke.sh greps for "PROCESS-TEST OK").
(ns process-test
  (:require [jolt.process :as p :refer [process sh check pipeline]]
            [jolt.fs :as fs]
            [clojure.string :as str]))

(def failures (atom []))
(defn check-eq [label got want]
  (when-not (= got want)
    (swap! failures conj (str label ": want " (pr-str want) " got " (pr-str got)))))

;; tokenize (pure)
(check-eq "tokenize" (p/tokenize "a  b 'c d'") ["a" "b" "c d"])
(check-eq "tokenize empty" (p/tokenize "") [])

;; capture stdout / exit codes
(check-eq "sh out" (:out (sh ["echo" "hello"])) "hello\n")
(check-eq "sh exit 0" (:exit (sh ["true"])) 0)
(check-eq "sh exit 1" (:exit (sh ["false"])) 1)
(check-eq "exit code passthrough" (:exit (sh ["sh" "-c" "exit 7"])) 7)

;; args are literal (no shell splitting/globbing of an argument)
(check-eq "literal arg" (:out (sh ["echo" "a b  c"])) "a b  c\n")

;; stderr: capture, and merge into stdout
(check-eq "err capture" (:err (sh ["sh" "-c" "echo boom 1>&2"] {:err :string})) "boom\n")
(check-eq "err->out" (:out (sh ["sh" "-c" "echo e 1>&2"] {:err :out})) "e\n")

;; stdin: feed a string
(check-eq "in string" (:out (sh ["cat"] {:in "line1\nline2\n"})) "line1\nline2\n")

;; :dir and :env / :extra-env
(check-eq "dir" (:out (sh ["pwd"] {:dir "/tmp"})) "/tmp\n")
(check-eq "env replace" (:out (sh ["sh" "-c" "echo $JP_VAR"] {:env {"JP_VAR" "set"}})) "set\n")
(check-eq "extra-env keeps PATH" (:out (sh ["sh" "-c" "echo $JP_X"] {:extra-env {"JP_X" "y"}})) "y\n")

;; check throws on non-zero, returns the derefed process on success
(check-eq "check ok exit" (:exit (check (process ["true"]))) 0)
(check-eq "check throws"
          (try (check (process ["false"])) :no-throw (catch Exception _ :threw)) :threw)

;; pipelines via threading and via pipeline
(check-eq "pipe ->" (-> (process ["printf" "a\nb\nc\n"]) (process ["grep" "b"]) :out slurp) "b\n")
(check-eq "pipeline count" (count (pipeline (-> (process ["echo" "x"]) (process ["cat"])))) 2)

;; process record deref carries :out/:exit
(let [res @(process ["echo" "derefed"] {:out :string})]
  (check-eq "deref out" (:out res) "derefed\n")
  (check-eq "deref exit" (:exit res) 0))

;; :out to a file
(let [tmp (str (fs/create-temp-file {:prefix "jp-" :suffix ".txt"}))]
  @(process ["echo" "to-file"] {:out tmp})
  (check-eq "out->file" (slurp tmp) "to-file\n")
  (fs/delete-if-exists tmp))

;; alive? / destroy / signal exit code
(let [proc (process ["sleep" "10"])]
  (check-eq "alive?" (p/alive? proc) true)
  (p/destroy proc)
  (check-eq "sigterm exit" (:exit @proc) 143)
  (check-eq "dead after destroy" (p/alive? proc) false))

;; a spawned child inherits the user's cwd (user.dir / JOLT_PWD), not jolt's OS cwd
;; (the launcher cd's to the repo root but preserves the user's cwd in JOLT_PWD)
(check-eq "child cwd = user.dir"
          (str (fs/canonicalize (str/trim (:out (sh ["pwd"])))))
          (str (fs/canonicalize (System/getProperty "user.dir"))))
;; an explicit :dir sets the child's cwd (pwd echoes the logical cd path)
(let [sub (fs/create-temp-dir {:prefix "jp-dir-"})]
  (check-eq "dir set" (str/trim (:out (sh ["pwd"] {:dir (str sub)}))) (str sub))
  (fs/delete-tree sub))

;; ProcessBuilder.start throws (like the JVM) when the program can't be resolved,
;; with a "No such file" message — not a shell "not found" after spawning
(check-eq "missing program throws"
          (try (sh ["definitely-no-such-program-xyz"]) :no-throw
               (catch Exception e (if (re-find #"No such file" (str (ex-message e))) :nosuch :other)))
          :nosuch)

;; class / instance? derive from the central registry
(check-eq "pb instance?" (instance? java.lang.ProcessBuilder (java.lang.ProcessBuilder. ["true"])) true)
(check-eq "proc class" (.getName (class (:proc @(process ["true"])))) "java.lang.Process")

(if (empty? @failures)
  (println "PROCESS-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "PROCESS-TEST FAILED:" (count @failures))))
