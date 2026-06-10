# Vendored stdlib-namespace battery (jolt-0mb).
#
# clojure.test suites for stdlib namespaces beyond clojure.core, vendored from
# clojurust's clojure-test-suite fork (test/clojure-stdlib/, with corrected
# fixtures where the upstream expectations disagreed with real Clojure). Each
# file runs in the shared per-file worker; we guard a minimum pass count so a
# regression is caught and improvements (e.g. finishing clojure.edn) can raise
# the floor.

(def files
  # [relative-path  min-pass  must-be-clean?]
  [["clojure/walk_test/walk.cljc"            34 true]
   ["clojure/zip_test/zip.cljc"              33 true]
   ["clojure/data_test/diff.cljc"            61 true]
   # clojure.edn reads via clojure.core/read-string (opts/:eof + nil/blank) and
   # constructs set/nested values. Only #uuid remains (no real uuid type) —
   # jolt-b7y. Guard the passing subset.
   ["clojure/edn_test/read_string.cljc"      50 false]])

(def root "test/clojure-stdlib")
(def per-file-timeout 6)

(defn- run-file [path]
  (def proc (os/spawn ["janet" "test/integration/suite-worker.janet" path] :p {:out :pipe}))
  (def out (proc :out))
  (var data nil)
  (def ok (try
            (ev/with-deadline per-file-timeout
              (set data (ev/read out 0x10000))
              (os/proc-wait proc) true)
            ([err] false)))
  (when (not ok)
    (protect (os/proc-kill proc true))
    (protect (ev/with-deadline 2 (os/proc-wait proc))))
  (protect (:close out))
  (if (and ok data) (string data) nil))

(defn- counts [s]
  (var r nil)
  (each line (string/split "\n" (or s ""))
    (when (string/has-prefix? "@@COUNTS " line)
      (let [p (string/split " " (string/trim line))]
        (when (= 4 (length p)) (set r [(scan-number (p 1)) (scan-number (p 2)) (scan-number (p 3))])))))
  r)

(var failures 0)
(each [rel min-pass clean?] files
  (def path (string root "/" rel))
  (def c (counts (run-file path)))
  (if (nil? c)
    (do (++ failures) (printf "FAIL %s: no result (crash/timeout)" rel))
    (let [[p f e] c]
      (printf "  %-34s pass=%d fail=%d err=%d" rel p f e)
      (when (< p min-pass)
        (++ failures) (printf "FAIL %s: pass %d < baseline %d" rel p min-pass))
      (when (and clean? (or (pos? f) (pos? e)))
        (++ failures) (printf "FAIL %s: expected clean, got %d fail / %d err" rel f e)))))

(if (pos? failures)
  (do (printf "clojure-stdlib-suite: %d failure(s)" failures) (os/exit 1))
  (print "clojure-stdlib-suite: OK"))
