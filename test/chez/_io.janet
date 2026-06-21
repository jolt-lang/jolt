# jolt-yyud — clojure.java.io/file + java.io.File interop + slurp/spit/flush/
# file-seq on Chez. A File is a path-backed jfile record (instance? java.io.File,
# str -> path, the File method surface). This is a Chez-native implementation.
# Reader-coupled cases (line-seq, slurp over a reader, toURL) are deferred to jolt-at0a.
#
#
#   janet test/chez/_io.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

(defn io [body] (string "(do (require (quote [clojure.java.io :as io])) " body ")"))

(def cases
  [# --- File construction + method surface ---
   [(io "(str (io/file \"/a/b\"))")                          "/a/b"]
   [(io "(str (io/file \"/a\" \"b\"))")                      "/a/b"]
   [(io "(.getName (io/file \"/a/b/c.txt\"))")               "c.txt"]
   [(io "(.getPath (io/file \"/a\" \"b\"))")                 "/a/b"]
   [(io "(.isDirectory (io/file \"docs\"))")                 "true"]
   [(io "(.isFile (io/file \"project.janet\"))")             "true"]
   [(io "(.isFile (io/file \"docs\"))")                      "false"]
   [(io "(.exists (io/file \"/no/such/path/xyz\"))")         "false"]
   [(io "(.exists (io/file \"project.janet\"))")             "true"]
   # --- instance? + type ---
   [(io "(instance? java.io.File (io/file \"/a/b\"))")       "true"]
   [(io "(instance? java.io.File \"/a/b\")")                 "false"]
   [(io "(str (type (io/file \"/a\")))")                     ":jolt/file"]
   # --- file-seq ---
   [(io "(every? (fn [f] (instance? java.io.File f)) (file-seq (io/file \"docs\")))") "true"]
   [(io "(pos? (count (filter (fn [f] (.isFile f)) (file-seq (io/file \"docs\")))))") "true"]
   ["(do (require (quote [clojure.string :as s])) (boolean (some (fn [p] (s/ends-with? p \"project.janet\")) (file-seq \".\"))))" "true"]
   # --- slurp / spit / flush ---
   ["(string? (slurp \"project.janet\"))"                                            "true"]
   ["(do (spit \"/tmp/jolt-io-test.txt\" \"hello\") (slurp \"/tmp/jolt-io-test.txt\"))" "hello"]
   ["(do (spit \"/tmp/jolt-io-test.txt\" \"a\") (spit \"/tmp/jolt-io-test.txt\" \"b\" :append true) (slurp \"/tmp/jolt-io-test.txt\"))" "ab"]
   ["(flush)"                                                                         ""]
   [(io "(string? (slurp (io/file \"project.janet\")))")                              "true"]])

(defn run-capture [bin expr]
  (def proc (os/spawn [bin "-e" expr] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (def err (ev/read (proc :err) 0x100000))
  (def code (os/proc-wait proc))
  (def lines (filter (fn [l] (not (empty? l)))
                     (string/split "\n" (string/trim (if out (string out) "")))))
  [code (if (empty? lines) "" (last lines)) (string/trim (if err (string err) ""))])

(var pass 0)
(def fails @[])
(each [expr expected] cases
  (def [code got err] (run-capture jolt-bin expr))
  (cond
    (not= code 0) (array/push fails [expr (string "exit " code "; err: " err)])
    (= got expected) (++ pass)
    (array/push fails [expr (string "want `" expected "`, got `" got "`")])))

(printf "\n_io parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
