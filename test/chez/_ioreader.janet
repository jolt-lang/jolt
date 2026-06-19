# jolt-at0a (inc W) — reader-coupled io deferred from inc V (jolt-yyud):
# clojure.java.io/reader (a StringReader over slurp/string/char[]/File/path),
# char-array, File.toURL/.toURI (-> a java.net.URL jhost), slurp draining a
# StringReader, and with-open's __close seam over both jhost readers and plain
# :close maps. All Chez-native (host/chez/io.ss); no analyzer change. Reader/edn
# runtime read (clojure.edn/read over a PushbackReader) stays jolt-r8ku.
# Oracle = build/jolt.
#
#   janet test/chez/_ioreader.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/jolt-chez"))

(defn io [body] (string "(do (require (quote [clojure.java.io :as io])) " body ")"))

(def cases
  [# --- char-array ---
   ["(str (char-array \"abc\"))"                                   "(\\a \\b \\c)"]
   ["(.read (StringReader. (apply str (char-array \"Qz\"))))"      "81"]
   # --- io/reader: char[] / string-reader passthrough / path ---
   [(io "(.read (io/reader (char-array \"abc\")))")                "97"]
   [(io "(.read (io/reader (StringReader. \"k\")))")               "107"]
   [(io "(slurp (io/reader (char-array \"xyz\")))")                "xyz"]
   [(io "(string? (slurp (io/reader \"project.janet\")))")         "true"]
   # --- File.toURL / .toURI ---
   [(io "(.toString (.toURL (io/file \"/tmp/x\")))")               "file:/tmp/x"]
   [(io "(.toURI (io/file \"/tmp/x\"))")                           "file:/tmp/x"]
   [(io "(.getPath (.toURL (io/file \"/tmp/x\")))")                "/tmp/x"]
   [(io "(.getAbsolutePath (io/file \"/a/b\"))")                   "/a/b"]
   # --- slurp drains a StringReader (+ ignores :encoding opts) ---
   ["(slurp (StringReader. \"a=1\"))"                              "a=1"]
   ["(slurp (StringReader. \"b\") :encoding \"UTF-8\")"            "b"]
   # --- with-open: jhost reader + plain :close map ---
   ["(with-open [r (StringReader. \"a\")] (.read r))"              "97"]
   ["(let [log (atom [])] (with-open [c {:close (fn [] (swap! log conj :closed))}] :r) (deref log))" "[:closed]"]
   ["(let [log (atom [])] (try (with-open [c {:close (fn [] (swap! log conj :closed))}] (throw (ex-info \"boom\" {}))) (catch Exception e nil)) (deref log))" "[:closed]"]
   ["(let [log (atom [])] (with-open [a {:close (fn [] (swap! log conj :outer))} b {:close (fn [] (swap! log conj :inner))}] :r) (deref log))" "[:inner :outer]"]
   ["(with-open [c {:close (fn [] nil) :v 5}] (:v c))"             "5"]])

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
  (def [ocode oracle _] (run-capture "build/jolt" expr))
  (def [code got err] (run-capture jolt-bin expr))
  (cond
    (not= ocode 0) (array/push fails [expr (string "ORACLE FAILED exit " ocode)])
    (not= oracle expected) (array/push fails [expr (string "ORACLE MISMATCH want `" expected "` got `" oracle "`")])
    (not= code 0) (array/push fails [expr (string "exit " code "; err: " err)])
    (= got expected) (++ pass)
    (array/push fails [expr (string "want `" expected "`, got `" got "`")])))

(printf "\n_ioreader parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
