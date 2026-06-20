# Phase 0b — extract the spec corpus into a host-neutral contract file.
#
# Parses every test/spec/*.janet as DATA (no eval), pulls each
# (defspec "suite" [label expected actual] ...) triple, and writes a corpus that
# is valid BOTH as EDN (a future Chez-jolt runner reads it) and as Janet data
# (the current runner reads it via `parse`). Run from repo root:
#   janet test/chez/extract-corpus.janet
(use ../../src/jolt/reader)   # not needed for parse, but keeps paths obvious

(defn parse-all [src]
  (def p (parser/new))
  (parser/consume p src)
  (parser/eof p)
  (def out @[])
  (while (parser/has-more p) (array/push out (parser/produce p)))
  out)

(defn edn-str [s]
  # escape a Clojure-source string into an EDN/Janet string literal
  (def b @`"`)
  (each c s
    (cond
      (= c (chr `"`)) (buffer/push b `\"`)
      (= c (chr "\\")) (buffer/push b "\\\\")
      (= c (chr "\n")) (buffer/push b "\\n")
      (= c (chr "\t")) (buffer/push b "\\t")
      (buffer/push-byte b c)))
  (buffer/push b `"`)
  (string b))

(defn triples-from [form]
  # form is (defspec "suite" [l e a] ...) as a parsed tuple
  (when (and (indexed? form) (> (length form) 0) (= (first form) 'defspec))
    (def suite (in form 1))
    (def rows @[])
    (each case (slice form 2)
      (when (and (indexed? case) (>= (length case) 3))
        (def label (in case 0))
        (def expected (in case 1))
        (def actual (in case 2))
        # expected is either a Clojure-source string or the :throws keyword;
        # actual is always a Clojure-source string. Skip non-literal rows.
        (when (and (string? label) (string? actual)
                   (or (string? expected) (keyword? expected)))
          (array/push rows {:suite suite :label label
                            :expected expected :actual actual})))
      )
    rows))

(def spec-dir "test/spec")
(def all @[])
(each f (sort (os/dir spec-dir))
  (when (string/has-suffix? "-spec.janet" f)
    (def path (string spec-dir "/" f))
    (each form (parse-all (slurp path))
      (when-let [rows (triples-from form)]
        (array/concat all rows)))))

# --- fold in the inline conformance cases (jolt-ohtd) -------------------------
# test/integration/conformance-test.janet carries ~355 hand-written cases as a
# (def cases [["label" "expected" "actual"] ...]) vector — historically invisible to
# the corpus because extract only reads test/spec/. Pull them in too, deduped by
# :actual against the spec rows, so the host-neutral corpus is the union of both.
# Suites come from the file's `### ---- section ----` headers via a line scan (the
# parser drops comments, so section structure is recovered from raw text).
(def conf-path "test/integration/conformance-test.janet")

(defn- section-map [src]
  # label-string -> section-name, by scanning raw lines: track the most recent
  # `### ---- NAME ----` / `### ==== NAME ====` header above each `["label" ...]`.
  (def out @{})
  (var section "misc")
  (each line (string/split "\n" src)
    (def tl (string/trim line))
    (cond
      (string/has-prefix? "###" tl)
      (let [body (string/trim (string/trim (string/trim tl "#") " ") "-= ")]
        (when (> (length body) 0) (set section body)))
      (string/has-prefix? "[\"" tl)
      (let [close (string/find "\"" tl 2)]
        (when close (put out (string/slice tl 2 close) section)))))
  out)

(def conf-src (slurp conf-path))
(def sections (section-map conf-src))
(def seen-actual (tabseq [r :in all] (r :actual) true))
(var conf-added 0)
(each form (parse-all conf-src)
  (when (and (indexed? form) (>= (length form) 3)
             (= (first form) 'def) (= (in form 1) 'cases))
    (each c (in form 2)
      (when (and (indexed? c) (= 3 (length c))
                 (string? (in c 0)) (string? (in c 1)) (string? (in c 2)))
        (def [label expected actual] [(in c 0) (in c 1) (in c 2)])
        (unless (seen-actual actual)
          (put seen-actual actual true)
          (++ conf-added)
          (array/push all {:suite (string "conformance / " (get sections label "misc"))
                           :label label :expected expected :actual actual}))))))
(printf "folded %d unique conformance cases (deduped by :actual)" conf-added)

# emit EDN-and-Janet-valid corpus
(def out @"[\n")
(each row all
  (buffer/push out
    (string "  {:suite " (edn-str (row :suite))
            " :label " (edn-str (row :label))
            " :expected " (if (keyword? (row :expected)) ":throws" (edn-str (row :expected)))
            " :actual " (edn-str (row :actual)) "}\n")))
(buffer/push out "]\n")
(spit "test/chez/corpus.edn" out)
(printf "extracted %d cases from %s into test/chez/corpus.edn" (length all) spec-dir)
