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
