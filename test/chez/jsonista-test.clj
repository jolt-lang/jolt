;; jsonista.core (compatibility/), over clojure.data.json (#1683). These were
;; unit.edn rows; they live in a .clj file so the code they call is code a
;; reader of the repository (jolt-metrics, jolt-lsp) can see calling it.
;; Each expected string is jsonista 0.3.8's own answer on JVM Clojure 1.12,
;; printed, except the two rows on Jackson's options and modules, which have no
;; JVM counterpart to match. As in the corpus, the printed value is compared, so
;; hash order and number formats count.
(ns jsonista-test
  (:require [jsonista.core :as j]))

(def failures (atom 0))

(defn check [label got want]
  (let [printed (pr-str got)]
    (when-not (= printed want)
      (swap! failures inc)
      (println "jsonista FAIL:" label "\n  want" want "\n  got " printed))))

(check "write-value-as-string writes a keyword key or value with its namespace, a ratio as a double and NaN as a string"
       (j/write-value-as-string {:a/b [:c/d (quote e/f) \x 1/4 Double/NaN nil true 1.5 2N]})
       "\"{\\\"a/b\\\":[\\\"c/d\\\",\\\"e/f\\\",\\\"x\\\",0.25,\\\"NaN\\\",null,true,1.5,2]}\"")

(check "collections, sets, java.util collections and Java arrays are JSON arrays; a byte array is base64"
       (j/write-value-as-string [#{1} (list 2) (java.util.ArrayList. [3]) (java.util.HashMap. {"k" 4}) (int-array [5 6]) (.getBytes "hi")])
       "\"[[1],[2],[3],{\\\"k\\\":4},[5,6],\\\"aGk=\\\"]\"")

(check "a java.util.Date is UTC in the default format or :date-format, and an Instant is ISO"
       [(j/write-value-as-string (java.util.Date. 0)) (j/write-value-as-string (java.util.Date. 0) (j/object-mapper {:date-format "yyyy-MM-dd"})) (j/write-value-as-string (java.time.Instant/ofEpochSecond 0))]
       "[\"\\\"1970-01-01T00:00:00Z\\\"\" \"\\\"1970-01-01\\\"\" \"\\\"1970-01-01T00:00:00Z\\\"\"]")

(check "strings are escaped as Jackson does, and :escape-non-ascii escapes past U+007F as UTF-16 units"
       [(j/write-value-as-string "a\"b\\c\n\t\u0001/\u00e9") (j/write-value-as-string "\u00e9\uD83D\uDE00" (j/object-mapper {:escape-non-ascii true}))]
       "[\"\\\"a\\\\\\\"b\\\\\\\\c\\\\n\\\\t\\\\u0001/é\\\"\" \"\\\"\\\\u00E9\\\\uD83D\\\\uDE00\\\"\"]")

(check ":pretty is Jackson's default pretty printer"
       (j/write-value-as-string {:a [1 {:b 2}] :c {} :d []} (j/object-mapper {:pretty true}))
       "\"{\\n  \\\"a\\\" : [ 1, {\\n    \\\"b\\\" : 2\\n  } ],\\n  \\\"c\\\" : { },\\n  \\\"d\\\" : [ ]\\n}\"")

(check ":strip-nils leaves out nil and empty values, :order-by-keys sorts the keys"
       [(j/write-value-as-string {:a nil :b [] :c "" :d {} :e 0 :f false :g [nil]} (j/object-mapper {:strip-nils true})) (j/write-value-as-string {:b 1 :a 2 :c {:z 1 :y 2}} (j/object-mapper {:order-by-keys true}))]
       "[\"{\\\"e\\\":0,\\\"f\\\":false,\\\"g\\\":[null]}\" \"{\\\"a\\\":2,\\\"b\\\":1,\\\"c\\\":{\\\"y\\\":2,\\\"z\\\":1}}\"]")

(check ":strip-nils drops a nested map whose every value is stripped, as Jackson's NON_EMPTY asks the map"
       (let [m (j/object-mapper {:strip-nils true})] [(j/write-value-as-string {:a {:b nil} :c {:d {:e ""}} :f [{:g nil}] :h {:i [] :j 1}} m) (j/write-value-as-string {:a (doto (java.util.HashMap.) (.put "x" nil))} m)])
       "[\"{\\\"f\\\":[{}],\\\"h\\\":{\\\"j\\\":1}}\" \"{}\"]")

(defrecord JsnR [a b])

(defrecord JsnQ [alpha beta gamma delta epsilon zeta])

(check "a record is written in its entrySet's order, as Jackson walks it, not field order"
       [(j/write-value-as-string (->JsnR 1 2)) (j/write-value-as-string (assoc (->JsnR 1 2) :zz 3 :c 4)) (j/write-value-as-string (->JsnQ 1 2 3 4 5 6))]
       "[\"{\\\"b\\\":2,\\\"a\\\":1}\" \"{\\\"zz\\\":3,\\\"b\\\":2,\\\"a\\\":1,\\\"c\\\":4}\" \"{\\\"epsilon\\\":5,\\\"gamma\\\":3,\\\"alpha\\\":1,\\\"beta\\\":2,\\\"zeta\\\":6,\\\"delta\\\":4}\"]")

(check "a nil map key throws an IOException, as Jackson's does"
       [(try (j/write-value-as-string {nil 1}) (catch java.io.IOException e (ex-message e))) (try (j/write-value-as-string {:a 1 "b" 2} (j/object-mapper {:order-by-keys true})) (catch java.io.IOException e :io))]
       "[\"Null key for a Map not allowed in JSON\" :io]")

(check ":encode-key-fn false keeps a keyword key's colon, a fn is applied to keyword keys only"
       [(j/write-value-as-string {:a 1} (j/object-mapper {:encode-key-fn false})) (j/write-value-as-string {:a 1 "s" 2} (j/object-mapper {:encode-key-fn (fn [k] (str "x-" (name k)))}))]
       "[\"{\\\":a\\\":1}\" \"{\\\"x-a\\\":1,\\\"s\\\":2}\"]")

(check "read-value gives string keys, vectors and hash maps; :decode-key-fn true or a fn sets the keys"
       [(j/read-value "{\"a\":1,\"b\":[1,2,{\"c\":null}],\"d\":true,\"e\":\"x\"}") (j/read-value "{\"a\":{\"b/c\":1}}" j/keyword-keys-object-mapper) (j/read-value "{\"a\":1}" (j/object-mapper {:decode-key-fn (fn [k] (str k "!"))}))]
       "[{\"d\" true, \"e\" \"x\", \"a\" 1, \"b\" [1 2 {\"c\" nil}]} {:a #:b{:c 1}} {\"a!\" 1}]")

(check "numbers read as longs, doubles and big integers, or with :bigdecimals as BigDecimals"
       (let [v (j/read-value "[1,1.5,1e3,12345678901234567890,-0.0]")] [(assoc v 3 :big) (= 12345678901234567890 (nth v 3)) (j/read-value "[1,1.5,1e3]" (j/object-mapper {:bigdecimals true}))])
       "[[1 1.5 1000.0 :big -0.0] true [1 1.5M 1E+3M]]")

(check "read-value reads the first value, from a string, bytes, an InputStream or a Reader; nil is nil"
       [(j/read-value "1 2") (j/read-value (.getBytes "{\"a\":\"\u00e9\"}" "UTF-8")) (j/read-value (java.io.ByteArrayInputStream. (.getBytes "[1]")) j/keyword-keys-object-mapper) (j/read-value (java.io.StringReader. "{\"a\":[1]}") j/keyword-keys-object-mapper) (j/read-value nil)]
       "[1 {\"a\" \"é\"} [1] {:a [1]} nil]")

(check "write-value writes UTF-8 to an OutputStream or Writer and closes it, as Jackson does"
       (let [closed (atom []) o (proxy [java.io.ByteArrayOutputStream] [] (close [] (swap! closed conj :o))) w (proxy [java.io.StringWriter] [] (close [] (swap! closed conj :w)))] (j/write-value o [1 "\u00e9"]) (j/write-value w {:a 1}) [(.toString o "UTF-8") (.toString w) @closed (String. (j/write-value-as-bytes {:a "\u00e9"}) "UTF-8")])
       "[\"[1,\\\"é\\\"]\" \"{\\\"a\\\":1}\" [:o :w] \"{\\\"a\\\":\\\"é\\\"}\"]")

(check "a mapper is an ObjectMapper, as muuntaja tests it"
       [(instance? com.fasterxml.jackson.databind.ObjectMapper (j/object-mapper)) (instance? com.fasterxml.jackson.databind.ObjectMapper j/default-object-mapper) (instance? com.fasterxml.jackson.databind.ObjectMapper {})]
       "[true true false]")

(check "parse errors and unencodable values throw IOExceptions, as Jackson's exceptions are"
       [(try (j/read-value "{\"a\":}") (catch java.io.IOException e :io)) (try (j/read-value "") (catch java.io.IOException e :io)) (try (j/write-value-as-string (Object.)) (catch java.io.IOException e :io))]
       "[:io :io :io]")

(check "Jackson's own options throw rather than be ignored"
       (mapv (fn [o] (try (j/object-mapper o) :accepted (catch UnsupportedOperationException e (ex-message e)))) [{:factory 1} {:encoders {String (fn [x g])}} {:modules [1]} {:mapper 1}])
       "[\"jsonista.core on jolt has no Jackson: :factory is not supported\" \"jsonista.core on jolt has no Jackson: :encoders is not supported\" \"jsonista.core on jolt has no Jackson: the module 1 is not supported\" \"jsonista.core on jolt has no Jackson: a :mapper that is a java.lang.Long is not supported\"]")

(check "java-collection-module decodes to java.util.HashMap and ArrayList; :mapper starts from another mapper"
       (let [m (j/object-mapper {:modules [(j/java-collection-module)] :decode-key-fn true}) v (j/read-value "{\"a\":[1]}" m)] [(class v) (class (get v :a)) (j/read-value "{\"a\":1}" (j/object-mapper {:mapper j/keyword-keys-object-mapper}))])
       "[java.util.HashMap java.util.ArrayList {:a 1}]")

(if (zero? @failures)
  (println "JSONISTA OK")
  (println "JSONISTA FAIL:" @failures "failure(s)"))
