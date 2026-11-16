;; clojure.pprint dispatch-fn gate: simple-dispatch and code-dispatch must be
;; real multimethods on class so libraries can extend them. The driver is
;; core.logic's nominal namespace, which does
;;   (. clojure.pprint/simple-dispatch addMethod Tie pprint-tie)
;; and dies with "No matching method addMethod found for clojure.pprint$simple
;; _dispatch" while those vars are plain functions.
;;
;; Not a corpus row: it requires loading clojure.pprint and registering methods,
;; which the corpus runner never does. Runs through the real CLI like the
;; instant / cl-format suites; emits PPRINT-DISPATCH OK / FAIL for smoke.sh.
;;
;; The built-in arms are asserted against the exact strings the old plain-fn case
;; form produced (captured before this change). A defrecord still prints as a
;; bare map: pprint routes it through IPersistentMap, as it does on the JVM --
;; pr is the one that prints #ns.R{...}, so changing that here would be a
;; regression, not a fix.
;;
;; Order matters: the addMethod / defmethod tests below MUTATE the global
;; dispatch table (that is what they exist to test). On the unfixed code
;; (defmethod on a plain fn redefines the var as a fresh, near-empty
;; multimethod), so anything that reads simple-dispatch's table must run first.
;; The no-ambiguity check therefore leads, on the pristine dispatch; the
;; mutation tests come last.
(ns pprint-dispatch-gate
  (:require [clojure.pprint :refer [pprint]]))

(def ^:private fails (atom []))
(def ^:private passes (atom 0))

(defn- ok= [got want label]
  (if (= got want)
    (swap! passes inc)
    (swap! fails conj (str label ": want " (pr-str want) " got " (pr-str got)))))

(defn- ok [thunk label]
  (try (thunk) (swap! passes inc)
       (catch Throwable e (swap! fails conj (str label ": threw " (.getMessage e))))))

(defn- out [obj] (with-out-str (pprint obj)))

(defrecord R [a])

;; --- no dispatch ambiguity across the full type space (read first, pristine) ----
;; On the unfixed code simple-dispatch is a plain fn, so this branch records one
;; failure ("is a multimethod" => false) and skips -- the run then fails only on
;; that plus the addMethod/defmethod cases below. On the fixed code it fires and
;; demands get-method resolve to a real method for every type's class; get-method
;; throws "Multiple methods" on any value matching two arms -- the ordering hazard
;; the task warned about (a record is IPersistentMap + IRecord; a map and a vector
;; are both Associative). The method bodies never run, so this is pure dispatch
;; resolution, not a pretty-writer check.
(let [multifn? (instance? clojure.lang.MultiFn clojure.pprint/simple-dispatch)]
  (ok= multifn? true "simple-dispatch is a multimethod")
  (when multifn?
    (doseq [[label obj] [["vector" [1 2]] ["array-map" (array-map :a 1)]
                         ["hash-map" {:b 2}] ["sorted-map" (sorted-map :c 3)]
                         ["hash-set" #{1}] ["sorted-set" (sorted-set 2)]
                         ["queue" (into clojure.lang.PersistentQueue/EMPTY [1])]
                         ["list" (list 1)] ["lazy-seq" (lazy-seq (cons 1 nil))]
                         ["vec-seq" (seq [1])] ["map-seq" (seq {:a 1})]
                         ["record" (->R 1)] ["nil" nil] ["atom" (atom 1)]
                         ["string" "x"] ["keyword" :k]]]
      (ok= (some? (get-method clojure.pprint/simple-dispatch (class obj))) true
           (str "simple-dispatch resolves a " label " without ambiguity"))
      (ok= (some? (get-method clojure.pprint/code-dispatch (class obj))) true
           (str "code-dispatch resolves a " label " without ambiguity")))))

;; --- built-in arms print exactly what the old case form produced ---------------
(ok= (out [1 2 3])      "[1 2 3]\n" "vector arm")
(ok= (out {:a 1})       "{:a 1}\n"  "map arm")
(ok= (out #{1})         "#{1}\n"    "set arm")
(ok= (out (list 1 2 3)) "(1 2 3)\n" "seq arm")
(ok= (out nil)          "nil\n"     "nil arm")
(ok= (out (->R 1))      "{:a 1}\n"  "record prints as a bare map")

;; --- code-dispatch: the Symbol arm honours *print-suppress-namespaces* ---------
;; Read before the mutation tests; it binds *print-pprint-dispatch* to code-dispatch
;; but does not alter the dispatch tables.
(ok= (binding [clojure.pprint/*print-pprint-dispatch* clojure.pprint/code-dispatch
               clojure.pprint/*print-suppress-namespaces* true]
       (out 'foo.bar/baz))
     "baz\n"
     "code-dispatch drops a symbol's namespace under *print-suppress-namespaces*")
(ok= (binding [clojure.pprint/*print-pprint-dispatch* clojure.pprint/code-dispatch]
       (out 'foo.bar/baz))
     "foo.bar/baz\n"
     "code-dispatch keeps a symbol's namespace without suppression")

;; --- code-dispatch: the code table ----------------------------------------------
;; A list whose head names a known form gets that form's layout, as in the
;; reference's *code-table*. Expected strings are Clojure 1.12.0's output.
(defn- code-out [margin form]
  (binding [clojure.pprint/*print-right-margin* margin
            clojure.pprint/*print-pprint-dispatch* clojure.pprint/code-dispatch]
    (out form)))

(ok= (code-out 72 '(defn classify [{:keys [text limit]}]
                     (let [n (or limit 25) words (clojure.string/split text #" ")]
                       (if (> (count words) n) {:severity "high" :count (count words)} {:severity "low"}))))
     (str "(defn classify [{:keys [text limit]}]\n"
          "  (let [n (or limit 25) words (clojure.string/split text #\" \")]\n"
          "    (if (> (count words) n)\n"
          "      {:severity \"high\", :count (count words)}\n"
          "      {:severity \"low\"})))\n")
     "code-dispatch: defn, let and if hold their head arguments")
(ok= (code-out 72 '(fn [xs] (cond (empty? xs) {:error "no items to process here"}
                                  (> (count xs) 10) (take 10 (sort-by :name xs))
                                  :else (map #(assoc % :seen true) xs))))
     (str "(fn [xs]\n"
          "  (cond\n"
          "    (empty? xs) {:error \"no items to process here\"}\n"
          "    (> (count xs) 10) (take 10 (sort-by :name xs))\n"
          "    :else (map #(assoc % :seen true) xs)))\n")
     "code-dispatch: cond pairs its clauses and #() prints as the reader form")
(ok= (code-out 72 '(ns my.app.core "The core namespace of the application here." {:author "someone"}
                     (:require [clojure.string :as str] [clojure.set :refer [union intersection difference]]
                               [my.app.db :as db])
                     (:import (java.util Date UUID) [java.io File])))
     (str "(ns my.app.core\n"
          "  \"The core namespace of the application here.\"\n"
          "  {:author \"someone\"}\n"
          "  (:require [clojure.string :as str]\n"
          "            [clojure.set :refer [union intersection difference]]\n"
          "            [my.app.db :as db])\n"
          "  (:import (java.util Date UUID) [java.io File]))\n")
     "code-dispatch: ns keeps its name on the head line and aligns libspecs")
(ok= (code-out 72 '(condp = x 1 "one one one one one one" 2 "two two two two two two two"
                     "something else entirely here"))
     (str "(condp = x\n"
          "  1 \"one one one one one one\"\n"
          "  2 \"two two two two two two two\"\n"
          "  \"something else entirely here\")\n")
     "code-dispatch: condp pairs its clauses")
(ok= (code-out 72 '(defn- helper "A docstring that is reasonably long for the test."
                     ([a] (helper a 1)) ([a b] (+ a b b b b b b b b b b b b b b b b))))
     (str "(defn- helper\n"
          "  \"A docstring that is reasonably long for the test.\"\n"
          "  ([a] (helper a 1))\n"
          "  ([a b] (+ a b b b b b b b b b b b b b b b b)))\n")
     "code-dispatch: a multi-arity defn- with a docstring")
(ok= (code-out 72 '(map #(+ %1 %2 100000000 200000000 300000000 400000000) first-list-of-things second-list))
     (str "(map\n"
          "  #(+ %1 %2 100000000 200000000 300000000 400000000)\n"
          "  first-list-of-things\n"
          "  second-list)\n")
     "code-dispatch: #() numbers its params past one")
(ok= (code-out 72 '(let [a-very-long-binding-name (compute-something-expensive with-arguments)
                         another-binding-name (other-computation a-very-long-binding-name)]
                     (+ a-very-long-binding-name another-binding-name)))
     (str "(let [a-very-long-binding-name (compute-something-expensive\n"
          "                                 with-arguments)\n"
          "      another-binding-name (other-computation\n"
          "                             a-very-long-binding-name)]\n"
          "  (+ a-very-long-binding-name another-binding-name))\n")
     "code-dispatch: let bindings break in pairs")
(ok= (code-out 72 '(-> request (assoc :user current-user) (update :count inc) (dissoc :password :secret)))
     (str "(-> request\n"
          " (assoc :user current-user)\n"
          " (update :count inc)\n"
          " (dissoc :password :secret))\n")
     "code-dispatch: -> holds its first argument")

;; (cl-format true ...) inside a dispatch fn writes into the active pretty
;; writer, so its pretty directives see the enclosing logical block (pprint-ns
;; does this for a docstring).
(ok= (binding [clojure.pprint/*print-pprint-dispatch*
               (fn [x] (clojure.pprint/pprint-logical-block :prefix "<" :suffix ">"
                         (clojure.pprint/cl-format true "~a~:@_~a" (first x) (second x))))]
       (out [:a :b]))
     "<:a\n :b>\n"
     "cl-format true inside a dispatch fn writes into the pretty writer")

;; --- *print-right-margin* nil: no line is too long (#1680) -------------------
;; With no right margin pprint takes none of the optional newlines (fill, linear,
;; miser), however long the line, and still takes a mandatory one. Expected
;; strings are Clojure 1.12's.
(ok= (binding [clojure.pprint/*print-right-margin* nil]
       (out {:alpha (range 30) :beta {:gamma "a long string value here" :delta (vec (range 1 21))}}))
     "{:alpha (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29), :beta {:gamma \"a long string value here\", :delta [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20]}}\n"
     "*print-right-margin* nil keeps a long map on one line")
(ok= (code-out nil '(defn f "doc" ([x] (let [a 1 b 2] (cond (pos? x) (inc x) :else (dec x)))) ([x y] (+ x y))))
     "(defn f \"doc\" ([x] (let [a 1 b 2] (cond (pos? x) (inc x) :else (dec x)))) ([x y] (+ x y)))\n"
     "*print-right-margin* nil keeps code-dispatch's defn on one line")
(ok= (binding [clojure.pprint/*print-right-margin* nil]
       (clojure.pprint/cl-format nil "~<a~:@_b ~_c~:>" nil))
     "a\nb\nc"
     "*print-right-margin* nil still takes a mandatory newline")
(ok= (with-out-str (clojure.pprint/write (vec (range 40)) :right-margin nil :stream true))
     "[0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39]"
     "write's :right-margin nil is the same")

;; --- the interop form core.logic uses: (. simple-dispatch addMethod Type f) -----
;; These mutate the dispatch table. On a record's class (an IPersistentMap) the
;; exact-class method must win over the built-in IPersistentMap arm.
(defrecord Widget [n])
(def widget-class (class (->Widget 0)))
(ok #(do (. clojure.pprint/simple-dispatch addMethod widget-class
            (fn [x] (print (str "W-" (:n x)))))
         nil)
    "simple-dispatch addMethod interop registers without error")
(ok= (out (->Widget 7)) "W-7\n" "a Widget pprint routes through its addMethod method")

;; --- defmethod against simple-dispatch -----------------------------------------
(defrecord Gadget [n])
(def gadget-class (class (->Gadget 0)))
(ok #(do (defmethod clojure.pprint/simple-dispatch gadget-class [x]
           (print (str "G-" (:n x))))
         nil)
    "defmethod against simple-dispatch registers without error")
(ok= (out (->Gadget 4)) "G-4\n" "a Gadget pprint routes through its defmethod method")

;; --- verdict -------------------------------------------------------------------
(let [n @passes f @fails]
  (doseq [m f] (println "pprint-dispatch FAIL " m))
  (println "PPRINT-DISPATCH-RESULT pass" n "fail" (count f))
  (println (if (zero? (count f)) "PPRINT-DISPATCH OK" "PPRINT-DISPATCH FAIL"))
  (flush))
