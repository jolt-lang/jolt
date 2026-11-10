;;   Portions of this file are from Clojure's src/clj/clojure/repl.clj,
;;   Copyright (c) Chris Houser, Dec 2008. All rights reserved.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

; Jolt Standard Library: clojure.repl
; Partial port from Clojure 1.12's clojure.repl: the special-form documentation
; data (special-doc-map / special-doc) and the metadata-driven interactive fns
; (doc, find-doc, apropos, dir) plus demunge, and the stack-trace fns (pst,
; root-cause, stack-element-str). source-fn answers nil, the JVM's answer for a
; var whose source file it cannot find, and source prints "Source not found":
; image-baked vars carry no :file, so there is no source text to point at.

(ns clojure.repl)

;; Compiler.demunge's CHAR_MAP, inverted. A built binary carries no compiler
;; image, so clojure.lang.Compiler/demunge is not there to ask, and the frames it
;; reports name their fns the JVM way (app.core$_main): demunge answers from this
;; table then.
(def ^:private demunge-names
  {"COLON" ":" "PLUS" "+" "GT" ">" "LT" "<" "EQ" "=" "TILDE" "~" "BANG" "!"
   "CIRCA" "@" "SHARP" "#" "SINGLEQUOTE" "'" "DOUBLEQUOTE" "\"" "PERCENT" "%"
   "CARET" "^" "AMPERSAND" "&" "STAR" "*" "BAR" "|" "LBRACE" "{" "RBRACE" "}"
   "LBRACK" "[" "RBRACK" "]" "SLASH" "/" "BSLASH" "\\" "QMARK" "?"})

(defn- demunge-by-table [fn-name]
  (let [s (.replace fn-name "$" "/")
        n (count s)
        sb (StringBuilder.)]
    (loop [i 0]
      (if (< i n)
        (let [c (.charAt s i)]
          (if (= c \_)
            (let [j (.indexOf s "_" (inc i))
                  ch (when (pos? j) (get demunge-names (subs s (inc i) j)))]
              (if ch
                (do (.append sb ch) (recur (inc j)))
                (do (.append sb "-") (recur (inc i)))))
            (do (.append sb c) (recur (inc i)))))
        (str sb)))))

(defn demunge
  "Given a string representation of a fn class,
  as in a stack trace element, returns a readable version."
  [fn-name]
  (try
    (clojure.lang.Compiler/demunge fn-name)
    (catch Throwable _ (demunge-by-table fn-name))))

(def ^:private special-doc-map
  '{. {:url "java_interop#dot"
       :forms [(.instanceMember instance args*)
               (.instanceMember Classname args*)
               (Classname/staticMethod args*)
               Classname/staticField]
       :doc "The instance member form works for both fields and methods.
  They all expand into calls to the dot operator at macroexpansion time."}
    def {:forms [(def symbol doc-string? init?)]
         :doc "Creates and interns a global var with the name
  of symbol in the current namespace (*ns*) or locates such a var if
  it already exists.  If init is supplied, it is evaluated, and the
  root binding of the var is set to the resulting value.  If init is
  not supplied, the root binding of the var is unaffected."}
    do {:forms [(do exprs*)]
        :doc "Evaluates the expressions in order and returns the value of
  the last. If no expressions are supplied, returns nil."}
    if {:forms [(if test then else?)]
        :doc "Evaluates test. If not the singular values nil or false,
  evaluates and yields then, otherwise, evaluates and yields else. If
  else is not supplied it defaults to nil."}
    monitor-enter {:forms [(monitor-enter x)]
                   :doc "Synchronization primitive that should be avoided
  in user code. Use the 'locking' macro."}
    monitor-exit {:forms [(monitor-exit x)]
                  :doc "Synchronization primitive that should be avoided
  in user code. Use the 'locking' macro."}
    new {:forms [(Classname. args*) (new Classname args*)]
         :url "java_interop#new"
         :doc "The args, if any, are evaluated from left to right, and
  passed to the constructor of the class named by Classname. The
  constructed object is returned."}
    quote {:forms [(quote form)]
           :doc "Yields the unevaluated form."}
    recur {:forms [(recur exprs*)]
           :doc "Evaluates the exprs in order, then, in parallel, rebinds
  the bindings of the recursion point to the values of the exprs.
  Execution then jumps back to the recursion point, a loop or fn method."}
    set! {:forms [(set! var-symbol expr)
                  (set! (. instance-expr instanceFieldName-symbol) expr)
                  (set! (. Classname-symbol staticFieldName-symbol) expr)]
          :url "vars#set"
          :doc "Used to set thread-local-bound vars, Java object instance
fields, and Java class static fields."}
    throw {:forms [(throw expr)]
           :doc "The expr is evaluated and thrown, therefore it should
  yield an instance of some derivee of Throwable."}
    try {:forms [(try expr* catch-clause* finally-clause?)]
         :doc "catch-clause => (catch classname name expr*)
  finally-clause => (finally expr*)

  Catches and handles Java exceptions."}
    var {:forms [(var symbol)]
         :doc "The symbol must resolve to a var, and the Var object
itself (not its value) is returned. The reader macro #'x expands to (var x)."}})

(defn- special-doc [name-symbol]
  (assoc (or (special-doc-map name-symbol) (meta (resolve name-symbol)))
         :special-form true))

(defn- namespace-doc [nspace]
  (assoc (meta nspace) :name (ns-name nspace)))

; Verbatim from Clojure minus the trailing spec block (jolt's image carries no
; clojure.spec, so there is no fnspec to describe).
(defn- print-doc [{n :ns
                   nm :name
                   :keys [forms arglists special-form doc url macro]
                   :as m}]
  (println "-------------------------")
  (println (str (when n (str (ns-name n) "/")) nm))
  (when forms
    (doseq [f forms]
      (print "  ")
      (prn f)))
  (when arglists
    (prn arglists))
  (cond
    special-form
    (println "Special Form")
    macro
    (println "Macro"))
  (when doc (println " " doc))
  (when special-form
    (if (contains? m :url)
      (when url
        (println (str "\n  Please see http://clojure.org/" url)))
      (println (str "\n  Please see http://clojure.org/special_forms#" nm)))))

(defn find-doc
  "Prints documentation for any var whose documentation or name
 contains a match for re-string-or-pattern"
  {:added "1.0"}
  [re-string-or-pattern]
    (let [re (re-pattern re-string-or-pattern)
          ms (concat (mapcat #(sort-by :name (map meta (vals (ns-interns %))))
                             (all-ns))
                     (map namespace-doc (all-ns))
                     (map special-doc (keys special-doc-map)))]
      (doseq [m ms
              :when (and (:doc m)
                         (or (re-find (re-matcher re (:doc m)))
                             (re-find (re-matcher re (str (:name m))))))]
               (print-doc m))))

; Verbatim minus the keyword branch (spec lookup — no clojure.spec on jolt).
(defmacro doc
  "Prints documentation for a var or special form given its name"
  {:added "1.0"}
  [name]
  (if-let [special-name ('{& fn catch try finally try} name)]
    `(#'print-doc (#'special-doc '~special-name))
    (cond
      (special-doc-map name) `(#'print-doc (#'special-doc '~name))
      (find-ns name) `(#'print-doc (#'namespace-doc (find-ns '~name)))
      (resolve name) `(#'print-doc (meta (var ~name))))))

(defn apropos
  "Given a regular expression or stringable thing, return a seq of all
public definitions in all currently-loaded namespaces that match the
str-or-pattern."
  [str-or-pattern]
  (let [matches? (if (instance? java.util.regex.Pattern str-or-pattern)
                   #(re-find str-or-pattern (str %))
                   #(.contains (str %) (str str-or-pattern)))]
    (sort (mapcat (fn [ns]
                    (let [ns-name (str ns)]
                      (map #(symbol ns-name (str %))
                           (filter matches? (keys (ns-publics ns))))))
                  (all-ns)))))

(defn dir-fn
  "Returns a sorted seq of symbols naming public vars in
  a namespace or namespace alias. Looks for aliases in *ns*"
  [ns]
  (sort (map first (ns-publics (the-ns (get (ns-aliases *ns*) ns ns))))))

(defmacro dir
  "Prints a sorted directory of public vars in a namespace"
  [nsname]
  `(doseq [v# (dir-fn '~nsname)]
     (println v#)))

(defn source-fn
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.

  Jolt keeps no source text for a var, so this always returns nil, the
  answer the JVM gives when the source file cannot be found.

  Example: (source-fn 'filter)"
  [x]
  nil)

(defmacro source
  "Prints the source code for the given symbol, if it can find it.
  This requires that the symbol resolve to a Var defined in a
  namespace for which the .clj is in the classpath.

  Example: (source filter)"
  [n]
  `(println (or (source-fn '~n) (str "Source not found"))))

;; The reference stops at a CompilerException that names a source file; jolt's
;; compile errors carry their position as ex-data instead, so the walk is the
;; plain cause chain.
(defn root-cause
  "Returns the initial cause of an exception or error by peeling off all of
  its wrappers"
  {:added "1.3"}
  [t]
  (loop [cause t]
    (if-let [cause' (.getCause cause)]
      (recur cause')
      cause)))

(defn stack-element-str
  "Returns a (possibly unmunged) string representation of a StackTraceElement"
  {:added "1.3"}
  [el]
  (let [file (.getFileName el)
        clojure-fn? (and file (or (.endsWith file ".clj") (.endsWith file ".cljc")
                                  (= file "NO_SOURCE_FILE")))]
    (str (if clojure-fn?
           (demunge (.getClassName el))
           (str (.getClassName el) "." (.getMethodName el)))
         " (" (.getFileName el) ":" (.getLineNumber el) ")")))

(defn pst
  "Prints a stack trace of the exception, to the depth requested. If none supplied, uses the root cause of the
  most recent repl exception (*e), and a depth of 12."
  {:added "1.3"}
  ([] (pst 12))
  ([e-or-depth]
   (if (instance? Throwable e-or-depth)
     (pst e-or-depth 12)
     (when-let [e *e]
       (pst (root-cause e) e-or-depth))))
  ([e depth]
   (binding [*out* *err*]
     (when (#{:read-source :macro-syntax-check :macroexpansion :compile-syntax-check :compilation}
            (-> e ex-data :clojure.error/phase))
       (println "Note: The following stack trace applies to the reader or compiler, your code was not executed."))
     (println (str (-> e class .getSimpleName) " "
                   (.getMessage e)
                   (when-let [info (ex-data e)] (str " " (pr-str info)))))
     (let [st (.getStackTrace e)
           cause (.getCause e)]
       (doseq [el (take depth
                        (remove #(#{"clojure.lang.RestFn" "clojure.lang.AFn"} (.getClassName %))
                                st))]
         (println (str \tab (stack-element-str el))))
       (when cause
         (println "Caused by:")
         (pst cause (min depth
                         (+ 2 (- (count st)
                                 (count (.getStackTrace cause)))))))))))
