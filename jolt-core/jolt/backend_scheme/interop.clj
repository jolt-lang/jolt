(ns jolt.backend-scheme.interop
  "Host method calls — .m on a target and its args — for the Scheme back end: the direct
  forms for a receiver proven a string, keyword or string builder, the
  per-site caches for field reads and small calls, and the generic
  record-method-dispatch fallback."
  (:require [clojure.string :as str]
            [jolt.backend-scheme.names :refer [chez-str-lit]]
            [jolt.backend-scheme.session :refer [fresh-label target var-cache?]]
            [jolt.backend-scheme.hoist :refer [*const-pool* hoist-const-per-site]]))

;; Host interop methods with a Chez RT shim (rt.ss jolt-host-call). A `.method`
;; call on any other method routes to record-method-dispatch (a reify/record
;; protocol method).
(def ^:private supported-host-methods #{"isDirectory" "listFiles"})

;; Direct emission for (.m target arg*) whose target is PROVEN a string at
;; compile time (:target-type :str, stamped by the analyzer from a ^String hint
;; or a string literal). Emits the jolt-string-method arm's body inline — no
;; record-method-dispatch arm walk, no jolt-vector rest-args allocation
;; (~65-150ns/call on the honeysql interop path). Fixed arities only; any other
;; method or arity falls back to the generic dispatch (identical result).
;; Chez-only: these natives live in java/natives-str.ss, which the Gambit boot
;; excludes — a :gambit target keeps the generic path (checked at the emit site).
;; One deliberate edge divergence: a nil/non-string prefix on startsWith/endsWith
;; routes to str-starts-with?/str-ends-with?, which throw the JVM's NPE/CCE
;; instead of the generic arm's Scheme type error.
;;
;; The table maps method -> argument count -> a fn of the emitted target and
;; arguments. Each argument is spliced exactly once (they are raw expressions
;; here, not temporaries: a second splice would evaluate the argument twice).
;; The jolt-str-* natives (java/natives-str.ss) are the SAME procedures the
;; generic jolt-string-method arm calls, so the hinted and unhinted paths cannot
;; drift.
(def ^:private string-direct-forms
  {"length"              {0 (fn [t] (str "(string-length " t ")"))}
   "toString"            {0 (fn [t] t)}
   "isEmpty"             {0 (fn [t] (str "(fx=? (string-length " t ") 0)"))}
   "toUpperCase"         {0 (fn [t] (str "(string-upcase " t ")"))}
   "toLowerCase"         {0 (fn [t] (str "(string-downcase " t ")"))}
   "trim"                {0 (fn [t] (str "(str-trim " t ")"))}
   "hashCode"            {0 (fn [t] (str "(java-string-hash " t ")"))}
   "charAt"              {1 (fn [t a] (str "(jolt-char-at " t " " a ")"))}
   "indexOf"             {1 (fn [t a] (str "(str-index-of-any " t " " a " 0)"))
                          2 (fn [t a from] (str "(str-index-of-any " t " " a " (jolt->idx " from "))"))}
   "startsWith"          {1 (fn [t a] (str "(str-starts-with? " t " " a ")"))}
   "endsWith"            {1 (fn [t a] (str "(str-ends-with? " t " " a ")"))}
   "contains"            {1 (fn [t a] (str "(fx>=? (str-index-of " t " (str-needle " a ") 0) 0)"))}
   "concat"              {1 (fn [t a] (str "(string-append " t " " a ")"))}
   "substring"           {2 (fn [t a b] (str "(jolt-substr " t " (jolt->idx " a ") (jolt->idx " b "))"))}
   "replace"             {2 (fn [t a b] (str "(str-replace-literal " t " (str-needle " a ") (str-needle " b "))"))}
   "equals"              {1 (fn [t a] (str "(jolt-str-equals? " t " " a ")"))}
   "equalsIgnoreCase"    {1 (fn [t a] (str "(jolt-str-equals-ci? " t " " a ")"))}
   "compareTo"           {1 (fn [t a] (str "(jolt-str-compare " t " " a ")"))}
   "compareToIgnoreCase" {1 (fn [t a] (str "(jolt-str-compare-ci " t " " a ")"))}
   "isBlank"             {0 (fn [t] (str "(jolt-str-blank? " t ")"))}
   "repeat"              {1 (fn [t a] (str "(jolt-str-repeat " t " " a ")"))}
   "codePointAt"         {1 (fn [t a] (str "(jolt-str-code-point-at " t " " a ")"))}
   "lastIndexOf"         {1 (fn [t a] (str "(jolt-str-last-index-of " t " " a ")"))}
   "strip"               {0 (fn [t] (str "(jolt-str-strip " t " #t #t)"))}
   "stripLeading"        {0 (fn [t] (str "(jolt-str-strip " t " #t #f)"))}
   "stripTrailing"       {0 (fn [t] (str "(jolt-str-strip " t " #f #t)"))}
   "toCharArray"         {0 (fn [t] (str "(jolt-str-to-char-array " t ")"))}
   "getBytes"            {0 (fn [t] (str "(jolt-str-get-bytes " t " \"utf-8\")"))
                          1 (fn [t a] (str "(jolt-str-get-bytes " t " " a ")"))}
   "matches"             {1 (fn [t a] (str "(jolt-str-matches? " t " " a ")"))}
   "replaceAll"          {2 (fn [t a b] (str "(jolt-str-replace-all " t " " a " " b ")"))}
   "replaceFirst"        {2 (fn [t a b] (str "(jolt-str-replace-first " t " " a " " b ")"))}
   "split"               {1 (fn [t a] (str "(jolt-str-split " t " " a " 0)"))
                          2 (fn [t a b] (str "(jolt-str-split " t " " a " " b ")"))}
   "subSequence"         {2 (fn [t a b] (str "(jolt-str-sub-sequence " t " " a " " b ")"))}
   "intern"              {0 (fn [t] (str "(jolt-str-intern " t ")"))}})

;; The emitted form for method m applied to target t and the emitted args, from
;; one of the tables above; nil when the table has no form for that arity.
(defn- direct-form [table m t args]
  (when-let [emit-form (get (get table m) (count args))]
    (apply emit-form t args)))

(defn- string-direct-emit [m t args]
  (direct-form string-direct-forms m t args))

;; Direct emission for (.m target …) whose target is PROVEN a keyword
;; (:target-type :kw, from a ^clojure.lang.Keyword hint or a keyword literal).
;; Mirrors the keyword arm of record-method-dispatch (records-dispatch.ss)
;; method-for-method, body-for-body — no dispatch walk, no rest-args vector.
;; Chez-only, guarded at the emit site like the string path. A wrong hint (a
;; non-keyword at runtime) fails in the record accessor, the same contract a
;; wrong ^String hint has.
;;
;; `t` is spliced MORE THAN ONCE by sym, toString and hashCode, so it must be an
;; expression that is free to re-evaluate. It always is: the analyzer stamps
;; :target-type only for a tagged SYMBOL, a :hint-carrying local (analyzer.clj's
;; hints ride on :local nodes), or a literal — so `t` is an identifier or a
;; constant, never a call. A future stamp source that admits a compound target
;; has to bind t to a temporary first, or these three arms silently evaluate it
;; two and three times.
;;
;; honeysql's kw->sym is the shape that motivated this, and jolt reaches it:
;; honeysql orders its conditional #?(:bb … :clj (.sym ^Keyword k)) with :bb
;; first at all three of its .sym sites, and jolt stopped matching :bb in #893.
;; Also fires for code that writes .sym unconditionally or puts :clj first.
(def ^:private keyword-direct-forms
  {"sym"          {0 (fn [t] (str "(jolt-symbol (keyword-t-ns " t ") (keyword-t-name " t "))"))}
   "getName"      {0 (fn [t] (str "(keyword-t-name " t ")"))}
   "getNamespace" {0 (fn [t] (str "(or (keyword-t-ns " t ") jolt-nil)"))}
   "toString"     {0 (fn [t] (str "(string-append \":\" (if (keyword-t-ns " t ") (string-append (keyword-t-ns " t ") \"/\") \"\") (keyword-t-name " t "))"))}
   "hashCode"     {0 (fn [t] (str "(jolt-s32 (+ (java-symbol-hash (keyword-t-name " t ") (keyword-t-ns " t ")) #x9e3779b9))"))}
   "equals"       {1 (fn [t a] (str "(eq? " t " " a ")"))}})

(defn- keyword-direct-emit [m t args]
  (direct-form keyword-direct-forms m t args))

;; Direct emission for (.m target …) whose target is PROVEN a StringBuilder
;; (:target-type :sb — a ^StringBuilder tag, a local bound to (StringBuilder.),
;; or a constructor in target position). This is the most expensive interop shape
;; jolt had: a jhost method call hashes the tag STRING to find the method table,
;; hashes the method name to find the handler, converts the jolt-vector of rest
;; args back to a list for `apply`, and hands them to a shim lambda that takes a
;; further rest list — 270 ns for a 0-arg method and 500 ns for (.append sb "a"),
;; where sb-append! itself is three vector-set!s.
;;
;; Bodies match host-static-methods.ss's own accessors exactly, so a mixed program
;; that reaches the generic path for an untyped target sees identical behavior. In
;; particular append RETURNS THE BUILDER (the JVM's fluent contract, which is what
;; makes (-> sb (.append a) (.append b)) work), so `t` is spliced twice and, as
;; with the keyword table, that is only sound because the analyzer stamps :sb for
;; identifiers and constructor calls alone. A constructor in target position is the
;; one compound case, and it is bound to a let by emit-let before reaching here.
;;
;; append's 3-arg (x, start, end) form and setLength/insert/delete are left to the
;; generic path: they are not hot and the range checks are worth more than the
;; nanoseconds.
;;
;; The 1-arg body is sb-piece, NOT render-piece, for exactly the reason the
;; paragraph above gives: sb-piece is what the table's append arm calls, and it is
;; where a char[] becomes its characters rather than "#object[[C]". Open-coding
;; render-piece here meant a ^StringBuilder-tagged target got the rendering while an
;; untyped one got the characters.
;; LOCKED? is a StringBuffer (:sbuf): the same forms over the primitives that take
;; the buffer's monitor, since the JDK's StringBuffer methods are synchronized.
(defn- sb-direct-emit [m argc t args locked?]
  (let [a0 (first args)
        [app st len ch] (if locked?
                          ["sb-append*!" "sb-str*" "sb-length*" "sb-char-at*"]
                          ["sb-append!" "sb-str" "sb-length" "sb-char-at"])]
    (cond
      (= m "append")    (when (= argc 1)
                          (str "(begin (" app " " t " (sb-piece " a0 ")) " t ")"))
      (= m "toString")  (when (= argc 0) (str "(" st " " t ")"))
      (= m "length")    (when (= argc 0) (str "(->num (" len " " t "))"))
      (= m "isEmpty")   (when (= argc 0) (str "(fx=? (" len " " t ") 0)"))
      (= m "charAt")    (when (= argc 1) (str "(" ch " " t " " a0 ")"))
      :else nil)))

;; (.method target arg*) as a Scheme form. A node carrying :sited-target /
;; :sited-args (the tail-site emission in emit*) uses those already-bound temps
;; instead of emitting the receiver and args itself.
;; A call on an unproven receiver with up to three arguments and a plain name
;; goes through a per-site cache (host-static.ss jolt-host-site-callN): a host
;; object's method is then one tag test away, with no argument vector, and any
;; other receiver takes record-method-dispatch as before. Needs the def's
;; constant pool for the site's box, as the field-read site does.
(defn- host-site-emit [t m args]
  (when (and (<= (count args) 3) (not (str/starts-with? m "-"))
             *const-pool* (var-cache?))
    (str "(jolt-host-site-call" (count args) " " (hoist-const-per-site "(jolt-host-site-make)")
         " " t " " (chez-str-lit m)
         (apply str (map #(str " " %) args)) ")")))

;; The direct form for a receiver the analyzer proved a string, keyword, string
;; builder or string buffer (:target-type), or nil.
(defn- proven-receiver-emit [node m t args]
  (case (:target-type node)
    :str (string-direct-emit m t args)
    :kw (keyword-direct-emit m t args)
    :sb (sb-direct-emit m (count args) t args false)
    :sbuf (sb-direct-emit m (count args) t args true)
    nil))

;; The generic path: the receiver's own record or reify method.
(defn- method-dispatch-emit [t m args]
  (str "(record-method-dispatch " t " " (chez-str-lit m)
       " (jolt-vector" (if (empty? args) "" (str " " (str/join " " args))) "))"))

;; An UNPROVEN receiver whose method has a string or keyword
;; direct form: test the receiver's type at the site and take
;; that form, with the generic dispatch as the slow arm — the
;; same open-code-the-fast-case shape the bit ops use. Strings
;; and keywords are what library code calls .length/.charAt/
;; .getName on without a hint, and the generic walk cost
;; 60-135 ns per call against 3-11 for the direct form. The
;; receiver and args are bound once, in order, so nothing is
;; evaluated twice and the direct forms may splice `t` freely.
;; A receiver of any other type behaves exactly as before.
(defn- unproven-receiver-emit [m t args]
  (let [tt (fresh-label "_ht$")
        as (mapv (fn [_] (fresh-label "_ha$")) args)
        sd (string-direct-emit m tt as)
        kd (keyword-direct-emit m tt as)
        generic (or (host-site-emit tt m as) (method-dispatch-emit tt m as))]
    (if (or sd kd)
      (str "(let* ((" tt " " t ")"
           (apply str (map (fn [a e] (str " (" a " " e ")")) as args))
           ") (cond"
           (when sd (str " ((string? " tt ") " sd ")"))
           (when kd (str " ((keyword-t? " tt ") " kd ")"))
           " (else " generic ")))")
      (or (host-site-emit t m args) (method-dispatch-emit t m args)))))

;; EMIT lowers the receiver and arguments when the node does not carry them
;; already bound (the back end's own emit: this namespace sits below it).
(defn host-call-emit [emit node]
  (let [m (:method node)
        chez? (not= :gambit (target))
        t (or (:sited-target node) (emit (:target node)))
        args (or (:sited-args node) (map emit (:args node)))
        direct (when chez? (proven-receiver-emit node m t args))]
    (cond
      direct direct
      (supported-host-methods m)
      (str "(jolt-host-call " (chez-str-lit m) " " t
           (if (empty? args) "" (str " " (str/join " " args))) ")")
      ;; (.-name x): a field read. The site caches the record type it last read
      ;; and that type's slot (records-dispatch.ss jrec-field-site-ref); any
      ;; other receiver takes record-method-dispatch as below.
      (and chez? (empty? args) (str/starts-with? m "-") (> (count m) 1)
           *const-pool* (var-cache?))
      (str "(jrec-field-site-ref " (hoist-const-per-site "(jrec-field-site-make)") " "
           t " " (chez-str-lit m) ")")
      chez? (unproven-receiver-emit m t args)
      :else (method-dispatch-emit t m args))))


