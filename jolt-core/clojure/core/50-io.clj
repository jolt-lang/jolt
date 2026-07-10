;; clojure.core — IO tier: the *in* reader family.
;;
;; *in* is a dynamic var holding a READER: a plain map whose two ops close
;; over their source — :read-line-fn (next line, newline
;; stripped, nil at EOF) and :read-fn (next FORM, advancing past exactly that
;; form; the eof sentinel at end of input). The default *in* reads real stdin
;; through the host seam __stdin-read-line, with a shared leftover buffer so
;; read and read-line interleave; with-in-str rebinds *in* to a string reader
;; over one atom-held buffer, so (read) consumes its form and a following
;; (read-line) returns the REST of that line — as in Clojure.
;;
;; Forms are parsed by the host seam __parse-next (one form + the rest of the
;; string, nil when only whitespace remains). Known wart shared with that
;; contract: input that is only a comment reads as nil rather than EOF.

(def ^:private reader-eof :jolt/reader-eof)

;; *in* is a Reader, not a map — so (map? *in*) is false, matching the JVM
;; (java.io.Reader). The reader is a reify over IReader (a non-map value, unlike
;; a defrecord which IS a map); read/read-line/read+string dispatch through its
;; methods. Each implementation closes over its own buffer atom: read may pull a
;; whole line to parse a form and hands the remainder to the next read/read-line.
(defprotocol IReader
  (-read-line [rdr] "Next line, newline stripped; nil at EOF.")
  (-read-form [rdr] "Next form; the reader-eof sentinel at end of input.")
  (-read+string [rdr eof-error? eof-value]
    "Next form plus the exact text consumed (leading whitespace included), as
    [form text]. On EOF: throws, or returns [eof-value \"\"] when eof-error? is false."))

(defn __string-reader
  "A reader over string s (the with-in-str expansion calls this)."
  [s]
  (let [buf (atom s)]
    (reify IReader
      (-read-line [_]
        (let [cur @buf]
          (when (pos? (count cur))
            (let [i (str-find "\n" cur)]
              (if (nil? i)
                (do (reset! buf "") cur)
                (do (reset! buf (subs cur (inc i))) (subs cur 0 i)))))))
      (-read-form [_]
        (let [r (__parse-next @buf)]
          (if (nil? r)
            reader-eof
            (do (reset! buf (nth r 1)) (nth r 0)))))
      (-read+string [_ eof-error? eof-value]
        (let [s @buf
              r (__parse-next s)]
          (if (nil? r)
            (if eof-error?
              (throw (ex-info "EOF while reading" {}))
              [eof-value ""])
            (do (reset! buf (nth r 1))
                [(nth r 0) (subs s 0 (- (count s) (count (nth r 1))))])))))))

;; Real stdin, with a leftover buffer shared by read and read-line.
(def ^:private stdin-buf (atom ""))

(def ^:dynamic *in*
  (reify IReader
    (-read-line [_]
      (let [cur @stdin-buf]
        (if (pos? (count cur))
          (let [i (str-find "\n" cur)]
            (if (nil? i)
              (do (reset! stdin-buf "") cur)
              (do (reset! stdin-buf (subs cur (inc i))) (subs cur 0 i))))
          (__stdin-read-line))))
    (-read-form [_]
      (loop []
        (let [r (__parse-next @stdin-buf)]
          (if (nil? r)
            (let [line (__stdin-read-line)]
              (if (nil? line)
                reader-eof
                (do (swap! stdin-buf (fn [b] (str b line "\n"))) (recur))))
            (do (reset! stdin-buf (nth r 1)) (nth r 0))))))
    (-read+string [_ eof-error? eof-value]
      (loop []
        (let [s @stdin-buf
              r (__parse-next s)]
          (if (nil? r)
            (let [line (__stdin-read-line)]
              (if (nil? line)
                (if eof-error?
                  (throw (ex-info "EOF while reading" {}))
                  [eof-value ""])
                (do (swap! stdin-buf (fn [b] (str b line "\n"))) (recur))))
            (do (reset! stdin-buf (nth r 1))
                [(nth r 0) (subs s 0 (- (count s) (count (nth r 1))))])))))))

(defn read-line
  "Reads the next line from the stream that is the current value of *in*.
  Returns nil at EOF."
  []
  (-read-line *in*))

(defn read
  "Reads the next object from stream (defaults to *in*). At EOF, throws —
  or returns eof-value when eof-error? is false."
  ([] (read *in*))
  ([stream]
   (let [v (-read-form stream)]
     (if (= v reader-eof)
       (throw (ex-info "EOF while reading" {}))
       v)))
  ([stream eof-error? eof-value]
   (let [v (-read-form stream)]
     (if (= v reader-eof)
       (if eof-error? (throw (ex-info "EOF while reading" {})) eof-value)
       v))))

(defmacro with-in-str
  "Evaluates body with *in* bound to a fresh reader over string s."
  [s & body]
  `(binding [*in* (__string-reader ~s)]
     ~@body))

(defn read+string
  ([] (read+string *in*))
  ([stream] (read+string stream true nil))
  ([stream eof-error? eof-value]
   (-read+string stream eof-error? eof-value)))

(defn line-seq
  "Returns the lines of text from rdr as a lazy sequence of strings, as by
  read-line. (Jolt extension kept from the old kernel stub: a plain string
  splits into its lines.)"
  [rdr]
  (if (string? rdr)
    (seq (str-split "\n" rdr))
    (lazy-seq
      (let [line (-read-line rdr)]
        (when line
          (cons line (line-seq rdr)))))))

;; --- print-method ------------------------------------------------
;; Canonical dispatch (clojure/core.clj 3693): the :type metadata when it's a
;; keyword, else the value's type. On jolt, type is the keyword tag for
;; builtins and the deftype name SYMBOL for records — so a record method is
;; (defmethod print-method 'ns.Type [r w] ...) (class names aren't values
;; here, the quoted full name is the dispatch value).
;;
;; The :default renders through the host's fast printer. The host renderer
;; calls BACK into this table for records (the api wires the hook after the
;; overlay loads), so a record method fires nested inside collections too.
;; Builtin overrides (e.g. a :number method) fire only when print-method is
;; called directly — pr/pr-str keep the native fast path for builtins (a
;; documented jolt divergence).
(defmulti print-method (fn [x writer]
                         (let [t (get (meta x) :type)]
                           (if (keyword? t) t (__type-tag x)))))

(defmethod print-method :default [o w]
  (.write w (__pr-str1 o))
  nil)

;; print-dup: jolt has one print representation, so dup routes to print-method
;; (as Clojure's default does for most types).
(defmulti print-dup (fn [x writer]
                      (let [t (get (meta x) :type)]
                        (if (keyword? t) t (__type-tag x)))))

(defmethod print-dup :default [o w] (print-method o w))

;; Cold tagged-type renderings, migrated from the host renderer (the hot
;; types — numbers, strings, symbols, collections — stay native). Each is the
;; exact output the host branch produced.
(defmethod print-method :jolt/uuid [u w]
  (.write w (str "#uuid \"" (get u :str) "\""))
  nil)

(defmethod print-method :jolt/regex [re w]
  (.write w (str "#\"" (get re :source) "\""))
  nil)

;; a transient's get IS the dispatched collection lookup — read the wrapper's
;; own :kind field with the host accessor (same trap as sorted colls).
(defmethod print-method :jolt/transient [t w]
  (.write w (str "#<transient " (name (jolt.host/ref-get t :kind)) ">"))
  nil)

(defmethod print-method :jolt/chan [c w]
  (.write w "#<channel>")
  nil)

;; Minimal synchronous agent shim. jolt has no thread pool or STM, so this is
;; enough for libraries that hold an agent but don't depend on asynchronous
;; dispatch (e.g. clojure.tools.logging's *logging-agent*, which only sends from
;; within a transaction — never the case here, so it always logs directly). An
;; agent is an atom; send/send-off apply the action immediately. NOT concurrent.
(defn agent
  "Creates an agent (an atom on jolt — synchronous, no async dispatch)."
  [state & _opts]
  (atom state))

(defn send-off
  "Apply (action state & args) to the agent's state immediately; return the agent."
  [a f & args]
  (apply swap! a f args)
  a)

(defn send
  "Like send-off on jolt (no separate thread pool)."
  [a f & args]
  (apply swap! a f args)
  a)

(defn agent-error
  "jolt agents never enter an error state."
  [_a]
  nil)

;; cast — (cast c x): nil passes through (Class.cast of null), otherwise x must
;; be an instance of c or ClassCastException is thrown.
(defn cast [c x]
  (cond
    (nil? x) nil
    (instance? c x) x
    :else (throw (ClassCastException. (str "Cannot cast " x " to " c)))))

;; iteration — a seqable/reducible pager over a step fn of a continuation token k.
;; jolt has no Seqable reify dispatch, so this returns a lazy-seq (seqable, and
;; reducible through seq); the step/kf/vf/somef contract matches clojure.core.
(defn iteration
  "Creates a seqable/reducible via repeated calls to step, a function of some
  (continuation token) 'k'. The first call to step will be passed initk,
  returning 'ret'. Iff (somef ret) is true, (vf ret) will be included in the
  iteration, else iteration will terminate and vf/kf will not be called. If
  (kf ret) is non-nil it will be passed to the next step call, else iteration
  will terminate.

   step - (possibly impure) fn of 'k' -> 'ret'
   :somef - fn of 'ret' -> logical true/false, default 'some?'
   :vf - fn of 'ret' -> 'v', a value produced by the iteration, default 'identity'
   :kf - fn of 'ret' -> 'next-k' or nil (signaling 'do not continue'), default 'identity'
   :initk - the first value passed to step, default 'nil'"
  {:added "1.11"}
  [step & {:keys [somef vf kf initk]
           :or {vf identity
                kf identity
                somef some?
                initk nil}}]
  ((fn step* [ret]
     (lazy-seq
       (when (somef ret)
         (cons (vf ret)
               (when-some [k (kf ret)]
                 (step* (step k)))))))
   (step initk)))

;; print-simple — print without print-method dispatch (no print-meta in jolt).
(defn print-simple [o w]
  (.write w (str o)))

;; StackTraceElement->vec — [class method file line]. jolt stack traces are
;; empty, so this exists for API compatibility; nil -> [].
(defn StackTraceElement->vec [o]
  (if (nil? o)
    []
    [(.getClassName o) (.getMethodName o) (.getFileName o) (.getLineNumber o)]))
