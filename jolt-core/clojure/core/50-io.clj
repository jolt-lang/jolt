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
;; Forms are parsed by the host seam __parse-next-from (one form + the index
;; just past it, nil when only whitespace remains). The readers hold a CURSOR
;; into their input rather than the shrinking rest of the string: re-copied
;; tails made line/form drains O(input x items) (`make ioscaling` gates the
;; shape). Known wart shared with the parse contract: input that is only a
;; comment reads as nil rather than EOF.

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
  (let [pos (atom 0)]
    (reify IReader
      (-read-line [_]
        ;; \n, \r and \r\n all end a line, and none of them is part of it —
        ;; java.io.BufferedReader's rule, which the host seam owns so this
        ;; reader and the port-backed ones cannot disagree about CRLF input.
        (let [r (__string-line-from s @pos)]
          (when-not (nil? r)
            (reset! pos (nth r 1))
            (nth r 0))))
      (-read-form [_]
        (let [r (__parse-next-from s @pos)]
          (if (nil? r)
            reader-eof
            (do (reset! pos (nth r 1)) (nth r 0)))))
      (-read+string [_ eof-error? eof-value]
        (let [p @pos
              r (__parse-next-from s p)]
          (if (nil? r)
            (if eof-error?
              (throw (ex-info "EOF while reading" {}))
              [eof-value ""])
            (do (reset! pos (nth r 1))
                [(nth r 0) (subs s p (nth r 1))])))))))

;; Real stdin: a leftover [buffer cursor] shared by read and read-line. When a
;; parse needs another line, the consumed prefix is dropped as the line is
;; appended, so the buffer only ever holds the unread tail — a single form
;; still arriving pays a copy and a re-parse per line (the parse seam is not
;; incremental), but cost is bounded by that form, never the whole session.
(def ^:private stdin-buf (atom ["" 0]))

(def ^:dynamic *in*
  (reify IReader
    (-read-line [_]
      (let [sp @stdin-buf
            s (nth sp 0)
            p (nth sp 1)]
        (if (< p (count s))
          (let [i (str-find "\n" s p)]
            (if (nil? i)
              (do (reset! stdin-buf ["" 0]) (subs s p))
              (do (reset! stdin-buf [s (inc i)]) (subs s p i))))
          (__stdin-read-line))))
    (-read-form [_]
      (loop []
        (let [sp @stdin-buf
              s (nth sp 0)
              p (nth sp 1)
              r (__parse-next-from s p)]
          (if (nil? r)
            (let [line (__stdin-read-line)]
              (if (nil? line)
                reader-eof
                (do (reset! stdin-buf [(str (subs s p) line "\n") 0]) (recur))))
            (do (reset! stdin-buf [s (nth r 1)]) (nth r 0))))))
    (-read+string [_ eof-error? eof-value]
      (loop []
        (let [sp @stdin-buf
              s (nth sp 0)
              p (nth sp 1)
              r (__parse-next-from s p)]
          (if (nil? r)
            (let [line (__stdin-read-line)]
              (if (nil? line)
                (if eof-error?
                  (throw (ex-info "EOF while reading" {}))
                  [eof-value ""])
                (do (reset! stdin-buf [(str (subs s p) line "\n") 0]) (recur))))
            (do (reset! stdin-buf [s (nth r 1)])
                [(nth r 0) (subs s p (nth r 1))])))))))

(defn read-line
  "Reads the next line from the stream that is the current value of *in*.
  Returns nil at EOF."
  []
  (-read-line *in*))

(defn read
  "Reads the next object from stream (defaults to *in*). At EOF, throws —
  or returns eof-value when eof-error? is false.

  The 2-arity is Clojure's opts-map form, (read opts stream): an :eof key in
  opts is the value returned at end of input, and its ABSENCE (not a nil value)
  is what makes EOF throw. The remaining opts keys the JVM reads — :read-cond,
  :features, :readers — are the reader's, not this function's; jolt's reader
  resolves conditionals and tags itself, so they are accepted and ignored.
  The 4-arity's recursive? flag is likewise the JVM reader's own bookkeeping."
  ([] (read *in*))
  ([stream]
   (let [v (-read-form stream)]
     (if (= v reader-eof)
       (throw (ex-info "EOF while reading" {}))
       v)))
  ([opts stream]
   (let [v (-read-form stream)]
     (if (= v reader-eof)
       (if (contains? opts :eof)
         (get opts :eof)
         (throw (ex-info "EOF while reading" {})))
       v)))
  ([stream eof-error? eof-value]
   (let [v (-read-form stream)]
     (if (= v reader-eof)
       (if eof-error? (throw (ex-info "EOF while reading" {})) eof-value)
       v)))
  ([stream eof-error? eof-value _recursive?]
   (read stream eof-error? eof-value)))

(defmacro with-in-str
  "Evaluates body with *in* bound to a fresh reader over string s."
  [s & body]
  `(binding [*in* (__string-reader ~s)]
     ~@body))

(defn read+string
  "Like read, plus the exact text consumed, as [form text]. Carries the same
  arity set as read, opts map included."
  ([] (read+string *in*))
  ([stream] (read+string stream true nil))
  ([opts stream]
   (if (contains? opts :eof)
     (-read+string stream false (get opts :eof))
     (-read+string stream true nil)))
  ([stream eof-error? eof-value]
   (-read+string stream eof-error? eof-value))
  ([stream eof-error? eof-value _recursive?]
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
;; builtins and the class name for records, so a record or host-class method is
;; (defmethod print-method SomeType [x w] ...) — the class token is the value.
;;
;; The :default renders through the host's fast printer, and the host printer
;; consults this table first (io-streams.ss), so a method here IS the printer for
;; its type, as on the JVM: it fires from pr/prn/pr-str and nested inside
;; collections. A method keyed by a class fires for a record or a host object; a
;; built-in (number, string, vector) is keyed by its tag, since resolving a class
;; per element printed would cost more than the feature is worth.
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

;; The reference's private print entry, reached by libraries through
;; @#'clojure.core/pr-on (nREPL's print middleware binds it as its default
;; print fn). Private there and here; verbatim.
(defn pr-on
  {:private true
   :static true}
  [x w]
  (if *print-dup*
    (print-dup x w)
    (print-method x w))
  nil)

;; An Eduction prints as the seq it yields — (2 3 4), not the deftype's fields.
;; Registered against the type rather than derived from its interfaces because
;; that is what the JVM does: a bare Sequential/Seqable deftype prints as
;; #object[…] there, and only Eduction renders sequentially.
;; (dispatch value is the __type-tag STRING for a deftype, not a symbol)
(defmethod print-method "clojure.core.Eduction" [e w]
  (print-method (apply list (seq e)) w))

;; A channel has no readable form, so name it here; every other host type the
;; printer knows renders through :default, which is the native renderer.
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
