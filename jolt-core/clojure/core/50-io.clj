;; clojure.core — IO tier: the *in* reader family (jolt-0d9).
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

(defn __string-reader
  "A reader over string s (the with-in-str expansion calls this)."
  [s]
  (let [buf (atom s)]
    {:buf buf
     :fill-fn nil
     :read-line-fn
     (fn []
       (let [cur @buf]
         (when (pos? (count cur))
           (let [i (str-find "\n" cur)]
             (if (nil? i)
               (do (reset! buf "") cur)
               (do (reset! buf (subs cur (inc i))) (subs cur 0 i)))))))
     :read-fn
     (fn []
       (let [r (__parse-next @buf)]
         (if (nil? r)
           reader-eof
           (do (reset! buf (nth r 1)) (nth r 0)))))}))

;; Real stdin, with a leftover buffer shared by read and read-line: read may
;; pull a whole line to parse a form and must hand the remainder to the next
;; read/read-line.
(def ^:private stdin-buf (atom ""))

(def ^:dynamic *in*
  {:buf stdin-buf
   :fill-fn (fn []
              (let [line (__stdin-read-line)]
                (if (nil? line)
                  false
                  (do (swap! stdin-buf (fn [b] (str b line "\n"))) true))))
   :read-line-fn
   (fn []
     (let [cur @stdin-buf]
       (if (pos? (count cur))
         (let [i (str-find "\n" cur)]
           (if (nil? i)
             (do (reset! stdin-buf "") cur)
             (do (reset! stdin-buf (subs cur (inc i))) (subs cur 0 i))))
         (__stdin-read-line))))
   :read-fn
   (fn []
     (loop []
       (let [r (__parse-next @stdin-buf)]
         (if (nil? r)
           (let [line (__stdin-read-line)]
             (if (nil? line)
               reader-eof
               (do (swap! stdin-buf (fn [b] (str b line "\n"))) (recur))))
           (do (reset! stdin-buf (nth r 1)) (nth r 0))))))})

(defn read-line
  "Reads the next line from the stream that is the current value of *in*.
  Returns nil at EOF."
  []
  ((:read-line-fn *in*)))

(defn read
  "Reads the next object from stream (defaults to *in*). At EOF, throws —
  or returns eof-value when eof-error? is false."
  ([] (read *in*))
  ([stream]
   (let [v ((:read-fn stream))]
     (if (= v reader-eof)
       (throw (ex-info "EOF while reading" {}))
       v)))
  ([stream eof-error? eof-value]
   (let [v ((:read-fn stream))]
     (if (= v reader-eof)
       (if eof-error? (throw (ex-info "EOF while reading" {})) eof-value)
       v))))

(defmacro with-in-str
  "Evaluates body with *in* bound to a fresh reader over string s."
  [s & body]
  `(binding [*in* (__string-reader ~s)]
     ~@body))

;; Like read, and also returns the exact text consumed for the form (leading
;; whitespace included). On EOF: throws, or returns [eof-value ""] when
;; eof-error? is false.
(defn read+string
  ([] (read+string *in*))
  ([stream] (read+string stream true nil))
  ([stream eof-error? eof-value]
   (let [buf (get stream :buf)
         fill (get stream :fill-fn)]
     (loop []
       (let [s (deref buf)
             r (__parse-next s)]
         (if (nil? r)
           (if (and fill (fill))
             (recur)
             (if eof-error?
               (throw (ex-info "EOF while reading" {}))
               [eof-value ""]))
           (do (reset! buf (nth r 1))
               [(nth r 0) (subs s 0 (- (count s) (count (nth r 1))))])))))))

(defn line-seq
  "Returns the lines of text from rdr as a lazy sequence of strings, as by
  read-line. (Jolt extension kept from the old kernel stub: a plain string
  splits into its lines.)"
  [rdr]
  (if (string? rdr)
    (seq (str-split "\n" rdr))
    (lazy-seq
      (let [line ((:read-line-fn rdr))]
        (when line
          (cons line (line-seq rdr)))))))

;; --- print-method (jolt-g1r) ------------------------------------------------
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
                           (if (keyword? t) t (type x)))))

(defmethod print-method :default [o w]
  (.write w (__pr-str1 o))
  nil)

;; print-dup: jolt has one print representation, so dup routes to print-method
;; (as Clojure's default does for most types).
(defmulti print-dup (fn [x writer]
                      (let [t (get (meta x) :type)]
                        (if (keyword? t) t (type x)))))

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
