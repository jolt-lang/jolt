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
    {:read-line-fn
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
  {:read-line-fn
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
