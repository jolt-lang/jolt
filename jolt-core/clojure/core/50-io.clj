;; clojure.core — IO tier: the *in* reader family.
;;
;; *in* is a dynamic var holding a READER: a reify over IReader whose ops close
;; over their source — a LINE (newline stripped, nil at EOF), a FORM (advancing
;; past exactly that form; the eof sentinel at end of input), and a CHARACTER
;; (a code point, -1 at end of input), all off one cursor so they interleave.
;; clojure.main/repl-read is what wants the character: it skips whitespace one
;; character at a time before handing the rest to a form read. The default *in*
;; reads real stdin through the host seam __stdin-read-line, with a shared
;; leftover buffer; with-in-str rebinds *in* to a string reader over one
;; atom-held buffer, so (read) consumes its form and a following (read-line)
;; returns the REST of that line — as in Clojure.
;;
;; Forms are parsed by the host seam __parse-next-numbered (one form + the index
;; just past it, nil when only whitespace remains). A JVM *in* is a
;; LineNumberingPushbackReader, so these readers number their reads the same
;; way: each keeps the counters (__numbering) its buffer's index 0 stands at, a
;; read error escapes as a LispReader$ReaderException at the stream's line and
;; column, and an unclosed collection names the line it opened on. The readers hold a CURSOR
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
    "Next form plus the text consumed, trimmed as String.trim does, as
    [form text]. On EOF: throws, or returns [eof-value \"\"] when eof-error? is false.")
  (-read-eof [rdr]
    "Raises end of input, as a read of this reader does when it finds nothing:
    the LispReader$ReaderException standing past the input.")
  (-read-char [rdr]
    "Next character as a code point; -1 at end of input — java.io.Reader's
    .read contract, which clojure.main/repl-read walks the input with.")
  (-unread-char [rdr c]
    "Steps back over the character just read, so the next -read-char,
    -read-form or -read-line sees it again. A reader here holds a CURSOR into
    its input rather than a pushback buffer, so unlike .unread on a
    PushbackReader only the character just read can go back: c is the value
    -read-char returned, and the step is what actually undoes the read.")
  (-at-line-start? [rdr]
    "True before anything is read, and afterwards whether the last read ended
    a line or hit end of input — clojure.lang.LineNumberingPushbackReader's
    .atLineStart, which clojure.main/repl asks before re-prompting."))

;; The two slots behind -at-line-start?: what the reader answers now, and what
;; it answered before the last read, so an unread can put the first back — the
;; same pair the host PushbackReader shim keeps (host-static-classes.ss).
(defn- line-start! [a v] (swap! a (fn [st] [v (nth st 0)])) nil)
(defn- line-start-undo! [a] (swap! a (fn [st] [(nth st 1) (nth st 1)])) nil)

(defn __string-reader
  "A reader over string s (the with-in-str expansion calls this)."
  [s]
  (let [pos (atom 0)
        line-start (atom [true true])
        numbering (__numbering)]
    (reify IReader
      (-read-line [_]
        ;; \n, \r and \r\n all end a line, and none of them is part of it —
        ;; java.io.BufferedReader's rule, which the host seam owns so this
        ;; reader and the port-backed ones cannot disagree about CRLF input.
        (line-start! line-start true)
        (let [r (__string-line-from s @pos)]
          (when-not (nil? r)
            (reset! pos (nth r 1))
            (nth r 0))))
      (-read-form [_]
        (let [r (__parse-next-numbered s @pos numbering #(reset! pos %))]
          (if (nil? r)
            (do (line-start! line-start true) reader-eof)
            (do (reset! pos (nth r 1))
                (line-start! line-start false)
                (nth r 0)))))
      (-read+string [_ eof-error? eof-value]
        (let [p @pos
              r (__parse-next-numbered s p numbering #(reset! pos %))]
          (if (nil? r)
            (do (line-start! line-start true)
                (reset! pos (count s))
                (if eof-error?
                  (__read-eof s numbering)
                  [eof-value ""]))
            (do (reset! pos (nth r 1))
                (line-start! line-start false)
                [(nth r 0) (__jtrim (subs s p (nth r 1)))]))))
      (-read-eof [_] (__read-eof s numbering))
      (-read-char [_]
        (let [p @pos]
          (if (< p (count s))
            (let [c (int (get s p))]
              (reset! pos (inc p))
              (line-start! line-start (or (= c 10) (= c 13)))
              c)
            (do (line-start! line-start true) -1))))
      (-unread-char [_ _c]
        (swap! pos (fn [p] (if (pos? p) (dec p) p)))
        (line-start-undo! line-start)
        nil)
      (-at-line-start? [_] (nth @line-start 0)))))

;; Real stdin: a leftover [buffer cursor] shared by read and read-line. When a
;; parse needs another line, the consumed prefix is dropped as the line is
;; appended, so the buffer only ever holds the unread tail — a single form
;; still arriving pays a copy and a re-parse per line (the parse seam is not
;; incremental), but cost is bounded by that form, never the whole session.
(def ^:private stdin-buf (atom ["" 0]))
(def ^:private stdin-line-start (atom [true true]))
;; the counters index 0 of stdin-buf stands at: whatever the buffer drops, and
;; every line read around it, moves them on
(def ^:private stdin-numbering (__numbering))
(defn- stdin-drop! [s p] (__numbering-advance! stdin-numbering s 0 p))
(defn- stdin-line! [line]
  (when-not (nil? line)
    (let [l (str line "\n")] (__numbering-advance! stdin-numbering l 0 (count l))))
  line)

(def ^:dynamic *in*
  (reify IReader
    (-read-line [_]
      (line-start! stdin-line-start true)
      (let [sp @stdin-buf
            s (nth sp 0)
            p (nth sp 1)]
        (if (< p (count s))
          (let [i (str-find "\n" s p)]
            (if (nil? i)
              (do (stdin-drop! s (count s)) (reset! stdin-buf ["" 0]) (subs s p))
              (do (reset! stdin-buf [s (inc i)]) (subs s p i))))
          (do (stdin-drop! s (count s))
              (reset! stdin-buf ["" 0])
              (stdin-line! (__stdin-read-line))))))
    (-read-form [_]
      (loop []
        (let [sp @stdin-buf
              s (nth sp 0)
              p (nth sp 1)
              r (__parse-next-numbered s p stdin-numbering #(reset! stdin-buf [s %]))]
          (if (nil? r)
            (let [line (__stdin-read-line)]
              (if (nil? line)
                (do (line-start! stdin-line-start true) reader-eof)
                (do (stdin-drop! s p)
                    (reset! stdin-buf [(str (subs s p) line "\n") 0]) (recur))))
            (do (reset! stdin-buf [s (nth r 1)])
                (line-start! stdin-line-start false)
                (nth r 0))))))
    (-read+string [_ eof-error? eof-value]
      (loop []
        (let [sp @stdin-buf
              s (nth sp 0)
              p (nth sp 1)
              r (__parse-next-numbered s p stdin-numbering #(reset! stdin-buf [s %]))]
          (if (nil? r)
            (let [line (__stdin-read-line)]
              (if (nil? line)
                (do (line-start! stdin-line-start true)
                    (if eof-error?
                      (__read-eof s stdin-numbering)
                      [eof-value ""]))
                (do (stdin-drop! s p)
                    (reset! stdin-buf [(str (subs s p) line "\n") 0]) (recur))))
            (do (reset! stdin-buf [s (nth r 1)])
                (line-start! stdin-line-start false)
                [(nth r 0) (__jtrim (subs s p (nth r 1)))])))))
    (-read-eof [_] (__read-eof (nth @stdin-buf 0) stdin-numbering))
    ;; A character comes off the same buffer the form and line ops read, so the
    ;; three interleave: clojure.main/repl-read skips whitespace character by
    ;; character, unreads the one that ended the skip, and then reads a form
    ;; from the very next position. The pull when the buffer runs dry blocks on
    ;; a line, as .read on the JVM's stdin reader blocks on input.
    (-read-char [_]
      (loop []
        (let [sp @stdin-buf
              s (nth sp 0)
              p (nth sp 1)]
          (if (< p (count s))
            (let [c (int (get s p))]
              (reset! stdin-buf [s (inc p)])
              (line-start! stdin-line-start (or (= c 10) (= c 13)))
              c)
            (let [line (__stdin-read-line)]
              (if (nil? line)
                (do (line-start! stdin-line-start true) -1)
                (do (stdin-drop! s (count s))
                    (reset! stdin-buf [(str line "\n") 0]) (recur))))))))
    (-unread-char [_ _c]
      (let [sp @stdin-buf
            p (nth sp 1)]
        (when (pos? p)
          (reset! stdin-buf [(nth sp 0) (dec p)])))
      (line-start-undo! stdin-line-start)
      nil)
    (-at-line-start? [_] (nth @stdin-line-start 0))))

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
       (-read-eof stream)
       v)))
  ([opts stream]
   (let [v (-read-form stream)]
     (if (= v reader-eof)
       (if (contains? opts :eof)
         (get opts :eof)
         (-read-eof stream))
       v)))
  ([stream eof-error? eof-value]
   (let [v (-read-form stream)]
     (if (= v reader-eof)
       (if eof-error? (-read-eof stream) eof-value)
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

;; The reference's namespace-map lifting (core_print.clj), private there and
;; here: clojure.pprint's map printer reaches it as #'clojure.core/lift-ns, the
;; way the JVM's does. jolt's own printer lifts in the host (printing.ss).
(defn- strip-ns
  [named]
  (if (symbol? named)
    (symbol nil (name named))
    (keyword nil (name named))))

(defn- lift-ns
  "Returns [lifted-ns lifted-kvs] or nil if m can't be lifted."
  [m]
  (when *print-namespace-maps*
    (loop [ns nil
           [[k v :as entry] & entries] (seq m)
           kvs []]
      (if entry
        (when (qualified-ident? k)
          (if ns
            (when (= ns (namespace k))
              (recur ns entries (conj kvs [(strip-ns k) v])))
            (when-let [new-ns (namespace k)]
              (recur new-ns entries (conj kvs [(strip-ns k) v])))))
        [ns kvs]))))

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

;; StackTraceElement->vec — [class method file line], class and method as
;; symbols, as the reference returns them.
(defn StackTraceElement->vec [o]
  [(symbol (.getClassName o)) (symbol (.getMethodName o)) (.getFileName o) (.getLineNumber o)])

;; The reference's Inst protocol (core.clj): inst-ms* is its one method, inst-ms
;; calls it, inst? is satisfies?. So a type that extends Inst is an inst to
;; both, which the host instance checks these replaced could not answer, and a
;; miss is the protocol's own message. java.util.Date is the class every #inst
;; and java.sql date value reports here; java.time.Instant is extended by the
;; java.time provider when it loads (stdlib/jolt/time/instant.clj), the way
;; core_instant18.clj does it on the JVM.
(defprotocol Inst
  (inst-ms* [inst]))

(extend-protocol Inst
  java.util.Date
  (inst-ms* [inst] (.getTime ^java.util.Date inst)))

(defn inst-ms [inst] (inst-ms* inst))
(defn inst? [x] (satisfies? Inst x))
