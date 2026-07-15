;; clojure.pprint — a column-aware pretty printer and a Common Lisp compatible
;; cl-format. The writer accumulates into a StringBuilder; pprint/write/cl-format
;; bind *out* (this ns's own dynamic binding) to a pretty-writer over it and emit
;; the result through clojure.core/print, so with-out-str captures it.
;;
;; Native print is not involved: this ns defines its own print/pr/prn/println
;; that route through (-write *out* ...).
(ns clojure.pprint
  (:refer-clojure :exclude [deftype print println pr prn]))

;;======================================================================
;; Macros (must precede their first use)
;;======================================================================

(defmacro getf
  "Get the value of the field named by the keyword sym from *out*'s field atom."
  [sym]
  `(~sym (deref (.-fields ~'this))))

(defmacro setf
  "Set the value of field sym in *out*'s field atom."
  [sym new-val]
  `(swap! (.-fields ~'this) assoc ~sym ~new-val))

(defmacro deftype
  "Builds a defrecord plus make-X / X? helpers (a tagged record used as a
  pretty-printer buffer token)."
  [type-name & fields]
  (let [name-str (name type-name)
        fields (map (comp symbol name) fields)]
    `(do
       (defrecord ~type-name [~'type-tag ~@fields])
       (defn- ~(symbol (str "make-" name-str))
         ~(vec fields)
         (~(symbol (str "->" type-name)) ~(keyword name-str) ~@fields))
       (defn- ~(symbol (str name-str "?")) [x#] (= (:type-tag x#) ~(keyword name-str))))))

(defmacro pprint-logical-block
  "Execute body as a pretty-printing logical block, output to *out* which must be
  a pretty-printing writer. Options :prefix, :per-line-prefix and :suffix may
  precede the body."
  [& args]
  (let [[options body] (loop [body args acc []]
                         (if (#{:prefix :per-line-prefix :suffix} (first body))
                           (recur (drop 2 body) (concat acc (take 2 body)))
                           [(apply hash-map acc) body]))]
    `(do (if (clojure.pprint/level-exceeded)
           (-write clojure.core/*out* "#")
           (clojure.core/binding [clojure.pprint/*current-level* (inc clojure.pprint/*current-level*)
                                  clojure.pprint/*current-length* 0]
             (clojure.pprint/start-block clojure.core/*out*
                                        ~(:prefix options)
                                        ~(:per-line-prefix options)
                                        ~(:suffix options))
             ~@body
             (clojure.pprint/end-block clojure.core/*out*)))
         nil)))

(defmacro print-length-loop
  "A loop for pretty-printer dispatch functions. Stops after *print-length*
  items (if set), printing a single \"...\" as an extra element and terminating."
  [bindings & body]
  `(loop ~bindings
     (if (and clojure.pprint/*current-length*
              clojure.pprint/*print-length*
              (>= clojure.pprint/*current-length* clojure.pprint/*print-length*))
       (do (-write *out* "...") nil)
       (do ~@body))))

(defmacro formatter-out
  "Returns a function (fn [& args]) that runs the compiled format against *out*.
  format-in is a control string or a previously compiled format."
  [format-in]
  `(let [format-in# ~format-in
         cf# (if (string? format-in#) (clojure.pprint/cached-compile format-in#) format-in#)]
     (fn [& args#]
       (let [navigator# (clojure.pprint/init-navigator args#)]
         (clojure.pprint/execute-format cf# navigator#)))))

(defmacro formatter
  "Returns a function (fn [stream & args]) that runs the compiled format."
  [format-in]
  `(let [format-in# ~format-in
         cf# (if (string? format-in#) (clojure.pprint/cached-compile format-in#) format-in#)]
     (fn [stream# & args#]
       (let [navigator# (clojure.pprint/init-navigator args#)]
         (clojure.pprint/execute-format stream# cf# navigator#)))))

(defmacro with-pprint-dispatch
  "Execute body with the pretty-print dispatch function bound to function. A
  non-function dispatch (e.g. a keyword) is accepted and ignored."
  [function & body]
  `(clojure.core/binding [clojure.pprint/*print-pprint-dispatch* ~function]
     ~@body))

(defmacro pp
  "Pretty print the last REPL result. (jolt has no *1, so this is a no-op stub.)"
  [] `nil)

;;======================================================================
;; print fns that route through *out*
;;======================================================================

(defn- print [& more]
  (-write *out* (apply print-str more)))

(defn- println [& more]
  (apply print more)
  (-write *out* "\n"))

(defn- print-char [c]
  (-write *out* (condp = c
                  \backspace "\\backspace"
                  \space "\\space"
                  \tab "\\tab"
                  \newline "\\newline"
                  \formfeed "\\formfeed"
                  \return "\\return"
                  \" "\\\""
                  \\ "\\\\"
                  (str "\\" c))))

(defn- ^:dynamic pr [& more]
  (-write *out* (apply pr-str more)))

(defn- prn [& more]
  (apply pr more)
  (-write *out* "\n"))

;;======================================================================
;; utils
;;======================================================================

(defn- float? [n]
  (and (number? n)
       (not (integer? n))))

(defn- char-code [c]
  (cond
    (number? c) c
    (char? c) (int c)
    (and (string? c) (= (count c) 1)) (int (.charAt c 0))
    :else (throw (Exception. "Argument to char must be a character or number"))))

(defn- map-passing-context [func initial-context lis]
  (loop [context initial-context
         lis lis
         acc []]
    (if (empty? lis)
      [acc context]
      (let [this (first lis)
            remainder (next lis)
            [result new-context] (apply func [this context])]
        (recur new-context remainder (conj acc result))))))

(defn- consume [func initial-context]
  (loop [context initial-context
         acc []]
    (let [[result new-context] (apply func [context])]
      (if (not result)
        [acc new-context]
        (recur new-context (conj acc result))))))

(defn- unzip-map [m]
  [(into {} (for [[k [v1 v2]] m] [k v1]))
   (into {} (for [[k [v1 v2]] m] [k v2]))])

(defn- tuple-map [m v1]
  (into {} (for [[k v] m] [k [v v1]])))

(defn- rtrim [s c]
  (let [len (count s)]
    (if (and (pos? len) (= (nth s (dec (count s))) c))
      (loop [n (dec len)]
        (cond
          (neg? n) ""
          (not (= (nth s n) c)) (subs s 0 (inc n))
          true (recur (dec n))))
      s)))

(defn- ltrim [s c]
  (let [len (count s)]
    (if (and (pos? len) (= (nth s 0) c))
      (loop [n 0]
        (if (or (= n len) (not (= (nth s n) c)))
          (subs s n)
          (recur (inc n))))
      s)))

(defn- prefix-count [aseq val]
  (let [test (if (coll? val) (set val) #{val})]
    (loop [pos 0]
      (if (or (= pos (count aseq)) (not (test (nth aseq pos))))
        pos
        (recur (inc pos))))))

(defprotocol IPrettyFlush
  (-ppflush [pp]))

(defprotocol IPrettyWriter
  (-write [w x])
  (-pflush [w]))

;;======================================================================
;; column writer
;;======================================================================

(def ^:dynamic ^{:private true} *default-page-width* 72)

(defn- get-field [this sym]
  (sym (deref (.-fields this))))

(defn- set-field [this sym new-val]
  (swap! (.-fields this) assoc sym new-val))

(defn- get-column [this]
  (get-field this :cur))

(defn- get-line [this]
  (get-field this :line))

(defn- get-max-column [this]
  (get-field this :max))

(defn- set-max-column [this new-max]
  (set-field this :max new-max)
  nil)

(defn- get-writer [this]
  (get-field this :base))

(defn- c-write-char [this c]
  (if (= c \newline)
    (do
      (set-field this :cur 0)
      (set-field this :line (inc (get-field this :line))))
    (set-field this :cur (inc (get-field this :cur))))
  (-write (get-field this :base) c))

;; the base sink: accumulates into a StringBuilder. Columns are tracked by the
;; column-writer that wraps it; this just appends.
(defrecord StringBufferWriter [sb]
  IPrettyWriter
  (-write [_ x]
    (.append sb (if (char? x) (str x) x))
    nil)
  (-pflush [_] nil)
  IPrettyFlush
  (-ppflush [_] nil))

(defrecord ColumnWriter [fields]
  IPrettyWriter
  (-write [this x]
    (cond
      (string? x)
      (let [s x
            nl (.lastIndexOf s "\n")]
        (if (neg? nl)
          (set-field this :cur (+ (get-field this :cur) (count s)))
          (do
            (set-field this :cur (- (count s) nl 1))
            (set-field this :line (+ (get-field this :line)
                                     (count (filter #(= % \newline) s))))))
        (-write (get-field this :base) s))
      (or (char? x) (number? x))
      (c-write-char this x)))
  (-pflush [this] (-pflush (get-field this :base)))
  IPrettyFlush
  (-ppflush [_] nil))

(defn- column-writer
  ([writer] (column-writer writer *default-page-width*))
  ([writer max-columns]
   (->ColumnWriter (atom {:max max-columns :cur 0 :line 0 :base writer}))))

;;======================================================================
;; pretty writer
;;======================================================================

(declare ^{:arglists '([this])} get-miser-width)

(defrecord logical-block
  [parent section start-col indent
   done-nl intra-block-nl
   prefix per-line-prefix suffix
   logical-block-callback])

(defn- ancestor? [parent child]
  (loop [child (:parent child)]
    (cond
      (nil? child) false
      (identical? parent child) true
      :else (recur (:parent child)))))

(defn- buffer-length [l]
  (let [l (seq l)]
    (if l
      (- (:end-pos (last l)) (:start-pos (first l)))
      0)))

(deftype buffer-blob :data :trailing-white-space :start-pos :end-pos)
(deftype nl-t :type :logical-block :start-pos :end-pos)
(deftype start-block-t :logical-block :start-pos :end-pos)
(deftype end-block-t :logical-block :start-pos :end-pos)
(deftype indent-t :logical-block :relative-to :offset :start-pos :end-pos)

(def ^:private pp-newline (fn [] "\n"))

(declare emit-nl)

(defmulti ^{:private true} write-token (fn [this token] (:type-tag token)))

(defmethod write-token :start-block-t [this token]
  (when-let [cb (getf :logical-block-callback)] (cb :start))
  (let [lb (:logical-block token)]
    (when-let [prefix (:prefix lb)]
      (-write (getf :base) prefix))
    (let [col (get-column (getf :base))]
      (reset! (:start-col lb) col)
      (reset! (:indent lb) col))))

(defmethod write-token :end-block-t [this token]
  (when-let [cb (getf :logical-block-callback)] (cb :end))
  (when-let [suffix (:suffix (:logical-block token))]
    (-write (getf :base) suffix)))

(defmethod write-token :indent-t [this token]
  (let [lb (:logical-block token)]
    (reset! (:indent lb)
            (+ (:offset token)
               (condp = (:relative-to token)
                 :block @(:start-col lb)
                 :current (get-column (getf :base)))))))

(defmethod write-token :buffer-blob [this token]
  (-write (getf :base) (:data token)))

(defmethod write-token :nl-t [this token]
  (if (or (= (:type token) :mandatory)
          (and (not (= (:type token) :fill))
               @(:done-nl (:logical-block token))))
    (emit-nl this token)
    (if-let [tws (getf :trailing-white-space)]
      (-write (getf :base) tws)))
  (setf :trailing-white-space nil))

(defn- write-tokens [this tokens force-trailing-whitespace]
  ;; Trailing whitespace stays PENDING between tokens even when forced — the
  ;; next token decides its fate (an nl-t that emits a newline discards it, so
  ;; a buffered separator before a line break can't leak a trailing space).
  ;; Only whitespace still pending after the last token is force-written.
  (doseq [token tokens]
    (if-not (= (:type-tag token) :nl-t)
      (if-let [tws (getf :trailing-white-space)]
        (-write (getf :base) tws)))
    (write-token this token)
    (setf :trailing-white-space (:trailing-white-space token)))
  (let [tws (getf :trailing-white-space)]
    (when (and force-trailing-whitespace tws)
      (-write (getf :base) tws)
      (setf :trailing-white-space nil))))

(defn- tokens-fit? [this tokens]
  (let [maxcol (get-max-column (getf :base))]
    (or
      (nil? maxcol)
      (< (+ (get-column (getf :base)) (buffer-length tokens)) maxcol))))

(defn- linear-nl? [this lb section]
  (or @(:done-nl lb)
      (not (tokens-fit? this section))))

(defn- miser-nl? [this lb section]
  (let [miser-width (get-miser-width this)
        maxcol (get-max-column (getf :base))]
    (and miser-width maxcol
         (>= @(:start-col lb) (- maxcol miser-width))
         (linear-nl? this lb section))))

(defmulti ^{:private true} emit-nl? (fn [t _ _ _] (:type t)))

(defmethod emit-nl? :linear [newl this section _]
  (let [lb (:logical-block newl)]
    (linear-nl? this lb section)))

(defmethod emit-nl? :miser [newl this section _]
  (let [lb (:logical-block newl)]
    (miser-nl? this lb section)))

(defmethod emit-nl? :fill [newl this section subsection]
  (let [lb (:logical-block newl)]
    (or @(:intra-block-nl lb)
        (not (tokens-fit? this subsection))
        (miser-nl? this lb section))))

(defmethod emit-nl? :mandatory [_ _ _ _]
  true)

(defn- get-section [buffer]
  (let [nl (first buffer)
        lb (:logical-block nl)
        section (seq (take-while #(not (and (nl-t? %) (ancestor? (:logical-block %) lb)))
                                 (next buffer)))]
    [section (seq (drop (inc (count section)) buffer))]))

(defn- get-sub-section [buffer]
  (let [nl (first buffer)
        lb (:logical-block nl)
        section (seq (take-while #(let [nl-lb (:logical-block %)]
                                    (not (and (nl-t? %) (or (= nl-lb lb) (ancestor? nl-lb lb)))))
                                 (next buffer)))]
    section))

(defn- update-nl-state [lb]
  (reset! (:intra-block-nl lb) true)
  (reset! (:done-nl lb) true)
  (loop [lb (:parent lb)]
    (if lb
      (do (reset! (:done-nl lb) true)
          (reset! (:intra-block-nl lb) true)
          (recur (:parent lb))))))

(defn- emit-nl [this nl]
  (-write (getf :base) (pp-newline))
  (setf :trailing-white-space nil)
  (let [lb (:logical-block nl)
        prefix (:per-line-prefix lb)]
    (if prefix
      (-write (getf :base) prefix))
    (let [istr (apply str (repeat (- @(:indent lb) (count prefix)) \space))]
      (-write (getf :base) istr))
    (update-nl-state lb)))

(defn- split-at-newline [tokens]
  (let [pre (seq (take-while #(not (nl-t? %)) tokens))]
    [pre (seq (drop (count pre) tokens))]))

(defn- write-token-string [this tokens]
  (let [[a b] (split-at-newline tokens)]
    (if a (write-tokens this a false))
    (if b
      (let [[section remainder] (get-section b)
            newl (first b)]
        (let [do-nl (emit-nl? newl this section (get-sub-section b))
              result (if do-nl
                       (do
                         (emit-nl this newl)
                         (next b))
                       b)
              long-section (not (tokens-fit? this result))
              result (if long-section
                       (let [rem2 (write-token-string this section)]
                         (if (= rem2 section)
                           (do
                             (write-tokens this section false)
                             remainder)
                           (into [] (concat rem2 remainder))))
                       result)]
          result)))))

(defn- write-line [this]
  (loop [buffer (getf :buffer)]
    (setf :buffer (into [] buffer))
    (if (not (tokens-fit? this buffer))
      (let [new-buffer (write-token-string this buffer)]
        (if-not (identical? buffer new-buffer)
          (recur new-buffer))))))

(defn- add-to-buffer [this token]
  (setf :buffer (conj (getf :buffer) token))
  (if (not (tokens-fit? this (getf :buffer)))
    (write-line this)))

(defn- write-buffered-output [this]
  (write-line this)
  (if-let [buf (getf :buffer)]
    (do
      (write-tokens this buf true)
      (setf :buffer []))))

(defn- write-white-space [this]
  (when-let [tws (getf :trailing-white-space)]
    (-write (getf :base) tws)
    (setf :trailing-white-space nil)))

(defn- write-initial-lines [this s]
  (let [lines (clojure.string/split s #"\n" -1)]
    (if (= (count lines) 1)
      s
      (let [prefix (:per-line-prefix (first (getf :logical-blocks)))
            l (first lines)]
        (if (= :buffering (getf :mode))
          (let [oldpos (getf :pos)
                newpos (+ oldpos (count l))]
            (setf :pos newpos)
            (add-to-buffer this (make-buffer-blob l nil oldpos newpos))
            (write-buffered-output this))
          (do
            (write-white-space this)
            (-write (getf :base) l)))
        (-write (getf :base) "\n")
        (doseq [l (next (butlast lines))]
          (-write (getf :base) l)
          (-write (getf :base) (pp-newline))
          (if prefix
            (-write (getf :base) prefix)))
        (setf :buffering :writing)
        (last lines)))))

(defn- p-write-char [this c]
  (if (= (getf :mode) :writing)
    (do
      (write-white-space this)
      (-write (getf :base) c))
    (if (= c \newline)
      (write-initial-lines this "\n")
      (let [oldpos (getf :pos)
            newpos (inc oldpos)]
        (setf :pos newpos)
        (add-to-buffer this (make-buffer-blob (str c) nil oldpos newpos))))))

(defrecord PrettyWriter [fields]
  IPrettyWriter
  (-write [this x]
    (cond
      (string? x)
      (let [s0 (write-initial-lines this x)
            s (clojure.string/replace-first s0 #"\s+$" "")
            white-space (subs s0 (count s))
            mode (getf :mode)]
        (if (= mode :writing)
          (do
            (write-white-space this)
            (-write (getf :base) s)
            (setf :trailing-white-space white-space))
          (let [oldpos (getf :pos)
                newpos (+ oldpos (count s0))]
            (setf :pos newpos)
            (add-to-buffer this (make-buffer-blob s white-space oldpos newpos)))))
      (or (char? x) (number? x))
      (p-write-char this x)))
  (-pflush [this]
    (-ppflush this)
    (-pflush (getf :base)))
  IPrettyFlush
  (-ppflush [this]
    (if (= (getf :mode) :buffering)
      (do
        (write-tokens this (getf :buffer) true)
        (setf :buffer []))
      (write-white-space this))))

(defn- pretty-writer [writer max-columns miser-width]
  (let [lb (->logical-block nil nil (atom 0) (atom 0) (atom false) (atom false)
                            nil nil nil nil)]
    (->PrettyWriter
      (atom {:pretty-writer true
             :base (column-writer writer max-columns)
             :logical-blocks lb
             :sections nil
             :mode :writing
             :buffer []
             :buffer-block lb
             :buffer-level 1
             :miser-width miser-width
             :trailing-white-space nil
             :pos 0}))))

(defn- start-block
  [this prefix per-line-prefix suffix]
  (let [lb (->logical-block (getf :logical-blocks) nil (atom 0) (atom 0)
                            (atom false) (atom false)
                            prefix per-line-prefix suffix nil)]
    (setf :logical-blocks lb)
    (if (= (getf :mode) :writing)
      (do
        (write-white-space this)
        (when-let [cb (getf :logical-block-callback)] (cb :start))
        (if prefix
          (-write (getf :base) prefix))
        (let [col (get-column (getf :base))]
          (reset! (:start-col lb) col)
          (reset! (:indent lb) col)))
      (let [oldpos (getf :pos)
            newpos (+ oldpos (if prefix (count prefix) 0))]
        (setf :pos newpos)
        (add-to-buffer this (make-start-block-t lb oldpos newpos))))))

(defn- end-block [this]
  (let [lb (getf :logical-blocks)
        suffix (:suffix lb)]
    (if (= (getf :mode) :writing)
      (do
        (write-white-space this)
        (if suffix
          (-write (getf :base) suffix))
        (when-let [cb (getf :logical-block-callback)] (cb :end)))
      (let [oldpos (getf :pos)
            newpos (+ oldpos (if suffix (count suffix) 0))]
        (setf :pos newpos)
        (add-to-buffer this (make-end-block-t lb oldpos newpos))))
    (setf :logical-blocks (:parent lb))))

(defn- nl [this type]
  (setf :mode :buffering)
  (let [pos (getf :pos)]
    (add-to-buffer this (make-nl-t type (getf :logical-blocks) pos pos))))

(defn- indent [this relative-to offset]
  (let [lb (getf :logical-blocks)]
    (if (= (getf :mode) :writing)
      (do
        (write-white-space this)
        (reset! (:indent lb)
                (+ offset (condp = relative-to
                            :block @(:start-col lb)
                            :current (get-column (getf :base))))))
      (let [pos (getf :pos)]
        (add-to-buffer this (make-indent-t lb relative-to offset pos pos))))))

(defn- get-miser-width [this]
  (getf :miser-width))

;;======================================================================
;; pprint base
;;======================================================================

(def ^:dynamic *print-pretty* true)
(def ^:dynamic *print-pprint-dispatch* nil)
(def ^:dynamic *print-right-margin* 72)
(def ^:dynamic *print-miser-width* 40)
(def ^:dynamic ^{:private true} *print-lines* nil)
(def ^:dynamic ^{:private true} *print-circle* nil)
(def ^:dynamic ^{:private true} *print-shared* nil)
(def ^:dynamic *print-suppress-namespaces* nil)
(def ^:dynamic *print-radix* nil)
(def ^:dynamic *print-base* 10)
(def ^:dynamic ^{:private true} *current-level* 0)
(def ^:dynamic ^{:private true} *current-length* nil)

;; jolt has no bindable clojure.core/*print-length* / *print-level* vars; define
;; them here so the printer-control machinery can bind and read them.
(def ^:dynamic *print-length* nil)
(def ^:dynamic *print-level* nil)

(declare ^{:arglists '([n])} format-simple-number)

(defn- pretty-writer? [x]
  (and (instance? PrettyWriter x) (:pretty-writer (deref (.-fields x)))))

(defn- make-pretty-writer [base-writer right-margin miser-width]
  (pretty-writer base-writer right-margin miser-width))

(defmacro ^{:private true} with-pretty-writer [base-writer & body]
  `(let [base-writer# ~base-writer
         new-writer# (not (pretty-writer? base-writer#))]
     (clojure.core/binding [clojure.core/*out* (if new-writer#
                                                 (make-pretty-writer base-writer# *print-right-margin* *print-miser-width*)
                                                 base-writer#)]
       ;; route core print into this pretty-writer even under an outer
       ;; with-out-str; nested pr-str/print-str re-suppress so their captures work
       (clojure.core/__with-pprint-routing
         (fn []
           ~@body
           (-ppflush clojure.core/*out*))))))

(defn write-out
  "Write an object to *out* subject to the current bindings of the printer control
  variables. *out* must be a PrettyWriter when pretty printing is enabled."
  [object]
  (let [length-reached (and *current-length*
                            *print-length*
                            (>= *current-length* *print-length*))]
    (if-not *print-pretty*
      (pr object)
      (if length-reached
        (-write *out* "...")
        (do
          (if *current-length* (set! *current-length* (inc *current-length*)))
          (*print-pprint-dispatch* object))))
    length-reached))

(defn write
  "Write an object subject to the current bindings of the printer control
  variables. Returns the string result if :stream is nil, nil otherwise."
  [object & kw-args]
  (let [options (merge {:stream true} (apply hash-map kw-args))]
    (binding [clojure.pprint/*print-base* (get options :base clojure.pprint/*print-base*)
              clojure.pprint/*print-circle* (get options :circle clojure.pprint/*print-circle*)
              clojure.pprint/*print-length* (get options :length clojure.pprint/*print-length*)
              clojure.pprint/*print-level* (get options :level clojure.pprint/*print-level*)
              clojure.pprint/*print-lines* (get options :lines clojure.pprint/*print-lines*)
              clojure.pprint/*print-miser-width* (get options :miser-width clojure.pprint/*print-miser-width*)
              clojure.pprint/*print-pprint-dispatch* (get options :dispatch clojure.pprint/*print-pprint-dispatch*)
              clojure.pprint/*print-pretty* (get options :pretty clojure.pprint/*print-pretty*)
              clojure.pprint/*print-radix* (get options :radix clojure.pprint/*print-radix*)
              clojure.core/*print-readably* (get options :readably clojure.core/*print-readably*)
              clojure.pprint/*print-right-margin* (get options :right-margin clojure.pprint/*print-right-margin*)
              clojure.pprint/*print-suppress-namespaces* (get options :suppress-namespaces clojure.pprint/*print-suppress-namespaces*)]
      (let [sb (StringBuilder.)
            optval (if (contains? options :stream)
                     (:stream options)
                     true)
            base-writer (if (or (true? optval) (nil? optval))
                          (->StringBufferWriter sb)
                          optval)]
        (if *print-pretty*
          (with-pretty-writer base-writer
            (write-out object))
          (binding [*out* base-writer]
            (pr object)))
        (if (true? optval)
          (clojure.core/print (str sb)))
        (if (nil? optval)
          (str sb))))))

(defn pprint
  "Pretty print object. With one arg, prints to *out* (captured by with-out-str).
  The 2-arg form writes to the supplied pretty-writer."
  ([object]
   (let [sb (StringBuilder.)]
     (binding [*out* (->StringBufferWriter sb)]
       (pprint object *out*)
       (clojure.core/print (str sb)))))
  ([object writer]
   (with-pretty-writer writer
     (binding [*print-pretty* true]
       (write-out object))
     (if (not (= 0 (get-column *out*)))
       (-write *out* "\n")))))

(defn set-pprint-dispatch [function]
  (set! *print-pprint-dispatch* function)
  nil)

(defn- check-enumerated-arg [arg choices]
  (if-not (choices arg)
    (throw (Exception. (str "Bad argument: " arg ". It must be one of " choices)))))

(defn- level-exceeded []
  (and *print-level* (>= *current-level* *print-level*)))

(defn pprint-newline
  "Print a conditional newline (:linear :miser :fill or :mandatory) to *out*,
  which must be a pretty-printing writer."
  [kind]
  (check-enumerated-arg kind #{:linear :miser :fill :mandatory})
  (nl *out* kind))

(defn pprint-indent
  "Create an indent at this point in the pretty-printing stream. relative-to is
  :block or :current; n is an offset."
  [relative-to n]
  (check-enumerated-arg relative-to #{:block :current})
  (indent *out* relative-to n))

(defn pprint-tab
  [kind colnum colinc]
  (check-enumerated-arg kind #{:line :section :line-relative :section-relative})
  (throw (Exception. "pprint-tab is not yet implemented")))

;;======================================================================
;; cl-format
;;======================================================================

(declare ^{:arglists '([format-str])} compile-format)
(declare ^{:arglists '([stream format args] [format args])} execute-format)
(declare ^{:arglists '([s])} init-navigator)

(defn cl-format
  "A Common Lisp compatible format function. If writer is nil, returns the
  formatted string; if true, prints to *out*; otherwise writes to writer."
  [writer format-in & args]
  (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
        navigator (init-navigator args)]
    (execute-format writer compiled-format navigator)))

(def ^:dynamic ^{:private true} *format-str* nil)

(defn- format-error [message offset]
  (let [full-message (str message "\n" *format-str* "\n"
                          (apply str (repeat offset \space)) "^" "\n")]
    (throw (Exception. full-message))))

(defrecord ^{:private true} arg-navigator [seq rest pos])

(defn- init-navigator [s]
  (let [s (seq s)]
    (->arg-navigator s s 0)))

(defn- next-arg [navigator]
  (let [rst (:rest navigator)]
    (if rst
      [(first rst) (->arg-navigator (:seq navigator) (next rst) (inc (:pos navigator)))]
      (throw (Exception. "Not enough arguments for format definition")))))

(defn- next-arg-or-nil [navigator]
  (let [rst (:rest navigator)]
    (if rst
      [(first rst) (->arg-navigator (:seq navigator) (next rst) (inc (:pos navigator)))]
      [nil navigator])))

(defn- get-format-arg [navigator]
  (let [[raw-format navigator] (next-arg navigator)
        compiled-format (if (string? raw-format)
                          (compile-format raw-format)
                          raw-format)]
    [compiled-format navigator]))

(declare relative-reposition)

(defn- absolute-reposition [navigator position]
  (if (>= position (:pos navigator))
    (relative-reposition navigator (- position (:pos navigator)))
    (->arg-navigator (:seq navigator) (drop position (:seq navigator)) position)))

(defn- relative-reposition [navigator position]
  (let [newpos (+ (:pos navigator) position)]
    (if (neg? position)
      (absolute-reposition navigator newpos)
      (->arg-navigator (:seq navigator) (drop position (:rest navigator)) newpos))))

(defrecord ^{:private true} compiled-directive [func dirdef params offset])

(defn- realize-parameter [[param [raw-val offset]] navigator]
  (let [[real-param new-navigator]
        (cond
          (contains? #{:at :colon} param)
          [raw-val navigator]

          (= raw-val :parameter-from-args)
          (next-arg navigator)

          (= raw-val :remaining-arg-count)
          [(count (:rest navigator)) navigator]

          true
          [raw-val navigator])]
    [[param [real-param offset]] new-navigator]))

(defn- realize-parameter-list [parameter-map navigator]
  (let [[pairs new-navigator]
        (map-passing-context realize-parameter navigator parameter-map)]
    [(into {} pairs) new-navigator]))

(declare ^{:arglists '([base val])} opt-base-str)

(def ^{:private true}
  special-radix-markers {2 "#b" 8 "#o" 16 "#x"})

(defn- format-simple-number [n]
  (cond
    (integer? n) (if (= *print-base* 10)
                   (str n (if *print-radix* "."))
                   (str
                     (if *print-radix* (or (get special-radix-markers *print-base*) (str "#" *print-base* "r")))
                     (opt-base-str *print-base* n)))
    :else nil))

(defn- format-ascii [print-func params arg-navigator offsets]
  (let [[arg arg-navigator] (next-arg arg-navigator)
        base-output (or (format-simple-number arg) (print-func arg))
        base-width (count base-output)
        min-width (+ base-width (:minpad params))
        width (if (>= min-width (:mincol params))
                min-width
                (+ min-width
                   (* (+ (quot (- (:mincol params) min-width 1)
                               (:colinc params))
                         1)
                      (:colinc params))))
        chars (apply str (repeat (- width base-width) (:padchar params)))]
    (if (:at params)
      (print (str chars base-output))
      (print (str base-output chars)))
    arg-navigator))

(defn- integral? [x]
  (cond
    (integer? x) true
    (float? x) (= x (Math/floor x))
    :else false))

(defn- remainders [base val]
  (reverse
    (first
      (consume #(if (pos? %)
                  [(rem % base) (quot % base)]
                  [nil nil])
               val))))

(defn- base-str [base val]
  (if (zero? val)
    "0"
    (apply str
           (map
             #(if (< % 10) (char (+ (char-code \0) %)) (char (+ (char-code \a) (- % 10))))
             (remainders base val)))))

(defn- opt-base-str [base val]
  (base-str base val))

(defn- group-by* [unit lis]
  (reverse
    (first
      (consume (fn [x] [(seq (reverse (take unit x))) (seq (drop unit x))]) (reverse lis)))))

(defn- format-integer [base params arg-navigator offsets]
  (let [[arg arg-navigator] (next-arg arg-navigator)]
    (if (integral? arg)
      (let [neg (neg? arg)
            pos-arg (if neg (- arg) arg)
            raw-str (opt-base-str base pos-arg)
            group-str (if (:colon params)
                        (let [groups (map #(apply str %) (group-by* (:commainterval params) raw-str))
                              commas (repeat (count groups) (:commachar params))]
                          (apply str (next (interleave commas groups))))
                        raw-str)
            signed-str (cond
                         neg (str "-" group-str)
                         (:at params) (str "+" group-str)
                         true group-str)
            padded-str (if (< (count signed-str) (:mincol params))
                         (str (apply str (repeat (- (:mincol params) (count signed-str))
                                                 (:padchar params)))
                              signed-str)
                         signed-str)]
        (print padded-str))
      (format-ascii print-str {:mincol (:mincol params) :colinc 1 :minpad 0
                               :padchar (:padchar params) :at true}
                    (init-navigator [arg]) nil))
    arg-navigator))

;;======================================================================
;; real-number formatting (~F, ~$) — lifted from the JVM cl_format.clj. jolt is
;; JVM-like (real chars, ratios), so the JVM mantissa/exponent decomposition via
;; (.toString Double) ports directly. convert-ratio collapses to (double x); the
;; bigdec subnormal-precision fallback is a documented residual corner.
;;======================================================================

(defn- convert-ratio [x]
  (if (ratio? x) (double x) x))

(defn- float-parts-base [f]
  (let [s (.toLowerCase (.toString f))
        exploc (.indexOf s (int \e))
        dotloc (.indexOf s (int \.))]
    (if (neg? exploc)
      (if (neg? dotloc)
        [s (str (dec (count s)))]
        [(str (subs s 0 dotloc) (subs s (inc dotloc))) (str (dec dotloc))])
      (if (neg? dotloc)
        [(subs s 0 exploc) (subs s (inc exploc))]
        [(str (subs s 0 1) (subs s 2 exploc)) (subs s (inc exploc))]))))

(defn- float-parts [f]
  (let [[m e] (float-parts-base f)
        m1 (rtrim m \0)
        m2 (ltrim m1 \0)
        delta (- (count m1) (count m2))
        e (if (and (pos? (count e)) (= (nth e 0) \+)) (subs e 1) e)]
    (if (empty? m2)
      ["0" 0]
      [m2 (- (Long/parseLong e) delta)])))

(defn- inc-s [s]
  (let [len-1 (dec (count s))]
    (loop [i len-1]
      (cond
        (neg? i) (apply str "1" (repeat (inc len-1) "0"))
        (= \9 (nth s i)) (recur (dec i))
        :else (apply str (subs s 0 i)
                     (char (inc (int (nth s i))))
                     (repeat (- len-1 i) "0"))))))

(defn- round-str [m e d w]
  (if (or d w)
    (let [len (count m)
          w (if w (max 2 w))
          round-pos (cond
                      d (+ e d 1)
                      (>= e 0) (max (inc e) (dec w))
                      :else (+ w e))
          [m1 e1 round-pos len] (if (= round-pos 0)
                                  [(str "0" m) (inc e) 1 (inc len)]
                                  [m e round-pos len])]
      (if round-pos
        (if (neg? round-pos)
          ["0" 0 false]
          (if (> len round-pos)
            (let [round-char (nth m1 round-pos)
                  result (subs m1 0 round-pos)]
              (if (>= (int round-char) (int \5))
                (let [round-up-result (inc-s result)
                      expanded (> (count round-up-result) (count result))]
                  [(if expanded
                     (subs round-up-result 0 (dec (count round-up-result)))
                     round-up-result)
                   e1 expanded])
                [result e1 false]))
            [m e false]))
        [m e false]))
    [m e false]))

(defn- expand-fixed [m e d]
  (let [[m1 e1] (if (neg? e)
                  [(str (apply str (repeat (dec (- e)) \0)) m) -1]
                  [m e])
        len (count m1)
        target-len (if d (+ e1 d 1) (inc e1))]
    (if (< len target-len)
      (str m1 (apply str (repeat (- target-len len) \0)))
      m1)))

(defn- insert-decimal [m e]
  (if (neg? e)
    (str "." m)
    (let [loc (inc e)]
      (str (subs m 0 loc) "." (subs m loc)))))

(defn- get-fixed [m e d]
  (insert-decimal (expand-fixed m e d) e))

(defn- fixed-float [params navigator offsets]
  (let [w (:w params)
        d (:d params)
        [arg navigator] (next-arg navigator)
        [sign abs] (if (neg? arg) ["-" (- arg)] ["+" arg])
        abs (convert-ratio abs)
        [mantissa exp] (float-parts abs)
        scaled-exp (+ exp (:k params))
        add-sign (or (:at params) (neg? arg))
        append-zero (and (not d) (<= (dec (count mantissa)) scaled-exp))
        [rounded-mantissa scaled-exp expanded] (round-str mantissa scaled-exp
                                                          d (if w (- w (if add-sign 1 0))))
        fixed-repr (get-fixed rounded-mantissa (if expanded (inc scaled-exp) scaled-exp) d)
        fixed-repr (if (and w d
                            (>= d 1)
                            (= (nth fixed-repr 0) \0)
                            (= (nth fixed-repr 1) \.)
                            (> (count fixed-repr) (- w (if add-sign 1 0))))
                     (subs fixed-repr 1)
                     fixed-repr)
        prepend-zero (= (first fixed-repr) \.)]
    (if w
      (let [len (count fixed-repr)
            signed-len (if add-sign (inc len) len)
            prepend-zero (and prepend-zero (not (>= signed-len w)))
            append-zero (and append-zero (not (>= signed-len w)))
            full-len (if (or prepend-zero append-zero) (inc signed-len) signed-len)]
        (if (and (> full-len w) (:overflowchar params))
          (print (apply str (repeat w (:overflowchar params))))
          (print (str (apply str (repeat (- w full-len) (:padchar params)))
                      (if add-sign sign)
                      (if prepend-zero "0")
                      fixed-repr
                      (if append-zero "0")))))
      (print (str (if add-sign sign)
                  (if prepend-zero "0")
                  fixed-repr
                  (if append-zero "0"))))
    navigator))

(defn- dollar-float [params navigator offsets]
  (let [[arg navigator] (next-arg navigator)
        [mantissa exp] (float-parts (Math/abs (convert-ratio arg)))
        d (:d params)
        n (:n params)
        w (:w params)
        add-sign (or (:at params) (neg? arg))
        [rounded-mantissa scaled-exp expanded] (round-str mantissa exp d nil)
        fixed-repr (get-fixed rounded-mantissa (if expanded (inc scaled-exp) scaled-exp) d)
        full-repr (str (apply str (repeat (- n (.indexOf fixed-repr (int \.))) \0)) fixed-repr)
        full-len (+ (count full-repr) (if add-sign 1 0))]
    (print (str
             (if (and (:colon params) add-sign) (if (neg? arg) \- \+))
             (apply str (repeat (- w full-len) (:padchar params)))
             (if (and (not (:colon params)) add-sign) (if (neg? arg) \- \+))
             full-repr))
    navigator))

;;======================================================================
;; ~C character formatting
;;======================================================================

(def ^:private special-chars {8 "Backspace", 9 "Tab", 10 "Newline", 13 "Return", 32 "Space"})

(defn- pretty-character [params navigator offsets]
  (let [[c navigator] (next-arg navigator)
        as-int (int c)
        base-char (bit-and as-int 127)
        meta (bit-and as-int 128)
        special (get special-chars base-char)]
    (if (> meta 0) (print "Meta-"))
    (print (cond
             special special
             (< base-char 32) (str "Control-" (char (+ base-char 64)))
             (= base-char 127) "Control-?"
             :else (char base-char)))
    navigator))

(defn- readable-character [params navigator offsets]
  (let [[c navigator] (next-arg navigator)]
    (cond
      (= (:char-format params) \o) (cl-format true "\\o~3,'0o" (int c))
      (= (:char-format params) \u) (cl-format true "\\u~4,'0x" (int c))
      :else (pr c))
    navigator))

(defn- plain-character [params navigator offsets]
  (let [[c navigator] (next-arg navigator)]
    (print c)
    navigator))

;;======================================================================
;; ~R radix / english / roman. Roman (the tested path) is fully lifted; the
;; cardinal/ordinal English branches are a documented residual (format-error).
;;======================================================================

(def ^:private old-roman-table
  [["I" "II" "III" "IIII" "V" "VI" "VII" "VIII" "VIIII"]
   ["X" "XX" "XXX" "XXXX" "L" "LX" "LXX" "LXXX" "LXXXX"]
   ["C" "CC" "CCC" "CCCC" "D" "DC" "DCC" "DCCC" "DCCCC"]
   ["M" "MM" "MMM"]])

(def ^:private new-roman-table
  [["I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
   ["X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
   ["C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
   ["M" "MM" "MMM"]])

(defn- format-roman [table params navigator offsets]
  (let [[arg navigator] (next-arg navigator)]
    (if (and (number? arg) (> arg 0) (< arg 4000))
      (let [digits (remainders 10 arg)]
        (loop [acc []
               pos (dec (count digits))
               digits digits]
          (if (empty? digits)
            (print (apply str acc))
            (let [digit (first digits)]
              (recur (if (= 0 digit)
                       acc
                       (conj acc (nth (nth table pos) (dec digit))))
                     (dec pos)
                     (next digits))))))
      (format-integer 10
                      {:mincol 0, :padchar \space, :commachar \, :commainterval 3, :colon true}
                      (init-navigator [arg])
                      {:mincol 0, :padchar 0, :commachar 0, :commainterval 0}))
    navigator))

(defn- format-old-roman [params navigator offsets]
  (format-roman old-roman-table params navigator offsets))

(defn- format-new-roman [params navigator offsets]
  (format-roman new-roman-table params navigator offsets))

(defn- format-cardinal-english [_params _navigator _offsets]
  (format-error "cardinal-English ~R is not implemented (use ~@R for roman)" 0))

(defn- format-ordinal-english [_params _navigator _offsets]
  (format-error "ordinal-English ~:R is not implemented (use ~@R for roman)" 0))

;;======================================================================
;; ~( ~) case conversion. The JVM streams through a proxy Writer; jolt accumulates
;; into a StringBuilder, so we capture the clause output then apply the transform.
;;======================================================================

(defn- str-downcase [s] (clojure.string/lower-case s))
(defn- str-upcase [s] (clojure.string/upper-case s))

(defn- str-capitalize-words [s]
  (let [s (clojure.string/lower-case s)
        s (clojure.string/replace s #"\W\w" (fn [m] (clojure.string/upper-case m)))]
    (if (and (pos? (count s)) (re-find #"[a-zA-Z]" (subs s 0 1)))
      (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))
      s)))

(defn- str-init-cap [s]
  (let [s (clojure.string/lower-case s)]
    (loop [i 0]
      (cond
        (>= i (count s)) s
        (re-find #"[a-zA-Z]" (subs s i (inc i)))
        (str (subs s 0 i) (clojure.string/upper-case (subs s i (inc i))) (subs s (inc i)))
        :else (recur (inc i))))))

(defn- modify-case [transform params navigator offsets]
  (let [clause (first (:clauses params))
        sb (StringBuilder.)]
    (binding [*out* (->StringBufferWriter sb)]
      (execute-sub-format clause navigator (:base-args params)))
    (print (transform (str sb)))
    navigator))

;; Check to see if a result is an abort (~^) construct
(defn- abort? [context]
  (let [token (first context)]
    (or (= :up-arrow token) (= :colon-up-arrow token))))

(defn- execute-sub-format [format args base-args]
  (second
    (map-passing-context
      (fn [element context]
        (if (abort? context)
          [nil context]
          (let [[params args] (realize-parameter-list (:params element) context)
                [params offsets] (unzip-map params)
                params (assoc params :base-args base-args)]
            [nil (apply (:func element) [params args offsets])])))
      args
      format)))

;;----------------------------------------------------------------------
;; conditional ~[...~]
;;----------------------------------------------------------------------

(defn- choice-conditional [params arg-navigator offsets]
  (let [arg (:selector params)
        [arg navigator] (if arg [arg arg-navigator] (next-arg arg-navigator))
        clauses (:clauses params)
        clause (if (or (neg? arg) (>= arg (count clauses)))
                 (first (:else params))
                 (nth clauses arg))]
    (if clause
      (execute-sub-format clause navigator (:base-args params))
      navigator)))

(defn- boolean-conditional [params arg-navigator offsets]
  (let [[arg navigator] (next-arg arg-navigator)
        clauses (:clauses params)
        clause (if arg
                 (second clauses)
                 (first clauses))]
    (if clause
      (execute-sub-format clause navigator (:base-args params))
      navigator)))

(defn- check-arg-conditional [params arg-navigator offsets]
  (let [[arg navigator] (next-arg arg-navigator)
        clauses (:clauses params)
        clause (if arg (first clauses))]
    (if arg
      (if clause
        (execute-sub-format clause arg-navigator (:base-args params))
        arg-navigator)
      navigator)))

;;----------------------------------------------------------------------
;; iteration ~{...~}
;;----------------------------------------------------------------------

(defn- iterate-sublist [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause)
                             (get-format-arg navigator)
                             [param-clause navigator])
        [arg-list navigator] (next-arg navigator)
        args (init-navigator arg-list)]
    (loop [count 0
           args args
           last-pos -1]
      (if (and (not max-count) (= (:pos args) last-pos) (> count 1))
        (throw (Exception. "~{ construct not consuming any arguments: Infinite loop!")))
      (if (or (and (empty? (:rest args))
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [iter-result (execute-sub-format clause args (:base-args params))]
          (if (= :up-arrow (first iter-result))
            navigator
            (recur (inc count) iter-result (:pos args))))))))

(defn- iterate-list-of-sublists [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause)
                             (get-format-arg navigator)
                             [param-clause navigator])
        [arg-list navigator] (next-arg navigator)]
    (loop [count 0
           arg-list arg-list]
      (if (or (and (empty? arg-list)
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [iter-result (execute-sub-format
                            clause
                            (init-navigator (first arg-list))
                            (init-navigator (next arg-list)))]
          (if (= :colon-up-arrow (first iter-result))
            navigator
            (recur (inc count) (next arg-list))))))))

(defn- iterate-main-list [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause)
                             (get-format-arg navigator)
                             [param-clause navigator])]
    (loop [count 0
           navigator navigator
           last-pos -1]
      (if (and (not max-count) (= (:pos navigator) last-pos) (> count 1))
        (throw (Exception. "~@{ construct not consuming any arguments: Infinite loop!")))
      (if (or (and (empty? (:rest navigator))
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [iter-result (execute-sub-format clause navigator (:base-args params))]
          (if (= :up-arrow (first iter-result))
            (second iter-result)
            (recur
              (inc count) iter-result (:pos navigator))))))))

(defn- iterate-main-sublists [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause)
                             (get-format-arg navigator)
                             [param-clause navigator])]
    (loop [count 0
           navigator navigator]
      (if (or (and (empty? (:rest navigator))
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [[sublist navigator] (next-arg-or-nil navigator)
              iter-result (execute-sub-format clause (init-navigator sublist) navigator)]
          (if (= :colon-up-arrow (first iter-result))
            navigator
            (recur (inc count) navigator)))))))

;;----------------------------------------------------------------------
;; ~< ... ~> justification / logical block
;;----------------------------------------------------------------------

(declare ^{:arglists '([params navigator offsets])} format-logical-block)
(declare ^{:arglists '([params navigator offsets])} justify-clauses)

(defn- logical-block-or-justify [params navigator offsets]
  (if (:colon (:right-params params))
    (format-logical-block params navigator offsets)
    (justify-clauses params navigator offsets)))

(defn- render-clauses [clauses navigator base-navigator]
  (loop [clauses clauses
         acc []
         navigator navigator]
    (if (empty? clauses)
      [acc navigator]
      (let [clause (first clauses)
            [iter-result result-str] (let [sb (StringBuilder.)]
                                       (binding [*out* (->StringBufferWriter sb)]
                                         [(execute-sub-format clause navigator base-navigator)
                                          (str sb)]))]
        (if (= :up-arrow (first iter-result))
          [acc (second iter-result)]
          (recur (next clauses) (conj acc result-str) iter-result))))))

(defn- justify-clauses [params navigator offsets]
  (let [[[eol-str] new-navigator] (when-let [else (:else params)]
                                    (render-clauses else navigator (:base-args params)))
        navigator (or new-navigator navigator)
        [else-params new-navigator] (when-let [p (:else-params params)]
                                      (realize-parameter-list p navigator))
        navigator (or new-navigator navigator)
        min-remaining (or (first (:min-remaining else-params)) 0)
        max-columns (or (first (:max-columns else-params))
                        (get-max-column *out*))
        clauses (:clauses params)
        [strs navigator] (render-clauses clauses navigator (:base-args params))
        slots (max 1
                   (+ (dec (count strs)) (if (:colon params) 1 0) (if (:at params) 1 0)))
        chars (reduce + (map count strs))
        mincol (:mincol params)
        minpad (:minpad params)
        colinc (:colinc params)
        minout (+ chars (* slots minpad))
        result-columns (if (<= minout mincol)
                         mincol
                         (+ mincol (* colinc
                                      (+ 1 (quot (- minout mincol 1) colinc)))))
        total-pad (- result-columns chars)
        pad (max minpad (quot total-pad slots))
        extra-pad (- total-pad (* pad slots))
        pad-str (apply str (repeat pad (:padchar params)))]
    (if (and eol-str (> (+ (get-column (:base (deref (.-fields *out*)))) min-remaining result-columns)
                        max-columns))
      (print eol-str))
    (loop [slots slots
           extra-pad extra-pad
           strs strs
           pad-only (or (:colon params)
                        (and (= (count strs) 1) (not (:at params))))]
      (if (seq strs)
        (do
          (print (str (if (not pad-only) (first strs))
                      (if (or pad-only (next strs) (:at params)) pad-str)
                      (if (pos? extra-pad) (:padchar params))))
          (recur
            (dec slots)
            (dec extra-pad)
            (if pad-only strs (next strs))
            false))))
    navigator))

;;----------------------------------------------------------------------
;; ~T tabulation, ~& fresh-line, get-pretty-writer
;;----------------------------------------------------------------------

(defn get-pretty-writer
  "Returns writer wrapped in a pretty writer unless it already is one."
  [writer]
  (if (pretty-writer? writer)
    writer
    (pretty-writer writer *print-right-margin* *print-miser-width*)))

(defn fresh-line []
  (if (instance? PrettyWriter *out*)
    (if (not (= 0 (get-column (:base (deref (.-fields *out*))))))
      (prn))
    (prn)))

(defn- absolute-tabulation [params navigator offsets]
  (let [colnum (:colnum params)
        colinc (:colinc params)
        current (get-column (:base (deref (.-fields *out*))))
        space-count (cond
                      (< current colnum) (- colnum current)
                      (= colinc 0) 0
                      :else (- colinc (rem (- current colnum) colinc)))]
    (print (apply str (repeat space-count \space))))
  navigator)

(defn- relative-tabulation [params navigator offsets]
  (let [colrel (:colnum params)
        colinc (:colinc params)
        start-col (+ colrel (get-column (:base (deref (.-fields *out*)))))
        offset (if (pos? colinc) (rem start-col colinc) 0)
        space-count (+ colrel (if (= 0 offset) 0 (- colinc offset)))]
    (print (apply str (repeat space-count \space))))
  navigator)

;;----------------------------------------------------------------------
;; ~< ... ~:> logical block, ~_ newline, ~I indent
;;----------------------------------------------------------------------

(defn- format-logical-block [params navigator offsets]
  (let [clauses (:clauses params)
        clause-count (count clauses)
        prefix (cond
                 (> clause-count 1) (:string (:params (first (first clauses))))
                 (:colon params) "(")
        body (nth clauses (if (> clause-count 1) 1 0))
        suffix (cond
                 (> clause-count 2) (:string (:params (first (nth clauses 2))))
                 (:colon params) ")")
        [arg navigator] (next-arg navigator)]
    (pprint-logical-block :prefix prefix :suffix suffix
      (execute-sub-format
        body
        (init-navigator arg)
        (:base-args params)))
    navigator))

(defn- set-indent [params navigator offsets]
  (let [relative-to (if (:colon params) :current :block)]
    (pprint-indent relative-to (:n params))
    navigator))

(defn- conditional-newline [params navigator offsets]
  (let [kind (if (:colon params)
               (if (:at params) :mandatory :fill)
               (if (:at params) :miser :linear))]
    (pprint-newline kind)
    navigator))

;;----------------------------------------------------------------------
;; directive table
;;----------------------------------------------------------------------

(defmacro ^{:private true} defdirectives [& directives]
  (let [process (fn [[char params flags bracket-info & generator-fn]]
                  [char
                   {:directive char
                    :params `(array-map ~@params)
                    :flags flags
                    :bracket-info bracket-info
                    :generator-fn (concat '(fn [params offset]) generator-fn)}])]
    `(def ^{:private true}
       ~'directive-table (hash-map ~@(mapcat process directives)))))

(defdirectives
  (\A
    [:mincol [0 Long] :colinc [1 Long] :minpad [0 Long] :padchar [\space Character]]
    #{:at :colon :both} {}
    #(format-ascii print-str %1 %2 %3))

  (\S
    [:mincol [0 Long] :colinc [1 Long] :minpad [0 Long] :padchar [\space Character]]
    #{:at :colon :both} {}
    #(format-ascii pr-str %1 %2 %3))

  (\D
    [:mincol [0 Long] :padchar [\space Character] :commachar [\, Character]
     :commainterval [3 Long]]
    #{:at :colon :both} {}
    #(format-integer 10 %1 %2 %3))

  (\B
    [:mincol [0 Long] :padchar [\space Character] :commachar [\, Character]
     :commainterval [3 Long]]
    #{:at :colon :both} {}
    #(format-integer 2 %1 %2 %3))

  (\O
    [:mincol [0 Long] :padchar [\space Character] :commachar [\, Character]
     :commainterval [3 Long]]
    #{:at :colon :both} {}
    #(format-integer 8 %1 %2 %3))

  (\X
    [:mincol [0 Long] :padchar [\space Character] :commachar [\, Character]
     :commainterval [3 Long]]
    #{:at :colon :both} {}
    #(format-integer 16 %1 %2 %3))

  (\R
    [:base [nil Long] :mincol [0 Long] :padchar [\space Character] :commachar [\, Character]
     :commainterval [3 Long]]
    #{:at :colon :both} {}
    (do
      (cond
        (first (:base params)) #(format-integer (:base %1) %1 %2 %3)
        (and (:at params) (:colon params)) format-old-roman
        (:at params) format-new-roman
        (:colon params) format-ordinal-english
        true format-cardinal-english)))

  (\C
    [:char-format [nil Character]]
    #{:at :colon :both} {}
    (cond
      (:colon params) pretty-character
      (:at params) readable-character
      :else plain-character))

  (\F
    [:w [nil Long] :d [nil Long] :k [0 Long] :overflowchar [nil Character]
     :padchar [\space Character]]
    #{:at} {}
    fixed-float)

  (\$
    [:d [2 Long] :n [1 Long] :w [0 Long] :padchar [\space Character]]
    #{:at :colon :both} {}
    dollar-float)

  (\%
    [:count [1 Long]]
    #{} {}
    (fn [params arg-navigator offsets]
      (dotimes [i (:count params)]
        (prn))
      arg-navigator))

  (\&
    [:count [1 Long]]
    #{:pretty} {}
    (fn [params arg-navigator offsets]
      (let [cnt (:count params)]
        (if (pos? cnt) (fresh-line))
        (dotimes [i (dec cnt)]
          (prn)))
      arg-navigator))

  (\|
    [:count [1 Long]]
    #{} {}
    (fn [params arg-navigator offsets]
      (dotimes [i (:count params)]
        (print \formfeed))
      arg-navigator))

  (\~
    [:n [1 Long]]
    #{} {}
    (fn [params arg-navigator offsets]
      (let [n (:n params)]
        (print (apply str (repeat n \~)))
        arg-navigator)))

  (\newline
    []
    #{:colon :at} {}
    (fn [params arg-navigator offsets]
      (if (:at params)
        (prn))
      arg-navigator))

  (\T
    [:colnum [1 Long] :colinc [1 Long]]
    #{:at :pretty} {}
    (if (:at params)
      #(relative-tabulation %1 %2 %3)
      #(absolute-tabulation %1 %2 %3)))

  (\*
    [:n [1 Long]]
    #{:colon :at} {}
    (fn [params navigator offsets]
      (let [n (:n params)]
        (if (:at params)
          (absolute-reposition navigator n)
          (relative-reposition navigator (if (:colon params) (- n) n))))))

  (\(
    []
    #{:colon :at :both} {:right \), :allows-separator nil, :else nil}
    (let [transform (cond
                      (and (:at params) (:colon params)) str-upcase
                      (:colon params) str-capitalize-words
                      (:at params) str-init-cap
                      :else str-downcase)]
      #(modify-case transform %1 %2 %3)))

  (\) [] #{} {} nil)

  (\?
    []
    #{:at} {}
    (if (:at params)
      (fn [params navigator offsets]
        (let [[subformat navigator] (get-format-arg navigator)]
          (execute-sub-format subformat navigator (:base-args params))))
      (fn [params navigator offsets]
        (let [[subformat navigator] (get-format-arg navigator)
              [subargs navigator] (next-arg navigator)
              sub-navigator (init-navigator subargs)]
          (execute-sub-format subformat sub-navigator (:base-args params))
          navigator))))

  (\) [] #{} {} nil)

  (\[
    [:selector [nil Long]]
    #{:colon :at} {:right \], :allows-separator true, :else :last}
    (cond
      (:colon params)
      boolean-conditional

      (:at params)
      check-arg-conditional

      true
      choice-conditional))

  (\; [:min-remaining [nil Long] :max-columns [nil Long]]
    #{:colon} {:separator true} nil)

  (\] [] #{} {} nil)

  (\{
    [:max-iterations [nil Long]]
    #{:colon :at :both} {:right \}, :allows-separator false}
    (cond
      (and (:at params) (:colon params))
      iterate-main-sublists

      (:colon params)
      iterate-list-of-sublists

      (:at params)
      iterate-main-list

      true
      iterate-sublist))

  (\} [] #{:colon} {} nil)

  (\<
    [:mincol [0 Long] :colinc [1 Long] :minpad [0 Long] :padchar [\space Character]]
    #{:colon :at :both :pretty} {:right \>, :allows-separator true, :else :first}
    logical-block-or-justify)

  (\> [] #{:colon} {} nil)

  (\^ [:arg1 [nil Long] :arg2 [nil Long] :arg3 [nil Long]]
    #{:colon} {}
    (fn [params navigator offsets]
      (let [arg1 (:arg1 params)
            arg2 (:arg2 params)
            arg3 (:arg3 params)
            exit (if (:colon params) :colon-up-arrow :up-arrow)]
        (cond
          (and arg1 arg2 arg3)
          (if (<= arg1 arg2 arg3) [exit navigator] navigator)

          (and arg1 arg2)
          (if (= arg1 arg2) [exit navigator] navigator)

          arg1
          (if (= arg1 0) [exit navigator] navigator)

          true
          (if (if (:colon params)
                (empty? (:rest (:base-args params)))
                (empty? (:rest navigator)))
            [exit navigator] navigator)))))

  (\W
    []
    #{:at :colon :both :pretty} {}
    (if (or (:at params) (:colon params))
      (let [bindings (concat
                       (if (:at params) [:level nil :length nil] [])
                       (if (:colon params) [:pretty true] []))]
        (fn [params navigator offsets]
          (let [[arg navigator] (next-arg navigator)]
            (if (apply write arg bindings)
              [:up-arrow navigator]
              navigator))))
      (fn [params navigator offsets]
        (let [[arg navigator] (next-arg navigator)]
          (if (write-out arg)
            [:up-arrow navigator]
            navigator)))))

  (\_
    []
    #{:at :colon :both} {}
    conditional-newline)

  (\I
    [:n [0 Long]]
    #{:colon} {}
    set-indent))

;;----------------------------------------------------------------------
;; compiling format strings
;;----------------------------------------------------------------------

(def ^{:private true}
  param-pattern #"^([vV]|#|('.)|([+-]?\d+)|(?=,))")

(def ^{:private true}
  special-params #{:parameter-from-args :remaining-arg-count})

(defn- extract-param [[s offset saw-comma]]
  ;; param-pattern is ^-anchored; re-find returns [whole g1 g2 g3] (groups nil)
  ;; or nil. The whole match (possibly empty, from the comma lookahead) is the
  ;; token; its length advances the cursor.
  (let [param (re-find param-pattern s)
        token-str (if (vector? param) (first param) param)]
    (if token-str
      (let [len (count token-str)
            remainder (subs s len)
            new-offset (+ offset len)]
        (if (not (= \, (nth remainder 0 nil)))
          [[token-str offset] [remainder new-offset false]]
          [[token-str offset] [(subs remainder 1) (inc new-offset) true]]))
      (if saw-comma
        (format-error "Badly formed parameters in format directive" offset)
        [nil [s offset]]))))

(defn- extract-params [s offset]
  (consume extract-param [s offset false]))

(defn- translate-param [[p offset]]
  [(cond
     (= (count p) 0) nil
     (and (= (count p) 1) (contains? #{\v \V} (nth p 0))) :parameter-from-args
     (and (= (count p) 1) (= \# (nth p 0))) :remaining-arg-count
     (and (= (count p) 2) (= \' (nth p 0))) (nth p 1)
     true (Long/parseLong p))
   offset])

(def ^{:private true} flag-defs {\: :colon, \@ :at})

(defn- extract-flags [s offset]
  (consume
    (fn [[s offset flags]]
      (if (empty? s)
        [nil [s offset flags]]
        (let [flag (get flag-defs (first s))]
          (if flag
            (if (contains? flags flag)
              (format-error
                (str "Flag \"" (first s) "\" appears more than once in a directive")
                offset)
              [true [(subs s 1) (inc offset) (assoc flags flag [true offset])]])
            [nil [s offset flags]]))))
    [s offset {}]))

(defn- check-flags [dirdef flags]
  (let [allowed (:flags dirdef)]
    (if (and (not (:at allowed)) (:at flags))
      (format-error (str "\"@\" is an illegal flag for format directive \"" (:directive dirdef) "\"")
                    (nth (:at flags) 1)))
    (if (and (not (:colon allowed)) (:colon flags))
      (format-error (str "\":\" is an illegal flag for format directive \"" (:directive dirdef) "\"")
                    (nth (:colon flags) 1)))
    (if (and (not (:both allowed)) (:at flags) (:colon flags))
      (format-error (str "Cannot combine \"@\" and \":\" flags for format directive \""
                         (:directive dirdef) "\"")
                    (min (nth (:colon flags) 1) (nth (:at flags) 1))))))

(defn- map-params [dirdef params flags offset]
  (check-flags dirdef flags)
  (if (> (count params) (count (:params dirdef)))
    (format-error
      (cl-format
        nil
        "Too many parameters for directive \"~C\": ~D~:* ~[were~;was~:;were~] specified but only ~D~:* ~[are~;is~:;are~] allowed"
        (:directive dirdef) (count params) (count (:params dirdef)))
      (second (first params))))
  (doall
    (map #(let [val (first %1)]
            (if (not (or (nil? val) (contains? special-params val)
                         (instance? (second (second %2)) val)))
              (format-error (str "Parameter " (name (first %2))
                                 " has bad type in directive \"" (:directive dirdef) "\": "
                                 (class val))
                            (second %1))))
         params (:params dirdef)))

  (merge
    (into {}
          (reverse (for [[name [default]] (:params dirdef)] [name [default offset]])))
    (reduce #(apply assoc %1 %2) {} (filter #(first (nth % 1)) (zipmap (keys (:params dirdef)) params)))
    flags))

(defn- compile-directive [s offset]
  (let [[raw-params [rest offset]] (extract-params s offset)
        [_ [rest offset flags]] (extract-flags rest offset)
        directive (first rest)
        dirdef (if directive
                 (or (get directive-table (first (clojure.string/upper-case (str directive))))
                     (get directive-table directive)))
        params (if dirdef (map-params dirdef (map translate-param raw-params) flags offset))]
    (if (not directive)
      (format-error "Format string ended in the middle of a directive" offset))
    (if (not dirdef)
      (format-error (str "Directive \"" directive "\" is undefined") offset))
    [(->compiled-directive ((:generator-fn dirdef) params offset) dirdef params offset)
     (let [remainder (subs rest 1)
           offset (inc offset)
           trim? (and (= \newline (:directive dirdef))
                      (not (:colon params)))
           trim-count (if trim? (prefix-count remainder [\space \tab]) 0)
           remainder (subs remainder trim-count)
           offset (+ offset trim-count)]
       [remainder offset])]))

(defn- compile-raw-string [s offset]
  (->compiled-directive (fn [_ a _] (print s) a) nil {:string s} offset))

(defn- right-bracket [this] (:right (:bracket-info (:dirdef this))))
(defn- separator? [this] (:separator (:bracket-info (:dirdef this))))
(defn- else-separator? [this]
  (and (:separator (:bracket-info (:dirdef this)))
       (:colon (:params this))))

(declare ^{:arglists '([bracket-info offset remainder])} collect-clauses)

(defn- process-bracket [this remainder]
  (let [[subex remainder] (collect-clauses (:bracket-info (:dirdef this))
                                           (:offset this) remainder)]
    [(->compiled-directive
       (:func this) (:dirdef this)
       (merge (:params this) (tuple-map subex (:offset this)))
       (:offset this))
     remainder]))

(defn- process-clause [bracket-info offset remainder]
  (consume
    (fn [remainder]
      (if (empty? remainder)
        (format-error "No closing bracket found." offset)
        (let [this (first remainder)
              remainder (next remainder)]
          (cond
            (right-bracket this)
            (process-bracket this remainder)

            (= (:right bracket-info) (:directive (:dirdef this)))
            [nil [:right-bracket (:params this) nil remainder]]

            (else-separator? this)
            [nil [:else nil (:params this) remainder]]

            (separator? this)
            [nil [:separator nil nil remainder]]

            true
            [this remainder]))))
    remainder))

(defn- collect-clauses [bracket-info offset remainder]
  (second
    (consume
      (fn [[clause-map saw-else remainder]]
        (let [[clause [type right-params else-params remainder]]
              (process-clause bracket-info offset remainder)]
          (cond
            (= type :right-bracket)
            [nil [(merge-with concat clause-map
                              {(if saw-else :else :clauses) [clause]
                               :right-params right-params})
                  remainder]]

            (= type :else)
            (cond
              (:else clause-map)
              (format-error "Two else clauses (\"~:;\") inside bracket construction." offset)

              (not (:else bracket-info))
              (format-error "An else clause (\"~:;\") is in a bracket type that doesn't support it."
                            offset)

              (and (= :first (:else bracket-info)) (seq (:clauses clause-map)))
              (format-error
                "The else clause (\"~:;\") is only allowed in the first position for this directive."
                offset)

              true
              (if (= :first (:else bracket-info))
                [true [(merge-with concat clause-map {:else [clause] :else-params else-params})
                       false remainder]]
                [true [(merge-with concat clause-map {:clauses [clause]})
                       true remainder]]))

            (= type :separator)
            (cond
              saw-else
              (format-error "A plain clause (with \"~;\") follows an else clause (\"~:;\") inside bracket construction." offset)

              (not (:allows-separator bracket-info))
              (format-error "A separator (\"~;\") is in a bracket type that doesn't support it."
                            offset)

              true
              [true [(merge-with concat clause-map {:clauses [clause]})
                     false remainder]]))))
      [{:clauses []} false remainder])))

(defn- process-nesting [format]
  (first
    (consume
      (fn [remainder]
        (let [this (first remainder)
              remainder (next remainder)
              bracket (:bracket-info (:dirdef this))]
          (if (:right bracket)
            (process-bracket this remainder)
            [this remainder])))
      format)))

(defn- compile-format [format-str]
  (binding [*format-str* format-str]
    (process-nesting
      (first
        (consume
          (fn [[s offset]]
            (if (empty? s)
              [nil s]
              (let [tilde (.indexOf s "~")]
                (cond
                  (neg? tilde) [(compile-raw-string s offset) ["" (+ offset (count s))]]
                  (zero? tilde) (compile-directive (subs s 1) (inc offset))
                  true
                  [(compile-raw-string (subs s 0 tilde) offset) [(subs s tilde) (+ tilde offset)]]))))
          [format-str 0])))))

(defn- needs-pretty [format]
  (loop [format format]
    (if (empty? format)
      false
      (if (or (:pretty (:flags (:dirdef (first format))))
              (some needs-pretty (first (:clauses (:params (first format)))))
              (some needs-pretty (first (:else (:params (first format))))))
        true
        (recur (next format))))))

(defn- execute-format
  ([stream format args]
   (let [sb (StringBuilder.)
         real-stream (if (or (not stream) (true? stream))
                       (->StringBufferWriter sb)
                       stream)
         wrapped-stream (if (and (needs-pretty format)
                                 (not (pretty-writer? real-stream)))
                          (get-pretty-writer real-stream)
                          real-stream)]
     (binding [*out* wrapped-stream]
       (try
         (execute-format format args)
         (finally
           (if-not (identical? real-stream wrapped-stream)
             (-pflush wrapped-stream))))
       (cond
         (not stream) (str sb)
         (true? stream) (clojure.core/print (str sb))
         :else nil))))
  ([format args]
   (map-passing-context
     (fn [element context]
       (if (abort? context)
         [nil context]
         (let [[params args] (realize-parameter-list
                               (:params element) context)
               [params offsets] (unzip-map params)
               params (assoc params :base-args args)]
           [nil (apply (:func element) [params args offsets])])))
     args
     format)
   nil))

(def ^{:private true} cached-compile (memoize compile-format))

;;======================================================================
;; dispatch
;;======================================================================

(def ^{:private true} reader-macros
  {'quote "'"
   'var "#'"
   'clojure.core/deref "@"
   'clojure.core/unquote "~"})

(defn- pprint-reader-macro [alis]
  (let [macro-char (reader-macros (first alis))]
    (when (and macro-char (= 2 (count alis)))
      (-write *out* macro-char)
      (write-out (second alis))
      true)))

;;; simple dispatch

(defn- pprint-simple-list [alis]
  (pprint-logical-block :prefix "(" :suffix ")"
    (print-length-loop [alis (seq alis)]
      (when alis
        (write-out (first alis))
        (when (next alis)
          (-write *out* " ")
          (pprint-newline :linear)
          (recur (next alis)))))))

(defn- pprint-list [alis]
  (if-not (pprint-reader-macro alis)
    (pprint-simple-list alis)))

(defn- pprint-vector [avec]
  (pprint-logical-block :prefix "[" :suffix "]"
    (print-length-loop [aseq (seq avec)]
      (when aseq
        (write-out (first aseq))
        (when (next aseq)
          (-write *out* " ")
          (pprint-newline :linear)
          (recur (next aseq)))))))

(def ^{:private true} pprint-array (formatter-out "~<[~;~@{~w~^, ~:_~}~;]~:>"))

(defn- pprint-map [amap]
  (pprint-logical-block :prefix "{" :suffix "}"
    (print-length-loop [aseq (seq amap)]
      (when aseq
        (pprint-logical-block
          (write-out (ffirst aseq))
          (-write *out* " ")
          (pprint-newline :linear)
          (set! *current-length* 0)
          (write-out (fnext (first aseq))))
        (when (next aseq)
          (-write *out* ", ")
          (pprint-newline :linear)
          (recur (next aseq)))))))

(defn- pprint-simple-default [obj]
  (-write *out* (pr-str obj)))

(def pprint-set (formatter-out "~<#{~;~@{~w~^ ~:_~}~;}~:>"))

(defn- type-dispatcher [obj]
  (cond
    (symbol? obj) :symbol
    (seq? obj) :list
    (map? obj) :map
    (vector? obj) :vector
    (set? obj) :set
    (nil? obj) nil
    :else :default))

;; simple-dispatch / code-dispatch are plain functions rather than multimethods:
;; a multimethod baked into the seed can't capture this namespace's load context,
;; and the printer never extends these tables externally.
(defn simple-dispatch
  "The pretty print dispatch function for simple data structure format."
  [obj]
  (case (type-dispatcher obj)
    :list (pprint-list obj)
    :vector (pprint-vector obj)
    :map (pprint-map obj)
    :set (pprint-set obj)
    nil (-write *out* (pr-str nil))
    (pprint-simple-default obj)))

;;; code dispatch

(declare ^{:arglists '([alis])} pprint-simple-code-list)

(defn- brackets [form]
  (if (vector? form)
    ["[" "]"]
    ["(" ")"]))

(defn- pprint-simple-code-list [alis]
  (pprint-logical-block :prefix "(" :suffix ")"
    (pprint-indent :block 1)
    (print-length-loop [alis (seq alis)]
      (when alis
        (write-out (first alis))
        (when (next alis)
          (-write *out* " ")
          (pprint-newline :linear)
          (recur (next alis)))))))

(defn- pprint-code-list [alis]
  (if-not (pprint-reader-macro alis)
    (pprint-simple-code-list alis)))

(defn- pprint-code-symbol [sym]
  (if *print-suppress-namespaces*
    (print (name sym))
    (pr sym)))

(defn code-dispatch
  "The pretty print dispatch function for pretty printing Clojure code."
  [obj]
  (case (type-dispatcher obj)
    :list (pprint-code-list obj)
    :symbol (pprint-code-symbol obj)
    :vector (pprint-vector obj)
    :map (pprint-map obj)
    :set (pprint-set obj)
    nil (pr obj)
    (pprint-simple-default obj)))

(alter-var-root (var *print-pprint-dispatch*) (constantly simple-dispatch))

;;======================================================================
;; print-table
;;======================================================================

(defn- add-padding [width s]
  (let [padding (max 0 (- width (count s)))]
    (apply str (clojure.string/join (repeat padding \space)) s)))

(defn print-table
  "Prints a collection of maps in a textual table."
  ([ks rows]
   (when (seq rows)
     (let [widths (map
                    (fn [k]
                      (apply max (count (str k)) (map #(count (str (get % k))) rows)))
                    ks)
           spacers (map #(apply str (repeat % "-")) widths)
           fmt-row (fn [leader divider trailer row]
                     (str leader
                          (apply str (interpose divider
                                                (for [[col width] (map vector (map #(get row %) ks) widths)]
                                                  (add-padding width (str col)))))
                          trailer))]
       (clojure.core/println)
       (clojure.core/println (fmt-row "| " " | " " |" (zipmap ks ks)))
       (clojure.core/println (fmt-row "|-" "-+-" "-|" (zipmap ks spacers)))
       (doseq [row rows]
         (clojure.core/println (fmt-row "| " " | " " |" row))))))
  ([rows] (print-table (keys (first rows)) rows)))

;;======================================================================
;; core print routing
;;======================================================================

;; Route clojure.core/print et al. into the active pretty-writer when *out* is
;; bound to one, matching JVM Clojure where core print honours *out*. Custom
;; dispatch fns (e.g. clojure.data.json's, which calls clojure.core/print and
;; (PrintWriter. *out*)) depend on this. Declines (nil) when *out* is anything
;; else, so the string falls through to the normal output seam.
(clojure.core/__set-pprint-write-hook!
  (fn [s]
    (let [o clojure.core/*out*]
      (when (instance? PrettyWriter o)
        (-write o s)
        true))))
