(ns jolt.reader
  "Reads Clojure source text into reader forms.

  The lexing and parsing is portable Clojure; form construction and
  string->number parsing delegate to the jolt.host contract (form-make-symbol/
  char, form-char-from-name, form-scan-number). A Clojure source file can't write
  a {:jolt/type :symbol} literal — it would parse as a tagged reader form — and
  the concrete form representation belongs to the host. The analyzer uses the same
  split. Once cross-compiled this runs on Chez to drive compile-from-source.

  Positions are character indices; for ASCII source they coincide with byte
  indices, and form values are identical either way — the parity gate compares
  values, not positions."
  (:require [clojure.string :as str]
            [jolt.host :refer [form-make-symbol form-make-char form-char-from-name
                               form-scan-number form-make-list form-make-vector
                               form-make-map form-sym-merge-meta form-make-set
                               form-make-tagged form-gensym-name
                               form-sym? form-sym-name form-sym-ns form-char?
                               form-list? form-vec? form-set? form-map?
                               form-elements form-vec-items form-set-items
                               form-map-pairs]]))

;; Source access by CHARACTER codepoint
;; (identical to byte access for ASCII). cp = codepoint at i; len = character count.
(defn- cp [s i] (int (nth s i)))
(defn- len [s] (count s))

(defn- whitespace? [c] (or (= c 32) (= c 9) (= c 10) (= c 13) (= c 44)))  ; space tab nl cr ,
(defn- digit? [c] (and (>= c 48) (<= c 57)))
(defn- hex-digit? [c]
  (or (digit? c) (and (>= c 65) (<= c 70)) (and (>= c 97) (<= c 102))))
(defn- symbol-start? [c]
  (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122))
      (= c 42) (= c 43) (= c 33) (= c 95) (= c 45) (= c 63) (= c 46)
      (= c 60) (= c 62) (= c 61) (= c 38) (= c 124) (= c 36) (= c 37) (= c 47)))
(defn- symbol-char? [c]
  (or (symbol-start? c) (digit? c) (= c 35) (= c 39) (= c 58)))  ; + # ' :

(defn- skip-whitespace [s pos]
  (if (and (< pos (len s)) (whitespace? (cp s pos)))
    (recur s (inc pos))
    pos))

(defn- read-until-newline [s pos]
  (if (or (>= pos (len s)) (= (cp s pos) 10)) pos (recur s (inc pos))))

;; --- symbols -----------------------------------------------------------------
(defn- read-symbol-name [s pos end]
  (if (and (< end (len s)) (symbol-char? (cp s end))) (recur s pos (inc end)) end))

(defn- read-symbol* [s pos]
  (let [end (read-symbol-name s pos pos)]
    (when (= end pos)
      (throw (ex-info (str "Unrecognized character: " (char (cp s pos))) {})))
    (let [nm (subs s pos end)]
      (cond
        (= nm "nil") [nil end]
        (= nm "true") [true end]
        (= nm "false") [false end]
        :else [(form-make-symbol nm) end]))))

;; --- keywords ----------------------------------------------------------------
(defn- read-keyword-name [s pos end]
  (if (and (< end (len s)) (symbol-char? (cp s end))) (recur s pos (inc end)) end))

(defn- read-keyword* [s pos]
  ;; pos is at the first colon; ::foo is treated as :foo (no auto-resolution).
  (let [start (if (and (< (inc pos) (len s)) (= (cp s (inc pos)) 58)) (+ pos 2) (inc pos))
        end (read-keyword-name s start start)]
    [(keyword (subs s start end)) end]))

;; --- strings -----------------------------------------------------------------
(defn- escape-char [c]
  (cond (= c 110) "\n" (= c 116) "\t" (= c 114) "\r" (= c 92) "\\" (= c 34) "\""
        :else (str (char c))))

(defn- read-string* [s pos]
  ;; pos at opening double-quote
  (loop [p (inc pos) acc []]
    (when (>= p (len s)) (throw (ex-info "Unterminated string" {})))
    (let [c (cp s p)]
      (cond
        (= c 92) (let [np (inc p)]
                   (when (>= np (len s)) (throw (ex-info "Unterminated escape" {})))
                   (recur (+ p 2) (conj acc (escape-char (cp s np)))))
        (= c 34) [(apply str acc) (inc p)]
        :else (recur (inc p) (conj acc (str (char c))))))))

;; --- numbers -----------------------------------------------------------------
(defn- read-digits [s pos end]
  (if (and (< end (len s)) (digit? (cp s end))) (recur s pos (inc end)) end))
(defn- read-hex-digits [s pos end]
  (if (and (< end (len s)) (hex-digit? (cp s end))) (recur s pos (inc end)) end))

;; Value of an alphanumeric digit for radix parsing (0-9, a-z/A-Z = 10-35).
(defn- radix-digit-val [c]
  (cond
    (and (>= c 48) (<= c 57)) (- c 48)
    (and (>= c 97) (<= c 122)) (+ 10 (- c 97))
    (and (>= c 65) (<= c 90)) (+ 10 (- c 65))
    :else nil))
(defn- read-alnum [s pos end]
  (if (and (< end (len s)) (radix-digit-val (cp s end))) (recur s pos (inc end)) end))

(defn- read-exponent [s end]
  ;; if s[end] is e/E (optionally signed) followed by digits, return index past it
  (if (and (< end (len s)) (let [c (cp s end)] (or (= c 101) (= c 69))))
    (let [p (if (and (< (inc end) (len s)) (let [c (cp s (inc end))] (or (= c 43) (= c 45))))
              (+ end 2) (inc end))
          de (read-digits s p p)]
      (if (> de p) de end))
    end))

;; Jolt has no bignum/ratio: N (bigint) / M (bigdec) suffixes read as the plain
;; number, a ratio a/b reads as the double quotient, radixed ints by base.
(defn- read-number* [s pos]
  (let [length (len s)
        ;; optional leading sign: - negates; + is a positive no-op (Clojure reads
        ;; +5 as 5). read-form only dispatches +digit/-digit, so the sign is real.
        neg (and (< pos length) (= (cp s pos) 45))
        plus (and (< pos length) (= (cp s pos) 43))
        start (if (or neg plus) (inc pos) pos)
        hex? (and (< (inc start) length) (= (cp s start) 48)
                  (let [c1 (cp s (inc start))] (or (= c1 120) (= c1 88))))]  ; 0x / 0X
    (if hex?
      (let [hs (+ start 2) he (read-hex-digits s hs hs)]
        (when (= he hs) (throw (ex-info "Expected hex digits" {})))
        (let [he2 (if (and (< he length) (= (cp s he) 78)) (inc he) he)   ; trailing N
              val (form-scan-number (str "0x" (subs s hs he)))]
          [(if neg (- val) val) he2]))
      (let [iend (read-digits s start start)]
        (when (= iend start) (throw (ex-info "Expected number" {})))
        (cond
          ;; radix integer <base>r<digits>
          (and (< iend length) (let [c (cp s iend)] (or (= c 114) (= c 82))))
          (let [base (form-scan-number (subs s start iend))
                ds (inc iend) de (read-alnum s ds ds)]
            (when (= de ds) (throw (ex-info "Expected radix digits" {})))
            (let [acc (reduce (fn [a i] (+ (* a base) (radix-digit-val (cp s i)))) 0 (range ds de))]
              [(if neg (- acc) acc) de]))
          ;; ratio <int>/<int> (only when a digit follows the slash)
          (and (< (inc iend) length) (= (cp s iend) 47) (digit? (cp s (inc iend))))
          (let [ds (inc iend) de (read-digits s ds ds)
                numr (form-scan-number (subs s start iend))
                den (form-scan-number (subs s ds de))]
            [(if neg (- (/ numr den)) (/ numr den)) de])
          ;; fractional and/or exponent, optional trailing N/M
          :else
          (let [frac-end (if (and (< iend length) (= (cp s iend) 46))
                           (let [fs (inc iend) fe (read-digits s fs fs)]
                             (when (= fe fs) (throw (ex-info "Expected digit after ." {})))
                             fe)
                           iend)
                exp-end (read-exponent s frac-end)
                val (form-scan-number (subs s start exp-end))
                fin (if (and (< exp-end length) (let [c (cp s exp-end)] (or (= c 78) (= c 77))))
                      (inc exp-end) exp-end)]
            [(if neg (- val) val) fin]))))))

;; --- characters --------------------------------------------------------------
(defn- read-char-name-end [s pos]
  (if (and (< pos (len s)) (symbol-char? (cp s pos))) (recur s (inc pos)) pos))

(defn- read-char* [s pos]
  (when (>= (inc pos) (len s)) (throw (ex-info "unexpected end of input after \\" {})))
  (let [end (read-char-name-end s (inc pos))]
    (if (= end (inc pos))
      ;; a non-symbol char right after \ is a one-character literal of itself
      [(form-make-char (cp s (inc pos))) (+ pos 2)]
      [(form-char-from-name (subs s (inc pos) end)) end])))

;; --- dispatcher --------------------------------------------------------------
;; read-form returns a CONTROL triple [kind payload pos]:
;;   :form  payload=the form          a real datum
;;   :skip  payload=nil               a comment (;) or #_ discard — produced nothing
;;   :splice payload=items-vector     #?@ — contributes 0+ items to the enclosing coll
;; Out-of-band control (rather than :jolt/skip / :jolt/splice sentinel
;; FORMS) keeps it collision-free and host-neutral — no tagged-struct to build or
;; recognize. Collection readers dispatch on kind; read-next-form skips :skip.
(declare read-form)

(defn- number-start? [s pos c]
  (or (digit? c)
      (and (= c 45) (< (inc pos) (len s)) (digit? (cp s (inc pos))))
      (and (= c 43) (< (inc pos) (len s)) (digit? (cp s (inc pos))))))

;; Read items until `close`, dispatching control kinds. Returns [items-vec end].
(defn- read-delimited [s start-pos close errmsg]
  (loop [pos start-pos items []]
    (let [pos (skip-whitespace s pos)]
      (when (>= pos (len s)) (throw (ex-info errmsg {})))
      (if (= (cp s pos) close)
        [items (inc pos)]
        (let [[kind payload np] (read-form s pos)]
          (case kind
            :skip (recur np items)
            :splice (recur np (into items payload))
            :form (recur np (conj items payload))))))))

(defn- read-list* [s pos]
  (let [[items end] (read-delimited s (inc pos) 41 "Unterminated list")]   ; )
    [:form (form-make-list items) end]))

(defn- read-vector* [s pos]
  (let [[items end] (read-delimited s (inc pos) 93 "Unterminated vector")] ; ]
    [:form (form-make-vector items) end]))

;; Map: pair up keys and values, skipping comments/#_ in either slot while keeping
;; the pending key (dropping both desyncs the pairing). A key/value is always a
;; single :form (or :skip) — splice in a map slot is not supported.
(defn- read-map* [s pos]
  (loop [pos (inc pos) kvs []]
    (let [pos (skip-whitespace s pos)]
      (when (>= pos (len s)) (throw (ex-info "Unterminated map" {})))
      (if (= (cp s pos) 125)   ; }
        [:form (form-make-map kvs) (inc pos)]
        (let [[kk kp knp] (read-form s pos)]
          (if (= kk :skip)
            (recur knp kvs)
            ;; key in hand; read the value slot, skipping trivia but keeping the key
            (let [[v vnp]
                  (loop [vp (skip-whitespace s knp)]
                    (when (>= vp (len s)) (throw (ex-info "Unterminated map" {})))
                    (let [[vk vp2 vnp2] (read-form s vp)]
                      (if (= vk :skip) (recur (skip-whitespace s vnp2)) [vp2 vnp2])))]
              (recur vnp (conj (conj kvs kp) v)))))))))

;; Read the next REAL form (skip :skip), returning [form pos]. Used wherever a
;; single datum is needed (quote/meta/top level).
(defn- read-next-form [s pos]
  (let [[kind payload np] (read-form s pos)]
    (case kind
      :skip (recur s np)
      :form [payload np]
      :splice (throw (ex-info "splice (#?@) not inside a collection" {})))))

;; syntax-quote of a self-evaluating literal collapses to the literal at read time
;; (so nested backticks over literals are inert). NOT symbols (they qualify) or
;; collections (they template).
(defn- self-evaluating-literal? [form]
  (or (nil? form) (true? form) (false? form) (number? form)
      (string? form) (keyword? form) (form-char? form)))

(defn- read-quote* [s newpos token-sym]
  (let [[form finalpos] (read-next-form s newpos)]
    (if (and (= "syntax-quote" (form-sym-name token-sym)) (self-evaluating-literal? form))
      [:form form finalpos]
      [:form (form-make-list [token-sym form]) finalpos])))

;; Normalize a metadata reader form: keyword -> {kw true}; symbol/string -> {:tag …}
;; (a symbol tag keeps its ns qualifier); else nil (a map-literal meta).
(defn- meta-form->map [meta-form]
  (cond
    (keyword? meta-form) {meta-form true}
    (form-sym? meta-form) {:tag (if (form-sym-ns meta-form)
                                  (str (form-sym-ns meta-form) "/" (form-sym-name meta-form))
                                  (form-sym-name meta-form))}
    (string? meta-form) {:tag meta-form}
    :else nil))

(defn- read-meta* [s pos]
  ;; pos at ^
  (let [[meta-form np] (read-next-form s (inc pos))
        [form np2] (read-next-form s np)
        m (meta-form->map meta-form)]
    (if (and m (form-sym? form))
      ;; attach to the symbol itself (^Type x / ^:dynamic) — stays a bare symbol
      [:form (form-sym-merge-meta form m) np2]
      ;; non-symbol target -> a runtime with-meta form (normalized map, or the
      ;; raw map-literal meta when m is nil)
      [:form (form-make-list [(form-make-symbol "with-meta") form (if m m meta-form)]) np2])))

;; --- dispatch (#) ------------------------------------------------------------
;; Reader-conditional feature set (spec 02-reader). jolt's portable default; the
;; JOLT_FEATURES env override is a host concern wired later. :default always honored.
(def reader-features (atom #{:jolt :default}))
(defn set-reader-features! [features] (reset! reader-features (conj (set features) :default)))

(defn- read-set* [s pos]
  ;; pos at #, next char {
  (let [[items end] (read-delimited s (+ pos 2) 125 "Unterminated set")]  ; }
    [:form (form-make-set items) end]))

(defn- read-var-quote* [s pos]
  ;; pos at #, next char '
  (let [[form np] (read-next-form s (+ pos 2))]
    [:form (form-make-list [(form-make-symbol "var") form]) np]))

(defn- read-regex* [s pos]
  ;; pos at #, next char "; read raw to the unescaped closing " (backslashes kept)
  (loop [i (+ pos 2)]
    (when (>= i (len s)) (throw (ex-info "Unterminated regex literal" {})))
    (let [c (cp s i)]
      (cond
        (= c 92) (recur (+ i 2))   ; backslash escapes next char
        (= c 34) [:form (form-make-tagged :regex (subs s (+ pos 2) i)) (inc i)]
        :else (recur (inc i))))))

;; #?(…) / #?@(…): pick the first clause whose feature key is active (clause order,
;; like Clojure). #? -> :skip when the result is nil (e.g. a :cljs branch); #?@ ->
;; :splice the resolved items into the enclosing collection.
(defn- rc-resolve [clauses]
  ;; clauses: a jolt vector of [feature-kw form feature-kw form ...]
  (loop [i 0]
    (if (>= i (count clauses))
      [false nil]
      (if (contains? @reader-features (nth clauses i))
        [true (nth clauses (inc i))]
        (recur (+ i 2))))))

(defn- read-reader-conditional* [s pos]
  ;; pos at #, next char ? (optionally ?@)
  (let [splice? (and (< (+ pos 2) (len s)) (= (cp s (+ pos 2)) 64))  ; @
        form-start (if splice? (+ pos 3) (+ pos 2))
        [form np] (read-next-form s form-start)]
    (if (form-list? form)
      (let [clauses (form-elements form)
            [matched result] (rc-resolve clauses)]
        (if splice?
          (let [items (cond (not matched) []
                            (form-list? result) (vec (form-elements result))
                            (form-vec? result) (vec (form-vec-items result))
                            :else [result])]
            [:splice items np])
          (if (or (not matched) (nil? result)) [:skip nil np] [:form result np])))
      (throw (ex-info "reader conditional body must be a list" {})))))

;; Symbolic values ##Inf ##-Inf ##NaN.
(defn- read-symbolic* [s pos]
  (let [end (read-symbol-name s (+ pos 2) (+ pos 2))
        nm (subs s (+ pos 2) end)]
    (cond
      (= nm "Inf") [:form ##Inf end]
      (= nm "-Inf") [:form ##-Inf end]
      (= nm "NaN") [:form ##NaN end]
      :else (throw (ex-info (str "Invalid symbolic value: ##" nm) {})))))

(defn- read-tagged* [s pos]
  ;; unknown dispatch -> a tagged literal (#inst, #uuid, #foo). The tag includes
  ;; the leading # (read-symbol-name starts at #).
  (let [end (read-symbol-name s pos pos)
        tag (subs s pos end)
        [form np] (read-next-form s end)]
    [:form (form-make-tagged (keyword tag) form) np]))

(declare read-anon-fn*)

(defn- read-dispatch* [s pos]
  ;; pos at #
  (when (>= (inc pos) (len s)) (throw (ex-info "Unexpected end after #" {})))
  (let [c (cp s (inc pos))]
    (cond
      (= c 123) (read-set* s pos)                 ; #{
      (= c 40) (read-anon-fn* s pos)              ; #(
      (= c 63) (read-reader-conditional* s pos)   ; #?
      (= c 95) (let [[_ _ np] (read-form s (+ pos 2))] [:skip nil np])  ; #_ discard
      (= c 39) (read-var-quote* s pos)            ; #'
      (= c 94) (read-meta* s (inc pos))           ; #^ (deprecated, = ^)
      (= c 34) (read-regex* s pos)                ; #"
      (= c 35) (read-symbolic* s pos)             ; ##
      :else (read-tagged* s pos))))

;; #(...) anonymous fn. Positional %-arg index: % and %1 => 1, %N => N, %& => the
;; rest param (:rest); anything else is not positional (nil). Fixed arity = max
;; index used (Clojure: #(do %2 %&) => [p1 p2 & rest], unused lower slots still
;; get a placeholder param).
(defn- pct-index [nm]
  (cond
    (= nm "%") 1
    (= nm "%&") :rest
    (and (> (count nm) 1) (= "%" (subs nm 0 1)))
    (let [n (form-scan-number (subs nm 1))]
      (if (and n (integer? n) (>= n 1)) n nil))
    :else nil))

;; Pass 1: collect every %-index used anywhere in the form tree.
(defn- collect-pcts [form acc]
  (cond
    (form-sym? form) (let [i (pct-index (form-sym-name form))] (if i (conj acc i) acc))
    (form-list? form) (reduce (fn [a x] (collect-pcts x a)) acc (form-elements form))
    (form-vec? form) (reduce (fn [a x] (collect-pcts x a)) acc (form-vec-items form))
    (form-set? form) (reduce (fn [a x] (collect-pcts x a)) acc (form-set-items form))
    (form-map? form) (reduce (fn [a p] (collect-pcts (nth p 1) (collect-pcts (nth p 0) a)))
                             acc (form-map-pairs form))
    :else acc))

;; Pass 2: replace each %-symbol with its slot's gensym (rebuilding collections).
(defn- replace-pct [form slot-syms rest-sym]
  (cond
    (form-sym? form) (let [idx (pct-index (form-sym-name form))]
                       (cond (= idx :rest) rest-sym
                             idx (get slot-syms idx)
                             :else form))
    (form-list? form) (form-make-list (mapv #(replace-pct % slot-syms rest-sym) (form-elements form)))
    (form-vec? form) (form-make-vector (mapv #(replace-pct % slot-syms rest-sym) (form-vec-items form)))
    (form-set? form) (form-make-set (mapv #(replace-pct % slot-syms rest-sym) (form-set-items form)))
    (form-map? form) (form-make-map
                       (vec (mapcat (fn [p] [(replace-pct (nth p 0) slot-syms rest-sym)
                                             (replace-pct (nth p 1) slot-syms rest-sym)])
                                    (form-map-pairs form))))
    :else form))

(defn- gensym-param [] (form-make-symbol (str (form-gensym-name) "#")))

(defn- read-anon-fn* [s pos]
  ;; pos at #, next char (
  (let [[form np] (read-next-form s (inc pos))
        pcts (collect-pcts form [])
        max-n (reduce (fn [m i] (if (and (number? i) (> i m)) i m)) 0 pcts)
        has-rest (boolean (some #(= :rest %) pcts))
        slot-syms (into {} (map (fn [i] [i (gensym-param)]) (range 1 (inc max-n))))
        rest-sym (when has-rest (gensym-param))
        replaced (replace-pct form slot-syms rest-sym)
        arg-names (let [base (mapv #(get slot-syms %) (range 1 (inc max-n)))]
                    (if has-rest (conj base (form-make-symbol "&") rest-sym) base))]
    [:form (form-make-list [(form-make-symbol "fn*") (form-make-vector arg-names) replaced]) np]))

(defn read-form [s pos]
  (let [pos (skip-whitespace s pos)]
    (if (>= pos (len s))
      [:form nil pos]
      (let [c (cp s pos)]
        (cond
          (= c 59) [:skip nil (read-until-newline s pos)]              ; ; comment
          (= c 34) (let [r (read-string* s pos)] [:form (nth r 0) (nth r 1)])
          (= c 58) (let [r (read-keyword* s pos)] [:form (nth r 0) (nth r 1)])
          (= c 92) (let [r (read-char* s pos)] [:form (nth r 0) (nth r 1)])
          (= c 40) (read-list* s pos)                                  ; (
          (= c 91) (read-vector* s pos)                                ; [
          (= c 123) (read-map* s pos)                                  ; {
          (= c 39) (read-quote* s (inc pos) (form-make-symbol "quote"))            ; '
          (= c 96) (read-quote* s (inc pos) (form-make-symbol "syntax-quote"))     ; `
          (= c 126) (if (and (< (inc pos) (len s)) (= (cp s (inc pos)) 64))        ; ~ / ~@
                      (read-quote* s (+ pos 2) (form-make-symbol "unquote-splicing"))
                      (read-quote* s (inc pos) (form-make-symbol "unquote")))
          (= c 64) (read-quote* s (inc pos) (form-make-symbol "clojure.core/deref")) ; @
          (= c 94) (read-meta* s pos)                                  ; ^
          (= c 41) (throw (ex-info "Unmatched delimiter: )" {}))
          (= c 93) (throw (ex-info "Unmatched delimiter: ]" {}))
          (= c 125) (throw (ex-info "Unmatched delimiter: }" {}))
          (= c 35) (read-dispatch* s pos)                             ; #
          (number-start? s pos c) (let [r (read-number* s pos)] [:form (nth r 0) (nth r 1)])
          (symbol-start? c) (let [r (read-symbol* s pos)] [:form (nth r 0) (nth r 1)])
          :else (throw (ex-info (str "read-form: unexpected char '" (char c) "' (" c ")") {})))))))

(defn read-one
  "Read the first form of `s` (skipping leading trivia). Returns the form."
  [s]
  (first (read-next-form s 0)))

(defn read-all
  "Read every top-level form of `s`, returning them in a vector (trivia skipped)."
  [s]
  (loop [pos 0 acc []]
    (let [p (skip-whitespace s pos)]
      (if (>= p (len s))
        acc
        (let [[kind payload np] (read-form s p)]
          (case kind
            :skip (recur np acc)
            :splice (recur np (into acc payload))
            :form (recur np (conj acc payload))))))))
