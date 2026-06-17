# Jolt Clojure Reader
# Recursive descent parser for Clojure source text.
# Output convention:
#   Symbols foo, foo/bar → {:jolt/type :symbol :ns "foo" :name "bar"}
#   Keywords :foo, :foo/bar → Janet keyword :foo, :foo/bar
#   Lists (a b c)  → Janet array @[a b c]
#   Vectors [a b c] → Janet tuple [a b c]
#   Maps {:a 1}     → Janet struct {:a 1}
#   Sets #{1 2}     → tagged struct {:jolt/type :jolt/set :value [1 2]}

(use ./types)
(import ./phm :as phm)

# Forward declaration for mutual recursion
(var read-form nil)
(var read-meta nil)  # forward decl: read-dispatch (#^) calls it before its defn below

# Source-position tracking for the success checker (jolt-fqy). When enabled, the
# reader records each LIST form's absolute start offset (lists are the forms that
# become :invoke nodes — what the checker reports on). Off by default: a flag
# check per list is the only cost when the checker isn't running. Keyed by form
# IDENTITY (lists are fresh arrays, never interned), so a position survives
# macroexpansion exactly when the user's own sub-form is spliced through, and is
# absent for macro-synthesized structure — which is what we want (fall back to
# the call site). Not cleared between parses: nested parses (a require mid-load)
# would otherwise drop an outer file's positions; the table is bounded by forms
# compiled this process and only populated when the checker is on.
(def form-pos-table @{})
(var track-positions false)
(var pos-base 0)   # absolute offset of the slice read-form currently sees

(defn track-positions!
  "Enable/disable list-form position recording (jolt-fqy)."
  [on] (set track-positions on))

(defn set-pos-base!
  "Tell the reader the absolute offset of the slice it is about to read, so
  recorded list positions are absolute (parse-all-positioned reads a shrinking
  remainder)."
  [b] (set pos-base b))

(defn form-pos
  "Absolute start offset recorded for a list form, or nil."
  [form] (get form-pos-table form))

(defn checker-enabled?
  "True when JOLT_TYPE_CHECK selects a non-off level — the loaders use this to
  decide whether to record form positions for the checker (jolt-fqy)."
  []
  (def tc (os/getenv "JOLT_TYPE_CHECK"))
  (if (and tc (not= tc "off") (not= tc "0")) true false))

(def whitespace-chars " \t\n\r,")

(defn whitespace? [c]
  (or (= c 32)   # space
      (= c 10)   # \n
      (= c 9)    # \t
      (= c 13)   # \r
      (= c 44))) # comma

# Reader errors carry the raw byte OFFSET; the parse entry points (parse-string,
# parse-all-positioned) convert offset -> line:col against the full source and
# re-raise Clojure's 'Syntax error reading source at (file:line:col): msg'
# shape. Raising structured here keeps the 15 error sites one-liners.
# (jolt-2o7.5)
(defn- reader-error [msg pos]
  (error {:jolt/type :jolt/reader-error :msg msg :pos pos}))

(defn line-col
  "1-based [line col] of byte offset in source."
  [source offset]
  (var line 1)
  (var bol 0)
  (def stop (min offset (length source)))
  (loop [i :range [0 stop]]
    (when (= (in source i) (chr "\n")) (++ line) (set bol (+ i 1))))
  [line (+ 1 (- stop bol))])

(defn format-reader-error
  "Clojure-shaped syntax error message for a reader-error struct raised
  against source; file may be nil (omitted)."
  [e source file]
  (def [l c] (line-col source (e :pos)))
  (if file
    (string "Syntax error reading source at (" file ":" l ":" c "): " (e :msg))
    (string "Syntax error reading source at (" l ":" c "): " (e :msg))))

(defn reader-error? [e]
  (and (struct? e) (= :jolt/reader-error (e :jolt/type))))

(defn skip-whitespace [s pos]
  (if (and (< pos (length s))
           (whitespace? (s pos)))
    (skip-whitespace s (+ pos 1))
    pos))

(defn digit? [c]
  (and (>= c 48) (<= c 57)))

(defn hex-digit? [c]
  (or (and (>= c 48) (<= c 57))
      (and (>= c 65) (<= c 70))
      (and (>= c 97) (<= c 102))))

(defn symbol-start? [c]
  (or (and (>= c 65) (<= c 90))   # A-Z
      (and (>= c 97) (<= c 122))  # a-z
      (= c 42)  # *
      (= c 43)  # +
      (= c 33)  # !
      (= c 95)  # _
      (= c 45)  # -
      (= c 63)  # ?
      (= c 46)  # .
      (= c 60)  # <
      (= c 62)  # >
      (= c 61)  # =
      (= c 38)  # &
      (= c 124) # |
      (= c 36)  # $
      (= c 37)  # %
      (= c 47))) # /

(defn symbol-char? [c]
  (or (symbol-start? c)
      (digit? c)
      (= c 35)  # #
      (= c 39)  # '
      (= c 58))) # :

(defn read-symbol-name [s pos end]
  (if (and (< end (length s))
           (symbol-char? (s end)))
    (read-symbol-name s pos (+ end 1))
    end))

(defn make-symbol
  "Create a Jolt symbol struct."
  [name]
  (let [slash (string/find "/" name)]
    (if (and slash (> slash 0))
      {:jolt/type :symbol
       :ns (string/slice name 0 slash)
       :name (string/slice name (+ slash 1))}
      {:jolt/type :symbol
       :ns nil
       :name name})))

(defn sym
  "Convenience to create a symbol during testing."
  [name]
  (make-symbol name))

(defn read-symbol [s pos]
  (let [end (read-symbol-name s pos pos)]
    (if (= end pos)
      (reader-error (string "Unrecognized character: " (string/from-bytes (s pos))) pos)
      (let [name (string/slice s pos end)]
        (if (= name "nil") [nil end]
          (if (= name "true") [true end]
            (if (= name "false") [false end]
              [(make-symbol name) end])))))))

(defn read-keyword-name [s pos end]
  (if (and (< end (length s))
           (symbol-char? (s end)))
    (read-keyword-name s pos (+ end 1))
    end))

(defn read-keyword [s pos]
  # pos is at the first colon
  (if (and (< (+ pos 1) (length s)) (= (s (+ pos 1)) 58))
    # ::foo/bar — auto-resolved keyword
    (let [start (+ pos 2)
          end (read-keyword-name s start start)
          name (string/slice s start end)]
      [(keyword name) end])
    # :foo or :foo/bar
    (let [start (+ pos 1)
          end (read-keyword-name s start start)
          name (string/slice s start end)]
      [(keyword name) end])))

(defn escape-char [c]
  (if (= c 110) "\n"
    (if (= c 116) "\t"
      (if (= c 114) "\r"
        (if (= c 92) "\\"
          (if (= c 34) "\""
            (string/from-bytes c)))))))

(defn read-string-chars [s pos end chars]
  (if (>= pos end)
    (reader-error "Unterminated string" pos)
    (let [c (s pos)]
      (if (= c 92) # backslash
        (let [next-pos (+ pos 1)]
          (if (>= next-pos end)
            (reader-error "Unterminated escape" pos)
            (read-string-chars s (+ pos 2) end
              (array/push chars (escape-char (s next-pos))))))
        (if (= c 34) # closing quote
          [pos chars]
          (read-string-chars s (+ pos 1) end
            (array/push chars (string/from-bytes c))))))))

(defn read-string [s pos]
  # pos is at opening double-quote
  (let [end (length s)
        [new-pos chars] (read-string-chars s (+ pos 1) end @[])]
    [(string/join chars "") (+ new-pos 1)]))

(defn read-hex-digits [s pos end]
  (if (and (< end (length s)) (hex-digit? (s end)))
    (read-hex-digits s pos (+ end 1))
    end))

(defn read-digits [s pos end]
  (if (and (< end (length s)) (digit? (s end)))
    (read-digits s pos (+ end 1))
    end))

(defn read-fractional [s pos end]
  (if (and (< end (length s)) (digit? (s end)))
    (read-fractional s pos (+ end 1))
    end))

# Value of an alphanumeric digit for radix parsing (0-9, a-z/A-Z = 10-35).
(defn- radix-digit-val [c]
  (cond
    (and (>= c 48) (<= c 57)) (- c 48)       # 0-9
    (and (>= c 97) (<= c 122)) (+ 10 (- c 97)) # a-z
    (and (>= c 65) (<= c 90)) (+ 10 (- c 65))  # A-Z
    nil))
(defn- read-alnum [s pos end]
  (if (and (< end (length s)) (not (nil? (radix-digit-val (s end)))))
    (read-alnum s pos (+ end 1))
    end))
(defn- read-exponent
  "If s[end] is e/E (optionally with sign) followed by digits, return the index
  past the exponent; else end."
  [s end]
  (let [len (length s)]
    (if (and (< end len) (or (= (s end) 101) (= (s end) 69)))   # e / E
      (let [p (if (and (< (+ end 1) len) (or (= (s (+ end 1)) 43) (= (s (+ end 1)) 45))) (+ end 2) (+ end 1))
            de (read-digits s p p)]
        (if (> de p) de end))
      end)))

# Jolt has no true bignum/ratio types (see README): an integer/float literal
# suffixed N (bigint) or M (bigdec) reads as the plain number, a ratio a/b reads
# as the double quotient, and radixed integers (2r101, 16rFF) are parsed by base.
(defn read-number [s pos]
  (var start pos)      # start is mutable for sign handling
  (var neg false)
  (def len (length s))
  # optional sign
  (if (and (< pos len) (= (s pos) 45))
    (do (set start (+ pos 1)) (set neg true)))

  (let [pos start
        hex? (and (< (+ pos 1) len)
                  (= (s pos) 48) (or (= (s (+ pos 1)) 120) (= (s (+ pos 1)) 88)))]  # 0x / 0X
    (if hex?
      (let [hs (+ pos 2) he (read-hex-digits s hs hs)]
        (if (= he hs) (reader-error "Expected hex digits" pos))
        (let [he2 (if (and (< he len) (= (s he) 78)) (+ he 1) he)   # trailing N
              val (scan-number (string "0x" (string/slice s hs he)))]
          [(if neg (- val) val) he2]))
      (let [iend (read-digits s pos pos)]
        (if (= iend pos) (reader-error "Expected number" pos))
        (cond
          # radix integer: <base>r<digits>, e.g. 2r1010, 16rFF, 36rZ
          (and (< iend len) (or (= (s iend) 114) (= (s iend) 82)))
            (let [base (scan-number (string/slice s pos iend))
                  ds (+ iend 1)
                  de (read-alnum s ds ds)]
              (if (= de ds) (reader-error "Expected radix digits" ds))
              (var acc 0)
              (var i ds)
              (while (< i de) (set acc (+ (* acc base) (radix-digit-val (s i)))) (++ i))
              [(if neg (- acc) acc) de])
          # ratio: <int>/<int> (only when a digit follows the slash)
          (and (< (+ iend 1) len) (= (s iend) 47) (digit? (s (+ iend 1))))
            (let [ds (+ iend 1) de (read-digits s ds ds)
                  numr (scan-number (string/slice s pos iend))
                  den (scan-number (string/slice s ds de))]
              [(if neg (- (/ numr den)) (/ numr den)) de])
          # fractional and/or exponent, optional trailing N/M
          (let [frac-end (if (and (< iend len) (= (s iend) 46))
                           (let [fs (+ iend 1) fe (read-fractional s fs fs)]
                             (if (= fe fs) (error "Expected digit after .")) fe)
                           iend)
                exp-end (read-exponent s frac-end)
                val (scan-number (string/slice s start exp-end))
                # consume a trailing N (bigint) or M (bigdec) suffix
                fin (if (and (< exp-end len) (or (= (s exp-end) 78) (= (s exp-end) 77)))
                      (+ exp-end 1) exp-end)]
            [(if neg (- val) val) fin]))))))

# Read forms until `close` (a byte), returning [items end-pos]. Shared by the
# list/vector/set readers: skip #_ discards and splice #?@ items uniformly, so the
# skip/splice handling can't drift between them (it did once). The map reader
# keeps its own loop — its key/value pairing needs a different value-slot scan.
(defn read-delimited [s start-pos close err]
  (defn read-items [s pos items]
    (let [pos (skip-whitespace s pos)]
      (if (>= pos (length s))
        (reader-error err pos))
      (if (= (s pos) close)
        [items (+ pos 1)]
        (let [[form new-pos] (read-form s pos)]
          # skip #_ discarded forms
          (if (and (struct? form) (= :jolt/skip (form :jolt/type)))
            (read-items s new-pos items)
            # splice #?@ items into the collection
            (if (and (struct? form) (= :jolt/splice (form :jolt/type)))
              (read-items s new-pos (array/concat items (form :items)))
              (read-items s new-pos (array/push items form))))))))
  (read-items s start-pos @[]))

(defn read-list [s pos]
  # pos is at opening paren
  (read-delimited s (+ pos 1) 41 "Unterminated list"))  # )

(defn read-vector [s pos]
  # pos is at opening bracket
  (let [[items end] (read-delimited s (+ pos 1) 93 "Unterminated vector")]  # ]
    [(tuple/slice (tuple ;items)) end]))

# A map-literal form. Janet structs drop nil keys/values, so when a key or value
# is nil (e.g. {:a nil}) build a phm — it preserves nil, matching Clojure. The
# common nil-free case stays a struct: fast, and what the downstream map-form
# handling (evaluator/analyzer) already expects. Collection keys are left to
# eval-time construction (build-map-literal/eval-form), which phm-ifies them.
(defn- reader-map [kvs]
  (var has-nil false) (var i 0)
  (while (< i (length kvs)) (when (nil? (in kvs i)) (set has-nil true) (break)) (++ i))
  # Source order rides along out-of-band (jolt-p3c): struct iteration is hash
  # order, but Clojure evaluates literal entries left to right. A struct
  # PROTOTYPE carries it without changing the form's map behavior (keys/kvs/
  # length ignore protos; jolt-equal? compares maps structurally); the phm rep
  # (nil key/value present) gets a plain extra field.
  (if has-nil
    (let [m (phm/make-phm kvs)]
      (put m :jolt/kv-order (tuple/slice kvs))
      m)
    (struct/with-proto (struct :jolt/kv-order (tuple/slice kvs)) ;kvs)))

(defn form-kv-order
  "Source-ordered [k v k v ...] tuple of a map FORM (nil for maps built at
  runtime, which carry no reader order)."
  [form]
  (cond
    (struct? form) (get (struct/getproto form) :jolt/kv-order)
    (table? form) (get form :jolt/kv-order)))

(defn read-map [s pos]
  # pos is at opening brace
  (defn read-kvs [s pos kvs]
    (let [pos (skip-whitespace s pos)]
      (if (>= pos (length s))
        (reader-error "Unterminated map" pos))
      (if (= (s pos) 125) # }
        [(reader-map kvs) (+ pos 1)]
        (let [[key new-pos] (read-form s pos)]
          (if (and (struct? key) (= :jolt/skip (key :jolt/type)))
            (read-kvs s new-pos kvs)
            (if (and (struct? key) (= :jolt/splice (key :jolt/type)))
              (read-kvs s new-pos (array/concat kvs (key :items)))
              (let [pos (skip-whitespace s new-pos)
                    # The VALUE slot must skip comments/#_ while KEEPING the
                    # pending key: dropping both (the old behavior) desynced
                    # the kv pairing — {:a ; comment\n 1} read 1 as the next
                    # KEY and the closing } landed in value position
                    # ("Unmatched closing brace", jolt-ou8 / Selmer deps.edn).
                    [val new-pos2]
                    (do
                      (var vp pos)
                      (var v nil)
                      (var looking true)
                      (while looking
                        (def [f np] (read-form s vp))
                        (set vp np)
                        (if (and (struct? f) (= :jolt/skip (f :jolt/type)))
                          (set vp (skip-whitespace s np))
                          (do (set v f) (set looking false))))
                      [v vp])]
                (if (and (struct? val) (= :jolt/splice (val :jolt/type)))
                    # Only push key if splice contributes items
                    (if (> (length (val :items)) 0)
                      (do (array/push kvs key) (read-kvs s new-pos2 (array/concat kvs (val :items))))
                      (read-kvs s new-pos2 kvs))
                    (read-kvs s new-pos2 (-> kvs (array/push key) (array/push val)))))))))))
  (read-kvs s (+ pos 1) @[]))

(defn read-set [s pos]
  # pos is at #, next char is {
  (let [[items end] (read-delimited s (+ pos 2) 125 "Unterminated set")]  # }
    [{:jolt/type :jolt/set :value (tuple/slice (tuple ;items))} end]))

(defn read-char-name-end [s pos]
  (if (and (< pos (length s)) (symbol-char? (s pos)))
    (read-char-name-end s (+ pos 1))
    pos))

(defn read-char [s pos]
  # pos is at backslash; produce a char value directly (self-evaluating)
  (when (>= (+ pos 1) (length s))
    (error "unexpected end of input after \\"))
  (let [end (read-char-name-end s (+ pos 1))]
    (if (= end (+ pos 1))
      # The char right after \ isn't a symbol char (e.g. \{ \( \, \% \" ): it's a
      # one-character literal of that character itself.
      [(make-char (s (+ pos 1))) (+ pos 2)]
      (let [char-name (string/slice s (+ pos 1) end)]
        [(char-from-name char-name) end]))))

(defn read-anon-fn [s pos]
  # pos is at #, next char is (
  (let [[form new-pos] (read-form s (+ pos 1))]
    # Positional index of a %-symbol name: % and %1 are both 1, %N is N, %& is the
    # rest param (:rest); anything else is not a positional (nil). The fixed arity
    # is the MAX index used (Clojure semantics: #(do %2 %&) -> [p1 p2 & rest], so
    # unused lower positions still get a placeholder param and %& starts after %2).
    (defn- pct-index [nm]
      (cond
        (= nm "%") 1
        (= nm "%&") :rest
        (and (> (length nm) 1) (= "%" (string/slice nm 0 1)))
          (let [n (scan-number (string/slice nm 1))]
            (if (and n (= n (math/floor n)) (>= n 1)) n nil))
        nil))
    # Pass 1: max positional index + whether %& is used.
    (var max-n 0)
    (var has-rest false)
    (defn- scan-pct [f]
      (cond
        (and (struct? f) (= :symbol (f :jolt/type)))
          (let [i (pct-index (f :name))]
            (cond (= i :rest) (set has-rest true)
                  (and i (> i max-n)) (set max-n i)))
        (or (array? f) (tuple? f)) (each x f (scan-pct x))
        # set literal form — scan its elements (#(... #{%} ...))
        (and (struct? f) (= :jolt/set (f :jolt/type))) (each x (f :value) (scan-pct x))
        # map literal form — scan its keys AND values (#(... {:k %} ...))
        (not (nil? (form-kv-order f))) (each x (form-kv-order f) (scan-pct x))
        nil))
    (scan-pct form)
    # One canonical gensym per slot 1..max-n (placeholders for unused), plus rest.
    # Param names carry a trailing `#` (auto-gensym suffix, matching Clojure's
    # `p1__N#`) so a #() written inside a syntax-quote survives: sq-symbol treats
    # `#`-suffixed names as auto-gensyms (mapped consistently, left unqualified)
    # rather than qualifying them to the current ns — a qualified symbol is not a
    # valid fn param. Harmless outside a backtick (just a regular symbol name).
    (def slot-syms @{})
    (var i 1)
    (while (<= i max-n) (put slot-syms i (sym (string (gensym) "#"))) (++ i))
    (def rest-sym (if has-rest (sym (string (gensym) "#")) nil))
    # Pass 2: replace each %-symbol with its slot's gensym.
    (defn- replace-pct [f]
      (cond
        (and (struct? f) (= :symbol (f :jolt/type)))
          (let [idx (pct-index (f :name))]
            (cond (= idx :rest) rest-sym
                  idx (get slot-syms idx)
                  f))
        (array? f) (array ;(map replace-pct f))
        (tuple? f) (tuple ;(map replace-pct f))
        (and (struct? f) (= :jolt/set (f :jolt/type)))
          {:jolt/type :jolt/set :value (tuple ;(map replace-pct (f :value)))}
        (not (nil? (form-kv-order f)))
          (reader-map (array ;(map replace-pct (form-kv-order f))))
        f))
    (def replaced (replace-pct form))
    (def arg-names @[])
    (set i 1)
    (while (<= i max-n) (array/push arg-names (get slot-syms i)) (++ i))
    (when has-rest
      (array/push arg-names (sym "&"))
      (array/push arg-names rest-sym))
    [@[(sym "fn*") (tuple ;arg-names) replaced] new-pos]))

# The reader-conditional feature set (spec 02-reader S18): jolt is its own
# dialect, so the portable convention applies — the dialect key plus :default.
# Matching is by CLAUSE order (the first clause whose key is in the feature
# set wins), exactly like Clojure — NOT key-priority. JOLT_FEATURES overrides
# (comma-separated, e.g. "jolt,clj,default") for compat experiments and A/B
# measurement; :default is always honored.
# Mutable so a loading context can opt a clj-targeted foreign library into
# :clj compatibility (e.g. SCI) — see reader-features-set!. jolt itself and
# the conformance surface read under the portable set.
(var reader-features nil)

(defn reader-features-set!
  "Replace the active reader-conditional feature set (a list of keyword-name
  strings or keywords). :default is always honored. Returns the previous set
  so callers can restore."
  [names]
  (def prev reader-features)
  (def t @{})
  (each n names (put t (if (keyword? n) n (keyword n)) true))
  (put t :default true)
  (set reader-features t)
  prev)

(reader-features-set!
  (let [env (os/getenv "JOLT_FEATURES")]
    (if env (filter |(> (length $) 0) (string/split "," env))
      ["jolt" "default"])))

(defn read-reader-conditional [s pos]
  # pos is at #, next char is ? or ?@
  (def splice? (and (< (+ pos 2) (length s)) (= (s (+ pos 2)) 64))) # @ = 64
  (def form-start (if splice? (+ pos 3) (+ pos 2)))
  (let [[form new-pos] (read-form s form-start)]
    (if (array? form)
      (do
        # First clause (in clause order) whose feature key is in the set.
        # `matched` is tracked separately: a matched clause may be nil.
        (var result nil)
        (var matched false)
        (var i 0)
        (while (and (not matched) (< i (length form)))
          (when (get reader-features (in form i))
            (set result (in form (+ i 1)))
            (set matched true))
          (+= i 2))
        (if splice?
          # #?@ splicing: resolve :clj branch, wrap for splice
          (let [items (if (nil? result)
                        @[]
                        (if (or (array? result) (tuple? result))
                          result
                          @[result]))]
            [{:jolt/type :jolt/splice :items items} new-pos])
          # #? non-splicing: skip nil results (from :cljs branches on CLJ)
          (if (nil? result)
            [{:jolt/type :jolt/skip} new-pos]
            [result new-pos])))
      [{:jolt/type :jolt/reader-conditional :clauses form} new-pos])))

(defn read-var-quote [s pos]
  # pos is at #, next char is '
  (let [[form new-pos] (read-form s (+ pos 2))]
    [(array (sym "var") form) new-pos]))

(defn read-regex [s pos]
  # pos is at #, next char is "
  # Read until unescaped closing "
  (var i (+ pos 2))
  (var done nil)
  (while (and (< i (length s)) (not done))
    (if (= (s i) 92)  # backslash — skip next char
      (+= i 2)
      (if (= (s i) 34)  # closing quote
        (set done [(struct ;[:jolt/type :jolt/tagged :tag :regex :form (string/slice s (+ pos 2) i)])
                   (+ i 1)])
        (++ i))))
  (if done done (reader-error "Unterminated regex literal" pos)))

(defn read-dispatch [s pos]
  # pos is at #
  (if (>= (+ pos 1) (length s))
    (error "Unexpected end after #"))
  (let [c (s (+ pos 1))]
    (cond
      (= c 123) (read-set s pos)           # #{
      (= c 40) (read-anon-fn s pos)        # #(
      (= c 63) (read-reader-conditional s pos) # #?
      (= c 95) (let [[_ new-pos] (read-form s (+ pos 2))]  # #_
                  [{:jolt/type :jolt/skip} new-pos])
      (= c 39) (read-var-quote s pos)      # #'
      (= c 94) (read-meta s (+ pos 1))     # #^ — deprecated metadata reader macro (= ^)
      (= c 34) (read-regex s pos)          # #"regex
      (= c 35)                             # ## symbolic value: ##Inf ##-Inf ##NaN
        (let [end (read-symbol-name s (+ pos 2) (+ pos 2))
              name (string/slice s (+ pos 2) end)]
          (cond
            (= name "Inf") [math/inf end]
            (= name "-Inf") [(- math/inf) end]
            (= name "NaN") [math/nan end]
            (reader-error (string "Invalid symbolic value: ##" name) pos)))
      # unknown dispatch — tagged literal
      (let [end (read-symbol-name s pos pos)
            tag (string/slice s pos end)
            [form new-pos] (read-form s end)]
        [{:jolt/type :jolt/tagged :tag (keyword tag) :form form} new-pos]))))

(defn- self-evaluating-literal?
  "True for forms syntax-quote passes through unchanged: strings, numbers,
  booleans, nil, keywords, and character literals. NOT symbols (they qualify)
  or collections (they template)."
  [form]
  (or (nil? form) (= true form) (= false form) (number? form)
      (string? form) (buffer? form) (keyword? form)
      (and (struct? form) (= :jolt/char (form :jolt/type)))))

(defn read-quote [s new-pos token-sym]
  (let [[form final-pos] (read-form s new-pos)]
    # Spec 02-reader S25: syntax-quote of a self-evaluating literal is the
    # literal, collapsed at READ time (matching Clojure's reader) — so nested
    # backticks over literals are inert: ```"meow" reads as "meow".
    (if (and (= "syntax-quote" (token-sym :name)) (self-evaluating-literal? form))
      [form final-pos]
      [(array token-sym form) final-pos])))

(defn- meta-form->map
  "Normalize a metadata reader form (Clojure semantics): a symbol or string is a
  :tag, a keyword is {kw true}. Returns a metadata table, or nil if it isn't one
  of those simple shapes (e.g. a map literal — handled via with-meta instead)."
  [meta-form]
  (cond
    (keyword? meta-form) {meta-form true}
    # A symbol tag keeps its namespace qualifier (^t/Ray -> "t/Ray", not "Ray") so
    # an aliased/qualified record hint resolves through the ns's aliases the same
    # way a bare referred one does; dropping it silently mis-hinted across
    # namespaces. Bare symbols (^Ray, ^String) are unchanged.
    (and (struct? meta-form) (= :symbol (meta-form :jolt/type)))
    {:tag (if (meta-form :ns)
            (string (meta-form :ns) "/" (meta-form :name))
            (meta-form :name))}
    (string? meta-form) {:tag meta-form}
    nil))

(set read-meta (fn read-meta [s pos]
  # pos is at ^
  (let [[meta-form new-pos] (read-form s (+ pos 1))
        [form new-pos2] (read-form s new-pos)
        m (meta-form->map meta-form)]
    (if (and m (struct? form) (= :symbol (form :jolt/type)))
      # Attach the metadata to the symbol itself and keep it a bare symbol, so
      # type hints (^String x) and ^:dynamic etc. are transparent in every
      # position (params, lets, bodies) — the evaluator reads :name and ignores
      # :meta. This is what makes type hints "parse and otherwise do nothing".
      [(struct ;(kvs form) :meta (merge (or (form :meta) {}) m)) new-pos2]
      # Non-symbol targets (collections etc.) keep a runtime with-meta form. Use the
      # NORMALIZED metadata map (:kw -> {:kw true}, tag -> {:tag …}); a map-literal
      # meta-form (m is nil) is already a map, so pass it through.
      [(array (sym "with-meta") form (if m m meta-form)) new-pos2]))))

(defn read-until-newline [s pos]
  (if (or (>= pos (length s)) (= (s pos) 10))
    pos
    (read-until-newline s (+ pos 1))))

(set read-form (fn [s pos]
  (let [pos (skip-whitespace s pos)]
    (if (>= pos (length s))
      [nil pos]
      (let [c (s pos)]
        (cond
          # comment
          (= c 59)
          (let [line-end (read-until-newline s pos)]
            [{:jolt/type :jolt/skip} line-end])
          
          # dispatch
          (= c 35)
          (read-dispatch s pos)
          
          # string
          (= c 34)
          (read-string s pos)
          
          # list
          (= c 40)
          (let [r (read-list s pos)]
            (when track-positions (put form-pos-table (in r 0) (+ pos-base pos)))
            r)
          
          # unmatched closing delimiters
          (= c 41)
          (reader-error "Unmatched delimiter: )" pos)
          
          (= c 93)
          (reader-error "Unmatched delimiter: ]" pos)
          
          (= c 125)
          (reader-error "Unmatched delimiter: }" pos)
          
          # vector
          (= c 91)
          (read-vector s pos)
          
          # map
          (= c 123)
          (read-map s pos)
          
          # keyword
          (= c 58)
          (read-keyword s pos)
          
          # quote
          (= c 39)
          (read-quote s (+ pos 1) (sym "quote"))

          # syntax-quote / backtick
          (= c 96)
          (read-quote s (+ pos 1) (sym "syntax-quote"))

          # unquote ~
          (= c 126)
          (if (and (< (+ pos 1) (length s)) (= (s (+ pos 1)) 64))
            (read-quote s (+ pos 2) (sym "unquote-splicing"))
            (read-quote s (+ pos 1) (sym "unquote")))

          # deref — @x is (clojure.core/deref x), QUALIFIED like Clojure, so it
          # still derefs even where a ns shadows `deref` (malli excludes and
          # redefines clojure.core/deref for its own schema-deref).
          (= c 64)
          (read-quote s (+ pos 1) (sym "clojure.core/deref"))
          
          # metadata
          (= c 94)
          (read-meta s pos)
          
          # character
          (= c 92)
          (read-char s pos)
          
          # number or symbol
          (if (or (digit? c)
                  (and (= c 45) (< (+ pos 1) (length s)) (digit? (s (+ pos 1))))
                  (and (= c 43) (< (+ pos 1) (length s)) (digit? (s (+ pos 1)))))
            (read-number s pos)
            (read-symbol s pos))))))))

(defn parse-string
  "Parse a Clojure source string and return the first form."
  [s]
  (try
    (let [[form pos] (read-form s 0)]
      (if (and (struct? form) (= :jolt/skip (form :jolt/type)))
        (parse-string (string/slice s pos))
        form))
    ([err fib]
      (if (reader-error? err)
        (error (format-reader-error err s nil))
        (propagate err fib)))))

(defn parse-next
  "Parse the first form from a string. Returns [form remaining-string]."
  [s]
  (defn- parse-next-loop [pos]
    (let [[form new-pos] (read-form s pos)]
      (if (and (struct? form) (= :jolt/skip (form :jolt/type)))
        (parse-next-loop new-pos)
        [form (string/slice s new-pos)])))
  (parse-next-loop 0))

(defn parse-all-positioned
  "Parse every top-level form of source, returning an array of [form line]
  where line is the 1-based source line the form starts on. parse-next eats
  leading trivia itself, so the form's start line is the running newline
  count plus the newlines in the trivia (whitespace, commas, ; comments)
  ahead of it. (jolt-2o7.4)"
  [source &opt file]
  (def out @[])
  (var s source)
  (var line 1)
  (while (> (length (string/trim s)) 0)
    # newlines in the leading trivia belong BEFORE the form's line
    (var i 0)
    (def n (length s))
    (var scanning true)
    (while (and scanning (< i n))
      (def c (in s i))
      (cond
        (= c (chr "\n")) (do (++ line) (++ i))
        (or (= c (chr " ")) (= c (chr "\t")) (= c (chr "\r")) (= c (chr ","))) (++ i)
        (= c (chr ";")) (while (and (< i n) (not= (in s i) (chr "\n"))) (++ i))
        (set scanning false)))
    # list-form positions recorded during this parse-next are relative to s;
    # tell the reader the slice base so they land absolute (jolt-fqy)
    (when track-positions (set-pos-base! (- (length source) (length s))))
    (def [form rest*]
      (try (parse-next s)
        ([err fib]
          (if (reader-error? err)
            # err's :pos is relative to the remaining slice — rebase onto the
            # full source so line:col are absolute (jolt-2o7.5)
            (error (format-reader-error
                     {:jolt/type :jolt/reader-error :msg (err :msg)
                      :pos (+ (- (length source) (length s)) (err :pos))}
                     source file))
            (propagate err fib)))))
    (def consumed (- (length s) (length rest*)))
    (def form-line line)
    # count newlines inside the consumed chunk past the trivia
    (loop [j :range [i consumed]]
      (when (= (in s j) (chr "\n")) (++ line)))
    (set s rest*)
    (when (not (nil? form)) (array/push out [form form-line])))
  out)

(defn parse-all-spans
  "Parse every top-level form of source, returning an array of
  [form abs-start abs-end] where abs-start is the byte offset of the form's
  first non-trivia char and abs-end is just past it (leading whitespace/comment
  trivia is excluded). Lets a caller reconstruct source by slicing verbatim —
  used by `jolt uberscript`'s dead-code elimination to drop a defn's exact byte
  span without re-printing (so formatting and reader macros survive)."
  [source &opt file]
  (def out @[])
  (var s source)
  (while (> (length (string/trim s)) 0)
    (def base (- (length source) (length s)))
    (var i 0)
    (def n (length s))
    (var scanning true)
    (while (and scanning (< i n))
      (def c (in s i))
      (cond
        (or (= c (chr "\n")) (= c (chr " ")) (= c (chr "\t")) (= c (chr "\r")) (= c (chr ","))) (++ i)
        (= c (chr ";")) (while (and (< i n) (not= (in s i) (chr "\n"))) (++ i))
        (set scanning false)))
    (def [form rest*]
      (try (parse-next s)
        ([err fib]
          (if (reader-error? err)
            (error (format-reader-error
                     {:jolt/type :jolt/reader-error :msg (err :msg)
                      :pos (+ base (err :pos))}
                     source file))
            (propagate err fib)))))
    (def consumed (- (length s) (length rest*)))
    (set s rest*)
    (when (not (nil? form)) (array/push out [form (+ base i) (+ base consumed)])))
  out)
