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

# Forward declaration for mutual recursion
(var read-form nil)

(def whitespace-chars " \t\n\r,")

(defn whitespace? [c]
  (or (= c 32)   # space
      (= c 10)   # \n
      (= c 9)    # \t
      (= c 13)   # \r
      (= c 44))) # comma

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
      (error (string "Unrecognized character at " pos ": " (string/from-bytes (s pos))))
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
    (error "Unterminated string")
    (let [c (s pos)]
      (if (= c 92) # backslash
        (let [next-pos (+ pos 1)]
          (if (>= next-pos end)
            (error "Unterminated escape")
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

(defn read-number [s pos]
  (var start pos)      # start is mutable for sign handling
  (var neg false)
  
  # optional sign
  (if (and (< pos (length s)) (= (s pos) 45))
    (do (set start (+ pos 1)) (set neg true)))
  
  (let [pos start
        hex? (and (< (+ pos 1) (length s))
                  (= (s pos) 48) (= (s (+ pos 1)) 120))
        start (if hex? (+ pos 2) pos)
        end (if hex?
              (read-hex-digits s start start)
              (read-digits s start start))]
    (if (= end start) (error (string "Expected number at " pos)))
    
    # check for fractional part
    (if (and (not hex?)
             (< end (length s))
             (= (s end) 46))
      (let [frac-start (+ end 1)
            frac-end (read-fractional s frac-start frac-start)]
        (if (= frac-end frac-start) (error "Expected digit after ."))
        (let [num-str (string/slice s start frac-end)
              val (scan-number num-str)]
          [(if neg (- val) val) frac-end]))
      
      # integer or hex
      (let [num-str (string/slice s start end)
            val (if hex?
                  (string/format "0x%s" num-str)
                  num-str)
            val (scan-number val)]
        [(if neg (- val) val) end]))))

(defn read-list [s pos]
  # pos is at opening paren
  (defn read-list-items [s pos items]
    (let [pos (skip-whitespace s pos)]
      (if (>= pos (length s))
        (error "Unterminated list"))
      (if (= (s pos) 41) # )
        [items (+ pos 1)]
        (let [[form new-pos] (read-form s pos)]
          # skip #_ discarded forms
          (if (and (struct? form) (= :jolt/skip (form :jolt/type)))
            (read-list-items s new-pos items)
            # splice #?@ items into the list
            (if (and (struct? form) (= :jolt/splice (form :jolt/type)))
              (read-list-items s new-pos (array/concat items (form :items)))
              (read-list-items s new-pos (array/push items form))))))))
  (read-list-items s (+ pos 1) @[]))

(defn read-vector [s pos]
  # pos is at opening bracket
  (defn read-vec-items [s pos items]
    (let [pos (skip-whitespace s pos)]
      (if (>= pos (length s))
        (error "Unterminated vector"))
      (if (= (s pos) 93) # ]
        [(tuple/slice (tuple ;items)) (+ pos 1)]
        (let [[form new-pos] (read-form s pos)]
          (if (and (struct? form) (= :jolt/skip (form :jolt/type)))
            (read-vec-items s new-pos items)
            (if (and (struct? form) (= :jolt/splice (form :jolt/type)))
              (read-vec-items s new-pos (array/concat items (form :items)))
              (read-vec-items s new-pos (array/push items form))))))))
  (read-vec-items s (+ pos 1) @[]))

(defn read-map [s pos]
  # pos is at opening brace
  (defn read-kvs [s pos kvs]
    (let [pos (skip-whitespace s pos)]
      (if (>= pos (length s))
        (error "Unterminated map"))
      (if (= (s pos) 125) # }
        [(struct ;kvs) (+ pos 1)]
        (let [[key new-pos] (read-form s pos)]
          (if (and (struct? key) (= :jolt/skip (key :jolt/type)))
            (read-kvs s new-pos kvs)
            (if (and (struct? key) (= :jolt/splice (key :jolt/type)))
              (read-kvs s new-pos (array/concat kvs (key :items)))
              (let [pos (skip-whitespace s new-pos)
                    [val new-pos2] (read-form s pos)]
                (if (and (struct? val) (= :jolt/skip (val :jolt/type)))
                  (read-kvs s new-pos2 kvs)
                  (if (and (struct? val) (= :jolt/splice (val :jolt/type)))
                    # Only push key if splice contributes items
                    (if (> (length (val :items)) 0)
                      (do (array/push kvs key) (read-kvs s new-pos2 (array/concat kvs (val :items))))
                      (read-kvs s new-pos2 kvs))
                    (read-kvs s new-pos2 (-> kvs (array/push key) (array/push val))))))))))))
  (read-kvs s (+ pos 1) @[]))

(defn read-set [s pos]
  # pos is at #, next char is {
  (defn read-set-items [s pos items]
    (let [pos (skip-whitespace s pos)]
      (if (>= pos (length s))
        (error "Unterminated set"))
      (if (= (s pos) 125) # }
        [{:jolt/type :jolt/set :value (tuple/slice (tuple ;items))} (+ pos 1)]
        (let [[form new-pos] (read-form s pos)]
          (if (and (struct? form) (= :jolt/skip (form :jolt/type)))
            (read-set-items s new-pos items)
            (if (and (struct? form) (= :jolt/splice (form :jolt/type)))
              (read-set-items s new-pos (array/concat items (form :items)))
              (read-set-items s new-pos (array/push items form))))))))
  (read-set-items s (+ pos 2) @[]))

(defn read-char-name-end [s pos]
  (if (and (< pos (length s)) (symbol-char? (s pos)))
    (read-char-name-end s (+ pos 1))
    pos))

(defn read-char [s pos]
  # pos is at backslash; produce a char value directly (self-evaluating)
  (let [end (read-char-name-end s (+ pos 1))
        char-name (string/slice s (+ pos 1) end)]
    [(char-from-name char-name) end]))

(defn read-anon-fn [s pos]
  # pos is at #, next char is (
  (let [[form new-pos] (read-form s (+ pos 1))]
    # Collect % arg references and rename them to gensyms
    (var arg-map @{})
    (defn- replace-pct [f]
      (cond
        (and (struct? f) (= :symbol (f :jolt/type)))
        (let [nm (f :name)]
          (if (and (> (length nm) 0) (= "%" (string/slice nm 0 1)))
            (let [existing (get arg-map nm)]
              (if existing
                {:jolt/type :symbol :ns nil :name existing}
                (let [gen (gensym)]
                  (put arg-map nm (string gen))
                  {:jolt/type :symbol :ns nil :name (string gen)})))
            f))
        (array? f) (array ;(map replace-pct f))
        (tuple? f) (tuple ;(map replace-pct f))
        f))
    (def replaced (replace-pct form))
    (def arg-names @[])
    # Positional params %, %1, %2, ... in order; %& becomes a `& rest` param.
    (def pos-keys (sort (filter |(not= $ "%&") (keys arg-map))))
    (each k pos-keys
      (array/push arg-names {:jolt/type :symbol :ns nil :name (get arg-map k)}))
    (when (get arg-map "%&")
      (array/push arg-names (sym "&"))
      (array/push arg-names {:jolt/type :symbol :ns nil :name (get arg-map "%&")}))
    (def result @[(sym "fn*")])
    (array/push result (tuple ;arg-names))
    (array/push result replaced)
    [result new-pos]))

(defn read-reader-conditional [s pos]
  # pos is at #, next char is ? or ?@
  (def splice? (and (< (+ pos 2) (length s)) (= (s (+ pos 2)) 64))) # @ = 64
  (def form-start (if splice? (+ pos 3) (+ pos 2)))
  (let [[form new-pos] (read-form s form-start)]
    (if (array? form)
      (do
        (var result nil)
        (var i 0)
        (while (< i (length form))
          (if (= (in form i) :clj)
            (do
              (set result (in form (+ i 1)))
              (set i (length form)))
            (++ i)))
        # Fallback to :default if :clj not matched
        (when (nil? result)
          (set i 0)
          (while (< i (length form))
            (if (= (in form i) :default)
              (do
                (set result (in form (+ i 1)))
                (set i (length form)))
              (++ i))))
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
  (if done done (error "Unterminated regex literal")))

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
      (= c 34) (read-regex s pos)          # #"regex
      # unknown dispatch — tagged literal
      (let [end (read-symbol-name s pos pos)
            tag (string/slice s pos end)
            [form new-pos] (read-form s end)]
        [{:jolt/type :jolt/tagged :tag (keyword tag) :form form} new-pos]))))

(defn read-quote [s pos new-pos token-sym]
  (let [[form final-pos] (read-form s new-pos)]
    [(array token-sym form) final-pos]))

(defn read-meta [s pos]
  # pos is at ^
  (let [[meta-form new-pos] (read-form s (+ pos 1))
        [form new-pos2] (read-form s new-pos)]
    [(array (sym "with-meta") form meta-form) new-pos2]))

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
          (read-list s pos)
          
          # unmatched closing delimiters
          (= c 41)
          (error (string "Unmatched closing paren at " pos))
          
          (= c 93)
          (error (string "Unmatched closing bracket at " pos))
          
          (= c 125)
          (error (string "Unmatched closing brace at " pos))
          
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
          (read-quote s pos (+ pos 1) (sym "quote"))
          
          # syntax-quote / backtick
          (= c 96)
          (read-quote s pos (+ pos 1) (sym "syntax-quote"))
          
          # unquote ~
          (= c 126)
          (if (and (< (+ pos 1) (length s)) (= (s (+ pos 1)) 64))
            (read-quote s pos (+ pos 2) (sym "unquote-splicing"))
            (read-quote s pos (+ pos 1) (sym "unquote")))
          
          # deref
          (= c 64)
          (read-quote s pos (+ pos 1) (sym "deref"))
          
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
  (let [[form pos] (read-form s 0)]
    (if (and (struct? form) (= :jolt/skip (form :jolt/type)))
      (parse-string (string/slice s pos))
      form)))

(defn parse-next
  "Parse the first form from a string. Returns [form remaining-string]."
  [s]
  (defn- parse-next-loop [pos]
    (let [[form new-pos] (read-form s pos)]
      (if (and (struct? form) (= :jolt/skip (form :jolt/type)))
        (parse-next-loop new-pos)
        [form (string/slice s new-pos)])))
  (parse-next-loop 0))
