# Regex support for Jolt, compiled to Janet's PEG engine.
#
# Path A: a real regex parser -> AST -> PEG grammar with continuation passing
# and position-based group captures. This gives genuine backtracking (greedy and
# lazy) that threads through capturing groups, plus capturing groups returned in
# Clojure's [whole g1 g2 ...] order, lookahead, anchors, classes, flags.
#
# Supported: literals, `.`, char classes `[...]`/`[^...]` (ranges, POSIX,
# `\d \w \s` etc.), quantifiers `* + ? {n} {n,} {n,m}` (greedy + lazy `?`),
# groups `(...)` (numbered), non-capturing `(?:...)`, lookahead `(?=...)`/`(?!...)`,
# alternation `|`, anchors `^ $ \b \B`, escapes, inline flag `(?i)`.
# Not supported: lookbehind, backreferences, named groups (rare; documented).

(defn- chr [s] (get s 0))
(defn regex? [x] (and (table? x) (= :jolt/regex (x :jolt/type))))

# ============================================================
# Parser: regex source -> AST
# ============================================================
# AST nodes (structs): {:op ...}
#   :char {:b byte :ci bool}        :any {:dotall bool}
#   :class {:peg <frag> :neg bool}  :pred {:peg <frag>}
#   :seq {:items [...]}             :alt {:items [...]}
#   :star/:plus/:quest {:item ast :greedy bool}
#   :rep {:item ast :min n :max m-or-nil :greedy bool}
#   :group {:n num :item ast}       :ncgroup {:item ast}
#   :look {:neg bool :item ast}
#   :anchor {:kind :start/:end/:wordb/:nwordb}

(defn- lower-b [b] (if (and (>= b 65) (<= b 90)) (+ b 32) b))
(defn- upper-b [b] (if (and (>= b 97) (<= b 122)) (- b 32) b))

(defn- pred-frag [c]
  (case c
    (chr "d") '(range "09")
    (chr "D") '(if-not (range "09") 1)
    (chr "w") '(choice (range "az" "AZ" "09") (set "_"))
    (chr "W") '(if-not (choice (range "az" "AZ" "09") (set "_")) 1)
    (chr "s") '(set " \t\n\r\f\v")
    (chr "S") '(if-not (set " \t\n\r\f\v") 1)
    nil))

# Unicode property classes \p{...} (jolt-xlp), mapped onto the byte PEGs:
# ASCII exactly; any high byte (>= 0x80, i.e. inside a UTF-8 sequence) counts
# as a LETTER byte — so ^\p{L}+$ accepts UTF-8 words, while \p{N}/\p{Z}
# stay ASCII-only. Lu/Ll are ASCII (case is byte-based throughout this
# engine). Unknown property names error at compile.
(defn- prop-frag [name]
  (case name
    "L"  '(choice (range "az" "AZ") (range "\x80\xFF"))
    "Lu" '(range "AZ")
    "Ll" '(range "az")
    "N"  '(range "09")
    "Nd" '(range "09")
    "Z"  '(set " ")
    "Zs" '(set " ")
    "P"  '(set "!\"#%&'()*,-./:;?@[\\]_{}")
    "Ps" '(set "([{")
    "Pe" '(set ")]}")
    "Alpha" '(choice (range "az" "AZ") (range "\x80\xFF"))
    "Digit" '(range "09")
    nil))

# At s[i] = backslash: if this is \p{Name} / \P{Name}, return [peg-frag end-i]
# (end-i past the closing brace), else nil.
(defn- parse-prop [s i]
  (def pc (get s (+ i 1)))
  (when (and (or (= pc (chr "p")) (= pc (chr "P")))
             (= (get s (+ i 2)) (chr "{")))
    (def close (string/find "}" s (+ i 3)))
    (unless close (error "regex: unterminated \\p{...}"))
    (def nm (string/slice s (+ i 3) close))
    (def frag (prop-frag nm))
    (unless frag (error (string "regex: unsupported property class \\p{" nm "}")))
    [(if (= pc (chr "P")) ~(if-not ,frag 1) frag) (+ close 1)]))

(defn- esc-byte [c]
  (case c
    (chr "n") 10 (chr "t") 9 (chr "r") 13 (chr "f") 12 (chr "v") 11 (chr "0") 0
    c))

# Parse a [...] character class body, returns a PEG fragment + negation flag.
(defn- parse-class [s start ci]
  (var i start)
  (def neg (and (< i (length s)) (= (s i) (chr "^"))))
  (when neg (++ i))
  (def alts @[])
  (while (and (< i (length s)) (not= (s i) (chr "]")))
    (cond
      # POSIX class [:alpha:] etc.
      (and (= (s i) (chr "[")) (< (+ i 1) (length s)) (= (s (+ i 1)) (chr ":")))
        (let [close (string/find ":]" s i)
              name (string/slice s (+ i 2) close)]
          (array/push alts (case name
                             "alpha" '(range "az" "AZ")
                             "digit" '(range "09")
                             "alnum" '(range "az" "AZ" "09")
                             "space" '(set " \t\n\r\f\v")
                             "upper" '(range "AZ")
                             "lower" '(range "az")
                             '(set "")))
          (set i (+ close 2)))
      # escape inside class — \p{...} first (multi-char), then 2-char escapes
      (= (s i) (chr "\\"))
        (if-let [pr (parse-prop s i)]
          (do (array/push alts (pr 0)) (set i (pr 1)))
          (let [c (s (+ i 1)) p (pred-frag c)]
            (if p (array/push alts p)
              (array/push alts ~(set ,(string/from-bytes (esc-byte c)))))
            (set i (+ i 2))))
      # range a-z
      (and (< (+ i 2) (length s)) (= (s (+ i 1)) (chr "-")) (not= (s (+ i 2)) (chr "]")))
        (do
          (if ci
            (array/push alts ~(range ,(string/from-bytes (lower-b (s i)) (lower-b (s (+ i 2)))
                                                          (upper-b (s i)) (upper-b (s (+ i 2))))))
            (array/push alts ~(range ,(string/from-bytes (s i) (s (+ i 2))))))
          (set i (+ i 3)))
      # single char
      (do
        (if ci
          (array/push alts ~(set ,(string/from-bytes (lower-b (s i)) (upper-b (s i)))))
          (array/push alts ~(set ,(string/from-bytes (s i)))))
        (++ i))))
  (def frag (if (= 1 (length alts)) (alts 0) ~(choice ,;alts)))
  [(if neg ~(if-not ,frag 1) frag) (+ i 1)])  # i is at ], skip it

(var parse-alt nil)

(defn- parse-atom [st]
  # returns ast; advances (st :pos)
  (def s (st :s))
  (def c (s (st :pos)))
  (def ci (st :ci))
  (cond
    (= c (chr "("))
      (let [nx (if (< (+ (st :pos) 1) (length s)) (s (+ (st :pos) 1)) 0)]
        (if (= nx (chr "?"))
          (let [k (s (+ (st :pos) 2))]
            (cond
              (= k (chr ":")) (do (+= (st :pos) 3)
                                  (let [inner (parse-alt st)] (+= (st :pos) 1) {:op :ncgroup :item inner}))
              (= k (chr "=")) (do (+= (st :pos) 3)
                                  (let [inner (parse-alt st)] (+= (st :pos) 1) {:op :look :neg false :item inner}))
              (= k (chr "!")) (do (+= (st :pos) 3)
                                  (let [inner (parse-alt st)] (+= (st :pos) 1) {:op :look :neg true :item inner}))
               # inline flags (?i) / (?m) / (?im:...) etc.
              (do
                (var j (+ (st :pos) 2))
                (var seti false)
                (var setm false)
                (while (and (< j (length s)) (not= (s j) (chr ")")) (not= (s j) (chr ":")))
                  (when (= (s j) (chr "i")) (set seti true))
                  (when (= (s j) (chr "m")) (set setm true))
                  (++ j))
                (if (= (s j) (chr ":"))
                  (do (set (st :pos) (+ j 1))
                      (def savedci (st :ci))
                      (def savedml (st :ml))
                      (when seti (set (st :ci) true))
                      (when setm (set (st :ml) true))
                      (def inner (parse-alt st))
                      (set (st :ci) savedci)
                      (set (st :ml) savedml)
                      (+= (st :pos) 1)
                      {:op :ncgroup :item inner})
                  (do (set (st :pos) (+ j 1))   # (?i) / (?m) — flag for rest of pattern
                      (when seti (set (st :ci) true))
                      (when setm (set (st :ml) true))
                      {:op :seq :items @[]})))))
          # capturing group
          (let [n (++ (st :ngroup))]
            (+= (st :pos) 1)
            (let [inner (parse-alt st)]
              (+= (st :pos) 1)  # skip )
              {:op :group :n n :item inner}))))
    (= c (chr "["))
      (let [[frag np] (parse-class s (+ (st :pos) 1) ci)]
        (set (st :pos) np)
        {:op :class :peg frag})
    (= c (chr "."))
      (do (++ (st :pos)) {:op :any :dotall (st :dotall)})
    (= c (chr "\\"))
      (if-let [pr (parse-prop s (st :pos))]
        (do (set (st :pos) (pr 1)) {:op :pred :peg (pr 0)})
        (let [nc (s (+ (st :pos) 1)) p (pred-frag nc)]
          (+= (st :pos) 2)
          (cond
            p {:op :pred :peg p}
            (= nc (chr "b")) {:op :anchor :kind :wordb}
            (= nc (chr "B")) {:op :anchor :kind :nwordb}
            {:op :char :b (esc-byte nc) :ci ci})))
    (= c (chr "^")) (do (++ (st :pos)) {:op :anchor :kind :start})
    (= c (chr "$")) (do (++ (st :pos)) {:op :anchor :kind :end})
    (do (++ (st :pos)) {:op :char :b c :ci ci})))

(defn- parse-quant [st]
  (def atom (parse-atom st))
  (def s (st :s))
  (if (>= (st :pos) (length s))
    atom
    (let [q (s (st :pos))]
      # called after advancing past the quantifier char: a trailing `?` -> lazy
      (defn lazy? []
        (if (and (< (st :pos) (length s)) (= (s (st :pos)) (chr "?")))
          (do (++ (st :pos)) false)   # lazy -> greedy=false
          true))
      (cond
        (= q (chr "*")) (do (++ (st :pos)) {:op :star :item atom :greedy (lazy?)})
        (= q (chr "+")) (do (++ (st :pos)) {:op :plus :item atom :greedy (lazy?)})
        (= q (chr "?")) (do (++ (st :pos)) {:op :quest :item atom :greedy (lazy?)})
        (= q (chr "{"))
          (let [close (string/find "}" s (st :pos))
                spec (string/slice s (+ (st :pos) 1) close)
                comma (string/find "," spec)]
            (set (st :pos) (+ close 1))
            (def greedy (lazy?))
            (if comma
              (let [lo (scan-number (string/slice spec 0 comma))
                    hs (string/slice spec (+ comma 1))
                    hi (if (= 0 (length hs)) nil (scan-number hs))]
                {:op :rep :item atom :min lo :max hi :greedy greedy})
              {:op :rep :item atom :min (scan-number spec) :max (scan-number spec) :greedy greedy}))
        atom))))

(defn- parse-seq [st]
  (def s (st :s))
  (def items @[])
  (while (and (< (st :pos) (length s))
              (not= (s (st :pos)) (chr "|"))
              (not= (s (st :pos)) (chr ")")))
    (array/push items (parse-quant st)))
  (if (= 1 (length items)) (items 0) {:op :seq :items items}))

(set parse-alt (fn [st]
  (def branches @[(parse-seq st)])
  (def s (st :s))
  (while (and (< (st :pos) (length s)) (= (s (st :pos)) (chr "|")))
    (++ (st :pos))
    (array/push branches (parse-seq st)))
  (if (= 1 (length branches)) (branches 0) {:op :alt :items branches})))

(defn- parse [source]
  (def st @{:s source :pos 0 :ngroup 0 :ci false :ml false :dotall false})
  (def ast (parse-alt st))
  [ast (st :ngroup) (st :ml)])

# ============================================================
# Emit: AST -> PEG grammar (continuation passing)
# ============================================================

(def- word-frag '(choice (range "az" "AZ" "09") (set "_")))

(defn- char-peg [b ci]
  (if (and ci (not= (lower-b b) (upper-b b)))
    ~(set ,(string/from-bytes (lower-b b) (upper-b b)))
    ~(set ,(string/from-bytes b))))

(defn- make-emitter [grammar ml]
  (var ctr 0)
  (defn fresh [] (++ ctr) (keyword (string "r" ctr)))
  (var emit nil)
  (set emit (fn [ast k]
    (case (ast :op)
      :char ~(sequence ,(char-peg (ast :b) (ast :ci)) ,k)
      :any ~(sequence ,(if (ast :dotall) 1 ~(if-not "\n" 1)) ,k)
      :class ~(sequence ,(ast :peg) ,k)
      :pred ~(sequence ,(ast :peg) ,k)
      :seq (do (var acc k) (var i (dec (length (ast :items))))
               (while (>= i 0) (set acc (emit (in (ast :items) i) acc)) (-- i)) acc)
      :alt (let [kr (fresh)]
             (put grammar kr k)
             ~(choice ,;(map (fn [a] (emit a (keyword kr))) (ast :items))))
      :star (let [r (fresh)]
              (put grammar r (if (ast :greedy)
                               ~(choice ,(emit (ast :item) (keyword r)) ,k)
                               ~(choice ,k ,(emit (ast :item) (keyword r)))))
              (keyword r))
      :plus (let [r (fresh)]
              (put grammar r (if (ast :greedy)
                               ~(choice ,(emit (ast :item) (keyword r)) ,k)
                               ~(choice ,k ,(emit (ast :item) (keyword r)))))
              (emit (ast :item) (keyword r)))
      :quest (if (ast :greedy)
               ~(choice ,(emit (ast :item) k) ,k)
               ~(choice ,k ,(emit (ast :item) k)))
      :rep (let [lo (ast :min) hi (ast :max) item (ast :item) greedy (ast :greedy)]
             # desugar: lo required, then (hi-lo) optional, or star if hi is nil
             (var tail (if (nil? hi)
                         (let [r (fresh)]
                           (put grammar r (if greedy
                                            ~(choice ,(emit item (keyword r)) ,k)
                                            ~(choice ,k ,(emit item (keyword r)))))
                           (keyword r))
                         # Each optional level is its OWN grammar rule that
                         # references the previous level by name. Inlining instead
                         # (choice (emit item acc) acc) duplicates `acc` per level,
                         # so {0,n} built a structure the PEG compiler expanded to
                         # 2^n nodes and hung (jolt-3xur). Naming keeps it O(n).
                         (do (def kr (fresh)) (put grammar kr k)
                             (var acc (keyword kr)) (var c (- hi lo))
                             (while (> c 0)
                               (let [r (fresh)]
                                 (put grammar r (if greedy ~(choice ,(emit item acc) ,acc)
                                                          ~(choice ,acc ,(emit item acc))))
                                 (set acc (keyword r)))
                               (-- c))
                             acc)))
             (var acc tail) (var c lo)
             (while (> c 0) (set acc (emit item acc)) (-- c))
             acc)
      :group (let [n (ast :n)]
               ~(sequence (/ (position) ,(fn [p] [n :s p]))
                          ,(emit (ast :item)
                                 ~(sequence (/ (position) ,(fn [p] [n :e p])) ,k))))
      :ncgroup (emit (ast :item) k)
      :look (if (ast :neg)
              ~(sequence (not ,(emit (ast :item) 0)) ,k)
              ~(sequence (not (not ,(emit (ast :item) 0))) ,k))
      :anchor (case (ast :kind)
                :start (if ml
                         ~(sequence (choice (not (look -1 1)) (look -1 "\n")) ,k)
                         ~(sequence (not (look -1 1)) ,k))
                :end (if ml
                       ~(sequence (choice (not 1) (not (not "\n"))) ,k)
                       ~(sequence (not 1) ,k))
                :wordb ~(sequence (choice (sequence (look -1 ,word-frag) (not ,word-frag))
                                          (sequence (not (look -1 ,word-frag)) (not (not ,word-frag))))
                                  ,k)
                :nwordb ~(sequence (choice (sequence (look -1 ,word-frag) (not (not ,word-frag)))
                                           (sequence (not (look -1 ,word-frag)) (not ,word-frag)))
                                   ,k)
                ~(sequence 0 ,k))
      (error (string "regex emit: unhandled op " (ast :op))))))
  emit)

(defn compile-regex [source]
  (def [ast ngroups ml] (parse source))
  (def grammar @{})
  (def emit (make-emitter grammar ml))
  # group 0 = whole match: mark start, body, mark end
  (def body (emit ast ~(sequence (/ (position) ,(fn [p] [0 :e p])) 0)))
  (put grammar :main ~(sequence (/ (position) ,(fn [p] [0 :s p])) ,body))
  (def gstruct (table/to-struct grammar))
  # anchored variant for re-matches: whole input must be consumed
  (def anchored (table/to-struct (merge grammar {:main ~(sequence ,(grammar :main) -1)})))
  @{:jolt/type :jolt/regex
    :source source
    :ngroups ngroups
    :peg (peg/compile gstruct)
    :anchored (peg/compile anchored)})

(defn re-pattern [source]
  (if (regex? source) source (compile-regex source)))

# ============================================================
# Matching
# ============================================================

(defn- marks->groups [marks s ngroups]
  # marks: array of [n :s pos] / [n :e pos]; build [g0 g1 ... gN], slicing input
  (def starts (array/new-filled (+ ngroups 1)))
  (def ends (array/new-filled (+ ngroups 1)))
  (each m marks
    (let [n (m 0)]
      (if (= (m 1) :s) (put starts n (m 2)) (put ends n (m 2)))))
  (def groups (array/new-filled (+ ngroups 1)))
  (var n 0)
  (while (<= n ngroups)
    (if (and (not (nil? (in starts n))) (not (nil? (in ends n))))
      (put groups n (string/slice s (in starts n) (in ends n)))
      (put groups n nil))
    (++ n))
  groups)

(defn- match-at [re s start]
  # returns groups array or nil
  (def marks (peg/match (re :peg) s start))
  (if marks (marks->groups marks s (re :ngroups)) nil))

(defn- groups->result [groups ngroups]
  # 0 groups -> whole-match string; else tuple [whole g1 ...]
  (if (= ngroups 0) (in groups 0) (tuple/slice groups)))

(defn re-find [re s]
  (def re (re-pattern re))
  (var result nil) (var pos 0)
  (while (and (nil? result) (<= pos (length s)))
    (def g (match-at re s pos))
    (if g (set result (groups->result g (re :ngroups))) (++ pos)))
  result)

(defn re-matches [re s]
  (def re (re-pattern re))
  (def marks (peg/match (re :anchored) s 0))
  (if marks (groups->result (marks->groups marks s (re :ngroups)) (re :ngroups)) nil))

(defn re-seq [re s]
  (def re (re-pattern re))
  (def out @[]) (var pos 0)
  (while (<= pos (length s))
    (def g (match-at re s pos))
    (if g
      (let [whole (in g 0)]
        (array/push out (groups->result g (re :ngroups)))
        (set pos (+ pos (max 1 (length whole)))))
      (++ pos)))
  # Clojure's re-seq is nil (not an empty seq) when there are no matches, so
  # `(if-let [m (re-seq ...)] ...)` works — an empty seq would be truthy.
  (if (= 0 (length out)) nil out))

(defn re-split [re s &opt limit]
  # limit (Java Pattern.split n>0): at most `limit` parts — stop after limit-1
  # splits and keep the rest of the string as the final unsplit part. nil/<=0
  # splits at every match.
  (def re (re-pattern re))
  (def lim (if (and limit (> limit 0)) limit nil))
  (def out @[]) (var pos 0) (var last 0)
  (while (and (<= pos (length s)) (or (nil? lim) (< (length out) (dec lim))))
    (def g (match-at re s pos))
    (if (and g (> (length (in g 0)) 0))
      (do (array/push out (string/slice s last pos))
          (set pos (+ pos (length (in g 0))))
          (set last pos))
      (++ pos)))
  (array/push out (string/slice s last))
  out)

(defn- expand-replacement [repl groups]
  # $0 / $1 ... substitution in replacement string
  (def buf @"") (var i 0)
  (while (< i (length repl))
    (let [c (repl i)]
      (if (and (= c (chr "$")) (< (+ i 1) (length repl)) (>= (repl (+ i 1)) 48) (<= (repl (+ i 1)) 57))
        (let [n (- (repl (+ i 1)) 48)]
          (when (and (< n (length groups)) (in groups n)) (buffer/push-string buf (in groups n)))
          (set i (+ i 2)))
        (do (buffer/push-string buf (string/from-bytes c)) (++ i)))))
  (string buf))

(defn- replacement-for
  "One match's replacement text. A string replacement gets $N expansion; a FN
  replacement (Clojure: fn of the match — string, or [whole g1 ...] when the
  pattern has groups) is called and its result used literally."
  [replacement g ngroups]
  (cond
    (string? replacement) (expand-replacement replacement g)
    (or (function? replacement) (cfunction? replacement))
      (string (replacement (groups->result g ngroups)))
    (string replacement)))

(defn re-replace-all [re s replacement]
  (def re (re-pattern re))
  (def buf @"") (var pos 0) (var last 0)
  (while (<= pos (length s))
    (def g (match-at re s pos))
    (if (and g (> (length (in g 0)) 0))
      (do (buffer/push-string buf (string/slice s last pos))
          (buffer/push-string buf (replacement-for replacement g (re :ngroups)))
          (set pos (+ pos (length (in g 0))))
          (set last pos))
      (++ pos)))
  (buffer/push-string buf (string/slice s last))
  (string buf))

(defn re-replace-first [re s replacement]
  (def re (re-pattern re))
  (var pos 0) (var done nil)
  (while (and (nil? done) (<= pos (length s)))
    (def g (match-at re s pos))
    (if (and g (> (length (in g 0)) 0))
      (set done [pos g])
      (++ pos)))
  (if done
    (let [[p g] done]
      (string (string/slice s 0 p)
              (replacement-for replacement g (re :ngroups))
              (string/slice s (+ p (length (in g 0))))))
    s))
