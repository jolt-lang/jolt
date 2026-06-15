(use ../../src/jolt/reader)

# Helper: create a symbol
(defn sym [name]
  (let [slash (string/find "/" name)]
    (if slash
      {:jolt/type :symbol
       :ns (string/slice name 0 slash)
       :name (string/slice name (+ slash 1))}
      {:jolt/type :symbol
       :ns nil
       :name name})))

# Symbols
(assert (deep= (sym "foo") (parse-string "foo"))
        "bare symbol")
(assert (deep= (sym "foo/bar") (parse-string "foo/bar"))
        "namespaced symbol")
(assert (deep= (sym "+") (parse-string "+"))
        "operator symbol")
(assert (deep= (sym "->foo") (parse-string "->foo"))
        "arrow symbol")

# Keywords
(assert (= :foo (parse-string ":foo"))
        "bare keyword")
(assert (= :foo/bar (parse-string "::foo/bar"))
        "auto-resolved keyword")
(assert (= :foo/bar (parse-string ":foo/bar"))
        "namespaced keyword")

# Numbers
(assert (= 1 (parse-string "1"))
        "integer")
(assert (= -42 (parse-string "-42"))
        "negative integer")
(assert (= 3.14 (parse-string "3.14"))
        "float")

# Strings
(assert (= "hello" (parse-string "\"hello\""))
        "simple string")

# Nil, booleans
(assert (= nil (parse-string "nil"))
        "nil")
(assert (= true (parse-string "true"))
        "true")
(assert (= false (parse-string "false"))
        "false")

# Lists → Janet arrays (to distinguish from vectors)
(assert (array? (parse-string "(1 2 3)"))
        "list produces array")
(assert (deep= @[1 2 3] (parse-string "(1 2 3)"))
        "simple list")

# Vectors → Janet tuples
(assert (tuple? (parse-string "[1 2 3]"))
        "vector produces tuple")
(assert (deep= [1 2 3] (parse-string "[1 2 3]"))
        "simple vector")

# Maps → Janet structs
(let [m (parse-string "{:a 1 :b 2}")]
  (assert (struct? m) "map is struct")
  (assert (= 1 (m :a)) "map key lookup"))

# Sets → tagged with :jolt/set
(let [form (parse-string "#{1 2 3}")]
  (assert (struct? form) "set is struct")
  (assert (= :jolt/set (form :jolt/type)) "set type tag"))

# Quote and shorthand
(assert (deep= @[(sym "quote") (sym "x")] (parse-string "'x"))
        "quote shorthand")
(assert (deep= @[(sym "syntax-quote") (sym "x")] (parse-string "`x"))
        "syntax-quote")
(assert (deep= @[(sym "unquote") (sym "x")] (parse-string "~x"))
        "unquote")
(assert (deep= @[(sym "unquote-splicing") (sym "x")] (parse-string "~@x"))
        "unquote-splicing")
# @x reads as the QUALIFIED clojure.core/deref (like Clojure) so it derefs even
# where a ns excludes/rebinds `deref` (e.g. malli).
(assert (deep= @[(sym "clojure.core/deref") (sym "x")] (parse-string "@x"))
        "deref shorthand")

# Metadata: a keyword/symbol/string hint on a symbol attaches to the symbol's
# :meta and keeps it a bare symbol (so type hints are transparent everywhere).
(let [form (parse-string "^:meta x")]
  (assert (and (struct? form) (= :symbol (form :jolt/type))) "meta-hinted symbol stays a symbol")
  (assert (= "x" (form :name)) "symbol name preserved")
  (assert (= true (get (form :meta) :meta)) "keyword hint -> {kw true}"))
(let [form (parse-string "^String s")]
  (assert (= "s" (form :name)) "type-hinted symbol name preserved")
  (assert (= "String" (get (form :meta) :tag)) "symbol hint -> {:tag name}"))
# Map metadata on a symbol still uses a runtime with-meta form.
(let [form (parse-string "^{:doc \"d\"} y")]
  (assert (and (array? form) (struct? (first form)) (= "with-meta" ((first form) :name)))
          "map metadata -> with-meta form"))

# Comments (skip to end of line)
(assert (= 42 (parse-string "; comment\n42"))
        "comment then form")

# Discard #_
(assert (= 42 (parse-string "#_ (ignored 1 2) 42"))
        "discard skips next form")

# Anonymous function #()
(let [form (parse-string "#(+ %1 %2)")]
  (assert (array? form) "fn form is array")
  (assert (deep= (sym "fn*") (in form 0)) "first element is fn*"))

# Nested forms
(let [form (parse-string "(+ 1 (* 2 3))")]
  (assert (array? form) "outer list is array")
  (assert (deep= (sym "+") (in form 0)) "+ is first")
  (assert (= 1 (in form 1)) "1 is second")
  (assert (array? (in form 2)) "nested list is array"))

# Multiple forms: parse-next
(let [[form1 rest-str] (parse-next "(1 2) [3 4]")]
  (assert (deep= @[1 2] form1) "first form is list")
  (let [[form2 _] (parse-next rest-str)]
    (assert (deep= [3 4] form2) "second form is vector")))

# Reader conditional — feature set is #{:jolt :default} (spec 02-reader S18,
# RFC 0002); matching is by clause order. reader-features-set! lets a loading
# context opt a clj-targeted library into :clj compat (restored below).
(assert (= 3 (parse-string "#?(:clj 1 :jolt 3 :cljs 2)"))
        "#?(...) picks :jolt branch")
(assert (= nil (parse-string "#?(:cljs 999)"))
        "unmatched conditional reads as nothing")
(assert (= 7 (parse-string "#?(:clj 1 :default 7)"))
        ":default reached when :clj not in feature set")
(assert (= 5 (parse-string "#?(:default 5 :jolt 6)"))
        "clause order wins, not key priority")
(assert (deep= @[(sym "+") 1 3] (parse-string "(+ 1 #?(:jolt 3 :cljs 4))"))
        "#? inside list picks :jolt")
(let [prev (reader-features-set! ["jolt" "clj" "default"])]
  (assert (= 1 (parse-string "#?(:clj 1 :cljs 2)"))
          "clj-compat opt-in picks :clj")
  (reader-features-set! (keys prev)))

# Characters — the reader now produces char values {:jolt/type :jolt/char :ch N}
(let [form (parse-string "\\newline")]
  (assert (struct? form) "char is struct")
  (assert (= :jolt/char (form :jolt/type)) "char type")
  (assert (= 10 (form :ch)) "newline codepoint"))

(let [form (parse-string "\\a")]
  (assert (= 97 (form :ch)) "simple char codepoint"))

(print "All reader tests passed!")
