;; Parser combinators, adapted from rm-hull/jasentaa (MIT).

(ns jolt.parser.combinators
  (:require
   [jolt.parser.basic :refer [match from-re]]
   [jolt.parser.monad :as m :refer [>>=]]
   [jolt.parser.collections :refer [join]]))

(defn and-then
  "(ab)"
  [p1 p2]
  (m/do*
   (r1 <- p1)
   (r2 <- p2)
   (m/return (join r1 r2))))

(defn or-else
  "(a|b)

  Non-deterministic choice (++) operator. Applies both parsers
  to the argument string, and appends their list of results."
  [p1 p2]
  (fn [input]
    (lazy-cat (m/bind input p1) (m/bind input p2))))

(defn choice
  "(a|b)

  Deterministic choice (+++) operator. Has the same behaviour
  as `or-else`, except that at most one result is returned."
  [p1 p2]
  (fn [input]
    (let [[x & xs] (m/bind input (or-else p1 p2))]
      (if (nil? x)
        []
        [x]))))

(declare plus)
(declare optional)

(defn many
  "(a*)

  Parse repeated applications of a parser; the many combinator
  permits zero or more applications of the parser."
  [p]
  (optional (plus p)))

(defn plus
  "(a+) is equivalent to (aa*)

  Parse repeated applications of a parser; the plus combinator
  permits one or more applications of the parser."
  [p]
  (m/do*
   (a <- p)
   (as <- (many p))
   (m/return (cons a as))))

(defn optional
  "(a?)

  Parse zero or one applications of a parser. With `default`, that value is
  the result when p does not match (otherwise nil)."
  ([p] (or-else p (m/return nil)))
  ([p default] (or-else p (m/return default))))

(defn any-of [& ps]
  "(a|b|c|...)

  Parse application any of the given parsers."
  (reduce or-else ps))

(def space
  "Parse a single space, tab, newline or carriage-return."
  (any-of
   (match " ")
   (match "\t")
   (match "\n")
   (match "\r")))

(def spaces
  "Parse a string of (zero or more) spaces, tabs, and newlines."
  (many space))

(defn string
  "Parse a specific string."
  [input]
  (reduce and-then (map (comp match str) input)))

(defn token
  "Parse a token using a parser p, throwing away any trailing space."
  [p]
  (m/do*
   (a <- p)
   spaces
   (m/return a)))

(def symb
  "Parse a symbolic token."
  (comp token string))

(defn chain-left
  "Parse repeated applications of a parser p, separated by
  applications of a parser op whose result value is an
  operator that is assumed to associate to the left, and
  which is used to combine the results from the p parsers."
  ([p op a]
   (choice
    (chain-left p op)
    (m/return a)))

  ([p op]
   (m/do*
    (a <- p)
    (rst <- (many
             (m/do*
              (f <- op)
              (b <- p)
              (m/return [f b]))))
    (m/return
     (reduce
      (fn [acc [f b]] (f acc b))
      a
      rst)))))

(defn chain-right
  "Parse repeated applications of a parser p, separated by
  applications of a parser op whose result value is an
  operator that is assumed to associate to the right, and
  which is used to combine the results from the p parsers.
   "
  ([p op a]
   (choice
    (chain-right p op)
    (m/return a)))

  ([p op]
   (m/do*
    (scan <- (many
              (m/do*
               (a <- p)
               (f <- op)
               (m/return [f a]))))
    (b <- p)
    (m/return
     (reduce
      (fn [acc [f a]] (f a acc))
      b
      (reverse scan))))))

(defn separated-by
  "Parse repeated applications of a parser p, separated by
  applications of a parser sep whose result values are
  thrown away.

  This parser will process at least one application of p.
  For a list of zero or more, use `sep-by`."
  [p sep]
  (m/do*
   (fst <- p)
   (rst <- (many (m/do* sep p)))
   (m/return (cons fst rst))))

(defn sep-by
  "Parse zero or more applications of p separated by sep (sep's results are
  discarded), returning a list of p's results (nil when none match). The
  zero-or-more counterpart to `separated-by`."
  [p sep]
  (or-else (separated-by p sep) (m/return nil)))

(defn between
  "Parse p wrapped by open and close (open's and close's results are
  discarded), returning p's result — e.g. (between (symb \"(\") (symb \")\") expr)."
  [open close p]
  (m/do*
   open
   (x <- p)
   close
   (m/return x)))

;; Common character-class parsers, each matching a single character.
(def digit
  "Match a single decimal digit (0-9)."
  (from-re #"[0-9]"))

(def letter
  "Match a single ASCII letter (a-z, A-Z)."
  (from-re #"[a-zA-Z]"))

(def alpha-num
  "Match a single letter or digit."
  (any-of letter digit))
