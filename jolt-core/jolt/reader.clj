(ns jolt.reader
  "Portable Clojure reader: source text -> reader forms (Chez Phase 3, jolt-cf1q.4).

  The no-Janet replacement for src/jolt/reader.janet. All the lexing/parsing LOGIC
  is portable Clojure; form CONSTRUCTION and string->number parsing delegate to the
  jolt.host contract (form-make-symbol/char, form-char-from-name, form-scan-number)
  — a Clojure source file cannot write a {:jolt/type :symbol} literal (it parses as
  a tagged reader form), and the concrete form representation is the host's to own.
  Same split the analyzer uses for the form-* readers. Once cross-compiled this runs
  ON Chez so compile-from-source needs no Janet reader.

  Positions are CHARACTER indices (the Janet reader uses byte indices); for ASCII
  source they coincide, and form VALUES are identical either way — the parity gate
  compares values, not positions.

  INCREMENT 5a (jolt-50xx): the ATOM layer — whitespace/comments, symbols (+ nil/
  true/false), keywords, strings, numbers (sign/hex/radix/ratio/fractional/
  exponent, trailing N/M), characters. Collections, quote/deref/meta, and dispatch
  (#) land in 5b/5c (they throw not-yet-ported so a hit is loud)."
  (:require [clojure.string :as str]
            [jolt.host :refer [form-make-symbol form-make-char form-char-from-name
                               form-scan-number]]))

;; Source access by CHARACTER codepoint, mirroring the Janet reader's byte access
;; (identical for ASCII). cp = codepoint at i; len = character count.
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
  ;; pos is at the first colon; ::foo is treated as :foo (no auto-resolution),
  ;; matching the Janet reader.
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
(defn- number-start? [s pos c]
  (or (digit? c)
      (and (= c 45) (< (inc pos) (len s)) (digit? (cp s (inc pos))))
      (and (= c 43) (< (inc pos) (len s)) (digit? (cp s (inc pos))))))

(defn read-form [s pos]
  (let [pos (skip-whitespace s pos)]
    (if (>= pos (len s))
      [nil pos]
      (let [c (cp s pos)]
        (cond
          (= c 59) (read-form s (read-until-newline s pos))   ; ; comment: skip + reread
          (= c 34) (read-string* s pos)
          (= c 58) (read-keyword* s pos)
          (= c 92) (read-char* s pos)
          (number-start? s pos c) (read-number* s pos)
          (symbol-start? c) (read-symbol* s pos)
          :else (throw (ex-info (str "read-form: not yet ported (inc 5a atoms): '"
                                     (char c) "' (" c ")") {})))))))

(defn read-one
  "Read the first form of `s` (skipping leading trivia). Returns the form."
  [s]
  (first (read-form s 0)))
