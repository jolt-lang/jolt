; Jolt Standard Library: clojure.string
; String manipulation functions using Jolt core string interop.

(defn blank?
  [s]
  (if (nil? s) true
    (= 0 (count (str-trim s)))))

;; The case fns and the searches take any Object s through its toString, like
;; the reference ((upper-case :kw) is ":KW", (capitalize 1) is "1"); nil throws
;; like calling a method on null.
(defn- to-str [s]
  (if (nil? s)
    (throw (new NullPointerException "s"))
    (.toString s)))
(defn capitalize
  [s]
  (let [s (to-str s)]
    (if (< 1 (count s))
      (str (str-upper (subs s 0 1))
           (str-lower (subs s 1)))
      (str-upper s))))

(defn lower-case
  [s]
  (str-lower (to-str s)))

(defn upper-case
  [s]
  (str-upper (to-str s)))

(defn includes?
  [s substr]
  (not (nil? (str-find substr (to-str s)))))

(defn join
  
  ([coll] (str-join coll))
  ([separator coll] (str-join coll separator)))

(defn replace
  [s match replacement]
  (str-replace-all match replacement (to-str s)))

(defn replace-first
  [s match replacement]
  (str-replace match replacement (to-str s)))

(defn reverse
  [s]
  (str-reverse-b s))

(defn str-reverse
  [s]
  (str-reverse-b s))

(defn split
  ([s re] (split s re 0))
  ([s re limit]
   ;; Java Pattern.split semantics: limit > 0 caps the parts (trailing empties
   ;; kept); limit < 0 splits fully and keeps trailing empties; limit 0 (the
   ;; default) splits fully then drops trailing empty strings — but a no-match
   ;; result ([input], the only 1-element case) is returned as-is.
   (let [parts (vec (str-split re s (if (pos? limit) limit nil)))]
     (if (and (zero? limit) (> (count parts) 1))
       (loop [v parts] (if (and (seq v) (= "" (peek v))) (recur (pop v)) v))
       parts))))

(defn split-lines
  "Split s on \\n or \\r\\n, returning a vector of lines."
  [s]
  (vec (str-split #"\r?\n" s)))

(defn starts-with?
  [s substr]
  (when (nil? substr) (throw (new NullPointerException "substr")))
  (let [s (to-str s)
        slen (count s) slen2 (count substr)]
    (and (>= slen slen2)
         (= (subs s 0 slen2) substr))))

(defn ends-with?
  [s substr]
  (when (nil? substr) (throw (new NullPointerException "substr")))
  (let [s (to-str s)
        slen (count s) slen2 (count substr)]
    (and (>= slen slen2)
         (= (subs s (- slen slen2)) substr))))

(defn trim
  
  [s]
  (str-trim s))

(defn triml
  
  [s]
  (str-triml s))

(defn trimr
  
  [s]
  (str-trimr s))

(defn escape
  [s cmap]
  (when (nil? s) (throw (new NullPointerException "s")))
  (apply str
    (map (fn [ch]
           (if-let [rep (cmap ch)] rep (str ch)))
         s)))

(defn index-of
  "0-based index of the first occurrence of value in s, or nil."
  ([s value]
   (str-find value (to-str s)))
  ([s value from]
   (let [idx (str-find value (subs (to-str s) from))]
     (when idx (+ from idx)))))

(defn last-index-of
  
  ([s value]
   (let [r (str-reverse-b s) sval (str-reverse-b value)
         idx (str-find sval r)]
     (when idx (- (count s) (+ idx (count value))))))
  ([s value from]
   (let [sub (subs s 0 from) r (str-reverse-b sub) sval (str-reverse-b value)
         idx (str-find sval r)]
     (when idx (- from (+ idx (count value)))))))

(defn re-quote-replacement
  "Escape special characters (backslash and dollar) in a regex replacement
  string so it is used literally rather than interpreted."
  [replacement]
  (apply str
    (map (fn [ch]
           (let [c (str ch)]
             (if (or (= c "\\") (= c "$")) (str "\\" c) c)))
         (seq replacement))))

;; Ported from clojure.string/trim-newline (CharSequence interop replaced with
;; portable count/subs). Removes all trailing \n or \r characters.
(defn trim-newline
  "Removes all trailing newline \\n or return \\r characters from
  string.  Similar to Perl's chomp."
  [s]
  (loop [index (count s)]
    (if (zero? index)
      ""
      (let [c (subs s (dec index) index)]
        (if (or (= c "\n") (= c "\r"))
          (recur (dec index))
          (subs s 0 index))))))
