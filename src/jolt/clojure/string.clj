; Jolt Standard Library: clojure.string
; String manipulation functions using Jolt core string interop.

(defn blank?
  [s]
  (if (nil? s) true
    (= 0 (count (str-trim s)))))

(defn capitalize
  "Converts first character of the string to upper-case, all other
  characters to lower-case."
  [s]
  (if (< 1 (count s))
    (str (str-upper (subs s 0 1))
         (str-lower (subs s 1)))
    (str-upper s)))

(defn lower-case
  "Converts string to all lower-case."
  [s]
  (str-lower s))

(defn upper-case
  "Converts string to all upper-case."
  [s]
  (str-upper s))

(defn includes?
  "True if s includes substr."
  [s substr]
  (not (nil? (str-find substr s))))

(defn join
  "Returns a string of all elements in coll, separated by
  an optional separator."
  ([coll] (str-join coll))
  ([separator coll] (str-join coll separator)))

(defn replace
  "Replaces all instance of match with replacement in s."
  [s match replacement]
  (str-replace-all match s replacement))

(defn replace-first
  "Replaces the first instance of pattern in string with replacement."
  [s match replacement]
  (str-replace match s replacement))

(defn str-reverse
  "Returns s with its characters reversed."
  [s]
  (str-reverse-b s))

(defn split
  "Splits string on a regular expression. Optional limit."
  ([s re]
   (map str-trim (str-split re s)))
  ([s re limit]
   (take limit (split s re))))

(defn starts-with?
  "True if s starts with substr."
  [s substr]
  (let [slen (count s) slen2 (count substr)]
    (and (>= slen slen2)
         (= (subs s 0 slen2) substr))))

(defn ends-with?
  "True if s ends with substr."
  [s substr]
  (let [slen (count s) slen2 (count substr)]
    (and (>= slen slen2)
         (= (subs s (- slen slen2)) substr))))

(defn trim
  "Removes whitespace from both ends of string."
  [s]
  (str-trim s))

(defn triml
  "Removes whitespace from the left side of string."
  [s]
  (str-triml s))

(defn trimr
  "Removes whitespace from the right side of string."
  [s]
  (str-trimr s))

(defn trim-newline
  "Removes all trailing newline \\n or return \\r characters from string."
  [s]
  (var result s)
  (while (or (= (subs result (dec (count result))) "\n")
             (= (subs result (dec (count result))) "\r"))
    (set result (subs result 0 (dec (count result)))))
  result)

(defn escape
  "Return a new string, using cmap to escape each character ch from s."
  [s cmap]
  (apply str
    (map (fn [ch]
           (if-let [rep (cmap ch)] rep (str ch)))
         s)))

(defn index-of
  "Return index of value (string or char) in s, optionally
  from start. Returns nil if not found."
  ([s value]
   (let [idx (str-find value s)]
     (when idx (inc idx))))
  ([s value from]
   (let [idx (str-find value (subs s from))]
     (when idx (+ from (inc idx))))))

(defn last-index-of
  "Return last index of value (string or char) in s."
  ([s value]
   (let [r (str-reverse-b s) sval (str-reverse-b value)
         idx (str-find sval r)]
     (when idx (inc (- (count s) (+ idx (count value)))))))
  ([s value from]
   (let [sub (subs s 0 from) r (str-reverse-b sub) sval (str-reverse-b value)
         idx (str-find sval r)]
     (when idx (inc (- from (+ idx (count value))))))))
