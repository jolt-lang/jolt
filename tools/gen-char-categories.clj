;; Regenerates the char-category-overrides table in
;; host/chez/java/regex-translate.ss (between its GENERATED markers): the code
;; points where Chez's Unicode tables disagree with the JDK's about the general
;; category or the Uppercase / Lowercase / Alphabetic properties. Chez follows a
;; newer Unicode than the JDK the conformance oracle is pinned to, so the code
;; points Unicode assigned since then are UNASSIGNED to java.lang.Character (not
;; a letter, not a digit), and a few changed category.
;;
;;   CHEZ=.cache/local/chezscheme-10.4.1/bin/scheme \
;;   JAVA_CMD=/opt/homebrew/opt/openjdk@21/bin/java clojure -M tools/gen-char-categories.clj
;;
;; Run it on the JDK test/conformance/profile.edn pins, with the Chez the build uses.
(require '[clojure.java.shell :as sh]
         '[clojure.string :as str])

(def type-names
  {0 "Cn" 1 "Lu" 2 "Ll" 3 "Lt" 4 "Lm" 5 "Lo" 6 "Mn" 7 "Me" 8 "Mc" 9 "Nd" 10 "Nl"
   11 "No" 12 "Zs" 13 "Zl" 14 "Zp" 15 "Cc" 16 "Cf" 18 "Co" 19 "Cs" 20 "Pd" 21 "Ps"
   22 "Pe" 23 "Pc" 24 "Po" 25 "Sm" 26 "Sc" 27 "Sk" 28 "So" 29 "Pi" 30 "Pf"})

(def scalar? #(not (<= 0xD800 % 0xDFFF)))

;; flags: 4 Uppercase, 2 Lowercase, 1 Alphabetic
(defn- jdk [cp]
  [(type-names (Character/getType (int cp)))
   (+ (if (Character/isUpperCase (int cp)) 4 0)
      (if (Character/isLowerCase (int cp)) 2 0)
      (if (Character/isAlphabetic (int cp)) 1 0))])

(def chez-dump
  (let [script (java.io.File/createTempFile "chez-categories" ".ss")]
    (spit script
          (str "(let loop ((cp 0))"
               "  (when (<= cp #x10FFFF)"
               "    (unless (and (>= cp #xD800) (<= cp #xDFFF))"
               "      (let ((c (integer->char cp)))"
               "        (display (char-general-category c)) (display \" \")"
               "        (display (+ (if (char-upper-case? c) 4 0) (if (char-lower-case? c) 2 0)"
               "                    (if (char-alphabetic? c) 1 0)))"
               "        (newline)))"
               "    (loop (+ cp 1))))"))
    (let [{:keys [exit out err]} (sh/sh (or (System/getenv "CHEZ") "scheme") "--script" (str script))]
      (assert (zero? exit) err)
      (mapv #(let [[c f] (str/split % #" ")] [c (parse-long f)]) (str/split-lines out)))))

(def diffs
  (map (fn [cp chez] (when (not= chez (jdk cp)) (into [cp] (jdk cp))))
       (filter scalar? (range 0 0x110000))
       chez-dump))

;; runs of consecutive code points with the same JDK answer: [start end cat flags]
(def runs
  (reduce (fn [acc [cp c f]]
            (let [[s e c0 f0 :as l] (peek acc)]
              (if (and l (= cp (inc e)) (= c c0) (= f f0))
                (conj (pop acc) [s cp c f])
                (conj acc [cp cp c f]))))
          [] (remove nil? diffs)))

(def file "host/chez/java/regex-translate.ss")
(def begin-mark ";; BEGIN GENERATED char-category-overrides")
(def end-mark ";; END GENERATED char-category-overrides")
(def body
  (str begin-mark " (tools/gen-char-categories.clj, JDK "
       (System/getProperty "java.version") ") — do not edit.\n"
       "(define char-category-overrides '#(\n"
       (str/join "\n" (map #(str "  " (str/join " " %)) (partition-all 16 (apply concat runs))))
       "))\n"
       end-mark))
(let [src (slurp file)
      a (str/index-of src begin-mark)
      b (str/index-of src end-mark)]
  (assert (and a b) "markers not found")
  (spit file (str (subs src 0 a) body (subs src (+ b (count end-mark)))))
  (println (count runs) "runs written to" file))
(shutdown-agents)
