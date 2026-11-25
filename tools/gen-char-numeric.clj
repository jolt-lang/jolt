;; Regenerates the char-numeric-runs table in host/chez/java/host-static-methods.ss
;; (between its GENERATED markers): the code points whose
;; Character.getNumericValue is not derivable from the general category alone.
;; The derivable ones are a decimal digit (Nd, its offset in its run of ten) and
;; the ASCII and fullwidth Latin letters (10..35); everything else answers -1
;; unless it is listed here (Roman numerals, superscripts, circled numbers, and
;; the fractions and other non-integers, which answer -2).
;;
;;   JAVA_CMD=/opt/homebrew/opt/openjdk@21/bin/java clojure -M tools/gen-char-numeric.clj
;;
;; Run it on the JDK the conformance oracle is pinned to (test/conformance/profile.edn).
(require '[clojure.string :as str])

(defn- derivable [cp]
  (cond (= (Character/getType (int cp)) Character/DECIMAL_DIGIT_NUMBER) ::nd
        (<= 0x41 cp 0x5A) (+ 10 (- cp 0x41))
        (<= 0x61 cp 0x7A) (+ 10 (- cp 0x61))
        (<= 0xFF21 cp 0xFF3A) (+ 10 (- cp 0xFF21))
        (<= 0xFF41 cp 0xFF5A) (+ 10 (- cp 0xFF41))
        :else -1))

;; The Nd rule must hold for every digit, or the table is not enough.
(doseq [cp (range 0 0x110000)
        :when (= ::nd (derivable cp))]
  (assert (= (Character/getNumericValue (int cp)) (Character/digit (int cp) 10))
          (format "U+%04X: Nd digit whose numeric value is not its digit" cp)))

(def rows
  (for [cp (range 0 0x110000)
        :let [v (Character/getNumericValue (int cp)) d (derivable cp)]
        :when (and (not= d ::nd) (not= v d))]
    [cp v]))

;; runs of consecutive code points: [start end first-value step], step 0 or 1
(def runs
  (mapv (fn [[s e v st]] [s e v (or st 0)])
        (reduce (fn [acc [cp v]]
                  (let [[s e v0 st :as l] (peek acc)]
                    (if (and l (= cp (inc e))
                             (case st
                               1 (= v (+ v0 (- cp s)))
                               0 (= v v0)
                               nil (or (= v v0) (= v (inc v0)))))
                      (conj (pop acc) [s cp v0 (or st (if (= v v0) 0 1))])
                      (conj acc [cp cp v nil]))))
                [] rows)))

(def file "host/chez/java/host-static-methods.ss")
(def begin-mark ";; BEGIN GENERATED char-numeric-runs")
(def end-mark ";; END GENERATED char-numeric-runs")
(def body
  (str begin-mark " (tools/gen-char-numeric.clj, JDK "
       (System/getProperty "java.version") ") — do not edit.\n"
       "(define char-numeric-runs '#(\n"
       (str/join "\n" (map #(str "  " (str/join " " %)) (partition-all 16 (apply concat runs))))
       "))\n"
       end-mark))
(let [src (slurp file)
      a (str/index-of src begin-mark)
      b (str/index-of src end-mark)]
  (assert (and a b) "markers not found")
  (spit file (str (subs src 0 a) body (subs src (+ b (count end-mark)))))
  (println (count runs) "runs written to" file))
