;; The runtime specializer's payoff on tools.reader, as a RATIO in one process
;; (#1861).
;;
;; tools.reader reads every char through three protocol calls: an
;; IndexingPushbackReader over a PushbackReader over a StringReader. The #1713
;; benchmarks (tools.reader read-string, rewrite-clj parse-string) spend much of
;; their time there. Sampling finds those sites hot and monomorphic, and the
;; specializer recompiles the outer readers' methods with the inner ones inlined
;; behind a guard, their fields read and written with no receiver test
;; (host/chez/specialize.ss). On the real libraries that took read-char from 83
;; to 64 ns/char and the benchmarks 10-15%.
;;
;; Two arms run the same token-read mix -- peek-char, read-char, and every
;; eighth char unread and read again -- over an indexing push-back reader. One
;; is tools.reader's own reader-types (vendor/tools.reader, pinned), run with
;; sampling on, so the specializer recompiles it. The other is a verbatim copy
;; of the same file loaded under another name, run with sampling off, so it
;; keeps every call. Samples alternate, best of N. Without the specializer
;; the arms read ~1.0. A QUIET-GATE: a neighbour's burst on one arm moves a
;; ratio.
(ns specialize-ratio-test
  (:require [clojure.string :as str]
            [clojure.tools.reader.reader-types :as rt]
            [jolt.scheme :as scheme]))

(def ^:private ceiling 0.9)
(def ^:private samples 7)

(defn- sampling! [on?] (scheme/eval-string (if on? "(set-specialize-flag! #t)" "(set-specialize-flag! #f)")))

;; reader-types again as copy.reader-types, from the same source text
(def ^:private src-path "vendor/tools.reader/src/main/clojure/clojure/tools/reader/reader_types.clj")
(load-string (-> (slurp src-path)
                 (str/replace "clojure.tools.reader.reader-types" "copy.reader-types")
                 (str/replace "clojure.tools.reader.reader_types" "copy.reader_types")))
(in-ns 'specialize-ratio-test)

(def ^:private text (apply str (repeat 4000 "(defn f [x] (inc x))\n")))

;; [make peek read unread] for one copy of reader-types
(defn- arm [ns-sym]
  (let [v (fn [n] @(ns-resolve ns-sym n))]
    [(v 'indexing-push-back-reader) (v 'peek-char) (v 'read-char) (v 'unread)]))

(defn- drain-ns [[make peek-char read-char unread]]
  (let [r (make text)]
    (loop [n 0]
      (if (peek-char r)
        (let [c (read-char r)]
          (when (zero? (bit-and n 7)) (unread r c) (read-char r))
          (recur (inc n)))
        n))))

(defn- ns-per-char [a]
  (let [t0 (System/nanoTime) n (drain-ns a)]
    (/ (double (- (System/nanoTime) t0)) n)))

(def ^:private real (arm 'clojure.tools.reader.reader-types))
(def ^:private copy (arm 'copy.reader-types))

(defn- run-real [] (sampling! true) (ns-per-char real))
(defn- run-copy [] (sampling! false) (ns-per-char copy))

;; warm both; the real arm gets hot enough to sample, agree and recompile
(dotimes [_ 3] (run-real) (run-copy))

(def ^:private recompiled (scheme/eval-string "(hashtable-size spec-proc-owner)"))

(let [pairs (vec (repeatedly samples (fn [] [(run-real) (run-copy)])))
      on (apply min (map first pairs))
      off (apply min (map second pairs))
      ratio (/ on off)]
  (println (format "specialize-ratio: tools.reader indexing reader, specialized %.1f ns/char, unspecialized %.1f, ratio %.2f (ceiling %.2f, %d recompiled)"
                   on off ratio ceiling recompiled))
  (sampling! true)
  (cond
    (zero? recompiled)
    (do (println "FAIL specialize-ratio: nothing was specialized") (System/exit 1))
    (> ratio ceiling)
    (do (println "FAIL specialize-ratio: the specialized reader chain is not faster than the unspecialized one")
        (System/exit 1))
    :else (println "specialize-ratio: passed")))
