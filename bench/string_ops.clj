;; string-ops — INTEROP CALLS ON A PROVEN STRING OR KEYWORD, and the
;; `clojure.string` wrappers over them. Per-iteration work is a handful of method
;; calls on short strings, so the dispatch shape dominates over the operation.
;;
;; `string-build` measures the other side of interop — a StringBuilder target,
;; where the cost is the appending — and `char-scan` measures `.charAt` plus the
;; numeric casts around it. Neither reaches the ordinary String surface a library
;; actually calls: `.indexOf`, `.startsWith`, `.substring`, `.toLowerCase`.
;;
;; The axis is whether the call site can be resolved at compile time. An interop
;; call on an unproven target routes through a generic dispatcher that finds a
;; method table by hashing the target's tag string, finds the handler by hashing
;; the method name, and passes the arguments as a vector converted back to a list
;; for apply. When the target is PROVEN a string — by a `^String` hint on a param
;; or binding, or by the types pass proving it with no hint in the source — the
;; back end can emit the operation directly instead. Both proof seams are here:
;; `scan-hinted` carries the hint, `scan-proven` has none and depends on
;; inference. Keywords get the same treatment, so `.getName`/`.getNamespace` on a
;; hinted keyword are timed too.
;;
;; `chain-proven` is the follow-up seam. An interop call used to answer :any, so
;; only the FIRST call in a chain could be proven — (.toUpperCase (.trim s)) knew
;; its inner target and not its outer one, and emitted a runtime string? test plus
;; a dispatch fallback around the outer call. Once a method's return type is known
;; the whole chain proves, and so does a let-bound interop result. `direct-methods`
;; covers the methods that had no direct form at all: `.equals` on two proven
;; strings emitted a KEYWORD type test and then fell through to generic dispatch.
;;
;; `core-over-strings` is the same proof reaching clojure.core rather than interop:
;; `count` on a proven string, and `str` over proven strings, which otherwise pay a
;; var deref plus jolt-invoke plus a type walk (jolt-count tries pvec/pmap/pset
;; before string) to do a string-length and a string-append.
;;
;; `strfns` is the `clojure.string` layer: every fn there coerces its argument
;; through `to-str`, which took `.toString` unconditionally, so passing a plain
;; string paid the full dispatch chain to reach the String surface — over 90% of
;; `(upper-case "sel")` was deciding how to dispatch, on top of a ~40ns upcase.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh string-ops 100000
(ns string-ops
  (:require [clojure.string :as str]))

(def entity "some.qualified/entity-name")
(def words (mapv (fn [i] (str "word" i)) (range 16)))
(def kws (mapv (fn [i] (keyword "bench" (str "key" i))) (range 16)))

;; --- hinted String target -----------------------------------------------------
(defn scan-hinted [^String s]
  (unchecked-add
   (unchecked-add (.indexOf s ".") (.length s))
   (unchecked-add (if (.startsWith s "some") 1 0)
                  (count (.substring s 1 5)))))

;; --- unhinted: the target must be PROVEN a string by inference ---------------
(defn scan-proven [s]
  (let [t (str s "!")]
    (unchecked-add
     (unchecked-add (.indexOf t "/") (if (.endsWith t "!") 1 0))
     (count (.toLowerCase t)))))

;; --- chained interop: every target but the first is an interop RESULT ---------
;; Only provable once a method's return type is known; before that each outer call
;; carried a string? test and a dispatch arm.
(defn chain-proven [^String s]
  (unchecked-add
   (.length (.toUpperCase (.trim s)))
   (let [t (.substring s 0 8)]                  ; let-bound interop result
     (unchecked-add (.length (.toLowerCase t))
                    (if (.startsWith (.trim t) "some") 1 0)))))

;; --- methods that had no direct form: they fell to the generic dispatcher -----
(defn direct-methods [^String a ^String b]
  (unchecked-add
   (unchecked-add (if (.equals a b) 1 0) (if (.equalsIgnoreCase a b) 1 0))
   (unchecked-add (.lastIndexOf a "e")
                  (unchecked-add (if (.isBlank a) 1 0) (.compareTo a b)))))

;; --- clojure.core over PROVEN strings ----------------------------------------
(defn core-over-strings [^String s ^String t]
  (unchecked-add
   (unchecked-add (count s) (count t))
   (unchecked-add (count (str s t)) (count (str s "-" t)))))

;; --- the clojure.string layer over arguments that are already strings --------
(defn strfns [s]
  (unchecked-add
   (unchecked-add (count (str/upper-case s)) (count (str/lower-case s)))
   (unchecked-add (if (str/starts-with? s "some") 1 0)
                  (if (str/includes? s "entity") 1 0))))

(defn join-words [ws] (count (str/join "," ws)))

;; --- hinted Keyword target ----------------------------------------------------
(defn kw-parts [^clojure.lang.Keyword k]
  (unchecked-add (count (.getName k))
                 (count (.getNamespace k))))

(defn kw-core [k]
  (unchecked-add (count (name k)) (count (namespace k))))

(defn walk-kws [ks]
  (reduce (fn [acc k] (unchecked-add acc (unchecked-add (kw-parts k) (kw-core k))))
          0 ks))

(defn run [iters]
  (let [s entity
        other "some.qualified/other-name"]
    (loop [i 0 acc 0]
      (if (< i iters)
        (recur (inc i)
               (unchecked-add
                acc
                (unchecked-add
                 (unchecked-add
                  (unchecked-add (scan-hinted s) (scan-proven s))
                  (unchecked-add (chain-proven s)
                                 (unchecked-add (direct-methods s other)
                                                (core-over-strings s other))))
                 (unchecked-add
                  (unchecked-add (strfns s) (join-words words))
                  (walk-kws kws)))))
        acc))))

(defn -main [& args]
  (let [iters (if (seq args) (Integer/parseInt (first args)) 100000)]
    (dotimes [_ 2] (run (quot iters 4)))                 ; warmup
    (let [runs 3
          ts (mapv (fn [_]
                     (let [t0 (System/currentTimeMillis)
                           r (run iters)
                           el (- (System/currentTimeMillis) t0)]
                       (when (zero? r) (println "unexpected zero"))
                       el))
                   (range runs))]
      (println "runs:" ts)
      (println "mean:" (quot (reduce + ts) runs) "ms"))))
