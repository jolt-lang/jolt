;; typed-records — DECLARED FIELD TYPES on a record: what the compiler can do with
;; `^double`, `^long` and `^String` on a defrecord field, at construction and at
;; every read.
;;
;; `binary-trees` measures record ALLOCATION as GC pressure and `mono-dispatch`
;; measures protocol calls on a record; neither says anything about the field
;; TYPES, because neither record declares any. That is the gap here. Two axes:
;;
;;   Construction. A record whose field layout is known can be built by an inlined
;;   per-arity constructor — one allocation, no var deref, no argument list, no
;;   hashtable lookup. A `^double` field disqualified the whole record from that
;;   path, because the inlined form did not apply the widening the dispatched
;;   constructor does. So a coordinate record — precisely the shape `^double`
;;   exists for — always paid the slow constructor. `make-vecs` is that shape.
;;
;;   Field reads. A field's declared tag is what a read can be typed by. `^double`
;;   already flowed (it is what lets `(* (:x v) (:x v))` unbox); `^long` and
;;   `^String` did not, so a read off them answered "unknown" and everything
;;   downstream — the arithmetic, an interop call on the string, `count` — fell
;;   back to the dynamic path. `walk-rows` reads all three kinds and uses each the
;;   way the tag promises: arithmetic on the numbers, string operations on the
;;   string.
;;
;; The two are separated deliberately: `make-vecs` allocates and barely reads,
;; `walk-rows` reads a fixed set of rows and allocates nothing, so a change to one
;; path cannot be mistaken for a change to the other.
;;
;; Portable Clojure (jolt + JVM Clojure). On the JVM these tags are primitive
;; fields and a String field, which is the reference this is measured against.
;;   bench/run.sh typed-records 20000
(ns typed-records)

(defrecord Vec3 [^double x ^double y ^double z])
(defrecord Row [^long id ^String name ^double score])

;; --- construction: the inlined-constructor path ------------------------------
;; Integer arguments into ^double fields on purpose — that is the case the
;; inlined form has to widen, and the reason the fast path used to be refused.
;;
;; The `sink` write is what makes this measure a CONSTRUCTOR at all. A record that
;; never leaves the loop is removed outright by scalar-replace — an earlier draft
;; of this benchmark ran at 3ns an iteration and was timing nothing. Storing into
;; an array makes it escape, so the allocation is real.
(defn make-vecs ^double [^long n]
  (let [sink (object-array 1)]
    (loop [i 0 acc 0.0]
      (if (< i n)
        (let [v (->Vec3 i (+ i 1) 2.5)
              w (->Vec3 (:x v) (:y v) (:z v))]
          (aset sink 0 w)
          (recur (inc i) (+ acc (:z w))))
        acc))))

;; --- reads: each field used the way its declared tag promises ----------------
;; The `^Row` hint is load-bearing, and stating why matters: a declared field tag
;; can only type a read once the RECEIVER is proven. Here the rows arrive from a
;; top-level vector through a reduce closure, so nothing infers their type and
;; without the hint every field read answers "unknown" — an unhinted draft of this
;; benchmark emitted byte-identical code before and after the field-tag work. A
;; defrecord's own protocol methods get this hint on `this` for free; a plain
;; function has to say it.
(defn row-work ^double [^Row r]
  (let [id (:id r)                       ; ^long
        nm (:name r)                     ; ^String
        sc (:score r)]                   ; ^double
    (+ (* sc 1.5)
       (* 1.0e-6
          (unchecked-add
           (unchecked-add (unchecked-multiply id 3) (.length nm))
           (unchecked-add (count nm)
                          (unchecked-add (.indexOf nm "-")
                                         (count (.toUpperCase nm)))))))))

(defn walk-rows ^double [rows ^long passes]
  (loop [p 0 acc 0.0]
    (if (< p passes)
      (recur (inc p)
             (+ acc (reduce (fn [a r] (+ a (row-work r))) 0.0 rows)))
      acc)))

(def rows (mapv (fn [i] (->Row i (str "row-name-" i) (* i 0.25))) (range 32)))

(defn run ^double [^long n]
  (+ (make-vecs n) (walk-rows rows (quot n 32))))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 20000)]
    (dotimes [_ 2] (run (quot n 2)))                     ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "typed-records n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
