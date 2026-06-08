;; clojure.core — seq tier. Pure-Clojure leaf sequence fns on top of the kernel
;; tier (00-kernel) and the Janet seed. Loaded after the kernel tier; in compile
;; mode these self-host through the now-built analyzer (interpreted otherwise).
;;
;; Migration rule for adding fns here: the fn must (1) NOT be in
;; compiler/core-renames (that map emits core-X Janet symbols directly), (2) have
;; no internal Janet callers of its core-X binding, and (3) NOT be used by the
;; self-hosted compiler (jolt-core/jolt/*.clj). Compiler-facing structural fns go
;; in the kernel tier (00-kernel) instead — see its header.

(defn ffirst [coll] (first (first coll)))
(defn nfirst [coll] (next (first coll)))
(defn fnext  [coll] (first (next coll)))
(defn nnext  [coll] (next (next coll)))

;; Canonical Clojure defs: pure first/next/loop/recur, no Janet realize-for-iteration.
(defn last [s]
  (if (next s) (recur (next s)) (first s)))

(defn butlast [s]
  (loop [ret [] s s]
    (if (next s)
      (recur (conj ret (first s)) (next s))
      (seq ret))))

;; Lazy partition-by: groups consecutive elements by (f x), matching Clojure/CLJS.
(defn partition-by [f coll]
  (let [step (fn step [s]
               (lazy-seq
                 (let [s (seq s)]
                   (when s
                     (let [fst (first s)
                           fv (f fst)
                           run (cons fst (take-while (fn [x] (= fv (f x))) (rest s)))]
                       (cons run (step (lazy-seq (drop (count run) s)))))))))]
    (step coll)))

;; Lazy partition: yields complete partitions of size n. Optional step.
;; Ported from CLJS core.cljs (no chunked-seq branches).
(defn partition
  ([n coll] (partition n n coll))
  ([n step coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [p (take n s)]
        (when (= n (count p))
          (cons p (partition n step (nthrest coll step)))))))))

;; Lazy concat: concatenates colls lazily. Ported from CLJS core.cljs.
(defn concat
  ([] (lazy-seq nil))
  ([x] (lazy-seq x))
  ([x y]
   (lazy-seq
    (let [s (seq x)]
      (if s
        (cons (first s) (concat (rest s) y))
        y))))
  ([x y & zs]
   (let [step (fn step [xys zs]
                (lazy-seq
                  (let [xys (seq xys)]
                    (if xys
                      (cons (first xys) (step (rest xys) zs))
                      (when zs
                        (step (first zs) (next zs)))))))]
     (step (concat x y) zs))))
