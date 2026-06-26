;; clojure.core — lazy tier. Canonical CLJS-based lazy seq fns.
;; Loaded after 30-macros.clj, so lazy-seq macro is available.
;;
;; Each fn ported from CLJS core.cljs, stripped of chunked-seq branches.

;; --- distinct --- (transducer + lazy collection arity; value-based dedup)
(defn distinct
  ([]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn ([] (rf)) ([result] (rf result))
         ([result input]
          (if (contains? @seen input)
            result
            (do (vswap! seen conj input) (rf result input))))))))
  ([coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[f :as xs] seen]
                     (when-let [s (seq xs)]
                       (if (contains? seen f)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj seen f))))))
                    xs seen)))]
     (step coll #{}))))


;; --- keep ---
(defn keep
  ([f]
   (fn [rf]
     (fn ([] (rf)) ([result] (rf result))
       ([result input]
        (let [v (f input)]
          (if (nil? v) result (rf result v)))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [x (f (first s))]
        (if (nil? x)
          (keep f (rest s))
          (cons x (keep f (rest s)))))))))

;; --- keep-indexed ---
(defn keep-indexed
  ([f]
   (fn [rf]
     (let [ia (volatile! -1)]
       (fn ([] (rf)) ([result] (rf result))
         ([result input]
          (let [i (vswap! ia inc)
                v (f i input)]
            (if (nil? v) result (rf result v))))))))
  ([f coll]
   (letfn [(keepi [idx coll]
             (lazy-seq
               (when-let [s (seq coll)]
                 (let [x (f idx (first s))]
                   (if (nil? x)
                     (keepi (inc idx) (rest s))
                     (cons x (keepi (inc idx) (rest s))))))))]
     (keepi 0 coll))))

;; --- map-indexed ---
(defn map-indexed
  ([f]
   (fn [rf]
     (let [i (volatile! -1)]
       (fn ([] (rf)) ([result] (rf result))
         ([result input] (rf result (f (vswap! i inc) input)))))))
  ([f coll]
   (letfn [(mapi [idx coll]
             (lazy-seq
               (when-let [s (seq coll)]
                 (cons (f idx (first s)) (mapi (inc idx) (rest s))))))]
     (mapi 0 coll))))

;; --- cycle ---
;; Lazy, like the JVM: never counts coll, so it terminates on a lazy/infinite
;; argument instead of forcing it.
(defn cycle [coll]
  (if (seq coll)
    (lazy-seq (concat coll (cycle coll)))
    ()))

;; --- repeatedly --- ((f) throws on a non-fn; (take n …) throws on a non-number
;; count — both enforced by the host (jolt-call / take), so the canonical
;; CLJ form matches the repeatedly.cljc exception cases.)
(defn repeatedly
  ([f] (lazy-seq (cons (f) (repeatedly f))))
  ([n f] (take n (repeatedly f))))

;; --- repeat ---
(defn repeat
  ([x] (lazy-seq (cons x (repeat x))))
  ([n x] (take n (repeat x))))

;; --- iterate ---
;; f is applied lazily, inside the tail thunk — (first (iterate f x)) is x with no
;; call to f, matching clojure.lang.Iterate. Wrapping the whole body in lazy-seq
;; instead would force (f x) the moment the head realizes (it is an eager argument
;; to cons), realizing one step ahead.
(defn iterate [f x]
  (cons x (lazy-seq (iterate f (f x)))))


;; --- partition-all --- (transducer + [n coll] + [n step coll])
;; The collection arities realize EXACTLY n per chunk via a first/rest loop and
;; continue from the advanced cursor (not a re-drop / nthrest), so they realize
;; minimally — the laziness counters depend on this.
;; (A take/nthrest form is correct but over-realizes.)
(defn partition-all
  ([n]
   (fn [rf]
     (let [a (volatile! [])]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (zero? (count @a))
                         result
                         (let [v @a] (vreset! a []) (unreduced (rf result v))))]
            (rf result)))
         ([result input]
          (vswap! a conj input)
          (if (= n (count @a))
            (let [v @a] (vreset! a []) (rf result v))
            result))))))
  ([n coll]
   (letfn [(go [s]
             (lazy-seq
               (when (seq s)
                 ;; realize exactly n via first/rest (minimal realization), but emit
                 ;; the chunk as a SEQ — JVM partition-all chunks are seqs, not
                 ;; vectors (partitionv-all is the vector variant).
                 (loop [i 0 acc () cur s]
                   (if (and (< i n) (seq cur))
                     (recur (inc i) (cons (first cur) acc) (rest cur))
                     (cons (reverse acc) (go cur)))))))]
     (go coll)))
  ([n step coll]
   (letfn [(go [s]
             (lazy-seq
               (when (seq s)
                 (cons (take n s) (go (nthrest s step))))))]
     (go coll))))

;; --- canonical lazy + transducer arities -------------------------------------

(defn interpose
  ([sep]
   (fn [rf]
     (let [started (volatile! false)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (deref started)
            (let [sepr (rf result sep)]
              (if (reduced? sepr)
                sepr
                (rf sepr input)))
            (do (vreset! started true)
                (rf result input))))))))
  ([sep coll]
   (drop 1 (interleave (repeat sep) coll))))

(defn take-nth
  ([n]
   (fn [rf]
     (let [iv (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [i (vswap! iv inc)]
            (if (zero? (rem i n))
              (rf result input)
              result)))))))
  ([n coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (cons (first s) (take-nth n (drop n s)))))))

;; --- pmap family: parallel map over real-thread futures ----------------------
;; Each element's work runs on its own OS thread with SNAPSHOT semantics
;; (futures marshal captured state — pure fns only, mutations don't propagate
;; back). All futures are spawned up front (doall), then derefed in order:
;; coarse-grained work only, as with Clojure's pmap.

(defn pmap
  ([f coll]
   (map deref (doall (map (fn [x] (future (f x))) coll))))
  ([f coll & colls]
   (pmap (fn [xs] (apply f xs)) (apply map vector coll colls))))

(defn pcalls [& fns] (pmap (fn [f] (f)) fns))
