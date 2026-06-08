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

;; partition-by: (partition-by f) is a stateful transducer (buffer a run, emit on
;; key change, flush on completion — via volatiles, matching Clojure); (partition-by
;; f coll) is the lazy collection arity.
(defn partition-by
  ([f]
   (fn [rf]
     (let [buf (volatile! [])
           pv (volatile! nil)
           started (volatile! false)]
       (fn
         ([] (rf))
         ([result]
          (let [b @buf
                result (if (zero? (count b))
                         result
                         (do (vreset! buf []) (unreduced (rf result b))))]
            (rf result)))
         ([result input]
          (let [val (f input)]
            (if (or (not @started) (= val @pv))
              (do (vreset! started true) (vreset! pv val) (vswap! buf conj input) result)
              (let [b @buf]
                (vreset! buf []) (vreset! pv val)
                (let [ret (rf result b)]
                  (when-not (reduced? ret) (vswap! buf conj input))
                  ret)))))))))
  ([f coll]
   (let [step (fn step [s]
                (lazy-seq
                  (let [s (seq s)]
                    (when s
                      (let [fst (first s)
                            fv (f fst)
                            run (cons fst (take-while (fn [x] (= fv (f x))) (rest s)))]
                        (cons run (step (lazy-seq (drop (count run) s)))))))))]
     (step coll))))