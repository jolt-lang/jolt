;; Hash-mode maps and sets: the HAMT behind PersistentHashMap/PersistentHashSet.
;;   chez --script test/chez/hamt-test.ss
;;
;; A model check and a shape check. Random assoc/dissoc (persistent and through
;; a transient) run against an association-list model compared with jolt=2,
;; over keys that include whole collision families ("Aa"/"BB" strings share a
;; String.hashCode), nil and false keys and values, and keys of mixed kinds.
;; After each run the trie must hold the model's entries, iterate in hash-chunk
;; order (the JVM's order: low five bits first, a collision bucket in insertion
;; order), and keep the node layout: an entry bitmap and a branch bitmap, the
;; entries' key/value slot pairs first and the branches after them.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (evv s) (jolt-compile-eval (string-append "(do " s ")") "user"))
(define (ev s) (jolt-final-str (evv s)))
(define (is name s expect) (ok (string-append name " => " expect) (string=? (ev s) expect)))

;; --- deterministic PRNG (seed printed on failure) -----------------------------
(define seed 12345)
(define (rnd n) (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648)) (modulo (quotient seed 7) n))

;; --- the key universe -----------------------------------------------------------
;; 2^4 strings over {"Aa","BB"} all share one String.hashCode, so they collide on
;; every hash bit; fixnums and keywords spread; nil/false/true are edge keys.
(define collide
  (let loop ((n 4) (acc '("")))
    (if (zero? n) acc
        (loop (- n 1) (apply append (map (lambda (s) (list (string-append s "Aa") (string-append s "BB"))) acc))))))
(define universe
  (list->vector
    (append collide
            (list "" 0.0)                     ; hash 0: nil's bucket, with 0
            (let loop ((i 0) (acc '())) (if (= i 300) acc (loop (+ i 1) (cons i acc))))
            (map (lambda (i) (keyword #f (string-append "k" (number->string i)))) (iota 40))
            (list jolt-nil #f #t -1 (expt 2 40) "x" "y"))))
(define (rand-key) (vector-ref universe (rnd (vector-length universe))))
(define (rand-val) (case (rnd 5) ((0) jolt-nil) ((1) #f) (else (rnd 1000))))

;; --- the model --------------------------------------------------------------------
(define (model-get al k) (let loop ((al al)) (cond ((null? al) #f) ((jolt=2 (caar al) k) (car al)) (else (loop (cdr al))))))
(define (model-assoc al k v)
  (if (model-get al k)
      (map (lambda (p) (if (jolt=2 (car p) k) (cons (car p) v) p)) al)
      ;; nil goes first: the JVM keeps it outside the trie and seqs it ahead of
      ;; the hash-0 keys (0, "", 0.0) it shares a bucket with here
      (if (jolt-nil? k) (cons (cons k v) al) (append al (list (cons k v))))))
(define (model-dissoc al k) (filter (lambda (p) (not (jolt=2 (car p) k))) al))
;; a transient removal from a bucket moves the bucket's last entry into the hole
;; (HashCollisionNode.without(edit, ...)); removing nil, or a key alone in its
;; hash, is a plain removal
(define (model-dissoc! al k)
  (let ((p (model-get al k)))
    (if (or (not p) (jolt-nil? k))
        (model-dissoc al k)
        (let* ((h (key-hash k))
               (last (fold-left (lambda (q e) (if (= (key-hash (car e)) h) e q)) #f al)))
          (if (eq? last p)
              (model-dissoc al k)
              (map (lambda (e) (if (eq? e p) last e)) (filter (lambda (e) (not (eq? e last))) al)))))))

;; --- iteration order: hash chunks low to high, buckets in insertion order ---------
(define (chunk-list h) (let loop ((i 0) (acc '())) (if (> i 30) (reverse acc) (loop (+ i 5) (cons (bitwise-and (bitwise-arithmetic-shift-right h i) 31) acc)))))
(define (chunks<? a b) (cond ((null? a) #f) ((< (car a) (car b)) #t) ((> (car a) (car b)) #f) (else (chunks<? (cdr a) (cdr b)))))
;; a stable sort of the model's insertion order by chunk list: equal hashes keep
;; insertion order, which is a collision bucket's iteration order
(define (expected-order al)
  (map cdr (list-sort (lambda (x y) (chunks<? (car x) (car y)))
                      (map (lambda (p) (cons (chunk-list (key-hash (car p))) p)) al))))

;; --- node shape ----------------------------------------------------------------------
;; entries (two slots each) then branches (one slot each); the two maps disjoint;
;; no entry's key is a branch, every branch slot is one; no emptied branch left
;; below the root; a bucket holds at least two entries, all of its own hash.
(define (shape-ok? nd depth)
  (let* ((dm (hnode-dmap nd)) (nm (hnode-nmap nd)) (arr (hnode-arr nd))
         (d (bitwise-bit-count dm)) (n (bitwise-bit-count nm)))
    (and (= 0 (bitwise-and dm nm))
         (= (vector-length arr) (+ (* 2 d) n))
         (or (= depth 0) (> (+ d n) 0))
         (let loop ((j 0))
           (or (= j d)
               (let ((k (vector-ref arr (* 2 j))))
                 (and (not (hnode? k)) (not (hcoll? k)) (loop (+ j 1))))))
         (let loop ((j 0))
           (or (= j n)
               (let ((b (vector-ref arr (+ (* 2 d) j))))
                 (and (cond ((hnode? b) (shape-ok? b (+ depth 1)))
                            ((hcoll? b) (and (>= (length (hcoll-alist b)) 2)
                                             (for-all (lambda (p) (= (key-hash (car p)) (hcoll-hash b))) (hcoll-alist b))))
                            (else #f))
                      (loop (+ j 1)))))))))
(define (map-shape-ok? m) (let ((r (pmap-root m))) (or (not (hnode? r)) (shape-ok? r 0))))

(define (entries m) (pmap-fold-fwd m (lambda (k v a) (cons (cons k v) a)) '()))
(define (same-entries? m al)
  (and (= (pmap-cnt m) (length al))
       (for-all (lambda (p) (let ((e (pmap-entry-at m (car p)))) (and e (eq? (car e) (car p)) (jolt=2 (cdr e) (cdr p))))) al)
       (for-all (lambda (p) (jolt=2 (pmap-get m (car p) 'none) (cdr p))) al)))
(define (order-ok? m al)
  (let ((got (reverse (entries m))) (want (expected-order al)))
    (and (= (length got) (length want))
         (for-all (lambda (g w) (and (eq? (car g) (car w)) (jolt=2 (cdr g) (cdr w)))) got want))))

;; --- persistent model runs -------------------------------------------------------------
(define (run-persistent steps)
  (let loop ((i 0) (m empty-pmap-hash) (al '()) (ok? #t))
    (if (or (= i steps) (not ok?))
        (and ok? (same-entries? m al) (order-ok? m al) (map-shape-ok? m)
             (for-all (lambda (k) (or (model-get al k) (eq? 'none (pmap-get m k 'none)))) (vector->list universe)))
        (let ((k (rand-key)))
          (if (< (rnd 10) 7)
              (let ((v (rand-val)))
                (loop (+ i 1) (pmap-assoc m k v) (model-assoc al k v) #t))
              (let ((m2 (pmap-dissoc m k)))
                (loop (+ i 1) m2 (model-dissoc al k)
                      ;; a miss answers the map itself
                      (or (model-get al k) (eq? m2 m)))))))))

(let loop ((r 0))
  (when (< r 40)
    (let ((s seed))
      (unless (run-persistent (+ 50 (rnd 600)))
        (ok (format "persistent model run (seed ~a)" s) #f)))
    (loop (+ r 1))))
(ok "persistent model runs" #t)

;; --- transient model runs: start from a persistent map, edit, freeze -------------------
(define (run-transient steps)
  (let* ((base-n (rnd 400))
         (base-al (let loop ((i 0) (al '())) (if (= i base-n) al (loop (+ i 1) (model-assoc al (rand-key) (rand-val))))))
         (base (fold-left (lambda (m p) (pmap-assoc m (car p) (cdr p))) empty-pmap-hash base-al))
         (base-snapshot (entries base))
         (t (jolt-transient-new (if (hnode? (pmap-root base)) base (make-pmap (pmap-root (pmap-assoc empty-pmap-hash 'x 1)) 1)))))
    (if (not (hnode? (pmap-root base)))
        #t                                            ; too small to be a trie: nothing to test
        (let loop ((i 0) (al base-al))
          (if (= i steps)
              (let ((m (jolt-persistent! t)))
                (and (same-entries? m al) (order-ok? m al) (map-shape-ok? m)
                     ;; the source is untouched
                     (equal? (map car base-snapshot) (map car (entries base)))
                     (same-entries? base base-al)))
              (let ((k (rand-key)))
                (if (< (rnd 10) 7)
                    (let ((v (rand-val))) (jolt-assoc! t k v) (loop (+ i 1) (model-assoc al k v)))
                    (begin (jolt-dissoc! t k) (loop (+ i 1) (model-dissoc! al k))))))))))

(let loop ((r 0))
  (when (< r 40)
    (let ((s seed))
      (unless (run-transient (+ 20 (rnd 400)))
        (ok (format "transient model run (seed ~a)" s) #f)))
    (loop (+ r 1))))
(ok "transient model runs" #t)

;; --- sets ride the same trie -------------------------------------------------------------
(is "set conj/disj over collisions"
    "(let [ks (for [a [\"Aa\" \"BB\"] b [\"Aa\" \"BB\"] c [\"Aa\" \"BB\"]] (str a b c))
           s (into #{} (concat ks (range 100)))]
       [(count s) (count (reduce disj s ks)) (every? s ks) (contains? s \"x\")])"
    "[108 100 true false]")
(is "set keeps the stored element" "(let [a [1] s (conj (into #{} (range 50)) a)] (identical? a (get s [1])))" "true")
(is "transient set over collisions"
    "(let [ks (for [a [\"Aa\" \"BB\"] b [\"Aa\" \"BB\"] c [\"Aa\" \"BB\"]] (str a b c))]
       (count (persistent! (reduce disj! (reduce conj! (transient (set (range 40))) ks) (take 3 ks)))))"
    "45")

;; --- reduce over a set walks the trie in seq order --------------------------------------
(is "set reduce visits in seq order"
    "(let [s (into #{} (concat (range 200) (for [a [\"Aa\" \"BB\"] b [\"Aa\" \"BB\"]] (str a b))))]
       (= (reduce conj [] s) (vec (seq s))))"
    "true")
(is "set reduce stops at reduced"
    "(reduce (fn [a x] (if (= a 7) (reduced [:done a]) (inc a))) 0 (set (range 100)))"
    "[:done 7]")
(is "set reduce with a non-fn step" "(reduce (fn [a x] (+ a x)) 0 (set (range 100)))" "4950")
(is "small set reduce" "(reduce + 0 #{1 2 3})" "6")

;; --- what the trie answers beyond get ---------------------------------------------------
(is "find answers the stored key" "(let [k [1] m (assoc (zipmap (range 20) (range 20)) k :v)] (identical? k (key (find m [1]))))" "true")
(is "reduce-kv stops at reduced" "(reduce-kv (fn [a k v] (if (= a 5) (reduced a) (inc a))) 0 (zipmap (range 100) (range 100)))" "5")
(is "seq/keys/vals agree" "(let [m (zipmap (range 100) (range 100 200))] (= (map key m) (keys m) (map #(- % 100) (vals m))))" "true")
(is "print order is seq order" "(let [m (zipmap (range 12) (range 12))] (= (pr-str m) (str \"{\" (clojure.string/join \", \" (map (fn [[k v]] (str k \" \" v)) m)) \"}\")))" "true")
(is "equal maps built in different orders" "(= (zipmap (range 500) (range 500)) (zipmap (reverse (range 500)) (reverse (range 500))))" "true")
(is "hash agrees across build orders" "(= (hash (zipmap (range 500) (range 500))) (hash (into {} (map vector (reverse (range 500)) (reverse (range 500))))))" "true")
(is "nil and false keys and values"
    "(let [m (assoc (zipmap (range 20) (range 20)) nil false false nil)] [(get m nil :x) (get m false :x) (contains? m nil) (contains? m false) (count (dissoc m nil false))])"
    "[false nil true true 20]")

(printf "hamt: ~a/~a passed\n" (- total fails) total)
(exit (if (zero? fails) 0 1))
