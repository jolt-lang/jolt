;; Array-mode maps: the flat k/v slot representation (PersistentArrayMap).
;;   chez --script test/chez/arraymap-test.ss
;; Semantics are certified by the corpus; this pins the REPRESENTATION each mode
;; carries (a small map is one slot vector, never a trie; its transient is a
;; slot buffer; its seq view is vector-backed) and the promotion thresholds at
;; the representation level, so a regression back to a trie-backed small map
;; fails here even where every value test still passes. It also pins the other
;; mode's seq view: a hash map's is a lazy walk of the trie, a chunk at a time.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (evv s) (jolt-compile-eval (string-append "(do " s ")") "user"))
(define (ev s) (jolt-final-str (evv s)))
(define (is name s expect) (ok (string-append name " => " expect) (string=? (ev s) expect)))
(define (kw n) (keyword #f n))

;; --- representation by mode --------------------------------------------------
(ok "literal map is a slot vector"
    (let ((m (evv "{:a 1 :b 2}")))
      (and (pmap? m) (pmap-array? m) (not (hnode? (pmap-root m)))
           (fx=? 4 (vector-length (pmap-root m))) (fx=? 2 (pmap-cnt m)))))
(ok "{} is the shared empty array map" (let ((m (evv "{}"))) (and (pmap-array? m) (eq? m (evv "{}")))))
(ok "hash-map is a trie at any size" (hnode? (pmap-root (evv "(hash-map :a 1)"))))
(ok "8 entries stay a slot vector" (pmap-array? (evv "(reduce (fn [m i] (assoc m i i)) {} (range 8))")))
(ok "the 9th non-keyword promotes to a trie" (hnode? (pmap-root (evv "(reduce (fn [m i] (assoc m i i)) {} (range 9))"))))
(ok "a 9th keyword rides array mode" (pmap-array? (evv "(assoc (reduce (fn [m i] (assoc m i i)) {} (range 8)) :k 1)")))
(ok "64 keywords stay a slot vector" (pmap-array? (evv "(reduce (fn [m i] (assoc m (keyword (str \"k\" i)) i)) {} (range 64))")))
(ok "the 65th keyword promotes" (hnode? (pmap-root (evv "(reduce (fn [m i] (assoc m (keyword (str \"k\" i)) i)) {} (range 65))"))))
(ok "array-map never promotes" (pmap-array? (evv "(apply array-map (range 40))")))
(ok "a literal past 8 with keyword tail is array mode" (pmap-array? (evv "{\"a\" 0 \"b\" 1 \"c\" 2 \"d\" 3 \"e\" 4 \"f\" 5 \"g\" 6 \"h\" 7 :i 8 :j 9}")))
(ok "a literal past 8 with a non-keyword tail is hash mode" (hnode? (pmap-root (evv "{:a 0 :b 1 :c 2 :d 3 :e 4 :f 5 :g 6 :h 7 \"i\" 8 :j 9}"))))
(ok "zipmap of 8 is array mode, of 9 hash mode"
    (and (pmap-array? (evv "(zipmap (range 8) (range 8))"))
         (hnode? (pmap-root (evv "(zipmap (range 9) (range 9))")))))
(ok "dissoc to empty is a fresh array map, not the {} singleton"
    (let ((m (evv "(dissoc {:a 1} :a)")))
      (and (pmap-array? m) (fx=? 0 (pmap-cnt m)) (not (eq? m (evv "{}"))))))
(ok "dissoc on a trie stays a trie" (hnode? (pmap-root (evv "(dissoc (hash-map :a 1 :b 2) :a)"))))

;; --- identity where the reference returns `this` ------------------------------
(ok "assoc of the held value is the same map (array)"
    (let ((m (evv "{:a 1 :b 2}"))) (eq? m (pmap-assoc m (kw "a") 1))))
(ok "assoc of the held value is the same map (hash)"
    (let ((m (evv "(hash-map :a 1 :b 2)"))) (eq? m (pmap-assoc m (kw "a") 1))))
(ok "assoc of a new value is a new map" (let ((m (evv "{:a 1}"))) (not (eq? m (pmap-assoc m (kw "a") 2)))))
(ok "dissoc of an absent key is the same map"
    (let ((m (evv "{:a 1}")) (h (evv "(hash-map :a 1)")))
      (and (eq? m (pmap-dissoc m (kw "z"))) (eq? h (pmap-dissoc h (kw "z"))))))

;; --- the seq view is vector-backed: O(1) count, no per-element cells ----------
(ok "seq of an array map carries its entries vector"
    (let ((s (jolt-seq (evv "{:a 1 :b 2 :c 3}"))))
      (and (cseq? s) (cseq-cvec s) (fx=? (cseq-kind s) sk-arraymap-seq)
           (fx=? 3 (pvec-count (cseq-cvec s))))))
(ok "seq of a hash map carries its first chunk"
    (let ((s (jolt-seq (evv "(hash-map :a 1 :b 2)"))))
      (and (cseq? s) (cseq-cvec s) (fx=? (cseq-kind s) sk-hashmap-seq))))
(ok "keys and vals are vector-backed"
    (let ((k (jolt-keys (evv "{:a 1 :b 2}"))) (v (jolt-vals (evv "{:a 1 :b 2}"))))
      (and (cseq-cvec k) (fx=? (cseq-kind k) sk-key-seq)
           (cseq-cvec v) (fx=? (cseq-kind v) sk-val-seq))))
(ok "rest of the seq view is still vector-backed"
    (let ((s (jolt-seq (evv "{:a 1 :b 2 :c 3}"))))
      (cseq-cvec (jolt-seq (seq-more s)))))

;; --- a hash map's seq view is LAZY: one chunk per step, not a copy ------------
;; (seq m), (first m), (keys m) and a set's seq used to build every entry before
;; the first was read, so `(when (seq m) …)` was O(n) and a worklist that takes
;; `first` and `disj`s it was quadratic. The view is now a walk of the trie, a
;; 32-leaf chunk at a time (seq.ss hamt-chunk-seq). Allocation is deterministic,
;; so these pin it in bytes rather than in time.
(define (bytes-of thunk)
  (thunk)
  (let ((b0 (sstats-bytes (statistics))))
    (do ((i 0 (fx+ i 1))) ((fx= i 10)) (thunk))
    (quotient (- (sstats-bytes (statistics)) b0) 10)))
(define small-m (evv "(zipmap (range 1000) (range 1000))"))
(define big-m (evv "(zipmap (range 64000) (range 64000))"))
(define small-s (evv "(set (range 1000))"))
(define big-s (evv "(set (range 64000))"))
(ok "seq of a hash map is a ChunkedCons of the map's flavor, not the whole map"
    (let ((s (jolt-seq big-m)))
      (and (cseq? s) (cseq-crest s) (fx=? (cseq-kind s) sk-hashmap-seq)
           (fx<=? (pvec-count (cseq-cvec s)) 32))))
(let ((check (lambda (name f small big)
               (let ((a (bytes-of (lambda () (f small)))) (b (bytes-of (lambda () (f big)))))
                 (printf "  ~a: ~a bytes at 1k, ~a at 64k\n" name a b)
                 ;; one chunk of 32: ~0.8KB of keys, ~3.9KB of map entries
                 ;; (each entry is a pvec). The old fill was ~100 bytes per
                 ;; entry of the WHOLE map: ~6MB at 64k.
                 (ok (string-append name " costs one chunk at any size (<= 6KB, and 64x the map adds <= 1KB)")
                     (and (<= a 6144) (<= b 6144) (<= b (+ a 1024))))))))
  (check "seq of a hash map" jolt-seq small-m big-m)
  (check "first of a hash map" jolt-first small-m big-m)
  (check "keys of a hash map" jolt-keys small-m big-m)
  (check "vals of a hash map" jolt-vals small-m big-m)
  (check "seq of a set" jolt-seq small-s big-s))
;; the worklist: (first s) then (disj s x) until empty. Linear per element means
;; bytes per element hold flat as the set grows four times; quadratic is ~4x.
(let* ((drain (evv "(fn [s] (loop [s s n 0] (if-let [x (first s)] (recur (disj s x) (inc n)) n)))"))
       (per (lambda (n) (let ((s (evv (format "(set (range ~a))" n))))
                          (quotient (bytes-of (lambda () (jolt-invoke drain s))) n))))
       (a (per 4000)) (b (per 16000)))
  (printf "  first/disj worklist: ~a bytes/element at 4k, ~a at 16k\n" a b)
  (ok "a first/disj worklist stays linear (bytes/element ratio <= 1.5, quadratic ~4)" (<= b (* 3/2 a))))
(let ((per (quotient (bytes-of (lambda () (jolt-count (jolt-seq big-m)))) 64000)))
  (printf "  full walk (count (seq m)) over 64k: ~a bytes/entry\n" per)
  ;; ~96 of it is the map entry itself. The eager fill was ~113, all of it paid
  ;; before the first element; walking a chunk at a time must not cost much more.
  (ok "a full walk of the view stays cheap (<= 150 bytes per entry)" (<= per 150)))
;; disj drops an emptied branch, as BitmapIndexedNode.without does: draining
;; most of a set leaves no empty node for the next seq to step over.
(ok "draining a set leaves no empty branch in its trie"
    (let* ((s (evv "(reduce disj (set (range 5000)) (range 4990))"))
           (persistent-t (evv "(persistent! (reduce disj! (transient (set (range 5000))) (range 4990)))")))
      (define (no-empty? nd)
        (let ((arr (hnode-arr nd)))
          (let loop ((i 0))
            (or (fx=? i (vector-length arr))
                (let ((c (vector-ref arr i)))
                  (and (or (not (hnode? c)) (and (not (fx=? 0 (vector-length (hnode-arr c)))) (no-empty? c)))
                       (loop (fx+ i 1))))))))
      (and (no-empty? (pmap-root (pset-m s))) (no-empty? (pmap-root (pset-m persistent-t)))
           (fx=? 10 (pset-count s)) (fx=? 10 (pset-count persistent-t)))))
;; the order is exactly the one the old vector fill produced: the reverse of
;; pmap-fold's visit, collision buckets included. corpus rows depend on it.
(ok "the lazy view reads in pmap-fold order (entries, keys, vals, sets, collisions)"
    (let ((same? (lambda (m)
                   (let ((ks (pmap-fold m (lambda (k v a) (cons k a)) '()))
                         (vs (pmap-fold m (lambda (k v a) (cons v a)) '())))
                     (and (equal? ks (seq->list (jolt-keys m)))
                          (equal? vs (seq->list (jolt-vals m)))
                          (equal? ks (map (lambda (e) (jolt-nth e 0 #f)) (seq->list (jolt-seq m)))))))))
      (and (same? small-m) (same? big-m)
           (same? (evv "(reduce dissoc (zipmap (range 3000) (range 3000)) (range 0 3000 3))"))
           (same? (evv "(into (zipmap (range 40) (range 40)) (map vector (for [a [\"Aa\" \"BB\"] b [\"Aa\" \"BB\"] c [\"Aa\" \"BB\"]] (str a b c)) (range)))"))
           (equal? (pset-fold big-s cons '()) (seq->list (jolt-seq big-s))))))
(is "the lazy view counts, destructures and compares like the old one"
    "(let [m (zipmap (range 100) (range 100)) [[k v] & more] (seq m)] [(count (seq m)) (count more) (= (seq m) (seq m)) (= (vec (seq m)) (vec (map (fn [k] [k (m k)]) (keys m))))])"
    "[100 99 true true]")

;; --- transients: a slot buffer with the reference's capacity rule -------------
(ok "transient of an array map is a 16-slot buffer"
    (let ((t (evv "(transient {:a 1})")))
      (and (jolt-transient? t) (tmap-array? t)
           (fx=? 16 (vector-length (jolt-transient-buf t))) (fx=? 1 (jolt-transient-n t)))))
(ok "transient of a 30-keyword array map keeps its 60 slots"
    (let ((t (evv "(transient (apply array-map (mapcat (fn [i] [(keyword (str \"k\" i)) i]) (range 30))))")))
      (fx=? 60 (vector-length (jolt-transient-buf t)))))
(ok "transient of a hash map is an editable trie" (not (tmap-array? (evv "(transient (hash-map :a 1))"))))
(ok "persistent! hands back a slot vector" (pmap-array? (evv "(persistent! (assoc! (transient {:a 1}) :b 2))")))
(ok "a promoted transient persists as a trie" (hnode? (pmap-root (evv "(persistent! (reduce (fn [t i] (assoc! t i i)) (transient {}) (range 9)))"))))
(ok "the source map is untouched by the transient"
    (let ((m (evv "{:a 1}")))
      (let ((t (jolt-transient-new m)))
        (tmap-put! t (kw "a") 2)
        (tmap-put! t (kw "b") 3)
        (and (fx=? 1 (pmap-cnt m)) (eqv? 1 (pmap-get m (kw "a") #f))))))

;; --- value-level spot checks the representation must keep --------------------
(is "lookup by kind" "[(get {:a 1} 'a) (get {'a 1} :a) (get {\"a\" 1} 'a) (get {1 :i} 1.0) (get {1 :i} 1) (get {nil :n} nil)]" "[nil nil nil nil :i :n]")
(is "replace keeps position, append goes last" "(keys (assoc (assoc {:a 1 :b 2} :a 9) :c 3))" "(:a :b :c)")
(ok "vec-append2 copies and appends across empty, small and 24-key slot vectors"
    (for-all
      (lambda (nk)
        (let* ((v (list->vector (iota (* 2 nk))))
               (orig (vector-copy v))
               (out (vec-append2 v 'k 'x))
               (want (list->vector (append (iota (* 2 nk)) (list 'k 'x)))))
          (and (not (eq? out v))
               (equal? v orig)
               (equal? out want))))
      '(0 1 2 3 8 24 63)))
(ok "vec-remove2 drops slots i and i+1 at every position without mutating input"
    (for-all
      (lambda (nk)
        (let* ((n (* 2 nk))
               (v (list->vector (iota n)))
               (orig (vector-copy v)))
          (let loop ((i 0))
            (or (= i n)
                (let* ((out (vec-remove2 v i))
                       (want (list->vector
                               (let lp ((j 0) (acc '()))
                                 (cond ((= j n) (reverse acc))
                                       ((or (= j i) (= j (+ i 1))) (lp (+ j 1) acc))
                                       (else (lp (+ j 1) (cons j acc))))))))
                  (and (not (eq? out v))
                       (equal? v orig)
                       (equal? out want)
                       (loop (+ i 2))))))))
      '(1 2 3 4 5 8 24)))
(ok "vec-remove2 handles odd-length HAMT node arrays (entries + branches)"
    (let* ((v '#(k0 v0 k1 v1 b0))
           (orig (vector-copy v)))
      (and (equal? (vec-remove2 v 0) '#(k1 v1 b0))
           (equal? (vec-remove2 v 2) '#(k0 v0 b0))
           (equal? v orig))))
(is "transient dissoc! swaps the last entry in" "(keys (persistent! (dissoc! (transient {:a 1 :b 2 :c 3}) :a)))" "(:c :b)")
(is "equal across modes" "[(= {:a 1 :b 2} (hash-map :b 2 :a 1)) (= (hash {:a 1 :b 2}) (hash (hash-map :b 2 :a 1)))]" "[true true]")
(is "reduce-kv folds in place" "(reduce-kv (fn [a k v] (if (= k :b) (reduced a) (+ a v))) 0 (array-map :a 1 :b 2 :c 3))" "1")
(is "count of the seq view" "[(count (seq {:a 1 :b 2 :c 3})) (count (rest (seq {:a 1 :b 2 :c 3}))) (count (keys (hash-map :a 1 :b 2)))]" "[3 2 2]")

;; --- a literal with constant keyword keys builds its slots directly ---------
;; The reader refuses a repeated literal key, so a literal whose keys are all
;; constant keywords has nothing for jolt-hash-map's duplicate scan to find:
;; the emitter hands the slots to the array map as one vector (the reference
;; compiler's RT.mapUniqueKeys for the same shape), no rest list, no scan. A
;; 10-key literal with one runtime value measured 134 ns through the scan
;; (26k of them in one parse of standard-clojure-style's own source); the
;; direct build is the allocation alone. Values still evaluate left to right,
;; and a key that is not a constant keyword (a computed key, a string, a
;; number) keeps the checked constructor. Past the keyword array limit (64)
;; the literal is hash mode and jolt-hash-map builds it as before.
(define (has? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))
(define (emitf str)
  (let-values (((f j) (rdr-read-form str 0 (string-length str))))
    (let ((ctx (make-analyze-ctx "user")))
      (jolt-ce-emit (jolt-ce-run-passes (jolt-ce-analyze ctx f) ctx)))))
(define (kw-literal-direct? src)
  (let ((e (emitf src)))
    (and (has? e "(amap-slots->pmap (vector ") (not (has? e "(jolt-hash-map ")))))
(define (kw-literal-checked? src)
  (let ((e (emitf src)))
    (and (or (has? e "(jolt-hash-map ") (has? e "(jolt-hash-map-checked "))
         (not (has? e "amap-slots->pmap")))))
(ok "keyword-keyed literal with runtime values emits the direct slot build"
    (kw-literal-direct? "(fn [x y] {:a x :b y :c 3})"))
(ok "a computed key keeps the checked constructor"
    (kw-literal-checked? "(fn [k x] {k x :b 2})"))
;; keys that could collide at run time get the duplicate check; distinct
;; constants, which the reader already checked, and a lone key do not
(ok "a computed key gets the duplicate-checking constructor"
    (has? (emitf "(fn [k x] {k x :b 2})") "(jolt-hash-map-checked "))
(ok "distinct constant keys skip the duplicate check"
    (not (has? (emitf "(fn [x] {\"a\" x :b 2})") "-checked")))
(ok "a single computed key skips the duplicate check"
    (not (has? (emitf "(fn [k x] [{k x} #{k}])") "-checked")))
(ok "a set with computed elements gets the duplicate check"
    (has? (emitf "(fn [x y] #{x y})") "(jolt-hash-set-checked "))
(ok "a non-keyword constant key keeps the checked constructor"
    (kw-literal-checked? "(fn [x] {\"a\" x :b 2})"))
(ok "past the keyword array limit the literal is hash mode via the checked constructor"
    (let ((src (string-append "(fn [x] {"
                              (apply string-append
                                     (map (lambda (i) (string-append ":k" (number->string i) " " (if (= i 0) "x" (number->string i)) " "))
                                          (iota 65)))
                              "})")))
      (and (kw-literal-checked? src)
           (hnode? (pmap-root (jolt-invoke1 (evv src) 0))))))
(ok "the direct build is an array map with the literal's order and count"
    (let ((m (jolt-invoke2 (evv "(fn [x y] {:a x :b y :c 3})") 1 2)))
      (and (pmap? m) (pmap-array? m) (fx=? 3 (pmap-cnt m))
           (equal? (vector (kw "a") 1 (kw "b") 2 (kw "c") 3) (pmap-root m)))))
(is "values of a direct build evaluate left to right"
    "(let [log (atom [])] (let [m {:a (do (swap! log conj 1) 1) :b (do (swap! log conj 2) 2) :c (do (swap! log conj 3) 3)}] [@log (:b m)]))"
    "[[1 2 3] 2]")
(is "a direct build past 8 keys stays array mode and answers lookups"
    "(let [m {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i (+ 4 5) :j 10}] [(count m) (:i m) (:j m) (keys m)])"
    "[10 9 10 (:a :b :c :d :e :f :g :h :i :j)]")

;; --- a keyword-invoke site remembers the slot it hit ---------------------
;; (:k m) on an array map is amap-index's identity scan: 6.4 ns at slot 0 and
;; 10.8 at slot 9 of ten. Record-shaped maps — every map one literal builds —
;; put a key at the same slot every time, so a per-site cell holding the last
;; hit index answers the next call with one eq? and one vector-ref (measured
;; 2.0 ns), the shape of a monomorphic inline cache without the hidden class:
;; the cache only shortcuts a HIT, and a stale index (a different map at the
;; site, a key at another slot) falls back to the scan, which re-primes it. A
;; hash-mode map, a record, a nil receiver and a default all keep their old
;; path. Only a site inside a def gets a cell (a bare top-level form has no
;; constant pool to hold it).
(ok "a keyword-invoke site inside a def emits the site lookup over a hoisted cell"
    (let ((e (emitf "(defn kw-site-f [m] (:a m))")))
      (and (has? e "(jolt-kw-get-site ") (has? e "(jolt-kw-site)"))))
(ok "a keyword-invoke with a default keeps the default arity"
    (let ((e (emitf "(defn kw-site-g [m] (:a m 0))")))
      (has? e "(jolt-kw-get-site ")))
(is "site lookup: hit, miss, default, nil receiver"
    "(let [f (fn [m] (:c m)) g (fn [m] (:c m :none))] [(f {:a 1 :b 2 :c 3}) (f {:a 1}) (g {:a 1}) (f nil) (g nil)])"
    "[3 nil :none nil :none]")
(is "the same site over maps whose key sits at different slots stays right"
    "(let [f (fn [m] (:k m))] (mapv f [{:k 1} {:a 0 :k 2} {:a 0 :b 0 :c 0 :k 3} {:k 4} (hash-map :k 5) {:z 9}]))"
    "[1 2 3 4 5 nil]")
(is "a record receiver, a vector receiver and a set receiver keep their answers"
    "(do (defrecord KwSite [c]) (let [f (fn [m] (:c m))] [(f (->KwSite 7)) (f [1 2 3]) (f #{:c}) (f (transient {:c 8}))]))"
    "[7 nil :c 8]")
(is "hash-mode maps answer through the trie"
    "(let [f (fn [m] (:k40 m)) m (into {} (map (fn [i] [(keyword (str \"k\" i)) i]) (range 70)))] [(f m) (f (assoc m :k40 :x))])"
    "[40 :x]")
;; the point of the cell, as a ratio inside one process: a monomorphic site on
;; the tenth slot against the plain scan at that slot.
;;
;; Timed on this thread's clock in NANOSECONDS, best of seven alternating
;; samples per arm, and a miss measured once more before it counts. It was a
;; millisecond clock over best of three, and the arms run 5-25 ms on CI, so one
;; tick was 4-20% of the cached arm: the ratio read 0.22-0.59 across 62 x86
;; runs of the gate (bionic 0.41-0.46) against a 0.6 ceiling. With no site
;; cache the cached arm IS the scan plus the site's check, so the regression
;; reads ~1.0 or more; 0.75 sits between.
(define kw-site-ceiling 0.75)
(ok "a primed site answers in well under the scan's time"
    (let* ((ks (map (lambda (i) (keyword #f (string-append "k" (number->string i)))) (iota 10)))
           (m (amap-slots->pmap (list->vector (apply append (map (lambda (k i) (list k i)) ks (iota 10))))))
           (k9 (list-ref ks 9))
           (site (jolt-kw-site))
           (n 2000000)
           (thread-ns (lambda () (let ((t (current-time 'time-thread))) (+ (* 1000000000 (time-second t)) (time-nanosecond t)))))
           (time-it (lambda (thunk) (let ((t0 (thread-ns))) (do ((i 0 (fx+ i 1))) ((fx= i n)) (thunk)) (- (thread-ns) t0))))
           ;; interference (a collection, a loaded machine running the rest of
           ;; the gate) only ever makes an arm slower, so each arm's fastest
           ;; sample, the two arms alternating so a slow stretch lands on both
           (best (lambda (a b) (let loop ((i 0) (ba #f) (bb #f))
                                 (if (fx= i 7)
                                     (cons ba bb)
                                     (let* ((x (time-it a)) (y (time-it b)))
                                       (loop (fx+ i 1) (if ba (min ba x) x) (if bb (min bb y) y)))))))
           (measure (lambda () (best (lambda () (jolt-get m k9)) (lambda () (jolt-kw-get-site m k9 site)))))
           (ratio (lambda (p) (/ (cdr p) (max 1 (car p)))))
           (both (let ((p (measure)))
                   (if (< (ratio p) kw-site-ceiling)
                       p
                       (let ((q (measure)))
                         (printf "  kw site: ~a — over the ceiling, measuring again\n" (/ (round (* 100.0 (ratio p))) 100.0))
                         (cons (min (car p) (car q)) (min (cdr p) (cdr q)))))))
           (scan (car both))
           (cached (cdr both)))
      (printf "  kw site: scan ~a ms, cached ~a ms (ratio ~a, ceiling ~a)\n"
              (/ (round (/ scan 100000.0)) 10.0) (/ (round (/ cached 100000.0)) 10.0)
              (/ (round (* 100.0 (ratio both))) 100.0) kw-site-ceiling)
      (< (ratio both) kw-site-ceiling)))

;; --- set difference over shared hash tries ------------------------------------
(is "aligned subtraction keeps the left sequence, count, hash and metadata"
    "(let [s (with-meta (set (range 128)) {:tag :left})
           r (reduce disj s [13 35 57])
           expected (reduce disj s r)
           d (clojure.set/difference s r)]
       [(= (vec d) (vec expected)) (= (hash d) (hash expected))
        (count d) (meta d) (= d (jolt.host/try-set-difference s r disj))
        (count (clojure.set/difference s s))])"
    "[true true 3 {:tag :left} true 0]")
(is "a shared root cannot suppress non-reflexive or nested comparisons"
    "(let [base (set (range 128))
           s (into base [##NaN [##NaN]])
           r (disj s 13)]
       [(nil? (jolt.host/try-set-difference s s disj))
        (nil? (jolt.host/try-set-difference s r disj))
        (= (pr-str (reduce disj s r)) (pr-str (clojure.set/difference s r)))])"
    "[true true true]")
(is "aligned subtraction preserves order across a branch and a collision bucket"
    "(let [slot? #(zero? (bit-and 31 (hash %)))
           ks (range 1 128)
           common (conj (set (remove slot? ks)) 0)
           other (first (filter slot? ks))
           branch (conj common other)
           bucket (conj common (char 0))
           mixed (conj bucket other)]
       (every? true?
         (for [[l r] [[branch bucket] [bucket branch] [mixed branch] [mixed bucket]]
               :let [d (clojure.set/difference l r)]]
           (and (= (vec d) (vec (reduce disj l r)))
                (let [trial (jolt.host/try-set-difference l r disj)]
                  (if (< (count l) (count r)) (nil? trial) (= d trial)))))))"
    "true")
(is "public difference preserves both disj dispatch paths"
    "(let [s (set (range 128))
           bigger (conj s :extra)
           first-key (first s)
           seen (atom [])
           replacement (fn [acc key] (swap! seen conj [:new key]) acc)
           change (fn [acc key]
                    (swap! seen conj [:first key])
                    (when (= key first-key)
                      (alter-var-root #'disj (constantly replacement)))
                    acc)
           run (fn [right]
                 (reset! seen [])
                 (let [result (with-redefs [disj change] (clojure.set/difference s right))]
                   [(identical? s result) @seen]))]
       (and (= [true (mapv (fn [key] [:first key]) s)] (run s))
            (= [true (into [[:first first-key]] (map (fn [key] [:new key]) (rest s)))]
               (run bigger))
            (empty? (clojure.set/difference s s))))"
    "true")
(is "public difference honors thread-bound disj and native reduce and contains dispatch"
    "(let [s (set (range 128))
           bigger (conj s :extra)
           v #'disj
           seen (atom [])
           keep (fn [acc key] (swap! seen conj key) acc)
           poison (fn [& args] (throw (ex-info \"unexpected Var call\" {})))]
       (.setDynamic v)
       (try
         (and (with-bindings {v keep}
                (every? true?
                  (for [right [s bigger]]
                    (do (reset! seen [])
                        (and (identical? s (clojure.set/difference s right))
                             (= (vec s) @seen))))))
              (with-redefs [reduce poison contains? poison]
                (and (empty? (clojure.set/difference s s))
                     (empty? (clojure.set/difference s bigger)))))
         (finally (.setDynamic v false))))"
    "true")
;; --- multi-pair assoc (2..4 pairs) single-allocation fast path ---------------
(define (assoc-fold3 m kvs)
  (let loop ((acc m) (rest kvs))
    (if (null? rest)
        acc
        (loop (jolt-assoc3 acc (car rest) (cadr rest)) (cddr rest)))))

(define (assoc-apply-n m kvs)
  (case (length kvs)
    ((4) (jolt-assoc m (list-ref kvs 0) (list-ref kvs 1)
                       (list-ref kvs 2) (list-ref kvs 3)))
    ((6) (jolt-assoc m (list-ref kvs 0) (list-ref kvs 1)
                       (list-ref kvs 2) (list-ref kvs 3)
                       (list-ref kvs 4) (list-ref kvs 5)))
    ((8) (jolt-assoc m (list-ref kvs 0) (list-ref kvs 1)
                       (list-ref kvs 2) (list-ref kvs 3)
                       (list-ref kvs 4) (list-ref kvs 5)
                       (list-ref kvs 6) (list-ref kvs 7)))
    (else (apply jolt-assoc m kvs))))

(define (amap-same-result? a b)
  (and (pmap? a) (pmap? b)
       (eq? (pmap-array? a) (pmap-array? b))
       (fx=? (pmap-cnt a) (pmap-cnt b))
       (jolt=2 a b)
       (jolt=2 (jolt-meta a) (jolt-meta b))
       (or (not (pmap-array? a))
           (let ((ra (pmap-root a)) (rb (pmap-root b)))
             (and (fx=? (vector-length ra) (vector-length rb))
                  (let lp ((i 0))
                    (or (fx=? i (vector-length ra))
                        (and (jolt=2 (vector-ref ra i) (vector-ref rb i))
                             (lp (fx+ i 1))))))))))

;; Randomized differential test against left fold of jolt-assoc3 across
;; sizes around 8 and 64, mixed stable/non-stable keys, duplicates, and meta.
(let ()
  (define rng 2463534242)
  (define (next-u32)
    (let* ((x (bitwise-and #xffffffff (bitwise-xor rng (bitwise-arithmetic-shift-left rng 13))))
           (x (bitwise-and #xffffffff (bitwise-xor x (bitwise-arithmetic-shift-right x 17))))
           (x (bitwise-and #xffffffff (bitwise-xor x (bitwise-arithmetic-shift-left x 5)))))
      (set! rng x)
      x))
  (define (r-int n) (modulo (next-u32) n))
  (define kw-pool (list->vector (map (lambda (i) (keyword #f (string-append "dk" (number->string i)))) (iota 72))))
  (define fx-pool (list->vector (iota 24)))
  (define ch-pool (vector #\a #\b #\c #\d #\e #\f #\g #\h))
  (define nonstable-pool (vector "s0" "s1" "s2" (jolt-symbol #f "sym0") (jolt-vector 1 2) jolt-nil))
  (define vals (vector 0 1 2 3 4 #f #t jolt-nil "v"))
  (define (pick-key mode)
    (case mode
      ((kw) (vector-ref kw-pool (r-int 72)))
      ((stable) (case (r-int 6)
                  ((0 1 2 3) (vector-ref kw-pool (r-int 72)))
                  ((4) (vector-ref fx-pool (r-int 24)))
                  (else (vector-ref ch-pool (r-int 8)))))
      (else (case (r-int 8)
              ((0 1 2 3) (vector-ref kw-pool (r-int 72)))
              ((4) (vector-ref fx-pool (r-int 24)))
              ((5) (vector-ref ch-pool (r-int 8)))
              (else (vector-ref nonstable-pool (r-int (vector-length nonstable-pool))))))))
  (define (make-base sz kw-only?)
    (let lp ((i 0) (m empty-pmap))
      (if (fx=? i sz)
          (if (fx=? (r-int 3) 0)
              (jolt-with-meta m (make-pmap (vector (kw "tag") sz) 1))
              m)
          (let ((k (if kw-only?
                       (vector-ref kw-pool i)
                       (if (fx<? i 16) (vector-ref fx-pool i) (vector-ref kw-pool i)))))
            (lp (fx+ i 1) (jolt-assoc3 m k (vector-ref vals (r-int (vector-length vals)))))))))
  (define (pick-existing-or-new m mode)
    (if (and (pmap-array? m) (fx>? (pmap-cnt m) 0) (fx<? (r-int 10) 5))
        (let* ((r (pmap-root m))
               (slot (fx* 2 (r-int (pmap-cnt m)))))
          (vector-ref r slot))
        (pick-key mode)))
  (define sizes '(0 1 2 3 6 7 8 9 12 60 61 62 63 64 65))
  (define diff-ok #t)
  (define noop-ok #t)
  (let loop-trial ((t 0))
    (when (and (< t 1500) diff-ok noop-ok)
      (let* ((sz (list-ref sizes (r-int (length sizes))))
             (kw-only? (or (> sz 8) (fx=? (r-int 2) 0)))
             (m (make-base sz kw-only?))
             (pairs (+ 2 (r-int 3)))     ; 2, 3, or 4 pairs
             (key-mode (case (r-int 5) ((0 1) 'kw) ((2 3) 'stable) (else 'mixed)))
             (kvs (let mk ((j 0) (ks '()) (acc '()))
                    (if (fx=? j pairs)
                        (reverse acc)
                        (let* ((k (if (and (pair? ks) (fx=? (r-int 5) 0))
                                      (list-ref ks (r-int (length ks))) ; force batch duplicate
                                      (pick-existing-or-new m key-mode)))
                               (v (vector-ref vals (r-int (vector-length vals)))))
                          (mk (fx+ j 1) (cons k ks) (cons v (cons k acc)))))))
             (got (assoc-apply-n m kvs))
             (want (assoc-fold3 m kvs)))
        (unless (amap-same-result? got want)
          (printf "  diff mismatch trial ~a: sz=~a pairs=~a got=~a want=~a\n"
                  t sz pairs (jolt-pr-str got) (jolt-pr-str want))
          (set! diff-ok #f))
        ;; Also check all-no-op identity when m is an array map with stable keys
        (when (and (pmap-array? m) (fx>? (pmap-cnt m) 0))
          (let* ((r (pmap-root m))
                 (noop-kvs
                   (let mk ((j 0) (acc '()))
                     (if (fx=? j pairs)
                         (reverse acc)
                         (let* ((slot (fx* 2 (r-int (pmap-cnt m))))
                                (k (vector-ref r slot))
                                (v (vector-ref r (fx+ slot 1))))
                           (mk (fx+ j 1) (cons v (cons k acc)))))))
                 (res (assoc-apply-n m noop-kvs)))
            (unless (eq? res m)
              (printf "  no-op identity failed trial ~a: sz=~a pairs=~a\n" t sz pairs)
              (set! noop-ok #f)))))
      (loop-trial (+ t 1))))
  (ok "randomized differential multi-pair assoc (1500 trials, 2..4 pairs) matches left fold of jolt-assoc3" diff-ok)
  (ok "randomized all-no-op multi-pair assoc returns identical? receiver" noop-ok))

;; Exhaustive small matrix: bases of 0-9 (kw plain, kw with-meta, fixnum/nil)
;; and 60-64 (kw plain, kw with-meta), every 2-, 3-, and 4-pair key tuple from
;; a 7-key pool (existing keys, new keywords, and a new fixnum), compared with
;; a left fold of jolt-assoc1 (+ meta-carry) on contents, slot order,
;; array/hash mode, and metadata.
(let ()
  (define kw-base-keys (list->vector (map (lambda (i) (keyword #f (string-append "ek" (number->string i)))) (iota 64))))
  (define nx0 (kw "nx0")) (define nx1 (kw "nx1")) (define nx2 (kw "nx2"))
  (define nx3 (kw "nx3")) (define nx4 (kw "nx4")) (define nx5 (kw "nx5")) (define nx6 (kw "nx6"))
  (define (mk-base sz kw? with-meta?)
    (let ((m (let lp ((i 0) (acc empty-pmap))
               (if (fx=? i sz)
                   acc
                   (lp (fx+ i 1) (jolt-assoc1 acc (if kw? (vector-ref kw-base-keys i) i) i))))))
      (if with-meta?
          (jolt-with-meta m (make-pmap (vector (kw "sz") sz) 1))
          m)))
  (define (pool-for sz kw?)
    (let ((bk (lambda (i) (if kw? (vector-ref kw-base-keys i) i))))
      (if (fx<=? sz 9)
          ;; Up to 3 existing keys + 3 new keywords + 1 new fixnum (4 distinct new stable keys)
          (vector (if (fx>=? sz 1) (bk 0) nx4)
                  (if (fx>=? sz 2) (bk 1) nx5)
                  (if (fx>=? sz 3) (bk (fx- sz 1)) nx6)
                  nx0 nx1 nx2 999)
          ;; Sizes 60..64: 2 existing keys + 4 new keywords (tests 60+4=64 and 61+4=65) + 1 new fixnum
          (vector (bk 0) (bk (fx- sz 1)) nx0 nx1 nx2 nx3 999))))
  (define cases 0)
  (define matrix-ok #t)
  (define (fail-case label m ks got want)
    (when matrix-ok
      (printf "  exhaustive mismatch (~a, cnt=~a, ks=~a): got=~a want=~a\n"
              label (if (jolt-nil? m) 'nil (pmap-cnt m)) ks (jolt-pr-str got) (jolt-pr-str want))
      (set! matrix-ok #f)))
  (define (run-base label m pool)
    (let ((plen (vector-length pool)))
      ;; 2-pair (7^2 = 49)
      (do ((a 0 (fx+ a 1))) ((or (fx=? a plen) (not matrix-ok)))
        (let ((k1 (vector-ref pool a))
              (w1 (jolt-assoc1 m (vector-ref pool a) 101)))
          (do ((b 0 (fx+ b 1))) ((or (fx=? b plen) (not matrix-ok)))
            (let* ((k2 (vector-ref pool b))
                   (got (jolt-assoc m k1 101 k2 102))
                   (want (meta-carry m (jolt-assoc1 w1 k2 102))))
              (set! cases (fx+ cases 1))
              (unless (amap-same-result? got want)
                (fail-case label m (list k1 k2) got want))))))
      ;; 3-pair (7^3 = 343)
      (do ((a 0 (fx+ a 1))) ((or (fx=? a plen) (not matrix-ok)))
        (let* ((k1 (vector-ref pool a))
               (w1 (jolt-assoc1 m k1 101)))
          (do ((b 0 (fx+ b 1))) ((or (fx=? b plen) (not matrix-ok)))
            (let* ((k2 (vector-ref pool b))
                   (w2 (jolt-assoc1 w1 k2 102)))
              (do ((c 0 (fx+ c 1))) ((or (fx=? c plen) (not matrix-ok)))
                (let* ((k3 (vector-ref pool c))
                       (got (jolt-assoc m k1 101 k2 102 k3 103))
                       (want (meta-carry m (jolt-assoc1 w2 k3 103))))
                  (set! cases (fx+ cases 1))
                  (unless (amap-same-result? got want)
                    (fail-case label m (list k1 k2 k3) got want))))))))
      ;; 4-pair (7^4 = 2401)
      (do ((a 0 (fx+ a 1))) ((or (fx=? a plen) (not matrix-ok)))
        (let* ((k1 (vector-ref pool a))
               (w1 (jolt-assoc1 m k1 101)))
          (do ((b 0 (fx+ b 1))) ((or (fx=? b plen) (not matrix-ok)))
            (let* ((k2 (vector-ref pool b))
                   (w2 (jolt-assoc1 w1 k2 102)))
              (do ((c 0 (fx+ c 1))) ((or (fx=? c plen) (not matrix-ok)))
                (let* ((k3 (vector-ref pool c))
                       (w3 (jolt-assoc1 w2 k3 103)))
                  (do ((d 0 (fx+ d 1))) ((or (fx=? d plen) (not matrix-ok)))
                    (let* ((k4 (vector-ref pool d))
                           (got (jolt-assoc m k1 101 k2 102 k3 103 k4 104))
                           (want (meta-carry m (jolt-assoc1 w3 k4 104))))
                      (set! cases (fx+ cases 1))
                      (unless (amap-same-result? got want)
                        (fail-case label m (list k1 k2 k3 k4) got want))))))))))))
  ;; nil base
  (run-base "nil" jolt-nil (pool-for 0 #t))
  ;; 0..9 and 60..64 keyword bases (plain and with-meta)
  (for-each
    (lambda (sz)
      (let ((p (pool-for sz #t)))
        (run-base (format "kw-~a" sz) (mk-base sz #t #f) p)
        (run-base (format "kw-~a-meta" sz) (mk-base sz #t #t) p)))
    '(0 1 2 3 4 5 6 7 8 9 60 61 62 63 64))
  ;; 1..9 fixnum bases (tests 8-entry non-kw boundary and hash-mode at 9)
  (for-each
    (lambda (sz)
      (run-base (format "fx-~a" sz) (mk-base sz #f #f) (pool-for sz #f)))
    '(1 2 3 4 5 6 7 8 9))
  (printf "  exhaustive multi-pair assoc matrix: ~a cases checked\n" cases)
  (ok "exhaustive small matrix (bases 0-9, 60-64, nil; all 2/3/4-pair 7-key tuples) matches jolt-assoc1 fold"
      matrix-ok))

;; Directed promotion-order tests: 7-entry ("x", :k) vs (:k, "x") and stable (99, :k) vs (:k, 99)
(let ((m7 (evv "(reduce (fn [m i] (assoc m i i)) {} (range 7))")))
  (ok "7-entry + (\"x\", :k) stays array mode; + (:k, \"x\") promotes to hash mode"
      (let ((a (jolt-assoc m7 "x" 1 (kw "k") 2))
            (b (jolt-assoc m7 (kw "k") 2 "x" 1)))
        (and (pmap-array? a) (not (pmap-array? b))
             (amap-same-result? a (assoc-fold3 m7 (list "x" 1 (kw "k") 2)))
             (amap-same-result? b (assoc-fold3 m7 (list (kw "k") 2 "x" 1))))))
  (ok "7-entry + stable (99, :k) stays array mode; + (:k, 99) promotes to hash mode (2/3/4 pairs)"
      (let ((a2 (jolt-assoc m7 99 1 (kw "k") 2))
            (b2 (jolt-assoc m7 (kw "k") 2 99 1))
            (a3 (jolt-assoc m7 99 1 (kw "k1") 2 (kw "k2") 3))
            (b3 (jolt-assoc m7 (kw "k1") 2 99 1 (kw "k2") 3))
            (a4 (jolt-assoc m7 99 1 (kw "k1") 2 (kw "k2") 3 (kw "k3") 4))
            (b4 (jolt-assoc m7 (kw "k1") 2 (kw "k2") 3 99 1 (kw "k3") 4)))
        (and (pmap-array? a2) (not (pmap-array? b2))
             (pmap-array? a3) (not (pmap-array? b3))
             (pmap-array? a4) (not (pmap-array? b4))
             (amap-same-result? a2 (assoc-fold3 m7 (list 99 1 (kw "k") 2)))
             (amap-same-result? b2 (assoc-fold3 m7 (list (kw "k") 2 99 1)))
             (amap-same-result? a3 (assoc-fold3 m7 (list 99 1 (kw "k1") 2 (kw "k2") 3)))
             (amap-same-result? b3 (assoc-fold3 m7 (list (kw "k1") 2 99 1 (kw "k2") 3)))
             (amap-same-result? a4 (assoc-fold3 m7 (list 99 1 (kw "k1") 2 (kw "k2") 3 (kw "k3") 4)))
             (amap-same-result? b4 (assoc-fold3 m7 (list (kw "k1") 2 (kw "k2") 3 99 1 (kw "k3") 4)))))))

;; Directed 64-keyword promotion boundary (2/3/4 pairs)
(let ((m62 (evv "(reduce (fn [m i] (assoc m (keyword (str \"k\" i)) i)) {} (range 62))"))
      (m63 (evv "(reduce (fn [m i] (assoc m (keyword (str \"k\" i)) i)) {} (range 63))"))
      (m64 (evv "(reduce (fn [m i] (assoc m (keyword (str \"k\" i)) i)) {} (range 64))")))
  (ok "64-keyword boundary: <=64 stays array mode, 65th promotes, existing-only at 64 stays array mode"
      (and (pmap-array? (jolt-assoc m62 (kw "a") 1 (kw "b") 2))
           (not (pmap-array? (jolt-assoc m62 (kw "a") 1 (kw "b") 2 (kw "c") 3)))
           (not (pmap-array? (jolt-assoc m63 (kw "a") 1 (kw "b") 2)))
           (pmap-array? (jolt-assoc m63 (kw "k0") 99 (kw "a") 1))
           (pmap-array? (jolt-assoc m64 (kw "k0") 99 (kw "k1") 88))
           (pmap-array? (jolt-assoc m64 (kw "k0") 99 (kw "k1") 88 (kw "k2") 77 (kw "k3") 66))
           (not (pmap-array? (jolt-assoc m64 (kw "k0") 99 (kw "new") 1))))))

;; Directed receiver & key-type coverage
(is "multi-pair assoc on nil (distinct, duplicate, non-stable)"
    "(pr-str [(assoc nil :a 1 :b 2)
              (assoc nil :a 1 :a 2)
              (assoc nil :a 1 :b 2 :a 3)
              (assoc nil :a 1 :b 2 :c 3 :d 4)
              (keys (assoc nil :a 1 :b 2 :a 3 :c 4))
              (assoc nil \"x\" 1 :b 2)
              (assoc nil [1 2] 1 :b 2 :c 3)])"
    "[{:a 1, :b 2} {:a 2} {:a 3, :b 2} {:a 1, :b 2, :c 3, :d 4} (:a :b :c) {\"x\" 1, :b 2} {[1 2] 1, :b 2, :c 3}]")
(ok "multi-pair assoc on nil returns array-mode pmap"
    (and (pmap-array? (evv "(assoc nil :a 1 :b 2)"))
         (pmap-array? (evv "(assoc nil :a 1 :b 2 :c 3)"))
         (pmap-array? (evv "(assoc nil :a 1 :b 2 :c 3 :d 4)"))))
(ok "multi-pair assoc on hash-mode map preserves hash mode and metadata"
    (let* ((hm (evv "(with-meta (hash-map :a 1 :b 2) {:m 1})"))
           (r2 (jolt-assoc hm (kw "a") 10 (kw "c") 30))
           (r3 (jolt-assoc hm (kw "a") 10 (kw "c") 30 (kw "d") 40))
           (r4 (jolt-assoc hm (kw "a") 10 (kw "c") 30 (kw "d") 40 (kw "e") 50)))
      (and (not (pmap-array? r2)) (not (pmap-array? r3)) (not (pmap-array? r4))
           (jolt=2 (jolt-meta r2) (jolt-meta hm))
           (jolt=2 (jolt-meta r3) (jolt-meta hm))
           (jolt=2 (jolt-meta r4) (jolt-meta hm))
           (jolt=2 r2 (assoc-fold3 hm (list (kw "a") 10 (kw "c") 30)))
           (jolt=2 r3 (assoc-fold3 hm (list (kw "a") 10 (kw "c") 30 (kw "d") 40)))
           (jolt=2 r4 (assoc-fold3 hm (list (kw "a") 10 (kw "c") 30 (kw "d") 40 (kw "e") 50))))))
(is "multi-pair assoc on vector receiver (replace, append, meta)"
    "(let [v (with-meta [10 20 30] {:tag :v})
           r2 (assoc v 0 1 2 3)
           r3 (assoc v 0 1 3 40 4 50)
           r4 (assoc v 0 1 1 2 2 3 3 4)]
       [r2 (meta r2) r3 (meta r3) r4 (meta r4)])"
    "[[1 20 3] {:tag :v} [1 20 30 40 50] {:tag :v} [1 2 3 4] {:tag :v}]")
(is "multi-pair assoc on defrecord (declared fields, ext keys, meta)"
    "(do (defrecord AssocRec [a b])
         (let [r (with-meta (->AssocRec 1 2) {:rm 1})
               r2 (assoc r :a 10 :c 30)
               r3 (assoc r :a 10 :b 20 :c 30)
               r4 (assoc r :a 10 :b 20 :c 30 :d 40)]
           [[(:a r2) (:b r2) (:c r2) (meta r2) (instance? AssocRec r2)]
            [(:a r3) (:b r3) (:c r3) (meta r3) (instance? AssocRec r3)]
            [(:a r4) (:b r4) (:c r4) (:d r4) (meta r4) (instance? AssocRec r4)]]))"
    "[[10 2 30 {:rm 1} true] [10 20 30 {:rm 1} true] [10 20 30 40 {:rm 1} true]]")
(is "multi-pair assoc on sorted-map preserves sort order and metadata"
    "(let [sm (with-meta (sorted-map :b 2 :a 1) {:sm 1})
           r2 (assoc sm :d 4 :c 3)
           r3 (assoc sm :e 5 :c 3 :a 10)
           r4 (assoc sm :f 6 :d 4 :c 3 :e 5)]
       [(keys r2) (meta r2) (keys r3) (:a r3) (meta r3) (keys r4) (meta r4)])"
    "[(:a :b :c :d) {:sm 1} (:a :b :c :e) 10 {:sm 1} (:a :b :c :d :e :f) {:sm 1}]")
(is "multi-pair assoc on custom Associative deftype dispatches assoc in order"
    "(do (deftype CustomAssoc [m log]
           clojure.lang.Associative
           (containsKey [_ k] (contains? m k))
           (entryAt [_ k] (find m k))
           (assoc [_ k v]
             (swap! log conj [k v])
             (CustomAssoc. (assoc m k v) log))
           clojure.lang.ILookup
           (valAt [_ k] (get m k))
           (valAt [_ k d] (get m k d)))
         (let [log (atom [])
               c (CustomAssoc. {:a 0} log)
               r2 (assoc c :a 1 :b 2)
               r3 (assoc r2 :c 3 :d 4 :e 5)
               r4 (assoc r3 :f 6 :g 7 :h 8 :i 9)]
           [(:a r4) (:i r4) @log]))"
    "[1 9 [[:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6] [:g 7] [:h 8] [:i 9]]]")
(is "multi-pair assoc with non-stable keys (string, vector) falls back cleanly"
    "(pr-str (let [m (with-meta {:a 1} {:k 1})
                   r2 (assoc m \"x\" 2 :b 3)
                   r3 (assoc m [1 2] 2 :b 3 :c 4)
                   r4 (assoc m :b 2 \"y\" 3 [3 4] 4 :c 5)]
               [r2 (meta r2) r3 (meta r3) r4 (meta r4)]))"
    "[{:a 1, \"x\" 2, :b 3} {:k 1} {:a 1, [1 2] 2, :b 3, :c 4} {:k 1} {:a 1, :b 2, \"y\" 3, [3 4] 4, :c 5} {:k 1}]")
(is "multi-pair assoc via value position (swap!, update, 1-arg, 5-pair, odd-arg error)"
    "(let [a (atom {:a 1 :b 2})
           _ (swap! a assoc :a 10 :c 30)
           _ (swap! a assoc :b 20 :d 40 :e 50)
           _ (swap! a assoc :a 11 :b 22 :c 33 :f 60)
           _ (swap! a assoc :g 70)
           u (update {:sub {:x 1}} :sub assoc :x 2 :y 3)
           m5 (assoc {} :a 1 :b 2 :c 3 :d 4 :e 5)
           odd-err (try (assoc {} :a 1 :b) (catch IllegalArgumentException _ :iae))]
       [@a u m5 odd-err])"
    "[{:a 11, :b 22, :c 33, :d 40, :e 50, :f 60, :g 70} {:sub {:x 2, :y 3}} {:a 1, :b 2, :c 3, :d 4, :e 5} :iae]")

;; Reachability & allocation checks: compiled direct multi-pair assoc reaches
;; the case-lambda fixed clauses and allocates a single output slot vector.
(let* ((m24 (evv "(reduce (fn [m i] (assoc m (keyword (str \"k\" i)) i)) {} (range 24))"))
       (f2 (evv "(fn [m] (assoc m :ka 1 :kb 2))"))
       (f3 (evv "(fn [m] (assoc m :ka 1 :kb 2 :kc 3))"))
       (f4 (evv "(fn [m] (assoc m :ka 1 :kb 2 :kc 3 :kd 4))"))
       (ka (kw "ka")) (kb (kw "kb")) (kc (kw "kc")) (kd (kw "kd"))
       (k0 (kw "k0")) (k1 (kw "k1")) (k2 (kw "k2")) (k3 (kw "k3"))
       (b2-clj  (bytes-of (lambda () (jolt-invoke1 f2 m24))))
       (b3-clj  (bytes-of (lambda () (jolt-invoke1 f3 m24))))
       (b4-clj  (bytes-of (lambda () (jolt-invoke1 f4 m24))))
       (b2-fast (bytes-of (lambda () (jolt-assoc m24 ka 1 kb 2))))
       (b2-fold (bytes-of (lambda () (jolt-assoc3 (jolt-assoc3 m24 ka 1) kb 2))))
       (b3-fast (bytes-of (lambda () (jolt-assoc m24 ka 1 kb 2 kc 3))))
       (b3-fold (bytes-of (lambda () (jolt-assoc3 (jolt-assoc3 (jolt-assoc3 m24 ka 1) kb 2) kc 3))))
       (b4-fast (bytes-of (lambda () (jolt-assoc m24 ka 1 kb 2 kc 3 kd 4))))
       (b4-fold (bytes-of (lambda () (jolt-assoc3 (jolt-assoc3 (jolt-assoc3 (jolt-assoc3 m24 ka 1) kb 2) kc 3) kd 4))))
       (b-base  (bytes-of (lambda () m24)))
       (b2-noop (bytes-of (lambda () (jolt-assoc m24 k0 0 k1 1))))
       (b3-noop (bytes-of (lambda () (jolt-assoc m24 k0 0 k1 1 k2 2))))
       (b4-noop (bytes-of (lambda () (jolt-assoc m24 k0 0 k1 1 k2 2 k3 3)))))
  (printf "  compiled multi-pair assoc bytes on 24-kw map: 2p ~a vs ~a, 3p ~a vs ~a, 4p ~a vs ~a\n"
          b2-clj b2-fold b3-clj b3-fold b4-clj b4-fold)
  (printf "  direct multi-pair assoc bytes on 24-kw map: 2p ~a vs ~a, 3p ~a vs ~a, 4p ~a vs ~a; no-op ~a/~a/~a (base ~a)\n"
          b2-fast b2-fold b3-fast b3-fold b4-fast b4-fold b2-noop b3-noop b4-noop b-base)
  (ok "compiled (fn [m] (assoc m :ka 1 :kb 2)) reaches single-allocation fast path"
      (< b2-clj (- b2-fold 256)))
  (ok "compiled 3- and 4-pair assoc reach single-allocation fast path"
      (and (< (* 2 b3-clj) b3-fold)
           (< (* 2 b4-clj) b4-fold)))
  (ok "2-pair assoc allocates a single output vector (strictly less than 2x jolt-assoc3)"
      (< b2-fast (- b2-fold 256)))
  (ok "3-pair assoc allocates a single output vector (under half of 3x jolt-assoc3)"
      (< (* 2 b3-fast) b3-fold))
  (ok "4-pair assoc allocates a single output vector (under half of 4x jolt-assoc3)"
      (< (* 2 b4-fast) b4-fold))
  (ok "all-no-op 2/3/4-pair assoc allocates 0 extra bytes"
      (and (= b2-noop b-base) (= b3-noop b-base) (= b4-noop b-base))))

;; --- multi-arg conj (2 and 3 elements) single-copy fast path on vectors ------
(define (pvec-same-result? a b)
  (and (pvec? a) (pvec? b)
       (fx=? (pvec-cnt a) (pvec-cnt b))
       (fx=? (pvec-shift a) (pvec-shift b))
       (eq? (pvec-ent a) (pvec-ent b))
       (eq? (rrbnode? (pvec-root a)) (rrbnode? (pvec-root b)))
       (fx=? (vector-length (pvec-tail a)) (vector-length (pvec-tail b)))
       (jolt=2 a b)
       (jolt=2 (jolt-meta a) (jolt-meta b))))

;; Exhaustive small matrix for multi-arg conj (2 and 3 items) vs left fold of
;; jolt-conj2 across plain vectors (0..35, 60..66, 1053..1058), with-meta
;; vectors, subvectors (1..33), RRB vectors (via pvec-catvec, tail lengths 0..32),
;; and MapEntry.
(let ()
  (define (mk-vec n)
    (let lp ((i 0) (v (jolt-vector)))
      (if (fx=? i n) v (lp (fx+ i 1) (pvec-conj v i)))))
  (define cases 0)
  (define conj-matrix-ok #t)
  (define (check-vec label v)
    (let ((g2 (jolt-conj v 'x 'y))
          (w2 (jolt-conj2 (jolt-conj2 v 'x) 'y))
          (g3 (jolt-conj v 'x 'y 'z))
          (w3 (jolt-conj2 (jolt-conj2 (jolt-conj2 v 'x) 'y) 'z)))
      (set! cases (fx+ cases 2))
      (unless (and (pvec-same-result? g2 w2) (pvec-same-result? g3 w3))
        (when conj-matrix-ok
          (printf "  conj matrix mismatch (~a, cnt=~a, tl=~a, ent=~a)\n"
                  label (pvec-cnt v) (vector-length (pvec-tail v)) (pvec-ent v))
          (set! conj-matrix-ok #f)))))
  ;; Plain and with-meta vectors across tail boundaries 0..35, 60..66, 1053..1058
  (for-each
    (lambda (n)
      (let ((v (mk-vec n)))
        (check-vec (format "plain-~a" n) v)
        (check-vec (format "meta-~a" n) (jolt-with-meta v (make-pmap (vector (kw "n") n) 1)))))
    (append (iota 36) '(60 61 62 63 64 65 66 1053 1054 1055 1056 1057 1058)))
  ;; Subvectors across tail lengths 1..33 (plain and with-meta)
  (let ((big (mk-vec 70)))
    (do ((n 1 (fx+ n 1))) ((fx>? n 33))
      (let ((sv (pvec-as-subvec (pvec-slice big 3 (fx+ 3 n)))))
        (check-vec (format "subvec-~a" n) sv)
        (check-vec (format "subvec-meta-~a" n) (jolt-with-meta sv (make-pmap (vector (kw "sv") n) 1))))))
  ;; RRB vectors across tail lengths 1..32 (built by catvec of non-32-aligned prefix + suffix)
  (let ((left (mk-vec 37)))
    (do ((rlen 0 (fx+ rlen 1))) ((fx>? rlen 33))
      (let ((rv (pvec-catvec left (mk-vec rlen))))
        (check-vec (format "rrb-~a" rlen) rv)
        (check-vec (format "rrb-meta-~a" rlen) (jolt-with-meta rv (make-pmap (vector (kw "rrb") rlen) 1))))))
  ;; MapEntry receiver (must decay to plain vector)
  (check-vec "map-entry" (make-map-entry (kw "k") 42))
  (printf "  exhaustive multi-arg conj matrix: ~a cases checked\n" cases)
  (ok "exhaustive multi-arg conj matrix (plain/meta/subvec/RRB/MapEntry across tail boundaries) matches jolt-conj2 fold"
      conj-matrix-ok))

;; Directed conj receiver tests: MapEntry, subvector, RRB, nil, list, sets, maps, defrecord, queue, custom deftype
(ok "conj on MapEntry yields plain vector (not MapEntry) for 2 and 3 elements"
    (let* ((e (evv "(first {:a 1})"))
           (r2 (jolt-conj e 2 3))
           (r3 (jolt-conj e 2 3 4)))
      (and (eq? (pvec-ent e) #t)
           (eq? (pvec-ent r2) #f) (eq? (pvec-ent r3) #f)
           (jolt=2 r2 (evv "[:a 1 2 3]"))
           (jolt=2 r3 (evv "[:a 1 2 3 4]")))))
(ok "conj on subvector stays a subvector and carries metadata for 2 and 3 elements"
    (let* ((sv (evv "(with-meta (subvec [10 20 30 40 50] 1 4) {:tag :sv})"))
           (r2 (jolt-conj sv 60 70))
           (r3 (jolt-conj sv 60 70 80)))
      (and (eq? (pvec-ent r2) 'subvec) (eq? (pvec-ent r3) 'subvec)
           (jolt=2 r2 (evv "[20 30 40 60 70]"))
           (jolt=2 r3 (evv "[20 30 40 60 70 80]"))
           (jolt=2 (jolt-meta r2) (jolt-meta sv))
           (jolt=2 (jolt-meta r3) (jolt-meta sv)))))
(is "multi-arg conj on nil, list, sets, maps, queue, defrecord, and value-position callers"
    "(pr-str
       (let [l (with-meta '(1 2) {:l 1})
             el (with-meta '() {:el 1})
             s (with-meta #{1 2} {:s 1})
             ss (with-meta (sorted-set 3 1) {:ss 1})
             am (with-meta {:a 1} {:am 1})
             hm (with-meta (hash-map :a 1) {:hm 1})
             sm (with-meta (sorted-map :b 2) {:sm 1})
             q (conj clojure.lang.PersistentQueue/EMPTY 1 2)
             q3 (conj q 3 4 5)
             a (atom [1 2])
             _ (swap! a conj 3 4)
             _ (swap! a conj 5 6 7)
             _ (swap! a conj 8)
             u (update {:v [1]} :v conj 2 3)]
         [(conj)
          (conj nil)
          (conj nil 1 2)
          (conj nil 1 2 3)
          (conj l 3 4) (meta (conj l 3 4))
          (conj l 3 4 5) (meta (conj l 3 4 5))
          (conj el 1 2) (meta (conj el 1 2))
          (= #{1 2 3 4} (conj s 3 4)) (meta (conj s 3 4))
          (= #{1 2 3 4 5} (conj s 3 4 5)) (meta (conj s 3 4 5))
          (seq (conj ss 2 0)) (meta (conj ss 2 0))
          (conj am [:b 2] [:c 3]) (meta (conj am [:b 2] [:c 3]))
          (conj am [:b 2] nil {:c 3}) (meta (conj am [:b 2] nil {:c 3}))
          (= {:a 1 :b 2 :c 3} (conj hm [:b 2] [:c 3])) (meta (conj hm [:b 2] [:c 3]))
          (keys (conj sm [:a 1] [:c 3])) (meta (conj sm [:a 1] [:c 3]))
          (vec q) (vec q3)
          @a u
          (conj [1] 2 3 4 5)]))"
    "[[] nil (2 1) (3 2 1) (4 3 1 2) {:l 1} (5 4 3 1 2) {:l 1} (2 1) {:el 1} true {:s 1} true {:s 1} (0 1 2 3) {:ss 1} {:a 1, :b 2, :c 3} {:am 1} {:a 1, :b 2, :c 3} {:am 1} true {:hm 1} (:a :b :c) {:sm 1} [1 2] [1 2 3 4 5] [1 2 3 4 5 6 7 8] {:v [1 2 3]} [1 2 3 4 5]]")
(is "multi-arg conj on defrecord and custom IPersistentCollection deftype"
    "(do (defrecord ConjRec [a b])
         (deftype CustomConj [xs log]
           clojure.lang.IPersistentCollection
           (cons [_ x]
             (swap! log conj x)
             (CustomConj. (conj xs x) log))
           (count [_] (count xs))
           (empty [_] (CustomConj. [] log))
           (equiv [this o] (identical? this o))
           clojure.lang.Seqable
           (seq [_] (seq xs)))
         (let [r (with-meta (->ConjRec 1 2) {:rm 1})
               r2 (conj r [:c 3] [:d 4])
               r3 (conj r [:c 3] [:d 4] [:e 5])
               log (atom [])
               c (CustomConj. [] log)
               c2 (conj c 10 20)
               c3 (conj c2 30 40 50)]
           [[(:c r2) (:d r2) (meta r2) (instance? ConjRec r2)]
            [(:c r3) (:d r3) (:e r3) (meta r3) (instance? ConjRec r3)]
            (vec (seq c3)) @log]))"
    "[[3 4 {:rm 1} true] [3 4 5 {:rm 1} true] [10 20 30 40 50] [10 20 30 40 50]]")

;; Reachability & allocation checks: compiled direct multi-arg conj reaches the
;; case-lambda fixed clauses and allocates a single output tail + pvec.
(let* ((v20 (evv "(vec (range 20))"))
       (fc2 (evv "(fn [v] (conj v 101 102))"))
       (fc3 (evv "(fn [v] (conj v 101 102 103))"))
       (bc2-clj  (bytes-of (lambda () (jolt-invoke1 fc2 v20))))
       (bc3-clj  (bytes-of (lambda () (jolt-invoke1 fc3 v20))))
       (bc2-fold (bytes-of (lambda () (jolt-conj2 (jolt-conj2 v20 101) 102))))
       (bc3-fold (bytes-of (lambda () (jolt-conj2 (jolt-conj2 (jolt-conj2 v20 101) 102) 103)))))
  (printf "  compiled multi-arg conj bytes on 20-elem vec: 2-arg ~a vs ~a, 3-arg ~a vs ~a\n"
          bc2-clj bc2-fold bc3-clj bc3-fold)
  (ok "compiled (fn [v] (conj v 101 102)) reaches single-copy fast path"
      (< bc2-clj (- bc2-fold 128)))
  (ok "compiled (fn [v] (conj v 101 102 103)) reaches single-copy fast path"
      (< (* 2 bc3-clj) bc3-fold)))

;; --- multi-key dissoc (2..4 keys) single-allocation fast path ----------------
(define (dissoc-fold2 m ks)
  (let loop ((acc m) (rest ks))
    (if (null? rest) acc (loop (jolt-dissoc2 acc (car rest)) (cdr rest)))))

(define (dissoc-apply-n m ks)
  (case (length ks)
    ((2) (jolt-dissoc m (list-ref ks 0) (list-ref ks 1)))
    ((3) (jolt-dissoc m (list-ref ks 0) (list-ref ks 1) (list-ref ks 2)))
    ((4) (jolt-dissoc m (list-ref ks 0) (list-ref ks 1) (list-ref ks 2) (list-ref ks 3)))
    (else (apply jolt-dissoc m ks))))

;; Randomized differential test against left fold of jolt-dissoc2 across
;; array-mode and hash-mode sizes (0..65), mixed stable/non-stable keys,
;; duplicates, absent keys, emptying, and metadata.
(let ()
  (define rng 1357924681)
  (define (next-u32)
    (let* ((x (bitwise-and #xffffffff (bitwise-xor rng (bitwise-arithmetic-shift-left rng 13))))
           (x (bitwise-and #xffffffff (bitwise-xor x (bitwise-arithmetic-shift-right x 17))))
           (x (bitwise-and #xffffffff (bitwise-xor x (bitwise-arithmetic-shift-left x 5)))))
      (set! rng x)
      x))
  (define (r-int n) (modulo (next-u32) n))
  (define kw-pool (list->vector (map (lambda (i) (keyword #f (string-append "rk" (number->string i)))) (iota 72))))
  (define fx-pool (list->vector (iota 24)))
  (define ch-pool (vector #\a #\b #\c #\d #\e #\f #\g #\h))
  (define nonstable-pool (vector "s0" "s1" "s2" (jolt-symbol #f "sym0") (jolt-vector 1 2) jolt-nil))
  (define (pick-key mode)
    (case mode
      ((kw) (vector-ref kw-pool (r-int 72)))
      ((stable) (case (r-int 6)
                  ((0 1 2 3) (vector-ref kw-pool (r-int 72)))
                  ((4) (vector-ref fx-pool (r-int 24)))
                  (else (vector-ref ch-pool (r-int 8)))))
      (else (case (r-int 8)
              ((0 1 2 3) (vector-ref kw-pool (r-int 72)))
              ((4) (vector-ref fx-pool (r-int 24)))
              ((5) (vector-ref ch-pool (r-int 8)))
              (else (vector-ref nonstable-pool (r-int (vector-length nonstable-pool))))))))
  (define (make-base sz kw-only?)
    (let lp ((i 0) (m empty-pmap))
      (if (fx=? i sz)
          (if (fx=? (r-int 3) 0)
              (jolt-with-meta m (make-pmap (vector (kw "dm") sz) 1))
              m)
          (let ((k (if kw-only?
                       (vector-ref kw-pool i)
                       (if (fx<? i 16) (vector-ref fx-pool i) (vector-ref kw-pool i)))))
            (lp (fx+ i 1) (jolt-assoc3 m k i))))))
  (define (pick-existing-or-new m mode)
    (if (and (pmap-array? m) (fx>? (pmap-cnt m) 0) (fx<? (r-int 10) 6))
        (vector-ref (pmap-root m) (fx* 2 (r-int (pmap-cnt m))))
        (pick-key mode)))
  (define sizes '(0 1 2 3 4 5 6 7 8 9 12 24 63 64 65))
  (define diff-ok #t)
  (define noop-ok #t)
  (define fresh-empty-ok #t)
  (let loop-trial ((t 0))
    (when (and (< t 1500) diff-ok noop-ok fresh-empty-ok)
      (let* ((sz (list-ref sizes (r-int (length sizes))))
             (kw-only? (or (> sz 8) (fx=? (r-int 2) 0)))
             (m (make-base sz kw-only?))
             (nkeys (+ 2 (r-int 3)))     ; 2, 3, or 4 keys
             (key-mode (case (r-int 5) ((0 1) 'kw) ((2 3) 'stable) (else 'mixed)))
             (ks (let mk ((j 0) (acc '()))
                   (if (fx=? j nkeys)
                       (reverse acc)
                       (let ((k (if (and (pair? acc) (fx=? (r-int 5) 0))
                                    (list-ref acc (r-int (length acc)))
                                    (pick-existing-or-new m key-mode))))
                         (mk (fx+ j 1) (cons k acc))))))
             (got (dissoc-apply-n m ks))
             (want (dissoc-fold2 m ks)))
        (unless (amap-same-result? got want)
          (printf "  dissoc diff mismatch trial ~a: sz=~a nkeys=~a got=~a want=~a\n"
                  t sz nkeys (jolt-pr-str got) (jolt-pr-str want))
          (set! diff-ok #f))
        (when (and (pmap-array? m) (fx>? (pmap-cnt m) 0) (fx=? (pmap-cnt got) 0) (eq? got empty-pmap))
          (printf "  dissoc emptied returned shared empty-pmap trial ~a\n" t)
          (set! fresh-empty-ok #f))
        (when (and (fx=? (pmap-cnt got) (pmap-cnt m)) (not (eq? got m)))
          (printf "  dissoc all-absent did not return identical? trial ~a\n" t)
          (set! noop-ok #f)))
      (loop-trial (+ t 1))))
  (ok "randomized differential multi-key dissoc (1500 trials, 2..4 keys) matches left fold of jolt-dissoc2" diff-ok)
  (ok "randomized all-absent multi-key dissoc returns identical? receiver" noop-ok)
  (ok "randomized emptying multi-key dissoc returns fresh empty array map" fresh-empty-ok))

;; Exhaustive small matrix for multi-key dissoc (2, 3, and 4 keys): bases of
;; 0..6, 8, 9 (kw plain, kw with-meta, fixnum plain) + nil, every 2-, 3-, and
;; 4-key tuple from a 6-key pool (existing start/mid/end + absent kw/fixnum),
;; checked against a left fold of jolt-dissoc2 on contents, slot order, mode,
;; metadata, identical? on all-absent, and fresh empty on emptying.
(let ()
  (define kw-base-keys (list->vector (map (lambda (i) (keyword #f (string-append "dkm" (number->string i)))) (iota 10))))
  (define ax0 (kw "ax0")) (define ax1 (kw "ax1")) (define ax2 (kw "ax2")) (define ax3 (kw "ax3"))
  (define (mk-base sz kw? with-meta?)
    (let ((m (let lp ((i 0) (acc empty-pmap))
               (if (fx=? i sz)
                   acc
                   (lp (fx+ i 1) (jolt-assoc1 acc (if kw? (vector-ref kw-base-keys i) i) i))))))
      (if with-meta?
          (jolt-with-meta m (make-pmap (vector (kw "dsz") sz) 1))
          m)))
  (define (pool-for sz kw?)
    (let ((bk (lambda (i) (if kw? (vector-ref kw-base-keys i) i))))
      (vector (if (fx>=? sz 1) (bk 0) ax2)
              (if (fx>=? sz 2) (bk 1) ax3)
              (if (fx>=? sz 3) (bk (fxquotient sz 2)) ax0)
              (if (fx>=? sz 4) (bk (fx- sz 1)) ax1)
              ax0 999)))
  (define cases 0)
  (define matrix-ok #t)
  (define (check-one label m got want)
    (set! cases (fx+ cases 1))
    (cond
      ((jolt-nil? m)
       (unless (jolt-nil? got)
         (when matrix-ok (printf "  dissoc matrix nil mismatch (~a)\n" label) (set! matrix-ok #f))))
      (else
       (unless (and (amap-same-result? got want)
                    (or (not (fx=? (pmap-cnt got) (pmap-cnt m))) (eq? got m))
                    (or (not (pmap-array? m)) (fx=? (pmap-cnt m) 0) (not (fx=? (pmap-cnt got) 0))
                        (not (eq? got empty-pmap))))
         (when matrix-ok
           (printf "  dissoc matrix mismatch (~a, cnt=~a): got=~a want=~a\n"
                   label (pmap-cnt m) (jolt-pr-str got) (jolt-pr-str want))
           (set! matrix-ok #f))))))
  (define (run-base label m pool)
    (let ((plen (vector-length pool)))
      (do ((a 0 (fx+ a 1))) ((or (fx=? a plen) (not matrix-ok)))
        (let* ((k1 (vector-ref pool a))
               (w1 (jolt-dissoc2 m k1)))
          (do ((b 0 (fx+ b 1))) ((or (fx=? b plen) (not matrix-ok)))
            (let* ((k2 (vector-ref pool b))
                   (w2 (jolt-dissoc2 w1 k2)))
              (check-one label m (jolt-dissoc m k1 k2) w2)
              (do ((c 0 (fx+ c 1))) ((or (fx=? c plen) (not matrix-ok)))
                (let* ((k3 (vector-ref pool c))
                       (w3 (jolt-dissoc2 w2 k3)))
                  (check-one label m (jolt-dissoc m k1 k2 k3) w3)
                  (do ((d 0 (fx+ d 1))) ((or (fx=? d plen) (not matrix-ok)))
                    (let* ((k4 (vector-ref pool d))
                           (w4 (jolt-dissoc2 w3 k4)))
                      (check-one label m (jolt-dissoc m k1 k2 k3 k4) w4)))))))))))
  (run-base "nil" jolt-nil (pool-for 0 #t))
  (for-each
    (lambda (sz)
      (let ((p (pool-for sz #t)))
        (run-base (format "kw-~a" sz) (mk-base sz #t #f) p)
        (run-base (format "kw-~a-meta" sz) (mk-base sz #t #t) p)))
    '(0 1 2 3 4 5 6 8 9))
  (for-each
    (lambda (sz)
      (run-base (format "fx-~a" sz) (mk-base sz #f #f) (pool-for sz #f)))
    '(1 2 3 4 5 8 9))
  (printf "  exhaustive multi-key dissoc matrix: ~a cases checked\n" cases)
  (ok "exhaustive small matrix (bases 0-6, 8, 9, nil; all 2/3/4-key 6-key tuples) matches jolt-dissoc2 fold"
      matrix-ok))

;; Directed dissoc receiver & key-type coverage
(ok "emptying array map via 2/3/4-key dissoc returns fresh empty pmap carrying metadata"
    (let* ((m2 (evv "(with-meta {:a 1 :b 2} {:tag :e2})"))
           (m3 (evv "(with-meta {:a 1 :b 2 :c 3} {:tag :e3})"))
           (m4 (evv "(with-meta {:a 1 :b 2 :c 3 :d 4} {:tag :e4})"))
           (r2 (jolt-dissoc m2 (kw "a") (kw "b")))
           (r3 (jolt-dissoc m3 (kw "a") (kw "b") (kw "c")))
           (r4 (jolt-dissoc m4 (kw "a") (kw "b") (kw "c") (kw "d"))))
      (and (pmap-array? r2) (fx=? 0 (pmap-cnt r2)) (not (eq? r2 empty-pmap)) (jolt=2 (jolt-meta r2) (jolt-meta m2))
           (pmap-array? r3) (fx=? 0 (pmap-cnt r3)) (not (eq? r3 empty-pmap)) (jolt=2 (jolt-meta r3) (jolt-meta m3))
           (pmap-array? r4) (fx=? 0 (pmap-cnt r4)) (not (eq? r4 empty-pmap)) (jolt=2 (jolt-meta r4) (jolt-meta m4)))))
(ok "multi-key dissoc on hash-mode map preserves hash mode, metadata, and identical? when absent"
    (let* ((hm (evv "(with-meta (hash-map :a 1 :b 2 :c 3 :d 4 :e 5) {:hm 1})"))
           (r2 (jolt-dissoc hm (kw "a") (kw "c")))
           (r3 (jolt-dissoc hm (kw "a") (kw "b") (kw "d")))
           (r4 (jolt-dissoc hm (kw "a") (kw "b") (kw "c") (kw "d")))
           (rn (jolt-dissoc hm (kw "z1") (kw "z2") (kw "z3"))))
      (and (not (pmap-array? r2)) (not (pmap-array? r3)) (not (pmap-array? r4))
           (eq? rn hm)
           (jolt=2 (jolt-meta r2) (jolt-meta hm))
           (jolt=2 (jolt-meta r3) (jolt-meta hm))
           (jolt=2 (jolt-meta r4) (jolt-meta hm))
           (jolt=2 r2 (evv "{:b 2 :d 4 :e 5}"))
           (jolt=2 r3 (evv "{:c 3 :e 5}"))
           (jolt=2 r4 (evv "{:e 5}")))))
(is "multi-key dissoc on nil, non-stable keys, value-position callers, and error cases"
    "(pr-str
       (let [m (with-meta {:a 1 \"x\" 2 [1 2] 3 :b 4 :c 5} {:k 1})
             a (atom {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6})
             _ (swap! a dissoc :a :b)
             _ (swap! a dissoc :c :d :e)
             _ (swap! a dissoc :z)
             u (update {:sub {:x 1 :y 2 :z 3}} :sub dissoc :x :z)
             m5 (dissoc {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6} :a :b :c :d :e)
             err2 (try (dissoc [1 2] 0 1) (catch ClassCastException _ :cce))
             err0 (try (dissoc 42) (catch ClassCastException _ :cce))]
         [(dissoc nil)
          (dissoc nil :a)
          (dissoc nil :a :b)
          (dissoc nil :a :b :c)
          (dissoc nil :a :b :c :d)
          (dissoc nil :a :b :c :d :e)
          (dissoc m \"x\" :b) (meta (dissoc m \"x\" :b))
          (dissoc m [1 2] \"x\" :c) (meta (dissoc m [1 2] \"x\" :c))
          (dissoc m :a \"x\" [1 2] :b) (meta (dissoc m :a \"x\" [1 2] :b))
          (identical? m (dissoc m))
          @a u m5 err2 err0]))"
    "[nil nil nil nil nil nil {:a 1, [1 2] 3, :c 5} {:k 1} {:a 1, :b 4} {:k 1} {:c 5} {:k 1} true {:f 6} {:sub {:y 2}} {:f 6} :cce :cce]")
(is "multi-key dissoc on defrecord (declared fields, extmap, mixed, absent, meta)"
    "(do (defrecord DissocRec [a b c])
         (let [r (with-meta (assoc (->DissocRec 1 2 3) :x 10 :y 20 :z 30) {:rm 1})
               re2 (dissoc r :x :y)
               re3 (dissoc r :x :y :z)
               rd2 (dissoc r :a :x)
               rd3 (dissoc r :a :b :y)
               rd4 (dissoc r :a :b :x :y)
               rabs (dissoc r :no1 :no2)]
           [[(instance? DissocRec re2) (:z re2) (:x re2) (meta re2)]
            [(instance? DissocRec re3) (:a re3) (:x re3) (meta re3)]
            [(instance? DissocRec rd2) rd2 (meta rd2)]
            [(instance? DissocRec rd3) rd3 (meta rd3)]
            [(instance? DissocRec rd4) rd4 (meta rd4)]
            [(instance? DissocRec rabs) (count rabs) (meta rabs)]]))"
    "[[true 30 nil {:rm 1}] [true 1 nil {:rm 1}] [false {:b 2, :c 3, :y 20, :z 30} {:rm 1}] [false {:c 3, :x 10, :z 30} {:rm 1}] [false {:c 3, :z 30} {:rm 1}] [true 6 {:rm 1}]]")
(is "multi-key dissoc on sorted-map preserves sort order and metadata for 0..5 keys"
    "(let [sm (with-meta (sorted-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6) {:sm 1})
           r0 (dissoc sm)
           r1 (dissoc sm :b)
           r2 (dissoc sm :b :e)
           r3 (dissoc sm :a :c :f)
           r4 (dissoc sm :a :b :d :f)
           r5 (dissoc sm :a :b :c :d :e)]
       [(identical? sm r0)
        (keys r1) (meta r1)
        (keys r2) (meta r2)
        (keys r3) (meta r3)
        (keys r4) (meta r4)
        (keys r5) (meta r5)])"
    "[true (:a :c :d :e :f) {:sm 1} (:a :c :d :f) {:sm 1} (:b :d :e) {:sm 1} (:c :e) {:sm 1} (:f) {:sm 1}]")
(is "multi-key dissoc on custom IPersistentMap deftype dispatches without in order"
    "(do (deftype CustomDissoc [m log]
           clojure.lang.IPersistentMap
           (without [_ k]
             (swap! log conj k)
             (CustomDissoc. (dissoc m k) log))
           (assoc [_ k v] (CustomDissoc. (assoc m k v) log))
           (assocEx [_ k v] (CustomDissoc. (assoc m k v) log))
           clojure.lang.ILookup
           (valAt [_ k] (get m k))
           (valAt [_ k d] (get m k d))
           clojure.lang.Counted
           (count [_] (count m))
           clojure.lang.Seqable
           (seq [_] (seq m)))
         (let [log (atom [])
               c (CustomDissoc. {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10} log)
               c2 (dissoc c :a :b)
               c3 (dissoc c2 :c :d :e)
               c4 (dissoc c3 :f :g :h :i)]
           [(count c4) (:j c4) @log]))"
    "[1 10 [:a :b :c :d :e :f :g :h :i]]")

;; Reachability & allocation checks: compiled direct multi-key dissoc bypasses
;; both variadic wrappers (host-table.ss and records-coll.ss) and allocates a
;; single output slot vector (and 0 extra bytes when all keys are absent).
(let* ((m24 (evv "(reduce (fn [m i] (assoc m (keyword (str \"k\" i)) i)) {} (range 24))"))
       (fd2 (evv "(fn [m] (dissoc m :k1 :k12))"))
       (fd3 (evv "(fn [m] (dissoc m :k1 :k10 :k20))"))
       (fd4 (evv "(fn [m] (dissoc m :k1 :k6 :k12 :k18))"))
       (fd2-abs (evv "(fn [m] (dissoc m :abs1 :abs2))"))
       (fd3-abs (evv "(fn [m] (dissoc m :abs1 :abs2 :abs3))"))
       (fd4-abs (evv "(fn [m] (dissoc m :abs1 :abs2 :abs3 :abs4))"))
       (k1 (kw "k1")) (k6 (kw "k6")) (k10 (kw "k10")) (k12 (kw "k12")) (k18 (kw "k18")) (k20 (kw "k20"))
       (bd2-clj  (bytes-of (lambda () (jolt-invoke1 fd2 m24))))
       (bd3-clj  (bytes-of (lambda () (jolt-invoke1 fd3 m24))))
       (bd4-clj  (bytes-of (lambda () (jolt-invoke1 fd4 m24))))
       (bd2-fold (bytes-of (lambda () (jolt-dissoc2 (jolt-dissoc2 m24 k1) k12))))
       (bd3-fold (bytes-of (lambda () (jolt-dissoc2 (jolt-dissoc2 (jolt-dissoc2 m24 k1) k10) k20))))
       (bd4-fold (bytes-of (lambda () (jolt-dissoc2 (jolt-dissoc2 (jolt-dissoc2 (jolt-dissoc2 m24 k1) k6) k12) k18))))
       (bd-base  (bytes-of (lambda () m24)))
       (bd2-abs  (bytes-of (lambda () (jolt-invoke1 fd2-abs m24))))
       (bd3-abs  (bytes-of (lambda () (jolt-invoke1 fd3-abs m24))))
       (bd4-abs  (bytes-of (lambda () (jolt-invoke1 fd4-abs m24)))))
  (printf "  compiled multi-key dissoc bytes on 24-kw map: 2k ~a vs ~a, 3k ~a vs ~a, 4k ~a vs ~a; absent ~a/~a/~a (base ~a)\n"
          bd2-clj bd2-fold bd3-clj bd3-fold bd4-clj bd4-fold bd2-abs bd3-abs bd4-abs bd-base)
  (ok "compiled (fn [m] (dissoc m :k1 :k12)) reaches single-allocation fast path"
      (< bd2-clj (- bd2-fold 256)))
  (ok "compiled 3- and 4-key dissoc reach single-allocation fast path"
      (and (< (* 2 bd3-clj) bd3-fold)
           (< (* 2 bd4-clj) bd4-fold)))
  (ok "compiled all-absent 2/3/4-key dissoc allocates 0 extra bytes"
      (and (= bd2-abs bd-base) (= bd3-abs bd-base) (= bd4-abs bd-base))))

;; --- the clauses of the variadic entries the matrices above do not reach ------
;; 1 element / 1 key arrives here only in value position (swap!, reduce, apply,
;; update); 5+ takes the rest clause. Each is its own clause in jolt-conj,
;; jolt-dissoc and the two jolt-dissoc wrappers, so each needs its own check.
(is "1-element (value position) and 5+ element conj keep the receiver's metadata"
    "(let [v (with-meta [1] {:m 1}) l (with-meta '(1) {:m 1}) s (with-meta #{1} {:m 1}) a (atom v)]
       [(meta (reduce conj v [2 3])) (meta (swap! a conj 2)) (meta (apply conj l [2])) (meta (apply conj s [2]))
        (meta (conj v 2 3 4 5 6)) (meta (conj l 2 3 4 5 6)) (conj v 2 3 4 5 6)])"
    "[{:m 1} {:m 1} {:m 1} {:m 1} {:m 1} {:m 1} [1 2 3 4 5 6]]")
(is "1-key (value position) and 5+ key dissoc keep the receiver's metadata"
    "(let [m (with-meta {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6} {:m 1}) a (atom m)]
       [(meta (swap! a dissoc :a)) (meta (reduce dissoc m [:a :b])) (meta (apply dissoc m [:a]))
        (meta (dissoc m :a :b :c :d :e)) (dissoc m :a :b :c :d :e)])"
    "[{:m 1} {:m 1} {:m 1} {:m 1} {:f 6}]")
;; the keys are built at run time so they are equal to, never eq? to, the map's
(is "a present non-stable key in ANY position of a 2/3/4-key dissoc takes the fallback"
    "(pr-str
       (let [m {:a 1 \"x\" 2 [1 2] 3 :b 4 :c 5} x (str \\x) v (vector 1 2)]
         [(dissoc m :a x) (dissoc m x :a)
          (dissoc m :a x :b) (dissoc m :a :b v) (dissoc m v :a :b)
          (dissoc m :a x :b :c) (dissoc m :a :b v :c) (dissoc m :a :b :c x) (dissoc m x :a :b :c)]))"
    "[{[1 2] 3, :b 4, :c 5} {[1 2] 3, :b 4, :c 5} {[1 2] 3, :c 5} {\"x\" 2, :c 5} {\"x\" 2, :c 5} {[1 2] 3} {\"x\" 2} {[1 2] 3} {[1 2] 3}]")
(is "record and sorted-map receivers through the 1-key and 5+ key clauses"
    "(do (defrecord VR [a b])
         (defrecord NPR [a b] clojure.lang.Counted (count [_] 99))
         (deftype VW [m log]
           clojure.lang.IPersistentMap
           (without [_ k] (swap! log conj k) (VW. (dissoc m k) log))
           clojure.lang.Counted (count [_] (count m)))
         (let [log (atom [])
               a (atom (VW. {:a 1 :b 2} log))
               _ (swap! a dissoc :a)
               r (with-meta (assoc (->VR 1 2) :p 1 :q 2 :r 3 :s 4 :t 5) {:m 1})
               r5 (dissoc r :p :q :r :s :t)
               np (with-meta (assoc (->NPR 1 2) :x 1) {:m 1})
               sm (atom (with-meta (sorted-map :a 1 :b 2 :c 3) {:m 1}))]
           [@log (count @a)
            (= r5 (->VR 1 2)) (instance? VR r5) (meta r5)
            (dissoc np :x :a :zz) (meta (dissoc np :x :a :zz)) (meta (dissoc np :x :zz :yy))
            (keys (swap! sm dissoc :a)) (meta @sm) (keys (apply dissoc @sm [:c]))]))"
    "[[:a] 1 true true {:m 1} {:b 2} {:m 1} {:m 1} (:b :c) {:m 1} (:b)]")

(printf "arraymap-test: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
