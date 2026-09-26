;; Array-mode maps: the flat k/v slot representation (PersistentArrayMap).
;;   chez --script test/chez/arraymap-test.ss
;; Semantics are certified by the corpus; this pins the REPRESENTATION each mode
;; carries (a small map is one slot vector, never a trie; its transient is a
;; slot buffer; its seq view is vector-backed) and the promotion thresholds at
;; the representation level, so a regression back to a trie-backed small map
;; fails here even where every value test still passes.

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
(ok "seq of a hash map carries its entries vector"
    (let ((s (jolt-seq (evv "(hash-map :a 1 :b 2)"))))
      (and (cseq? s) (cseq-cvec s) (fx=? (cseq-kind s) sk-hashmap-seq))))
(ok "keys and vals are vector-backed"
    (let ((k (jolt-keys (evv "{:a 1 :b 2}"))) (v (jolt-vals (evv "{:a 1 :b 2}"))))
      (and (cseq-cvec k) (fx=? (cseq-kind k) sk-key-seq)
           (cseq-cvec v) (fx=? (cseq-kind v) sk-val-seq))))
(ok "rest of the seq view is still vector-backed"
    (let ((s (jolt-seq (evv "{:a 1 :b 2 :c 3}"))))
      (cseq-cvec (jolt-seq (seq-more s)))))

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
    (and (has? e "(jolt-hash-map ") (not (has? e "amap-slots->pmap")))))
(ok "keyword-keyed literal with runtime values emits the direct slot build"
    (kw-literal-direct? "(fn [x y] {:a x :b y :c 3})"))
(ok "a computed key keeps the checked constructor"
    (kw-literal-checked? "(fn [k x] {k x :b 2})"))
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
;; the tenth slot against the plain scan at that slot
(ok "a primed site answers in well under the scan's time"
    (let* ((ks (map (lambda (i) (keyword #f (string-append "k" (number->string i)))) (iota 10)))
           (m (amap-slots->pmap (list->vector (apply append (map (lambda (k i) (list k i)) ks (iota 10))))))
           (k9 (list-ref ks 9))
           (site (jolt-kw-site))
           (n 2000000)
           (time-it (lambda (thunk) (let ((t0 (real-time))) (do ((i 0 (fx+ i 1))) ((fx= i n)) (thunk)) (- (real-time) t0))))
           ;; best of three per arm, alternating: interference (a collection, a
           ;; loaded machine running the rest of the gate) only ever makes an arm
           ;; slower, and one bad sample in the cached arm read as a regression
           ;; (29 ms against the scan's 23 under `make test`, 8 against 21 alone)
           (best (lambda (a b) (let loop ((i 0) (ba #f) (bb #f))
                                 (if (fx= i 3)
                                     (cons ba bb)
                                     (let* ((x (time-it a)) (y (time-it b)))
                                       (loop (fx+ i 1) (if ba (min ba x) x) (if bb (min bb y) y)))))))
           (both (best (lambda () (jolt-get m k9)) (lambda () (jolt-kw-get-site m k9 site))))
           (scan (car both))
           (cached (cdr both)))
      (printf "  kw site: scan ~a ms, cached ~a ms (ratio ~a, ceiling 0.6)\n" scan cached (if (> scan 0) (/ (round (* 100.0 (/ cached scan))) 100.0) 'n/a))
      (< cached (* 0.6 scan))))

(printf "arraymap-test: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
