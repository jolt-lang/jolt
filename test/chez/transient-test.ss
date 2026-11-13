;; Transient regression: mutable backing + snapshot-on-persist. Run:
;;   chez --script test/chez/transient-test.ss
;; Semantics are covered broadly by the corpus; this pins the invariants the
;; mutable implementation must keep AND that large builds stay linear (a
;; copy-on-write regression would make the 200k builds quadratic and time the
;; gate out).

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (ev s) (jolt-final-str (jolt-compile-eval (string-append "(do " s ")") "user")))
(define (is name s expect) (ok (string-append name " => " expect) (string=? (ev s) expect)))

;; --- mutation is in place; persistent! snapshots back -----------------------
(is "vector build" "(persistent! (reduce conj! (transient []) (range 5)))" "[0 1 2 3 4]")
(is "map build"    "(= {0 0 1 1 2 2} (persistent! (reduce (fn [t i] (assoc! t i i)) (transient {}) (range 3))))" "true")
(is "set build"    "(count (persistent! (reduce conj! (transient #{}) [1 2 2 3])))" "3")
(is "pop!"         "(persistent! (pop! (conj! (transient [1 2]) 3)))" "[1 2]")
(is "dissoc!"      "(persistent! (dissoc! (assoc! (transient {}) :a 1 :b 2) :a))" "{:b 2}")
(is "disj!"        "(persistent! (disj! (conj! (transient #{}) :x :y) :x))" "#{:y}")

;; --- a transient never mutates its source -----------------------------------
(is "source map unchanged"    "(let [m {:a 1} _ (persistent! (assoc! (transient m) :b 2))] (= m {:a 1}))" "true")
(is "source vector unchanged" "(let [v [1 2] _ (persistent! (conj! (transient v) 3))] (= v [1 2]))" "true")

;; --- edges the implementation must keep -------------------------------------
(is "nil key"            "(get (persistent! (assoc! (transient {}) nil :v)) nil)" ":v")
(is "collection key"     "(get (persistent! (assoc! (transient {}) [1 2] :v)) [1 2])" ":v")
(is "dangling key pads"  "(= {:a 1 :b nil} (persistent! (assoc! (transient {}) :a 1 :b)))" "true")
(is "vector? is false"   "(vector? (transient []))" "false")
(ok "transient refuses a sorted map" (guard (e (#t #t)) (ev "(transient (sorted-map :b 2))") #f))
(ok "transient refuses a sorted set" (guard (e (#t #t)) (ev "(transient (sorted-set 1))") #f))
(ok "lone key throws"        (guard (e (#t #t)) (ev "(persistent! (assoc! (transient {}) :a))") #f))
(ok "use after persistent!"  (guard (e (#t #t)) (ev "(let [t (transient [])] (persistent! t) (conj! t 1))") #f))

(is "vector reduction appends a relaxed slice after a based transient's buffer"
    "(let [v (vec (range 1100)) src (subvec v 17 1089) base (subvec v 100 201)
           t (conj! (transient base) :tail) r (reduce conj! t src)]
       (and (identical? t r)
            (= (persistent! (assoc! r 102 :edit))
               (vec (concat base [:tail :edit] (range 18 1089))))
            (= src (vec (range 17 1089)))))"
    "true")
(is "vector reduction accepts an empty source on a retired transient"
    "(let [t (transient [])] (persistent! t) (identical? t (reduce conj! t [])))" "true")
(is "vector reduction rejects a write to a retired transient"
    "(let [t (transient [:a])] (persistent! t)
       (try (reduce conj! t [:b]) false (catch IllegalAccessError _ true)))" "true")
(is "custom reducer on a vector can stop before the last leaf"
    "(persistent! (reduce (fn [t x] (if (= x 3) (reduced t) (conj! t x)))
                          (transient [:a]) (vec (range 64))))" "[:a 0 1 2]")
(is "map reduction keeps prior entries when a later pair is malformed"
    "(let [t (transient {})]
       (try (reduce conj! t [nil [:a false] {:b 2} [:bad] [:late 3]])
            false
            (catch IllegalArgumentException _ (= (persistent! t) {:a false :b 2}))))"
    "true")
(is "map and set reductions consume exactly the relaxed slice"
    "(let [xs (subvec (vec (range 2000)) 17 1093)
           pairs (subvec (mapv (fn [k] [k (inc k)]) (range 2000)) 17 1093)]
       (and (= (set xs) (persistent! (reduce conj! (transient #{}) xs)))
            (= (zipmap xs (map inc xs)) (persistent! (reduce conj! (transient {}) pairs)))))"
    "true")
(is "a hash callback can retire a transient before the next leaf element"
    "(do (deftype RetiringKey [t] clojure.lang.IHashEq (hasheq [_] (persistent! t) 0))
         (let [t (transient #{})]
           (try (reduce conj! t [(RetiringKey. t) :late]) false
                (catch IllegalAccessError _ true))))"
    "true")

;; --- one-way promotion: a transient that grew past the array limit and shrank
;; back comes down a HASH map (JVM TransientArrayMap promotes on the way up and
;; never returns; jolt used to decide lazily from the final count).
(is "promoted stays hash (type)"
    "(type (persistent! (reduce dissoc! (reduce (fn [t i] (assoc! t i i)) (transient {}) (range 20)) (range 17))))"
    "clojure.lang.PersistentHashMap")
(is "promoted stays hash (contents)"
    "(= {17 17 18 18 19 19} (persistent! (reduce dissoc! (reduce (fn [t i] (assoc! t i i)) (transient {}) (range 20)) (range 17))))"
    "true")
;; a transient promotes at CAPACITY regardless of key type (TransientArrayMap:
;; the keyword-to-64 extension is the persistent assoc path only), so 9 keyword
;; keys through (transient {}) come out a hash map on the JVM and here.
(is "9 kw keys promote to hash" "(type (persistent! (reduce (fn [t k] (assoc! t k k)) (transient {}) (map keyword (map (fn [i] (str \"k\" i)) (range 9))))))" "clojure.lang.PersistentHashMap")
(is "8 kw keys stay array" "(keys (persistent! (reduce (fn [t k] (assoc! t k k)) (transient {}) (map keyword (map (fn [i] (str \"k\" i)) (range 8))))))" "(:k0 :k1 :k2 :k3 :k4 :k5 :k6 :k7)")
(is "transient of a large kw array map keeps its capacity" "(let [m (apply array-map (mapcat (fn [i] [(keyword (str \"k\" i)) i]) (range 12)))] (= (keys m) (keys (persistent! (transient m)))))" "true")

;; --- the leaf-sharing trap at DEPTH: a source map big enough to be a real HAMT ---
;; A claimed node's arr is a shallow copy, so the (cons k v) leaves still belong
;; to the source; overwriting a value must cons a fresh pair, never mutate one.
(is "source HAMT unchanged (1000)"
    "(let [m (into {} (map (fn [i] [i i]) (range 1000))) t (transient m)] (assoc! t 500 :new) (persistent! t) (get m 500))"
    "500")
(is "overwritten key in source HAMT (1000)"
    "(let [m (into {} (map (fn [i] [i i]) (range 1000))) t (transient m)] (assoc! t 500 :new) (get (persistent! t) 500))"
    ":new")
(is "source HAMT still equal (1000)"
    "(let [m (into {} (map (fn [i] [i i]) (range 1000))) t (transient m)] (assoc! t 500 :new) (persistent! t) (= m (into {} (map (fn [i] [i i]) (range 1000)))))"
    "true")

;; --- hash collisions through the editable path -------------------------------
;; "Aa" and "BB" share a hasheq, so they land in one collision bucket. The map
;; must be in HASH mode for that bucket to exist at all — with only a handful of
;; entries it stays an array map and the row proves nothing — so pad past the
;; array limit first. Each row re-asserts the collision itself, so if the pair
;; ever stops colliding these fail loudly instead of quietly going vacuous.
(is "collision pair still collides" "(= (hash \"Aa\") (hash \"BB\"))" "true")
(is "collision keys all retrievable"
    "(let [m (persistent! (reduce (fn [t s] (assoc! t s s)) (transient {}) (concat (map (fn [i] (str \"k\" i)) (range 50)) [\"Aa\" \"BB\"])))] (and (= 52 (count m)) (= \"Aa\" (get m \"Aa\")) (= \"BB\" (get m \"BB\")) (= (hash \"Aa\") (hash \"BB\"))))"
    "true")
(is "persistent dissoc collapses bucket"
    "(let [m (dissoc (persistent! (reduce (fn [t s] (assoc! t s s)) (transient {}) (concat (map (fn [i] (str \"k\" i)) (range 50)) [\"Aa\" \"BB\"]))) \"Aa\")] (and (= 51 (count m)) (= \"BB\" (get m \"BB\")) (nil? (get m \"Aa\"))))"
    "true")
(is "dissoc! collapses bucket"
    "(let [t (reduce (fn [t s] (assoc! t s s)) (transient {}) (concat (map (fn [i] (str \"k\" i)) (range 50)) [\"Aa\" \"BB\"])) _ (dissoc! t \"Aa\") m (persistent! t)] (and (= 51 (count m)) (= \"BB\" (get m \"BB\")) (nil? (get m \"Aa\"))))"
    "true")

(is "packed map order and counts survive transient removals and replacements"
    "(let [ks (vec (concat (range 1024) [nil false \"Aa\" \"BB\"]))
           source (zipmap ks ks)
           removed (vec (take-nth 3 ks))
           t (reduce dissoc! (transient source) removed)
           pairs (mapv (fn [k] [k false]) (reverse ks))
           result (persistent! (reduce conj! t pairs))
           expected (reduce conj (apply dissoc source removed) pairs)]
       (and (= source (zipmap ks ks)) (= (count ks) (count result))
            (= (vec expected) (vec result)) (= expected result)))"
    "true")

;; --- linear, not quadratic: 200k builds finish near-instantly ---------------
(is "big vector build"  "(count (persistent! (reduce conj! (transient []) (range 200000))))" "200000")
(is "big map build"     "(count (persistent! (reduce (fn [t i] (assoc! t i i)) (transient {}) (range 200000))))" "200000")
(is "big set build"     "(count (persistent! (reduce conj! (transient #{}) (range 200000))))" "200000")
(is "big map see-through count" "(let [t (reduce (fn [t i] (assoc! t i i)) (transient {}) (range 200000))] [(count t) (get t 199999) (contains? t 0)])" "[200000 199999 true]")
(is "big zipmap"        "(count (zipmap (range 200000) (range 200000)))" "200000")
(is "big array-map source" "(count (persistent! (reduce (fn [t i] (assoc! t i i)) (transient (apply array-map (range 200))) (range 200000))))" "200000")

;; --- a vector built from a flat source is built as a trie, not by conj ------
;; persistent! hands its buffer to make-pvec, which for more than 32 elements
;; conj'd one element at a time — each conj a fresh tail copy, so a 26k-element
;; build allocated 5.5 MB for 210 KB of leaves and took 1 ms. The trie is now
;; assembled bottom-up: leaves of 32 straight from the source, branches over
;; them, the last 1..32 as the tail — the shape every conj would have produced,
;; so nth/conj/pop/subvec/seq read it as any other vector. Bytes are
;; deterministic: per element, the leaf slot (8) plus the branch and tail
;; share, well under 16. The same build serves vec, (apply vector …), mapv and
;; jolt-vector with many arguments.
(define (bytes-per-element n thunk)
  (thunk)
  (let ((b0 (sstats-bytes (statistics))))
    (do ((i 0 (fx+ i 1))) ((fx= i 20)) (thunk))
    (quotient (quotient (- (sstats-bytes (statistics)) b0) 20) n)))
(let* ((n 26000) (src (make-vector n 7))
       (per (bytes-per-element n (lambda () (make-pvec src)))))
  (printf "  make-pvec from a flat ~a-element vector: ~a bytes/element\n" n per)
  (ok "make-pvec builds the trie in bulk (<= 16 bytes per element, was ~210)" (<= per 16)))
(let* ((n 26000)
       ;; compiled once; the source is a vector (reduce walks it with no seq cells),
       ;; so what is measured is conj!'s buffer growth plus persistent!'s trie
       (build (jolt-compile-eval "(let [v (vec (range 26000))] (fn [] (persistent! (reduce conj! (transient []) v))))" "user"))
       (per (bytes-per-element n (lambda () (jolt-invoke0 build)))))
  (printf "  conj! x26k + persistent!: ~a bytes/element\n" per)
  (ok "a large transient vector build stays under 48 bytes per element (buffer growth + trie)" (<= per 48)))
;; the bulk-built trie is the conj-built trie: same reads at every boundary
(let* ((n 1057)   ; 33 full leaves + a 1-element tail: a two-level root
       (src (let ((v (make-vector n))) (do ((i 0 (fx+ i 1))) ((fx= i n)) (vector-set! v i i)) v))
       (bulk (make-pvec src))
       (conjd (let loop ((p empty-pvec) (i 0)) (if (fx= i n) p (loop (pvec-conj p i) (fx+ i 1))))))
  (ok "bulk build has the conj build's count, shift, tail and root shape"
      (and (= (pvec-cnt bulk) (pvec-cnt conjd)) (= (pvec-shift bulk) (pvec-shift conjd))
           (equal? (pvec-tail bulk) (pvec-tail conjd)) (equal? (pvec-root bulk) (pvec-root conjd))))
  (ok "every element reads back, and conj/pop/nth after the bulk build agree with the list"
      (and (let lp ((i 0)) (or (fx= i n) (and (= i (pvec-nth-d bulk i #f)) (lp (fx+ i 1)))))
           (= n (pvec-nth-d (pvec-conj bulk n) n #f))
           (= (fx- n 1) (pvec-cnt (pvec-pop bulk)))
           (= (fx- n 2) (pvec-nth-d (pvec-pop bulk) (fx- n 2) #f)))))
(let* ((n 26000)
       (mv (jolt-compile-eval "(let [v (vec (range 26000))] (fn [] (mapv inc v)))" "user"))
       (per (bytes-per-element n (lambda () (jolt-invoke0 mv)))))
  (printf "  mapv inc over 26k: ~a bytes/element\n" per)
  (ok "mapv over one collection is the transient fold (<= 48 bytes per element, was ~180)" (<= per 48)))
(let ((sizes '(33 64 65 1024 1025 1056 1057 33000)))
  (ok "bulk and conj builds agree at every tail/root boundary"
      (let loop ((ss sizes))
        (or (null? ss)
            (let* ((n (car ss))
                   (src (let ((v (make-vector n))) (do ((i 0 (fx+ i 1))) ((fx= i n)) (vector-set! v i i)) v))
                   (bulk (make-pvec src))
                   (conjd (let lp ((p empty-pvec) (i 0)) (if (fx= i n) p (lp (pvec-conj p i) (fx+ i 1))))))
              (and (= (pvec-shift bulk) (pvec-shift conjd))
                   (equal? (pvec-root bulk) (pvec-root conjd))
                   (equal? (pvec-tail bulk) (pvec-tail conjd))
                   (loop (cdr ss))))))))

;; --- a transient vector shares its source: O(1) creation ---------------------
;; The JVM's TransientVector starts from the source's root and tail, so
;; (into big-v xs) costs the xs. jolt flattened the whole source into a buffer
;; and persistent! rebuilt the trie, so each (into v [x]) was O(count v) and a
;; loop of them quadratic (writ's into-inside-vswap!). A source past one tail
;; chunk is now the transient's base; conj! buffers past it and persistent!
;; appends the buffer a chunk at a time — the shape a conj loop would build.
(let ((bases '(33 64 65 1024 1025 1056 1057 33000)) (adds '(0 1 31 32 33 100 1100)))
  (ok "persistent! of a based transient is the conj-built trie at every boundary"
      (let loop ((bs bases))
        (or (null? bs)
            (let ((b (car bs)))
              (and (let aloop ((as adds))
                     (or (null? as)
                         (let* ((k (car as))
                                (src (let lp ((p empty-pvec) (i 0)) (if (fx= i b) p (lp (pvec-conj p i) (fx+ i 1)))))
                                (want (let lp ((p src) (i 0)) (if (fx= i k) p (lp (pvec-conj p (fx+ b i)) (fx+ i 1)))))
                                (t (jolt-transient-new src))
                                (_ (do ((i 0 (fx+ i 1))) ((fx= i k)) (jolt-conj! t (fx+ b i))))
                                (got (jolt-persistent! t)))
                           (and (= (pvec-cnt got) (pvec-cnt want)) (= (pvec-shift got) (pvec-shift want))
                                (equal? (pvec-root got) (pvec-root want)) (equal? (pvec-tail got) (pvec-tail want))
                                (= b (pvec-cnt src))
                                (aloop (cdr as))))))
                   (loop (cdr bs))))))))
(is "based: reads see base and buffer"
    "(let [t (conj! (transient (vec (range 100))) :x :y)] [(count t) (get t 0) (nth t 99) (nth t 100) (get t 101) (get t 102 :none) (contains? t 101) (t 50)])"
    "[102 0 99 :x :y :none true 50]")
(is "based: assoc! into the base and the buffer"
    "(let [v (vec (range 100)) t (transient v)] (conj! t 100) (assoc! t 3 :a 100 :b 101 :c) [(get t 3) (persistent! t) (= v (vec (range 100)))])"
    (string-append "[:a [0 1 2 :a " (let lp ((i 4) (acc "")) (if (= i 100) acc (lp (+ i 1) (string-append acc (number->string i) " ")))) ":b :c] true]"))
(is "based: pop! through the buffer into the base, then conj!"
    "(let [v (vec (range 40)) t (conj! (transient v) :a :b)] (dotimes [_ 5] (pop! t)) (conj! t :z) [(count t) (nth t 37) (persistent! (pop! (pop! t))) (count v)])"
    "[38 :z [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35] 40]")
(is "based: pop! a base to empty" "(let [t (transient (vec (range 33)))] (dotimes [_ 33] (pop! t)) [(count t) (persistent! (conj! t 1))])" "[0 [1]]")
(is "based: persistent! drops the source's meta" "(nil? (meta (persistent! (conj! (transient (with-meta (vec (range 40)) {:a 1})) 1))))" "true")
(is "based: unchanged base drops meta too" "(nil? (meta (persistent! (transient (with-meta (vec (range 40)) {:a 1})))))" "true")
(is "based: a subvec source comes back a plain vector"
    "(let [r (persistent! (conj! (transient (subvec (vec (range 2000)) 7 1500)) :e))] [(type r) (count r) (first r) (nth r 1492) (peek r) (= r (conj (vec (range 7 1500)) :e))])"
    "[clojure.lang.PersistentVector 1494 7 1499 :e true]")
(is "based: into big from a sequence" "(let [v (into (vec (range 1000)) (range 1000 3000))] [(count v) (nth v 1999) (= v (vec (range 3000)))])" "[3000 1999 true]")
;; (into v small-vector) appends the source's one chunk flat rather than catvec'ing
(is "into small vector across tail boundaries"
    "(every? (fn [[n k]] (= (into (vec (range n)) (vec (range n (+ n k)))) (vec (range (+ n k))))) (for [n [0 1 31 32 33 1055 1056 1057] k [1 2 31 32]] [n k]))"
    "true")
(is "into small vector keeps to's meta" "(meta (into (with-meta (vec (range 40)) {:a 1}) [1 2]))" "{:a 1}")
(is "into small vector onto an RRB vector" "(let [v (into (subvec (vec (range 2000)) 3 1700) [:a :b])] [(count v) (nth v 0) (peek v) (= v (conj (vec (range 3 1700)) :a :b))])" "[1699 3 :b true]")
(is "into a map entry" "(let [v (into (first {:a 1}) [2 3])] [v (map-entry? v) (vector? v)])" "[[:a 1 2 3] false true]")
(let* ((build (jolt-compile-eval "(let [v (vec (range 100000))] (fn [] (into v (list 1))))" "user"))
       (per (bytes-per-element 1 (lambda () (jolt-invoke0 build)))))
  (printf "  (into 100k-vector (list x)): ~a bytes\n" per)
  (ok "into a 100k vector allocates per element added, not per element held (<= 4096 bytes; it was O(count v))" (<= per 4096)))

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
