;; run-wp.ss — whole-program param-type fixpoint gate (jolt.passes.types/wp-infer!).
;;
;; run-infer.ss drives the per-form inference; this drives the inter-procedural
;; driver: analyze a multi-def unit, run wp-infer!, and assert that a record type
;; flows across fn boundaries — a callee's param picks up its caller's ctor return
;; type, so a field read off it is marked for the bare-index back-end path.
;;
;;   chez --script host/chez/run-wp.ss
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define analyze            (var-deref "jolt.analyzer" "analyze"))
(define run-inference      (var-deref "jolt.passes.types" "run-inference"))
(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define wp-infer!          (var-deref "jolt.passes.types" "wp-infer!"))
(define param-seeds-for    (var-deref "jolt.passes.types" "param-seeds-for"))
(define reinfer-def        (var-deref "jolt.passes.types" "reinfer-def"))
(define pr-str             (var-deref "clojure.core" "pr-str"))

(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (contains-sub? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))

(define fails 0) (define total 0)
(define (check label actual expected)
  (set! total (+ total 1))
  (unless (equal? actual expected)
    (set! fails (+ fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))

;; Node record shape (left/right untagged), like binary-trees.
(set-record-shapes!
  (jolt-hash-map "user/->Node"
                 (jolt-hash-map (keyword #f "fields") (jolt-vector (keyword #f "left") (keyword #f "right"))
                                (keyword #f "tags")   (jolt-vector jolt-nil jolt-nil)
                                (keyword #f "type")   "user.Node")))
(set-protocol-methods! (jolt-hash-map))

;; a 3-def unit: make-tree returns ->Node, run calls check-tree with a make-tree
;; result, so check-tree's `node` param must be inferred as a Node.
(define mt (anode "(def make-tree (fn [depth] (if (zero? depth) (->Node nil nil) (->Node (make-tree (dec depth)) (make-tree (dec depth))))))"))
(define ct (anode "(def check-tree (fn [node] (:left node)))"))
(define rn (anode "(def run (fn [d] (check-tree (make-tree d))))"))

(wp-infer! (jolt-vector mt ct rn))

;; check-tree's param `node` should be seeded with a struct carrying the Node type
(define seed (param-seeds-for "user/check-tree"))
(check "check-tree has a param seed" (jolt-truthy? seed) #t)
(when (jolt-truthy? seed)
  (check "node seeded as user.Node struct"
         (contains-sub? (pr-str seed) "user.Node") #t))

;; reinfer-def then must mark the (:left node) read site for the bare-index path
(define marked (reinfer-def ct seed))
(check "read site marked :hint :struct" (contains-sub? (pr-str marked) ":hint :struct") #t)

;; a fn used only via value position (escape) must NOT be specialized — unknown
;; callers make a concrete seed unsound.
(define ev (anode "(def use-it (fn [f] (f 1)))"))
(define ec (anode "(def caller (fn [] (use-it check-tree)))"))  ; check-tree escapes
(wp-infer! (jolt-vector mt ct rn ev ec))
(check "escaped fn keeps no param seed" (jolt-truthy? (param-seeds-for "user/check-tree")) #f)

;; a self-recursive fn that recurses on a NILABLE field (an untagged record field
;; is :any, so the child can be nil) must NOT be specialized — the recursion can
;; pass nil, so typing the param as a non-nil record would be unsound.
(define ctr (anode "(def walk (fn [node] (let [l (:left node)] (if (nil? l) 1 (walk l)))))"))
(define rnr (anode "(def run2 (fn [d] (walk (make-tree d))))"))
(wp-infer! (jolt-vector mt ctr rnr))
(check "self-recursive nilable param not specialized"
       (jolt-truthy? (param-seeds-for "user/walk")) #f)

;; a self-recursive fn that recurses passing the SAME record type (make-tree always
;; returns a Node) is still safe to specialize — the recursion preserves the type.
(define mtt (anode "(def grow (fn [n acc] (if (zero? n) acc (grow (dec n) (->Node acc acc)))))"))
(define gcl (anode "(def gcaller (fn [] (grow 5 (->Node nil nil))))"))
(wp-infer! (jolt-vector mtt gcl))
(check "self-recursive same-type param keeps its seed"
       (jolt-truthy? (param-seeds-for "user/grow")) #t)

(if (= fails 0)
    (begin (printf "wp gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "wp gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
