;; run-wp.ss — whole-program param-type fixpoint gate (jolt.passes.types/wp-infer!).
;;
;; run-infer.ss drives the per-form inference; this drives the inter-procedural
;; driver: analyze a multi-def unit, run wp-infer!, and assert that a record type
;; flows across fn boundaries — a callee's param picks up its caller's ctor return
;; type, so a field read off it is marked for the bare-index back-end path.
;;
;;   chez --script host/chez/run-wp.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze            (var-deref "jolt.analyzer" "analyze"))
(define run-inference      (var-deref "jolt.passes.types" "run-inference"))
(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define wp-infer!          (var-deref "jolt.passes.types" "wp-infer!"))
(define param-seeds-for    (var-deref "jolt.passes.types" "param-seeds-for"))
(define reinfer-def        (var-deref "jolt.passes.types" "reinfer-def"))
(define pr-str             (var-deref "clojure.core" "pr-str"))
(define U ((var-deref "jolt.passes.types" "new-unit")))

(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))

;; Node record shape (left/right untagged), like binary-trees.
(set-record-shapes! U
  (jolt-hash-map "user/->Node"
                 (jolt-hash-map (keyword #f "fields") (jolt-vector (keyword #f "left") (keyword #f "right"))
                                (keyword #f "tags")   (jolt-vector jolt-nil jolt-nil)
                                (keyword #f "type")   "user.Node")))
(set-protocol-methods! U (jolt-hash-map))

;; a 3-def unit: make-tree returns ->Node, run calls check-tree with a make-tree
;; result, so check-tree's `node` param must be inferred as a Node.
(define mt (anode "(def make-tree (fn [depth] (if (zero? depth) (->Node nil nil) (->Node (make-tree (dec depth)) (make-tree (dec depth))))))"))
(define ct (anode "(def check-tree (fn [node] (:left node)))"))
(define rn (anode "(def run (fn [d] (check-tree (make-tree d))))"))

(wp-infer! U (jolt-vector mt ct rn))

;; check-tree's param `node` should be seeded with a struct carrying the Node type
(define seed (param-seeds-for U "user/check-tree"))
(gate-check "check-tree has a param seed" (jolt-truthy? seed) #t)
(when (jolt-truthy? seed)
  (gate-check "node seeded as user.Node struct"
         (gate-sub? (pr-str seed) "user.Node") #t))

;; reinfer-def then must mark the (:left node) read site for the bare-index path
(define marked (reinfer-def U ct seed))
(gate-check "read site marked :hint :struct" (gate-sub? (pr-str marked) ":hint :struct") #t)

;; a fn used only via value position (escape) must NOT be specialized — unknown
;; callers make a concrete seed unsound.
(define ev (anode "(def use-it (fn [f] (f 1)))"))
(define ec (anode "(def caller (fn [] (use-it check-tree)))"))  ; check-tree escapes
(wp-infer! U (jolt-vector mt ct rn ev ec))
(gate-check "escaped fn keeps no param seed" (jolt-truthy? (param-seeds-for U "user/check-tree")) #f)

;; a self-recursive fn that recurses on a NILABLE field (an untagged record field
;; is :any, so the child can be nil) must NOT be specialized — the recursion can
;; pass nil, so typing the param as a non-nil record would be unsound.
(define ctr (anode "(def walk (fn [node] (let [l (:left node)] (if (nil? l) 1 (walk l)))))"))
(define rnr (anode "(def run2 (fn [d] (walk (make-tree d))))"))
(wp-infer! U (jolt-vector mt ctr rnr))
(gate-check "self-recursive nilable param not specialized"
       (jolt-truthy? (param-seeds-for U "user/walk")) #f)

;; a self-recursive fn that recurses passing the SAME record type (make-tree always
;; returns a Node) is still safe to specialize — the recursion preserves the type.
(define mtt (anode "(def grow (fn [n acc] (if (zero? n) acc (grow (dec n) (->Node acc acc)))))"))
(define gcl (anode "(def gcaller (fn [] (grow 5 (->Node nil nil))))"))
(wp-infer! U (jolt-vector mtt gcl))
(gate-check "self-recursive same-type param keeps its seed"
       (jolt-truthy? (param-seeds-for U "user/grow")) #t)

;; a recursive fn that threads a param STRAIGHT THROUGH its recursion (same arg at
;; the same position) must keep that param's type — a pass-through self-call adds no
;; information and must not poison the param to :any. This is the ray tracer's
;; hittables, passed unchanged through ray-cast's recursion while its reduce element
;; reads the records' fields.
(define cwalk (anode "(def cwalk (fn [hs] (reduce (fn [acc h] (:left h)) nil hs)))"))
(define crec  (anode "(def crec (fn [hs d] (if (< d 0) nil (do (cwalk hs) (crec hs (- d 1))))))"))
(define cdrv  (anode "(def cdrive (fn [] (crec [(->Node nil nil) (->Node nil nil)] 5)))"))
(wp-infer! U (jolt-vector cwalk crec cdrv))
(gate-check "recursion pass-through param keeps its vec element type"
       (gate-sub? (pr-str (param-seeds-for U "user/crec")) "user.Node") #t)

(gate-summary "wp")
