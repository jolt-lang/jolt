;; run-fieldread.ss — native record field-read gate (backend_scheme emit).
;;
;; When the inference types a keyword-lookup receiver as a record (it carries the
;; field-order :shape + :hint :struct), the back end reads the field by its static
;; slot via jrec-field-at instead of jolt-get. This gate pins the emit shape and
;; that the value matches jolt-get — for a declared field, a non-field key (no
;; bare path), and a default-arg form (no bare path).
;;
;;   chez --script host/chez/run-fieldread.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))
(define emit    (var-deref "jolt.backend-scheme" "emit"))
(define kw      (lambda (n) (keyword #f n)))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))

(evals "(defrecord Vec3 [x y z])")
(evals "(def a (->Vec3 10 20 30))")

;; emit (:KEY a [default]) with arg 0 marked as a Vec3 struct receiver.
(define (mark-emit src)
  (let* ((ir (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
         (a0 (jolt-nth (jolt-get ir (kw "args")) 0))
         (marked (jolt-assoc a0 (kw "hint") (kw "struct")
                             (kw "shape") (jolt-vector (kw "x") (kw "y") (kw "z"))))
         (args (jolt-get ir (kw "args")))
         (args2 (jolt-assoc args 0 marked)))
    (emit (jolt-assoc ir (kw "args") args2))))

(define (run-emit scm) (eval (read (open-input-string scm)) (interaction-environment)))

;; a declared field -> bare-index path, value matches jolt-get
(let ((e (mark-emit "(:y a)")))
  (gate-check "declared field uses direct accessor jrec3-f1" (gate-sub? e "jrec3-f1") #t)
  (gate-check "direct path leaves no jrec-field-at cond" (gate-sub? e "jrec-field-at") #f)
  (gate-check "bare read == jolt-get" (run-emit e) (evals "(:y a)")))   ; 20

;; first/last fields too
(gate-check "field x == jolt-get" (run-emit (mark-emit "(:x a)")) (evals "(:x a)"))   ; 10
(gate-check "field z == jolt-get" (run-emit (mark-emit "(:z a)")) (evals "(:z a)"))   ; 30

;; a key that is NOT a declared field -> no bare path, still correct (nil)
(let ((e (mark-emit "(:w a)")))
  (gate-check "non-field key no jrec-field-at" (gate-sub? e "jrec-field-at") #f)
  (gate-check "non-field key == jolt-get" (run-emit e) (evals "(:w a)")))   ; nil

;; a default-arg form keeps jolt-get (the bare path is no-default only)
(let ((e (mark-emit "(:y a 99)")))
  (gate-check "default-arg keeps jolt-get" (gate-sub? e "jrec-field-at") #f))

;; field-tag resolution across same-named records: two namespaces each define
;; Node; a record's simple ^Node field tag resolves to the SAME-NS Node, and a
;; qualified ^nsa.Node tag resolves to exactly that namespace's Node from
;; anywhere — never last-writer-wins on the simple name.
(register-record-shape! "nsa/->Node" (list (kw "x")) (list jolt-nil) "nsa.Node")
(register-record-shape! "nsb/->Node" (list (kw "x")) (list jolt-nil) "nsb.Node")
(register-record-shape! "nsa/->Holder"  (list (kw "n")) (list "Node")     "nsa.Holder")
(register-record-shape! "nsb/->Holder"  (list (kw "n")) (list "Node")     "nsb.Holder")
(register-record-shape! "nsb/->HolderQ" (list (kw "n")) (list "nsa.Node") "nsb.HolderQ")
(let* ((m (chez-record-shapes-map))
       (tag-of (lambda (ck) (jolt-nth (jolt-get (jolt-get m ck) (kw "tags")) 0))))
  (gate-check "simple ^Node in nsa -> nsa's Node" (tag-of "nsa/->Holder") "nsa/->Node")
  (gate-check "simple ^Node in nsb -> nsb's Node" (tag-of "nsb/->Holder") "nsb/->Node")
  (gate-check "qualified ^nsa.Node from nsb -> nsa's Node" (tag-of "nsb/->HolderQ") "nsa/->Node"))

(gate-summary "fieldread")
