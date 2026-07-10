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
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

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
(define (has-sub? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0)) (cond ((> (+ i m) n) #f)
                            ((string=? (substring s i (+ i m)) sub) #t)
                            (else (loop (+ i 1)))))))
(define fails 0) (define total 0)
(define (check label actual expected)
  (set! total (+ total 1))
  (unless (equal? actual expected)
    (set! fails (+ fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))

;; a declared field -> bare-index path, value matches jolt-get
(let ((e (mark-emit "(:y a)")))
  (check "declared field uses jrec-field-at" (has-sub? e "jrec-field-at") #t)
  (check "field 1 -> static slot 1" (has-sub? e " 1 ") #t)
  (check "bare read == jolt-get" (run-emit e) (evals "(:y a)")))   ; 20

;; first/last fields too
(check "field x == jolt-get" (run-emit (mark-emit "(:x a)")) (evals "(:x a)"))   ; 10
(check "field z == jolt-get" (run-emit (mark-emit "(:z a)")) (evals "(:z a)"))   ; 30

;; a key that is NOT a declared field -> no bare path, still correct (nil)
(let ((e (mark-emit "(:w a)")))
  (check "non-field key no jrec-field-at" (has-sub? e "jrec-field-at") #f)
  (check "non-field key == jolt-get" (run-emit e) (evals "(:w a)")))   ; nil

;; a default-arg form keeps jolt-get (the bare path is no-default only)
(let ((e (mark-emit "(:y a 99)")))
  (check "default-arg keeps jolt-get" (has-sub? e "jrec-field-at") #f))

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
  (check "simple ^Node in nsa -> nsa's Node" (tag-of "nsa/->Holder") "nsa/->Node")
  (check "simple ^Node in nsb -> nsb's Node" (tag-of "nsb/->Holder") "nsb/->Node")
  (check "qualified ^nsa.Node from nsb -> nsa's Node" (tag-of "nsb/->HolderQ") "nsa/->Node"))

(if (= fails 0)
    (begin (printf "fieldread gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "fieldread gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
