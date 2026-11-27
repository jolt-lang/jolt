;; Return-type claims vs the runtime (jolt-3h7.2). Loads test/chez/rettype-check.clj
;; and requires every hand-written return-type claim the compiler trusts to hold
;; on representative values (see that file), then proves the check is not vacuous
;; by feeding it claims known to be false. Run:
;;   chez --script test/chez/rettype-test.ss

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define (load-forms path ns)
  (let* ((src (call-with-input-file path get-string-all)) (n (string-length src)))
    (let loop ((i 0))
      (let skip ((i i))
        (cond ((>= i n) (void))
              ((char-whitespace? (string-ref src i)) (skip (+ i 1)))
              ((char=? (string-ref src i) #\;)
               (let eol ((j i)) (if (or (>= j n) (char=? (string-ref src j) #\newline)) (skip j) (eol (+ j 1)))))
              (else
               (let-values (((f j) (rdr-read-form src i n)))
                 (jolt-compile-eval (substring src i j) ns)
                 (loop j))))))))
(load-forms "test/chez/rettype-check.clj" "rettype-check")
(define (ev s) (jolt-compile-eval s "rettype-check"))

(let ((lines (ev "(vec (summary (all-problems)))")))
  (do ((i 0 (+ i 1))) ((= i (jolt-count lines)))
    (printf "  claim violated: ~a\n" (jolt-nth lines i)))
  (ok "every registry/lattice return-type claim holds on the representatives"
      (= 0 (jolt-count lines))))

;; the check is not vacuous: claims known to be false are caught
(ok "a false :num-result? claim is caught (str)"
    (< 0 (ev "(count (spec-problems \"str\" str {:num-result? true}))")))
(ok "a false :bool? claim is caught (identity)"
    (< 0 (ev "(count (spec-problems \"identity\" identity {:bool? true}))")))
(ok "a false :num-args? claim is caught (str)"
    (< 0 (ev "(count (spec-problems \"str\" str {:num-args? true}))")))
(ok "a false :dbl-contagion? claim is caught (long)"
    (< 0 (ev "(count (spec-problems \"long\" long {:dbl-contagion? true :arity #(= % 1)}))")))
(ok "a false string-return claim is caught (first)"
    (< 0 (ev "(count (set-problems :str-ret string? \"clojure.core\" #{\"first\"}))")))
(ok "a false vector-return claim is caught (list)"
    (< 0 (ev "(count (set-problems :vec-ret vector? \"clojure.core\" #{\"list\"}))")))
;; the entry this gate found wrong: without :arg1-identity?, + claims (+ nil) is a number
(ok "+ without its arg1-identity flag fails its :num-result? claim"
    (< 0 (ev "(count (spec-problems \"+\" + (dissoc (get jolt.op-registry/op-registry \"+\") :arg1-identity?)))")))

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
