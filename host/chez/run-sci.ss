;; run-sci.ss — SCI conformance: load borkdude/sci's own source (vendor/sci) through
;; joltc and require its forms to compile+eval. A real-world Clojure-compatibility
;; stress test. Pure Chez, no Janet. Floor-gated like the corpus: a regression below
;; the floor (or the count today, 202/218) fails. Raise the floor as host gaps close
;; (the tail is genuine gaps — set! on vars, some macro/def shapes).
;;
;;   chez --script host/chez/run-sci.ss
;;   JOLT_SCI_FLOOR=N    override the floor (default 202)
;;   SCI_VERBOSE=1       print each failing form's error
(import (chezscheme))

;; Skip cleanly when the submodule isn't checked out.
(unless (file-exists? "vendor/sci/src/sci/core.cljc")
  (display "skip: vendor/sci not checked out (git submodule update --init vendor/sci)\n")
  (exit 0))

(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

;; SCI's .cljc selects host code via #?(:clj ...) with no :jolt branch — read clj.
(set! rdr-features (list "clj" "jolt" "default"))

(define (slurp path)
  (call-with-input-file path
    (lambda (p) (let loop ((cs '()) (c (read-char p)))
      (if (eof-object? c) (list->string (reverse cs)) (loop (cons c cs) (read-char p)))))))

;; Load every form in a file, evaluating each in the current ns (an (ns ...) form
;; switches it). Returns (ok . fail); failures are tolerated (lenient — SCI requires
;; host libs that don't exist here).
(define (load-forms path verbose)
  (let ((src (slurp path)) (ok 0) (fail 0))
    (let ((end (string-length src)))
      (let loop ((i 0))
        (call-with-values (lambda () (rdr-read-form src i end))
          (lambda (form j)
            (unless (rdr-eof? form)
              (guard (e (#t (set! fail (+ fail 1))
                            (when verbose
                              (printf "    FAIL: ~a\n" (call-with-string-output-port
                                (lambda (p) (display-condition (if (condition? e) e
                                  (make-message-condition (jolt-final-str e))) p)))))))
                (jolt-compile-eval-form form (chez-current-ns))
                (set! ok (+ ok 1)))
              (loop j))))))
    (cons ok fail)))

(define verbose (and (getenv "SCI_VERBOSE") #t))

;; stubs first (host shims SCI's source expects)
(for-each (lambda (f) (load-forms (string-append "src/jolt/clojure/sci/" f) verbose))
          '("lang_stubs.clj" "io_stubs.clj" "host_stubs.clj"))

(define sci-base "vendor/sci/src/sci/")
(define load-order
  '("impl/macros.cljc" "impl/protocols.cljc" "impl/types.cljc" "impl/unrestrict.cljc"
    "impl/vars.cljc" "lang.cljc" "impl/utils.cljc" "ctx_store.cljc" "impl/deftype.cljc"
    "impl/records.cljc" "impl/core_protocols.cljc" "impl/hierarchies.cljc"
    "impl/destructure.cljc" "impl/doseq_macro.cljc" "impl/for_macro.cljc" "impl/fns.cljc"
    "impl/multimethods.cljc" "impl/namespaces.cljc" "core.cljc"))

(define total-ok 0) (define total-fail 0)
(for-each
  (lambda (f)
    (let* ((r (load-forms (string-append sci-base f) verbose)) (ok (car r)) (fail (cdr r)))
      (set! total-ok (+ total-ok ok)) (set! total-fail (+ total-fail fail))
      (printf "  ~a: ~a ok, ~a fail\n" f ok fail)))
  load-order)

(printf "\nSCI load: ~a/~a forms ok (~a fail)\n" total-ok (+ total-ok total-fail) total-fail)
(define floor (let ((s (getenv "JOLT_SCI_FLOOR"))) (if s (string->number s) 202)))
(when (< total-ok floor)
  (printf "REGRESSION: ~a forms loaded < floor ~a\n" total-ok floor))
(flush-output-port)
(exit (if (< total-ok floor) 1 0))
