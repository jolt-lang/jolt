;; run-sci.ss — SCI conformance: load borkdude/sci's own source (vendor/sci) through
;; jolt and require its forms to compile+eval. A real-world Clojure-compatibility
;; stress test. Floor-gated like the corpus: a regression below the floor (or the
;; count today, 411/434) fails. Raise the floor as host gaps close.
;;
;; The tail is this gate's own load-order, not a host gap. load-order below is a
;; CURATED SUBSET in a fixed sequence, so a reference across it can land before its
;; target is loaded — SCI's real requires are transitive and cyclic, and jolt has no
;; trouble with them: the scifunctional gate loads the same library through the
;; ordinary dependency path and passes. Of the twelve: seven cannot resolve an
;; unqualified name from a namespace this list omits (four of them copy-core-var,
;; from sci.impl.copy-vars) and five name the namespace outright —
;; sci.impl.interpreter twice, sci.impl.types, sci.impl.cljs, edamame.impl.parser.
;;
;; That second group is why the floor moved 416 -> 412 (jolt-z88). A qualified name
;; whose namespace is not loaded used to compile to a class static, so the form
;; "loaded" and only a call would have found out; the analyzer reports it now, and
;; four forms moved from silently-latent to counted. Nothing about SCI or jolt
;; changed with them — the same four could never have run.
;;
;; 412 -> 399 (jolt-d1ip) is the same move for an ALIAS: `types/foo` in
;; protocols.cljc, `i/foo` and `opts/foo` in core.cljc name an :as from an ns form
;; whose require failed under this order, so the alias was never registered. The
;; compiler's require pre-scan used to register it anyway, before the ns form
;; ran, and the reference compiled to the same dead class static; without the
;; pre-scan an unregistered lowercase alias is "No such namespace" at compile
;; time, as on the JVM, and thirteen such forms are counted.
;;
;; 399/424 -> 417/434 (#1095): sci.impl.cljs and sci.impl.copy-vars joined the
;; list, ahead of namespaces.cljc. `:refer` of a name that does not exist now
;; throws `x does not exist`, as the JVM's refer does, so namespaces.cljc's ns
;; form — which refers copy-core-var and friends — failed outright once they
;; were missing, and every alias it registers went with it. Loading their
;; namespace first is the order SCI's own requires give; the four copy-core-var
;; forms counted above load with it.
;;
;; 417 -> 412 is the same move once more, for a LOADED namespace missing the var:
;; sci.impl.parser's ns form fails here (edamame and tools.reader are not on this
;; gate's roots), so the namespace exists with nothing in it, and core.cljc's
;; `parser/parse-string`, `parser/reader`, `parser/get-line-number`,
;; `parser/get-column-number` and `parser/eof` compiled to class statics that
;; could never run. A form that runs as soon as it compiles reports "No such var"
;; now, as on the JVM, and the five are counted.
;;
;; 412 -> 411 is that alias move again, for a call with NO arguments: protocols.cljc's
;; `(store/get-ctx)` names sci.ctx-store through an :as its failed ns form never
;; registered. A no-arg qualified call took the static-member arm, which read the
;; alias as a class and compiled; it now checks the namespace as every other
;; reference does, and is "No such namespace: store" at compile time, as on the
;; JVM. One form whose only unresolvable reference was such a call is counted.
;;
;;   chez --script host/chez/run-sci.ss
;;   JOLT_SCI_FLOOR=N    override the floor (default 411)
;;   SCI_VERBOSE=1       print each failing form's error
(import (chezscheme))

;; Skip cleanly when the submodule isn't checked out.
(unless (file-exists? "vendor/sci/src/sci/core.cljc")
  (display "skip: vendor/sci not checked out (git submodule update --init vendor/sci)\n")
  (exit 0))

(load "host/chez/run-gate-harness.ss")

;; SCI's .cljc selects host code via #?(:clj ...) with no :jolt branch — read clj.
(set! rdr-features (list "clj" "jolt" "default"))

(define (slurp path)
  (call-with-input-file path
    (lambda (p) (let loop ((cs '()) (c (read-char p)))
      (if (eof-object? c) (list->string (reverse cs)) (loop (cons c cs) (read-char p)))))))

;; Load every form in a file, evaluating each in the current ns (an (ns ...) form
;; switches it). Returns (ok . fail); failures are tolerated (lenient — SCI requires
;; host libs that don't exist here). Push thread bindings for *warn-on-reflection*
;; and *assert* so vendored SCI code that (set! *warn-on-reflection* true) finds a
;; thread-local slot instead of throwing "Can't change/establish root binding".
;;
;; A #_ discard or a reader conditional with no matching branch (SCI's .cljc has
;; plenty of `#?(:cljs …)`) reads as rdr-eof with the position ADVANCED — "no
;; form here", not end of input. Keying the loop on that marker alone stopped the
;; read at the first one, silently dropping the rest of the file: utils.cljc gave
;; up 13 forms in, so its allowed-loop/allowed-recur never got defined and every
;; later file referring to them failed. Branch on `j` like load-jolt-file and
;; ei-read-all do, and stop only when the position doesn't move.
(define (load-forms path verbose)
  (let ((src (slurp path)) (ok 0) (fail 0)
        (warn-cell (guard (_ (#t #f)) (jolt-var "clojure.core" "*warn-on-reflection*")))
        (assert-cell (guard (_ (#t #f)) (jolt-var "clojure.core" "*assert*"))))
    (let ((end (string-length src)))
      (let loop ((i 0))
        (when (< i end)
          (call-with-values (lambda () (rdr-read-form src i end))
            (lambda (form j)
              (when (> j i)
                (unless (rdr-eof? form)
                  (guard (e (#t (set! fail (+ fail 1))
                                (when verbose
                                  (printf "    FAIL: ~a\n" (call-with-string-output-port
                                    (lambda (p) (display-condition (if (condition? e) e
                                      (make-message-condition (jolt-final-str e))) p)))))))
                    (when warn-cell
                      (jolt-push-thread-bindings
                        (jolt-hash-map warn-cell (var-cell-root warn-cell)
                                       assert-cell (var-cell-root assert-cell))))
                    (dynamic-wind
                      (lambda () #f)
                      (lambda ()
                        (jolt-compile-eval-form form (chez-current-ns))
                        (set! ok (+ ok 1)))
                      (lambda ()
                        (when warn-cell (jolt-pop-thread-bindings))))))
                (loop j)))))))
    (cons ok fail)))

(define verbose (and (getenv "SCI_VERBOSE") #t))

;; stubs first (host shims SCI's source expects)
(for-each (lambda (f) (load-forms (string-append "stdlib/clojure/sci/" f) verbose))
          '("lang_stubs.clj" "io_stubs.clj" "host_stubs.clj"))

(define sci-base "vendor/sci/src/sci/")
(define load-order
  '("impl/macros.cljc" "impl/protocols.cljc" "impl/types.cljc" "impl/unrestrict.cljc"
    "impl/vars.cljc" "lang.cljc" "impl/utils.cljc" "ctx_store.cljc" "impl/deftype.cljc"
    "impl/records.cljc" "impl/core_protocols.cljc" "impl/hierarchies.cljc"
    "impl/destructure.cljc" "impl/doseq_macro.cljc" "impl/for_macro.cljc" "impl/fns.cljc"
    "impl/multimethods.cljc" "impl/cljs.cljc" "impl/copy_vars.cljc" "impl/namespaces.cljc" "core.cljc"))

(define total-ok 0) (define total-fail 0)
(for-each
  (lambda (f)
    (let* ((r (load-forms (string-append sci-base f) verbose)) (ok (car r)) (fail (cdr r)))
      (set! total-ok (+ total-ok ok)) (set! total-fail (+ total-fail fail))
      (printf "  ~a: ~a ok, ~a fail\n" f ok fail)))
  load-order)

(printf "\nSCI load: ~a/~a forms ok (~a fail)\n" total-ok (+ total-ok total-fail) total-fail)
(define floor (let ((s (getenv "JOLT_SCI_FLOOR"))) (if s (string->number s) 411)))
(when (< total-ok floor)
  (printf "REGRESSION: ~a forms loaded < floor ~a\n" total-ok floor))
(flush-output-port)
(exit (if (< total-ok floor) 1 0))
