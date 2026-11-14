;; jolt.host/class-names and all-classes (#1663): every class the runtime
;; models, by canonical JVM name, as babashka.classes/all-classes answers.
;; No one table holds that set, so this reads the class graph (rows and the
;; supers they name), the statics and constructor registries, the host tags'
;; FQNs, and the lazy-shim index (listing never loads a shim).
;; Registries also hold short aliases ("System"), so only dotted names are
;; kept, in Class.getName spelling (jclass-jvm-name). A reify's class is left
;; out: it has no name a caller could ask for.
;; Each call reads the tables as they are then. The graph scan holds
;; jch-cache-mutex, like every whole-table scan of it.

(define (class-enum-keys t)
  (let-values (((ks vs) (hashtable-entries t))) (vector->list ks)))

(define (class-enum-dotted? n)
  (let loop ((i (fx- (string-length n) 1)))
    (and (fx>= i 0) (or (char=? (string-ref n i) #\.) (loop (fx- i 1))))))

(define (class-enum-graph-names)
  (jolt-with-mutex jch-cache-mutex
    (let-values (((ks vs) (hashtable-entries jvm-class-parents)))
      (let loop ((i 0) (acc '()))
        (if (fx=? i (vector-length ks))
            acc
            (loop (fx+ i 1) (append (vector-ref vs i) (cons (vector-ref ks i) acc))))))))

(define (class-enum-reify? n)
  (let ((m "$reify__") (len (string-length n)))
    (let loop ((i 0))
      (and (fx<= (fx+ i 8) len)
           (or (string=? (substring n i (fx+ i 8)) m) (loop (fx+ i 1)))))))

(define (jolt-class-names)
  (let ((seen (make-hashtable string-hash string=?)))
    (for-each
     (lambda (n)
       (when (and (string? n) (class-enum-dotted? n) (not (class-enum-reify? n)))
         (hashtable-set! seen (jch-munge-segments (or (jch-registered-name n) n)) #t)))
     (append (class-enum-graph-names)
             (class-enum-keys class-statics-tbl)
             (class-enum-keys class-ctors-tbl)
             (class-enum-keys lazy-shim-class-tbl)
             (let-values (((ks vs) (hashtable-entries jhost-tag->fqn))) (vector->list vs))))
    (list-sort string<? (vector->list (hashtable-keys seen)))))

(def-var! "jolt.host" "class-names" (lambda () (apply jolt-vector (jolt-class-names))))
(def-var! "jolt.host" "all-classes" (lambda () (apply jolt-vector (map jolt-class-for (jolt-class-names)))))
