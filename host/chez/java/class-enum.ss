;; class-enum.ss — every class the runtime models, by its canonical JVM name
;; (#1663): jolt.host/class-names and jolt.host/all-classes, what babashka's
;; babashka.classes/all-classes answers, for an SCI host building its class map
;; or an editor completing class names.
;;
;; No one table holds that set, so this reads every place a class is modeled:
;;   - the class graph (class-hierarchy.ss): each row, and each super a row
;;     names (clojure.lang.Seqable, clojure.lang.IMeta, ... are supers with no
;;     row of their own, yet instance? and isa? answer for them);
;;   - the statics and constructor registries (host-static.ss): classes such
;;     as java.util.Collections, java.nio.file.Files or java.net.URL that have
;;     no graph row;
;;   - the host tags' FQNs (jhost-tag->fqn): java.nio.file.WatchKey and other
;;     classes known only as the class of some value;
;;   - the lazy-shim index (lazy-shims.ss): a class a lazy file registers is
;;     listed whether or not the file has loaded, and listing never loads it.
;; The registries hold short aliases beside the canonical names (a class
;; registered as "System" is also "java.lang.System"), so a name is kept only
;; in its canonical spelling: one with a dot, in the spelling Class.getName
;; answers (jclass-jvm-name: a deftype in a dashed namespace is registered as
;; rf.def-two.R3 and named rf.def_two.R3). A short name is the alias of a
;; canonical one that is listed anyway. A reify's class (ns$reify__<n>) is
;; left out: it has no name a caller could ask for, and one appears for every
;; reify form a program evaluates.
;;
;; Each call reads the tables as they are then, so a class a deftype, a
;; defrecord or a library registers later is there on the next call. The graph
;; is scanned under jch-cache-mutex, as every whole-table scan of it is; the
;; registries follow their own rule, lock-free reads beside one writer.

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
