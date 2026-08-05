;; state-image.ss — dump a running program's state to a file and read it back.
;;
;; This is a STATE image, not a process image. Chez removed save-heap, and
;; fasl-write rejects every procedure, continuation, port and thread, so nothing
;; here can capture in-flight execution. What travels is the value graph.
;;
;; The body is pure data by construction: a procedure is written as the NAME of
;; the var it is bound to, never as code. Because the stream then holds no code
;; objects, Chez stamps it machine-type 0 (machine-independent), which is what
;; makes restoring on another architecture work.
;;
;; File layout — three fasl objects written back-to-back on one port:
;;   1. header    : vector, version + compat fields
;;   2. externals : list of descriptors, one per object fasl-write refused
;;   3. body      : the graph, with each refused object replaced by a placeholder
;; Reading resolves the descriptors first and hands the resulting vector to
;; fasl-read, which fails loudly if the count disagrees with the body.
;;
;; Loaded LAST from rt.ss: needs the collections, the var table, the printers,
;; and proc-name-tbl (rt.ss) for the procedure -> "ns/name" direction.

(define jolt-image-format-version 1)

;; --- classification -----------------------------------------------------------
;; An eq hashtable is the ONE hashtable kind Chez can fasl; eqv/equal/string-hash
;; tables carry their hash and equivalence procedures, so they need descriptors.
(define (image-eq-hashtable? x)
  (and (hashtable? x) (eq? (hashtable-equivalence-function x) eq?)))

;; Objects that must not travel as raw fasl. Two distinct reasons:
;;
;;  - Chez REFUSES them (procedures, non-eq hashtables, ports, threads).
;;  - Chez would happily write them, but the copy that comes back is WRONG.
;;    Keywords are interned (values.ss) and jolt map lookup compares them by
;;    identity, so a fasl-copied keyword is a key nothing can ever find: the map
;;    prints and counts correctly but every (:k m) returns nil and = is false.
;;    They are re-interned through the externals path instead. Their cached
;;    khash is content-derived, so re-interning yields an equal hash and the
;;    restored trie stays valid.
;;
;; Symbols are deliberately NOT here: they are not interned and compare by
;; ns/name, so a copy behaves correctly as a map key.
(define (image-external? x)
  (or (procedure? x)
      (keyword? x)
      (and (hashtable? x) (not (image-eq-hashtable? x)))
      (port? x)
      (thread? x)))

;; --- path-tracking walker ------------------------------------------------------
;; fasl-write's externals-pred sees objects but not where they live, and an
;; "unserializable object" error with no path is close to useless on a real
;; application state graph. So the walk is ours: it classifies every reachable
;; object and, for anything it cannot encode, records the route to it.

(define (image-path->string path)
  ;; path is accumulated innermost-first
  (let loop ((p (reverse path)) (acc ""))
    (if (null? p)
        (if (string=? acc "") "<root>" acc)
        (loop (cdr p)
              (if (string=? acc "") (car p) (string-append acc " -> " (car p)))))))

(define (image-describe-obj x)
  (cond
    ((procedure? x) "#<procedure>")
    ((port? x) "#<port>")
    ((thread? x) "#<thread>")
    ((hashtable? x) "#<hashtable>")
    (else (call/cc (lambda (k)
            (with-exception-handler (lambda (e) (k "#<object>"))
              (lambda () (jolt-pr-readable x))))))))

;; Walk the graph. Calls (visit obj path) on every object; collects nothing
;; itself. Cycle-safe via an eq table of in-progress/seen nodes.
(define (image-walk root visit)
  (let ((seen (make-eq-hashtable)))
    (let walk ((x root) (path '()))
      (unless (or (null? x) (boolean? x) (number? x) (char? x)
                  (symbol? x) (string? x) (bytevector? x))
        (if (hashtable-ref seen x #f)
            #t
            (begin
              (hashtable-set! seen x #t)
              (visit x path)
              (cond
                ;; jolt collections first — their internal trie shape would make
                ;; useless paths, so walk them as maps/vectors/sets instead.
                ((pmap? x)
                 (pmap-fold-fwd x (lambda (k v acc)
                                    (walk k (cons "<key>" path))
                                    (walk v (cons (image-describe-obj k) path))
                                    acc)
                                #f))
                ((pset? x)
                 (pset-fold x (lambda (e acc) (walk e (cons (image-describe-obj e) path)) acc) #f))
                ((pvec? x)
                 (let ((n (pvec-count x)))
                   (let loop ((i 0))
                     (when (fx<? i n)
                       (walk (pvec-nth-d x i jolt-nil) (cons (number->string i) path))
                       (loop (fx+ i 1))))))
                ((var-cell? x)
                 (walk (var-cell-root x)
                       (cons (string-append "#'" (var-cell-ns x) "/" (var-cell-name x)) path)))
                ((jolt-atom? x) (walk (jolt-atom-val x) (cons "@" path)))
                ((pair? x) (walk (car x) (cons "car" path)) (walk (cdr x) (cons "cdr" path)))
                ((vector? x)
                 (let ((n (vector-length x)))
                   (let loop ((i 0))
                     (when (fx<? i n)
                       (walk (vector-ref x i) (cons (number->string i) path))
                       (loop (fx+ i 1))))))
                ((and (hashtable? x) (hashtable-mutable? x))
                 (let-values (((ks vs) (hashtable-entries x)))
                   (let loop ((i 0))
                     (when (fx<? i (vector-length ks))
                       (walk (vector-ref vs i) (cons (image-describe-obj (vector-ref ks i)) path))
                       (loop (fx+ i 1))))))
                ;; generic record: walk declared fields by name. Covers user
                ;; defrecords, lazy seqs, refs, everything not special-cased.
                ((and (record? x) (record-rtd x))
                 (let* ((rtd (record-rtd x))
                        (names (record-type-field-names rtd))
                        (n (vector-length names)))
                   (let loop ((i 0))
                     (when (fx<? i n)
                       (let ((v (call/cc (lambda (k)
                                  (with-exception-handler (lambda (e) (k #f))
                                    (lambda () ((record-accessor rtd i) x)))))))
                         (walk v (cons (symbol->string (vector-ref names i)) path)))
                       (loop (fx+ i 1))))))
                (else #t)))))))
  jolt-nil)

;; --- externals: encode one refused object as data ------------------------------
;; Returns a descriptor (pure data) or #f when the object cannot be encoded.
;; Handlers registered from jolt get first refusal, so an application can teach
;; the encoder about its own resources.
(define image-handlers '())   ; list of (pred dump restore)

(define (jolt-image-register-handler! pred dump restore)
  (set! image-handlers (cons (list pred dump restore) image-handlers))
  jolt-nil)

(define (image-handler-for x)
  (let loop ((hs image-handlers))
    (cond ((null? hs) #f)
          ((jolt-truthy? (jolt-invoke (caar hs) x)) (car hs))
          (else (loop (cdr hs))))))

(define (image-encode-external x)
  (let ((h (image-handler-for x)))
    (cond
      (h (list 'handler (jolt-invoke (cadr h) x)))
      ((keyword? x) (list 'kw (keyword-t-ns x) (keyword-t-name x)))
      ((procedure? x)
       (let ((p (hashtable-ref proc-name-tbl x #f)))
         ;; A named fn travels as its var's name. A bare closure has no stable
         ;; identity to write, so it is refused here and reported with its path.
         (and p (list 'fn-ref (car p) (cdr p)))))
      ;; A non-eq hashtable is refused rather than described. Its contents would
      ;; have to ride in the descriptor stream, which is written WITHOUT an
      ;; externals-pred — so a table holding procedures (a sorted map's internal
      ;; comparator machinery is exactly this) would blow up there, outside the
      ;; mechanism that is supposed to catch it. Refusing keeps the failure inside
      ;; the path-reporting path. Sorted maps and sorted sets are therefore not
      ;; writable in this format version.
      (else #f))))

(define (image-decode-external d)
  (case (car d)
    ;; back through the intern table, so the restored keyword IS the live one
    ((kw) (keyword (cadr d) (caddr d)))
    ((fn-ref)
     (let ((c (var-cell-lookup (cadr d) (caddr d))))
       (if (and c (not (jolt-var-unbound? (var-cell-root c))))
           (var-cell-root c)
           (jolt-throw (jolt-ex-info
                         (string-append "image: no var " (cadr d) "/" (caddr d)
                                        " in this build to restore a function reference")
                         jolt-nil)))))
    ((handler)
     (let loop ((hs image-handlers))
       (if (null? hs)
           (jolt-throw (jolt-ex-info "image: no handler registered to restore a resource" jolt-nil))
           ;; restore fns are tried in registration order; the first that accepts wins
           (let ((r (call/cc (lambda (k)
                      (with-exception-handler (lambda (e) (k 'image-no))
                        (lambda () (jolt-invoke (caddr (car hs)) (cadr d))))))))
             (if (eq? r 'image-no) (loop (cdr hs)) r)))))
    (else (jolt-throw (jolt-ex-info "image: unknown external descriptor" jolt-nil)))))

;; --- scan ----------------------------------------------------------------------
;; Dry run: every object that cannot be encoded, with the route to it. Returns a
;; jolt vector of maps so callers can render or assert on it.
(define (jolt-image-scan v)
  (let ((bad '()))
    (image-walk v (lambda (x path)
                    (when (and (image-external? x) (not (image-encode-external x)))
                      (set! bad (cons (cons (image-path->string path)
                                            (image-describe-obj x))
                                      bad)))))
    (apply jolt-vector
           (map (lambda (p)
                  (jolt-hash-map (jolt-keyword "path") (car p)
                                 (jolt-keyword "object") (cdr p)))
                (reverse bad)))))

;; --- header --------------------------------------------------------------------
(define (image-header)
  (vector 'jolt-image
          jolt-image-format-version
          (jolt-image-runtime-version)
          (symbol->string (machine-type))))

(define (image-check-header! h path)
  (unless (and (vector? h) (fx=? (vector-length h) 4) (eq? (vector-ref h 0) 'jolt-image))
    (jolt-throw (jolt-ex-info (string-append "image: " path " is not a jolt image") jolt-nil)))
  (unless (equal? (vector-ref h 1) jolt-image-format-version)
    (jolt-throw (jolt-ex-info
                  (string-append "image: " path " has format version "
                                 (jolt-str-one (vector-ref h 1)) ", this build reads version "
                                 (number->string jolt-image-format-version))
                  jolt-nil)))
  ;; The fasl version moves with Chez, and a mismatch otherwise surfaces as an
  ;; opaque fasl-read error, so name it here instead.
  (unless (equal? (vector-ref h 2) (jolt-image-runtime-version))
    (jolt-throw (jolt-ex-info
                  (string-append "image: " path " was written by runtime "
                                 (jolt-str-one (vector-ref h 2)) ", this is "
                                 (jolt-image-runtime-version))
                  jolt-nil)))
  #t)

;; Runtime identity an image is pinned to. The fasl format moves with the Chez
;; release, so the Chez version is the honest key; the architecture deliberately
;; is NOT part of it.
(define (jolt-image-runtime-version)
  (string-append "chez-" (number->string (scheme-version-number*))))

(define (scheme-version-number*)
  ;; (scheme-version) is like "Chez Scheme Version 10.4.1"; reduce to an integer
  ;; so the check is a cheap equal? and prints readably.
  (let* ((s (scheme-version))
         (n (string-length s)))
    (let loop ((i 0) (acc 0) (seen #f))
      (if (fx>=? i n)
          acc
          (let ((c (string-ref s i)))
            (cond ((char-numeric? c) (loop (fx+ i 1) (+ (* acc 10) (- (char->integer c) 48)) #t))
                  ((and seen (char=? c #\.)) (loop (fx+ i 1) acc #t))
                  (seen acc)
                  (else (loop (fx+ i 1) acc seen))))))))

;; --- write / read --------------------------------------------------------------
;; Metadata lives in a weak side table keyed by object identity (natives-meta.ss),
;; so it cannot ride on the object itself. It rides in the SAME fasl stream
;; instead: fasl preserves sharing within one stream, so the objects in this
;; alist come back eq? to the ones in the graph and the meta can be re-attached.
(define (image-collect-meta v)
  (let ((acc '()))
    (image-walk v (lambda (x path)
                    (unless (var-cell? x)
                      (let ((m (call/cc (lambda (k)
                                 (with-exception-handler (lambda (e) (k jolt-nil))
                                   (lambda () (jolt-meta x)))))))
                        (unless (jolt-nil? m) (set! acc (cons (cons x m) acc)))))))
    acc))

(define (image-reattach-meta! pairs)
  (for-each (lambda (p) (hashtable-set! meta-table (car p) (cdr p))) pairs))

(define (jolt-image-write! path v)
  ;; externals are collected in encounter order; the eq table is only for
  ;; membership, since a keyword-dense graph makes a list scan quadratic.
  (let ((externals '()) (ext-seen (make-eq-hashtable)) (ext-tail #f))
    ;; Body first: the externals list is discovered during fasl-write, so it
    ;; cannot be written ahead of the body.
    (let ((body (call-with-bytevector-output-port
                  (lambda (p)
                    (fasl-write (vector v (image-collect-meta v)) p
                      (lambda (x)
                        (and (image-external? x)
                             (begin
                               (unless (hashtable-ref ext-seen x #f)
                                 (hashtable-set! ext-seen x #t)
                                 (let ((cell (list x)))
                                   (if ext-tail
                                       (begin (set-cdr! ext-tail cell) (set! ext-tail cell))
                                       (begin (set! externals cell) (set! ext-tail cell)))))
                               #t))))))))
      (let ((descs (map (lambda (x)
                          (or (image-encode-external x)
                              ;; Re-walk for the path only on the failure branch,
                              ;; so the happy path pays nothing for it.
                              (let ((where "<unknown>"))
                                (image-walk v (lambda (o p)
                                                (when (eq? o x) (set! where (image-path->string p)))))
                                (jolt-throw (jolt-ex-info
                                              (string-append "image: cannot write "
                                                             (image-describe-obj x)
                                                             " at " where)
                                              jolt-nil)))))
                        externals)))
        ;; Descriptors are written WITHOUT an externals-pred, so a handler that
        ;; returns something non-data would fail here with a raw Chez error.
        ;; Check before opening the file, so a rejected dump never leaves a
        ;; half-written image behind.
        (let ((desc-bytes
                (call/cc (lambda (k)
                  (with-exception-handler
                    (lambda (e)
                      (k (jolt-throw (jolt-ex-info
                                       "image: a resource handler returned a value that is not plain data"
                                       jolt-nil))))
                    (lambda () (call-with-bytevector-output-port
                                 (lambda (p) (fasl-write descs p)))))))))
          (let ((port (open-file-output-port path (file-options no-fail))))
            (fasl-write (image-header) port)
            (put-bytevector port desc-bytes)
            (put-bytevector port body)
            (close-port port)))))
    jolt-nil))

(define (jolt-image-read path)
  (unless (file-exists? path)
    (jolt-throw (jolt-ex-info (string-append "image: no such file: " path) jolt-nil)))
  (let ((port (open-file-input-port path)))
    (let* ((h (fasl-read port))
           (_ (image-check-header! h path))
           (descs (fasl-read port))
           (exts (list->vector (map image-decode-external descs)))
           (b (fasl-read port 'load exts)))
      (close-port port)
      (unless (and (vector? b) (fx=? (vector-length b) 2))
        (jolt-throw (jolt-ex-info (string-append "image: malformed body in " path) jolt-nil)))
      (image-reattach-meta! (vector-ref b 1))
      (vector-ref b 0))))

(def-var! "jolt.host" "image-write!" jolt-image-write!)
(def-var! "jolt.host" "image-read" jolt-image-read)
(def-var! "jolt.host" "image-scan" jolt-image-scan)
(def-var! "jolt.host" "image-register-handler!" jolt-image-register-handler!)
(def-var! "jolt.host" "image-runtime-version" jolt-image-runtime-version)
