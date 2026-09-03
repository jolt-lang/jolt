;; records-gambit.ss — GENERATED from host/chez/{records,records-coll,
;; protocols,records-dispatch}.ss by host/gambit/gen-records.ss (make
;; gambitgen). Do not edit; regenerate when any of the four changes. The
;; define-jrec-family transformer is expansion-phase-hostile on Gambit;
;; its uses are pre-expanded here.

(define-record-type (jrdesc make-jrdesc-rec jrdesc?)
  (fields tag fkeys index (mutable ptable))
  (nongenerative chez-jrdesc-v3))

(define (make-jrdesc tag fkey-list)
  (let ((index (make-eq-hashtable)))
    (let loop ((ks fkey-list) (i 0))
      (unless (null? ks)
        (hashtable-set! index (car ks) i)
        (loop (cdr ks) (+ i 1))))
    (make-jrdesc-rec tag (list->vector fkey-list) index #f)))

(define (jrdesc-nfields d) (vector-length (jrdesc-fkeys d)))

;; (define-syntax define-jrec-family ...) — pre-expanded below

;; expansion of (define-jrec-family 8)
(define-record-type (jrec make-jrec0 jrec?)
  (fields (immutable desc) (immutable ext) (mutable hasheq))
  (nongenerative chez-jrec-v5))

(define-record-type (jrec1 make-jrec1 jrec1?)
  (parent jrec)
  (fields (mutable f0))
  (nongenerative chez-jrec1v2))

(define-record-type (jrec2 make-jrec2 jrec2?)
  (parent jrec)
  (fields (mutable f0) (mutable f1))
  (nongenerative chez-jrec2v2))

(define-record-type (jrec3 make-jrec3 jrec3?)
  (parent jrec)
  (fields (mutable f0) (mutable f1) (mutable f2))
  (nongenerative chez-jrec3v2))

(define-record-type (jrec4 make-jrec4 jrec4?)
  (parent jrec)
  (fields (mutable f0) (mutable f1) (mutable f2) (mutable f3))
  (nongenerative chez-jrec4v2))

(define-record-type (jrec5 make-jrec5 jrec5?)
  (parent jrec)
  (fields (mutable f0) (mutable f1) (mutable f2) (mutable f3)
    (mutable f4))
  (nongenerative chez-jrec5v2))

(define-record-type (jrec6 make-jrec6 jrec6?)
  (parent jrec)
  (fields (mutable f0) (mutable f1) (mutable f2) (mutable f3)
    (mutable f4) (mutable f5))
  (nongenerative chez-jrec6v2))

(define-record-type (jrec7 make-jrec7 jrec7?)
  (parent jrec)
  (fields (mutable f0) (mutable f1) (mutable f2) (mutable f3)
    (mutable f4) (mutable f5) (mutable f6))
  (nongenerative chez-jrec7v2))

(define-record-type (jrec8 make-jrec8 jrec8?)
  (parent jrec)
  (fields (mutable f0) (mutable f1) (mutable f2) (mutable f3)
    (mutable f4) (mutable f5) (mutable f6) (mutable f7))
  (nongenerative chez-jrec8v2))

(define-record-type (jrec* make-jrec* jrec*?)
  (parent jrec)
  (fields (mutable vals))
  (nongenerative chez-jrecsp-v2))

(define (jrec-nfields r)
  (cond
    ((jrec1? r) 1)
    ((jrec2? r) 2)
    ((jrec3? r) 3)
    ((jrec4? r) 4)
    ((jrec5? r) 5)
    ((jrec6? r) 6)
    ((jrec7? r) 7)
    ((jrec8? r) 8)
    ((jrec*? r) (vector-length (jrec*-vals r)))
    (else 0)))

(define (jrec-field-ref r i)
  (let ((n (jrdesc-nfields (jrec-desc r))))
    (case n
      ((1)
       (case i
         ((0) (jrec1-f0 r))
         (else (error 'jrec-field-ref "index out of range" i))))
      ((2)
       (case i
         ((0) (jrec2-f0 r))
         ((1) (jrec2-f1 r))
         (else (error 'jrec-field-ref "index out of range" i))))
      ((3)
       (case i
         ((0) (jrec3-f0 r))
         ((1) (jrec3-f1 r))
         ((2) (jrec3-f2 r))
         (else (error 'jrec-field-ref "index out of range" i))))
      ((4)
       (case i
         ((0) (jrec4-f0 r))
         ((1) (jrec4-f1 r))
         ((2) (jrec4-f2 r))
         ((3) (jrec4-f3 r))
         (else (error 'jrec-field-ref "index out of range" i))))
      ((5)
       (case i
         ((0) (jrec5-f0 r))
         ((1) (jrec5-f1 r))
         ((2) (jrec5-f2 r))
         ((3) (jrec5-f3 r))
         ((4) (jrec5-f4 r))
         (else (error 'jrec-field-ref "index out of range" i))))
      ((6)
       (case i
         ((0) (jrec6-f0 r))
         ((1) (jrec6-f1 r))
         ((2) (jrec6-f2 r))
         ((3) (jrec6-f3 r))
         ((4) (jrec6-f4 r))
         ((5) (jrec6-f5 r))
         (else (error 'jrec-field-ref "index out of range" i))))
      ((7)
       (case i
         ((0) (jrec7-f0 r))
         ((1) (jrec7-f1 r))
         ((2) (jrec7-f2 r))
         ((3) (jrec7-f3 r))
         ((4) (jrec7-f4 r))
         ((5) (jrec7-f5 r))
         ((6) (jrec7-f6 r))
         (else (error 'jrec-field-ref "index out of range" i))))
      ((8)
       (case i
         ((0) (jrec8-f0 r))
         ((1) (jrec8-f1 r))
         ((2) (jrec8-f2 r))
         ((3) (jrec8-f3 r))
         ((4) (jrec8-f4 r))
         ((5) (jrec8-f5 r))
         ((6) (jrec8-f6 r))
         ((7) (jrec8-f7 r))
         (else (error 'jrec-field-ref "index out of range" i))))
      (else
       (if (jrec*? r)
           (vector-ref (jrec*-vals r) i)
           (error 'jrec-field-ref "not a fielded record" r))))))

(define (jrec-field-set! r i v)
  (cond
    ((jrec1? r)
     (case i
       ((0) (jrec1-f0-set! r v))
       (else (error 'jrec-field-set! "index out of range" i))))
    ((jrec2? r)
     (case i
       ((0) (jrec2-f0-set! r v))
       ((1) (jrec2-f1-set! r v))
       (else (error 'jrec-field-set! "index out of range" i))))
    ((jrec3? r)
     (case i
       ((0) (jrec3-f0-set! r v))
       ((1) (jrec3-f1-set! r v))
       ((2) (jrec3-f2-set! r v))
       (else (error 'jrec-field-set! "index out of range" i))))
    ((jrec4? r)
     (case i
       ((0) (jrec4-f0-set! r v))
       ((1) (jrec4-f1-set! r v))
       ((2) (jrec4-f2-set! r v))
       ((3) (jrec4-f3-set! r v))
       (else (error 'jrec-field-set! "index out of range" i))))
    ((jrec5? r)
     (case i
       ((0) (jrec5-f0-set! r v))
       ((1) (jrec5-f1-set! r v))
       ((2) (jrec5-f2-set! r v))
       ((3) (jrec5-f3-set! r v))
       ((4) (jrec5-f4-set! r v))
       (else (error 'jrec-field-set! "index out of range" i))))
    ((jrec6? r)
     (case i
       ((0) (jrec6-f0-set! r v))
       ((1) (jrec6-f1-set! r v))
       ((2) (jrec6-f2-set! r v))
       ((3) (jrec6-f3-set! r v))
       ((4) (jrec6-f4-set! r v))
       ((5) (jrec6-f5-set! r v))
       (else (error 'jrec-field-set! "index out of range" i))))
    ((jrec7? r)
     (case i
       ((0) (jrec7-f0-set! r v))
       ((1) (jrec7-f1-set! r v))
       ((2) (jrec7-f2-set! r v))
       ((3) (jrec7-f3-set! r v))
       ((4) (jrec7-f4-set! r v))
       ((5) (jrec7-f5-set! r v))
       ((6) (jrec7-f6-set! r v))
       (else (error 'jrec-field-set! "index out of range" i))))
    ((jrec8? r)
     (case i
       ((0) (jrec8-f0-set! r v))
       ((1) (jrec8-f1-set! r v))
       ((2) (jrec8-f2-set! r v))
       ((3) (jrec8-f3-set! r v))
       ((4) (jrec8-f4-set! r v))
       ((5) (jrec8-f5-set! r v))
       ((6) (jrec8-f6-set! r v))
       ((7) (jrec8-f7-set! r v))
       (else (error 'jrec-field-set! "index out of range" i))))
    ((jrec*? r) (vector-set! (jrec*-vals r) i v))
    (else (error 'jrec-field-set! "not a fielded record" r))))

(define (jrec-field-at r i k)
  (cond
    ((jrec1? r)
     (case i ((0) (jrec1-f0 r)) (else (jolt-get r k))))
    ((jrec2? r)
     (case i
       ((0) (jrec2-f0 r))
       ((1) (jrec2-f1 r))
       (else (jolt-get r k))))
    ((jrec3? r)
     (case i
       ((0) (jrec3-f0 r))
       ((1) (jrec3-f1 r))
       ((2) (jrec3-f2 r))
       (else (jolt-get r k))))
    ((jrec4? r)
     (case i
       ((0) (jrec4-f0 r))
       ((1) (jrec4-f1 r))
       ((2) (jrec4-f2 r))
       ((3) (jrec4-f3 r))
       (else (jolt-get r k))))
    ((jrec5? r)
     (case i
       ((0) (jrec5-f0 r))
       ((1) (jrec5-f1 r))
       ((2) (jrec5-f2 r))
       ((3) (jrec5-f3 r))
       ((4) (jrec5-f4 r))
       (else (jolt-get r k))))
    ((jrec6? r)
     (case i
       ((0) (jrec6-f0 r))
       ((1) (jrec6-f1 r))
       ((2) (jrec6-f2 r))
       ((3) (jrec6-f3 r))
       ((4) (jrec6-f4 r))
       ((5) (jrec6-f5 r))
       (else (jolt-get r k))))
    ((jrec7? r)
     (case i
       ((0) (jrec7-f0 r))
       ((1) (jrec7-f1 r))
       ((2) (jrec7-f2 r))
       ((3) (jrec7-f3 r))
       ((4) (jrec7-f4 r))
       ((5) (jrec7-f5 r))
       ((6) (jrec7-f6 r))
       (else (jolt-get r k))))
    ((jrec8? r)
     (case i
       ((0) (jrec8-f0 r))
       ((1) (jrec8-f1 r))
       ((2) (jrec8-f2 r))
       ((3) (jrec8-f3 r))
       ((4) (jrec8-f4 r))
       ((5) (jrec8-f5 r))
       ((6) (jrec8-f6 r))
       ((7) (jrec8-f7 r))
       (else (jolt-get r k))))
    ((jrec*? r)
     (if (fx< i (vector-length (jrec*-vals r)))
         (vector-ref (jrec*-vals r) i)
         (jolt-get r k)))
    (else (jolt-get r k))))

(define jrec-ctor-vec
  (vector make-jrec0 make-jrec1 make-jrec2 make-jrec3
    make-jrec4 make-jrec5 make-jrec6 make-jrec7 make-jrec8))

(define (make-jrec-from-existing src ov ov-val ext)
  (let ((desc (jrec-desc src)))
    (case (jrec-nfields src)
      ((0) (make-jrec0 desc ext 0))
      ((1)
       (make-jrec1
         desc
         ext
         0
         (if (eq? ov 0) ov-val (jrec1-f0 src))))
      ((2)
       (make-jrec2 desc ext 0 (if (eq? ov 0) ov-val (jrec2-f0 src))
         (if (eq? ov 1) ov-val (jrec2-f1 src))))
      ((3)
       (make-jrec3 desc ext 0 (if (eq? ov 0) ov-val (jrec3-f0 src))
         (if (eq? ov 1) ov-val (jrec3-f1 src))
         (if (eq? ov 2) ov-val (jrec3-f2 src))))
      ((4)
       (make-jrec4 desc ext 0 (if (eq? ov 0) ov-val (jrec4-f0 src))
         (if (eq? ov 1) ov-val (jrec4-f1 src))
         (if (eq? ov 2) ov-val (jrec4-f2 src))
         (if (eq? ov 3) ov-val (jrec4-f3 src))))
      ((5)
       (make-jrec5 desc ext 0 (if (eq? ov 0) ov-val (jrec5-f0 src))
         (if (eq? ov 1) ov-val (jrec5-f1 src))
         (if (eq? ov 2) ov-val (jrec5-f2 src))
         (if (eq? ov 3) ov-val (jrec5-f3 src))
         (if (eq? ov 4) ov-val (jrec5-f4 src))))
      ((6)
       (make-jrec6 desc ext 0 (if (eq? ov 0) ov-val (jrec6-f0 src))
         (if (eq? ov 1) ov-val (jrec6-f1 src))
         (if (eq? ov 2) ov-val (jrec6-f2 src))
         (if (eq? ov 3) ov-val (jrec6-f3 src))
         (if (eq? ov 4) ov-val (jrec6-f4 src))
         (if (eq? ov 5) ov-val (jrec6-f5 src))))
      ((7)
       (make-jrec7 desc ext 0 (if (eq? ov 0) ov-val (jrec7-f0 src))
         (if (eq? ov 1) ov-val (jrec7-f1 src))
         (if (eq? ov 2) ov-val (jrec7-f2 src))
         (if (eq? ov 3) ov-val (jrec7-f3 src))
         (if (eq? ov 4) ov-val (jrec7-f4 src))
         (if (eq? ov 5) ov-val (jrec7-f5 src))
         (if (eq? ov 6) ov-val (jrec7-f6 src))))
      ((8)
       (make-jrec8 desc ext 0 (if (eq? ov 0) ov-val (jrec8-f0 src))
         (if (eq? ov 1) ov-val (jrec8-f1 src))
         (if (eq? ov 2) ov-val (jrec8-f2 src))
         (if (eq? ov 3) ov-val (jrec8-f3 src))
         (if (eq? ov 4) ov-val (jrec8-f4 src))
         (if (eq? ov 5) ov-val (jrec8-f5 src))
         (if (eq? ov 6) ov-val (jrec8-f6 src))
         (if (eq? ov 7) ov-val (jrec8-f7 src))))
      (else
       (let ((nv (jrec-vec-copy (jrec*-vals src))))
         (when ov (vector-set! nv ov ov-val))
         (make-jrec* desc ext 0 nv))))))

(define (make-jrec desc vals ext)
  (let ((n (vector-length vals)))
    (if (fx<= n 8)
        (apply (vector-ref jrec-ctor-vec n) desc ext 0
          (vector->list vals))
        (make-jrec* desc ext 0 vals))))

(define jrec-fast-type-probe
  (make-jrec 'fast-type-probe (vector) jolt-nil))

(define (jrec-vals r)
  (let* ((n (jrec-nfields r)) (v (make-vector n)))
    (do ((i 0 (fx+ i 1)))
        ((fx= i n) v)
      (vector-set! v i (jrec-field-ref r i)))))

(define (jrec-tag r) (jrdesc-tag (jrec-desc r)))

(define (jrec-pic-desc x) (and (jrec? x) (jrec-desc x)))

(define rec-tbl-mu (make-mutex))

(define chez-record-type-tbl
  (make-hashtable string-hash string=?))

(define (jrec-record?-uncached x)
  (and (jrec? x)
       (hashtable-ref chez-record-type-tbl (jrec-tag x) #f)
       #t))

(define (jrec-record? x)
  (and (jrec? x) (vector-ref (jrdesc-ifc-of x) 3)))

(define chez-deftype-tag-set
  (make-hashtable string-hash string=?))

(define chez-deftype-ctor-tag-box
  (box (make-weak-eq-hashtable)))

(define chez-deftype-ctor-tag-mu (make-mutex))

(define (deftype-ctor-tag p)
  (hashtable-ref (unbox chez-deftype-ctor-tag-box) p #f))

(define (deftype-ctor-tag-set! ctor tag)
  (jolt-identity-hasheq-seed! ctor (jolt-hash tag))
  (jolt-with-mutex
    chez-deftype-ctor-tag-mu
    (let ((t (hashtable-copy
               (unbox chez-deftype-ctor-tag-box)
               #t)))
      (hashtable-set! t ctor tag)
      (set-box! chez-deftype-ctor-tag-box t))))

(define (mm-dispatch-val-canon dval)
  (or (and (procedure? dval) (deftype-ctor-tag dval)) dval))

(define chez-simple-name-tag
  (make-hashtable string-hash string=?))

(define (chez-deftype-simple->tag nm)
  (hashtable-ref chez-simple-name-tag nm #f))

(define chez-tag-desc (make-hashtable string-hash string=?))

(define (jrec-collection? x)
  (and (jrec? x)
       (or (jrec-record? x)
           (let ((tag (jrec-tag x)))
             (or (find-method-any-protocol tag "cons")
                 (find-method-any-protocol tag "first"))))
       #t))

(define (jrec-maplike? x)
  (and (jrec? x)
       (or (jrec-record? x)
           (find-method-any-protocol (jrec-tag x) "without"))
       #t))

(define jolt-deftype-kw (keyword "jolt" "deftype"))

(define jrec-absent (list 'jrec-absent))

(define chez-record-shapes-tbl
  (make-hashtable string-hash string=?))

(define chez-protocol-methods-tbl
  (make-hashtable string-hash string=?))

(define chez-record-dbl-tbl
  (make-hashtable string-hash string=?))

(define (chez-double-tag? t)
  (and (string? t) (string=? t "double")))

(define chez-record-fields-tbl
  (make-hashtable string-hash string=?))

(define (chez-record-field-kws type-tag)
  (or (hashtable-ref chez-record-fields-tbl type-tag #f) '()))

(define (register-record-shape! ctor-key field-kws
         field-tags type-tag)
  (jolt-with-mutex
    rec-tbl-mu
    (hashtable-set!
      chez-record-shapes-tbl
      ctor-key
      (vector field-kws field-tags type-tag))
    (hashtable-set! chez-record-fields-tbl type-tag field-kws)
    (hashtable-set!
      chez-record-dbl-tbl
      type-tag
      (list->vector (map chez-double-tag? field-tags)))))

(define (chez-shape-simple-name s)
  (let loop ((i (- (string-length s) 1)))
    (cond
      ((< i 0) s)
      ((or (char=? (string-ref s i) #\.)
           (char=? (string-ref s i) #\/))
       (substring s (+ i 1) (string-length s)))
      (else (loop (- i 1))))))

(define (chez-resolve-field-tag tag by-name owner-ns)
  (cond
    ((or (not tag) (jolt-nil-t? tag)) jolt-nil)
    ((string=? tag "num") "num")
    ((string=? tag "double") "double")
    (else
     (let* ((simple (chez-shape-simple-name tag))
            (qualified? (not (string=? simple tag)))
            (ck (or (and qualified? (hashtable-ref by-name tag #f))
                    (hashtable-ref
                      by-name
                      (string-append owner-ns "." simple)
                      #f)
                    (hashtable-ref by-name simple #f))))
       (if ck ck jolt-nil)))))

(define (chez-ctor-key-ns k)
  (let loop ((i 0))
    (cond
      ((>= i (string-length k)) k)
      ((char=? (string-ref k i) #\/) (substring k 0 i))
      (else (loop (+ i 1))))))

(define (chez-record-shapes-map)
  (let ((by-name (make-hashtable string-hash string=?))
        (kw-fields (keyword #f "fields"))
        (kw-tags (keyword #f "tags"))
        (kw-type (keyword #f "type"))
        (out (jolt-hash-map)))
    (let-values (((ks vs)
                  (jolt-with-mutex
                    rec-tbl-mu
                    (let-values (((a b)
                                  (hashtable-entries
                                    chez-record-shapes-tbl)))
                      (values a b)))))
      (vector-for-each
        (lambda (k v)
          (let ((type-tag (vector-ref v 2)))
            (hashtable-set! by-name type-tag k)
            (hashtable-set!
              by-name
              (chez-shape-simple-name type-tag)
              k)))
        ks
        vs)
      (vector-for-each
        (lambda (k v)
          (let* ((fields (vector-ref v 0))
                 (tags (vector-ref v 1))
                 (type-tag (vector-ref v 2))
                 (owner-ns (chez-ctor-key-ns k))
                 (rtags (map (lambda (t)
                               (chez-resolve-field-tag t by-name owner-ns))
                             tags)))
            (set! out
              (jolt-assoc
                out
                k
                (jolt-hash-map kw-fields (apply jolt-vector fields) kw-tags
                  (apply jolt-vector rtags) kw-type type-tag)))))
        ks
        vs))
    out))

(define (chez-find-ctor-key name ns)
  (let* ((simple (chez-shape-simple-name name))
         (target (string-append "->" simple))
         (preferred (string-append ns "/->" simple)))
    (if (hashtable-ref chez-record-shapes-tbl preferred #f)
        preferred
        (let loop ((ks (vector->list
                         (jolt-with-mutex
                           rec-tbl-mu
                           (hashtable-keys chez-record-shapes-tbl)))))
          (cond
            ((null? ks) #f)
            ((string=? (chez-shape-simple-name (car ks)) target)
             (car ks))
            (else (loop (cdr ks))))))))

(define (chez-protocol-methods-map)
  (let ((out (jolt-hash-map)))
    (let-values (((ks vs)
                  (jolt-with-mutex
                    rec-tbl-mu
                    (let-values (((a b)
                                  (hashtable-entries
                                    chez-protocol-methods-tbl)))
                      (values a b)))))
      (vector-for-each
        (lambda (k v)
          (set! out (jolt-assoc out k (jolt-vector (car v) (cdr v)))))
        ks
        vs))
    out))

(define (jrec-field-index r k)
  (hashtable-ref (jrdesc-index (jrec-desc r)) k #f))

(define (jrec-vec-copy v)
  (let* ((n (vector-length v)) (out (make-vector n)))
    (let loop ((i 0))
      (when (< i n)
        (vector-set! out i (vector-ref v i))
        (loop (+ i 1))))
    out))

(define (jrec-ext-pairs ext)
  (let loop ((s (jolt-seq ext)) (acc '()))
    (if (jolt-nil? s)
        (reverse acc)
        (let ((e (seq-first s)))
          (loop
            (jolt-seq (seq-more s))
            (cons (cons (jolt-nth e 0) (jolt-nth e 1)) acc))))))

(define (jrec-lookup r k d)
  (if (eq? k jolt-deftype-kw)
      (jrec-tag r)
      (let ((i (jrec-field-index r k)))
        (if i
            (jrec-field-ref r i)
            (let ((ext (jrec-ext r)))
              (if (jolt-nil? ext)
                  d
                  (let ((v (jolt-get ext k jrec-absent)))
                    (if (eq? v jrec-absent) d v))))))))

(define (jrec-has? r k)
  (and (not (eq? k jolt-deftype-kw))
       (or (and (jrec-field-index r k) #t)
           (let ((ext (jrec-ext r)))
             (and (not (jolt-nil? ext))
                  (not (eq? jrec-absent (jolt-get ext k jrec-absent))))))))

(define (jrec-ref coll k d)
  (if (eq? k jolt-deftype-kw)
      (jrec-tag coll)
      (let ((i (jrec-field-index coll k)))
        (if i
            (jrec-field-ref coll i)
            (let* ((ext (jrec-ext coll))
                   (v (if (jolt-nil? ext)
                          jrec-absent
                          (jolt-get ext k jrec-absent))))
              (if (eq? v jrec-absent)
                  (cond
                    ((find-method-any-protocol (jrec-tag coll) "valAt") =>
                     (lambda (m) (jolt-invoke m coll k d)))
                    ((find-method-any-protocol (jrec-tag coll) "get") =>
                     (lambda (m)
                       (let ((r (jolt-invoke m coll k)))
                         (if (jolt-nil? r) d r))))
                    (else d))
                  v))))))

(define (jolt-set-field! inst k v)
  (if (jrec? inst)
      (let ((i (jrec-field-index inst k)))
        (if i
            (let* ((flags (hashtable-ref
                            chez-record-dbl-tbl
                            (jrec-tag inst)
                            #f))
                   (v2 (if (and flags
                                (fx< i (vector-length flags))
                                (vector-ref flags i)
                                (number? v)
                                (not (flonum? v)))
                           (exact->inexact v)
                           v)))
              (jrec-field-set! inst i v2)
              v2)
            (throw-jvm
              'IllegalArgumentException
              (string-append
                "set! of an unknown field: "
                (jolt-final-str k)))))
      (if (let ((kn (cond
                      ((string? k) k)
                      ((keyword-t? k) (keyword-t-name k))
                      (else #f))))
            (and kn (string=? kn "__methodImplCache")))
          v
          (throw-jvm
            'IllegalArgumentException
            "set! of a field on a non-record"))))

(define (jrec-ext=? ea eb)
  (cond
    ((and (jolt-nil? ea) (jolt-nil? eb)) #t)
    ((or (jolt-nil? ea) (jolt-nil? eb)) #f)
    (else (jolt=2 ea eb))))

(define (jrec=? a b)
  (and (string=? (jrec-tag a) (jrec-tag b))
       (let ((n (jrec-nfields a)))
         (and (= n (jrec-nfields b))
              (let loop ((i 0))
                (or (= i n)
                    (and (jolt=2 (jrec-field-ref a i) (jrec-field-ref b i))
                         (loop (+ i 1)))))))
       (jrec-ext=? (jrec-ext a) (jrec-ext b))))

(define jrdesc-class-hash-tbl (make-weak-eq-hashtable))

(define jrdesc-class-hash-mu (make-mutex))

(define (jrdesc-class-hash d)
  (or (hashtable-ref jrdesc-class-hash-tbl d #f)
      (let ((h (compute-symbol-hasheq #f (jrdesc-tag d))))
        (jolt-with-mutex
          jrdesc-class-hash-mu
          (hashtable-set! jrdesc-class-hash-tbl d h))
        h)))

(define (jrec-hash r)
  (let* ((class-hash (jrdesc-class-hash (jrec-desc r)))
         (fkeys (jrdesc-fkeys (jrec-desc r)))
         (n (jrec-nfields r))
         (result (let loop ((i 0) (acc 0) (cnt 0))
                   (if (= i n)
                       (cons acc cnt)
                       (loop
                         (+ i 1)
                         (add32
                           acc
                           (entry-hasheq
                             (vector-ref fkeys i)
                             (jrec-field-ref r i)))
                         (+ cnt 1)))))
         (ext (jrec-ext r))
         (total (if (jolt-nil? ext)
                    result
                    (pmap-fold
                      ext
                      (lambda (k v a)
                        (cons
                          (add32 (car a) (entry-hasheq k v))
                          (+ (cdr a) 1)))
                      result)))
         (map-hash (mix-coll-hash (car total) (cdr total))))
    (i32 (bitwise-xor class-hash map-hash))))

(define (jrec-hash-cached r)
  (if (jrec-record? r)
      (let ((h (jrec-hasheq r)))
        (if (eqv? h 0)
            (let ((h2 (jrec-hash r))) (jrec-hasheq-set! r h2) h2)
            h))
      (jrec-hash r)))

(define (jrec-coll-print-shape r)
  (vector-ref (jrdesc-ifc-of r) 1))

(define (jrec-coll-pr r shape)
  (if (jolt-print-hash?)
      "#"
      (with-deeper-print
        (if (eq? shape 'map)
            (string-append
              "{"
              (jolt-str-join-comma
                (jolt-limited-list-strs
                  (map (lambda (e)
                         (string-append
                           (jolt-pr-readable (jolt-nth e 0))
                           " "
                           (jolt-pr-readable (jolt-nth e 1))))
                       (seq->list r))))
              "}")
            (let ((body (jolt-str-join
                          (jolt-limited-seq-strs
                            (jolt-seq r)
                            jolt-pr-readable))))
              (case shape
                ((seq) (string-append "(" body ")"))
                ((vec) (string-append "[" body "]"))
                (else (string-append "#{" body "}"))))))))

(define (jrec-pr r)
  (cond
    ((jrec-coll-print-shape r) =>
     (lambda (shape) (jrec-coll-pr r shape)))
    (else (jrec-field-pr r))))

(define (jrec-field-pr r)
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r)))
         (n (vector-length fkeys))
         (entry-strs (let loop ((i 0) (acc '()))
                       (if (= i n)
                           (let ((ext (jrec-ext r)))
                             (reverse
                               (if (jolt-nil? ext)
                                   acc
                                   (fold-left
                                     (lambda (a p)
                                       (cons
                                         (string-append
                                           (jolt-pr-readable (car p))
                                           " "
                                           (jolt-pr-readable (cdr p)))
                                         a))
                                     acc
                                     (jrec-ext-pairs ext)))))
                           (loop
                             (+ i 1)
                             (cons
                               (string-append
                                 (jolt-pr-readable (vector-ref fkeys i))
                                 " "
                                 (jolt-pr-readable (jrec-field-ref r i)))
                               acc))))))
    (string-append "#" (jrec-tag r) "{"
      (jolt-str-join-comma entry-strs) "}")))

(register-eq-arm!
  (lambda (a b) (or (jrec? a) (jrec? b)))
  (lambda (a b)
    (cond
      ((and (jrec? a) (jrec-cl a "equiv")) =>
       (lambda (m) (if (jolt-truthy? (jolt-invoke m a b)) #t #f)))
      ((and (jrec? b) (jrec-cl b "equiv")) =>
       (lambda (m) (if (jolt-truthy? (jolt-invoke m b a)) #t #f)))
      ((and (jrec? a) (jrec-cl a "equals")) =>
       (lambda (m) (if (jolt-truthy? (jolt-invoke m a b)) #t #f)))
      ((and (jrec? b) (jrec-cl b "equals")) =>
       (lambda (m) (if (jolt-truthy? (jolt-invoke m b a)) #t #f)))
      ((or (jrec-sequential-decl? a) (jrec-sequential-decl? b))
       (and (seq-eq-candidate? a)
            (seq-eq-candidate? b)
            (seq=? a b)))
      ((and (jrec-record? a) (jrec-record? b)) (jrec=? a b))
      (else (eq? a b)))))

(define (jrec-hasheq-slow x)
  (cond
    ((jrec-cl x "hasheq") => (lambda (m) (jolt-invoke m x)))
    ((jrec-cl x "hashCode") => (lambda (m) (jolt-invoke m x)))
    ((jrec-record? x)
     (let ((h (jrec-hash x))) (jrec-hasheq-set! x h) h))
    (else
     (let ((h (jolt-identity-hasheq x)))
       (jrec-hasheq-set! x h)
       h))))

(define (jrec-hasheq-fast x)
  (let ((h (jrec-hasheq x)))
    (if (eqv? h 0) (jrec-hasheq-slow x) h)))

(define jrec-cl rec-coll-method)

(define (jrec-declares? x interface)
  (and (jrec? x) (jch-isa? (jrec-tag x) interface)))

(define jrdesc-ifc-tbl (make-weak-eq-hashtable))

(define jrdesc-ifc-mutex (make-mutex))

(define (jrdesc-ifc d) (hashtable-ref jrdesc-ifc-tbl d #f))

(define (jrdesc-ifc-set! d v)
  (jolt-with-mutex
    jrdesc-ifc-mutex
    (hashtable-set! jrdesc-ifc-tbl d v)))

(define (jrdesc-ifc-epoch)
  (fx+ jch-graph-epoch jolt-proto-epoch))

(define (jrdesc-derive-ifc d record?)
  (let ((tag (jrdesc-tag d)) (epoch (jrdesc-ifc-epoch)))
    (vector epoch
      (and (not record?)
           (cond
             ((jch-isa? tag "clojure.lang.ISeq") 'seq)
             ((jch-isa? tag "clojure.lang.IPersistentVector") 'vec)
             ((jch-isa? tag "clojure.lang.IPersistentSet") 'set)
             ((jch-isa? tag "clojure.lang.IPersistentMap") 'map)
             (else #f)))
      (jch-isa? tag "java.lang.CharSequence") record?
      (and (not record?) (tag-declares-coll-iface? tag))
      (and (not record?) (tag-declares-sequential? tag)))))

(define (jrdesc-ifc-of x)
  (let* ((d (jrec-desc x)) (c (jrdesc-ifc d)))
    (if (and c (fx=? (vector-ref c 0) (jrdesc-ifc-epoch)))
        c
        (let ((fresh (jrdesc-derive-ifc
                       d
                       (jrec-record?-uncached x))))
          (jrdesc-ifc-set! d fresh)
          fresh))))

(define (jrec-charseq? x)
  (and (jrec? x) (vector-ref (jrdesc-ifc-of x) 2)))

(define (jrec-charseq-method x name)
  (and (jrec-charseq? x) (jrec-cl x name)))

(define (jrec-charseq-length-method x)
  (or (jrec-cl x "length")
      (jrec-abstract-method-error x "length")))

(define (jrec-charseq->seq x len charat)
  (let ((n (->idx (jolt-invoke len x))))
    (let build ((i 0))
      (if (fx>=? i n)
          jolt-nil
          (cseq-lazy
            (jolt-invoke charat x i)
            (lambda () (build (fx+ i 1))))))))

(define (jrec-charseq->string x)
  (cond
    ((jrec-charseq-method x "toString") =>
     (lambda (m) (jolt-need-str (jolt-invoke m x))))
    ((jrec-charseq-method x "charAt") =>
     (lambda (m)
       (list->string
         (seq->list
           (jrec-charseq->seq x (jrec-charseq-length-method x) m)))))
    (else #f)))

(define jrec-coll-iface-names
  '("IPersistentCollection" "IPersistentMap" "IPersistentVector" "IPersistentSet"
     "IPersistentStack" "IPersistentList" "ISeq" "Seqable"
     "Indexed" "Counted" "Associative" "ILookup" "Reversible"
     "Sorted"))

(define (tag-declares-coll-iface? tag)
  (let ((ti (hashtable-ref type-registry tag #f)))
    (and ti
         (let loop ((ps (vector->list
                          (jolt-with-mutex
                            rec-tbl-mu
                            (hashtable-keys ti)))))
           (cond
             ((null? ps) #f)
             ((member (jch-last-segment (car ps)) jrec-coll-iface-names)
              #t)
             (else (loop (cdr ps))))))))

(define (jrec-declares-coll-iface? x)
  (and (jrec? x) (vector-ref (jrdesc-ifc-of x) 4)))

(define (tag-declares-sequential? tag)
  (let ((ti (hashtable-ref type-registry tag #f)))
    (and ti
         (let loop ((ps (vector->list
                          (jolt-with-mutex
                            rec-tbl-mu
                            (hashtable-keys ti)))))
           (cond
             ((null? ps) #f)
             ((string=? (jch-last-segment (car ps)) "Sequential") #t)
             (else (loop (cdr ps))))))))

(define (jrec-sequential-decl? x)
  (and (jrec? x) (vector-ref (jrdesc-ifc-of x) 5)))

(define (seq-eq-candidate? x)
  (or (jolt-sequential? x)
      (jolt-lazyseq? x)
      (jrec-sequential-decl? x)))

(define (jrec-abstract-method-error x method)
  (jolt-throw
    (jolt-host-throwable
      "java.lang.AbstractMethodError"
      (string-append "Method " (jrec-tag x) "/" method
        "() is abstract"))))

(define (iface-method v method nargs)
  (cond
    ((jrec? v)
     (if nargs
         (find-method-any-protocol-arity (jrec-tag v) method nargs)
         (find-method-any-protocol (jrec-tag v) method)))
    ((jreify? v)
     (let ((rm (reified-methods v)))
       (and rm (hashtable-ref rm method #f))))
    (else #f)))

(define (jrec-field-count coll)
  (+ (jrec-nfields coll)
     (let ((ext (jrec-ext coll)))
       (if (jolt-nil? ext) 0 (jolt-count ext)))))

(register-count-arm!
  (lambda (coll) (or (jrec? coll) (jolt-transient? coll)))
  (lambda (coll)
    (cond
      ((jolt-transient? coll) (t-count coll))
      ((jrec-cl coll "count") =>
       (lambda (m) (jolt-invoke m coll)))
      (else
       (let ((ifc (jrdesc-ifc-of coll)))
         (cond
           ((vector-ref ifc 3) (jrec-field-count coll))
           ((vector-ref ifc 4)
            (jrec-abstract-method-error coll "count"))
           ((vector-ref ifc 2)
            (jolt-invoke (jrec-charseq-length-method coll) coll))
           (else (jrec-field-count coll))))))))

(register-contains-arm!
  (lambda (coll) (jrec-cl coll "containsKey"))
  (lambda (coll k)
    (if (jolt-truthy?
          (jolt-invoke (jrec-cl coll "containsKey") coll k))
        #t
        #f)))

(register-contains-arm!
  (lambda (coll) (and (jrec? coll) (jrec-cl coll "contains")))
  (lambda (coll k)
    (if (jolt-truthy?
          (jolt-invoke (jrec-cl coll "contains") coll k))
        #t
        #f)))

(register-contains-arm!
  (lambda (coll)
    (and (jrec? coll)
         (not (jrec-cl coll "containsKey"))
         (not (jrec-cl coll "contains"))))
  (lambda (coll k) (jrec-has? coll k)))

(register-contains-arm! jolt-transient? t-contains?)

(register-empty-arm!
  jolt-transient?
  (lambda (t) (zero? (t-count t))))

(define %r-jolt-nth jolt-nth)

(set! jolt-nth
  (case-lambda
    ((coll i)
     (if (jolt-transient? coll)
         (begin
           (jolt-trans-check coll "nth")
           (if (eq? (jolt-transient-kind coll) 'vec)
               (let ((idx (->idx i)))
                 (if (tvec-in-bounds? coll idx)
                     (vector-ref (jolt-transient-buf coll) idx)
                     (error 'nth "index out of bounds")))
               (%r-jolt-nth (jolt-transient-buf coll) i)))
         (%r-jolt-nth coll i)))
    ((coll i d)
     (if (jolt-transient? coll)
         (if (eq? (jolt-transient-kind coll) 'vec)
             (let ((idx (->idx i)))
               (if (tvec-in-bounds? coll idx)
                   (vector-ref (jolt-transient-buf coll) idx)
                   d))
             (%r-jolt-nth (jolt-transient-buf coll) i d))
         (%r-jolt-nth coll i d)))))

(define %r-jolt-assoc1 jolt-assoc1)

(set! jolt-assoc1
  (lambda (coll k v)
    (cond
      ((jrec-cl coll "assoc") =>
       (lambda (m) (jolt-invoke m coll k v)))
      ((jrec? coll)
       (let ((i (and (keyword? k) (jrec-field-index coll k))))
         (if i
             (let ((v2 (let ((flags (hashtable-ref
                                      chez-record-dbl-tbl
                                      (jrec-tag coll)
                                      #f)))
                         (if (and flags
                                  (fx< i (vector-length flags))
                                  (vector-ref flags i)
                                  (number? v)
                                  (not (flonum? v)))
                             (exact->inexact v)
                             v))))
               (make-jrec-from-existing coll i v2 (jrec-ext coll)))
             (let ((ext (jrec-ext coll)))
               (make-jrec-from-existing
                 coll
                 #f
                 #f
                 (%r-jolt-assoc1
                   (if (jolt-nil? ext) empty-pmap ext)
                   k
                   v))))))
      (else (%r-jolt-assoc1 coll k v)))))

(define (jrec->map-without r drop-k)
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r)))
         (n (vector-length fkeys)))
    (let loop ((i 0) (m empty-pmap))
      (if (= i n)
          (let ((ext (jrec-ext r)))
            (if (jolt-nil? ext)
                m
                (fold-left
                  (lambda (mm p) (%r-jolt-assoc1 mm (car p) (cdr p)))
                  m
                  (jrec-ext-pairs ext))))
          (let ((fk (vector-ref fkeys i)))
            (loop
              (+ i 1)
              (if (eq? fk drop-k)
                  m
                  (%r-jolt-assoc1 m fk (jrec-field-ref r i)))))))))

(define %r-jolt-dissoc jolt-dissoc)

(define %r-jolt-dissoc2 jolt-dissoc2)

(define (jrec-dissoc1 coll k)
  (if (not (jrec? coll))
      (%r-jolt-dissoc coll k)
      (let ((i (and (keyword? k) (jrec-field-index coll k))))
        (if i
            (jrec->map-without coll k)
            (let ((ext (jrec-ext coll)))
              (if (jolt-nil? ext)
                  coll
                  (let ((ne (%r-jolt-dissoc ext k)))
                    (make-jrec-from-existing
                      coll
                      #f
                      #f
                      (if (= 0 (jolt-count ne)) jolt-nil ne)))))))))

(set! jolt-dissoc
  (lambda (coll . ks)
    (cond
      ((jrec-cl coll "without") =>
       (lambda (m)
         (fold-left (lambda (c k) (jolt-invoke m c k)) coll ks)))
      ((jrec? coll) (fold-left jrec-dissoc1 coll ks))
      (else (apply %r-jolt-dissoc coll ks)))))

(set! jolt-dissoc2
  (lambda (coll k)
    (cond
      ((jrec-cl coll "without") =>
       (lambda (m) (jolt-invoke m coll k)))
      ((jrec? coll) (jrec-dissoc1 coll k))
      (else (%r-jolt-dissoc2 coll k)))))

(define (jrec-seq-col m which)
  (let loop ((s (jolt-seq m)) (acc '()))
    (if (jolt-nil? s)
        (list->cseq (reverse acc))
        (loop
          (jolt-seq (seq-more s))
          (cons (jolt-nth (seq-first s) which) acc)))))

(define %r-jolt-keys jolt-keys)

(set! jolt-keys
  (lambda (m)
    (if (jrec? m) (jrec-seq-col m 0) (%r-jolt-keys m))))

(define %r-jolt-vals jolt-vals)

(set! jolt-vals
  (lambda (m)
    (if (jrec? m) (jrec-seq-col m 1) (%r-jolt-vals m))))

(define (jrec-entry-list r)
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r)))
         (n (vector-length fkeys)))
    (let loop ((i 0) (acc '()))
      (if (= i n)
          (let ((ext (jrec-ext r)))
            (append
              (reverse acc)
              (if (jolt-nil? ext)
                  '()
                  (map (lambda (p) (make-map-entry (car p) (cdr p)))
                       (jrec-ext-pairs ext)))))
          (loop
            (+ i 1)
            (cons
              (make-map-entry (vector-ref fkeys i) (jrec-field-ref r i))
              acc))))))

(register-seq-arm!
  (lambda (x) (and (jrec? x) (jrec-declares-coll-iface? x)))
  (lambda (x) (jrec-abstract-method-error x "seq")))

(register-seq-arm!
  (lambda (x) (jrec-charseq-method x "charAt"))
  (lambda (x)
    (jrec-charseq->seq
      x
      (jrec-charseq-length-method x)
      (jrec-cl x "charAt"))))

(register-seq-arm!
  jrec-record?
  (lambda (x) (list->cseq (jrec-entry-list x))))

(register-seq-arm!
  (lambda (x) (jrec-cl x "seq"))
  (lambda (x) (jolt-seq (jolt-invoke (jrec-cl x "seq") x))))

(register-conj-arm!
  (lambda (coll) (jrec-cl coll "cons"))
  (lambda (coll x)
    (jolt-invoke (jrec-cl coll "cons") coll x)))

(register-conj-arm!
  (lambda (coll)
    (and (jrec? coll) (not (jrec-cl coll "cons"))))
  (lambda (coll x)
    (if (pmap? x)
        (pmap-fold-fwd x (lambda (k v c) (jolt-assoc1 c k v)) coll)
        (jolt-assoc1 coll (jolt-nth x 0) (jolt-nth x 1)))))

(register-empty-arm!
  jrec-collection?
  (lambda (coll) (jolt-nil? (jolt-seq coll))))

(define %r-jolt-peek jolt-peek)

(set! jolt-peek
  (lambda (coll)
    (cond
      ((jrec-cl coll "peek") => (lambda (m) (jolt-invoke m coll)))
      (else (%r-jolt-peek coll)))))

(define %r-jolt-pop jolt-pop)

(set! jolt-pop
  (lambda (coll)
    (cond
      ((jrec-cl coll "pop") => (lambda (m) (jolt-invoke m coll)))
      (else (%r-jolt-pop coll)))))

(register-pr-arm! jrec? jrec-pr)

(register-map-pred-arm! jrec-maplike?)

(def-var! "clojure.core" "map?" jolt-map?)

(def-var!
  "clojure.core"
  "coll?"
  (lambda (x) (or (jrec-collection? x) (jolt-coll-pred? x))))

(define proto-kw-jtype (keyword #f "jolt/type"))

(define proto-kw-protocol (keyword #f "jolt/protocol"))

(define proto-kw-name (keyword #f "name"))

(define (jolt-protocol-value? v)
  (and (pmap? v)
       (eq? (jolt-get v proto-kw-jtype jolt-nil)
            proto-kw-protocol)))

(define (protocol-value-key v)
  (and (jolt-protocol-value? v)
       (let ((n (jolt-get v proto-kw-name jolt-nil)))
         (cond
           ((symbol-t? n) (symbol-t-name n))
           ((string? n) n)
           (else #f)))))

(define (proto-str-index s ch)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond
        ((fx=? i n) #f)
        ((char=? (string-ref s i) ch) i)
        (else (loop (fx+ i 1)))))))

(define (proto-iface-name key)
  (let ((i (proto-str-index key #\/)))
    (jch-munge-segments
      (if i
          (string-append
            (substring key 0 i)
            "."
            (substring key (fx+ i 1) (string-length key)))
          key))))

(define (proto-key-qualified? key)
  (and (proto-str-index key #\/) #t))

(define (dotted-name? s) (and (proto-str-index s #\.) #t))

(define (proto-class-match? key qname)
  (let ((ki (proto-iface-name key))
        (qi (proto-iface-name qname)))
    (or (string=? ki qi)
        (and (or (not (dotted-name? ki)) (not (dotted-name? qi)))
             (string=? (jch-last-segment ki) (jch-last-segment qi))))))

(define type-registry (make-hashtable string-hash string=?))

(define type-method-index
  (make-hashtable string-hash string=?))

(define (tmi-add! type-tag proto method fn)
  (let* ((mi (or (hashtable-ref type-method-index type-tag #f)
                 (let ((h (make-hashtable string-hash string=?)))
                   (hashtable-set! type-method-index type-tag h)
                   h)))
         (entries (hashtable-ref mi method '())))
    (hashtable-set!
      mi
      method
      (if (assoc proto entries)
          (map (lambda (p)
                 (if (string=? (car p) proto) (cons proto fn) p))
               entries)
          (append entries (list (cons proto fn)))))))

(define (tmi-entries type-tag method)
  (let ((mi (hashtable-ref type-method-index type-tag #f)))
    (if mi (hashtable-ref mi method '()) '())))

(define (prune-type-registry! keep?)
  (vector-for-each
    (lambda (k)
      (unless (keep? k)
        (hashtable-delete! type-registry k)
        (hashtable-delete! type-method-index k)))
    (hashtable-keys type-registry))
  (vector-for-each
    (lambda (k)
      (unless (keep? k) (hashtable-delete! type-method-index k)))
    (hashtable-keys type-method-index))
  (vector-for-each
    (lambda (k)
      (unless (keep? k) (hashtable-delete! type-class-memo k)))
    (hashtable-keys type-class-memo)))

(define jolt-proto-epoch 0)

(define proto-method-keys
  (make-hashtable string-hash string=?))

(define (intern-pm-key proto method)
  (let* ((s (string-append
              proto
              (string (integer->char 0))
              method))
         (k (hashtable-ref proto-method-keys s #f)))
    (or k
        (jolt-with-mutex
          rec-tbl-mu
          (or (hashtable-ref proto-method-keys s #f)
              (let ((nk (gensym (string-append proto "." method))))
                (hashtable-set! proto-method-keys s nk)
                nk))))))

(define (find-protocol-method-desc desc proto method)
  (let ((pt (jrdesc-ptable desc)))
    (and pt
         (hashtable-ref pt (intern-pm-key proto method) #f))))

(define (register-protocol-method type-tag proto method fn)
  (jolt-with-mutex
    rec-tbl-mu
    (set! jolt-proto-epoch (fx+ jolt-proto-epoch 1))
    (let* ((ti (or (hashtable-ref type-registry type-tag #f)
                   (let ((h (make-hashtable string-hash string=?)))
                     (hashtable-set! type-registry type-tag h)
                     h)))
           (pi (or (hashtable-ref ti proto #f)
                   (let ((h (make-hashtable string-hash string=?)))
                     (hashtable-set! ti proto h)
                     h))))
      (hashtable-set! pi method fn)
      (tmi-add! type-tag proto method fn)))
  (let ((desc (hashtable-ref chez-tag-desc type-tag #f)))
    (when desc
      (let ((k (intern-pm-key proto method)))
        (jolt-with-mutex
          rec-tbl-mu
          (let ((pt (or (jrdesc-ptable desc)
                        (let ((h (make-eq-hashtable)))
                          (jrdesc-ptable-set! desc h)
                          h))))
            (hashtable-set! pt k fn))))))
  (remove-clone! type-tag proto method)
  (if #f #f))

(define (find-protocol-method type-tag proto method)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti
         (let ((pi (hashtable-ref ti proto #f)))
           (and pi (hashtable-ref pi method #f))))))

(define (find-method-any-protocol type-tag method)
  (let ((entries (tmi-entries type-tag method)))
    (and (pair? entries) (cdar entries))))

(define (proc-accepts? f n)
  (and (procedure? f)
       (bitwise-bit-set? (procedure-arity-mask f) n)))

(define (find-method-any-protocol-arity type-tag method
         nargs)
  (let ((entries (tmi-entries type-tag method)))
    (and (pair? entries)
         (let loop ((es entries))
           (cond
             ((null? es) (cdar entries))
             ((proc-accepts? (cdar es) nargs) (cdar es))
             (else (loop (cdr es))))))))

(define (type-satisfies? type-tag proto)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (hashtable-ref ti proto #f) #t)))

(define (type-implements-class?-uncached type-tag qname)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti
         (or (and (hashtable-ref ti qname #f) #t)
             (let* ((ks (jolt-with-mutex rec-tbl-mu (hashtable-keys ti)))
                    (n (vector-length ks)))
               (let loop ((i 0))
                 (and (fx< i n)
                      (or (proto-class-match? (vector-ref ks i) qname)
                          (loop (fx+ i 1))))))))))

(define type-class-memo
  (make-hashtable string-hash string=?))

(define type-class-memo-mu (make-mutex))

(define (type-implements-class? type-tag qname)
  (let* ((epoch (jrdesc-ifc-epoch))
         (inner (hashtable-ref type-class-memo type-tag #f))
         (hit (and inner (hashtable-ref inner qname #f))))
    (if (and hit (fx=? (car hit) epoch))
        (cdr hit)
        (let ((v (type-implements-class?-uncached type-tag qname)))
          (jolt-with-mutex
            type-class-memo-mu
            (let ((i2 (or (hashtable-ref type-class-memo type-tag #f)
                          (let ((h (make-hashtable string-hash string=?)))
                            (hashtable-set! type-class-memo type-tag h)
                            h))))
              (hashtable-set! i2 qname (cons epoch v))))
          v))))

(def-var!
  "jolt.host"
  "jrec-method?"
  (lambda (v name)
    (cond
      ((jrec? v)
       (if (find-method-any-protocol (jrec-tag v) name) #t #f))
      ((reified-methods v) =>
       (lambda (m) (if (hashtable-ref m name #f) #t #f)))
      (else #f))))

(set-str-tostring-hook!
  (lambda (v)
    (and (jrec? v)
         (find-method-any-protocol (jrec-tag v) "toString")
         (record-method-dispatch v "toString" jolt-nil))))

(define jrec-record-iface-tags
  '("IRecord" "clojure.lang.IRecord" "IPersistentMap"
     "clojure.lang.IPersistentMap" "APersistentMap" "Associative"
     "ILookup" "Seqable" "Counted" "IPersistentCollection" "IObj"
     "IMeta" "Map" "java.util.Map" "Iterable"
     "java.lang.Iterable" "Object"))

(define (jch-tags-sans-object ts)
  (if (null? (cdr ts))
      '()
      (cons (car ts) (jch-tags-sans-object (cdr ts)))))

(define (jreify-host-tags obj)
  (let loop ((ps (jreify-protos obj)) (acc '()))
    (if (null? ps)
        (reverse (cons "Object" acc))
        (let inner ((ts (jch-tags (proto-iface-name (car ps))))
                    (acc acc))
          (if (null? ts)
              (loop (cdr ps) acc)
              (inner
                (cdr ts)
                (let ((t (car ts)))
                  (if (or (string=? t "Object") (member t acc))
                      acc
                      (cons t acc)))))))))

(define (value-host-tags obj)
  (cond
    ((flonum? obj) '("Double" "Float" "Number" "Object"))
    ((and (number? obj) (exact? obj) (not (integer? obj)))
     '("Ratio" "Number" "Object"))
    ((and (number? obj) (exact? obj) (integer? obj))
     (if (jolt-bigint-print? obj)
         '("BigInt" "BigInteger" "Number" "Object")
         '("Long" "Integer" "Number" "Object")))
    ((number? obj) '("Number" "Object"))
    ((string? obj) '("String" "CharSequence" "Object"))
    ((boolean? obj) '("Boolean" "Object"))
    ((char? obj) (jch-tags "java.lang.Character"))
    ((keyword? obj) (jch-tags "clojure.lang.Keyword"))
    ((jolt-symbol? obj) (jch-tags "clojure.lang.Symbol"))
    ((jolt-map-entry? obj) (jch-tags "clojure.lang.MapEntry"))
    ((jolt-subvec-view? obj)
     (jch-tags "clojure.lang.APersistentVector$SubVector"))
    ((pvec? obj) (jch-tags "clojure.lang.PersistentVector"))
    ((pmap? obj)
     (if (pmap-array? obj)
         (jch-tags "clojure.lang.PersistentArrayMap")
         (jch-tags "clojure.lang.PersistentHashMap")))
    ((pset? obj) (jch-tags "clojure.lang.PersistentHashSet"))
    ((cseq? obj) (jch-tags (cseq-class-name obj)))
    ((empty-list-t? obj)
     (jch-tags "clojure.lang.PersistentList$EmptyList"))
    ((jolt-lazyseq? obj) (jch-tags "clojure.lang.LazySeq"))
    ((var-cell? obj) (jch-tags "clojure.lang.Var"))
    ((jclass? obj) '("Class" "java.lang.Class" "Object"))
    ((and (procedure? obj) (deftype-ctor-tag obj))
     '("Class" "java.lang.Class" "Object"))
    ((and (procedure? obj) (hashtable-ref proc-name-tbl obj #f)) =>
     (lambda (p)
       (cons
         (string-append
           (class-munge-name (car p))
           "$"
           (class-munge-name (cdr p)))
         (jch-tags "clojure.lang.AFunction"))))
    ((and (jhost? obj) (jhost-value-tags (jhost-tag obj))) =>
     (lambda (tags) tags))
    ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'byte))
     '("[B" "Object"))
    ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'char))
     '("[C" "Object"))
    ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'int))
     '("[I" "Object"))
    ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'long))
     '("[J" "Object"))
    ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'double))
     '("[D" "Object"))
    ((jolt-array? obj) '("[Ljava.lang.Object;" "Object"))
    ((regex-t? obj) (jch-tags "java.util.regex.Pattern"))
    ((juuid? obj) (jch-tags "java.util.UUID"))
    ((jinst? obj) (jch-tags "java.util.Date"))
    ((jbigdec? obj) (jch-tags "java.math.BigDecimal"))
    ((procedure? obj) (jch-tags "clojure.lang.AFunction"))
    ((jolt-nil? obj) '("nil"))
    ((jreify? obj) (jreify-host-tags obj))
    ((jrec-record? obj)
     (let* ((tag (jrec-tag obj)) (extra (cddr (jch-tags tag))))
       (cons
         tag
         (if (null? (cdr extra))
             jrec-record-iface-tags
             (append
               (jch-tags-sans-object extra)
               jrec-record-iface-tags)))))
    ((jrec? obj)
     (let ((tag (jrec-tag obj)))
       (cons tag (cddr (jch-tags tag)))))
    ((jolt-ex-info-record? obj)
     (jch-tags (jolt-ex-info-record-class-name obj)))
    ((jolt-atom? obj) (jch-tags "clojure.lang.Atom"))
    ((jns? obj) (jch-tags "clojure.lang.Namespace"))
    ((let ((n (jolt-class-name obj)))
       (and (string? n) (jch-known-exact? n) n)) =>
     jch-tags)
    (else '("Object"))))

(define (jrec-assoc-entries r ext)
  (let loop ((s (jolt-seq ext)) (r r))
    (if (jolt-nil? s)
        r
        (let ((e (seq-first s)))
          (loop
            (jolt-seq (seq-more s))
            (jolt-assoc r (jolt-nth e 0) (jolt-nth e 1)))))))

(define (make-deftype-ctor name-sym field-kws . rest-args)
  (let* ((tag (string-append
                (chez-current-ns)
                "."
                (symbol-t-name name-sym)))
         (kws (seq->list field-kws))
         (field-tags (if (pair? rest-args)
                         (seq->list (car rest-args))
                         '()))
         (dbl-flags (list->vector (map chez-double-tag? field-tags)))
         (ndbl (vector-length dbl-flags))
         (desc (make-jrdesc tag kws))
         (_ (jolt-with-mutex
              rec-tbl-mu
              (let ((old-desc (hashtable-ref chez-tag-desc tag #f)))
                (when old-desc (jrdesc-ptable-set! old-desc #f)))
              (hashtable-set! chez-tag-desc tag desc)))
         (nf (length kws))
         (ctor-name (string-append
                      (chez-current-ns)
                      "/->"
                      (symbol-t-name name-sym)))
         (build (lambda (args)
                  (let ((v (make-vector nf jolt-nil)))
                    (let loop ((as args) (i 0))
                      (if (or (null? as) (fx=? i nf))
                          (make-jrec desc v jolt-nil)
                          (let ((a (car as)))
                            (vector-set!
                              v
                              i
                              (if (and (fx< i ndbl)
                                       (vector-ref dbl-flags i)
                                       (number? a)
                                       (not (flonum? a)))
                                  (exact->inexact a)
                                  a))
                            (loop (cdr as) (+ i 1))))))))
         (ctor (lambda args
                 (let ((n (length args)))
                   (cond
                     ((= n nf) (build args))
                     ((and (= n (+ nf 2))
                           (hashtable-ref chez-record-type-tbl tag #f))
                      (let* ((r (build args))
                             (m (list-ref args nf))
                             (ext (list-ref args (+ nf 1)))
                             (r (if (jolt-nil? ext)
                                    r
                                    (jrec-assoc-entries r ext))))
                        (if (jolt-nil? m) r (jolt-with-meta r m))))
                     (else
                      (throw-jvm
                        'ArityException
                        (string-append
                          "Wrong number of args ("
                          (number->string n)
                          ") passed to: "
                          ctor-name))))))))
    (register-class-ctor! tag ctor)
    (when (or (not (hashtable-ref
                     class-ctors-tbl
                     (symbol-t-name name-sym)
                     #f))
              (hashtable-ref
                chez-simple-name-tag
                (symbol-t-name name-sym)
                #f))
      (register-class-ctor! (symbol-t-name name-sym) ctor))
    (jolt-with-mutex
      rec-tbl-mu
      (hashtable-set! chez-deftype-tag-set tag #t)
      (hashtable-set!
        chez-simple-name-tag
        (symbol-t-name name-sym)
        tag))
    (jch-set-supers! tag '("clojure.lang.IType"))
    (deftype-ctor-tag-set! ctor tag)
    (register-record-shape!
      (string-append
        (chez-current-ns)
        "/->"
        (symbol-t-name name-sym))
      kws
      field-tags
      tag)
    ctor))

(define (make-protocol name-str methods)
  (jolt-hash-map (keyword #f "jolt/type") (keyword #f "jolt/protocol")
    (keyword #f "name") (jolt-symbol jolt-nil name-str)
    (keyword #f "methods") methods))

(define (register-protocol-methods! proto-name method-names)
  (let ((ns (chez-current-ns)))
    (for-each
      (lambda (mn)
        (let ((m (if (symbol-t? mn) (symbol-t-name mn) mn)))
          (jolt-with-mutex
            rec-tbl-mu
            (hashtable-set!
              chez-protocol-methods-tbl
              (string-append ns "/" m)
              (cons proto-name m)))))
      (seq->list method-names)))
  jolt-nil)

(define host-type-set
  (let ((h (make-hashtable string-hash string=?)))
    (for-each
      (lambda (n) (hashtable-set! h n #t))
      '("Long" "Integer" "Number" "Double" "Ratio" "BigInt"
        "BigInteger" "String" "CharSequence" "Boolean" "Character"
        "Keyword" "Symbol" "Named" "Object" "nil" "Fn" "IFn" "AFn"
        "URI" "Var" "IDeref" "PersistentVector" "APersistentVector"
        "IPersistentVector" "PersistentArrayMap" "APersistentMap"
        "IPersistentMap" "PersistentHashSet" "APersistentSet"
        "IPersistentSet" "ASeq" "ISeq" "IPersistentCollection"
        "Associative" "Sequential" "PersistentList"
        "IPersistentList" "IPersistentStack" "Map" "java.util.Map"
        "List" "java.util.List" "Set" "java.util.Set" "Collection"
        "java.util.Collection" "Iterable" "java.lang.Iterable"
        "UUID" "BigDecimal" "Date" "Timestamp" "Instant"
        "java.sql.Date" "Pattern" "java.util.regex.Pattern"
        "Duration" "Period" "LocalDate" "LocalTime" "LocalDateTime"
        "ZonedDateTime" "OffsetDateTime" "OffsetTime" "ZoneId"
        "ZoneOffset" "Clock" "Year" "YearMonth" "Month" "DayOfWeek"
        "ChronoUnit" "ChronoField" "TemporalAmount" "TemporalUnit"
        "TemporalField" "ByteBuffer" "java.nio.ByteBuffer" "[B" "[C"
        "[I" "[J" "[D" "[Ljava.lang.Object;" "Reader"
        "java.io.Reader" "Writer" "java.io.Writer" "StringReader"
        "java.io.StringReader" "PushbackReader"
        "java.io.PushbackReader" "BufferedReader"
        "java.io.BufferedReader" "FilterReader"
        "java.io.FilterReader" "InputStream" "java.io.InputStream"
        "OutputStream" "java.io.OutputStream"))
    h))

(define (strip-prefix s p)
  (let ((pl (string-length p)))
    (and (> (string-length s) pl)
         (string=? (substring s 0 pl) p)
         (substring s pl (string-length s)))))

(define (canonical-host-tag type-name)
  (let ((base (or (strip-prefix type-name "java.lang.")
                  (strip-prefix type-name "java.util.regex.")
                  (strip-prefix type-name "java.util.")
                  (strip-prefix type-name "java.net.")
                  (strip-prefix type-name "java.math.")
                  (strip-prefix type-name "java.time.")
                  (strip-prefix type-name "clojure.lang.")
                  type-name)))
    (cond
      ((let loop ((i 0))
         (cond
           ((fx>=? i (string-length type-name)) #f)
           ((char=? (string-ref type-name i) #\$) #t)
           (else (loop (fx+ i 1)))))
       type-name)
      ((hashtable-ref host-type-set base #f) base)
      ((and (not (hashtable-ref
                   chez-simple-name-tag
                   type-name
                   #f))
            (not (hashtable-ref chez-deftype-tag-set type-name #f))
            (or (jch-known? base) (jch-known? type-name)))
       (jch-last-segment type-name))
      ((dotted-name? type-name) type-name)
      (else #f))))

(define extend-mark "__jolt_extend__")

(define (mark-extend! tag proto-name)
  (jolt-with-mutex
    rec-tbl-mu
    (let ((ti (hashtable-ref type-registry tag #f)))
      (when ti
        (let ((pi (hashtable-ref ti proto-name #f)))
          (when pi (hashtable-set! pi extend-mark #t)))))))

(define (register-method type-name proto-name method-name
         fn)
  (let* ((host (canonical-host-tag type-name))
         (local (string-append (chez-current-ns) "." type-name))
         (tag (cond
                (host host)
                ((hashtable-ref chez-deftype-tag-set local #f) local)
                ((hashtable-ref chez-deftype-tag-set type-name #f)
                 type-name)
                ((hashtable-ref chez-simple-name-tag type-name #f))
                (else local))))
    (register-protocol-method tag proto-name method-name fn)
    (mark-extend! tag proto-name)
    jolt-nil))

(define (register-inline-method type-name proto-name
         method-name fn)
  (register-protocol-method
    (string-append (chez-current-ns) "." type-name)
    proto-name
    method-name
    fn)
  jolt-nil)

(define (register-inline-protocol! type-name proto-name)
  (let ((tag (string-append (chez-current-ns) "." type-name)))
    (jolt-with-mutex
      rec-tbl-mu
      (let ((ti (or (hashtable-ref type-registry tag #f)
                    (let ((h (make-hashtable string-hash string=?)))
                      (hashtable-set! type-registry tag h)
                      h))))
        (unless (hashtable-ref ti proto-name #f)
          (hashtable-set!
            ti
            proto-name
            (make-hashtable string-hash string=?))))))
  (let ((iface (cond
                 ((proto-key-qualified? proto-name)
                  (proto-iface-name proto-name))
                 ((dotted-name? proto-name) proto-name)
                 (else
                  (let ((fqn (jch-fqn-of-simple proto-name)))
                    (if (string=? fqn proto-name)
                        (string-append
                          (jch-munge-segments (chez-current-ns))
                          "."
                          proto-name)
                        fqn))))))
    (jch-mark-interface! iface)
    (jch-register-supers!
      (string-append (chez-current-ns) "." type-name)
      (list iface)))
  jolt-nil)

(define (protocol-resolve proto-name method-name obj)
  (cond
    ((and (jrec? obj)
          (let* ((desc (jrec-desc obj))
                 (f (find-protocol-method-desc
                      desc
                      proto-name
                      method-name)))
            (or f
                (find-protocol-method
                  (jrdesc-tag desc)
                  proto-name
                  method-name)))))
    ((reified-methods obj) =>
     (lambda (rm)
       (or (hashtable-ref rm method-name #f)
           (let loop ((tags (value-host-tags obj)))
             (cond
               ((null? tags)
                (throw-jvm
                  'IllegalArgumentException
                  (string-append "No reified method " method-name)))
               ((find-protocol-method (car tags) proto-name method-name))
               (else (loop (cdr tags))))))))
    (else
     (let loop ((tags (value-host-tags obj)))
       (cond
         ((null? tags)
          (throw-jvm
            'IllegalArgumentException
            (string-append "No method " method-name " in " proto-name)))
         ((find-protocol-method (car tags) proto-name method-name))
         (else (loop (cdr tags))))))))

(define (protocol-dispatch1 proto-name method-name obj)
  ((protocol-resolve proto-name method-name obj) obj))

(define (protocol-dispatch2 proto-name method-name obj a)
  ((protocol-resolve proto-name method-name obj) obj a))

(define (protocol-dispatch3 proto-name method-name obj a b)
  ((protocol-resolve proto-name method-name obj) obj a b))

(define (protocol-dispatch proto-name method-name obj
         rest-args)
  (let ((rest (if (jolt-nil? rest-args)
                  '()
                  (seq->list rest-args))))
    (apply
      (protocol-resolve proto-name method-name obj)
      obj
      rest)))

(define jolt-pic-n 4)

(define (jolt-pic-make)
  (let ((v (make-vector (+ (* jolt-pic-n 2) 2) #f)))
    (vector-set! v (* jolt-pic-n 2) 0)
    (vector-set! v (+ (* jolt-pic-n 2) 1) -1)
    v))

(define (jolt-pic-install v d proto method obj)
  (let ((f (protocol-resolve proto method obj)))
    (when d
      (let ((slot (* (vector-ref v (* jolt-pic-n 2)) 2)))
        (vector-set! v slot d)
        (vector-set! v (fx+ slot 1) f)
        (vector-set!
          v
          (* jolt-pic-n 2)
          (if (fx= (vector-ref v (* jolt-pic-n 2)) (fx- jolt-pic-n 1))
              0
              (fx+ (vector-ref v (* jolt-pic-n 2)) 1)))))
    f))

(define (jolt-pic-rebuild v d proto method obj)
  (let ((f (protocol-resolve proto method obj)))
    (when d
      (let loop ((i 0))
        (when (fx< i (* jolt-pic-n 2))
          (vector-set! v i #f)
          (loop (fx+ i 1))))
      (vector-set! v 0 d)
      (vector-set! v 1 f)
      (vector-set! v (* jolt-pic-n 2) 1)
      (vector-set! v (+ (* jolt-pic-n 2) 1) jolt-proto-epoch))
    f))

(define (devirt-resolve type-tag proto-name method-name obj)
  (or (find-protocol-method type-tag proto-name method-name)
      (protocol-resolve proto-name method-name obj)))

(define clone-registry
  (make-hashtable string-hash string=?))

(define (register-clone type-tag proto method fn)
  (jolt-with-mutex
    rec-tbl-mu
    (let* ((ti (or (hashtable-ref clone-registry type-tag #f)
                   (let ((h (make-hashtable string-hash string=?)))
                     (hashtable-set! clone-registry type-tag h)
                     h)))
           (pi (or (hashtable-ref ti proto #f)
                   (let ((h (make-hashtable string-hash string=?)))
                     (hashtable-set! ti proto h)
                     h))))
      (hashtable-set! pi method fn)
      jolt-nil)))

(define (register-clone* type-name proto method fn)
  (register-clone
    (string-append (chez-current-ns) "." type-name)
    proto
    method
    fn))

(define (find-clone type-tag proto method)
  (let ((ti (hashtable-ref clone-registry type-tag #f)))
    (and ti
         (let ((pi (hashtable-ref ti proto #f)))
           (and pi (hashtable-ref pi method #f))))))

(define (remove-clone! type-tag proto method)
  (jolt-with-mutex
    rec-tbl-mu
    (let ((ti (hashtable-ref clone-registry type-tag #f)))
      (when ti
        (let ((pi (hashtable-ref ti proto #f)))
          (when pi (hashtable-delete! pi method)))))))

(define (devirt-resolve-fl type-tag proto-name method-name
         obj)
  (or (find-clone type-tag proto-name method-name)
      (devirt-resolve type-tag proto-name method-name obj)))

(define (jrec-comparable-method v)
  (and (jrec? v)
       (find-method-any-protocol (jrec-tag v) "compareTo")))

(register-compare-arm!
  (lambda (a b) (and (jrec-comparable-method a) #t))
  (lambda (a b)
    (jnum->exact (jolt-invoke (jrec-comparable-method a) a b))))

(define-record-type jiterator
  (fields (mutable cur))
  (nongenerative jolt-iterator-v1))

(register-seq-arm! jiterator? jiterator-cur)

(define (condition->message-string c)
  (if (message-condition? c)
      (let* ((m (condition-message c))
             (irr (if (irritants-condition? c)
                      (condition-irritants c)
                      '()))
             (irr (if (list? irr) irr '()))
             (who (and (who-condition? c) (condition-who c)))
             (text (if (string? m)
                       (condition-template-fill m irr)
                       (with-output-to-string (lambda () (display m))))))
        (cond
          ((symbol? who)
           (string-append (symbol->string who) ": " text))
          ((string? who) (string-append who ": " text))
          (else text)))
      (with-output-to-string (lambda () (display-condition c)))))

(define (condition-directive m i)
  (let* ((n (string-length m))
         (j (if (and (fx<? (fx+ i 1) n)
                     (char=? (string-ref m (fx+ i 1)) #\:))
                (fx+ i 2)
                (fx+ i 1)))
         (d (and (fx<? j n) (char-downcase (string-ref m j)))))
    (cond
      ((memv d '(#\s #\a)) (cons (fx+ j 1) d))
      ((eqv? d #\~) (cons (fx+ j 1) #\~))
      (else (cons (fx+ j 1) #f)))))

(define (condition-append-irritants s irr)
  (let loop ((xs irr) (acc s))
    (if (null? xs)
        acc
        (loop
          (cdr xs)
          (string-append
            acc
            " "
            (condition-irritant-string (car xs) #t))))))

(define (condition-template-fill m irr)
  (let ((n (string-length m)))
    (let scan ((i 0) (simple? #t))
      (cond
        ((fx>=? i n)
         (if simple?
             (condition-fill-simple m irr)
             (guard (e (#t (condition-append-irritants m irr)))
               (apply format m irr))))
        ((char=? (string-ref m i) #\~)
         (let ((d (condition-directive m i)))
           (scan (car d) (and simple? (cdr d) #t))))
        (else (scan (fx+ i 1) simple?))))))

(define (condition-irritant-string x readable?)
  (let ((s (if readable?
               (jolt-pr-readable x)
               (jolt-str-render-one x))))
    (if (string=? s "#object[:object]")
        (with-output-to-string (lambda () (write x)))
        s)))

(define (condition-fill-simple m irr)
  (let ((n (string-length m)))
    (let loop ((i 0) (start 0) (irr irr) (acc '()))
      (cond
        ((fx>=? i n)
         (condition-append-irritants
           (apply
             string-append
             (reverse (cons (substring m start n) acc)))
           irr))
        ((char=? (string-ref m i) #\~)
         (let* ((d (condition-directive m i))
                (next (car d))
                (kind (cdr d))
                (acc (cons (substring m start i) acc)))
           (cond
             ((eqv? kind #\~) (loop next next irr (cons "~" acc)))
             ((null? irr) (loop next next irr acc))
             (else
              (loop
                next
                next
                (cdr irr)
                (cons
                  (condition-irritant-string
                    (car irr)
                    (not (eqv? kind #\a)))
                  acc))))))
        (else (loop (fx+ i 1) start irr acc))))))

(def-var!
  "jolt.host"
  "condition-message"
  (lambda (c)
    (if (condition? c) (condition->message-string c) jolt-nil)))

(define rd-class-method-hook #f)

(define (set-rd-class-method-hook! f)
  (set! rd-class-method-hook f))

(define (rd-persistent-coll? obj)
  (or (pvec? obj)
      (pset? obj)
      (cseq? obj)
      (empty-list-t? obj)
      (jolt-lazyseq? obj)))

(define (rd-coll-last obj)
  (if (pvec? obj)
      (jolt-nth obj (fx- (jolt-count obj) 1))
      (let loop ((s (jolt-seq obj)))
        (let ((n (jolt-seq (seq-more s))))
          (if (jolt-nil? n) (seq-first s) (loop n))))))

(define rd-java-util-mutator-names
  '("add" "addAll" "addFirst" "addLast" "clear" "remove" "removeAll"
     "removeFirst" "removeLast" "removeIf" "replaceAll"
     "retainAll" "set" "sort"))

(define (rd-java-util-mutator? m)
  (and (member m rd-java-util-mutator-names) #t))

(define (record-method-dispatch-base obj method-name
         rest-args)
  (let ((rest (if (jolt-nil? rest-args)
                  '()
                  (seq->list rest-args))))
    (cond
      ((and (procedure? obj) (deftype-ctor-tag obj)) =>
       (lambda (tag)
         (let ((hit (and rd-class-method-hook
                         (rd-class-method-hook tag method-name rest))))
           (if (pair? hit)
               (car hit)
               (cond
                 ((or (string=? method-name "getName")
                      (string=? method-name "getCanonicalName")
                      (string=? method-name "getTypeName"))
                  tag)
                 ((string=? method-name "getSimpleName") (last-dot tag))
                 ((string=? method-name "toString")
                  (string-append "class " tag))
                 (else (dispatch-miss obj method-name rest)))))))
      ((jolt-multifn? obj)
       (cond
         ((string=? method-name "addMethod")
          (jolt-with-mutex
            mm-tbl-mu
            (hashtable-set!
              (jolt-multifn-methods obj)
              (car rest)
              (cadr rest))
            (set! jolt-mm-epoch (fx+ jolt-mm-epoch 1)))
          obj)
         ((string=? method-name "removeMethod")
          (jolt-with-mutex
            mm-tbl-mu
            (hashtable-delete! (jolt-multifn-methods obj) (car rest))
            (set! jolt-mm-epoch (fx+ jolt-mm-epoch 1)))
          obj)
         ((string=? method-name "getMethod")
          (or (hashtable-ref (jolt-multifn-methods obj) (car rest) #f)
              jolt-nil))
         ((string=? method-name "getMethodTable")
          (let* ((tbl (jolt-multifn-methods obj))
                 (kv (jolt-with-mutex
                       mm-tbl-mu
                       (let-values (((ks vs) (hashtable-entries tbl)))
                         (cons ks vs))))
                 (ks (car kv))
                 (vs (cdr kv)))
            (let loop ((i 0) (m (jolt-hash-map)))
              (if (fx>=? i (vector-length ks))
                  m
                  (loop
                    (fx+ i 1)
                    (jolt-assoc1
                      m
                      (vector-ref ks i)
                      (vector-ref vs i)))))))
         ((string=? method-name "toString")
          (jolt-str-render-one obj))
         (else (dispatch-miss obj method-name rest))))
      ((and (jrec? obj)
            (find-method-any-protocol-arity
              (jrec-tag obj)
              method-name
              (+ 1 (length rest)))) =>
       (lambda (f) (apply jolt-invoke f obj rest)))
      ((and (jrec? obj)
            (null? rest)
            (jrec-has? obj (keyword #f method-name)))
       (jrec-lookup obj (keyword #f method-name) jolt-nil))
      ((and (jrec-record? obj)
            (member
              method-name
              '("valAt" "assoc" "without" "containsKey" "cons" "count"
                 "seq" "equiv" "entryAt" "empty")))
       (cond
         ((string=? method-name "valAt")
          (if (null? (cdr rest))
              (jolt-get obj (car rest) jolt-nil)
              (jolt-get obj (car rest) (cadr rest))))
         ((string=? method-name "assoc")
          (jolt-assoc1 obj (car rest) (cadr rest)))
         ((string=? method-name "without")
          (jolt-dissoc obj (car rest)))
         ((string=? method-name "containsKey")
          (if (jolt-truthy? (jolt-contains? obj (car rest))) #t #f))
         ((string=? method-name "cons") (jolt-conj1 obj (car rest)))
         ((string=? method-name "count") (jolt-count obj))
         ((string=? method-name "seq") (jolt-seq obj))
         ((string=? method-name "equiv")
          (if (jolt= obj (car rest)) #t #f))
         ((string=? method-name "entryAt")
          (if (jolt-truthy? (jolt-contains? obj (car rest)))
              (make-map-entry
                (car rest)
                (jolt-get obj (car rest) jolt-nil))
              jolt-nil))
         (else jolt-nil)))
      ((reified-methods obj) =>
       (lambda (rm)
         (let ((f (hashtable-ref rm method-name #f))
               (d (jreify-delegate obj)))
           (cond
             (f (apply jolt-invoke f obj rest))
             (d (record-method-dispatch d method-name rest-args))
             (else (dispatch-miss obj method-name rest))))))
      ((string? obj) (jolt-string-method method-name obj rest))
      ((jiterator? obj)
       (cond
         ((string=? method-name "hasNext")
          (not (jolt-nil? (jolt-seq (jiterator-cur obj)))))
         ((string=? method-name "next")
          (let ((s (jolt-seq (jiterator-cur obj))))
            (if (jolt-nil? s)
                (throw-jvm 'NoSuchElementException "iterator exhausted")
                (let ((v (jolt-first s)))
                  (jiterator-cur-set! obj (jolt-rest s))
                  v))))
         (else (dispatch-miss obj method-name rest))))
      ((string=? method-name "iterator")
       (make-jiterator (jolt-seq obj)))
      ((keyword-t? obj)
       (cond
         ((string=? method-name "sym")
          (jolt-symbol (keyword-t-ns obj) (keyword-t-name obj)))
         ((string=? method-name "getName") (keyword-t-name obj))
         ((string=? method-name "getNamespace")
          (or (keyword-t-ns obj) jolt-nil))
         ((string=? method-name "toString")
          (string-append
            ":"
            (if (keyword-t-ns obj)
                (string-append (keyword-t-ns obj) "/")
                "")
            (keyword-t-name obj)))
         ((string=? method-name "hashCode")
          (jolt-s32
            (+ (java-symbol-hash
                 (keyword-t-name obj)
                 (keyword-t-ns obj))
               2654435769)))
         ((string=? method-name "equals")
          (and (pair? rest) (eq? obj (car rest))))
         (else (dispatch-miss obj method-name rest))))
      ((symbol-t? obj)
       (cond
         ((string=? method-name "getName") (symbol-t-name obj))
         ((string=? method-name "getNamespace")
          (or (symbol-t-ns obj) jolt-nil))
         ((string=? method-name "toString")
          (string-append
            (if (symbol-t-ns obj)
                (string-append (symbol-t-ns obj) "/")
                "")
            (symbol-t-name obj)))
         ((string=? method-name "equals")
          (and (pair? rest) (jolt=2 obj (car rest))))
         ((string=? method-name "hashCode")
          (java-symbol-hash (symbol-t-name obj) (symbol-t-ns obj)))
         (else (dispatch-miss obj method-name rest))))
      ((jns? obj)
       (cond
         ((or (string=? method-name "name")
              (string=? method-name "getName"))
          (jolt-symbol #f (jns-name obj)))
         ((string=? method-name "toString") (jns-name obj))
         (else (dispatch-miss obj method-name rest))))
      ((var-cell? obj)
       (cond
         ((string=? method-name "ns") (intern-ns! (var-cell-ns obj)))
         ((or (string=? method-name "sym")
              (string=? method-name "name"))
          (jolt-symbol #f (var-cell-name obj)))
         ((string=? method-name "getName")
          (jolt-symbol (var-cell-ns obj) (var-cell-name obj)))
         ((string=? method-name "toString")
          (string-append
            "#'"
            (var-cell-ns obj)
            "/"
            (var-cell-name obj)))
         ((string=? method-name "getRawRoot") (var-cell-root obj))
         (else (dispatch-miss obj method-name rest))))
      ((condition? obj)
       (cond
         ((throwable-method obj method-name rest) => car)
         (else (dispatch-miss obj method-name rest))))
      ((char? obj)
       (cond
         ((string=? method-name "toString") (string obj))
         ((string=? method-name "charValue") obj)
         ((string=? method-name "hashCode") (char->integer obj))
         ((string=? method-name "equals")
          (and (pair? rest)
               (char? (car rest))
               (char=? obj (car rest))))
         ((string=? method-name "compareTo")
          (let ((o (car rest)))
            (cond ((char<? obj o) -1) ((char>? obj o) 1) (else 0))))
         (else (dispatch-miss obj method-name rest))))
      ((and (string=? method-name "getFirst")
            (rd-persistent-coll? obj))
       (let ((s (jolt-seq obj)))
         (if (jolt-nil? s)
             (throw-jvm 'NoSuchElementException "")
             (seq-first s))))
      ((and (string=? method-name "getLast")
            (rd-persistent-coll? obj))
       (if (jolt-nil? (jolt-seq obj))
           (throw-jvm 'NoSuchElementException "")
           (rd-coll-last obj)))
      ((and (string=? method-name "reversed")
            (rd-persistent-coll? obj))
       (let ((items (reverse (seq->list (jolt-seq obj)))))
         (if (pvec? obj)
             (apply jolt-vector items)
             (list->cseq items))))
      ((and (rd-java-util-mutator? method-name)
            (rd-persistent-coll? obj))
       (throw-jvm 'UnsupportedOperationException ""))
      ((or (string=? method-name "indexOf")
           (string=? method-name "lastIndexOf"))
       (let ((target (car rest))
             (last? (string=? method-name "lastIndexOf")))
         (let loop ((s (jolt-seq obj)) (i 0) (found -1))
           (cond
             ((jolt-nil? s) found)
             ((jolt=2 (seq-first s) target)
              (if last? (loop (jolt-seq (seq-more s)) (fx+ i 1) i) i))
             (else (loop (jolt-seq (seq-more s)) (fx+ i 1) found))))))
      ((string=? method-name "contains")
       (let ((target (car rest)))
         (let loop ((s (jolt-seq obj)))
           (cond
             ((jolt-nil? s) #f)
             ((jolt=2 (seq-first s) target) #t)
             (else (loop (jolt-seq (seq-more s))))))))
      ((string=? method-name "toString")
       (jolt-str-render-one obj))
      ((string=? method-name "hashCode") (jolt-hash obj))
      ((string=? method-name "equals")
       (and (pair? rest) (if (jolt= obj (car rest)) #t #f)))
      ((string=? method-name "__methodImplCache") jolt-nil)
      ((jrec? obj)
       (let ((boxed (dot-coll-method obj method-name rest)))
         (if boxed
             (car boxed)
             (dispatch-miss obj method-name rest))))
      (else (dispatch-miss obj method-name rest)))))

(define class-ext-fallback-hook #f)

(define (set-class-ext-fallback-hook! f)
  (set! class-ext-fallback-hook f))

(define (dispatch-miss obj method-name args)
  (let ((f (and class-ext-fallback-hook
                (class-ext-fallback-hook obj method-name))))
    (if f
        (apply jolt-invoke f obj args)
        (no-method-throw method-name obj (length args)))))

(define (no-method-throw method-name obj . maybe-argc)
  (let* ((argc (if (null? maybe-argc) 0 (car maybe-argc)))
         (dashed? (and (> (string-length method-name) 1)
                       (char=? (string-ref method-name 0) #\-)))
         (bare (if dashed?
                   (substring method-name 1 (string-length method-name))
                   method-name)))
    (cond
      ((jolt-nil? obj)
       (throw-jvm
         'NullPointerException
         (string-append
           "Cannot invoke \""
           method-name
           "\" because the target is null")))
      ((or dashed? (fx=? argc 0))
       (throw-jvm
         'IllegalArgumentException
         (string-append
           "No matching field found: "
           bare
           " for class "
           (guard (e (#t "?")) (jolt-class-name obj)))))
      (else
       (throw-jvm
         'IllegalArgumentException
         (string-append "No matching method " method-name " found taking "
           (number->string argc) " args for class "
           (guard (e (#t "?")) (jolt-class-name obj))))))))

(define method-dispatch-arms '())

(define (register-method-arm! priority arm)
  (set! method-dispatch-arms
    (let ins ((as method-dispatch-arms))
      (cond
        ((null? as) (list (cons priority arm)))
        ((< priority (caar as)) (cons (cons priority arm) as))
        (else (cons (car as) (ins (cdr as))))))))

(define arm-priority-user-override 1)

(define arm-priority-getclass 5)

(define arm-priority-string 6)

(define arm-priority-dotform 30)

(define arm-priority-date 40)

(define arm-priority-file 41)

(define arm-priority-regex 42)

(define arm-priority-nio-path 42)

(define arm-priority-htable 43)

(define arm-priority-host-type 44)

(define (record-method-dispatch obj method-name rest-args)
  (let loop ((as method-dispatch-arms))
    (if (null? as)
        (record-method-dispatch-base obj method-name rest-args)
        (let ((r ((cdar as) obj method-name rest-args)))
          (if (eq? r 'pass) (loop (cdr as)) r)))))

(define (method-rest-args->list rest-args)
  (cond
    ((jolt-nil? rest-args) '())
    ((and (pvec? rest-args)
          (fx=?
            (pvec-cnt rest-args)
            (vector-length (pvec-tail rest-args))))
     (vector->list (pvec-tail rest-args)))
    (else (seq->list rest-args))))

(register-method-arm!
  arm-priority-string
  (lambda (obj method-name rest-args)
    (if (string? obj)
        (jolt-string-method
          method-name
          obj
          (method-rest-args->list rest-args))
        'pass)))

(register-method-arm!
  arm-priority-getclass
  (lambda (obj method-name rest-args)
    (if (string=? method-name "getClass")
        (jolt-class obj)
        'pass)))

(define-record-type jreify
  (fields methods protos delegate)
  (nongenerative chez-jreify-v2))

(register-code-value! jreify?)

(define (reified-methods obj)
  (and (jreify? obj) (jreify-methods obj)))

(define (reify-delegate obj)
  (and (jreify? obj) (jreify-delegate obj)))

(register-get-arm!
  jreify?
  (lambda (coll k d)
    (let ((m (and (reified-methods coll)
                  (hashtable-ref (reified-methods coll) "valAt" #f))))
      (if m (jolt-invoke m coll k d) d))))

(define (make-reified-delegating methods-map delegate
         proto-names)
  (let ((ht (make-hashtable string-hash string=?))
        (protos (if (and (pair? proto-names)
                         (null? (cdr proto-names))
                         (jolt-coll-pred? (car proto-names)))
                    (seq->list (car proto-names))
                    proto-names)))
    (for-each
      (lambda (p)
        (hashtable-set!
          ht
          (if (keyword? p) (keyword-t-name p) p)
          (jolt-get methods-map p jolt-nil)))
      (seq->list (jolt-keys methods-map)))
    (make-jreify
      ht
      (map (lambda (p) (if (symbol-t? p) (symbol-t-name p) p))
           protos)
      delegate)))

(define (make-reified methods-map . proto-names)
  (make-reified-delegating methods-map #f proto-names))

(define (iface-prefers-seq? v)
  (or (jrec-declares-coll-iface? v)
      (and (iface-method v "seq" #f) #t)))

(define (iface-iterator-obj v)
  (and (or (jrec? v) (jreify? v))
       (not (iface-prefers-seq? v))
       (iface-method v "iterator" #f)))

(define (iface-iterator-cursor v)
  (and (or (jrec? v) (jreify? v))
       (not (iface-prefers-seq? v))
       (iface-method v "hasNext" #f)))

(define (iterator-cursor->seq it)
  (jolt-make-lazy-seq
    (lambda ()
      (if (jolt-truthy?
            (record-method-dispatch it "hasNext" jolt-nil))
          (let ((v (record-method-dispatch it "next" jolt-nil)))
            (jolt-cons v (iterator-cursor->seq it)))
          jolt-nil))))

(register-seq-arm!
  iface-iterator-cursor
  (lambda (x) (jolt-seq (iterator-cursor->seq x))))

(register-seq-arm!
  (lambda (x)
    (and (iface-iterator-obj x)
         (not (iface-iterator-cursor x))))
  (lambda (x)
    (jolt-seq (record-method-dispatch x "iterator" jolt-nil))))

(define (jolt-satisfies? proto obj)
  (if (jclass? proto)
      (if (instance-check proto obj) #t #f)
      (jolt-satisfies-protocol? proto obj)))

(define (jolt-satisfies-protocol? proto obj)
  (let* ((pn (jolt-get proto (keyword #f "name") jolt-nil))
         (pn-str (if (symbol-t? pn) (symbol-t-name pn) pn)))
    (unless (string? pn-str)
      (throw-jvm
        'IllegalArgumentException
        (string-append
          "satisfies? expects a protocol, got: "
          (cond
            ((jclass? proto) (jclass-name proto))
            ((jolt-nil? proto) "nil")
            (else (jolt-final-str proto))))))
    (or (cond
          ((jrec? obj)
           (and (type-satisfies? (jrec-tag obj) pn-str) #t))
          ((jreify? obj)
           (and (memp
                  (lambda (p)
                    (or (string=? p pn-str) (proto-class-match? p pn-str)))
                  (jreify-protos obj))
                #t))
          (else #f))
        (let loop ((tags (value-host-tags obj)))
          (cond
            ((null? tags) #f)
            ((type-satisfies? (car tags) pn-str) #t)
            (else (loop (cdr tags))))))))

(define (last-dot s)
  (let loop ((i (- (string-length s) 1)))
    (cond
      ((< i 0) s)
      ((char=? (string-ref s i) #\.)
       (substring s (+ i 1) (string-length s)))
      (else (loop (- i 1))))))

(define (memp pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) lst)
    (else (memp pred (cdr lst)))))

(define (extenders proto)
  (let* ((pn (jolt-get proto (keyword #f "name") jolt-nil))
         (pn-str (if (symbol-t? pn) (symbol-t-name pn) pn))
         (out '()))
    (vector-for-each
      (lambda (tag)
        (let ((ti (hashtable-ref type-registry tag #f)))
          (when ti
            (let ((pi (hashtable-ref ti pn-str #f)))
              (when (and pi (hashtable-ref pi extend-mark #f))
                (set! out (cons (jolt-symbol jolt-nil tag) out)))))))
      (jolt-with-mutex rec-tbl-mu (hashtable-keys type-registry)))
    (if (null? out) jolt-nil (list->cseq out))))

(register-str-render!
  jrec?
  (lambda (v)
    (let ((f (find-protocol-method
               (jrec-tag v)
               "Object"
               "toString")))
      (cond
        (f (jolt-invoke f v))
        ((jrec-coll-print-shape v) =>
         (lambda (shape) (jrec-coll-pr v shape)))
        (else
         (let ((s (jrec-field-pr v)))
           (substring s 1 (string-length s))))))))

(register-str-render!
  (lambda (v)
    (and (jreify? v)
         (reified-methods v)
         (hashtable-ref (reified-methods v) "toString" #f)
         #t))
  (lambda (v)
    (jolt-invoke
      (hashtable-ref (reified-methods v) "toString" #f)
      v)))

(def-var!
  "clojure.core"
  "make-deftype-ctor"
  make-deftype-ctor)

(define (register-record-type! name-sym)
  (let ((tag (string-append
               (chez-current-ns)
               "."
               (symbol-t-name name-sym))))
    (jolt-with-mutex
      rec-tbl-mu
      (hashtable-set! chez-record-type-tbl tag #t))
    (let ((protos (filter
                    (lambda (s) (not (string=? s "clojure.lang.IType")))
                    (jch-direct-supers tag))))
      (jch-set-supers!
        tag
        (append
          protos
          '("clojure.lang.IRecord" "clojure.lang.IObj" "clojure.lang.IPersistentMap"
             "java.util.Map" "clojure.lang.IHashEq"
             "java.io.Serializable"))))
    (let ((ctor (hashtable-ref class-ctors-tbl tag #f))
          (shape (hashtable-ref
                   chez-record-shapes-tbl
                   (string-append
                     (chez-current-ns)
                     "/->"
                     (symbol-t-name name-sym))
                   #f)))
      (when (and ctor shape)
        (register-class-statics!
          tag
          (list
            (cons
              "create"
              (lambda (m)
                (let ((kws (vector-ref shape 0)))
                  (let loop ((rec (apply
                                    ctor
                                    (map (lambda (k) (jolt-get m k)) kws)))
                             (ks (seq->list (jolt-seq (jolt-keys m)))))
                    (cond
                      ((null? ks) rec)
                      ((member (car ks) kws) (loop rec (cdr ks)))
                      (else
                       (loop
                         (jolt-assoc rec (car ks) (jolt-get m (car ks)))
                         (cdr ks)))))))))))))
  jolt-nil)

(def-var!
  "clojure.core"
  "register-record-type!"
  register-record-type!)

(def-var! "clojure.core" "make-protocol" make-protocol)

(def-var!
  "clojure.core"
  "register-protocol-methods!"
  register-protocol-methods!)

(def-var! "clojure.core" "register-method" register-method)

(def-var!
  "clojure.core"
  "register-inline-method"
  register-inline-method)

(def-var!
  "clojure.core"
  "register-inline-protocol!"
  register-inline-protocol!)

(def-var! "jolt.host" "set-field!" jolt-set-field!)

(def-var!
  "clojure.core"
  "protocol-dispatch"
  (lambda (pn mn obj rest)
    (protocol-dispatch pn mn obj rest)))

(def-var!
  "clojure.core"
  "protocol-dispatch1"
  (lambda (pn mn obj) (protocol-dispatch1 pn mn obj)))

(def-var!
  "clojure.core"
  "protocol-dispatch2"
  (lambda (pn mn obj a) (protocol-dispatch2 pn mn obj a)))

(def-var!
  "clojure.core"
  "protocol-dispatch3"
  (lambda (pn mn obj a b) (protocol-dispatch3 pn mn obj a b)))

(def-var! "clojure.core" "satisfies?" jolt-satisfies?)

(def-var! "clojure.core" "extenders" extenders)

(def-var! "jolt.host" "type-satisfies?" type-satisfies?)

(def-var!
  "jolt.host"
  "protocol-key-of"
  (lambda (sym)
    (or (and (symbol-t? sym)
             (let ((v (jolt-resolve sym)))
               (and (var-cell? v) (protocol-value-key (var-cell-root v)))))
        (and (symbol-t? sym)
             (not (symbol-t-ns sym))
             (let* ((nm (symbol-t-name sym))
                    (n (string-length nm))
                    (i (let loop ((k (- n 1)))
                         (cond
                           ((< k 1) #f)
                           ((char=? (string-ref nm k) #\.) k)
                           (else (loop (- k 1)))))))
               (and i
                    (< (+ i 1) n)
                    (let* ((ns-part (list->string
                                      (map (lambda (c)
                                             (if (char=? c #\_) #\- c))
                                           (string->list
                                             (substring nm 0 i)))))
                           (name-part (substring nm (+ i 1) n))
                           (cell (var-cell-lookup ns-part name-part)))
                      (and cell
                           (protocol-value-key (var-cell-root cell)))))))
        jolt-nil)))

(def-var!
  "clojure.core"
  "make-reified"
  (lambda (mm . rest) (apply make-reified mm rest)))

(def-var!
  "clojure.core"
  "record-method-dispatch"
  (lambda (obj m rest) (record-method-dispatch obj m rest)))

