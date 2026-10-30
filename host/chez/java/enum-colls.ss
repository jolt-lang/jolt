;; enum-colls.ss — java.util.EnumSet and java.util.EnumMap over the enum model
;; (java/enums.ss), and Comparators$NaturalOrderComparator declared as the enum
;; it is on the JDK.
;;
;; Both hold one slot per constant of their element (key) enum, indexed by
;; ordinal, so iteration is ordinal order and membership is one table lookup.
;; They are a java.util.Set and a java.util.Map to the rest of the runtime
;; through the java.util registry (jutil-colls.ss): =, hash, pr, str.
;;
;;   EnumSet  #(info bits)   bits: a vector of booleans
;;   EnumMap  #(info vals)   vals: a vector of values, emap-absent where unmapped
;;
;; An EnumSet over at most 64 constants is a java.util.RegularEnumSet and a
;; larger one a java.util.JumboEnumSet, as the JDK picks. An EnumMap's keySet,
;; values and entrySet are live views over the map.
;;
;; Loaded after jutil-colls.ss and tree-map.ss.

(register-enum-class! "java.util.Comparators$NaturalOrderComparator"
  (list (cons "INSTANCE" tm-natural-order)))

;; ---- the element universe ---------------------------------------------------------
(define (ecoll-class-info cls)
  (or (enum-class-info cls)
      (throw-jvm 'ClassCastException
                 (string-append "class " (if (jclass? cls) (jclass-jvm-name cls) (jolt-final-str cls))
                                " not an enum"))))
(define (ecoll-size info) (vector-length (enum-info-consts info)))
;; one enum class's info: the same object, or one a re-declaration replaced
(define (ecoll-same-info? a b)
  (or (eq? a b) (string=? (enum-info-fqn a) (enum-info-fqn b))))
;; the ordinal of X in INFO's universe, or #f
(define (ecoll-index info x)
  (let ((e (enum-of x))) (and e (ecoll-same-info? (car e) info) (cdr e))))
;; the enum info of X, refusing nil and a value that is no enum constant. WHAT
;; is the JDK parameter the NPE names (e for an element, key for a key).
(define (ecoll-npe what)
  (throw-jvm 'NullPointerException
             (string-append "Cannot invoke \"Object.getClass()\" because \"" what "\" is null")))
(define (ecoll-enum-of! x . what)
  (cond ((jolt-nil? x) (ecoll-npe (if (pair? what) (car what) "e")))
        ((enum-of x))
        (else (throw-jvm 'ClassCastException
                         (string-append "class " (jolt-class-name x)
                                        " cannot be cast to class java.lang.Enum")))))
;; the ordinal of an element being stored: nil and a foreign value are refused
(define (ecoll-index! info x . what)
  (let ((e (apply ecoll-enum-of! x what)))
    (if (ecoll-same-info? (car e) info)
        (cdr e)
        (throw-jvm 'ClassCastException
                   (string-append "class " (jolt-class-name x) " != class " (enum-info-fqn info))))))
(define (ecoll-list x) (seq->list (jolt-seq x)))

;; seq, count and contains? over every collection here, as ONE arm each: every
;; tag starts "enum-", so a value that is none of them is turned away by its
;; tag's first characters before any table is read.
;; tag -> #(elems count contains?)
(define ecoll-arm-tbl (make-hashtable string-hash string=?))
(define (ecoll-arms! tag elems count contains?)
  (hashtable-set! ecoll-arm-tbl tag (vector elems count contains?)))
(define (ecoll-ops x)
  (and (jhost? x)
       (let ((t (jhost-tag x)))
         (and (fx>? (string-length t) 5)
              (char=? (string-ref t 0) #\e) (char=? (string-ref t 1) #\n)
              (char=? (string-ref t 2) #\u) (char=? (string-ref t 3) #\m)
              (char=? (string-ref t 4) #\-)
              (hashtable-ref ecoll-arm-tbl t #f)))))
(register-seq-arm! ecoll-ops
  (lambda (x) (let ((xs ((vector-ref (ecoll-ops x) 0) x))) (if (null? xs) jolt-nil (list->cseq xs)))))
(register-count-arm! ecoll-ops (lambda (x) ((vector-ref (ecoll-ops x) 1) x)))
(register-contains-arm! ecoll-ops (lambda (x k) (and ((vector-ref (ecoll-ops x) 2) x k) #t)))

;; ---- EnumSet ------------------------------------------------------------------------
(define eset-tags '("enum-set-regular" "enum-set-jumbo"))
(define (eset? x) (and (jhost? x) (member (jhost-tag x) eset-tags) #t))
(define (make-eset info bits)
  (make-jhost (if (fx<=? (ecoll-size info) 64) "enum-set-regular" "enum-set-jumbo")
              (vector info bits)))
(define (eset-info s) (vector-ref (jhost-state s) 0))
(define (eset-bits s) (vector-ref (jhost-state s) 1))
(define (eset-of info fill) (make-eset info (make-vector (ecoll-size info) fill)))
(define (eset-elems s)
  (let ((bits (eset-bits s)) (cs (enum-info-consts (eset-info s))))
    (let loop ((i (fx- (vector-length bits) 1)) (acc '()))
      (if (fx<? i 0)
          acc
          (loop (fx- i 1) (if (vector-ref bits i) (cons (vector-ref cs i) acc) acc))))))
(define (eset-count s)
  (let ((bits (eset-bits s)))
    (let loop ((i 0) (n 0))
      (if (fx=? i (vector-length bits)) n (loop (fx+ i 1) (if (vector-ref bits i) (fx+ n 1) n))))))
(define (eset-contains? s x)
  (let ((i (ecoll-index (eset-info s) x))) (and i (vector-ref (eset-bits s) i))))
(define (eset-add! s x)
  (let* ((i (ecoll-index! (eset-info s) x)) (had (vector-ref (eset-bits s) i)))
    (vector-set! (eset-bits s) i #t)
    (not had)))
(define (eset-remove! s x)
  (let ((i (ecoll-index (eset-info s) x)))
    (and i (let ((had (vector-ref (eset-bits s) i)))
             (vector-set! (eset-bits s) i #f)
             had))))
(define (eset-copy s) (make-eset (eset-info s) (vector-copy (eset-bits s))))
;; a Collection method over each element, answering whether any call changed it
(define (eset-fold! s c op)
  (fold-left (lambda (changed x) (or (op s x) changed)) #f (ecoll-list c)))
(define (eset-retain! s c)
  (let ((keep (ecoll-list c)))
    (fold-left (lambda (changed x)
                 (if (exists (lambda (k) (eq? k x)) keep) changed (begin (eset-remove! s x) #t)))
               #f (eset-elems s))))

;; EnumSet.of(first, rest...) takes the rest loose or as one array.
(define (eset-of-elements first rest)
  (let* ((s (eset-of (car (ecoll-enum-of! first)) #f))
         (more (if (and (pair? rest) (null? (cdr rest)) (jolt-array? (car rest)))
                   (ecoll-list (car rest))
                   rest)))
    (eset-add! s first)
    (for-each (lambda (x) (eset-add! s x)) more)
    s))
(register-class-statics! "java.util.EnumSet"
  (list (cons "noneOf" (lambda (cls) (eset-of (ecoll-class-info cls) #f)))
        (cons "allOf" (lambda (cls) (eset-of (ecoll-class-info cls) #t)))
        (cons "of" (lambda (first . rest) (eset-of-elements first rest)))
        (cons "range"
              (lambda (from to)
                (let* ((info (car (ecoll-enum-of! from)))
                       (a (ecoll-index! info from)) (b (ecoll-index! info to)))
                  (when (fx>? a b)
                    (throw-jvm 'IllegalArgumentException
                               (string-append (jolt-str-render-one from) " > " (jolt-str-render-one to))))
                  (let ((s (eset-of info #f)))
                    (do ((i a (fx+ i 1))) ((fx>? i b) s)
                      (vector-set! (eset-bits s) i #t))))))
        (cons "copyOf"
              (lambda (c)
                (if (eset? c)
                    (eset-copy c)
                    (let ((xs (ecoll-list c)))
                      (when (null? xs) (throw-jvm 'IllegalArgumentException "Collection is empty"))
                      (eset-of-elements (car xs) (cdr xs))))))
        (cons "complementOf"
              (lambda (s)
                (unless (eset? s) (throw-jvm 'ClassCastException jolt-nil))
                (make-eset (eset-info s) (vector-map not (eset-bits s)))))))

(define eset-methods
  (list (cons "add" (lambda (self x) (eset-add! self x)))
        (cons "remove" (lambda (self x) (and (eset-remove! self x) #t)))
        (cons "contains" (lambda (self x) (and (eset-contains? self x) #t)))
        (cons "size" (lambda (self) (eset-count self)))
        (cons "isEmpty" (lambda (self) (fx=? 0 (eset-count self))))
        (cons "clear" (lambda (self) (vector-fill! (eset-bits self) #f) jolt-nil))
        (cons "addAll" (lambda (self c) (eset-fold! self c eset-add!)))
        (cons "removeAll" (lambda (self c) (eset-fold! self c (lambda (s x) (and (eset-remove! s x) #t)))))
        (cons "retainAll" (lambda (self c) (eset-retain! self c)))
        (cons "containsAll" (lambda (self c) (for-all (lambda (x) (eset-contains? self x)) (ecoll-list c))))
        (cons "iterator" (lambda (self)
                           (make-jutil-iterator (if (string=? (jhost-tag self) "enum-set-regular")
                                                    "enum-set-regular-iterator" "enum-set-jumbo-iterator")
                                                (eset-elems self) (lambda (x) (eset-remove! self x)))))
        (cons "toArray" (case-lambda
                          ((self) (jcoll-to-array (eset-elems self) '()))
                          ((self a) (jcoll-to-array (eset-elems self) (list a)))))
        (cons "forEach" (lambda (self f) (jcoll-for-each (eset-elems self) f)))
        (cons "removeIf" (lambda (self pred)
                           (let* ((xs (eset-elems self)) (keep (jcoll-keep xs pred)))
                             (for-each (lambda (x) (unless (memq x keep) (eset-remove! self x))) xs)
                             (not (fx=? (length keep) (length xs))))))
        (cons "clone" (lambda (self) (eset-copy self)))))
(for-each
  (lambda (tag fqn)
    (hashtable-set! jhost-tag->fqn tag fqn)
    (hashtable-set! jhost-tag->fqn (string-append tag "-iterator") (string-append fqn "$EnumSetIterator"))
    (register-jutil-iterator! (string-append tag "-iterator"))
    (register-host-methods! tag eset-methods)
    (register-jutil-coll! tag 'set eset-elems)
    (ecoll-arms! tag eset-elems eset-count eset-contains?))
  eset-tags '("java.util.RegularEnumSet" "java.util.JumboEnumSet"))

;; ---- EnumMap ------------------------------------------------------------------------
(define emap-absent (list 'emap-absent))
(define (emap? x) (and (jhost? x) (string=? (jhost-tag x) "enum-map")))
(define (make-emap info vals) (make-jhost "enum-map" (vector info vals)))
(define (emap-info m) (vector-ref (jhost-state m) 0))
(define (emap-vals m) (vector-ref (jhost-state m) 1))
(define (emap-new info) (make-emap info (make-vector (ecoll-size info) emap-absent)))
(define (emap-ref m k)
  (let ((i (ecoll-index (emap-info m) k)))
    (if i (vector-ref (emap-vals m) i) emap-absent)))
(define (emap-get m k) (let ((v (emap-ref m k))) (if (eq? v emap-absent) jolt-nil v)))
(define (emap-put! m k v)
  (let* ((i (ecoll-index! (emap-info m) k "key")) (old (vector-ref (emap-vals m) i)))
    (vector-set! (emap-vals m) i v)
    (if (eq? old emap-absent) jolt-nil old)))
(define (emap-remove! m k)
  (let ((i (ecoll-index (emap-info m) k)))
    (if i
        (let ((old (vector-ref (emap-vals m) i)))
          (vector-set! (emap-vals m) i emap-absent)
          (if (eq? old emap-absent) jolt-nil old))
        jolt-nil)))
;; (index . value) for every mapping, in ordinal order
(define (emap-slots m)
  (let ((vals (emap-vals m)))
    (let loop ((i (fx- (vector-length vals) 1)) (acc '()))
      (if (fx<? i 0)
          acc
          (loop (fx- i 1) (if (eq? (vector-ref vals i) emap-absent) acc (cons (cons i (vector-ref vals i)) acc)))))))
(define (emap-key m i) (vector-ref (enum-info-consts (emap-info m)) i))
(define (emap-keys m) (map (lambda (p) (emap-key m (car p))) (emap-slots m)))
(define (emap-values m) (map cdr (emap-slots m)))
;; A live entry, as the JDK's EnumMap$EntryIterator$Entry is: getValue reads the
;; map and setValue writes it. State: #(map index)
(define (emap-entry m i) (make-jhost "enum-map-entry" (vector m i)))
(define (emap-entries m) (map (lambda (p) (emap-entry m (car p))) (emap-slots m)))
(define (emap-count m) (length (emap-slots m)))
(define (emap-put-all! m src)
  (for-each (lambda (e) (emap-put! m (jolt-nth e 0) (jolt-nth e 1))) (ecoll-list src)))

;; (EnumMap. Class) | (EnumMap. EnumMap) | (EnumMap. Map), the last non-empty
(define (emap-ctor x)
  (cond ((emap? x) (make-emap (emap-info x) (vector-copy (emap-vals x))))
        ((or (jolt-map? x) (eq? (jutil-coll-kind x) 'map))
         (let ((es (ecoll-list x)))
           (when (null? es) (throw-jvm 'IllegalArgumentException "Specified map is empty"))
           (let ((m (emap-new (car (ecoll-enum-of! (jolt-nth (car es) 0))))))
             (emap-put-all! m x)
             m)))
        ((enum-class-info x) => emap-new)
        (else (throw-jvm 'NullPointerException
                         "Cannot read the array length because \"this.keyUniverse\" is null"))))
(register-class-ctor! "java.util.EnumMap" emap-ctor)
(register-class-ctor! "EnumMap" emap-ctor)

(define (emap-has-value? m v) (and (exists (lambda (p) (jolt=2 v (cdr p))) (emap-slots m)) #t))
(register-host-methods! "enum-map"
  (list (cons "put" (lambda (self k v) (emap-put! self k v)))
        (cons "get" (lambda (self k) (emap-get self k)))
        (cons "getOrDefault" (lambda (self k d) (let ((v (emap-ref self k))) (if (eq? v emap-absent) d v))))
        (cons "containsKey" (lambda (self k) (not (eq? (emap-ref self k) emap-absent))))
        (cons "containsValue" (lambda (self v) (emap-has-value? self v)))
        (cons "remove" (case-lambda
                         ((self k) (emap-remove! self k))
                         ((self k v) (let ((old (emap-ref self k)))
                                       (and (not (eq? old emap-absent)) (jolt=2 v old)
                                            (begin (emap-remove! self k) #t))))))
        ;; Map's defaults over the slots; a key mapped to nil counts as absent
        (cons "computeIfAbsent" (lambda (self k f)
                                  (let ((old (emap-get self k)))
                                    (if (jolt-nil? old)
                                        (let ((v (jolt-fi-call f "apply" k)))
                                          (unless (jolt-nil? v) (emap-put! self k v))
                                          v)
                                        old))))
        (cons "computeIfPresent" (lambda (self k f)
                                   (let ((old (emap-get self k)))
                                     (if (jolt-nil? old)
                                         jolt-nil
                                         (let ((v (jolt-fi-call f "apply" k old)))
                                           (if (jolt-nil? v) (begin (emap-remove! self k) jolt-nil)
                                               (begin (emap-put! self k v) v)))))))
        (cons "compute" (lambda (self k f)
                          (let* ((old (emap-get self k)) (v (jolt-fi-call f "apply" k old)))
                            (cond ((not (jolt-nil? v)) (emap-put! self k v) v)
                                  (else (emap-remove! self k) jolt-nil)))))
        (cons "merge" (lambda (self k v f)
                        (let* ((old (emap-get self k))
                               (nv (if (jolt-nil? old) v (jolt-fi-call f "apply" old v))))
                          (if (jolt-nil? nv)
                              (begin (emap-remove! self k) jolt-nil)
                              (begin (emap-put! self k nv) nv)))))
        (cons "replace" (case-lambda
                          ((self k v) (if (eq? (emap-ref self k) emap-absent) jolt-nil (emap-put! self k v)))
                          ((self k old v) (let ((cur (emap-ref self k)))
                                            (and (not (eq? cur emap-absent)) (jolt=2 old cur)
                                                 (begin (emap-put! self k v) #t))))))
        (cons "replaceAll" (lambda (self f)
                             (for-each (lambda (p)
                                         (vector-set! (emap-vals self) (car p)
                                                      (jolt-fi-call f "apply" (emap-key self (car p)) (cdr p))))
                                       (emap-slots self))
                             jolt-nil))
        (cons "size" (lambda (self) (emap-count self)))
        (cons "isEmpty" (lambda (self) (null? (emap-slots self))))
        (cons "clear" (lambda (self) (vector-fill! (emap-vals self) emap-absent) jolt-nil))
        (cons "putAll" (lambda (self src) (emap-put-all! self src) jolt-nil))
        (cons "putIfAbsent" (lambda (self k v)
                              (let ((old (emap-get self k)))
                                (if (jolt-nil? old) (begin (emap-put! self k v) jolt-nil) old))))
        (cons "forEach" (lambda (self f)
                          (for-each (lambda (p) (jolt-fi-call f "accept" (emap-key self (car p)) (cdr p)))
                                    (emap-slots self))
                          jolt-nil))
        (cons "keySet" (lambda (self) (jutil-view self 0 (lambda () (make-jhost "enum-map-keyset" self)))))
        (cons "values" (lambda (self) (jutil-view self 1 (lambda () (make-jhost "enum-map-values" self)))))
        (cons "entrySet" (lambda (self) (jutil-view self 2 (lambda () (make-jhost "enum-map-entryset" self)))))
        (cons "clone" (lambda (self) (emap-ctor self)))))
(hashtable-set! jhost-tag->fqn "enum-map" "java.util.EnumMap")
(hashtable-set! jhost-tag->fqn "enum-map-entry" "java.util.EnumMap$EntryIterator$Entry")
(define (emap-entry-value e)
  (let* ((st (jhost-state e)) (v (vector-ref (emap-vals (vector-ref st 0)) (vector-ref st 1))))
    (when (eq? v emap-absent) (throw-jvm 'IllegalStateException "Entry was removed"))
    v))
(register-jutil-entry! "enum-map-entry"
  (lambda (e) (let ((st (jhost-state e)))
                (cons (emap-key (vector-ref st 0) (vector-ref st 1)) (emap-entry-value e)))))
(register-host-methods! "enum-map-entry"
  (list (cons "setValue" (lambda (self v)
                           (let* ((st (jhost-state self)) (old (emap-entry-value self)))
                             (vector-set! (emap-vals (vector-ref st 0)) (vector-ref st 1) v)
                             old)))))
(register-jutil-coll! "enum-map" 'map emap-entries)
(ecoll-arms! "enum-map" emap-entries emap-count (lambda (m k) (not (eq? (emap-ref m k) emap-absent))))
(register-get-arm! emap? (lambda (m k d) (let ((v (emap-ref m k))) (if (eq? v emap-absent) d v))))

;; The three views: live over the map, removal writes through, add is refused as
;; on the JDK. State: the map.
(define (emap-view-entry-in? m e)
  (and (or (jolt-map-entry? e) (jutil-entry? e))
       (let ((v (emap-ref m (jolt-nth e 0)))) (and (not (eq? v emap-absent)) (jolt=2 v (jolt-nth e 1))))))
;; ITEM projects a slot (index . value) to what the view's iterator hands out
(define (emap-view tag elems contains? remove! item)
  (hashtable-set! jhost-tag->fqn tag
                  (cdr (assoc tag '(("enum-map-keyset" . "java.util.EnumMap$KeySet")
                                    ("enum-map-values" . "java.util.EnumMap$Values")
                                    ("enum-map-entryset" . "java.util.EnumMap$EntrySet")))))
  (hashtable-set! jhost-tag->fqn (string-append tag "-iterator")
                  (cdr (assoc tag '(("enum-map-keyset" . "java.util.EnumMap$KeyIterator")
                                    ("enum-map-values" . "java.util.EnumMap$ValueIterator")
                                    ("enum-map-entryset" . "java.util.EnumMap$EntryIterator")))))
  (register-jutil-iterator! (string-append tag "-iterator"))
  (let ((m-of jhost-state)
        (unsupported (lambda (self x) (throw-jvm 'UnsupportedOperationException jolt-nil))))
    (register-host-methods! tag
      (list (cons "size" (lambda (self) (emap-count (m-of self))))
            (cons "isEmpty" (lambda (self) (null? (emap-slots (m-of self)))))
            (cons "contains" (lambda (self x) (and (contains? (m-of self) x) #t)))
            (cons "remove" (lambda (self x) (and (remove! (m-of self) x) #t)))
            (cons "clear" (lambda (self) (vector-fill! (emap-vals (m-of self)) emap-absent) jolt-nil))
            (cons "add" unsupported)
            (cons "iterator"
                  (lambda (self)
                    (let ((m (m-of self)))
                      (make-jutil-iterator (string-append tag "-iterator") (emap-slots m)
                                           (lambda (p) (vector-set! (emap-vals m) (car p) emap-absent))
                                           (lambda (p) (item m p))))))
            (cons "toArray" (case-lambda
                              ((self) (jcoll-to-array (elems (m-of self)) '()))
                              ((self a) (jcoll-to-array (elems (m-of self)) (list a)))))))
    (ecoll-arms! tag (lambda (v) (elems (m-of v))) (lambda (v) (emap-count (m-of v)))
                 (lambda (v x) (contains? (m-of v) x)))))
(emap-view "enum-map-keyset" emap-keys
           (lambda (m k) (not (eq? (emap-ref m k) emap-absent)))
           (lambda (m k) (and (not (eq? (emap-ref m k) emap-absent)) (begin (emap-remove! m k) #t)))
           (lambda (m p) (emap-key m (car p))))
(emap-view "enum-map-values" emap-values
           emap-has-value?
           (lambda (m v)
             (let ((p (find (lambda (p) (jolt=2 v (cdr p))) (emap-slots m))))
               (and p (begin (emap-remove! m (emap-key m (car p))) #t))))
           (lambda (m p) (cdr p)))
(emap-view "enum-map-entryset" emap-entries
           emap-view-entry-in?
           (lambda (m e) (and (emap-view-entry-in? m e) (begin (emap-remove! m (jolt-nth e 0)) #t)))
           (lambda (m p) (emap-entry m (car p))))
(register-jutil-coll! "enum-map-keyset" 'set (lambda (v) (emap-keys (jhost-state v))))
(register-jutil-coll! "enum-map-values" 'coll (lambda (v) (emap-values (jhost-state v))))
(register-jutil-coll! "enum-map-entryset" 'entries (lambda (v) (emap-entries (jhost-state v))))
