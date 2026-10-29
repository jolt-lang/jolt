;; java.util.jar.Manifest, Attributes, and Attributes.Name.
;;
;; Attributes is a Map from Attributes.Name to String kept in insertion order,
;; as the JDK's LinkedHashMap-backed one is: a re-put keeps its place. It is a
;; LinkedHashMap chain (linked-hash.ss) under its own class, so the Map surface,
;; its live views and its seq come from there. A Name compares and hashes without
;; regard to case. Manifest holds the main Attributes and a live Map of entry
;; name to Attributes (a java.util.HashMap, what getEntries hands back).

(define-jhost-tag-pred (attributes-name? t) (string=? t "attributes-name"))
(define (attributes-name-ctor name)
  (let* ((s (jolt-str-render-one name)) (n (string-length s)))
    (when (or (= n 0) (> n 70))
      (throw-jvm 'IllegalArgumentException "attribute name must have 1-70 characters"))
    (let loop ((i 0))
      (when (< i n)
        (let ((c (string-ref s i)))
          (unless (or (and (char<? c #\x80) (or (char-alphabetic? c) (char-numeric? c)))
                      (char=? c #\_) (char=? c #\-))
            (throw-jvm 'IllegalArgumentException s)))
        (loop (+ i 1))))
    (make-jhost "attributes-name" s)))
(define (attributes-name-hash x) (java-string-hashcode (ascii-string-down (jhost-state x))))
(register-class-ctor! "Attributes$Name" attributes-name-ctor)
(register-class-ctor! "java.util.jar.Attributes$Name" attributes-name-ctor)
(register-host-methods! "attributes-name"
  (list (cons "toString" (lambda (self) (jhost-state self)))
        (cons "equals" (lambda (self other)
                         (and (attributes-name? other) (string-ci=? (jhost-state self) (jhost-state other)))))
        (cons "hashCode" attributes-name-hash)))
(register-eq-arm! (lambda (a b) (and (attributes-name? a) (attributes-name? b)))
                  (lambda (a b) (string-ci=? (jhost-state a) (jhost-state b))))
(register-hash-arm! attributes-name? attributes-name-hash)
(register-str-render! attributes-name? jhost-state)
(define (attributes-name-of key)
  (if (attributes-name? key) key (attributes-name-ctor key)))

;; ---- Attributes ------------------------------------------------------------------
(define-jhost-tag-pred (jar-attributes? t) (string=? t "jar-attributes"))
(define (make-manifest-attributes) (make-jhost "jar-attributes" (lh-new #f)))
(define (jar-attributes-chain a) (jhost-state a))
(define (jar-attributes-pairs a)
  (map (lambda (n) (cons (n-key n) (n-val n))) (lh-nodes (jar-attributes-chain a) #f)))
(define (jar-attributes-put! a key value)
  (lh-put! (jar-attributes-chain a) a key value))
(define (jar-attributes-value a key)
  (lh-get (jar-attributes-chain a) (attributes-name-of key) jolt-nil))
(define (manifest-attributes-copy a)
  (let ((copy (make-manifest-attributes)))
    (for-each (lambda (p) (jar-attributes-put! copy (car p) (cdr p))) (jar-attributes-pairs a))
    copy))
;; Map.equals and Map.hashCode: the same mappings in any order; the hash is the
;; sum of each key's hash XOR its value's, in int arithmetic.
(define (jar-attributes-equal? a b)
  (let ((pa (jar-attributes-pairs a)) (cb (jar-attributes-chain b)))
    (and (= (length pa) (lh-size cb))
         (for-all (lambda (p)
                    (let ((n (lh-node cb (car p))))
                      (and n (jolt=2 (cdr p) (n-val n)))))
                  pa))))
(define (jar-attributes-hash a)
  (int32-low (fold-left (lambda (h p) (+ h (bitwise-xor (attributes-name-hash (car p))
                                                        (java-string-hashcode (cdr p)))))
                        0 (jar-attributes-pairs a))))
;; the registry first: it installs Map's equals and hashCode, which the
;; Attributes members below then replace
(register-jutil-coll! "jar-attributes" 'map
                      (lambda (a) (map lh-entry (lh-nodes (jar-attributes-chain a) #f))))
(register-host-methods! "jar-attributes"
  (append
   (lhm-methods jar-attributes-chain #f)
   (list
    (cons "getValue" jar-attributes-value)
    (cons "putValue" (lambda (self key value)
                       (jar-attributes-put! self (attributes-name-ctor key) (jolt-str-render-one value))))
    (cons "put" (lambda (self key value)
                  (unless (attributes-name? key) (zip-class-cast key "java.util.jar.Attributes$Name"))
                  (unless (or (string? value) (jolt-nil? value)) (zip-class-cast value "java.lang.String"))
                  (jar-attributes-put! self key value)))
    (cons "putAll" (lambda (self m)
                     (unless (jar-attributes? m) (zip-class-cast m "java.util.jar.Attributes"))
                     (for-each (lambda (p) (jar-attributes-put! self (car p) (cdr p))) (jar-attributes-pairs m))
                     jolt-nil))
    (cons "clone" manifest-attributes-copy)
    (cons "equals" (lambda (self other) (and (jar-attributes? other) (jar-attributes-equal? self other))))
    (cons "hashCode" jar-attributes-hash))))
;; seq, count, contains? and get answer through linked-hash.ss's arms (lh-kind),
;; which already sit behind the HashMap family's.
(register-eq-arm! (lambda (a b) (and (jar-attributes? a) (jar-attributes? b))) jar-attributes-equal?)
(register-hash-arm! jar-attributes? jar-attributes-hash)

;; ---- Manifest --------------------------------------------------------------------
(define-jhost-tag-pred (jar-manifest? t) (string=? t "jar-manifest"))
(define (make-manifest) (make-jhost "jar-manifest" (vector (make-manifest-attributes) (host-new "java.util.HashMap"))))
(define (manifest-main self) (vector-ref (jhost-state self) 0))
(define (manifest-entry-map self) (vector-ref (jhost-state self) 1))
(define (manifest-entry-names self) (hm-keys-ordered (manifest-entry-map self)))
(define (manifest-entry self name)
  (hashtable-ref (hm-tbl (manifest-entry-map self)) name jolt-nil))
(define (manifest-copy self)
  (let ((copy (make-manifest)))
    (vector-set! (jhost-state copy) 0 (manifest-attributes-copy (manifest-main self)))
    (for-each (lambda (name)
                (record-method-dispatch (manifest-entry-map copy) "put"
                  (jolt-list name (manifest-attributes-copy (manifest-entry self name)))))
              (manifest-entry-names self))
    copy))
(define (manifest-equal? a b)
  (and (jar-attributes-equal? (manifest-main a) (manifest-main b))
       (let ((names (manifest-entry-names a)))
         (and (= (length names) (length (manifest-entry-names b)))
              (for-all (lambda (name)
                         (let ((x (manifest-entry a name)) (y (manifest-entry b name)))
                           (and (jar-attributes? x) (jar-attributes? y) (jar-attributes-equal? x y))))
                       names)))))
(define (manifest-hash self)
  (int32-low
   (+ (jar-attributes-hash (manifest-main self))
      (fold-left (lambda (h name)
                   (+ h (bitwise-xor (java-string-hashcode name)
                                     (jar-attributes-hash (manifest-entry self name)))))
                 0 (manifest-entry-names self)))))
(register-host-methods! "jar-manifest"
  (list
   (cons "getMainAttributes" manifest-main)
   (cons "getEntries" manifest-entry-map)
   (cons "getAttributes" manifest-entry)
   (cons "clear" (lambda (self)
                   (lh-clear! (jar-attributes-chain (manifest-main self)))
                   (record-method-dispatch (manifest-entry-map self) "clear" jolt-nil)
                   jolt-nil))
   (cons "write" (lambda (self out) (manifest-write self out) jolt-nil))
   (cons "read" (lambda (self in) (manifest-read! self in) jolt-nil))
   (cons "equals" (lambda (self other) (and (jar-manifest? other) (manifest-equal? self other))))
   (cons "hashCode" manifest-hash)
   (cons "clone" manifest-copy)))
(register-eq-arm! (lambda (a b) (and (jar-manifest? a) (jar-manifest? b))) manifest-equal?)
(register-hash-arm! jar-manifest? manifest-hash)

;; ---- reading (Manifest.read / Attributes.read) -------------------------------------
;; Lines end at "\r\n", "\n" or a lone "\r"; a line that starts with a space
;; continues the one before; an unterminated last line is dropped. The main
;; section runs to the first blank line and every line in it -- "Name" too --
;; is a main attribute; after it each section opens with "Name: " and blank
;; lines between sections are skipped. A repeated attribute keeps its first
;; place and takes the last value.
(define (manifest-split-lines text)
  (let ((n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((= i n) (list->vector (reverse acc)))                   ; the unterminated tail is dropped
        ((char=? (string-ref text i) #\newline)
         (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
        ((char=? (string-ref text i) #\return)
         (let ((next (if (and (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\newline)) (+ i 2) (+ i 1))))
           (loop next next (cons (substring text start i) acc))))
        (else (loop (+ i 1) start acc))))))
(define (manifest-io-error what line)
  (throw-jvm 'java.io.IOException (string-append what " (line " (number->string (+ line 1)) ")")))
(define (manifest-continued? lines i)
  (and (< (+ i 1) (vector-length lines))
       (let ((next (vector-ref lines (+ i 1))))
         (and (> (string-length next) 0) (char=? (string-ref next 0) #\space)))))
;; One logical line starting at I: (values text next-index)
(define (manifest-logical-line lines i strip)
  (let loop ((text (substring (vector-ref lines i) strip (string-length (vector-ref lines i)))) (i i))
    (if (manifest-continued? lines i)
        (let ((next (vector-ref lines (+ i 1))))
          (loop (string-append text (substring next 1 (string-length next))) (+ i 1)))
        (values text (+ i 1)))))
;; Attributes.read: attributes into ATTRS from line I to a blank line; the index
;; after it.
(define (manifest-read-attributes! attrs lines i)
  (let loop ((i i))
    (if (or (>= i (vector-length lines)) (string=? (vector-ref lines i) ""))
        (+ i 1)
        (let ((line (vector-ref lines i)))
          (when (char=? (string-ref line 0) #\space)
            (manifest-io-error "misplaced continuation line" i))
          (let ((colon (let find ((k 0))
                         (cond ((>= k (string-length line)) #f)
                               ((char=? (string-ref line k) #\:) k)
                               (else (find (+ k 1)))))))
            (unless (and colon (< (+ colon 1) (string-length line))
                         (char=? (string-ref line (+ colon 1)) #\space))
              (manifest-io-error "invalid header field" i))
            (let-values (((value next) (manifest-logical-line lines i (+ colon 2))))
              (let ((name (guard (e (#t (manifest-io-error
                                         (string-append "invalid header field name: " (substring line 0 colon))
                                         i)))
                            (attributes-name-ctor (substring line 0 colon)))))
                (jar-attributes-put! attrs name value))
              (loop next)))))))
(define (manifest-section-name line)
  (and (>= (string-length line) 6)
       (string-ci=? (substring line 0 4) "name")
       (char=? (string-ref line 4) #\:)
       (char=? (string-ref line 5) #\space)))
(define (manifest-read! self in)
  (let* ((bv (get-bytevector-all (in-stream-source-port in)))
         (text (utf8->string (if (eof-object? bv) (make-bytevector 0) bv)))
         (text (if (and (> (string-length text) 0) (char=? (string-ref text 0) (integer->char #xFEFF)))
                   (substring text 1 (string-length text)) text))
         (lines (manifest-split-lines text))
         (n (vector-length lines))
         (entries (manifest-entry-map self)))
    (let loop ((i (manifest-read-attributes! (manifest-main self) lines 0)))
      (when (< i n)
        (let ((line (vector-ref lines i)))
          (cond
            ((string=? line "") (loop (+ i 1)))
            ((not (manifest-section-name line)) (manifest-io-error "invalid manifest format" i))
            (else
             (let-values (((name next) (manifest-logical-line lines i 6)))
               (let ((attrs (let ((have (manifest-entry self name)))
                              (if (jar-attributes? have)
                                  have
                                  (let ((fresh (make-manifest-attributes)))
                                    (record-method-dispatch entries "put" (jolt-list name fresh))
                                    fresh)))))
                 (loop (manifest-read-attributes! attrs lines next)))))))))))

;; ---- writing (Manifest.write) ----------------------------------------------------
;; The main section leads with Manifest-Version, or failing that
;; Signature-Version, and writes nothing else without one; each section after it
;; is "Name: " and its attributes. Every section ends with a blank line, and a
;; line longer than 72 bytes continues on lines that start with a space, broken
;; between UTF-8 characters.
(define (manifest-write-raw out bv)
  (record-method-dispatch out "write"
    (list->cseq (list (na-byte-array bv) (->num 0) (->num (bytevector-length bv))))))
(define (manifest-println72 out text)
  (let*-values (((bytes) (string->utf8 text))
                ((n) (bytevector-length bytes))
                ((acc extract) (open-bytevector-output-port)))
    (define (boundary? i) (or (>= i n) (not (= #x80 (bitwise-and (bytevector-u8-ref bytes i) #xC0)))))
    (when (> n 0)
      (put-u8 acc (bytevector-u8-ref bytes 0))
      (let loop ((pos 1))
        (if (> (- n pos) 71)
            (let ((brk (let back ((b (+ pos 71))) (if (boundary? b) b (back (- b 1))))))
              (put-bytevector acc bytes pos (- brk pos))
              (put-bytevector acc (string->utf8 "\r\n "))
              (loop brk))
            (put-bytevector acc bytes pos (- n pos)))))
    (put-bytevector acc (string->utf8 "\r\n"))
    (manifest-write-raw out (extract))))
(define (manifest-write-attributes out attrs skip)
  (for-each (lambda (p)
              (unless (and skip (string-ci=? (jhost-state (car p)) skip))
                (manifest-println72 out (string-append (jhost-state (car p)) ": " (jolt-str-render-one (cdr p))))))
            (jar-attributes-pairs attrs))
  (manifest-write-raw out (string->utf8 "\r\n")))
(define (manifest-write self out)
  (let* ((main (manifest-main self))
         (version-name (cond ((not (jolt-nil? (jar-attributes-value main "Manifest-Version"))) "Manifest-Version")
                             ((not (jolt-nil? (jar-attributes-value main "Signature-Version"))) "Signature-Version")
                             (else #f))))
    (if version-name
        (begin
          (manifest-println72 out (string-append version-name ": " (jar-attributes-value main version-name)))
          (manifest-write-attributes out main version-name))
        (manifest-write-raw out (string->utf8 "\r\n")))
    (for-each (lambda (name)
                (manifest-println72 out (string-append "Name: " name))
                (manifest-write-attributes out (manifest-entry self name) #f))
              (manifest-entry-names self))))

(define (manifest-ctor . args)
  (let ((self (make-manifest)))
    (cond
      ((null? args) self)
      ((and (= (length args) 1) (jar-manifest? (car args))) (manifest-copy (car args)))
      ((= (length args) 1) (manifest-read! self (car args)) self)
      (else (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.jar.Manifest")))))
(register-class-ctor! "Manifest" manifest-ctor)
(register-class-ctor! "java.util.jar.Manifest" manifest-ctor)
(define (attributes-ctor . args)
  (cond ((null? args) (make-manifest-attributes))
        ((and (= (length args) 1) (number? (car args))) (make-manifest-attributes))
        ((and (= (length args) 1) (jar-attributes? (car args))) (manifest-attributes-copy (car args)))
        (else (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.jar.Attributes"))))
(register-class-ctor! "Attributes" attributes-ctor)
(register-class-ctor! "java.util.jar.Attributes" attributes-ctor)

(hashtable-set! jhost-tag->fqn "attributes-name" "java.util.jar.Attributes$Name")
(hashtable-set! jhost-tag->fqn "jar-attributes" "java.util.jar.Attributes")
(hashtable-set! jhost-tag->fqn "jar-manifest" "java.util.jar.Manifest")

;; ---- the embedded archive's manifest ------------------------------------------------
;; Parsed once, the first time anything asks: JarFile.getManifest hands back the
;; same Manifest each time, as the JDK's caching JarFile does, and whether the
;; archive is multi-release is read from it once.
(define embedded-archive-manifest-cache #f)
(define embedded-archive-manifest-mutex (make-mutex))
(define (embedded-archive-manifest-parse)
  (let* ((content (and (embedded-public-resource? "META-INF/MANIFEST.MF")
                       (embedded-resource-ref "META-INF/MANIFEST.MF")))
         (manifest (if content
                       (manifest-ctor (make-in-stream (open-bytevector-input-port
                                                       (if (bytevector? content) content (string->utf8 content)))))
                       jolt-nil))
         (multi-release? (and (jar-manifest? manifest)
                              (let ((v (jar-attributes-value (manifest-main manifest) "Multi-Release")))
                                (and (string? v) (string-ci=? v "true"))))))
    (cons manifest multi-release?)))
;; Parsed outside the lock (reading a stream may park); the first parse
;; published wins, so every caller sees the one Manifest.
(define (embedded-archive-manifest)
  (or embedded-archive-manifest-cache
      (let ((parsed (embedded-archive-manifest-parse)))
        (jolt-with-mutex embedded-archive-manifest-mutex
          (or embedded-archive-manifest-cache
              (begin (set! embedded-archive-manifest-cache parsed) parsed))))))
(define (embedded-archive-multi-release?) (cdr (embedded-archive-manifest)))
(define (embedded-jar-manifest archive) (car (embedded-archive-manifest)))
(define (embedded-jar-main-attributes archive)
  (let ((manifest (embedded-jar-manifest archive)))
    (if (jolt-nil? manifest) jolt-nil (manifest-main manifest))))
(define (embedded-jar-entry-attributes archive name)
  (let ((manifest (embedded-jar-manifest archive)))
    (if (jolt-nil? manifest) jolt-nil (manifest-entry manifest name))))
