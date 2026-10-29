;; jar-streams.ss — java.util.jar.JarInputStream and JarOutputStream (JDK 21
;; JarInputStream.java, JarOutputStream.java), on the ZipInputStream and
;; ZipOutputStream of zip-entries.ss and zip-out-entries.ss.
;;
;; A JarInputStream is a ZipInputStream that reads META-INF/MANIFEST.MF as it is
;; constructed, when that is the first entry or the second after META-INF/, and
;; hands back the entry after it as the first getNextEntry; its entries are
;; JarEntry values carrying their manifest section. A JarOutputStream is a
;; ZipOutputStream that writes the manifest it was given as its first entry, and
;; marks the first entry's extra field with the JAR magic 0xCAFE as the JDK does
;; (the JDK writes no META-INF/ directory entry here; the jar tool does).
;;
;; Nothing is signed in jolt, so the verify flag is accepted and there is no
;; verifier: getCertificates and getCodeSigners stay null, as they are on the JDK
;; for an unsigned jar.

;; --- JarInputStream ---------------------------------------------------------------
;; The state the JDK's subclass adds, beside the ZipInputStream's zipin record:
;; the manifest, the entry read ahead by the constructor, and whether the next
;; entry may still be a manifest (after META-INF/INDEX.LIST).
(define-record-type jarin
  (fields (mutable man) (mutable first) (mutable try-manifest))
  (nongenerative jolt-jarin-v1))
(define jarin-states (make-weak-eq-hashtable))
(define jarin-states-mu (make-mutex))
(define (jarin-of self)
  (let ((z (zipin-of self)))
    (and z (jolt-with-mutex jarin-states-mu (hashtable-ref jarin-states z #f)))))

;; The manifest section a JarInputStream's entry was created under.
(define jar-entry-stream-attrs (make-weak-eq-hashtable))
(define jar-entry-stream-attrs-mu (make-mutex))

;; read(b, off, len): -1 while the constructor's read-ahead entry is pending
(define (jarin-read z bv start count)
  (let ((j (jolt-with-mutex jarin-states-mu (hashtable-ref jarin-states z #f))))
    (if (and j (jarin-first j))
        0
        (zin-zip-read z bv start count))))

;; super.getNextEntry(), with createZipEntry's JarEntry: the ZipEntry the
;; ZipInputStream read, as a JarEntry over the same fields, with the manifest's
;; section for its name when there is a manifest by then.
(define (jarin-super-next self j)
  (let ((e (zipin-next-entry self)))
    (if (jolt-nil? e)
        jolt-nil
        (let ((je (make-jhost "jar-entry" (jhost-state e)))
              (man (jarin-man j)))
          (when man
            (let ((attrs (manifest-entry man (zentry-name (zentry-of je)))))
              (unless (jolt-nil? attrs)
                (jolt-with-mutex jar-entry-stream-attrs-mu
                  (hashtable-set! jar-entry-stream-attrs (zentry-of je) attrs)))))
          je))))

(define (jarin-name-is? e name)
  (and (not (jolt-nil? e)) (string-ci=? (zentry-name (zentry-of e)) name)))

;; checkManifest: when E is the manifest, read it, close it and answer the entry
;; after it.
(define (jarin-check-manifest self j e)
  (if (jarin-name-is? e "META-INF/MANIFEST.MF")
      (let* ((bytes (record-method-dispatch self "readAllBytes" jolt-nil))
             (man (manifest-ctor (make-in-stream (open-bytevector-input-port
                                                  (bytevector-copy (zip-bytes "readAllBytes" bytes)))))))
        (jarin-man-set! j man)
        (zipin-close-entry self)
        (jarin-super-next self j))
      e))

;; getNextEntry / getNextJarEntry
(define (jarin-next self j)
  (let ((first (jarin-first j)))
    (if first
        (begin
          (when (jarin-name-is? first "META-INF/INDEX.LIST") (jarin-try-manifest-set! j #t))
          (jarin-first-set! j #f)
          first)
        (let ((e (jarin-super-next self j)))
          (if (jarin-try-manifest j)
              (begin (jarin-try-manifest-set! j #f) (jarin-check-manifest self j e))
              e)))))

;; JarInputStream(InputStream) | JarInputStream(InputStream, boolean verify)
(define (make-jar-input-stream . args)
  (define (no-ctor)
    (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.jar.JarInputStream"))
  (unless (<= 1 (length args) 2) (no-ctor))
  (when (and (pair? (cdr args)) (not (boolean? (cadr args)))) (no-ctor))
  (let* ((self (make-zip-input-stream* "java.util.jar.JarInputStream" jarin-read (list (car args))))
         (j (make-jarin #f #f #f)))
    (jolt-with-mutex jarin-states-mu (hashtable-set! jarin-states (zipin-of self) j))
    (let* ((e (jarin-super-next self j))
           (e (if (jarin-name-is? e "META-INF/") (jarin-super-next self j) e)))
      (jarin-first-set! j (let ((f (jarin-check-manifest self j e))) (if (jolt-nil? f) #f f))))
    self))

(define (jarin-method name arities f)
  (host-arity-of arities #t
    (lambda (self . args)
      (let ((j (jarin-of self)))
        (if (and j (memv (length args) arities))
            (apply f self j args)
            (no-method-throw name self (length args)))))))

(define jarin-prior-next-entry
  (hashtable-ref (hashtable-ref host-methods-tbl "in-stream" #f) "getNextEntry" #f))
(register-host-methods! "in-stream"
  (list
   (cons "getNextEntry"
         (lambda (self . args)
           (let ((j (and (null? args) (jarin-of self))))
             (if j
                 (let ((e (jarin-next self j))) (or e jolt-nil))
                 (apply jarin-prior-next-entry self args)))))
   (cons "getNextJarEntry"
         (jarin-method "getNextJarEntry" '(0) (lambda (self j) (or (jarin-next self j) jolt-nil))))
   (cons "getManifest"
         (jarin-method "getManifest" '(0) (lambda (self j) (or (jarin-man j) jolt-nil))))))

;; JarEntry.getAttributes for an entry a JarInputStream made: its manifest
;; section. Any other JarEntry answers as it did.
(define jar-entry-prior-attributes
  (hashtable-ref (hashtable-ref host-methods-tbl "jar-entry" #f) "getAttributes" #f))
(register-host-methods! "jar-entry"
  (list
   (cons "getAttributes"
         (lambda (self . args)
           (let ((attrs (and (null? args)
                             (jolt-with-mutex jar-entry-stream-attrs-mu
                               (hashtable-ref jar-entry-stream-attrs (zentry-of self) #f)))))
             (or attrs (apply jar-entry-prior-attributes self args)))))))

;; --- JarOutputStream ------------------------------------------------------------
(define jar-magic #xCAFE)
;; The streams whose next entry is their first, keyed by the zout record.
(define jarout-first (make-weak-eq-hashtable))
(define jarout-first-mu (make-mutex))

;; hasMagic: does the extra field hold a 0xCAFE block? A malformed field
;; answers false, as the JDK's caught index exception does.
(define (jar-has-magic? bv)
  (let ((n (bytevector-length bv)))
    (let loop ((i 0))
      (and (fx<= (fx+ i 4) n)
           (or (= (bytevector-u16-ref bv i (endianness little)) jar-magic)
               (loop (fx+ i 4 (bytevector-u16-ref bv (fx+ i 2) (endianness little)))))))))

;; putNextEntry: the first entry's extra field gets the magic block in front,
;; on the caller's entry, as setExtra puts it there on the JDK.
(define (jarout-mark-first! self e)
  (let* ((z (zout-of self))
         (first? (jolt-with-mutex jarout-first-mu
                   (and (hashtable-ref jarout-first z #f)
                        (begin (hashtable-delete! jarout-first z) #t)))))
    (when (and first? (zip-entry? e))
      (let* ((x (record-method-dispatch e "getExtra" jolt-nil))
             (old (if (jolt-nil? x) #f (zip-bytes "getExtra" x))))
        (unless (and old (jar-has-magic? old))
          (let* ((n (if old (bytevector-length old) 0))
                 (bv (make-bytevector (+ n 4) 0)))
            (bytevector-u16-set! bv 0 jar-magic (endianness little))
            (when old (bytevector-copy! old 0 bv 4 n))
            (record-method-dispatch e "setExtra" (jolt-list (na-byte-array bv)))))))))

(define (jarout? self)
  (let ((z (zout-of self))) (and z (string=? (zout-fqn z) "java.util.jar.JarOutputStream"))))
(define jarout-prior-put
  (hashtable-ref (hashtable-ref host-methods-tbl "out-stream" #f) "putNextEntry" #f))
(register-host-methods! "out-stream"
  (list
   (cons "putNextEntry"
         (lambda (self . args)
           (when (and (= (length args) 1) (jarout? self))
             (jarout-mark-first! self (car args)))
           (apply jarout-prior-put self args)))))

;; JarOutputStream(OutputStream) | JarOutputStream(OutputStream, Manifest)
(define (make-jar-output-stream . args)
  (define (no-ctor)
    (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.jar.JarOutputStream"))
  (unless (<= 1 (length args) 2) (no-ctor))
  (when (and (pair? (cdr args)) (not (or (jolt-nil? (cadr args)) (jar-manifest? (cadr args)))))
    (zip-class-cast (cadr args) "java.util.jar.Manifest"))
  (let ((self (make-zip-output-stream* "java.util.jar.JarOutputStream" (list (car args)))))
    (jolt-with-mutex jarout-first-mu (hashtable-set! jarout-first (zout-of self) #t))
    (when (pair? (cdr args))
      (let ((man (cadr args)))
        (when (jolt-nil? man) (zip-throw "java.lang.NullPointerException" "man"))
        (record-method-dispatch self "putNextEntry" (jolt-list (make-zip-entry-named "META-INF/MANIFEST.MF")))
        (manifest-write man self)
        (record-method-dispatch self "closeEntry" jolt-nil)))
    self))

(reg-ctor! '("JarInputStream" "java.util.jar.JarInputStream") make-jar-input-stream)
(reg-ctor! '("JarOutputStream" "java.util.jar.JarOutputStream") make-jar-output-stream)
(jch-register-supers! "java.util.jar.JarInputStream" '("java.util.zip.ZipInputStream"))
(jch-register-supers! "java.util.jar.JarOutputStream" '("java.util.zip.ZipOutputStream"))
