;; zip-entries.ss — java.util.zip.ZipEntry and ZipInputStream (JDK 21
;; ZipEntry.java, ZipInputStream.java, ZipCoder.java). A ZipInputStream is an
;; in-stream on the frame of zip-in-streams.ss: its port's read! runs the JDK's
;; read(byte[], off, len) for the current entry and answers 0 at the entry's
;; end; getNextEntry moves the stream to the next local header.

;; --- ZipEntry ---------------------------------------------------------------
;; The fields of JDK 21 ZipEntry that reading sets. -1 is the JDK's "not set".
(define-record-type zentry
  (fields (mutable name) (mutable xdostime) (mutable crc) (mutable size)
          (mutable csize) (mutable method) (mutable extra) (mutable comment)
          (mutable mtime))   ; last-modified epoch millis when set by a FileTime, or #f
  (nongenerative jolt-zentry-v1))

(define-jhost-tag-pred (zip-entry? t) (string=? t "zip-entry"))
(define (zentry-of x) (jhost-state x))

;; --- DOS time (JDK 21 ZipUtils.java) ------------------------------------------
;; A zip entry's time is an MS-DOS date and time in the local zone, packed in 32
;; bits with two-second resolution; the JDK keeps the odd second and the
;; milliseconds in the bits above (extended DOS time). Chez's dates convert in
;; the process's own zone through libc, as ZoneId.systemDefault() does.
(define zip-dostime-before-1980 (bitwise-ior (bitwise-arithmetic-shift-left 1 21)
                                             (bitwise-arithmetic-shift-left 1 16)))
;; past 128 years from the epoch the JDK keeps the time as an mtime instead of
;; trusting the DOS field (ZipEntry.UPPER_DOSTIME_BOUND)
(define zip-upper-dostime-bound (* 128 365 24 60 60 1000))

;; epoch millis -> extended DOS time (javaToExtendedDosTime, lines 140-147).
(define (zip-java->xdostime ms)
  (if (< ms 0)
      zip-dostime-before-1980
      (let* ((secs (div ms 1000))
             (d (time-utc->date (make-time 'time-utc 0 secs)))
             (year (date-year d)))
        (if (< year 1980)
            zip-dostime-before-1980
            (let ((dostime (bitwise-and
                            (bitwise-ior (bitwise-arithmetic-shift-left (- year 1980) 25)
                                         (bitwise-arithmetic-shift-left (date-month d) 21)
                                         (bitwise-arithmetic-shift-left (date-day d) 16)
                                         (bitwise-arithmetic-shift-left (date-hour d) 11)
                                         (bitwise-arithmetic-shift-left (date-minute d) 5)
                                         (bitwise-arithmetic-shift-right (date-second d) 1))
                            #xffffffff)))
              (+ dostime (bitwise-arithmetic-shift-left (mod ms 2000) 32)))))))

;; extended DOS time -> epoch millis (extendedDosToJavaTime, lines 100-138). A
;; field out of range takes the Date constructor's normalization, which Chez's
;; make-date does not do: the excess rolls over by arithmetic here.
(define (zip-xdostime->java xdostime)
  (let* ((dtime (bitwise-and xdostime #xffffffff))
         (year (+ 1980 (bitwise-and (bitwise-arithmetic-shift-right dtime 25) #x7f)))
         (month (bitwise-and (bitwise-arithmetic-shift-right dtime 21) #x0f))
         (day (bitwise-and (bitwise-arithmetic-shift-right dtime 16) #x1f))
         (hour (bitwise-and (bitwise-arithmetic-shift-right dtime 11) #x1f))
         (minute (bitwise-and (bitwise-arithmetic-shift-right dtime 5) #x3f))
         (second (bitwise-and (bitwise-arithmetic-shift-left dtime 1) #x3e))
         (local-secs
          (lambda (y mo d h mi s)
            (time-second (date->time-utc (make-date 0 s mi h d mo y)))))
         (secs
          (or (guard (e (#t #f))
                (and (> month 0) (< month 13) (> day 0) (< hour 24) (< minute 60) (< second 60)
                     (local-secs year month day hour minute second)))
              ;; normalize as java.util.Date does: each excess carries up
              (let* ((mo0 (- (max month 1) 1))
                     (y (+ year (div mo0 12)))
                     (mo (+ 1 (mod mo0 12)))
                     (base (guard (e (#t 0)) (local-secs y mo 1 0 0 0))))
                (+ base (* (- (max day 1) 1) 86400) (* hour 3600) (* minute 60) second)))))
    (+ (* secs 1000) (bitwise-arithmetic-shift-right xdostime 32))))

;; setExtra0 (ZipEntry.java lines 549-628), less the Zip64 sizes: keep the
;; bytes, and read the modification time out of an extended-timestamp block
;; (0x5455, flag bit 1 then seconds since the epoch) or an NTFS block (0x000a,
;; attribute 1 with a Windows FILETIME), the way every JDK reader does — so an
;; entry's getTime is what the writer set, not the two-second DOS field.
(define (zentry-set-extra0! e bv)
  (let ((len (bytevector-length bv)))
    (let loop ((off 0))
      (when (<= (+ off 4) len)
        (let* ((tag (bytevector-u16-ref bv off (endianness little)))
               (sz (bytevector-u16-ref bv (+ off 2) (endianness little)))
               (at (+ off 4)))
          (when (<= (+ at sz) len)
            (cond
              ((and (= tag #x5455) (>= sz 1))
               (let ((flag (bytevector-u8-ref bv at)))
                 (when (and (not (zero? (bitwise-and flag 1))) (<= (+ at 5) (+ at sz)))
                   (zentry-mtime-set! e (* 1000 (bytevector-s32-ref bv (+ at 1) (endianness little)))))))
              ((and (= tag #x000a) (>= sz 32))
               (let ((pos (+ at 4)))
                 (when (and (= (bytevector-u16-ref bv pos (endianness little)) 1)
                            (= (bytevector-u16-ref bv (+ pos 2) (endianness little)) 24))
                   (let ((wtime (bytevector-s64-ref bv (+ pos 4) (endianness little))))
                     (unless (= wtime (- (expt 2 63)))
                       ;; FILETIME: 100 ns since 1601-01-01
                       (zentry-mtime-set! e (div (- wtime 116444736000000000) 10000))))))))
            (loop (+ at sz))))))
    (zentry-extra-set! e (na-byte-array (bytevector-copy bv)))))

;; String.length(): UTF-16 code units.
(define (zip-utf16-length s)
  (let loop ((i 0) (n 0))
    (if (fx= i (string-length s))
        n
        (loop (fx+ i 1) (fx+ n (if (fx>= (char->integer (string-ref s i)) #x10000) 2 1))))))

(define zentry-local-times (make-weak-eq-hashtable))
(define zentry-access-times (make-weak-eq-hashtable))
(define zentry-creation-times (make-weak-eq-hashtable))
(define zentry-real-names (make-weak-eq-hashtable))
(define (zentry-copy-side-data! source target)
  (for-each (lambda (table)
              (let ((value (hashtable-ref table source #f)))
                (when value (hashtable-set! table target value))))
            (list zentry-local-times zentry-access-times zentry-creation-times zentry-real-names)))

;; ZipEntry(String name): lines 105-111.
(define (make-zip-entry-named name)
  (when (jolt-nil? name) (zip-throw "java.lang.NullPointerException" "name"))
  (when (> (zip-utf16-length name) #xFFFF)
    (zip-throw "java.lang.IllegalArgumentException" "entry name too long"))
  (make-jhost "zip-entry" (make-zentry name -1 -1 -1 -1 -1 jolt-nil jolt-nil #f)))

;; ZipEntry(String) | ZipEntry(ZipEntry): lines 105-138. Any other argument
;; count, or one argument of another class, has no matching constructor. A nil
;; argument takes the String constructor.
(define (make-zip-entry . args)
  (define (no-ctor)
    (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.zip.ZipEntry"))
  (unless (= (length args) 1) (no-ctor))
  (let ((x (car args)))
    (cond
      ((and (jhost? x) (member (jhost-tag x) '("zip-entry" "jar-entry")))
       (let* ((e (zentry-of x))
              (copy (make-jhost "zip-entry"
                                (make-zentry (zentry-name e) (zentry-xdostime e) (zentry-crc e)
                                             (zentry-size e) (zentry-csize e) (zentry-method e)
                                             (zentry-extra e) (zentry-comment e) (zentry-mtime e)))))
         (zentry-copy-side-data! e (zentry-of copy))
         copy))
      ((or (string? x) (jolt-nil? x)) (make-zip-entry-named x))
      (else (no-ctor)))))

(hashtable-set! jhost-tag->fqn "zip-entry" "java.util.zip.ZipEntry")
;; Each member takes no arguments; a call with any is no-method-throw's.
(define (zentry-member name f) (cons name (zip-method name '(0) f)))
;; A setter takes one argument, cast as the compiled call casts it.
(define (zentry-setter name f) (cons name (zip-method name '(1) (lambda (self x) (f (zentry-of self) x) jolt-nil))))
;; A long argument: nil is NullPointerException, a non-number is not a Number.
(define (zentry-long-arg x)
  (cond ((jolt-nil? x)
         (throw-jvm 'NullPointerException
                    "Cannot invoke \"java.lang.Number.longValue()\" because \"x\" is null"))
        ((or (number? x) (jbigdec? x)) (jnum->exact x))
        (else (zip-class-cast x "java.lang.Number"))))
;; getTime(): the mtime when one was set, else the DOS time, else -1 (lines
;; 187-195).
(define (zentry-time e)
  (cond ((zentry-mtime e) => (lambda (ms) ms))
        ((= (zentry-xdostime e) -1) -1)
        (else (zip-xdostime->java (zentry-xdostime e)))))
;; setTime(long): lines 160-173. A time the DOS field cannot hold is kept as an
;; mtime, which the writer puts in an extended-timestamp field.
(define (zentry-set-time! e ms)
  (let ((x (zip-java->xdostime ms)))
    (zentry-xdostime-set! e x)
    (zentry-mtime-set! e (if (and (not (= x zip-dostime-before-1980)) (<= ms zip-upper-dostime-bound))
                             #f
                             ms))))
(define (embedded-local-date-time-ms ms)
  (if (= ms -1)
      jolt-nil
      (let ((fields (inst-fields ms)))
        (make-jhost "local-date-time"
                    (vector (list-ref fields 0) (list-ref fields 1) (list-ref fields 2)
                            (list-ref fields 3) (list-ref fields 4) (list-ref fields 5)
                            (* (list-ref fields 6) 1000000))))))
(register-host-methods! "local-date-time"
  (list
   (cons "getYear" (lambda (self) (vector-ref (jhost-state self) 0)))
   (cons "getMonthValue" (lambda (self) (vector-ref (jhost-state self) 1)))
   (cons "getDayOfMonth" (lambda (self) (vector-ref (jhost-state self) 2)))
   (cons "getHour" (lambda (self) (vector-ref (jhost-state self) 3)))
   (cons "getMinute" (lambda (self) (vector-ref (jhost-state self) 4)))
   (cons "getSecond" (lambda (self) (vector-ref (jhost-state self) 5)))
   (cons "getNano" (lambda (self) (vector-ref (jhost-state self) 6)))
   (cons "toString"
         (lambda (self)
           (let ((v (jhost-state self)))
             (string-append (number->string (vector-ref v 0)) "-"
                            (string-pad (number->string (vector-ref v 1)) 2 #\0) "-"
                            (string-pad (number->string (vector-ref v 2)) 2 #\0) "T"
                            (string-pad (number->string (vector-ref v 3)) 2 #\0) ":"
                            (string-pad (number->string (vector-ref v 4)) 2 #\0) ":"
                            (string-pad (number->string (vector-ref v 5)) 2 #\0)))))))
(define (zentry-local-ms value)
  (let* ((year (record-method-dispatch value "getYear" jolt-nil))
         (month (record-method-dispatch value "getMonthValue" jolt-nil))
         (day (record-method-dispatch value "getDayOfMonth" jolt-nil))
         (hour (record-method-dispatch value "getHour" jolt-nil))
         (minute (record-method-dispatch value "getMinute" jolt-nil))
         (second (record-method-dispatch value "getSecond" jolt-nil))
         (nano (record-method-dispatch value "getNano" jolt-nil)))
    (cal-ms (make-jhost "calendar"
                        (vector #f (vector year (- month 1) day hour minute second nano)
                                (calendar-arg-zone '()))))))
(define (zentry-set-local-time! e value)
  (let ((ms (zentry-local-ms value)))
    (hashtable-set! zentry-local-times e value)
    (zentry-xdostime-set! e (zip-java->xdostime ms))
    (zentry-mtime-set! e ms)))
(register-host-methods! "zip-entry"
  (list
   (zentry-member "getName" (lambda (self) (zentry-name (zentry-of self))))
   (zentry-member "getTime" (lambda (self) (->num (zentry-time (zentry-of self)))))
   (zentry-setter "setTime" (lambda (e x) (zentry-set-time! e (zentry-long-arg x))))
   (zentry-member "getTimeLocal"
                  (lambda (self)
                    (or (hashtable-ref zentry-local-times (zentry-of self) #f)
                        (embedded-local-date-time-ms (zentry-time (zentry-of self))))))
   (zentry-setter "setTimeLocal" zentry-set-local-time!)
   (zentry-member "getLastAccessTime"
                  (lambda (self) (or (hashtable-ref zentry-access-times (zentry-of self) #f) jolt-nil)))
   (zentry-setter "setLastAccessTime"
                  (lambda (e value)
                    (if (file-time? value)
                        (hashtable-set! zentry-access-times e value)
                        (zip-class-cast value "java.nio.file.attribute.FileTime"))))
   (zentry-member "getCreationTime"
                  (lambda (self) (or (hashtable-ref zentry-creation-times (zentry-of self) #f) jolt-nil)))
   (zentry-setter "setCreationTime"
                  (lambda (e value)
                    (if (file-time? value)
                        (hashtable-set! zentry-creation-times e value)
                        (zip-class-cast value "java.nio.file.attribute.FileTime"))))
   ;; getLastModifiedTime / setLastModifiedTime (lines 211-240): a FileTime, or
   ;; null when no time is set
   (zentry-member "getLastModifiedTime"
                  (lambda (self)
                    (let ((e (zentry-of self)))
                      (cond ((zentry-mtime e) => make-file-time)
                            ((= (zentry-xdostime e) -1) jolt-nil)
                            (else (make-file-time (zentry-time e)))))))
   ;; setLastModifiedTime answers the entry (public ZipEntry setLastModifiedTime),
   ;; so (-> e (.setLastModifiedTime t) ...) chains as on the JVM; the other
   ;; setters are void
   (cons "setLastModifiedTime"
         (zip-method "setLastModifiedTime" '(1)
                     (lambda (self t)
                       (let ((e (zentry-of self)))
                         (cond ((jolt-nil? t) (zip-throw "java.lang.NullPointerException" "lastModifiedTime"))
                               ((file-time? t)
                                (let ((ms (file-time-ms t)))
                                  (zentry-xdostime-set! e (zip-java->xdostime ms))
                                  (zentry-mtime-set! e ms)))
                               (else (zip-class-cast t "java.nio.file.attribute.FileTime"))))
                       self)))
   ;; the size, compressed size and CRC-32 setters check as lines 262-330 do
   (zentry-setter "setSize"
                  (lambda (e x)
                    (let ((n (zentry-long-arg x)))
                      (when (< n 0) (zip-throw "java.lang.IllegalArgumentException" "invalid entry size"))
                      (zentry-size-set! e n))))
   (zentry-setter "setCompressedSize" (lambda (e x) (zentry-csize-set! e (zentry-long-arg x))))
   (zentry-setter "setCrc"
                  (lambda (e x)
                    (let ((n (zentry-long-arg x)))
                      (when (or (< n 0) (> n #xffffffff))
                        (zip-throw "java.lang.IllegalArgumentException" "invalid entry crc-32"))
                      (zentry-crc-set! e n))))
   (zentry-setter "setMethod"
                  (lambda (e x)
                    (let ((m (zip-int-arg x)))
                      (unless (or (= m zip-stored) (= m zip-deflated))
                        (zip-throw "java.lang.IllegalArgumentException" "invalid compression method"))
                      (zentry-method-set! e m))))
   ;; setExtra(byte[]): the bytes are kept as given (lines 379-388); the JDK
   ;; also reads a Zip64 or timestamp block out of them, which is not done here
   (zentry-setter "setExtra"
                  (lambda (e x)
                    (cond ((jolt-nil? x) (zentry-extra-set! e jolt-nil))
                          ((zip-byte-array? x)
                           (when (> (bytevector-length (zip-bytes "setExtra" x)) #xffff)
                             (zip-throw "java.lang.IllegalArgumentException" "invalid extra field length"))
                           (zentry-set-extra0! e (zip-bytes "setExtra" x)))
                          (else (zip-class-cast x "[B")))))
   ;; setComment keeps any length (lines 655-657); the writer cuts it at 0xFFFF
   (zentry-setter "setComment"
                  (lambda (e x)
                    (cond ((jolt-nil? x) (zentry-comment-set! e jolt-nil))
                          ((string? x) (zentry-comment-set! e x))
                          (else (zip-class-cast x "java.lang.String")))))
   ;; the name ends with "/" (lines 675-677)
   (zentry-member "isDirectory"
                  (lambda (self)
                    (let* ((n (zentry-name (zentry-of self)))
                           (len (string-length n)))
                      (and (fx> len 0) (char=? (string-ref n (fx- len 1)) #\/)))))
   (zentry-member "getSize" (lambda (self) (->num (zentry-size (zentry-of self)))))
   (zentry-member "getCompressedSize" (lambda (self) (->num (zentry-csize (zentry-of self)))))
   (zentry-member "getCrc" (lambda (self) (->num (zentry-crc (zentry-of self)))))
   (zentry-member "getMethod" (lambda (self) (->num (zentry-method (zentry-of self)))))
   (zentry-member "getExtra" (lambda (self) (zentry-extra (zentry-of self))))
   (zentry-member "getComment" (lambda (self) (zentry-comment (zentry-of self))))
   (zentry-member "toString" (lambda (self) (zentry-name (zentry-of self))))
   (zentry-member "hashCode"
                  (lambda (self) (->num (java-string-hashcode (zentry-name (zentry-of self))))))
   ;; a copy whose extra field is a copy (lines 696-705)
   (zentry-member "clone"
                  (lambda (self)
                    (let* ((copy (make-jhost (jhost-tag self) (jhost-state (make-zip-entry self))))
                           (e (zentry-of copy))
                           (x (zentry-extra e)))
                      (unless (jolt-nil? x)
                        (zentry-extra-set! e (na-byte-array (bytevector-copy (zip-bytes "clone" x)))))
                      copy)))))

;; str is the name, as toString is; hash is hashCode, as Clojure's hash of a Java
;; object is.
(register-str-render! zip-entry? (lambda (x) (zentry-name (zentry-of x))))
(register-hash-arm! zip-entry? (lambda (x) (java-string-hashcode (zentry-name (zentry-of x)))))

;; --- names ------------------------------------------------------------------

;; A UTF-8 name, refused as JDK 21 refuses malformed input: the first bad
;; sequence is IllegalArgumentException "malformed input off : OFF, length : N"
;; (String.java lines 697-760 newStringUTF8NoRepl, 1125-1240 decodeUTF8_UTF16,
;; 1266-1269 throwMalformed). Valid input then decodes with zip-utf8->string.
(define (zip-utf8-malformed off n)
  (zip-throw "java.lang.IllegalArgumentException"
             (string-append "malformed input off : " (number->string off)
                            ", length : " (number->string n))))

(define (zip-decode-utf8 bv)
  (let ((sl (bytevector-length bv)))
    (define (u i) (bytevector-u8-ref bv i))
    (define (cont? b) (= (bitwise-and b #xc0) #x80))
    (let loop ((sp 0))
      (when (< sp sl)
        (let ((b1 (u sp))
              (sp (+ sp 1)))
          (cond
            ((< b1 #x80) (loop sp))
            ;; two bytes, C2..DF
            ((and (= (bitwise-and b1 #xe0) #xc0) (not (zero? (bitwise-and b1 #x1e))))
             (cond ((>= sp sl) (zip-utf8-malformed sp 1))
                   ((not (cont? (u sp))) (zip-utf8-malformed sp 1))
                   (else (loop (+ sp 1)))))
            ;; three bytes, E0..EF
            ((= (bitwise-and b1 #xf0) #xe0)
             (cond
               ((< (+ sp 1) sl)
                (let ((b2 (u sp))
                      (b3 (u (+ sp 1))))
                  (if (or (and (= b1 #xe0) (= (bitwise-and b2 #xe0) #x80))
                          (not (cont? b2))
                          (not (cont? b3)))
                      (zip-utf8-malformed (- sp 1) 3)
                      (let ((c (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b1 #x0f) 12)
                                            (bitwise-arithmetic-shift-left (bitwise-and b2 #x3f) 6)
                                            (bitwise-and b3 #x3f))))
                        (if (<= #xd800 c #xdfff)
                            (zip-utf8-malformed (- sp 1) 3)
                            (loop (+ sp 2)))))))
               ((and (< sp sl)
                     (or (and (= b1 #xe0) (= (bitwise-and (u sp) #xe0) #x80))
                         (not (cont? (u sp)))))
                (zip-utf8-malformed (- sp 1) 2))
               (else (zip-utf8-malformed sp 1))))
            ;; four bytes, F0..F7
            ((= (bitwise-and b1 #xf8) #xf0)
             (if (< (+ sp 2) sl)
                 (let* ((b2 (u sp))
                        (b3 (u (+ sp 1)))
                        (b4 (u (+ sp 2)))
                        (uc (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b1 #x07) 18)
                                         (bitwise-arithmetic-shift-left (bitwise-and b2 #x3f) 12)
                                         (bitwise-arithmetic-shift-left (bitwise-and b3 #x3f) 6)
                                         (bitwise-and b4 #x3f))))
                   (if (or (not (cont? b2)) (not (cont? b3)) (not (cont? b4))
                           (not (<= #x10000 uc #x10ffff)))
                       (zip-utf8-malformed (- sp 1) 4)
                       (loop (+ sp 3))))
                 (zip-utf8-malformed (- sp 1) 1)))
            (else (zip-utf8-malformed (- sp 1) 1))))))
    (zip-utf8->string bv)))

;; utf8->string drops a byte order mark at the start of its input (probe
;; 2026-09-15: EF BB BF 61 decodes to "a"). JDK 21 keeps each one as U+FEFF, so
;; the marks at the start are counted and put back.
(define (zip-utf8->string bv)
  (let ((len (bytevector-length bv)))
    (let loop ((i 0))
      (cond
        ((and (<= (+ i 3) len)
              (= (bytevector-u8-ref bv i) #xEF)
              (= (bytevector-u8-ref bv (+ i 1)) #xBB)
              (= (bytevector-u8-ref bv (+ i 2)) #xBF))
         (loop (+ i 3)))
        ((= i 0) (utf8->string bv))
        (else
         (let ((rest (make-bytevector (- len i))))
           (bytevector-copy! bv i rest 0 (- len i))
           (string-append (make-string (quotient i 3) #\xFEFF) (utf8->string rest))))))))

;; A name in another charset (ZipCoder.toString, ZipCoder.java lines 82-88):
;; bytes the charset refuses are IllegalArgumentException with the decoder's
;; exception as its message. US-ASCII is checked here; other charsets decode as
;; (String. bytes charset) does.
(define (zip-decode-charset bv name)
  (if (string=? name "us-ascii")
      (let loop ((i 0))
        (cond ((= i (bytevector-length bv)) (decode-bytevector bv (list name)))
              ((> (bytevector-u8-ref bv i) 127)
               (zip-throw "java.lang.IllegalArgumentException"
                          "java.nio.charset.MalformedInputException: Input length = 1"))
              (else (loop (+ i 1)))))
      (decode-bytevector bv (list name))))

;; --- ZipInputStream ---------------------------------------------------------
;; JDK 21 ZipConstants.java and ZipConstants64.java.
(define zip-locsig #x04034b50)
(define zip-extsig #x08074b50)
(define zip-lochdr 30)
(define zip-exthdr 16)
(define zip64-exthdr 24)
(define zip64-magicval #xFFFFFFFF)
(define zip-use-utf8 #x800)
(define zip-stored 0)
(define zip-deflated 8)

;; get16, get32 and get64 of a byte[] (ZipUtils.java lines 172-190).
(define (zip-get16 b off) (bytevector-u16-ref (zip-bytes "get16" b) off (endianness little)))
(define (zip-get32 b off) (bytevector-u32-ref (zip-bytes "get32" b) off (endianness little)))
(define (zip-get64 b off) (bytevector-s64-ref (zip-bytes "get64" b) off (endianness little)))

;; The fields of JDK 21 ZipInputStream. CODER is the charset's canonical name in
;; lower case, or #f for UTF-8 (ZipCoder.get, ZipCoder.java lines 51-56).
(define-record-type zipin
  (fields (mutable entry) (mutable flag) (mutable crc) (mutable remaining)
          tmpbuf (mutable entry-eof) coder)
  (nongenerative jolt-zipin-v1))

;; The zin record of X when X is a ZipInputStream, or #f.
(define (zipin-of x)
  (let ((z (zin-of x)))
    (and z (zipin? (zin-extra z)) z)))

;; in.read(b, off, len) on the PushbackInputStream; -1 at its end.
(define (zipin-read-inner z b off len)
  (jnum->exact (record-method-dispatch (zin-inner z) "read"
                 (list->cseq (list b (->num off) (->num len))))))

;; readFully(b, off, len): lines 633-642.
(define (zipin-read-fully z b off len)
  (let loop ((off off) (len len))
    (when (> len 0)
      (let ((n (zipin-read-inner z b off len)))
        (when (= n -1) (zip-throw "java.io.EOFException" #f))
        (loop (+ off n) (- len n))))))

;; ((PushbackInputStream) in).unread(b, off, len)
(define (zipin-unread z b off len)
  (record-method-dispatch (zin-inner z) "unread"
    (list->cseq (list b (->num off) (->num len)))))

;; Does EXTRA hold a Zip64 block? The walk is setExtra0's (ZipEntry.java lines
;; 549-628).
(define (zip-has-zip64-block? extra)
  (let* ((bv (zip-bytes "extra" extra))
         (len (bytevector-length bv)))
    (let loop ((off 0))
      (and (< (+ off 4) len)
           (let ((tag (bytevector-u16-ref bv off (endianness little)))
                 (sz (bytevector-u16-ref bv (+ off 2) (endianness little))))
             (and (<= (+ off 4 sz) len)
                  (or (= tag #x0001)
                      (loop (+ off 4 sz)))))))))

;; readLOC(): lines 495-545. #f at the end of the entries. A Zip64 entry (a size
;; of 0xFFFFFFFF in the header, or a Zip64 block in the extra field) is
;; ZipException; JDK 21 reads it, and known-divergences.edn lists that.
(define (zipin-read-loc z st)
  (let ((tmp (zipin-tmpbuf st)))
    (and (guard (e ((zip-thrown? e "EOFException") #f))
           (zipin-read-fully z tmp 0 zip-lochdr)
           #t)
         (= (zip-get32 tmp 0) zip-locsig)
         (let* ((flag (zip-get16 tmp 6))
                (len (zip-get16 tmp 26))
                (b (na-byte-array len)))
           (zipin-flag-set! st flag)
           (zipin-read-fully z b 0 len)
           (let* ((bv (zip-bytes "name" b))
                  (entry (make-zip-entry-named
                          (if (or (not (zero? (bitwise-and flag zip-use-utf8)))
                                  (not (zipin-coder st)))
                              (zip-decode-utf8 bv)
                              (zip-decode-charset bv (zipin-coder st)))))
                  (e (zentry-of entry)))
             (when (= (bitwise-and flag 1) 1)
               (zip-throw "java.util.zip.ZipException" "encrypted ZIP entry not supported"))
             (zentry-method-set! e (zip-get16 tmp 8))
             (zentry-xdostime-set! e (zip-get32 tmp 10))
             (if (= (bitwise-and flag 8) 8)
                 (unless (= (zentry-method e) zip-deflated)
                   (zip-throw "java.util.zip.ZipException"
                              "only DEFLATED entries can have EXT descriptor"))
                 (begin
                   (zentry-crc-set! e (zip-get32 tmp 14))
                   (zentry-csize-set! e (zip-get32 tmp 18))
                   (zentry-size-set! e (zip-get32 tmp 22))))
             (let ((elen (zip-get16 tmp 28)))
               (when (> elen 0)
                 (let ((extra (na-byte-array elen)))
                   (zipin-read-fully z extra 0 elen)
                   (zentry-set-extra0! e (zip-bytes "extra" extra)))))
             (when (or (= (zentry-csize e) zip64-magicval)
                       (= (zentry-size e) zip64-magicval)
                       (and (not (jolt-nil? (zentry-extra e)))
                            (zip-has-zip64-block? (zentry-extra e))))
               (zip-throw "java.util.zip.ZipException" "Zip64 entries are not supported"))
             entry)))))

;; Long.toHexString of a non-negative value.
(define (zip-hex n) (string-downcase (number->string n 16)))

(define (zip-crc-message expected got)
  (string-append "invalid entry CRC (expected 0x" (zip-hex expected)
                 " but got 0x" (zip-hex got) ")"))

;; readEnd(e): lines 574-628. Input the Inflater did not use goes back to the
;; PushbackInputStream. Then the data descriptor is read, with or without its
;; signature, and the sizes and CRC are checked.
(define (zipin-read-end z st e)
  (let* ((inf (zin-codec z))
         (n (inflater-remaining inf))
         (tmp (zipin-tmpbuf st)))
    (when (> n 0)
      (zipin-unread z (zin-buf z) (- (zin-len z) n) n))
    (when (= (bitwise-and (zipin-flag st) 8) 8)
      (if (or (> (inflater-bytes-written inf) zip64-magicval)
              (> (inflater-bytes-read inf) zip64-magicval))
          (begin
            (zipin-read-fully z tmp 0 zip64-exthdr)
            (let ((sig (zip-get32 tmp 0)))
              (if (not (= sig zip-extsig))
                  (begin
                    (zentry-crc-set! e sig)
                    (zentry-csize-set! e (zip-get64 tmp 4))
                    (zentry-size-set! e (zip-get64 tmp 12))
                    (zipin-unread z tmp 20 4))
                  (begin
                    (zentry-crc-set! e (zip-get32 tmp 4))
                    (zentry-csize-set! e (zip-get64 tmp 8))
                    (zentry-size-set! e (zip-get64 tmp 16))))))
          (begin
            (zipin-read-fully z tmp 0 zip-exthdr)
            (let ((sig (zip-get32 tmp 0)))
              (if (not (= sig zip-extsig))
                  (begin
                    (zentry-crc-set! e sig)
                    (zentry-csize-set! e (zip-get32 tmp 4))
                    (zentry-size-set! e (zip-get32 tmp 8))
                    (zipin-unread z tmp 12 4))
                  (begin
                    (zentry-crc-set! e (zip-get32 tmp 4))
                    (zentry-csize-set! e (zip-get32 tmp 8))
                    (zentry-size-set! e (zip-get32 tmp 12))))))))
    (let ((written (inflater-bytes-written inf))
          (used (inflater-bytes-read inf)))
      (unless (= (zentry-size e) written)
        (zip-throw "java.util.zip.ZipException"
                   (string-append "invalid entry size (expected " (number->string (zentry-size e))
                                  " but got " (number->string written) " bytes)")))
      (unless (= (zentry-csize e) used)
        (zip-throw "java.util.zip.ZipException"
                   (string-append "invalid entry compressed size (expected "
                                  (number->string (zentry-csize e))
                                  " but got " (number->string used) " bytes)")))
      (unless (= (zentry-crc e) (zipin-crc st))
        (zip-throw "java.util.zip.ZipException" (zip-crc-message (zentry-crc e) (zipin-crc st)))))))

;; read(byte[], off, len) for the current entry: lines 400-445, after the checks
;; zin-read makes. 0 is the JDK's -1.
(define (zin-zip-read z bv start count)
  (let* ((st (zin-extra z))
         (entry (zipin-entry st)))
    (if (not entry)
        0
        (let ((e (zentry-of entry)))
          (cond
            ((= (zentry-method e) zip-deflated)
             (let ((n (zin-inflate-read z bv start count)))
               (if (fx= n 0)
                   (begin
                     (zipin-read-end z st e)
                     (zipin-entry-eof-set! st #t)
                     (zipin-entry-set! st #f)
                     0)
                   (begin
                     (zipin-crc-set! st (zlib-crc32 (zipin-crc st) bv start n))
                     n))))
            ((= (zentry-method e) zip-stored)
             (if (<= (zipin-remaining st) 0)
                 (begin
                   (zipin-entry-eof-set! st #t)
                   (zipin-entry-set! st #f)
                   0)
                 (let* ((len (min count (zipin-remaining st)))
                        ;; the bytevector read! was given, seen as a byte[] by the wrapped read
                        (n (zipin-read-inner z (make-jolt-array bv 'byte) start len)))
                   (when (= n -1)
                     (zip-throw "java.util.zip.ZipException" "unexpected EOF"))
                   (zipin-crc-set! st (zlib-crc32 (zipin-crc st) bv start n))
                   (zipin-remaining-set! st (- (zipin-remaining st) n))
                   (when (and (= (zipin-remaining st) 0)
                              (not (= (zentry-crc e) (zipin-crc st))))
                     (zip-throw "java.util.zip.ZipException"
                                (zip-crc-message (zentry-crc e) (zipin-crc st))))
                   n)))
            (else (zip-throw "java.util.zip.ZipException" "invalid compression method")))))))

;; available(): lines 188-195.
(define (zin-zip-available z) (if (zipin-entry-eof (zin-extra z)) 0 1))

;; A method NAME that only ZipInputStream has, taking one of ARITIES arguments.
;; On any other in-stream, or with another count, there is no matching method.
(define (zipin-method name arities f)
  (host-arity-of arities #t
    (lambda (self . args)
      (if (and (zipin-of self) (memv (length args) arities))
          (apply f self args)
          (no-method-throw name self (length args))))))

;; closeEntry(): lines 169-173, through this stream's own read.
(define (zipin-close-entry self)
  (let ((z (zipin-of self)))
    (zin-live-port self z)
    (let* ((st (zin-extra z))
           (tmp (zipin-tmpbuf st)))
      (let loop ()
        (unless (= -1 (jnum->exact (record-method-dispatch self "read"
                                     (list->cseq (list tmp (->num 0) (->num 512))))))
          (loop)))
      (zipin-entry-eof-set! st #t)
      jolt-nil)))

;; getNextEntry(): lines 146-161. Before the next header, the port's end-of-file
;; flag comes off. A lookahead at the end of the previous entry sets it, and so
;; can a get-bytevector-n that comes up short; the next read would then answer
;; the flag instead of the new entry (Chez 10.4.1 s/io.ss lines 747-793).
(define (zipin-next-entry self)
  (let ((z (zipin-of self)))
    (let ((port (in-stream-live-port self))
          (st (zin-extra z)))
      (when (zipin-entry st)
        (zipin-close-entry self))
      (zipin-crc-set! st 0)
      (inflater-reset! (zin-codec z))
      (when (port-eof? port) (get-u8 port))
      (let ((entry (zipin-read-loc z st)))
        (zipin-entry-set! st entry)
        (if (not entry)
            jolt-nil
            (let ((e (zentry-of entry)))
              (when (= (zentry-method e) zip-stored)
                (zipin-remaining-set! st (zentry-size e)))
              (zipin-entry-eof-set! st #f)
              entry))))))

;; skip(n): lines 456-476. At the end of the entry, available() is 0. The count
;; is cast as the other zip streams cast it (zin-long-arg); another argument
;; count has no matching method. Other in-streams keep the skip this replaces.
(define zipin-prior-skip
  (hashtable-ref (hashtable-ref host-methods-tbl "in-stream" #f) "skip" #f))
(define (zipin-skip self . args)
  (let ((z (zipin-of self)))
    (cond
      ((not z) (apply zipin-prior-skip self args))
      ((not (= (length args) 1)) (no-method-throw "skip" self (length args)))
      (else
        (let ((n (zin-long-arg (car args))))
          (when (< n 0)
            (zip-throw "java.lang.IllegalArgumentException" "negative skip length"))
          (in-stream-live-port self)
          (let* ((st (zin-extra z))
                 (tmp (zipin-tmpbuf st))
                 (limit (min n 2147483647)))
            (let loop ((total 0))
              (if (>= total limit)
                  (->num total)
                  (let ((len (jnum->exact
                              (record-method-dispatch self "read"
                                (list->cseq (list tmp (->num 0) (->num (min (- limit total) 512))))))))
                    (if (= len -1)
                        (begin (zipin-entry-eof-set! st #t) (->num total))
                        (loop (+ total len))))))))))))

;; ZipInputStream(in) | ZipInputStream(in, charset): lines 110-137. The compiled
;; call casts in to InputStream and charset to Charset, nil passing both; then
;; the constructor checks in, then charset, for null. Jolt's StandardCharsets
;; constants are strings, so a string passes the Charset cast (JDK 21 refuses a
;; string there).
(define (zip-charset? x) (or (string? x) (and (jhost? x) (string=? (jhost-tag x) "charset"))))
(define (make-zip-input-stream . args)
  (unless (<= 1 (length args) 2)
    (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.zip.ZipInputStream"))
  (let* ((in (zin-stream-arg (car args)))
         (cs (if (pair? (cdr args))
                 (zin-codec-arg (cadr args) zip-charset? "java.nio.charset.Charset")
                 #f)))
    (when (jolt-nil? in) (zip-throw "java.lang.NullPointerException" "in is null"))
    (when (and cs (jolt-nil? cs)) (zip-throw "java.lang.NullPointerException" "charset is null"))
    (let ((coder (and cs
                      (let ((name (charset-canonical-down (charset-arg-name cs))))
                        (and (not (string=? name "utf-8")) name)))))
    (make-zin-stream
     (make-zin "java.util.zip.ZipInputStream" (make-pushback-in-stream in 512) (make-inflater #t) #t
               (na-byte-array 512) 0 #f
               zin-zip-read zin-zip-available zin-inflate-close
               (make-zipin #f 0 0 0 (na-byte-array 512) #f coder))))))

;; read(byte[], off, len): the arguments are cast, then ensureOpen runs, then
;; the null check. ZipInputStream.read has no null check of its own (lines
;; 400-402), so JDK 21's NullPointerException carries the message of the failed
;; b.length read (probe 2026-09-15).
(define zipin-prior-read
  (hashtable-ref (hashtable-ref host-methods-tbl "in-stream" #f) "read" #f))
(define (zipin-read self . rest)
  (when (and (= (length rest) 3) (zipin-of self))
    (let ((b (zin-bytes-arg (car rest))))
      (zip-int-arg (cadr rest))
      (zip-int-arg (caddr rest))
      (when (jolt-nil? b)
        (in-stream-live-port self)
        (zip-throw "java.lang.NullPointerException"
                   "Cannot read the array length because \"b\" is null"))))
  (apply zipin-prior-read self rest))

(register-host-methods! "in-stream"
  (list (cons "getNextEntry" (zipin-method "getNextEntry" '(0) zipin-next-entry))
        (cons "closeEntry" (zipin-method "closeEntry" '(0) zipin-close-entry))
        (cons "read" zipin-read)
        (cons "skip" zipin-skip)))

(register-class-statics! "java.util.zip.ZipEntry"
  (list (cons "STORED" (->num 0))
        (cons "DEFLATED" (->num 8))))
(reg-ctor! '("ZipEntry" "java.util.zip.ZipEntry") make-zip-entry)
(reg-ctor! '("ZipInputStream" "java.util.zip.ZipInputStream") make-zip-input-stream)
