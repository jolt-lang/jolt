;; charset-coding.ss — java.nio.CharBuffer, CodingErrorAction, CoderResult, and
;; the CharsetDecoder methods that need them.
;;
;; The decoder existed as a jhost with one method (.charset), which made the
;; documented way to decode a STREAM — a CharsetDecoder with
;; CodingErrorAction/REPLACE, fed a chunk at a time with endOfInput false —
;; inexpressible: the enum had no provider and .decode was a missing method. A
;; chunked reader cannot use String-over-bytes instead, because a multi-byte
;; character split across two chunks decodes to replacement characters that way.
;;
;; What makes the chunked idiom work is that decode(in, out, endOfInput) leaves a
;; TRAILING PARTIAL SEQUENCE in the input buffer: it stops at the last complete
;; character, returns UNDERFLOW, and the caller compacts the buffer so the next
;; chunk continues it. The decoder itself is therefore stateless apart from its
;; configuration — no carried bytes — which is exactly how it is modeled here.
;;
;; Loaded after byte-buffer.ss (ByteBuffer accessors) and host-static-classes.ss
;; (the charset jhost + decode-bytevector).

;; --- java.nio.CharBuffer -----------------------------------------------------
;; A CharBuffer is byte-buffer.ss's: a char-array buffer from allocate or
;; wrap(char[]), the read-only string one from wrap(CharSequence), or a view of
;; a ByteBuffer. The decode loop below writes into whichever the caller hands it,
;; through nb-char-appender.

;; --- java.nio.charset.CodingErrorAction --------------------------------------
;; An enum, modeled the way TimeUnit and Normalizer.Form are: one interned jhost
;; per constant, carrying its name so (str CodingErrorAction/REPLACE) reads right.
(define (coding-error-action? x) (and (jhost? x) (string=? (jhost-tag x) "coding-error-action")))
(define (coding-error-action-name a) (vector-ref (jhost-state a) 0))
(define coding-error-actions
  (map (lambda (nm) (cons nm (make-jhost "coding-error-action" (vector nm))))
       '("REPLACE" "REPORT" "IGNORE")))
(define (coding-error-action nm) (cdr (assoc nm coding-error-actions)))
(register-host-methods! "coding-error-action"
  (list (cons "name" coding-error-action-name)
        (cons "toString" coding-error-action-name)))
(register-str-render! coding-error-action? coding-error-action-name)
(register-class-arm! coding-error-action? (lambda (x) "java.nio.charset.CodingErrorAction"))
(register-class-statics! "java.nio.charset.CodingErrorAction" coding-error-actions)

;; --- java.nio.charset.CoderResult --------------------------------------------
;; state #(kind length). UNDERFLOW means "ran out of input" — the signal a
;; chunked reader loops on — and OVERFLOW "ran out of room in the output".
(define (coder-result? x) (and (jhost? x) (string=? (jhost-tag x) "coder-result")))
(define (coder-result-kind r) (vector-ref (jhost-state r) 0))
(define (coder-result-length r) (vector-ref (jhost-state r) 1))
(define coder-underflow (make-jhost "coder-result" (vector 'underflow 0)))
(define coder-overflow (make-jhost "coder-result" (vector 'overflow 0)))
(define (coder-malformed n) (make-jhost "coder-result" (vector 'malformed n)))
(define (coder-unmappable n) (make-jhost "coder-result" (vector 'unmappable n)))
(define (coder-result-error? r) (memq (coder-result-kind r) '(malformed unmappable)))
(define (coder-result-render r)
  (case (coder-result-kind r)
    ((underflow) "UNDERFLOW")
    ((overflow) "OVERFLOW")
    ((malformed) (string-append "MALFORMED[" (number->string (coder-result-length r)) "]"))
    (else (string-append "UNMAPPABLE[" (number->string (coder-result-length r)) "]"))))
(register-host-methods! "coder-result"
  (list (cons "isUnderflow" (lambda (self) (eq? (coder-result-kind self) 'underflow)))
        (cons "isOverflow" (lambda (self) (eq? (coder-result-kind self) 'overflow)))
        (cons "isError" (lambda (self) (and (coder-result-error? self) #t)))
        (cons "isMalformed" (lambda (self) (eq? (coder-result-kind self) 'malformed)))
        (cons "isUnmappable" (lambda (self) (eq? (coder-result-kind self) 'unmappable)))
        (cons "length" (lambda (self)
                         (if (coder-result-error? self)
                             (->num (coder-result-length self))
                             (throw-jvm (quote UnsupportedOperationException) (coder-result-render self)))))
        (cons "throwException" (lambda (self) (coder-result-throw self)))
        (cons "toString" coder-result-render)))
(register-str-render! coder-result? coder-result-render)
(register-class-arm! coder-result? (lambda (x) "java.nio.charset.CoderResult"))
(register-class-statics! "java.nio.charset.CoderResult"
  (list (cons "UNDERFLOW" coder-underflow)
        (cons "OVERFLOW" coder-overflow)
        (cons "malformedForLength" (lambda (n) (coder-malformed (jnum->exact n))))
        (cons "unmappableForLength" (lambda (n) (coder-unmappable (jnum->exact n))))))
;; The JVM's CoderResult.throwException raises the class matching the result, and
;; a caller catching CharacterCodingException expects to catch both.
(define (coder-result-throw r)
  (case (coder-result-kind r)
    ((malformed) (throw-jvm (quote java.nio.charset.MalformedInputException)
                            (string-append "Input length = " (number->string (coder-result-length r)))))
    ((unmappable) (throw-jvm (quote java.nio.charset.UnmappableCharacterException)
                             (string-append "Input length = " (number->string (coder-result-length r)))))
    ((overflow) (throw-jvm (quote java.nio.BufferOverflowException) "OVERFLOW"))
    (else (throw-jvm (quote java.nio.BufferUnderflowException) "UNDERFLOW"))))

;; --- the decode loop ---------------------------------------------------------
;; A decoder reads bytes out of a ByteBuffer through the byte-buffer accessors,
;; so a direct (FFI) buffer decodes as readily as a heap one.
(define (decoder-byte in i) (bitwise-and (bb-byte-ref in i) #xff))

;; How many bytes the UTF-8 sequence starting with this lead byte occupies, or 0
;; when the byte cannot start one (a continuation byte, or a lead the standard
;; retired: C0/C1 are overlong two-byte forms and F5..FF are past U+10FFFF).
(define (utf8-lead-length b)
  (cond ((fx<? b #x80) 1)
        ((fx<? b #xC2) 0)
        ((fx<? b #xE0) 2)
        ((fx<? b #xF0) 3)
        ((fx<? b #xF5) 4)
        (else 0)))
(define (utf8-continuation? b) (and (fx>=? b #x80) (fx<? b #xC0)))

;; One character from `in` at index i, as (values code-point consumed) — or
;; (values #f n) for a malformed sequence of n bytes, or (values #f 0) when the
;; sequence is merely INCOMPLETE and more input could finish it. That last case
;; is what the whole chunked idiom rests on.
(define (utf8-decode-one in i limit)
  (let* ((b (decoder-byte in i)) (n (utf8-lead-length b)))
    (cond
      ((fx=? n 0) (values #f 1))
      ((fx=? n 1) (values b 1))
      ((fx>? (fx+ i n) limit)
       ;; the bytes present must at least be valid continuations, or this is
       ;; malformed now and waiting for more input would never fix it.
       (let loop ((k (fx+ i 1)))
         (cond ((fx>=? k limit) (values #f 0))
               ((utf8-continuation? (decoder-byte in k)) (loop (fx+ k 1)))
               (else (values #f (fx- k i))))))
      (else
       (let loop ((k 1) (cp (case n
                              ((2) (fxand b #x1F))
                              ((3) (fxand b #x0F))
                              (else (fxand b #x07)))))
         (if (fx=? k n)
             ;; surrogates have no UTF-8 encoding, and a 3- or 4-byte form that
             ;; encodes a value the shorter form could hold is the classic
             ;; overlong attack; both are malformed input on the JVM.
             (if (or (and (fx>=? cp #xD800) (fx<=? cp #xDFFF))
                     (fx>? cp #x10FFFF)
                     (and (fx=? n 3) (fx<? cp #x800))
                     (and (fx=? n 4) (fx<? cp #x10000)))
                 (values #f n)
                 (values cp n))
             (let ((c (decoder-byte in (fx+ i k))))
               (if (utf8-continuation? c)
                   (loop (fx+ k 1) (fxior (fxarithmetic-shift-left cp 6) (fxand c #x3F)))
                   (values #f (fx+ k 1))))))))))

;; A single-byte charset decodes one byte to one character; US-ASCII has no
;; character for a byte over 127, which is a malformed-input error there.
(define (single-byte-decode-one in i ascii?)
  (let ((b (decoder-byte in i)))
    (if (and ascii? (fx>? b #x7F)) (values #f 1) (values b 1))))

;; Which of the two incremental decoders a canonical charset name uses, or #f for
;; a charset this file does not decode a byte at a time — see decoder-decode.
(define (decoder-kind name)
  (let ((cs (charset-canonical-down name)))
    (cond ((string=? cs "utf-8") 'utf8)
          ((string=? cs "iso-8859-1") 'latin1)
          ((string=? cs "us-ascii") 'ascii)
          (else #f))))

;; --- java.nio.charset.CharsetDecoder -----------------------------------------
;; state #(charset malformed-action unmappable-action replacement).
(define (decoder-charset d) (vector-ref (jhost-state d) 0))
;; A fresh decoder's actions are REPORT on the JVM (an object, not null); the
;; state slot starts #f because newDecoder is built in host-static-classes.ss,
;; before this file defines the constants — so the read supplies the default.
(define (decoder-malformed-action d) (or (vector-ref (jhost-state d) 1) (coding-error-action "REPORT")))
(define (decoder-unmappable-action d) (or (vector-ref (jhost-state d) 2) (coding-error-action "REPORT")))
(define (decoder-replacement d) (vector-ref (jhost-state d) 3))
(define (decoder-action-name a)
  (if (coding-error-action? a) (coding-error-action-name a) "REPORT"))

;; decode(in, out, endOfInput) — the streaming form, and the one the other two
;; are written in terms of. Consumes whole characters from `in` and appends them
;; to `out`, stopping at:
;;   UNDERFLOW  — `in` is exhausted, or holds only a partial character and more
;;                input may follow (endOfInput false). `in`'s position is left AT
;;                the partial sequence so the caller's compact() keeps it.
;;   OVERFLOW   — `out` is full; `in`'s position is left at the next character.
;;   MALFORMED  — only when the malformed action is REPORT; REPLACE writes the
;;                replacement string and IGNORE writes nothing, both skipping the
;;                offending bytes.
(define (decoder-decode-into d in out end-of-input?)
  (let ((kind (decoder-kind (charset-name (decoder-charset d))))
        (on-malformed (decoder-action-name (decoder-malformed-action d)))
        ;; appends one char, #f when out is full (the OVERFLOW result)
        (put (nb-char-appender out)))
    (define (emit-string s)         ; #f when out has no room for all of it
      (let loop ((i 0))
        (cond ((fx>=? i (string-length s)) #t)
              ((put (string-ref s i)) (loop (fx+ i 1)))
              (else #f))))
    (define (emit-code cp)
      (put (integer->char cp)))
    (let loop ()
      (let ((p (bb-pos in)) (limit (bb-limit in)))
        (if (fx>=? p limit)
            coder-underflow
            (let-values (((cp consumed)
                          (case kind
                            ((utf8) (utf8-decode-one in p limit))
                            ((ascii) (single-byte-decode-one in p #t))
                            (else (single-byte-decode-one in p #f)))))
              (cond
                ;; incomplete, and more input could complete it
                ((and (not cp) (fx=? consumed 0))
                 (if (not end-of-input?)
                     coder-underflow
                     ;; at end of input an incomplete sequence is malformed, and
                     ;; its length is everything left.
                     (let ((n (fx- limit p)))
                       (if (string=? on-malformed "REPORT")
                           (coder-malformed n)
                           (begin (bb-pos! in limit)
                                  (if (and (string=? on-malformed "REPLACE")
                                           (not (emit-string (decoder-replacement d))))
                                      coder-overflow
                                      coder-underflow))))))
                ((not cp)
                 (cond
                   ((string=? on-malformed "REPORT") (coder-malformed consumed))
                   ((string=? on-malformed "IGNORE") (bb-pos! in (fx+ p consumed)) (loop))
                   ((emit-string (decoder-replacement d)) (bb-pos! in (fx+ p consumed)) (loop))
                   (else coder-overflow)))
                ((emit-code cp) (bb-pos! in (fx+ p consumed)) (loop))
                (else coder-overflow))))))))

;; A charset this file has no incremental decoder for still has to decode
;; something: take the whole remaining input through decode-bytevector, which is
;; the same conversion (String. bytes charset) uses. The cost is that a trailing
;; partial character is not held back for the next chunk, so only the whole-input
;; decode is faithful for those charsets — the streaming form reports
;; endOfInput's answer either way.
(define (decoder-decode-bulk d in out)
  (let* ((p (bb-pos in)) (n (- (bb-limit in) p))
         (bv (make-bytevector n)))
    (do ((i 0 (fx+ i 1))) ((fx=? i n)) (bytevector-u8-set! bv i (decoder-byte in (+ p i))))
    (bb-pos! in (bb-limit in))
    (let ((s (decode-bytevector bv (list (charset-name (decoder-charset d)))))
          (put (nb-char-appender out)))
      (let loop ((i 0))
        (cond ((fx>=? i (string-length s)) coder-underflow)
              ((put (string-ref s i)) (loop (fx+ i 1)))
              (else coder-overflow))))))

(define (decoder-decode d in out end-of-input?)
  (if (decoder-kind (charset-name (decoder-charset d)))
      (decoder-decode-into d in out (jolt-truthy? end-of-input?))
      (decoder-decode-bulk d in out)))

;; decode(ByteBuffer) — the whole-input convenience form. It sizes its own output
;; and hands back a CharBuffer flipped and ready to read, and an error result is
;; raised rather than returned, as the JVM's is.
(define (decoder-decode-all d in)
  (let* ((n (max 1 (- (bb-limit in) (bb-pos in))))
         ;; one char per byte is the ceiling for every charset here: a multi-byte
         ;; sequence decodes to one char, and a replacement is one char too.
         (out (nb-char-allocate n))
         (r (decoder-decode d in out #t)))
    (when (coder-result-error? r) (coder-result-throw r))
    (bb-limit! out (bb-pos out))
    (bb-pos! out 0)
    out))

(define (charset-decoder-jhost? x) (and (jhost? x) (string=? (jhost-tag x) "charset-decoder")))
(define (charset-encoder-jhost? x) (and (jhost? x) (string=? (jhost-tag x) "charset-encoder")))
;; A decoder's malformed-input action is REPORT until onMalformedInput says otherwise.
(define (decoder-reports-malformed? d)
  (string=? (coding-error-action-name (decoder-malformed-action d)) "REPORT"))
;; Text through a charset's REPORTING coder, the one java.nio.file's Files and
;; a reader or writer built on a CharsetDecoder or CharsetEncoder use: malformed
;; input is MalformedInputException, and a character the charset cannot hold is
;; UnmappableCharacterException, each with the JDK's "Input length = N", where
;; String's own conversions replace them.
(define (charset-decode-report bv cs-name)
  (if (and (string=? (charset-canonical-down cs-name) "utf-8") (%utf8-java-plain? bv))
      (utf8->string bv)
      (let* ((decoder (make-jhost "charset-decoder" (vector (charset-for-name cs-name) #f #f "\xFFFD;")))
             (decoded (decoder-decode-all decoder
                                          (nb-wrap-bytes (na-bv->bytearray bv) 0 (bytevector-length bv)))))
        (nb-char-string decoded (bb-pos decoded) (bb-limit decoded)))))
(define (charset-report-malformed n) (coder-result-throw (coder-malformed n)))
(define (charset-report-unmappable c)
  ;; the length is the character's UTF-16 units
  (coder-result-throw (coder-unmappable (if (fx>=? (char->integer c) #x10000) 2 1))))
(define (charset-encode-report s cs-name)
  (let* ((name (charset-arg-name cs-name))
         (cs (charset-canonical-down name)))
    (io-check-encoding! name)
    (cond
      ((member cs '("iso-8859-1" "us-ascii"))
       (let ((top (if (string=? cs "us-ascii") 127 255)))
         (string-for-each (lambda (c) (when (fx>? (char->integer c) top) (charset-report-unmappable c))) s)
         (charset-encode-bv s name)))
      ;; every Unicode encoding holds every character a string can
      ((member cs '("utf-8" "utf-16" "utf-16be" "utf-16le" "utf-32" "utf-32be" "utf-32le"))
       (charset-encode-bv s name))
      ;; iconv refuses a character its target lacks; find which one, so the
      ;; length is that character's
      (else
       (or (iconv-bytes (string->utf8 s) "UTF-8" name)
           (begin
             (string-for-each
              (lambda (c) (unless (iconv-bytes (string->utf8 (string c)) "UTF-8" name)
                            (charset-report-unmappable c)))
              s)
             (charset-encode-bv s name)))))))

(register-host-methods! "charset-decoder"
  (list
   (cons "decode" (lambda (self in . rest)
                    (if (null? rest)
                        (decoder-decode-all self in)
                        (decoder-decode self in (car rest) (cadr rest)))))
   ;; flush has nothing to write out: the decoders here keep no pending state
   ;; between calls (the partial sequence stays in the caller's input buffer).
   (cons "flush" (lambda (self out) coder-underflow))
   (cons "reset" (lambda (self) self))
   (cons "onMalformedInput" (lambda (self a) (vector-set! (jhost-state self) 1 a) self))
   (cons "onUnmappableCharacter" (lambda (self a) (vector-set! (jhost-state self) 2 a) self))
   (cons "malformedInputAction" decoder-malformed-action)
   (cons "unmappableCharacterAction" decoder-unmappable-action)
   (cons "replaceWith" (lambda (self s) (vector-set! (jhost-state self) 3 (jolt-str-render-one s)) self))
   (cons "replacement" decoder-replacement)
   (cons "averageCharsPerByte" (lambda (self) (->num 1)))
   (cons "maxCharsPerByte" (lambda (self) (->num 1)))
   (cons "toString" host-object-tostring)))
