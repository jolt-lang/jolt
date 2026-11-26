;; zip-deflater.ss — java.util.zip.Deflater (JDK 21 Deflater.java, Deflater.c).
;; Uses zip-inflater.ss's window, output and throw helpers, and zip-base.ss's
;; method arguments.

;; The fields of JDK 21 Deflater. SET-PARAMS is true while a level or strategy
;; change waits for the next deflate call. APPLIED-LEVEL and APPLIED-STRATEGY
;; are the ones zlib uses now. STARTED is true once zlib has deflated since init
;; or reset. FINISH-SENT is true once zlib has been given Z_FINISH: zlib then
;; takes no other flush mode.
(define-record-type zdeflater
  (fields zs mu
          (mutable input) (mutable pos) (mutable lim)
          (mutable input-buffer)          ; the ByteBuffer INPUT was copied from, or #f
          (mutable level) (mutable strategy) (mutable set-params)
          (mutable applied-level) (mutable applied-strategy) (mutable started)
          (mutable finish) (mutable finish-sent) (mutable finished)
          (mutable read) (mutable written))
  (nongenerative jolt-zdeflater-v4))

;; The input ByteBuffer's position follows what the engine consumed.
(define (zdeflater-sync-buffer! st)
  (let ((b (zdeflater-input-buffer st)))
    (when b (zip-buffer-consumed! b (- (zdeflater-lim st) (zdeflater-pos st))))))

;; The input one zlib call gets: at most SIZE bytes of ARRAY from POS to LIM.
(define (zdeflater-window array pos lim size)
  (let ((n (min (- lim pos) size)))
    (if (<= n 0)
        (make-bytevector 0)
        (let ((out (make-bytevector n)))
          (bytevector-copy! (zip-bytes "input" array) pos out 0 n)
          out))))

;; zlib's compression function for LEVEL: stored (0), fast (1-3) or slow (4-9);
;; -1 is level 6 (deflate.c configuration_table).
(define (zip-deflate-func level)
  (let ((l (if (= level -1) 6 level)))
    (cond ((= l 0) 0) ((<= l 3) 1) (else 2))))

(define (deflater-locked self f)
  (let ((st (jhost-state self)))
    (jolt-with-mutex (zdeflater-mu st) (f st))))

(define (deflater-open self f)
  (deflater-locked self
    (lambda (st)
      (unless (zstream-open? (zdeflater-zs st))
        (zip-throw "java.lang.NullPointerException" "Deflater has been closed"))
      (f st))))

;; Deflater() | Deflater(int level) | Deflater(int level, boolean nowrap). A level
;; zlib refuses is IllegalArgumentException with no message (Deflater.c init).
(define (make-deflater . args)
  (let-values (((level nowrap)
                (case (length args)
                  ((0) (values -1 #f))
                  ((1) (values (zip-ctor-int-arg (car args)) #f))
                  ((2) (let* ((level (zip-ctor-int-arg (car args)))
                              (nowrap (zip-boolean-arg (cadr args))))
                         (values level nowrap)))
                  (else (throw-jvm 'IllegalArgumentException
                                   "No matching ctor found for class java.util.zip.Deflater")))))
    (let-values (((zs code msg) (zstream-open 'deflate (if nowrap -15 15) level 0)))
      (unless zs
        (cond ((= code z-mem-error) (zip-throw "java.lang.OutOfMemoryError" #f))
              ((= code z-stream-error) (zip-throw "java.lang.IllegalArgumentException" #f))
              (else (zip-throw "java.lang.InternalError"
                               (or msg
                                   (if (= code z-version-error)
                                       zip-version-error-message
                                       "unknown error initializing zlib library"))))))
      (let* ((st (make-zdeflater zs (make-mutex) (make-bytevector 0) 0 0 #f
                                 level 0 #f level 0 #f #f #f #f 0 0))
             (self (make-jhost "zip-deflater" st)))
        (zstream-guard! self zs (zdeflater-mu st))
        self))))

;; setInput(byte[]) | setInput(byte[], off, len)
(define (deflater-set-input! self . args)
  (if (and (null? (cdr args)) (bb? (car args)))
      ;; setInput(ByteBuffer): the remaining bytes, and the buffer to advance
      (let ((arr (zip-buffer-remaining-array (car args))))
        (deflater-locked self
          (lambda (st)
            (zdeflater-input-set! st arr)
            (zdeflater-pos-set! st 0)
            (zdeflater-lim-set! st (ja-len arr))
            (zdeflater-input-buffer-set! st (car args))))
        jolt-nil)
      (let-values (((bv off len more) (zip-array-args "setInput" self args "input")))
        (deflater-locked self
          (lambda (st)
            (zdeflater-input-set! st (car args))
            (zdeflater-pos-set! st off)
            (zdeflater-lim-set! st (+ off len))
            (zdeflater-input-buffer-set! st #f)))
        jolt-nil)))

;; setDictionary(byte[]) | setDictionary(byte[], off, len); the result check is
;; Deflater.c checkSetDictionaryResult.
(define (deflater-set-dictionary! self . args)
  (let-values (((bv off len more) (zip-array-args "setDictionary" self args "dictionary" "Cannot read the array length because \"dictionary\" is null")))
    (deflater-open self
      (lambda (st)
        (let* ((zs (zdeflater-zs st))
               (dict (let ((d (make-bytevector len))) (bytevector-copy! bv off d 0 len) d))
               (code (zstream-set-dictionary! zs dict)))
          (cond ((= code z-ok) #t)
                ((= code z-stream-error) (zip-throw "java.lang.IllegalArgumentException" #f))
                (else (zip-throw "java.lang.InternalError"
                                 (or (zstream-message zs) "unknown error in checkSetDictionaryResult")))))))
    jolt-nil))

(define (deflater-set-strategy! self strategy)
  (let ((s (zip-int-arg strategy)))
    (unless (memv s '(0 1 2))
      (zip-throw "java.lang.IllegalArgumentException" #f))
    (deflater-locked self
      (lambda (st)
        (unless (= s (zdeflater-strategy st))
          (zdeflater-strategy-set! st s)
          (zdeflater-set-params-set! st #t))))
    jolt-nil))

(define (deflater-set-level! self level)
  (let ((l (zip-int-arg level)))
    (when (and (or (< l 0) (> l 9)) (not (= l -1)))
      (zip-throw "java.lang.IllegalArgumentException" "invalid compression level"))
    (deflater-locked self
      (lambda (st)
        (unless (= l (zdeflater-level st))
          (zdeflater-level-set! st l)
          (zdeflater-set-params-set! st #t))))
    jolt-nil))

;; deflate(ByteBuffer) | deflate(ByteBuffer, int): the output goes into an array
;; of the buffer's remaining room through the byte[] overload, then into the
;; buffer, whose position advances by what was produced (Deflater.java lines
;; 700-760). The call casts both arguments first: nil passes the ByteBuffer
;; cast, so the flush is cast before the null check.
(define (deflater-deflate-buffer self b flush)
  (cond
    ((jolt-nil? b)
     (zip-int-arg flush)
     (throw-jvm 'NullPointerException
                "Cannot invoke \"java.nio.ByteBuffer.isReadOnly()\" because \"output\" is null"))
    ((bb? b)
     (let* ((arr (na-byte-array (max 0 (- (bb-limit b) (bb-pos b)))))
            (n (jnum->exact (deflater-deflate self arr (->num 0) (->num (ja-len arr)) flush))))
       (zip-buffer-put! b arr n)
       (->num n)))
    (else (zip-class-cast b "java.nio.ByteBuffer"))))

;; deflate(byte[]) | deflate(byte[], off, len) | deflate(byte[], off, len, flush):
;; Deflater.deflate over Deflater.c doDeflate and checkDeflateStatus. A pending
;; level or strategy change runs deflateParams instead of deflate.
;;
;; The JDK hands zlib all remaining input in one call. Here zlib gets at most one
;; window (zip-input-window) per zlib call, so one Java call offers the next
;; window while zlib took all of the last one and output room remains. Only the
;; last window gets the caller's flush mode or Z_FINISH: an earlier one gets
;; Z_NO_FLUSH, or zlib would flush or end the stream with input unread. A
;; pending parameter change deflates the earlier windows with the old
;; parameters and runs deflateParams on the last. So a caller that stops when a
;; call returns 0, or fewer bytes than it asked for, sees what the JDK gives.
;; zlib's deflateParams deflates pending input only when zlib has deflated since
;; init or reset and the change moves to another compression function or
;; strategy. Otherwise it only records the change and takes no input, so that
;; call runs alone, whatever input is pending, as the JDK's one call does.
;; At level 0 zlib copies input straight to the output and then fills its own
;; 64 KiB window from the input left, with no output room (deflate.c
;; deflate_stored), so a stored call is offered the output room plus a window.
(define (deflater-deflate self . args)
  (cond
    ((= (length args) 2)
     (deflater-deflate-buffer self (car args) (cadr args)))
    ((and (= (length args) 1) (bb? (car args)))
     (deflater-deflate-buffer self (car args) (->num z-no-flush)))
    (else
     (let ((n (apply deflater-deflate-array self args)))
       (deflater-locked self zdeflater-sync-buffer!)
       n))))
(define (deflater-deflate-array self . args)
  (let-values (((out-bv off len more) (zip-array-args "deflate" self args "output" "Cannot invoke \"java.nio.ByteBuffer.isReadOnly()\" because \"output\" is null")))
    (let ((out (car args))
          (flush (if (pair? more) (car more) z-no-flush)))
      (unless (memv flush (list z-no-flush z-sync-flush z-full-flush))
        (zip-throw "java.lang.IllegalArgumentException" #f))
      (deflater-open self
        (lambda (st)
          (let ((zs (zdeflater-zs st)))
            (let loop ((total 0))
              (let* ((pos (zdeflater-pos st))
                     (lim (zdeflater-lim st))
                     (size (if (= (zip-deflate-func (zdeflater-applied-level st)) 0)
                               (+ (- len total) zip-input-window)
                               zip-input-window))
                     (in (zdeflater-window (zdeflater-input st) pos lim size))
                     (last? (<= (- lim pos) size))
                     (pending? (zdeflater-set-params st))
                     (quiet? (and pending?
                                  (or (not (zdeflater-started st))
                                      (and (= (zdeflater-strategy st) (zdeflater-applied-strategy st))
                                           (= (zip-deflate-func (zdeflater-level st))
                                              (zip-deflate-func (zdeflater-applied-level st)))))))
                     (params? (and pending? (or quiet? last?)))
                     (mode (cond ((zdeflater-finish-sent st) z-finish)
                                 ((not last?) z-no-flush)
                                 ((zdeflater-finish st) z-finish)
                                 (else flush))))
                (let-values (((code consumed produced bytes)
                              (if params?
                                  (zstream-params! zs (zdeflater-level st) (zdeflater-strategy st)
                                                   in (- len total))
                                  (zstream-step! zs mode in (- len total)))))
                  (unless (if params?
                              (memv code (list z-ok z-buf-error))
                              (memv code (list z-ok z-stream-end z-buf-error)))
                    (zip-throw "java.lang.InternalError"
                               (or (zstream-message zs)
                                   (if params?
                                       "unknown error in checkDeflateStatus, setParams case"
                                       "unknown error in checkDeflateStatus"))))
                  (zip-store! out (+ off total) bytes produced)
                  (unless params?
                    (when (> (- len total) 0) (zdeflater-started-set! st #t))
                    (when (= mode z-finish) (zdeflater-finish-sent-set! st #t))
                    (when (= code z-stream-end) (zdeflater-finished-set! st #t)))
                  (when (and params? (= code z-ok))
                    (zdeflater-set-params-set! st #f)
                    (zdeflater-applied-level-set! st (zdeflater-level st))
                    (zdeflater-applied-strategy-set! st (zdeflater-strategy st)))
                  (zdeflater-pos-set! st (+ pos consumed))
                  (zdeflater-read-set! st (+ (zdeflater-read st) consumed))
                  (zdeflater-written-set! st (+ (zdeflater-written st) produced))
                  (let ((total (+ total produced)))
                    (if (and (not last?)
                             (not params?)
                             (= consumed (bytevector-length in))
                             (< total len))
                        (loop total)
                        (->num total))))))))))))

(define (deflater-needs-input? self)
  (deflater-locked self (lambda (st) (= (zdeflater-lim st) (zdeflater-pos st)))))
(define (deflater-finish! self)
  (deflater-locked self (lambda (st) (zdeflater-finish-set! st #t)))
  jolt-nil)
(define (deflater-finished? self) (deflater-locked self zdeflater-finished))
(define (deflater-bytes-read self) (deflater-open self zdeflater-read))
(define (deflater-bytes-written self) (deflater-open self zdeflater-written))

(define (deflater-reset! self)
  (deflater-open self
    (lambda (st)
      (unless (= (zstream-reset! (zdeflater-zs st)) z-ok)
        (zip-throw "java.lang.InternalError" "deflateReset failed"))
      (zdeflater-finish-set! st #f)
      (zdeflater-finish-sent-set! st #f)
      (zdeflater-started-set! st #f)
      (zdeflater-finished-set! st #f)
      (zdeflater-input-set! st (make-bytevector 0))
      (zdeflater-pos-set! st 0)
      (zdeflater-lim-set! st 0)
      (zdeflater-read-set! st 0)
      (zdeflater-written-set! st 0)))
  jolt-nil)

(define (deflater-end! self)
  (deflater-locked self
    (lambda (st)
      (let ((code (zstream-close! (zdeflater-zs st))))
        (zdeflater-input-set! st (make-bytevector 0))
        (zdeflater-pos-set! st 0)
        (zdeflater-lim-set! st 0)
        (when (= code z-stream-error) (zip-throw "java.lang.InternalError" "deflateEnd failed")))))
  jolt-nil)

(hashtable-set! jhost-tag->fqn "zip-deflater" "java.util.zip.Deflater")
(register-host-methods! "zip-deflater"
  (list
   (cons "setInput" (zip-method "setInput" '(1 3) deflater-set-input!))
   (cons "setDictionary" (zip-method "setDictionary" '(1 3) deflater-set-dictionary!))
   (cons "setStrategy" (zip-method "setStrategy" '(1) deflater-set-strategy!))
   (cons "setLevel" (zip-method "setLevel" '(1) deflater-set-level!))
   (cons "needsInput" (zip-method "needsInput" '(0) deflater-needs-input?))
   (cons "finish" (zip-method "finish" '(0) deflater-finish!))
   (cons "finished" (zip-method "finished" '(0) deflater-finished?))
   (cons "deflate" (zip-method "deflate" '(1 2 3 4) deflater-deflate))
   (cons "getAdler"
         (zip-method "getAdler" '(0)
           (lambda (self)
             (deflater-open self (lambda (st) (->num (jolt-s32 (zstream-adler (zdeflater-zs st)))))))))
   (cons "getTotalIn" (zip-method "getTotalIn" '(0) (lambda (self) (->num (jolt-s32 (deflater-bytes-read self))))))
   (cons "getBytesRead" (zip-method "getBytesRead" '(0) (lambda (self) (->num (deflater-bytes-read self)))))
   (cons "getTotalOut" (zip-method "getTotalOut" '(0) (lambda (self) (->num (jolt-s32 (deflater-bytes-written self))))))
   (cons "getBytesWritten" (zip-method "getBytesWritten" '(0) (lambda (self) (->num (deflater-bytes-written self)))))
   (cons "reset" (zip-method "reset" '(0) deflater-reset!))
   (cons "end" (zip-method "end" '(0) deflater-end!))))
(register-class-statics! "java.util.zip.Deflater"
  (list (cons "DEFLATED" (->num 8))
        (cons "NO_COMPRESSION" (->num 0))
        (cons "BEST_SPEED" (->num 1))
        (cons "BEST_COMPRESSION" (->num 9))
        (cons "DEFAULT_COMPRESSION" (->num -1))
        (cons "FILTERED" (->num 1))
        (cons "HUFFMAN_ONLY" (->num 2))
        (cons "DEFAULT_STRATEGY" (->num 0))
        (cons "NO_FLUSH" (->num 0))
        (cons "SYNC_FLUSH" (->num 2))
        (cons "FULL_FLUSH" (->num 3))))
(reg-ctor! '("Deflater" "java.util.zip.Deflater") make-deflater)
