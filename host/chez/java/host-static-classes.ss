;; host-static-classes.ss — instantiable host object classes: ArrayList, HashMap,
;; the String/Reader/Writer/Tokenizer shims, BigInteger/MapEntry ctors, and URL
;; codecs. Holds the tagged-table method dispatch (the (.method ...) arm on a jhost)
;; and the pluggable instance? hook. Loaded after host-static-methods.ss; the
;; `Class/member` static methods live there, the registry core in host-static.ss.

;; ---- java.util.ArrayList ----------------------------------------------------
;; A mutable list backed by a growable Scheme vector. State is #(backing count);
;; .add amortizes O(1) and .get is O(1) (a list backing made both O(n)). medley's
;; stateful transducers (window / partition-between) build one with .add / .size /
;; .toArray / .clear / .remove. (ArrayList.) | (ArrayList. n) | (ArrayList. coll).
;; State: #(backing-vector count head). Logical index i lives at physical
;; head+i. The head offset makes FRONT operations O(1) — removeFirst/poll/pop
;; advance head, addFirst/push retreat into head slack — where shifting the
;; whole vector made the standard deque worklist idiom (.poll + .add, or
;; tools.reader's .remove(0) on its pending-splice LinkedList) O(n^2). When an
;; append finds the physical end full it compacts into the head slack if that
;; is at least half the buffer (each compacted element was paid for by a prior
;; front-removal, so appends stay amortized O(1)), and grows otherwise.
(define al-min-cap 8)
(define (al-vec self) (vector-ref (jhost-state self) 0))
(define (al-cnt self) (vector-ref (jhost-state self) 1))
(define (al-cnt! self n) (vector-set! (jhost-state self) 1 n))
(define (al-head self) (vector-ref (jhost-state self) 2))
(define (al-head! self h) (vector-set! (jhost-state self) 2 h))
(define (al-ref self i) (vector-ref (al-vec self) (fx+ (al-head self) i)))
(define (al-set! self i x) (vector-set! (al-vec self) (fx+ (al-head self) i) x))
(define (make-arraylist xs)               ; xs: a Scheme list of initial elements
  (let* ((n (length xs)) (cap (fxmax al-min-cap n)) (v (make-vector cap jolt-nil)))
    (let loop ((i 0) (xs xs)) (when (pair? xs) (vector-set! v i (car xs)) (loop (fx+ i 1) (cdr xs))))
    (make-jhost "arraylist" (vector v n 0))))
;; room for one more element at the physical tail: compact into head slack or
;; grow (doubling), either way head returns to 0.
(define (al-ensure-tail! self)
  (let* ((v (al-vec self)) (n (al-cnt self)) (h (al-head self)))
    (when (fx=? (fx+ h n) (vector-length v))
      (if (fx>=? h n)
          (begin
            (do ((i 0 (fx+ i 1))) ((fx=? i n)) (vector-set! v i (vector-ref v (fx+ h i))))
            (do ((i n (fx+ i 1))) ((fx=? i (fx+ h n))) (vector-set! v i jolt-nil))
            (al-head! self 0))
          (let ((nv (make-vector (fxmax al-min-cap (fx* 2 (vector-length v))) jolt-nil)))
            (do ((i 0 (fx+ i 1))) ((fx=? i n)) (vector-set! nv i (vector-ref v (fx+ h i))))
            (vector-set! (jhost-state self) 0 nv)
            (al-head! self 0))))))
(define (al-push! self x)
  (al-ensure-tail! self)
  (let ((n (al-cnt self)))
    (vector-set! (al-vec self) (fx+ (al-head self) n) x)
    (al-cnt! self (fx+ n 1))))
(define (al-insert-at! self i x)
  (let ((n (al-cnt self)) (h (al-head self)))
    (if (and (fx=? i 0) (fx>? h 0))
        (begin (al-head! self (fx- h 1))
               (vector-set! (al-vec self) (fx- h 1) x)
               (al-cnt! self (fx+ n 1)))
        (begin
          (al-ensure-tail! self)
          (let ((v (al-vec self)) (h (al-head self)))
            (let shift ((j (fx+ h n))) (when (fx>? j (fx+ h i)) (vector-set! v j (vector-ref v (fx- j 1))) (shift (fx- j 1))))
            (vector-set! v (fx+ h i) x) (al-cnt! self (fx+ n 1)))))))
(define (al-remove-at! self i)
  (let ((n (al-cnt self)) (v (al-vec self)) (h (al-head self)))
    (if (fx=? i 0)
        (begin (vector-set! v h jolt-nil)
               (al-cnt! self (fx- n 1))
               ;; empty resets head so a drained deque reuses the whole buffer
               (al-head! self (if (fx=? n 1) 0 (fx+ h 1))))
        (begin
          (let shift ((j (fx+ h i))) (when (fx<? j (fx+ h (fx- n 1))) (vector-set! v j (vector-ref v (fx+ j 1))) (shift (fx+ j 1))))
          (vector-set! v (fx+ h (fx- n 1)) jolt-nil) (al-cnt! self (fx- n 1))))))
(define (al->list self)                   ; the `count` live elements as a Scheme list
  (let ((v (al-vec self)) (h (al-head self)))
    (let loop ((i (fx- (al-cnt self) 1)) (acc '())) (if (fx<? i 0) acc (loop (fx- i 1) (cons (vector-ref v (fx+ h i)) acc))))))
(register-class-ctor! "ArrayList"
  (lambda args
    (cond ((null? args) (make-arraylist '()))
          ((number? (car args)) (make-arraylist '()))   ; initial capacity, ignored
          (else (make-arraylist (seq->list (jolt-seq (car args))))))))
(register-class-ctor! "java.util.ArrayList"
  (lambda args
    (cond ((null? args) (make-arraylist '()))
          ((number? (car args)) (make-arraylist '()))
          (else (make-arraylist (seq->list (jolt-seq (car args))))))))
(define arraylist-methods
  (list
    (cons "add" (lambda (self . a)
                  ;; (.add x) -> append+true; (.add i x) -> insert at i, returns nil.
                  (if (= 1 (length a))
                      (begin (al-push! self (car a)) #t)
                      (begin (al-insert-at! self (jnum->exact (car a)) (cadr a)) jolt-nil))))
    (cons "add!" (lambda (self x) (al-push! self x) #t))
    (cons "addAll" (lambda (self . a)
                     ;; (.addAll coll) appends; (.addAll i coll) inserts at i.
                     (let* ((at-i (= 2 (length a)))
                            (i (if at-i (jnum->exact (car a)) (al-cnt self)))
                            (coll (if at-i (cadr a) (car a))))
                       (let loop ((xs (seq->list (jolt-seq coll))) (k i))
                         (if (null? xs) (pair? (seq->list (jolt-seq coll)))
                             (begin (al-insert-at! self k (car xs)) (loop (cdr xs) (fx+ k 1))))))))
    (cons "get" (lambda (self i) (al-ref self (jnum->exact i))))
    (cons "set" (lambda (self i x)
                  (let* ((idx (jnum->exact i)) (old (al-ref self idx)))
                    (al-set! self idx x) old)))
    (cons "size" (lambda (self) (->num (al-cnt self))))
    (cons "isEmpty" (lambda (self) (fx=? 0 (al-cnt self))))
    (cons "remove" (lambda (self i)
                     (let* ((idx (jnum->exact i)) (old (al-ref self idx)))
                       (al-remove-at! self idx) old)))
    (cons "clear" (lambda (self) (vector-set! (jhost-state self) 0 (make-vector al-min-cap jolt-nil)) (al-cnt! self 0) (al-head! self 0) jolt-nil))
    (cons "contains" (lambda (self x) (and (memp (lambda (e) (jolt=2 e x)) (al->list self)) #t)))
    (cons "toArray" (lambda (self . _) (apply jolt-vector (al->list self))))
    (cons "iterator" (lambda (self) (make-jiterator (list->cseq (al->list self)))))
    (cons "toString" (lambda (self) (jolt-pr-str (list->cseq (al->list self)))))))
;; java.util.SequencedCollection (JDK 21): List and Deque both have it, so the
;; first/last accessors and mutators sit on the shared ArrayList table and
;; LinkedList / ArrayDeque inherit them. On an empty list the accessors raise
;; NoSuchElementException, as on the JVM. reversed() — a live reverse-order VIEW
;; of a mutable list — is not modeled: a copy would silently detach from the
;; list it claims to view.
(define (al-first self)
  (if (fx=? 0 (al-cnt self)) (throw-jvm 'NoSuchElementException "") (al-ref self 0)))
(define (al-last self)
  (if (fx=? 0 (al-cnt self)) (throw-jvm 'NoSuchElementException "") (al-ref self (fx- (al-cnt self) 1))))
(define sequenced-list-methods
  (list
    (cons "addFirst" (lambda (self x) (al-insert-at! self 0 x) jolt-nil))
    (cons "addLast" (lambda (self x) (al-push! self x) jolt-nil))
    (cons "removeFirst" (lambda (self) (let ((o (al-first self))) (al-remove-at! self 0) o)))
    (cons "removeLast" (lambda (self) (let ((o (al-last self))) (al-remove-at! self (fx- (al-cnt self) 1)) o)))
    (cons "getFirst" al-first) (cons "getLast" al-last)))
(register-host-methods! "arraylist" (append arraylist-methods sequenced-list-methods))

;; java.util.LinkedList: the ArrayList backing plus the Deque surface
;; (offer/peek/poll/push/pop over the sequenced methods above).
;; tools.reader holds pending splice forms in one and (seq)s / .remove(0)s it.
(define linkedlist-methods
  (append arraylist-methods sequenced-list-methods
    (list
      (cons "offer" (lambda (self x) (al-push! self x) #t))
      (cons "peek" (lambda (self) (if (fx=? 0 (al-cnt self)) jolt-nil (al-first self))))
      (cons "poll" (lambda (self) (if (fx=? 0 (al-cnt self)) jolt-nil (let ((o (al-first self))) (al-remove-at! self 0) o))))
      (cons "push" (lambda (self x) (al-insert-at! self 0 x) jolt-nil))
      (cons "pop" (lambda (self) (let ((o (al-first self))) (al-remove-at! self 0) o))))))
(define (make-linkedlist xs)
  (let ((al (make-arraylist xs))) (make-jhost "linkedlist" (jhost-state al))))
(register-host-methods! "linkedlist" linkedlist-methods)
(let ((ctor (lambda args
              (cond ((null? args) (make-linkedlist '()))
                    (else (make-linkedlist (seq->list (jolt-seq (car args)))))))))
  (register-class-ctor! "LinkedList" ctor)
  (register-class-ctor! "java.util.LinkedList" ctor))

;; ArrayDeque: the same deque surface over the growable-array backing. (An int
;; capacity arg is a hint on the JVM — an empty deque here.)
(define (make-arraydeque xs)
  (let ((al (make-arraylist xs))) (make-jhost "arraydeque" (jhost-state al))))
(register-host-methods! "arraydeque" linkedlist-methods)
(let ((ctor (lambda args
              (cond ((null? args) (make-arraydeque '()))
                    ((number? (car args)) (make-arraydeque '()))
                    (else (make-arraydeque (seq->list (jolt-seq (car args)))))))))
  (register-class-ctor! "ArrayDeque" ctor)
  (register-class-ctor! "java.util.ArrayDeque" ctor))

;; ArrayList / LinkedList are Iterable: (seq al) walks the elements (nil if empty),
;; so (seq pending-forms) and reduce/into over one work like the JVM.
(define (al-family? x)
  (and (jhost? x) (or (string=? (jhost-tag x) "arraylist")
                       (string=? (jhost-tag x) "linkedlist")
                       (string=? (jhost-tag x) "arraydeque"))))
(register-seq-arm! al-family? (lambda (x) (list->cseq (al->list x))))

;; Appendable.append text: append(x) renders x; append(csq,start,end) appends the
;; subsequence csq[start,end) (data.json's writer appends string runs this way).
(define (append-text x rest)
  (if (null? rest)
      (render-piece x)
      (substring (render-piece x) (jnum->exact (car rest)) (jnum->exact (cadr rest)))))

;; Every index-taking StringBuilder method reports the same way the JVM does.
(define (sb-range-check s start end)
  (let ((n (string-length s)))
    (when (or (< start 0) (> end n) (> start end))
      (throw-jvm (quote StringIndexOutOfBoundsException)
                 (string-append "start " (number->string start) ", end " (number->string end)
                                ", length " (number->string n))))))

(register-class-ctor! "StringBuilder"
  (lambda args (make-jhost "string-builder"
    ;; a numeric first arg is a CAPACITY hint, not content.
    (vector (if (and (pair? args) (not (number? (car args)))) (render-piece (car args)) "")
            '() 0))))
(register-host-methods! "string-builder"
  (list (cons "append" (lambda (self x . rest) (sb-append! self (append-text x rest)) self))
        (cons "toString" (lambda (self) (sb-str self)))
        (cons "length" (lambda (self) (->num (sb-length self))))
        (cons "charAt" (lambda (self i) (string-ref (sb-str self) (jnum->exact i))))
        (cons "setLength" (lambda (self n)
                            (let ((cur (sb-str self)) (n (jnum->exact n)))
                              (sb-set! self (if (< n (string-length cur))
                                                (substring cur 0 n)
                                                (string-append cur (make-string (- n (string-length cur)) #\nul)))))
                            jolt-nil))
        (cons "isEmpty" (lambda (self) (= 0 (sb-length self))))
        (cons "substring" (lambda (self start . rest)
                            (let* ((cur (sb-str self)) (s (jnum->exact start))
                                   (e (if (null? rest) (string-length cur) (jnum->exact (car rest)))))
                              (sb-range-check cur s e)
                              (substring cur s e))))
        ;; CharSequence.subSequence — AbstractStringBuilder returns substring(a, b),
        ;; i.e. a String, which is itself a CharSequence.
        (cons "subSequence" (lambda (self a b)
                              (let* ((cur (sb-str self)) (s (jnum->exact a)) (e (jnum->exact b)))
                                (sb-range-check cur s e)
                                (substring cur s e))))
        (cons "indexOf" (lambda (self needle . rest)
                          (->num (str-index-of (sb-str self) (render-piece needle)
                                               (if (null? rest) 0 (jnum->exact (car rest)))))))
        (cons "lastIndexOf" (lambda (self needle)
                              (->num (str-last-index-of (sb-str self) (render-piece needle)))))
        (cons "setCharAt" (lambda (self i ch)
                            (let* ((cur (sb-str self)) (i (jnum->exact i)))
                              (sb-range-check cur i (+ i 1))
                              (sb-set! self (string-append (substring cur 0 i) (render-piece ch)
                                                           (substring cur (+ i 1) (string-length cur)))))
                            jolt-nil))
        (cons "deleteCharAt" (lambda (self i)
                               (let* ((cur (sb-str self)) (i (jnum->exact i)))
                                 (sb-range-check cur i (+ i 1))
                                 (sb-set! self (string-append (substring cur 0 i)
                                                              (substring cur (+ i 1) (string-length cur)))))
                               self))
        ;; delete clamps its end to the length, the way the JVM does.
        (cons "delete" (lambda (self start end)
                         (let* ((cur (sb-str self)) (n (string-length cur))
                                (s (jnum->exact start)) (e (min n (jnum->exact end))))
                           (sb-range-check cur s (max s e))
                           (sb-set! self (string-append (substring cur 0 s) (substring cur (max s e) n))))
                         self))
        (cons "replace" (lambda (self start end txt)
                          (let* ((cur (sb-str self)) (n (string-length cur))
                                 (s (jnum->exact start)) (e (min n (jnum->exact end))))
                            (sb-range-check cur s (max s e))
                            (sb-set! self (string-append (substring cur 0 s) (render-piece txt)
                                                         (substring cur (max s e) n))))
                          self))
        (cons "insert" (lambda (self offset x . rest)
                         (let* ((cur (sb-str self)) (n (string-length cur)) (i (jnum->exact offset)))
                           (sb-range-check cur i i)
                           (sb-set! self (string-append (substring cur 0 i) (append-text x rest)
                                                        (substring cur i n))))
                         self))
        (cons "reverse" (lambda (self)
                          (sb-set! self (list->string (reverse (string->list (sb-str self)))))
                          self))))
;; (str sb) / print a StringBuilder -> its accumulated content, like the JVM
;; (str calls toString). Without this str renders the opaque host object.
(define (sb-jhost? x) (and (jhost? x) (string=? (jhost-tag x) "string-builder")))
(register-str-render! sb-jhost? sb-str)
;; A StringBuilder IS a java.lang.CharSequence, so it answers (class …),
;; instance? through the class graph, and the three RT entry points that name a
;; CharSequence — count is its length, seq walks its characters, nth reads one.
;; Without the class arm (class sb) leaked the :object placeholder.
(register-class-arm! sb-jhost? (lambda (x) "java.lang.StringBuilder"))
;; An array class reaches instance-check as a raw string ("[C"), not a symbol, and
;; this arm is newer than the base taxonomy so it is asked first — hence the
;; symbol-t? guard before reading the name.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (sb-jhost? val) (symbol-t? type-sym))
        (jch-isa? "java.lang.StringBuilder" (symbol-t-name type-sym))
        'pass)))
(register-count-arm! sb-jhost? (lambda (x) (sb-length x)))
(register-seq-arm! sb-jhost? (lambda (x) (jolt-seq (sb-str x))))

;; ---- StringWriter -----------------------------------------------------------
;; Writer.write(int) writes the CHAR for that code; append(char) appends the char.
;; What one value puts into a stream. A number is the CHARACTER with that code
;; (write(int) writes one unit, not the digits). A char[] is its characters —
;; every write and print overload that takes one is defined that way, and it used
;; to render as "#object[[C …]" straight into the stream. A byte[] is NOT handled
;; here: print/println have no byte[] overload on the JVM and fall to
;; print(Object), so it renders as an object, and only write puts its bytes out.
(define (char-array-arg? x) (and (jolt-array? x) (eq? (jolt-array-kind x) 'char)))
(define (byte-array-arg? x) (and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)))
(define (char-array->string x)
  (list->string (map (lambda (c) (if (char? c) c (integer->char (jnum->exact c))))
                     (vector->list (jolt-array-vec x)))))
(define (writer-piece x)
  (cond ((number? x) (string (integer->char (jnum->exact x))))
        ((char-array-arg? x) (char-array->string x))
        (else (render-piece x))))
;; The bytes a byte[] .write argument carries, with the JVM's 3-arg (off len)
;; region applied. Out-of-range ends clamp rather than raise: a stream write is
;; not the place to turn a caller's off-by-one into a crash mid-output.
(define (byte-array-range x rest)
  (let ((bv (na-bytearray->bv x)))
    (if (and (pair? rest) (pair? (cdr rest)))
        (let* ((off (max 0 (jnum->exact (car rest))))
               (len (max 0 (jnum->exact (cadr rest))))
               (start (min off (bytevector-length bv)))
               (end (min (bytevector-length bv) (+ start len)))
               (sub (make-bytevector (- end start))))
          (bytevector-copy! bv start sub 0 (- end start))
          sub)
        bv)))
;; What a .write argument puts into a TEXT sink: writer-piece, plus the byte[]
;; whose CONTENT write is defined to emit — (.write w (.getBytes s)) is how
;; ported code writes raw output, and it used to put "#object[[B …]" in the
;; stream. Bytes decode as UTF-8, the encoding jolt renders in; they are sliced
;; before decoding, since the offsets are byte offsets and cutting the decoded
;; string would land in the middle of a multi-byte character. A sink with a real
;; descriptor under it takes the bytes themselves — see pw-write-bytes! below.
(define (writer-piece-range x rest)
  (cond
    ((byte-array-arg? x) (utf8->string (byte-array-range x rest)))
    ((and (pair? rest) (pair? (cdr rest)))
     (let* ((s (writer-piece x))
            (off (max 0 (jnum->exact (car rest))))
            (len (max 0 (jnum->exact (cadr rest))))
            (start (min off (string-length s)))
            (end (min (string-length s) (+ start len))))
       (substring s start end)))
    (else (writer-piece x))))
;; Same accumulator as StringBuilder, and for the same reason: writing to a
;; StringWriter a piece at a time — which is what printStackTrace and every
;; print-to-a-writer path does — used to copy the whole buffer per write.
(register-class-ctor! "StringWriter" (lambda args (make-jhost "writer" (vector "" '() 0))))
(register-host-methods! "writer"
  (list (cons "write" (lambda (self x . rest) (sb-append! self (writer-piece-range x rest)) jolt-nil))
        (cons "append" (lambda (self x . rest) (sb-append! self (append-text x rest)) self))
        (cons "flush" (lambda (self) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) (sb-str self)))))
;; (str sw) / print a StringWriter -> its accumulated content, like the JVM
;; (str calls toString) — data.csv writes CSV to a StringWriter and reads it back.
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "writer"))) sb-str)

;; a file-backed writer (clojure.java.io/writer of a File/path): accumulates like
;; StringWriter, then persists to the path on flush/close, so
;; (with-open [w (io/writer "f")] (.write w …)) writes the file. State #(path buf).
(define (fw-path self) (vector-ref (jhost-state self) 0))
(define (fw-buf self) (vector-ref (jhost-state self) 1))
(define (fw-append! self s) (vector-set! (jhost-state self) 1 (string-append (fw-buf self) s)))
(define (fw-flush! self) (jolt-spit (fw-path self) (fw-buf self)))  ; jolt-spit: io.ss
(register-host-methods! "file-writer"
  (list (cons "write" (lambda (self x . rest) (fw-append! self (writer-piece-range x rest)) jolt-nil))
        (cons "append" (lambda (self x . rest) (fw-append! self (append-text x rest)) self))
        (cons "flush" (lambda (self) (fw-flush! self) jolt-nil))
        (cons "close" (lambda (self) (fw-flush! self) jolt-nil))
        (cons "toString" (lambda (self) (fw-buf self)))))

;; a writer over a real Chez port — the values *out* / *err* hold. write/append
;; push to the port (so (.write *out* s) and (binding [*out* *err*] …) work);
;; it isn't a buffer, so toString is empty. Lets libraries that touch *out*/*err*
;; (tools.logging, selmer) compile and run.
;; *out*/*err* resolve their port LIVE — 'out -> (current-output-port), 'err ->
;; (current-error-port) — so a (.write *out* …) / (.flush *out*) follows a
;; with-out-str redirect (with-output-to-string rebinds current-output-port) the
;; same way print/__write do. Storing the startup port instead pinned *out* to the
;; real stdout, so rewrite-clj's (z/print) — which writes via *out* — escaped the
;; capture. A stored port object (should any other code make a port-writer) is used
;; as-is.
;;
;; 'stdout / 'stderr are the other half of that: the PROCESS streams, resolved
;; once at startup and never again. They back System/out and System/err, which
;; the JVM does NOT route through *out* — a with-out-str captures (print …) and
;; leaves (.println System/out …) going to the terminal. Resolving them live
;; through (current-output-port) like 'out made jolt capture both, so a library
;; that deliberately writes past the capture (a progress bar, a prompt, a logger
;; writing to the real stderr) had its output swallowed into the string instead.
(define port-writer-stdout (current-output-port))
(define port-writer-stderr (current-error-port))
(define (port-writer-port self)
  (let ((p (vector-ref (jhost-state self) 0)))
    (cond ((eq? p 'out) (current-output-port))
          ((eq? p 'err) (current-error-port))
          ((eq? p 'stdout) port-writer-stdout)
          ((eq? p 'stderr) port-writer-stderr)
          (else p))))

;; The process streams also have a BYTE path. System/out and System/err are
;; java.io.PrintStreams — OutputStreams — so bytes written to one have to reach
;; the descriptor as they are, and (io/copy System/in System/out) is the cat.
;; Decoding them as UTF-8 first replaced every byte that was not text with U+FFFD.
;; Chez's process port is textual and re-encodes whatever it is given, so the
;; bytes go out through a second port on the same descriptor. That port is
;; UNBUFFERED and the textual one is flushed ahead of it, so the two stay in the
;; order they were written even when stdout is a pipe.
;;
;; Only the real process stream takes this path: with-out-str resolves 'out to a
;; string port, which can only take characters, so a byte[] written there decodes
;; as before.
(define pw-byte-port-mu (make-mutex))
(define pw-stdout-bytes (box #f))
(define pw-stderr-bytes (box #f))
(define (pw-byte-port-memo cell open)
  (unless (unbox cell)
    (jolt-with-mutex pw-byte-port-mu
      (unless (unbox cell) (set-box! cell (open (buffer-mode none))))))
  (unbox cell))
(define (pw-byte-port port)
  (cond ((eq? port port-writer-stdout) (pw-byte-port-memo pw-stdout-bytes standard-output-port))
        ((eq? port port-writer-stderr) (pw-byte-port-memo pw-stderr-bytes standard-error-port))
        (else #f)))
;; Writes bv to the descriptor behind a process stream; #f when there is none, so
;; the caller falls back to writing it as text.
(define (pw-write-bytes! self bv)
  (let* ((port (port-writer-port self))
         (bp (pw-byte-port port)))
    (and bp
         (begin (flush-output-port port)
                (put-bytevector bp bv)
                #t))))
;; print / println / printf are PrintStream's, and System/out is a port-writer, so
;; ported code writing (.println System/out …) lands here. println with no argument
;; is the bare newline, as on the JVM.
(register-host-methods! "port-writer"
  (list (cons "write" (lambda (self x . rest)
                        (unless (and (byte-array-arg? x)
                                     (pw-write-bytes! self (byte-array-range x rest)))
                          (display (writer-piece-range x rest) (port-writer-port self)))
                        jolt-nil))
        (cons "append" (lambda (self x . rest) (display (append-text x rest) (port-writer-port self)) self))
        (cons "print" (lambda (self x) (display (writer-piece x) (port-writer-port self)) jolt-nil))
        (cons "println" (lambda (self . xs)
                          (let ((p (port-writer-port self)))
                            (unless (null? xs) (display (writer-piece (car xs)) p))
                            (display "\n" p))
                          jolt-nil))
        (cons "flush" (lambda (self) (flush-output-port (port-writer-port self)) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) ""))))
(def-dynvar! "clojure.core" "*out*" (make-jhost "port-writer" (vector 'out)))
(def-dynvar! "clojure.core" "*err*" (make-jhost "port-writer" (vector 'err)))

;; System/out and System/err — the process's own streams, so unlike *out*/*err*
;; they are NOT affected by a (binding [*out* …]) or a with-out-str, matching the
;; JVM. Ported code writes to them directly ((.println System/out …)). They are
;; port-writers over the 'stdout / 'stderr ports resolved at startup; io-streams.ss
;; then wraps each in the PrintStream shim, because on the JVM System.out is a
;; java.io.PrintStream and not the java.io.PrintWriter *out* is, and libraries
;; branch on that class.
;;
;; They live in the MUTABLE static cells rather than the plain statics table,
;; because setOut/setErr replace them wholesale — clojure.tools.logging's
;; log-capture! points them at a PrintStream over a proxy so every write becomes a
;; log record. Both spellings get the cell, since a read resolves the class name as
;; written.
(define system-out-writer (make-jhost "port-writer" (vector 'stdout)))
(define system-err-writer (make-jhost "port-writer" (vector 'stderr)))
(register-class-statics! "System"
  (list (cons "out" system-out-writer)
        (cons "err" system-err-writer)))
(define (sys-set-stream! member v)
  (for-each (lambda (c) (vector-set! (mutable-static-cell c member #t) 0 v))
            '("System" "java.lang.System"))
  jolt-nil)
(sys-set-stream! "out" system-out-writer)
(sys-set-stream! "err" system-err-writer)
(register-class-statics! "System"
  (list (cons "setOut" (lambda (v) (sys-set-stream! "out" v)))
        (cons "setErr" (lambda (v) (sys-set-stream! "err" v)))))

;; PrintWriter — a thin wrapper over a target writer. write/append/print forward
;; the rendered text to the target. clojure.data.json's pretty printer builds
;; (PrintWriter. *out*) where *out* is bound to clojure.pprint's pretty-writer (a
;; jolt record), so forwarding routes column-aware through clojure.pprint/-write;
;; for a host writer target it falls back to that writer's own write.
(define (pw-forward target s)
  (cond
    ;; through port-writer-port, not the raw slot: a port-writer holds the SYMBOL
    ;; 'out / 'err and resolves it per call, so a (PrintWriter. *out*) built inside
    ;; a with-out-str writes to the capture rather than past it.
    ((and (jhost? target) (string=? (jhost-tag target) "port-writer"))
     (display s (port-writer-port target)))
    ((and (jhost? target) (memv #t (list (string=? (jhost-tag target) "writer")
                                         (string=? (jhost-tag target) "string-builder"))))
     (sb-append! target s))
    ;; every other host writer knows how to write itself — a file-backed writer, an
    ;; OutputStreamWriter, a nested PrintWriter. Naming them one by one left
    ;; (PrintWriter. (io/writer f)) falling through to the pprint protocol below,
    ;; which a file writer does not implement.
    ((jhost? target) (record-method-dispatch target "write" (jolt-list s)))
    (else
     (jolt-invoke (var-deref "clojure.pprint" "-write") target s))))
(register-class-ctor! "PrintWriter"
  (lambda args (make-jhost "print-writer" (vector (if (pair? args) (car args) jolt-nil)))))
(register-class-ctor! "java.io.PrintWriter"
  (lambda args (make-jhost "print-writer" (vector (if (pair? args) (car args) jolt-nil)))))
(register-host-methods! "print-writer"
  ;; write takes (buf off len) and renders an int as its character; append takes
  ;; (csq start end) and renders everything as text. Sharing append-text between
  ;; them read the 3-arg write's LENGTH as an end index and printed (.write w 65)
  ;; as "65" rather than "A".
  (list (cons "write" (lambda (self x . rest) (pw-forward (vector-ref (jhost-state self) 0) (writer-piece-range x rest)) jolt-nil))
        (cons "print" (lambda (self x) (pw-forward (vector-ref (jhost-state self) 0) (render-piece x)) jolt-nil))
        (cons "append" (lambda (self x . rest) (pw-forward (vector-ref (jhost-state self) 0) (append-text x rest)) self))
        (cons "flush" (lambda (self) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) ""))))

;; PrintWriter-on — a writer that accumulates writes and, on flush, hands the
;; accumulated string to flush-fn and clears it; close calls close-fn if given.
;; (binding [*out* (PrintWriter-on f nil)] …) routes print/pr through it because
;; they write via the jhost's "write" method.
(define (pwo-buf self) (vector-ref (jhost-state self) 0))
(define (pwo-flush-fn self) (vector-ref (jhost-state self) 1))
(define (pwo-close-fn self) (vector-ref (jhost-state self) 2))
(define (jolt-print-writer-on flush-fn close-fn)
  (make-jhost "print-writer-on" (vector (box "") flush-fn close-fn)))
(register-host-methods! "print-writer-on"
  (list (cons "write" (lambda (self x . rest) (set-box! (pwo-buf self)
                                  (string-append (unbox (pwo-buf self)) (writer-piece-range x rest))) jolt-nil))
        (cons "append" (lambda (self x . rest) (set-box! (pwo-buf self)
                                  (string-append (unbox (pwo-buf self)) (append-text x rest))) self))
        (cons "flush" (lambda (self)
                        (let ((b (pwo-buf self)) (ff (pwo-flush-fn self)))
                          (unless (jolt-nil? ff) (jolt-invoke ff (unbox b)))
                          (set-box! b "") jolt-nil)))
        (cons "close" (lambda (self)
                        (let ((cf (pwo-close-fn self)))
                          (unless (jolt-nil? cf) (jolt-invoke cf)) jolt-nil)))
        (cons "toString" (lambda (self) (unbox (pwo-buf self))))))
(def-var! "clojure.core" "PrintWriter-on" jolt-print-writer-on)

;; ---- java.util.HashMap ------------------------------------------------------
;; A mutable map keyed by jolt values (jolt-hash / jolt=2). State #(chez-hashtable).
;; Constructors: () | (capacity) | (capacity load-factor) [sizing args ignored] |
;; (Map m) [copy]. Enough of the Map surface for libraries that build a fast lookup
;; (malli's fast-registry: (doto (HashMap. 1024 0.25) (.putAll m)) then .get).
(define (hm-hash k) (let ((h (jolt-hash k)))
                      (bitwise-and (if (and (integer? h) (exact? h)) (abs h) 0) #x3FFFFFFF)))
(define (hm-tbl self) (vector-ref (jhost-state self) 0))
;; insertion order for iteration (seq/keySet/values/entrySet render
;; deterministically; the JVM's hash order is arbitrary, insertion order is the
;; deterministic superset jolt's small maps already use).
(define (hm-ord self) (vector-ref (jhost-state self) 1))
(define (hm-ord! self v) (vector-set! (jhost-state self) 1 v))
(define (hm-note-key! self k)
  (when (not (hashtable-contains? (hm-tbl self) k))
    (hm-ord! self (cons k (hm-ord self)))))
(define (hm-drop-key! self k)
  (hm-ord! self (remove (lambda (e) (jolt=2 e k)) (hm-ord self))))
(define (hm-keys-ordered self) (reverse (hm-ord self)))
;; A hashmap-shaped jhost: the HashMap family and java.util.Properties, which is
;; a Hashtable with two string-typed accessors on top and so carries the same
;; #(table key-order) state. One predicate for every question about that shape —
;; seq, count, contains?, get, copy-into — so a new member of the family cannot
;; be map-like through one of them and opaque through another.
(define (hm-hashmap? x)
  (and (jhost? x)
       (let ((t (jhost-tag x)))
         (or (string=? t "hashmap") (string=? t "properties")))))
(define (hm-copy-into-ordered! self src)  ; copy a jolt map or hashmap, keeping insertion order
  (if (hm-hashmap? src)
      (for-each (lambda (k)
                  (hm-note-key! self k)
                  (hashtable-set! (hm-tbl self) k (hashtable-ref (hm-tbl src) k jolt-nil)))
                (hm-keys-ordered src))
      (for-each (lambda (e)
                  (hm-note-key! self (jolt-nth e 0))
                  (hashtable-set! (hm-tbl self) (jolt-nth e 0) (jolt-nth e 1)))
                (seq->list (jolt-seq src)))))
(register-class-ctor! "HashMap"
  (lambda args
    (let* ((ht (make-hashtable hm-hash jolt=2))
           (self (make-jhost "hashmap" (vector ht '()))))
      (when (and (pair? args) (or (pmap? (car args)) (hm-hashmap? (car args))))
        (hm-copy-into-ordered! self (car args)))
      self)))
(define (hm->pmap self)
  (let ((m (jolt-hash-map)))
    (for-each (lambda (k) (set! m (jolt-assoc m k (hashtable-ref (hm-tbl self) k jolt-nil))))
              (hm-keys-ordered self))
    m))
(define hashmap-methods
  (list (cons "put" (lambda (self k v) (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                          (hm-note-key! self k)
                                          (hashtable-set! (hm-tbl self) k v) old)))
        (cons "get" (lambda (self k) (hashtable-ref (hm-tbl self) k jolt-nil)))
        (cons "getOrDefault" (lambda (self k d) (hashtable-ref (hm-tbl self) k d)))
        ;; The java.util.Map default methods. A mapping to null counts as absent,
        ;; like the JVM's, and each returns what the JVM's returns (putIfAbsent
        ;; the PREVIOUS value — nil when it did insert; the compute/merge family
        ;; the NEW value). The function argument is called through jolt-invoke,
        ;; so a Clojure fn, a reify'd Function/BiFunction and a jolt keyword all
        ;; work as the mapper.
        (cons "putIfAbsent" (lambda (self k v)
          (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
            (if (jolt-nil? old)
                (begin (hm-note-key! self k) (hashtable-set! (hm-tbl self) k v) jolt-nil)
                old))))
        (cons "computeIfAbsent" (lambda (self k f)
          (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
            (if (jolt-nil? old)
                (let ((v (jolt-invoke1 f k)))
                  (unless (jolt-nil? v)
                    (hm-note-key! self k)
                    (hashtable-set! (hm-tbl self) k v))
                  v)
                old))))
        (cons "computeIfPresent" (lambda (self k f)
          (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
            (if (jolt-nil? old)
                jolt-nil
                (let ((v (jolt-invoke2 f k old)))
                  (if (jolt-nil? v)
                      (begin (hashtable-delete! (hm-tbl self) k) (hm-drop-key! self k) jolt-nil)
                      (begin (hashtable-set! (hm-tbl self) k v) v)))))))
        (cons "compute" (lambda (self k f)
          (let* ((old (hashtable-ref (hm-tbl self) k jolt-nil))
                 (v (jolt-invoke2 f k old)))
            (cond ((not (jolt-nil? v))
                   (hm-note-key! self k) (hashtable-set! (hm-tbl self) k v) v)
                  ((jolt-nil? old) jolt-nil)
                  (else (hashtable-delete! (hm-tbl self) k) (hm-drop-key! self k) jolt-nil)))))
        (cons "merge" (lambda (self k v f)
          (let* ((old (hashtable-ref (hm-tbl self) k jolt-nil))
                 (v (if (jolt-nil? old) v (jolt-invoke2 f old v))))
            (if (jolt-nil? v)
                (begin (hashtable-delete! (hm-tbl self) k) (hm-drop-key! self k) jolt-nil)
                (begin (hm-note-key! self k) (hashtable-set! (hm-tbl self) k v) v)))))
        ;; replace only touches a key that is already mapped — including one
        ;; mapped to nil, which is why this asks containsKey rather than reading
        ;; the value like putIfAbsent does.
        (cons "replace" (lambda (self k v)
          (if (hashtable-contains? (hm-tbl self) k)
              (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                (hashtable-set! (hm-tbl self) k v) old)
              jolt-nil)))
        (cons "forEach" (lambda (self f)
          (for-each (lambda (k) (jolt-invoke2 f k (hashtable-ref (hm-tbl self) k jolt-nil)))
                    (hm-keys-ordered self))
          jolt-nil))
        (cons "containsKey" (lambda (self k) (if (hashtable-contains? (hm-tbl self) k) #t #f)))
        (cons "containsValue" (lambda (self v)
          (let ((found #f))
            (vector-for-each (lambda (k) (when (jolt=2 v (hashtable-ref (hm-tbl self) k jolt-nil)) (set! found #t)))
                             (hashtable-keys (hm-tbl self))) found)))
        (cons "size" (lambda (self) (hashtable-size (hm-tbl self))))
        (cons "isEmpty" (lambda (self) (= 0 (hashtable-size (hm-tbl self)))))
        (cons "remove" (lambda (self k) (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                           (hashtable-delete! (hm-tbl self) k)
                                           (hm-drop-key! self k) old)))
        (cons "clear" (lambda (self) (hashtable-clear! (hm-tbl self)) (hm-ord! self '()) jolt-nil))
        (cons "putAll" (lambda (self m) (hm-copy-into-ordered! self m) jolt-nil))
        (cons "keySet" (lambda (self) (apply jolt-hash-set (hm-keys-ordered self))))
        (cons "values" (lambda (self) (apply jolt-vector
                          (map (lambda (k) (hashtable-ref (hm-tbl self) k jolt-nil))
                               (hm-keys-ordered self)))))
        (cons "entrySet" (lambda (self) (jolt-seq (hm->pmap self))))
        (cons "toString" (lambda (self) (jolt-pr-str (hm->pmap self))))))
(register-host-methods! "hashmap" hashmap-methods)

;; java.util.Properties — a Hashtable of strings with getProperty/setProperty and
;; a DEFAULTS chain. The JVM is precise about which operations see that chain:
;; getProperty, propertyNames and stringPropertyNames do; the inherited Hashtable
;; surface (get, containsKey, keySet, values, size) does NOT. Checked against the
;; reference — a Properties holding only defaults answers isEmpty true and an
;; empty keySet, while its propertyNames lists them.
;;
;; The WRITE-THROUGH target is jolt's own. System/getProperties hands back an
;; object whose entries are a snapshot, because jolt computes most system
;; properties on read (user.dir tracks the source roots, java.class.path the
;; resolved paths) and they cannot be a live table; but a setProperty through it
;; must still be what System/getProperty reports, so writes go to both.
;;
;; System/getProperties used to return a plain map, which answered .getProperty
;; as a key lookup and so read nil for every system property — how a library that
;; reads them through the Properties API saw nothing.
(define (make-properties-jhost . defaults)
  (make-jhost "properties"
              (vector (make-hashtable hm-hash jolt=2) (quote ())
                      (if (pair? defaults) (car defaults) #f)
                      #f)))
(define (props-slot self i)
  (let ((st (jhost-state self)))
    (and (fx>? (vector-length st) i) (vector-ref st i))))
(define (props-defaults self) (props-slot self 2))
(define (props-writethrough self) (props-slot self 3))
;; getProperty answers only STRING values, as on the JVM: a non-string mapping is
;; invisible to it (get still sees it) and the default is returned instead. The
;; defaults are themselves searched by getProperty, so a Properties whose
;; defaults is another Properties walks the WHOLE chain, as the JVM's recursive
;; defaults.getProperty does.
(define (props-string-ref self k dflt)
  (let ((v (hashtable-ref (hm-tbl self) k jolt-nil)))
    (cond ((string? v) v)
          ((not (jolt-nil? v)) dflt)
          (else (let ((d (props-defaults self)))
                  (cond ((not d) dflt)
                        ((and (jhost? d) (string=? (jhost-tag d) "properties"))
                         (props-string-ref d k dflt))
                        (else (let ((dv (jolt-get d k jolt-nil)))
                                (if (string? dv) dv dflt)))))))))
;; every name the object can answer for: its own keys plus its defaults' — down
;; the whole chain, since propertyNames enumerates it all on the JVM too.
(define (props-names self)
  (let* ((own (hm-keys-ordered self))
         (d (props-defaults self))
         (inherited
          (cond ((not d) (quote ()))
                ((and (jhost? d) (string=? (jhost-tag d) "properties")) (props-names d))
                (else (seq->list (jolt-keys d))))))
    (append own
            (filter (lambda (k) (not (hashtable-contains? (hm-tbl self) k))) inherited))))
(define (props-put! self k v)
  (hm-note-key! self k)
  (hashtable-set! (hm-tbl self) k v)
  (let ((wt (props-writethrough self)))
    (when wt (hashtable-set! wt k (if (string? v) v (jolt-str-render-one v))))))
(register-host-methods! "properties"
  (append
    hashmap-methods
    (list
      (cons "getProperty" (lambda (self k . dflt)
                            (props-string-ref self k (if (pair? dflt) (car dflt) jolt-nil))))
      ;; setProperty delegates to put on the JVM, so it returns THIS object's
      ;; previous value and never the one it was shadowing in the defaults.
      (cons "setProperty" (lambda (self k v)
                            (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                              (props-put! self k (if (string? v) v (jolt-str-render-one v)))
                              old)))
      (cons "put" (lambda (self k v)
                    (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                      (props-put! self k v) old)))
      (cons "remove" (lambda (self k)
                       (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                         (hashtable-delete! (hm-tbl self) k)
                         (hm-drop-key! self k)
                         (let ((wt (props-writethrough self)))
                           (when wt (hashtable-delete! wt k)))
                         old)))
      (cons "clear" (lambda (self)
                      (hashtable-clear! (hm-tbl self)) (hm-ord! self (quote ()))
                      (let ((wt (props-writethrough self)))
                        (when wt (hashtable-clear! wt)))
                      jolt-nil))
      ;; the two enumerations that DO span the defaults chain.
      (cons "stringPropertyNames"
            (lambda (self) (apply jolt-hash-set
                             (filter (lambda (k) (string? (props-string-ref self k #f)))
                                     (props-names self)))))
      (cons "propertyNames" (lambda (self) (list->cseq (props-names self)))))))
(register-class-ctor! "Properties"
  (lambda args (apply make-properties-jhost args)))
(register-class-ctor! "java.util.Properties"
  (lambda args (apply make-properties-jhost args)))
;; System/getProperties: real entries, so get/count/seq see them as on the JVM,
;; recomputed per call so user.dir and java.class.path stay current, with writes
;; going through to the override store.
(define (make-system-properties m override-tbl)
  (let ((self (make-jhost "properties"
                          (vector (make-hashtable hm-hash jolt=2) (quote ()) #f override-tbl))))
    (for-each (lambda (e)
                (hm-note-key! self (jolt-nth e 0))
                (hashtable-set! (hm-tbl self) (jolt-nth e 0) (jolt-nth e 1)))
              (seq->list (jolt-seq m)))
    self))

;; java.util.concurrent.ConcurrentHashMap — one shared heap, so the mutable
;; HashMap shim serves. (get a-hashmap k) reads the map (clojure.core/get).
(define (make-hashmap-jhost . args)
  (let* ((ht (make-hashtable hm-hash jolt=2))
         (self (make-jhost "hashmap" (vector ht '()))))
    (when (and (pair? args) (or (pmap? (car args)) (hm-hashmap? (car args))))
      (hm-copy-into-ordered! self (car args)))
    self))
(register-class-ctor! "ConcurrentHashMap" make-hashmap-jhost)
(register-class-ctor! "java.util.concurrent.ConcurrentHashMap" make-hashmap-jhost)
;; WeakHashMap: a HashMap shim. Chez has no weak-value hashtable, so entries are
;; not GC-evicted — a cache backed by it never shrinks (correct, just unbounded),
;; the same trade-off as SoftReference on this host.
(register-class-ctor! "WeakHashMap" make-hashmap-jhost)
(register-class-ctor! "java.util.WeakHashMap" make-hashmap-jhost)
;; IdentityHashMap keys on reference identity; the HashMap shim keys on value
;; equality. Close enough for its usual role (tracking visited nodes during a
;; walk — schema/clojure.walk cycle detection), where the tracked values differ.
(register-class-ctor! "IdentityHashMap" make-hashmap-jhost)
(register-class-ctor! "java.util.IdentityHashMap" make-hashmap-jhost)
;; java.util.concurrent.atomic.Atomic{Reference,Integer,Long,Boolean}: a
;; thread-safe mutable cell (mutex-guarded, shared heap). One "atomic" jhost
;; serves all four; the numeric ops are meaningful only on Integer/Long.
;; AtomicReference CAS compares object identity, while the primitive wrappers
;; compare their unboxed values. Keep that closed distinction in the cell
;; instead of making the shared CAS path guess from the values it contains (or
;; calling a stored, potentially generic comparator while the mutex is held).
(define (make-atomic init kind)
  (make-jhost "atomic" (vector (box init) (make-mutex) kind)))
(define (atomic-box self) (vector-ref (jhost-state self) 0))
(define (atomic-lock self) (vector-ref (jhost-state self) 1))
(define (atomic-kind self) (vector-ref (jhost-state self) 2))
;; Convert a modeled Java method argument at the signature boundary. Numeric
;; atomic cells never contain an unbounded jolt integer: the constructor and
;; every value-taking method accept exactly the primitive width named by their
;; Java signature. Keep this outside the mutex because conversion may throw.
(define (atomic-convert self value)
  (case (atomic-kind self)
    ((integer) (jolt-int-cast value))
    ((long) (jolt-long-cast value))
    (else value)))
;; Java's primitive atomic arithmetic is two's-complement arithmetic. Its
;; result wraps even though a checked cast at a method boundary rejects an
;; out-of-range argument. VALUE is an exact sum or difference of already
;; converted cell data, so this helper is leaf-only inside the counted mutex.
(define (atomic-wrap self value)
  (case (atomic-kind self)
    ((integer) (jolt-unchecked-int value))
    ((long) (jolt-wrap64 value))
    (else value)))
(define (atomic-numeric-transition! self delta return-old?)
  (jolt-with-mutex (atomic-lock self)
    (let* ((old (unbox (atomic-box self)))
           (next (atomic-wrap self (+ old delta))))
      (set-box! (atomic-box self) next)
      (if return-old? old next))))
;; CAS under the atomic's mutex. updateAndGet/getAndUpdate run the user fn
;; LOCK-FREE in a CAS retry loop (JVM parity), so the fn may re-enter the same
;; atomic; a mutex-held fn deadlocked on the non-recursive Chez mutex there
;; (PSL R4 cluster 4).
(define (atomic-cas! self o n)
  (let* ((kind (atomic-kind self))
         (expected (atomic-convert self o))
         (next (atomic-convert self n)))
    (jolt-with-mutex (atomic-lock self)
      (let ((current (unbox (atomic-box self))))
        (if (case kind
              ((reference boolean) (eq? current expected))
              ((integer long) (= current expected))
              (else #f))
            (begin (set-box! (atomic-box self) next) #t)
            #f)))))
(let ((ref-ctor (lambda args
                  (make-atomic (if (pair? args) (car args) jolt-nil) 'reference)))
      (int-ctor (lambda args
                  (make-atomic (jolt-int-cast (if (pair? args) (car args) 0)) 'integer)))
      (long-ctor (lambda args
                   (make-atomic (jolt-long-cast (if (pair? args) (car args) 0)) 'long)))
      (bool-ctor (lambda args
                   (make-atomic (if (pair? args) (car args) #f) 'boolean))))
  (for-each (lambda (n) (register-class-ctor! n ref-ctor))
            '("AtomicReference" "java.util.concurrent.atomic.AtomicReference"))
  (for-each (lambda (n) (register-class-ctor! n int-ctor))
            '("AtomicInteger" "java.util.concurrent.atomic.AtomicInteger"))
  (for-each (lambda (n) (register-class-ctor! n long-ctor))
            '("AtomicLong" "java.util.concurrent.atomic.AtomicLong"))
  (for-each (lambda (n) (register-class-ctor! n bool-ctor))
            '("AtomicBoolean" "java.util.concurrent.atomic.AtomicBoolean")))
(register-host-methods! "atomic"
  (list (cons "get" (lambda (self) (unbox (atomic-box self))))
        ;; Serialization of the plain set method against read-modify-write
        ;; methods is a separate concern. Preserve its current write boundary
        ;; here while ensuring conversion completes before state changes.
        (cons "set" (lambda (self v)
          (let ((next (atomic-convert self v)))
            (set-box! (atomic-box self) next)
            jolt-nil)))
        (cons "getAndSet" (lambda (self v)
          (let ((next (atomic-convert self v)))
            (jolt-with-mutex (atomic-lock self)
              (let ((o (unbox (atomic-box self))))
                (set-box! (atomic-box self) next)
                o)))))
        (cons "compareAndSet" (lambda (self o n) (atomic-cas! self o n)))
        (cons "updateAndGet" (lambda (self f)
          (let loop ((v (unbox (atomic-box self))))
            ;; atomic-cas! deliberately re-normalizes V and N. This keeps every
            ;; CAS caller behind one typed boundary; conversion is idempotent.
            (let ((n (atomic-convert self (jolt-invoke f v))))
              (if (atomic-cas! self v n) n (loop (unbox (atomic-box self))))))))
        (cons "getAndUpdate" (lambda (self f)
          (let loop ((v (unbox (atomic-box self))))
            (let ((n (atomic-convert self (jolt-invoke f v))))
              (if (atomic-cas! self v n) v (loop (unbox (atomic-box self))))))))
        (cons "incrementAndGet"
          (lambda (self) (atomic-numeric-transition! self 1 #f)))
        (cons "decrementAndGet"
          (lambda (self) (atomic-numeric-transition! self -1 #f)))
        (cons "getAndIncrement"
          (lambda (self) (atomic-numeric-transition! self 1 #t)))
        (cons "getAndDecrement"
          (lambda (self) (atomic-numeric-transition! self -1 #t)))
        (cons "addAndGet" (lambda (self d)
          (let ((delta (atomic-convert self d)))
            (atomic-numeric-transition! self delta #f))))
        (cons "getAndAdd" (lambda (self d)
          (let ((delta (atomic-convert self d)))
            (atomic-numeric-transition! self delta #t))))
        (cons "intValue" (lambda (self)
          (if (eq? (atomic-kind self) 'long)
              (jolt-unchecked-int (unbox (atomic-box self)))
              (jnum->exact (unbox (atomic-box self))))))
        (cons "longValue" (lambda (self) (jnum->exact (unbox (atomic-box self)))))
        (cons "toString" (lambda (self) (jolt-str-render-one (unbox (atomic-box self)))))))
;; java.util.Collections/synchronizedMap|List|Set wrap a collection for
;; thread-safe access. The shared-heap HashMap/ArrayList shims already serialize
;; individual ops adequately for these uses, so the wrapper returns its argument.
(let ((ident (lambda (c . _) c)))
  ;; registering under the FQN also registers the short name (shared table)
  (register-class-statics! "java.util.Collections"
    (list (cons "synchronizedMap" ident) (cons "synchronizedList" ident)
          (cons "synchronizedSet" ident) (cons "unmodifiableMap" ident)
          (cons "unmodifiableList" ident) (cons "unmodifiableSet" ident)
          (cons "emptyList" (lambda _ (jolt-vector))) (cons "emptyMap" (lambda _ (jolt-hash-map))))))

;; A mutable set over the same value-keyed hashtable (element -> #t).
;; Constructors: () | (capacity) [ignored] | (coll) [copy].
(define (hs-hashset? x) (and (jhost? x) (string=? (jhost-tag x) "hashset")))
(define (hs->list self) (hm-keys-ordered self))
(let ((ctor (lambda args
              (let* ((ht (make-hashtable hm-hash jolt=2))
                     (self (make-jhost "hashset" (vector ht '()))))
                (when (and (pair? args) (not (number? (car args))))
                  (for-each (lambda (e)
                              (hm-note-key! self e)
                              (hashtable-set! ht e #t))
                            (seq->list (jolt-seq (car args)))))
                self))))
  (register-class-ctor! "HashSet" ctor)
  (register-class-ctor! "java.util.HashSet" ctor))
(register-host-methods! "hashset"
  (list (cons "add" (lambda (self x) (let ((had (hashtable-contains? (hm-tbl self) x)))
                                        (hm-note-key! self x)
                                        (hashtable-set! (hm-tbl self) x #t) (not had))))
        (cons "remove" (lambda (self x) (let ((had (hashtable-contains? (hm-tbl self) x)))
                                           (hashtable-delete! (hm-tbl self) x)
                                           (hm-drop-key! self x) (if had #t #f))))
        (cons "contains" (lambda (self x) (if (hashtable-contains? (hm-tbl self) x) #t #f)))
        (cons "size" (lambda (self) (hashtable-size (hm-tbl self))))
        (cons "isEmpty" (lambda (self) (= 0 (hashtable-size (hm-tbl self)))))
        (cons "clear" (lambda (self) (hashtable-clear! (hm-tbl self)) (hm-ord! self '()) jolt-nil))
        (cons "toString" (lambda (self) (jolt-pr-str (apply jolt-hash-set (hs->list self)))))))
(register-seq-arm! hs-hashset? (lambda (x) (list->cseq (hs->list x))))
(register-get-arm! hm-hashmap?
                   (lambda (coll k d) (hashtable-ref (hm-tbl coll) k d)))
;; count / contains? over the mutable map shim (clojure.core/count + contains?,
;; which core.cache's SoftCache uses on its backing ConcurrentHashMap).
(define jhost-hashmap? hm-hashmap?)
(register-count-arm! jhost-hashmap? (lambda (c) (hashtable-size (hm-tbl c))))
(register-contains-arm! jhost-hashmap?
  (lambda (c k) (if (hashtable-contains? (hm-tbl c) k) #t #f)))

;; ---- java.lang.ref.Soft/WeakReference + ReferenceQueue ----------------------
;; Real GC reclamation via Chez's generational collector: the referent is held
;; through a weak pair (collected once otherwise unreachable, leaving the bwp
;; object), and a guardian registered on the referent makes the reference itself
;; available the moment its referent is reclaimed — which the ReferenceQueue
;; surfaces as enqueued, exactly like the JVM. (Chez has no softer-than-weak
;; reference, so a SoftReference clears on unreachability rather than under memory
;; pressure — its SoftCache evicts more eagerly than the JVM's, but it is genuine
;; GC eviction, not an unbounded strong cache. Immediates like fixnums/keywords
;; are never collected.)
;; ref-queue state: #(guardian pending-front pending-rear) — the pending refs as
;; a front/rear two-list queue (rq-add! is one cons; the old single list was
;; re-copied per append, O(n^2) across a mass eviction's pump).
;; reference state: #(weak-pair queue enqueued?).
(define (rq-guardian-of q) (vector-ref (jhost-state q) 0))
(define (rq-add! q ref)
  (let ((st (jhost-state q))) (vector-set! st 2 (cons ref (vector-ref st 2)))))
(define (rq-pump! q)                                  ; drain GC-reclaimed refs onto the queue
  (let loop ()
    (let ((rep ((rq-guardian-of q)))) (when rep (rq-add! q rep) (loop)))))
(define (rq-poll q)
  (rq-pump! q)
  (let ((st (jhost-state q)))
    (when (and (null? (vector-ref st 1)) (pair? (vector-ref st 2)))
      (vector-set! st 1 (reverse (vector-ref st 2)))
      (vector-set! st 2 '()))
    (let ((l (vector-ref st 1)))
      (if (null? l) jolt-nil (begin (vector-set! st 1 (cdr l)) (car l))))))
(define (a-ref-queue? x) (and (jhost? x) (string=? (jhost-tag x) "ref-queue")))
;; Each reference class has its own tag (class-hierarchy.ss maps the tag to the
;; class), sharing one method table: the two differ only in the class they
;; report, and a WeakReference used to report as a SoftReference.
(define (make-reference tag v rest)
  (let* ((rq (if (pair? rest) (car rest) jolt-nil))
         (ref (make-jhost tag (vector (weak-cons v #f) rq #f))))
    (when (a-ref-queue? rq) ((rq-guardian-of rq) v ref))   ; fire on the referent's collection
    ref))
(for-each (lambda (nm) (register-class-ctor! nm (lambda (v . rest) (make-reference "soft-ref" v rest))))
          '("SoftReference" "java.lang.ref.SoftReference"))
(for-each (lambda (nm) (register-class-ctor! nm (lambda (v . rest) (make-reference "weak-ref" v rest))))
          '("WeakReference" "java.lang.ref.WeakReference"))
;; the referent, or nil once cleared — by the program or by the collector
(define (reference-referent self)
  (let ((r (car (vector-ref (jhost-state self) 0))))
    (if (bwp-object? r) jolt-nil r)))
(register-host-methods! "weak-ref"
  (list (cons "get" reference-referent)
        (cons "clear" (lambda (self) (set-car! (vector-ref (jhost-state self) 0) jolt-nil) jolt-nil))
        ;; Reference.refersTo(obj): identity against the referent, so a cleared
        ;; reference refers to null — the JDK 16 spelling of "is it still x"
        ;; that avoids strengthening the referent the way get() does.
        (cons "refersTo" (lambda (self x) (eq? (reference-referent self) x)))
        (cons "isEnqueued" (lambda (self) (vector-ref (jhost-state self) 2)))
        (cons "enqueue" (lambda (self)
          (let* ((st (jhost-state self)) (rq (vector-ref st 1)))
            (if (vector-ref st 2) #f
                (begin (vector-set! st 2 #t) (when (a-ref-queue? rq) (rq-add! rq self)) #t)))))))
(alias-host-methods! "soft-ref" "weak-ref")
(for-each (lambda (nm) (register-class-ctor! nm (lambda _ (make-jhost "ref-queue" (vector (make-guardian) '() '())))))
          '("ReferenceQueue" "java.lang.ref.ReferenceQueue"))
(register-host-methods! "ref-queue"
  (list (cons "poll" (lambda (self . _) (rq-poll self)))
        (cons "remove" (lambda (self . _) (rq-poll self)))))

;; ---- StringReader -----------------------------------------------------------
;; state: a vector #(string pos marked).
(register-class-ctor! "StringReader"
  ;; src is a String or a char[] ((StringReader. (char-array s)) — selmer's parser
  ;; reads templates this way); a char-array becomes the string of its chars.
  (lambda (src . _)
    (make-jhost "string-reader"
      (vector (cond ((string? src) src)
                    ((jolt-array? src) (apply string-append (map jolt-str-render-one (seq->list (jolt-seq src)))))
                    (else (jolt-str-render-one src)))
              0 0))))
(define (sr-s self) (vector-ref (jhost-state self) 0))
(define (sr-pos self) (vector-ref (jhost-state self) 1))
(define (sr-pos! self p) (vector-set! (jhost-state self) 1 p))
(register-host-methods! "string-reader"
  (list (cons "read" (lambda (self . rest)
                       (let ((s (sr-s self)) (p (sr-pos self)))
                         (cond
                           ;; .read() -> one char code, -1 at EOF
                           ((null? rest)
                            (if (>= p (string-length s)) -1
                                (begin (sr-pos! self (+ p 1)) (->num (char->integer (string-ref s p))))))
                           ;; .read(cbuf, off, len) -> fill cbuf, return count or -1 at EOF
                           (else
                            (let ((slen (string-length s)))
                              (if (>= p slen) -1
                                  (let ((cbuf (car rest)) (off (jnum->exact (cadr rest))) (len (jnum->exact (caddr rest))))
                                    (let ((n (min len (- slen p))) (dv (jolt-array-vec cbuf)))
                                      (let loop ((i 0)) (when (< i n) (vector-set! dv (+ off i) (string-ref s (+ p i))) (loop (+ i 1))))
                                      (sr-pos! self (+ p n)) (->num n))))))))))
        (cons "mark" (lambda (self . _) (vector-set! (jhost-state self) 2 (sr-pos self)) jolt-nil))
        (cons "reset" (lambda (self) (sr-pos! self (vector-ref (jhost-state self) 2)) jolt-nil))
        (cons "skip" (lambda (self n) (let ((n (jnum->exact n)))
                                        (sr-pos! self (min (string-length (sr-s self)) (+ (sr-pos self) n))) (->num n))))
        ;; readLine: the next line without its terminator (\n or \r\n), nil at EOF —
        ;; what line-seq drives over a BufferedReader.
        (cons "readLine"
          (lambda (self)
            (let ((s (sr-s self)) (p (sr-pos self)) (len (string-length (sr-s self))))
              (if (>= p len) jolt-nil
                  (let scan ((i p))
                    (cond
                      ((>= i len) (sr-pos! self len) (substring s p len))
                      ((char=? (string-ref s i) #\newline)
                       (sr-pos! self (+ i 1))
                       (substring s p (if (and (> i p) (char=? (string-ref s (- i 1)) #\return)) (- i 1) i)))
                      (else (scan (+ i 1)))))))))
        (cons "close" (lambda (self) jolt-nil))))

;; ---- PushbackReader ---------------------------------------------------------
;; state: a vector #(wrapped-reader pushed-list line-numbering? line column skip-lf?)
(register-class-ctor! "PushbackReader"
  (lambda (rdr . _) (make-jhost "pushback-reader" (vector rdr '() #f 0 0 #f))))
;; Fully-qualified aliases so (java.io.PushbackReader. …) / (java.io.StringReader. …)
;; resolve to these built-ins even when a library defines a deftype of the same
;; simple name (tools.reader), which would otherwise take the bare-name slot.
(register-class-ctor! "java.io.PushbackReader" (lookup-class class-ctors-tbl "PushbackReader"))
(register-class-ctor! "java.io.StringReader" (lookup-class class-ctors-tbl "StringReader"))
;; clojure.lang.LineNumberingPushbackReader is pushback over java.io.LineNumberReader,
;; and that reader normalizes line terminators: \r, \n and \r\n each read as a single
;; \n and bump the line number. So a source read through it looks the same whether it
;; was written on Unix, Windows or a classic Mac — tools.reader's source logging
;; depends on that, and without it a \r leaks through as its own character.
;; Its own jhost tag, not "pushback-reader": the value has to report
;; clojure.lang.LineNumberingPushbackReader for tools.reader's
;; (extend LineNumberingPushbackReader IndexingReader …) to dispatch. The methods
;; are shared with the plain reader below, so the two cannot drift.
(define (make-lnpbr rdr . _)
  (make-jhost "line-numbering-pushback-reader" (vector rdr '() #t 0 0 #f)))
(register-class-ctor! "LineNumberingPushbackReader" make-lnpbr)
(register-class-ctor! "clojure.lang.LineNumberingPushbackReader" make-lnpbr)
(define (read-unit r)        ; read one code unit (flonum) from any reader, -1 at EOF
  (record-method-dispatch r "read" jolt-nil))
;; One character from the wrapped reader, terminators folded to \n. Pushback sits
;; ABOVE this (as it does on the JVM), so an unread \n is handed straight back and
;; does not count a second line.
(define (pbr-read-translated self)
  (let* ((st (jhost-state self))
         (c (read-unit (vector-ref st 0)))
         (n (and (number? c) (jnum->exact c))))
    (cond
      ((and (vector-ref st 5) (eqv? n 10))     ; the \n of a \r\n pair, already counted
       (vector-set! st 5 #f)
       (pbr-read-translated self))
      (else
       (vector-set! st 5 (eqv? n 13))
       (cond
         ((or (eqv? n 13) (eqv? n 10))
          (vector-set! st 3 (+ 1 (vector-ref st 3)))
          (vector-set! st 4 0)
          (->num 10))
         (else (vector-set! st 4 (+ 1 (vector-ref st 4))) c))))))
(register-host-methods! "pushback-reader"
  (list (cons "read"
          (lambda (self . rest)
            (define (read1)
              (let* ((st (jhost-state self)) (pushed (vector-ref st 1)))
                (cond
                  ((pair? pushed) (vector-set! st 1 (cdr pushed)) (car pushed))
                  ((vector-ref st 2) (pbr-read-translated self))
                  (else (read-unit (vector-ref st 0))))))
            (if (null? rest)
                (read1)
                ;; .read(cbuf, off, len) -> read one code unit at a time into cbuf,
                ;; return count or -1 at immediate EOF.
                (let ((off (jnum->exact (cadr rest))) (len (jnum->exact (caddr rest))) (dv (jolt-array-vec (car rest))))
                  (let loop ((i 0))
                    (if (>= i len) (->num i)
                        (let ((c (jnum->exact (read1))))
                          (if (= c -1) (if (= i 0) -1 (->num i))
                              (begin (vector-set! dv (+ off i) (integer->char c)) (loop (+ i 1)))))))))))
        (cons "unread"
          (lambda (self ch . rest)
            (if (null? rest)
                ;; unread(int|char) — push one code unit back
                (vector-set! (jhost-state self) 1
                  (cons (if (char? ch) (->num (char->integer ch)) ch) (vector-ref (jhost-state self) 1)))
                ;; unread(char[] cbuf, off, len) — push cbuf[off,off+len) so cbuf[off]
                ;; reads back first (the list head).
                (let ((dv (jolt-array-vec ch)) (off (jnum->exact (car rest))) (len (jnum->exact (cadr rest))))
                  (let loop ((i (- (+ off len) 1)) (acc (vector-ref (jhost-state self) 1)))
                    (if (< i off)
                        (vector-set! (jhost-state self) 1 acc)
                        (loop (- i 1) (cons (->num (char->integer (vector-ref dv i))) acc))))))
            jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        ;; 1-based, like clojure.lang.LineNumberingPushbackReader's own +1 over the
        ;; underlying LineNumberReader. A plain PushbackReader counts nothing.
        (cons "getLineNumber" (lambda (self) (->num (+ 1 (vector-ref (jhost-state self) 3)))))
        (cons "getColumnNumber" (lambda (self) (->num (vector-ref (jhost-state self) 4))))
        ;; readLine: the next line without its terminator, nil at EOF. On the JVM
        ;; only the line-numbering subclass has it (from its BufferedReader half);
        ;; the shared method table gives it to the plain PushbackReader as well,
        ;; which is a superset with one reasonable meaning. Goes through this
        ;; table's own `read`, so the pushback buffer and the \r\n folding above
        ;; are both honored rather than re-implemented.
        (cons "readLine"
          (lambda (self)
            (let loop ((acc '()))
              (let* ((u (read-unit self))
                     (n (and (number? u) (jnum->exact u))))
                (cond
                  ((or (jolt-nil? u) (and n (< n 0)))
                   (if (null? acc) jolt-nil (list->string (reverse acc))))
                  ((eqv? n 10)
                   (list->string (reverse (if (and (pair? acc) (char=? (car acc) #\return))
                                              (cdr acc) acc))))
                  (else (loop (cons (integer->char n) acc))))))))))
;; The line-numbering subclass IS a PushbackReader — same methods, same table.
;; Only the class it reports differs (see make-lnpbr).
(alias-host-methods! "line-numbering-pushback-reader" "pushback-reader")

;; ---- StringTokenizer --------------------------------------------------------
;; state: a vector #(tokens-list pos)
(define (tokenize s delims)
  (let ((dset (string->list delims)))
    (let loop ((chars (string->list s)) (cur '()) (toks '()))
      (cond ((null? chars) (reverse (if (null? cur) toks (cons (list->string (reverse cur)) toks))))
            ((memv (car chars) dset)
             (loop (cdr chars) '() (if (null? cur) toks (cons (list->string (reverse cur)) toks))))
            (else (loop (cdr chars) (cons (car chars) cur) toks))))))
;; state: #(token-VECTOR pos) — length/list-ref per token made a full drain
;; O(tokens^2); a vector makes every method O(1).
(define (st-toks self) (vector-ref (jhost-state self) 0))
(define (st-pos self) (vector-ref (jhost-state self) 1))
(define (st-next! self)
  (let ((toks (st-toks self)) (p (st-pos self)))
    (if (fx<? p (vector-length toks))
        (begin (vector-set! (jhost-state self) 1 (fx+ p 1)) (vector-ref toks p))
        (jolt-throw (jolt-host-throwable "java.util.NoSuchElementException" "no more tokens")))))
(define (st-more? self) (fx<? (st-pos self) (vector-length (st-toks self))))
(register-class-ctor! "StringTokenizer"
  (lambda (s . delims) (make-jhost "string-tokenizer"
    (vector (list->vector
             (tokenize (if (string? s) s (jolt-str-render-one s))
                       (if (null? delims) " \t\n\r\f" (car delims)))) 0))))
(register-host-methods! "string-tokenizer"
  (list (cons "hasMoreTokens" st-more?)
        (cons "countTokens" (lambda (self) (->num (fx- (vector-length (st-toks self)) (st-pos self)))))
        (cons "nextToken" st-next!)
        ;; StringTokenizer implements java.util.Enumeration — enumeration-seq drives
        ;; it through these, so alias them onto the token methods.
        (cons "hasMoreElements" st-more?)
        (cons "nextElement" st-next!)))

;; ---- String / BigInteger / MapEntry constructors ----------------------------
;; (String. bytes [charset]) decodes bytes (a bytevector OR a jolt byte-array)
;; with the named charset (UTF-8 default; ISO-8859-1/latin1/ascii = one byte per
;; char); else stringify. clj-http-lite's body coercion is (String. ^[B body cs).
(define (string-charset-name rest)
  (if (pair? rest)
      (let ((c (car rest)))
        ;; via charset-arg-name: a "charset" jhost's state is the
        ;; #(canonical-name encode-max) vector charset-for-name builds, not an
        ;; alist, and assq on it raised "improperly formed alist" — so
        ;; (String. bytes (Charset/forName …)) threw where the JVM decodes.
        (cond ((or (string? c) (and (jhost? c) (string=? (jhost-tag c) "charset")))
               (charset-arg-name c))
              (else "UTF-8")))
      "UTF-8"))
(define (decode-bytevector bv rest)
  (let* ((name (string-charset-name rest))
         (cs (charset-canonical-down name)))
    (cond
      ((string=? cs "utf-8") (utf8->string bv))
      ((or (string=? cs "iso-8859-1") (string=? cs "us-ascii"))
       (list->string (map integer->char (bytevector->u8-list bv))))
      ((or (string=? cs "utf-16") (string=? cs "utf-16be"))
       (utf16->string bv (endianness big)))   ; respects a leading BOM
      ((string=? cs "utf-16le") (utf16->string bv (endianness little)))
      ((or (string=? cs "utf-32") (string=? cs "utf-32be"))
       (utf32->string bv (endianness big)))
      ((string=? cs "utf-32le") (utf32->string bv (endianness little)))
      ;; anything else through the system iconv (natives-str.ss); a charset the
      ;; host does not have is the JVM's UnsupportedEncodingException rather than
      ;; a silent reinterpretation of the bytes as UTF-8.
      (else (let ((u8 (iconv-bytes bv name "UTF-8")))
              (if u8
                  (guard (e (#t (list->string (map integer->char (bytevector->u8-list u8)))))
                    (utf8->string u8))
                  (unsupported-encoding-throw name)))))))
;; (String. bytes offset length [charset]) — decode a SLICE. Returns (bv . rest')
;; where rest' is the charset args; a plain (String. bytes [charset]) is unsliced.
(define (bytes-slice-for-string bv rest)
  (if (and (pair? rest) (number? (car rest)) (pair? (cdr rest)) (number? (cadr rest)))
      (let* ((off (jnum->exact (car rest))) (len (jnum->exact (cadr rest)))
             (out (make-bytevector len)))
        (bytevector-copy! bv off out 0 len)
        (cons out (cddr rest)))
      (cons bv rest)))
(register-class-ctor! "String"
  (lambda (x . rest)
    (cond ((bytevector? x) (let ((p (bytes-slice-for-string x rest))) (decode-bytevector (car p) (cdr p))))
          ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte))
           (let ((p (bytes-slice-for-string (na-bytearray->bv x) rest))) (decode-bytevector (car p) (cdr p))))
          ;; (String. char[] [offset count]) — the whole array or a slice. Buffered
          ;; readers (data.json) build a string from a fill buffer this way.
          ((and (jolt-array? x) (eq? (jolt-array-kind x) 'char))
           (let ((v (jolt-array-vec x)))
             (if (pair? rest)
                 (let* ((off (jnum->exact (car rest))) (cnt (jnum->exact (cadr rest))) (out (make-string cnt)))
                   (let loop ((i 0)) (when (fx<? i cnt) (string-set! out i (vector-ref v (fx+ off i))) (loop (fx+ i 1))))
                   out)
                 (list->string (vector->list v)))))
          ((string? x) x)
          (else (jolt-str-render-one x)))))
;; (BigInteger. s) | (BigInteger. s radix) — parse a string in the given radix
;; (default 10). tools.reader's integer parser builds (BigInteger. digits radix).
;; (BigInteger. signum magnitude-bytes) is the other JVM constructor: an unsigned
;; big-endian magnitude with an explicit sign, which is how a digest is turned into
;; a number before hex-formatting it (Selmer's {{ x|hash:"md5" }} filter does
;; (format "%032x" (BigInteger. 1 bs))). Each byte contributes its UNSIGNED value,
;; so the sign of jolt's signed byte array does not leak into the result.
(define (bigint-from-magnitude signum bytes)
  (let* ((v (jolt-array-vec bytes))
         (n (ja-len v)))
    (let loop ((i 0) (acc 0))
      (if (fx>=? i n)
          (* (jnum->exact signum) acc)
          (loop (fx+ i 1)
                (+ (* acc 256) (bitwise-and (jnum->exact (ja-ref v i)) #xFF)))))))
(define (bigint-ctor v . r)
  (if (and (pair? r) (jolt-array? (car r)))
      (bigint-from-magnitude v (car r))
      (parse-int-or-throw v (if (null? r) 10 (jnum->exact (car r))) "BigInteger")))
(register-class-ctor! "BigInteger" bigint-ctor)
(register-class-ctor! "java.math.BigInteger" bigint-ctor)
(register-class-ctor! "MapEntry" (lambda (k v) (make-map-entry k v)))
;; clojure.lang.MapEntry/create — the static factory clojure.walk and kin use
;; when rebuilding map entries.
(register-class-statics! "MapEntry" (list (cons "create" (lambda (k v) (make-map-entry k v)))))
;; JVM exception ctors -> a typed host throwable carrying the canonical :jolt/class
;; (so class / instance? / getMessage / ex-message reflect the real type) and the
;; message. Supports (E. msg), (E. msg cause), (E. cause), and (E.).
;; Derived from the ONE exception hierarchy in class-hierarchy.ss: every
;; jch-isa? -> Throwable gets a ctor with no second list to maintain.
(define (make-exc-ctor canonical)
  (lambda args
    (let* ((a0 (if (pair? args) (car args) jolt-nil))
           (rest (if (pair? args) (cdr args) '()))
           (cause (if (pair? rest) (car rest) jolt-nil)))
      (cond
        ((string? a0) (jolt-host-throwable canonical a0 cause))
        ((jolt-nil? a0) (jolt-host-throwable canonical jolt-nil))
        ;; (E. cause): a lone throwable arg is the cause, message nil.
        ((and (null? rest) (ex-info-map? a0)) (jolt-host-throwable canonical jolt-nil a0))
        (else (jolt-host-throwable canonical (jolt-str-render-one a0) cause))))))
(let-values (((keys vals) (hashtable-entries jvm-class-parents)))
  (vector-for-each
    (lambda (canonical supers)
      (when (jch-isa? canonical "Throwable")
        (let ((short (jch-last-segment canonical)))
          (register-class-ctor! short (make-exc-ctor canonical))
          (unless (string=? short canonical)
            (register-class-ctor! canonical (make-exc-ctor canonical))))))
    keys vals))

;; clojure.lang.ArityException(int actual, String name) builds the JVM message.
(register-class-ctor! "ArityException"
  (lambda (actual name . _)
    (jolt-host-throwable "clojure.lang.ArityException"
      (string-append "Wrong number of args (" (jolt-str-render-one actual)
                     ") passed to: " (if (string? name) name (jolt-str-render-one name))))))
(register-class-ctor! "clojure.lang.ArityException"
  (lambda (actual name . _)
    (jolt-host-throwable "clojure.lang.ArityException"
      (string-append "Wrong number of args (" (jolt-str-render-one actual)
                     ") passed to: " (if (string? name) name (jolt-str-render-one name))))))

;; java.text.ParseException(String s, int errorOffset): unlike the exceptions
;; above, its second ctor arg is an int offset (getErrorOffset), not a cause.
;; Store the offset in the record's error-offset field (invisible to ex-data).
(let ((parse-exc-ctor
       (lambda args
         (let* ((a0 (if (pair? args) (car args) jolt-nil))
                (off (if (and (pair? args) (pair? (cdr args))) (cadr args) 0))
                (msg (if (string? a0) a0 (jolt-str-render-one a0)))
                (rec (jolt-host-throwable "java.text.ParseException" msg)))
           (jolt-ex-info-record-error-offset-set! rec off)
           rec))))
  (register-class-ctor! "ParseException" parse-exc-ctor)
  (register-class-ctor! "java.text.ParseException" parse-exc-ctor))

;; ---- URLEncoder / URLDecoder (www-form-urlencoded) --------------------------
;; Both honour the charset argument, which they used to ignore and always encode
;; UTF-8 — so (URLEncoder/encode "\u3044" "Shift_JIS") is %82%A2, not %E3%81%84.
;;
;; The conversion is per RUN, not per string or per character, which is what the
;; JVM does: unreserved ASCII passes through as itself and only the characters
;; between them are converted, together. It matters for a stateful charset —
;; UTF-16 writes a byte-order mark at the head of whatever it is handed, so
;; (URLEncoder/encode "foo/bar" "UTF-16") is foo%FE%FF%00%2Fbar, one mark before
;; the escaped "/" rather than one at the head of the whole string.
(define (url-unreserved-char? c)
  (let ((b (char->integer c)))
    (or (and (>= b 48) (<= b 57)) (and (>= b 65) (<= b 90)) (and (>= b 97) (<= b 122))
        (= b 46) (= b 42) (= b 95) (= b 45))))
(define hex-digits "0123456789ABCDEF")
;; The charset is validated up front, as on the JVM: a name the host cannot honour
;; is an error even when the input happens to be all-unreserved and no conversion
;; would have run.
(define (url-charset cs)
  (let ((name (charset-arg-name cs)))
    (if (or (charset-lookup name) (iconv-known? name)) cs (unsupported-encoding-throw name))))
(define (url-encode s . rest)
  (let* ((str (if (string? s) s (jolt-str-render-one s)))
         (cs (url-charset (if (null? rest) "UTF-8" (car rest))))
         (n (string-length str))
         (out '()))
    (define (emit-escaped! bv)
      (do ((i 0 (+ i 1))) ((= i (bytevector-length bv)))
        (let ((b (bytevector-u8-ref bv i)))
          (set! out (cons (string-ref hex-digits (bitwise-and b 15))
                     (cons (string-ref hex-digits (bitwise-arithmetic-shift-right b 4))
                       (cons #\% out)))))))
    (let loop ((i 0))
      (if (>= i n)
          (list->string (reverse out))
          (let ((c (string-ref str i)))
            (cond
              ((url-unreserved-char? c) (set! out (cons c out)) (loop (+ i 1)))
              ((char=? c #\space) (set! out (cons #\+ out)) (loop (+ i 1)))
              (else
               (let run ((j i))
                 (if (and (< j n)
                          (not (url-unreserved-char? (string-ref str j)))
                          (not (char=? (string-ref str j) #\space)))
                     (run (+ j 1))
                     (begin (emit-escaped! (charset-encode-bv (substring str i j) cs))
                            (loop j)))))))))))
(define (hexv c)
  (cond ((and (char<=? #\0 c) (char<=? c #\9)) (- (char->integer c) 48))
        ((and (char<=? #\A c) (char<=? c #\F)) (- (char->integer c) 55))
        ((and (char<=? #\a c) (char<=? c #\f)) (- (char->integer c) 87))
         (else (throw-jvm 'IllegalArgumentException "URLDecoder: illegal hex escape"))))
(define (url-decode s . rest)
  (let* ((str (if (string? s) s (jolt-str-render-one s)))
         (cs (list (url-charset (if (null? rest) "UTF-8" (car rest)))))
         (n (string-length str))
         (out '()))
    (let loop ((i 0))
      (if (>= i n)
          (list->string (reverse out))
          (let ((c (string-ref str i)))
            (cond
              ((char=? c #\+) (set! out (cons #\space out)) (loop (+ i 1)))
              ((char=? c #\%)
               (let run ((j i) (bytes '()))
                 (if (and (< j n) (char=? (string-ref str j) #\%))
                     (run (+ j 3) (cons (+ (* 16 (hexv (string-ref str (+ j 1))))
                                           (hexv (string-ref str (+ j 2))))
                                        bytes))
                     (let ((dec (decode-bytevector (u8-list->bytevector (reverse bytes)) cs)))
                       (do ((k 0 (+ k 1))) ((= k (string-length dec)))
                         (set! out (cons (string-ref dec k) out)))
                       (loop j)))))
              (else (set! out (cons c out)) (loop (+ i 1)))))))))
(define (u8-list->bytevector lst)
  (let ((bv (make-bytevector (length lst))))
    (let loop ((l lst) (i 0)) (if (null? l) bv (begin (bytevector-u8-set! bv i (car l)) (loop (cdr l) (+ i 1)))))))
(register-class-statics! "URLEncoder" (list (cons "encode" url-encode)))
(register-class-statics! "URLDecoder" (list (cons "decode" url-decode)))
;; Charset/forName yields the canonical name STRING (not an opaque object) so it
;; threads straight into (.getBytes s cs) / (String. bytes cs), which take a name.
;; defaultCharset is likewise the canonical name string ("UTF-8" — jolt's I/O is
;; UTF-8 throughout), so it threads into the same name-taking APIs as forName.
(register-class-statics! "Charset"
  (list (cons "forName" (lambda (nm) (jolt-str-render-one nm)))
        (cons "defaultCharset" (lambda () "UTF-8"))))

;; ---- Base64 (RFC 4648) ------------------------------------------------------
;; One codec, two alphabets: basic (+/) and URL-safe (-_), section 5 of the RFC.
;; An encoder/decoder jhost carries (vector alphabet pad?) as its state, so
;; getUrlEncoder/getUrlDecoder and .withoutPadding are the SAME tags with
;; different state — .withoutPadding returns a fresh encoder, like the JDK's
;; (whose encoders are immutable), and each decoder rejects the other
;; alphabet's chars because b64-char-val searches only its own alphabet.
(define b64-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define b64url-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
(define (b64-self-alphabet self)
  (let ((st (jhost-state self))) (if (vector? st) (vector-ref st 0) b64-alphabet)))
(define (b64-self-pad? self)
  (let ((st (jhost-state self))) (if (vector? st) (vector-ref st 1) #t)))
(define (->bytevector x)
  (cond ((bytevector? x) x)
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (na-bytearray->bv x))
        ((string? x) (string->utf8 x))
        (else (string->utf8 (jolt-str-render-one x)))))
(define (b64-encode x alphabet pad?)
  (let* ((bs (->bytevector x)) (n (bytevector-length bs)) (out '()))
    (let loop ((i 0))
      (if (>= i n) (list->string (filter char? (reverse out)))
          (let* ((b0 (bytevector-u8-ref bs i))
                 (b1 (if (< (+ i 1) n) (bytevector-u8-ref bs (+ i 1)) #f))
                 (b2 (if (< (+ i 2) n) (bytevector-u8-ref bs (+ i 2)) #f)))
            (set! out (cons (string-ref alphabet (bitwise-arithmetic-shift-right b0 2)) out))
            (set! out (cons (string-ref alphabet (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b0 3) 4)
                                                                  (bitwise-arithmetic-shift-right (or b1 0) 4))) out))
            (set! out (cons (if b1 (string-ref alphabet (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b1 15) 2)
                                                                         (bitwise-arithmetic-shift-right (or b2 0) 6))) (and pad? #\=)) out))
            (set! out (cons (if b2 (string-ref alphabet (bitwise-and b2 63)) (and pad? #\=)) out))
            (loop (+ i 3)))))))
(define (b64-char-val c alphabet)
  (let loop ((i 0)) (cond ((= i 64) (throw-jvm 'IllegalArgumentException "Base64: illegal character")) ((char=? (string-ref alphabet i) c) i) (else (loop (+ i 1))))))
(define (b64-decode x alphabet)
  (let* ((str (let ((s (if (string? x) x (utf8->string (->bytevector x)))))
                (list->string (filter (lambda (c) (not (char=? c #\=))) (string->list s)))))
         (out '()) (acc 0) (bits 0))
    (for-each (lambda (c)
                (set! acc (bitwise-ior (bitwise-arithmetic-shift-left acc 6) (b64-char-val c alphabet)))
                (set! bits (+ bits 6))
                (when (>= bits 8)
                  (set! bits (- bits 8))
                  (set! out (cons (bitwise-and (bitwise-arithmetic-shift-right acc bits) 255) out))))
              (string->list str))
    (u8-list->bytevector (reverse out))))
;; .encode / .decode return byte[] on the JVM, so they return a jolt byte-array here
;; (signed elements, seqable, bytes?) — they used to hand back a raw Chez bytevector,
;; which no collection dispatcher knows, so (vec (.decode dec s)) threw "Don't know
;; how to create ISeq from" and every caller had to route it through String. first.
(register-host-methods! "b64-encoder"
  (list (cons "encode" (lambda (self bs) (na-bv->bytearray (string->utf8 (b64-encode bs (b64-self-alphabet self) (b64-self-pad? self))))))
        (cons "encodeToString" (lambda (self bs) (b64-encode bs (b64-self-alphabet self) (b64-self-pad? self))))
        (cons "withoutPadding" (lambda (self) (make-jhost "b64-encoder" (vector (b64-self-alphabet self) #f))))))
(register-host-methods! "b64-decoder"
  (list (cons "decode" (lambda (self s) (na-bv->bytearray (b64-decode s (b64-self-alphabet self)))))))
(register-class-statics! "Base64"
  (list (cons "getEncoder" (lambda () (make-jhost "b64-encoder" (vector b64-alphabet #t))))
        (cons "getDecoder" (lambda () (make-jhost "b64-decoder" (vector b64-alphabet #t))))
        (cons "getUrlEncoder" (lambda () (make-jhost "b64-encoder" (vector b64url-alphabet #t))))
        (cons "getUrlDecoder" (lambda () (make-jhost "b64-decoder" (vector b64url-alphabet #t))))))

;; ---- java.util.regex.Pattern ------------------------------------------------
;; Pattern/compile returns a jolt-regex value (regex-t), so str/replace, re-find,
;; .split etc. accept it transparently.
(define pattern-multiline 8.0)
(define (pattern-quote s)
  (let ((meta "\\.[]{}()*+-?^$|&") (s (if (string? s) s (jolt-str-render-one s))) (out '()))
    (let loop ((i 0))
      (if (= i (string-length s)) (list->string (reverse out))
          (let ((c (string-ref s i)))
            (when (memv c (string->list meta)) (set! out (cons #\\ out)))
            (set! out (cons c out))
            (loop (+ i 1)))))))
;; the one Pattern statics block (compile / quote / MULTILINE). nio-file and
;; host-static-methods used to register competing compile/quote members that
;; last-wins clobbered; this is now the single source.
(let ((pattern-statics
       (list (cons "compile" (lambda (s . flags)
                               (if (and (pair? flags) (= (bitwise-and (jnum->exact (car flags)) 8) 8))
                                   (jolt-regex (string-append "(?m)" s))
                                   (jolt-regex s))))
             (cons "quote" (lambda (s) (pattern-quote s)))
             (cons "MULTILINE" pattern-multiline))))
  (register-class-statics! "Pattern" pattern-statics)
  (register-class-statics! "java.util.regex.Pattern" pattern-statics))
;; record-method-dispatch already routes string? -> jolt-string-method. Add a
;; regex-t arm (Pattern .split / .matcher-less surface used by corpus) by wrapping
;; once more — a regex-t isn't a jhost.
(register-method-arm! arm-priority-regex
  (lambda (obj method-name rest-args)
    (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
      (cond
        ((regex-t? obj)
         (cond ((string=? method-name "split")
                ;; .split returns a String[] — a real array, the same shape
                ;; String.split answers with. The optional second argument is the
                ;; JVM's limit; it used to be dropped, so
                ;; (.split COLON "user:pass:word" 2) split three ways.
                (jvm-split-array (regex-t-irx obj) (car rest) (split-limit-arg rest 1)))
               ((string=? method-name "pattern") (regex-t-source obj))
               ((or (string=? method-name "toString")) (regex-t-source obj))
               ;; (.matcher pattern s) -> a Matcher (matcher-t) for stepping matches.
               ((string=? method-name "matcher") (jolt-re-matcher obj (car rest)))
               ;; .flags — the int Pattern was compiled with. jolt has no compile-time
               ;; flag argument, so the only flags a pattern can carry are the inline
               ;; ones it opens with; read those. Callers use it to decide whether a
               ;; pattern is one they can handle (test.chuck's string-from-regex).
               ((string=? method-name "flags") (rx-inline-flags (regex-t-source obj)))
               (else (dispatch-miss obj method-name rest))))
        ;; java.util.regex.Matcher: .matches (anchored whole-region), .find
        ;; (next match), .group [n], .groupCount.
        ((jolt-matcher? obj)
         (cond ((string=? method-name "matches") (jolt-matcher-matches obj))
               ((string=? method-name "lookingAt") (jolt-matcher-looking-at obj))
               ((string=? method-name "find") (not (jolt-nil? (jolt-re-find obj))))
               ((string=? method-name "group") (apply jolt-matcher-group obj rest))
               ((string=? method-name "groupCount") (jolt-matcher-group-count obj))
               ;; start/end of the last successful find (whole match, or group n)
               ((string=? method-name "start")
                (let ((mm (matcher-t-last obj)))
                  (if mm (irregex-match-start-index mm (if (pair? rest) (jnum->exact (car rest)) 0))
                      (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "No match available")))))
               ((string=? method-name "end")
                (let ((mm (matcher-t-last obj)))
                  (if mm (irregex-match-end-index mm (if (pair? rest) (jnum->exact (car rest)) 0))
                      (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "No match available")))))
               (else (dispatch-miss obj method-name rest))))
        (else 'pass)))))

;; ---- def-var! the registry entry points so emit can also reach them ---------
(def-var! "clojure.core" "host-static-ref" host-static-ref)
(def-var! "clojure.core" "host-static-call" (lambda (c m . a) (apply host-static-call c m a)))
(def-var! "clojure.core" "host-new" (lambda (c . a) (apply host-new c a)))

;; Clojure-visible class-registration hooks. A host shim (e.g. reitit.trie-jolt,
;; which mirrors the reitit.Trie Java class) registers a constructor proc or a
;; map of static members against a class token so (Class. args) / (Class/member
;; args) resolve to it. The statics argument is a jolt map {member-name -> val}.
(define (jmap->static-alist m)
  (let loop ((s (jolt-seq m)) (acc '()))
    (if (jolt-nil? s) acc
        (let ((e (jolt-first s)))
          (loop (jolt-seq (jolt-rest s)) (cons (cons (jolt-nth e 0) (jolt-nth e 1)) acc))))))
(def-var! "clojure.core" "__register-class-ctor!"
  (lambda (name proc) (register-class-ctor-user! name proc) jolt-nil))
(def-var! "clojure.core" "__register-class-statics!"
  (lambda (name members) (register-class-statics! name (jmap->static-alist members)) jolt-nil))

;; ---- tagged-table method dispatch + pluggable instance? --------------------
;; A jolt library can build stateful host objects with (jolt.host/tagged-table
;; tag) and dispatch (.method obj ...) to handlers registered here, keyed by the
;; table's "jolt/type" tag — the htable analogue of the jhost method registry
;; above. jolt-lang/http-client uses this to emulate java.net URL /
;; HttpURLConnection / java.io byte streams so clj-http-lite runs unchanged.
(define tagged-methods-tbl (make-hashtable string-hash string=?))   ; tag-key -> (method-ht)
(define (tag->method-key tag)
  (if (keyword-t? tag)
      (let ((ns (keyword-t-ns tag)))
        (if (and ns (not (jolt-nil? ns))) (string-append ns "/" (keyword-t-name tag)) (keyword-t-name tag)))
      (jolt-str-render-one tag)))
;; hsc-mu covers the two global registries below that are written after boot:
;; this one (reachable from Clojure as clojure.core/__register-class-methods!, so
;; from a namespace load, which is now parallel) and jolt-class-for-tbl's intern.
;; Single-key reads of both stay unlocked — strong hashtables, per var-table.
(define hsc-mu (make-mutex))

;; The probe and the create are ONE step. Split, two threads registering methods
;; for one tag each built their own inner table and published it over the other's,
;; so every method in the loser's batch vanished — the same shape as the protocol
;; registry's type-registry. The member writes go under the same lock, since they
;; mutate the table this just published.
(define (register-tagged-methods! tag members)
  (jolt-with-mutex hsc-mu
    (let* ((key (tag->method-key tag))
           (h (or (hashtable-ref tagged-methods-tbl key #f)
                  (let ((nh (make-hashtable string-hash string=?)))
                    (hashtable-set! tagged-methods-tbl key nh) nh))))
      (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members))))

;; htable arm: dispatch (.method obj a*) through the table's tag method registry;
;; an unregistered method falls through (sorted colls are htables too).
(register-method-arm! arm-priority-htable
  (lambda (obj method-name rest-args)
    (let ((tag (and (htable? obj) (hashtable-ref (htable-h obj) "jolt/type" #f))))
      (let* ((mh (and tag (hashtable-ref tagged-methods-tbl (tag->method-key tag) #f)))
             (f  (and mh (hashtable-ref mh method-name #f))))
        (if f
            (apply f obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))
            'pass)))))

(def-var! "clojure.core" "__register-class-methods!"
  (lambda (tag members) (register-tagged-methods! tag (jmap->static-alist members)) jolt-nil))

;; java.lang.ThreadLocal via a Chez thread-parameter: real per-thread storage with
;; a lazy initialValue (the proxy macro lowers (proxy [ThreadLocal] …) to this).
;; .get returns the thread's value, computing initialValue once; .set / .remove.
(define tl-unset (list 'tl-unset))
(define (jolt-make-thread-local init-thunk)
  (make-jhost "threadlocal" (vector (make-thread-parameter tl-unset) init-thunk)))
(register-host-methods! "threadlocal"
  (list (cons "get" (lambda (self)
                      (let* ((st (jhost-state self)) (tp (vector-ref st 0)) (v (tp)))
                        (if (eq? v tl-unset)
                            (let ((nv (jolt-invoke (vector-ref st 1)))) (tp nv) nv)
                            v))))
        (cons "set" (lambda (self v) ((vector-ref (jhost-state self) 0) v) jolt-nil))
        (cons "remove" (lambda (self) ((vector-ref (jhost-state self) 0) tl-unset) jolt-nil))))
(def-var! "jolt.host" "make-thread-local" jolt-make-thread-local)

;; Pluggable instance? — a library registers (fn [class-name-string val] -> true
;; | false | nil); nil means "not my class, fall through". First non-nil wins.
(define user-instance-checks '())
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tname (symbol-t-name type-sym)))
      (let loop ((fs user-instance-checks))
        (if (null? fs)
            'pass
            (let ((r ((car fs) tname val)))
              (if (jolt-nil? r) (loop (cdr fs)) (if (jolt-truthy? r) #t #f))))))))
(def-var! "clojure.core" "__register-instance-check!"
  (lambda (f) (set! user-instance-checks (append user-instance-checks (list f))) jolt-nil))

;; ---- value-semantics seams -------------------------------------------------
;; A library that models its own host values (java.time via jolt-lang/time) needs
;; those values to compare, hash, print, and order like the real thing. These
;; expose the internal arm registries to Clojure: pred/handler are Clojure fns,
;; and results are coerced to the Scheme forms each arm expects (a boolean for
;; eq, an integer for hash/compare, a string for str/pr). pred should be cheap
;; and return false for values it doesn't own — it runs on the slow path of every
;; =/hash/compare/print.
(def-var! "clojure.core" "__register-eq!"
  (lambda (pred handler)
    (register-eq-arm! (lambda (a b) (jolt-truthy? (jolt-invoke pred a b)))
                      (lambda (a b) (jolt-truthy? (jolt-invoke handler a b))))
    jolt-nil))
(def-var! "clojure.core" "__register-hash!"
  (lambda (pred handler)
    (register-hash-arm! (lambda (x) (jolt-truthy? (jolt-invoke pred x)))
                        (lambda (x) (jolt-invoke handler x)))
    jolt-nil))
(def-var! "clojure.core" "__register-str!"
  (lambda (pred render)
    (register-str-render! (lambda (x) (jolt-truthy? (jolt-invoke pred x)))
                          (lambda (x) (jolt-invoke render x)))
    jolt-nil))
(def-var! "clojure.core" "__register-pr!"
  (lambda (pred render)
    (register-pr-arm! (lambda (x) (jolt-truthy? (jolt-invoke pred x)))
                      (lambda (x) (jolt-invoke render x)))
    jolt-nil))
(def-var! "clojure.core" "__register-compare!"
  (lambda (pred handler)
    (register-compare-arm! (lambda (a b) (jolt-truthy? (jolt-invoke pred a b)))
                           (lambda (a b) (jolt-invoke handler a b)))
    jolt-nil))

;; __register-class! makes a library's own host values answer (class x)/(type x)
;; AND dispatch protocols extended to their class. class-fn returns the class name;
;; tags-fn returns the list of class/interface names the value satisfies (its own
;; plus supertypes), which value-host-tags (records.ss) feeds to protocol dispatch.
;; Without this, (class x) is :object and (extend-protocol P TheClass …) never fires.
(define jt-user-value-tags-arms '())
(let ((prev value-host-tags))
  (set! value-host-tags
    (lambda (obj)
      (let loop ((as jt-user-value-tags-arms))
        (cond ((null? as) (prev obj))
              (((caar as) obj) ((cdar as) obj))
              (else (loop (cdr as))))))))
(define (jt-jolt-strs->list v)
  (let loop ((s (jolt-seq v)) (acc '()))
    (if (jolt-nil? s) (reverse acc) (loop (jolt-seq (jolt-rest s)) (cons (jolt-first s) acc)))))
(def-var! "clojure.core" "__register-class!"
  (lambda (pred class-fn tags-fn)
    (let ((p (lambda (x) (jolt-truthy? (jolt-invoke pred x)))))
      (register-class-arm! p (lambda (x) (jolt-invoke class-fn x)))
      (set! jt-user-value-tags-arms
            (append jt-user-value-tags-arms
                    (list (cons p (lambda (x) (jt-jolt-strs->list (jolt-invoke tags-fn x))))))))
    jolt-nil))

;; (instance? clojure.lang.IFoo x) for the core clojure.lang interfaces libraries
;; branch on — jolt's value model satisfies them, so report it. Matched by the
;; interface's last dotted segment, so "clojure.lang.IObj" and "IObj" both hit.
(define (hsc-last-segment s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))
;; Where a class name's nesting boundary is, or #f if it names a top-level class.
;; The JVM reads nesting off the class file's enclosing-class attribute, never off
;; the name, so a `$` does not by itself mean nested: java.util.Map$Entry is Map's
;; member class, while clojure.core$inc is a TOP-LEVEL class whose name merely
;; contains one — which is why the JVM answers "Entry" for the first and
;; "core$inc", not "inc", for the second. Derive that from the class model: a `$`
;; is a nesting boundary only when the name before it is itself a class jolt
;; knows. Scanning stops at the last dot, since a `$` can only appear in the
;; final segment.
(define (hsc-nesting-dollar cn)
  (let loop ((i (- (string-length cn) 1)))
    (cond ((fx<? i 0) #f)
          ((char=? (string-ref cn i) #\.) #f)
          ((and (char=? (string-ref cn i) #\$) (jch-known? (substring cn 0 i))) i)
          (else (loop (fx- i 1))))))
;; Class.getSimpleName drops the package and, for a nested class, the enclosing
;; class too. Class.getCanonicalName spells nesting with a dot instead of a `$`.
(define (hsc-simple-name cn)
  (let ((i (hsc-nesting-dollar cn)))
    (if i (substring cn (fx+ i 1) (string-length cn)) (hsc-last-segment cn))))
(define (hsc-canonical-name cn)
  (let ((i (hsc-nesting-dollar cn)))
    (if i
        (string-append (hsc-canonical-name (substring cn 0 i))
                       "." (substring cn (fx+ i 1) (string-length cn)))
        cn)))
;; values that carry metadata (mirrors jolt-with-meta's set in natives-meta.ss).
(define (hsc-imeta? x)
  (or (pvec? x) (pmap? x) (pset? x) (cseq? x) (empty-list-t? x)
      (jolt-lazyseq? x) (jrec? x) (jreify? x) (procedure? x) (symbol-t? x)))
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((iface (hsc-last-segment (symbol-t-name type-sym))))
      ;; the value's own class-graph tags (value-host-tags) are authoritative — the
      ;; SAME source protocol dispatch reads, so instance? and extend-protocol can't
      ;; disagree about the interfaces a builtin implements.
      (if (let ((tags (value-host-tags val)))
            (or (member (symbol-t-name type-sym) tags) (member iface tags)))
          #t
      (let ((hit (cond
                   ;; IObj/IMeta — metadata-bearing values not tagged via jch-tags
                   ;; (cseq, empty-list, procedure, sorted-map/set)
                   ((or (string=? iface "IObj") (string=? iface "IMeta")) (hsc-imeta? val))
                   ((or (string=? iface "IMapEntry") (string=? iface "MapEntry")) (jolt-map-entry? val))
                   ;; IRecord applies only to defrecord, not bare deftype (JVM contract).
                   ((string=? iface "IRecord") (jrec-record? val))
                   ;; IFn — maps/sets/vectors are callable in jolt beyond the JVM
                   ;; class hierarchy, so jch-tags doesn't cover them for these types.
                   ;; A no on these falls THROUGH rather than deciding: a deftype or
                   ;; reify that declares clojure.lang.IFn is callable and answers
                   ;; instance? through its own declared interfaces.
                   ((string=? iface "IFn")
                    (or (procedure? val) (keyword? val) (symbol-t? val)
                        (pmap? val) (pset? val) (pvec? val)
                        'none))
                   ;; reader jhosts — data.json re-wraps a reader in a new
                   ;; PushbackReader unless (instance? PushbackReader r), so this
                   ;; must hold for repeated reads from one reader to work.
                   ((string=? iface "PushbackReader")
                    (and (jhost? val) (pushback-reader-tag? (jhost-tag val))))
                   ((string=? iface "StringReader")
                    (and (jhost? val) (string=? (jhost-tag val) "string-reader")))
                   ((string=? iface "Reader") (reader-jhost? val))
                   ;; ...but a PushbackReader is NOT a BufferedReader: on the JVM
                   ;; it extends FilterReader, and library code branches on the
                   ;; difference — tools.reader's read-line routes a BufferedReader
                   ;; through *in* and reads anything else char by char. What
                   ;; io/reader hands back is an in-memory StringReader here and a
                   ;; BufferedReader on the JVM, so the string-reader tag keeps
                   ;; answering true; a bare (java.io.StringReader. s) shares that
                   ;; tag and answers with it (known-divergences, :host-model).
                   ((string=? iface "BufferedReader")
                    (and (reader-jhost? val)
                         (not (and (jhost? val) (pushback-reader-tag? (jhost-tag val))))))
                   (else 'none))))
        (if (eq? hit 'none) 'pass (if hit #t #f)))))))

;; java.lang.Class value: (class x) / (.getClass x) return one. It renders like
;; the JVM — str/.toString -> "class <name>", pr -> "<name>", .getName -> "<name>".
;; A class token (java.util.Date) now evaluates to a Class object (not a name
;; string), so (= (class x) java.util.Date) works by jclass identity.
(define (make-class-obj name) (make-jhost "class" (vector name)))
(define (jclass? x) (and (jhost? x) (string=? (jhost-tag x) "class")))
(define (jclass-name x) (vector-ref (jhost-state x) 0))

;; Global interner: class tokens resolve to the same eq? object per name, so
;; identity, =, and defmethod table keys are stable. Called by the analyzer for
;; every class-name symbol (java.util.Date, clojure.lang.Atom) at evaluation time.
(define jolt-class-for-tbl (make-hashtable string-hash string=?))
;; Double-checked, like every other interner in the runtime. The hit path — every
;; class name after the first — is the bare hashtable-ref it always was, which
;; matters because the analyzer calls this per class-name symbol, and only a
;; first-ever intern takes the lock. The blast radius of a lost intern is smaller
;; here than for keywords: the eq-arm below compares jclass by NAME and the hash
;; arm hashes the name, so a duplicate still answers = and still keys a defmethod
;; table (new-mm-table is keyed by jolt=). It is the eq?-identity this comment
;; promises that a race would take away.
(define (jolt-class-for name)
  (or (hashtable-ref jolt-class-for-tbl name #f)
      (jolt-with-mutex hsc-mu
        (or (hashtable-ref jolt-class-for-tbl name #f)
            (let ((obj (make-class-obj name)))
              (hashtable-set! jolt-class-for-tbl name obj)
              obj)))))
(def-var! "jolt.host" "jolt-class-for" jolt-class-for)

(define (class-key x)
  (cond ((jclass? x) (jclass-name x))
        ((string? x) x)
        ;; a deftype/defrecord NAME var holds its ctor; treat it as the class
        ((procedure? x) (deftype-ctor-tag x))
        (else #f)))
;; = compares jclass values by name (stable interning makes this eq?-level);
;; strings are no longer = to a jclass — class-key survives for internal
;; dispatch boundaries only (multimethod tables, catch dispatch, isa?).
(register-eq-arm! (lambda (a b) (and (jclass? a) (jclass? b)))
                  (lambda (a b) (let ((ka (class-key a)) (kb (class-key b)))
                                  (and ka kb (string=? ka kb) #t))))
;; A deftype/defrecord TYPE TOKEN and (class inst) are the same Class object on
;; the JVM — identical?, not merely =. jolt spells them differently (the token is
;; the ctor procedure, so it stays callable) and they compared unequal, so
;; (= Rec (class (->Rec 1))) was false and the two spellings were distinct keys in
;; the same map. =/hash stay in step: the token's identity hash is seeded from its
;; class name at registration (records.ss deftype-ctor-tag-set!), which costs the
;; procedure-hash fast path nothing. Half of this pair alone would be worse than
;; neither — that is the shape that makes a hash container answer nil for a key it
;; contains.
(register-eq-arm! (lambda (a b)
                    (or (and (jclass? a) (procedure? b) (deftype-ctor-tag b) #t)
                        (and (jclass? b) (procedure? a) (deftype-ctor-tag a) #t)))
                  (lambda (a b) (let ((ka (class-key a)) (kb (class-key b)))
                                  (and ka kb (string=? ka kb) #t))))
(register-hash-arm! jclass? (lambda (x) (jolt-hash (jclass-name x))))
;; Class.toString says which kind it is: "interface java.util.List", "class java.lang.String".
(register-str-render! jclass?
  (lambda (x) (string-append (if (jch-interface? (jclass-name x)) "interface " "class ")
                             (jclass-name x))))
(register-pr-arm! jclass? (lambda (x) (jclass-name x)))
;; print/println of a Class prints the bare name (getName), like pr — the JVM's
;; print-method for Class ignores *print-readably*. Only str is "class <name>".
(let ((prev (var-deref "clojure.core" "__print1")))
  (def-var! "clojure.core" "__print1"
    (lambda (x) (if (jclass? x) (jclass-name x) (jolt-invoke1 prev x)))))
(register-host-methods! "class"
  (list (cons "getName" (lambda (self) (jclass-name self)))
        (cons "getCanonicalName" (lambda (self) (hsc-canonical-name (jclass-name self))))
        (cons "getSimpleName" (lambda (self) (hsc-simple-name (jclass-name self))))
        (cons "toString" (lambda (self) (string-append "class " (jclass-name self))))
        (cons "isArray" (lambda (self) (let ((n (jclass-name self)))
                                         (and (fx>? (string-length n) 0) (char=? (string-ref n 0) #\[)))))
        ;; Class.getComponentType: for an array class returns the element class;
        ;; for a non-array returns nil. JVM: [Ljava.lang.Long; → java.lang.Long.
        (cons "getComponentType" (lambda (self)
                                   (let ((n (jclass-name self)))
                                     (cond ((and (fx>? (string-length n) 2) (char=? (string-ref n 0) #\[)
                                                 (char=? (string-ref n 1) #\L) (char=? (string-ref n (- (string-length n) 1)) #\;))
                                            (jolt-class-for (substring n 2 (- (string-length n) 1))))
                                           ((and (fx>? (string-length n) 1) (char=? (string-ref n 0) #\[))
                                            (cond ((char=? (string-ref n 1) #\B) (jolt-class-for "byte"))
                                                  ((char=? (string-ref n 1) #\C) (jolt-class-for "char"))
                                                  ((char=? (string-ref n 1) #\D) (jolt-class-for "double"))
                                                  ((char=? (string-ref n 1) #\F) (jolt-class-for "float"))
                                                  ((char=? (string-ref n 1) #\I) (jolt-class-for "int"))
                                                  ((char=? (string-ref n 1) #\J) (jolt-class-for "long"))
                                                  ((char=? (string-ref n 1) #\S) (jolt-class-for "short"))
                                                  ((char=? (string-ref n 1) #\Z) (jolt-class-for "boolean"))
                                                  (else jolt-nil)))
                                           (else jolt-nil)))))
        ;; Class.isInstance(o) == (instance? class o); core.logic's deftype .equals
        ;; uses (.. this getClass (isInstance o)).
        (cons "isInstance" (lambda (self o) (if (instance-check self o) #t #f)))
        ;; --- reflection over the jch graph (epic jolt-of08.3) -----------------
        ;; getSuperclass: the graph's class edge — nil for Object, for an
        ;; interface (the JVM's null), and for a name the graph does not model
        ;; (statics-only shims like Math; recorded divergence).
        (cons "getSuperclass" (lambda (self)
                                (let ((s (jch-superclass (jclass-name self))))
                                  (if s (jolt-class-for s) jolt-nil))))
        ;; getInterfaces: the DIRECT super-interfaces, as a seqable of Class
        ;; values (the JVM's Class[] surfaces to Clojure as a seq anyway).
        (cons "getInterfaces" (lambda (self)
                                (list->cseq
                                 (map jolt-class-for
                                      (filter jch-interface?
                                              (jch-direct-supers (jclass-name self)))))))
        (cons "isInterface" (lambda (self) (if (jch-interface? (jclass-name self)) #t #f)))
        ;; isAssignableFrom: the graph's isa?, JVM argument order — self is the
        ;; wanted supertype. class-key so a deftype ctor or a name string on
        ;; either side answers too.
        (cons "isAssignableFrom" (lambda (self other)
                                   (let ((ka (class-key self)) (kb (class-key other)))
                                     (if (and ka kb (jch-isa? kb ka)) #t #f))))
        (cons "getConstructors" (lambda (self) (class-constructors self)))
        (cons "getDeclaredConstructors" (lambda (self) (class-constructors self)))
        ;; getModifiers: the JVM bitmask, derived from the class graph (jolt has
        ;; no bytecode to read one out of). Modifier's predicates read it.
        (cons "getModifiers" (lambda (self) (->num (jch-modifiers (jclass-name self)))))
        (cons "getClass" (lambda (self) (make-class-obj "java.lang.Class")))))

;; ---- java.lang.reflect.Modifier ---------------------------------------------
;; The bit constants and their predicates, the JVM's values, over whatever int a
;; caller has — Class.getModifiers here, and a member's modifiers read the same.
(define (mod-bit? m bit) (if (fx=? 0 (fxand (jnum->exact m) bit)) #f #t))
;; Modifier.toString's rendering, as a procedure rather than only as a registered
;; static: the reflective member objects (java/natives-array.ss) spell their own
;; toString the way the JVM does — "public static final java.lang.Object Foo.BAR" —
;; and must name the modifiers with the very words Modifier/toString would.
(define (jmod-string m)
  (let loop ((ps (list (cons 1 "public") (cons 4 "protected") (cons 2 "private")
                       (cons 1024 "abstract") (cons 8 "static") (cons 16 "final")
                       (cons 32 "synchronized") (cons 256 "native") (cons 2048 "strictfp")
                       (cons 128 "transient") (cons 64 "volatile") (cons 512 "interface")))
             (acc '()))
    (if (null? ps)
        (let ((names (reverse acc)))
          (if (null? names) ""
              (fold-left (lambda (a b) (string-append a " " b)) (car names) (cdr names))))
        (loop (cdr ps) (if (mod-bit? m (caar ps)) (cons (cdar ps) acc) acc)))))
(register-class-statics! "java.lang.reflect.Modifier"
  (list (cons "PUBLIC" (->num 1)) (cons "PRIVATE" (->num 2)) (cons "PROTECTED" (->num 4))
        (cons "STATIC" (->num 8)) (cons "FINAL" (->num 16)) (cons "SYNCHRONIZED" (->num 32))
        (cons "VOLATILE" (->num 64)) (cons "TRANSIENT" (->num 128)) (cons "NATIVE" (->num 256))
        (cons "INTERFACE" (->num 512)) (cons "ABSTRACT" (->num 1024)) (cons "STRICT" (->num 2048))
        (cons "isPublic" (lambda (m) (mod-bit? m 1)))
        (cons "isPrivate" (lambda (m) (mod-bit? m 2)))
        (cons "isProtected" (lambda (m) (mod-bit? m 4)))
        (cons "isStatic" (lambda (m) (mod-bit? m 8)))
        (cons "isFinal" (lambda (m) (mod-bit? m 16)))
        (cons "isSynchronized" (lambda (m) (mod-bit? m 32)))
        (cons "isVolatile" (lambda (m) (mod-bit? m 64)))
        (cons "isTransient" (lambda (m) (mod-bit? m 128)))
        (cons "isNative" (lambda (m) (mod-bit? m 256)))
        (cons "isInterface" (lambda (m) (mod-bit? m 512)))
        (cons "isAbstract" (lambda (m) (mod-bit? m 1024)))
        (cons "isStrict" (lambda (m) (mod-bit? m 2048)))
        ;; Modifier.toString lists the set bits in the JLS's canonical order.
        (cons "toString" jmod-string)))

;; ---- constructors as values -------------------------------------------------
;; Enough of java.lang.reflect.Constructor to pick a constructor by arity and call
;; it, which is how a Clojure-level reader builds a #ns.Rec[…] literal: find the
;; constructor whose parameter count matches, then invoke it. jolt knows the arity
;; of a deftype or defrecord constructor from its declared fields; for any other
;; class it has no signature to report, so getConstructors is empty there and a
;; caller sees the same "no matching constructor" it would for a real mismatch.
;; A record also carries the JVM's (fields + meta + ext-map) constructor, so an
;; arity-counting caller finds the same two it would on the JVM.
(define ctor-arity-probe '(0 1 2 3 4 5))
(define (ctor-obj cls arity) (make-jhost "class-ctor" (vector cls arity)))
(define (class-constructors cls)
  (let* ((nm (jclass-name cls))
         (dbl (hashtable-ref chez-record-dbl-tbl nm #f)))
    (cond
      ;; a deftype or defrecord: its fields ARE its signature
      (dbl (let ((n (vector-length dbl)))
             (if (hashtable-ref chez-record-type-tbl nm #f)
                 (jolt-vector (ctor-obj cls n) (ctor-obj cls (+ n 2)))
                 (jolt-vector (ctor-obj cls n)))))
      ;; a host class jolt backs: the arities its registered constructor accepts.
      ;; Many are variadic and coerce, so the probe stops at 5 rather than claiming
      ;; every arity; past that a caller gets the constructor's own error.
      ((lookup-class class-ctors-tbl nm)
       => (lambda (c)
            (let ((mask (procedure-arity-mask c)))
              (apply jolt-vector
                     (map (lambda (k) (ctor-obj cls k))
                          (filter (lambda (k) (bitwise-bit-set? mask k)) ctor-arity-probe))))))
      (else (jolt-vector)))))
;; "java.lang.Object, java.lang.Object" — the parameter list a member's toString
;; prints between its parens. jolt carries no signatures, so every parameter is an
;; Object; the COUNT is the part that is real, and it is the part a caller reading
;; the string is looking for.
(define (jreflect-param-str n)
  (if (fx=? n 0) ""
      (let loop ((k (fx- n 1)) (acc "java.lang.Object"))
        (if (fx=? k 0) acc (loop (fx- k 1) (string-append acc ", java.lang.Object"))))))
(register-host-methods! "class-ctor"
  (list (cons "getParameterCount" (lambda (self) (->num (vector-ref (jhost-state self) 1))))
        (cons "getParameterTypes"
              (lambda (self)
                (apply jolt-vector
                       (make-list (vector-ref (jhost-state self) 1) (jolt-class-for "java.lang.Object")))))
        (cons "getDeclaringClass" (lambda (self) (vector-ref (jhost-state self) 0)))
        ;; Constructor.getName is the DECLARING CLASS's fully qualified name on
        ;; the JVM, not a member name of its own.
        (cons "getName" (lambda (self) (jclass-name (vector-ref (jhost-state self) 0))))
        (cons "getModifiers" (lambda (self) (->num 1)))
        (cons "getExceptionTypes" (lambda (self) (make-jolt-array (vector) 'objects)))
        (cons "setAccessible" (lambda (self v) jolt-nil))
        (cons "newInstance" (lambda (self . args) (apply reflect-construct (vector-ref (jhost-state self) 0) args)))
        ;; the JVM's shape — "public user.Foo(java.lang.Object, java.lang.Object)".
        ;; It used to print the bare class name, which is also what getName answers,
        ;; so two constructors of different arity were indistinguishable in output.
        (cons "toString"
              (lambda (self)
                (string-append "public " (jclass-name (vector-ref (jhost-state self) 0))
                               "(" (jreflect-param-str (vector-ref (jhost-state self) 1)) ")")))))
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "class-ctor")))
                      (lambda (x) (record-method-dispatch x "toString" jolt-nil)))

;; ---- clojure.lang.Reflector -------------------------------------------------
;; The reflective entry points Clojure code reaches for by name. Each is the
;; dynamic form of an interop call jolt already performs, so they route to the
;; same registries rather than to a second mechanism.
(define (reflect-args a) (if (jolt-nil? a) '() (seq->list (jolt-seq a))))
(define (reflect-construct cls . args)
  (apply host-new (if (jclass? cls) (jclass-name cls) (jolt-str-render-one cls)) args))
(register-class-statics! "clojure.lang.Reflector"
  (list (cons "invokeConstructor"
              (lambda (cls args) (apply reflect-construct cls (reflect-args args))))
        (cons "invokeStaticMethod"
              (lambda (cls method args)
                (apply host-static-call (if (jclass? cls) (jclass-name cls) (jolt-str-render-one cls))
                       (jolt-str-render-one method) (reflect-args args))))
        (cons "invokeInstanceMethod"
              (lambda (target method args)
                (record-method-dispatch target (jolt-str-render-one method)
                                        (list->cseq (reflect-args args)))))))
;; A deftype/defrecord type token answers every java.lang.Class method, through
;; the SAME table (class inst) uses — records-dispatch.ss owns the token arm and
;; loads before this file, so it calls back through here. Returns a one-element
;; list holding the result (so a nil/#f answer is still a hit) or #f for "no such
;; method", which is what lets that arm fall through to its own error.
(set-rd-class-method-hook!
  (lambda (tag method-name args)
    (let* ((h (hashtable-ref host-methods-tbl "class" #f))
           (f (and h (hashtable-ref h method-name #f))))
      (and f (list (apply f (jolt-class-for tag) args))))))

;; (class x) on a jclass value returns java.lang.Class, so (instance? Class
;; (class y)) and class-based dispatch see the correct JVM class.
(register-class-arm! jclass? (lambda (x) "java.lang.Class"))

;; (jolt.host/table? x) — is x a host tagged-table?
(def-var! "jolt.host" "table?" (lambda (x) (if (htable? x) #t #f)))

;; --- java.util.Arrays -------------------------------------------------------
;; Arrays/sort sorts IN PLACE and returns void, so it writes back through the
;; array's own backing (a Chez vector, or an flvector for the double/float element
;; kinds) rather than building a new array — orchard.profile relies on
;; (doto (Arrays/copyOfRange …) Arrays/sort). list-sort is a stable merge sort,
;; matching Arrays.sort over objects. The comparator goes through cmp->less, the
;; shared comparator seam, so a reify/deftype Comparator works here exactly as it
;; does in sort / sort-by.
;; JVM overloads: sort(a), sort(a, cmp), sort(a, from, to), sort(a, from, to, cmp).
;; Two args means a comparator — sort(a, from) is not an overload.
(define (arrays-sort! a from to cmp)
  (let* ((bv (jolt-array-vec a))
         (f (jnum->exact from))
         (t (if to (jnum->exact to) (ja-len bv)))
         (less? (cmp->less cmp))
         (items (let loop ((i f) (acc '()))
                  (if (fx>=? i t) (reverse acc) (loop (fx+ i 1) (cons (ja-ref bv i) acc))))))
    (let loop ((i f) (xs (list-sort less? items)))
      (if (null? xs) jolt-nil
          (begin (ja-set! bv i (car xs)) (loop (fx+ i 1) (cdr xs)))))))
(define arrays-sort
  (case-lambda
    ((a) (arrays-sort! a 0 #f jolt-compare))
    ((a cmp) (arrays-sort! a 0 #f cmp))
    ((a from to) (arrays-sort! a from to jolt-compare))
    ((a from to cmp) (arrays-sort! a from to cmp))))
(let ((arrays-statics
       (list
         (cons "equals" (lambda (a b)
                          (cond ((and (jolt-nil? a) (jolt-nil? b)) #t)
                                ((or (jolt-nil? a) (jolt-nil? b)) #f)
                                (else (equal? (jolt-array-vec a) (jolt-array-vec b))))))
         (cons "fill" (lambda (a v)
                        (let* ((bv (jolt-array-vec a)) (n (ja-len bv))
                               (v (na-elem-of (jolt-array-kind a) v)))
                          (do ((i 0 (fx+ i 1))) ((fx=? i n) jolt-nil) (ja-set! bv i v)))))
         (cons "copyOf" (lambda (a n)
                          (let* ((src (jolt-array-vec a)) (len (jnum->exact n)) (kind (jolt-array-kind a))
                                 (out (na-make-backing len kind (if (na-fl-kind? kind) 0.0 0))))
                            (do ((i 0 (fx+ i 1))) ((fx=? i (min len (ja-len src))))
                              (ja-set! out i (ja-ref src i)))
                            (make-jolt-array out kind))))
         (cons "copyOfRange" (lambda (a from to)
                               (let* ((src (jolt-array-vec a)) (f (jnum->exact from)) (tt (jnum->exact to))
                                      (len (- tt f)) (kind (jolt-array-kind a))
                                      (out (na-make-backing len kind (if (na-fl-kind? kind) 0.0 0))))
                                 (do ((i 0 (fx+ i 1))) ((fx=? i len))
                                   (ja-set! out i (ja-ref src (+ f i))))
                                 (make-jolt-array out kind))))
         (cons "sort" arrays-sort)
         ;; Arrays.toString is "[a, b]" — comma-separated element toString, "null"
         ;; for a nil array. It used to print the elements as a jolt VECTOR, which
         ;; renders "[a b]" (no commas) and pr-quotes a string element.
         (cons "toString" (lambda (a)
                            (if (jolt-nil? a) "null"
                                (let ((parts (map (lambda (x) (if (jolt-nil? x) "null" (jolt-str-render-one x)))
                                                  (ja->list (jolt-array-vec a)))))
                                  (string-append
                                    "[" (if (null? parts) ""
                                            (fold-left (lambda (acc s) (string-append acc ", " s))
                                                       (car parts) (cdr parts)))
                                    "]"))))))))
  (register-class-statics! "Arrays" arrays-statics)
  (register-class-statics! "java.util.Arrays" arrays-statics))

;; --- java.util.Random -------------------------------------------------------
;; Java-compatible LCG: java.util.Random's exact algorithm.
;; State is #(seed) where seed is a 48-bit exact integer.
;; Reference: JDK java.util.Random source.

(define random-multiplier #x5DEECE66D)
(define random-addend #xB)
(define random-mask #xFFFFFFFFFFFF)  ;; (1<<48)-1

(define (random-init-seed given-seed)
  (bitwise-and (bitwise-xor (exact (truncate given-seed)) random-multiplier) random-mask))

(define (random-next bits seed-vec)
  (let* ((old-seed (vector-ref seed-vec 0))
         (new-seed (bitwise-and (+ (* old-seed random-multiplier) random-addend) random-mask)))
    (vector-set! seed-vec 0 new-seed)
    (bitwise-arithmetic-shift-right new-seed (- 48 bits))))

;; Convert an unsigned 32-bit value to signed 32-bit (Java's (int) cast).
(define (random-u32->s32 v)
  (if (>= v #x80000000) (- v #x100000000) v))

;; Simulate Java's 32-bit signed addition/subtraction overflow — used for the
;; rejection-sampling check in nextInt(bound): (u - r + bound - 1) < 0,
;; where the overflow of the intermediate 32-bit signed expression is the
;; rejection criterion.
(define (random-overflow-lt0 u r bound)
  (let ((raw (+ (- u r) bound -1)))
    (< (random-u32->s32 (bitwise-and raw #xFFFFFFFF)) 0)))

(for-each
  (lambda (nm)
    (register-class-ctor! nm
      (lambda args
        ;; No-seed (Random.) used (truncate (current-time)) — current-time returns a
        ;; time OBJECT, not a number, so truncate threw and the no-arg ctor never
        ;; worked at all. The JVM seeds this instance from nanoTime mixed with a
        ;; uniquifier; jolt-random is already seeded per process and per thread, so
        ;; drawing the 48-bit seed from it gives distinct instances the same way.
        (let ((given (if (pair? args) (car args) (jolt-random 281474976710656))))
          (make-jhost "random" (vector (random-init-seed given)))))))
  '("Random" "java.util.Random"))
(register-host-methods! "random"
  (list
    ;; nextBytes draws one nextInt() per FOUR bytes, taking them low-to-high — it
    ;; does not draw 8 bits per byte, which consumed the LCG four times as fast and
    ;; produced a different stream from the JVM's for the same seed. Elements are
    ;; signed bytes (the JVM's (byte)rnd cast).
    (cons "nextBytes" (lambda (self ba)
                        (let* ((v (jolt-array-vec ba)) (n (vector-length v))
                               (st (jhost-state self)))
                          (let loop ((i 0))
                            (when (fx<? i n)
                              (let inner ((rnd (random-u32->s32 (random-next 32 st)))
                                          (k (min (fx- n i) 4)) (i i))
                                (if (fx=? k 0) (loop i)
                                    (begin (vector-set! v i (na-byte-of (bitwise-and rnd #xff)))
                                           (inner (bitwise-arithmetic-shift-right rnd 8)
                                                  (fx- k 1) (fx+ i 1)))))))
                          jolt-nil)))
    (cons "nextInt" (lambda (self . a)
                      (let ((st (jhost-state self)))
                        (if (pair? a)
                            (let ((bound (exact (truncate (car a)))))
                              (if (<= bound 0)
                                  (throw-jvm (quote IllegalArgumentException) "bound must be positive")
                                  (let ((m (- bound 1)))
                                    (if (fx=? (bitwise-and bound m) 0)
                                        ;; power of two
                                        (->num (random-u32->s32
                                                 (bitwise-arithmetic-shift-right
                                                   (* bound (random-next 31 st)) 31)))
                                        ;; rejection sample with 32-bit overflow semantics
                                        (let loop ((u (random-u32->s32 (random-next 31 st))))
                                          (let ((r (modulo u bound)))
                                            (if (random-overflow-lt0 u r bound)
                                                (loop (random-u32->s32 (random-next 31 st)))
                                                (->num r))))))))
                            (->num (random-u32->s32 (random-next 32 st)))))))
    (cons "nextLong" (lambda (self)
                       (let ((st (jhost-state self)))
                         (let* ((hi (random-u32->s32 (random-next 32 st)))
                                (lo (random-u32->s32 (random-next 32 st))))
                           (->num (+ (* hi (expt 2 32)) lo))))))
    (cons "nextDouble" (lambda (self)
                         (let* ((st (jhost-state self))
                                (hi (random-next 26 st))
                                (lo (random-next 27 st)))
                           (* (+ (* hi (expt 2 27)) lo)
                              (/ 1.0 (expt 2 53))))))
    (cons "nextFloat" (lambda (self)
                        (let ((st (jhost-state self)))
                          (/ (random-next 24 st) (exact->inexact (expt 2 24))))))
    (cons "nextBoolean" (lambda (self) (fx=? 1 (random-next 1 (jhost-state self)))))))

;; --- java.security.SecureRandom ----------------------------------------------
;; Every draw comes straight from the OS CSPRNG (jolt-random-bytes), so there is
;; no seed and no internal state to carry: unlike java.util.Random above, two
;; instances are not distinguishable and none of this is reproducible. That is
;; the contract — SecureRandom promises unpredictability, and the JVM explicitly
;; does not promise a repeatable stream for a given seed.
(define (sr-uint n)
  (let ((bv (jolt-random-bytes n)))
    (let loop ((i 0) (acc 0))
      (if (fx=? i n) acc (loop (fx+ i 1) (+ (* acc 256) (bytevector-u8-ref bv i)))))))

(define (sr-next-int-bound bound)
  (if (<= bound 0)
      (throw-jvm (quote IllegalArgumentException) "bound must be positive")
      ;; Rejection sampling, not a bare modulo: taking (mod u bound) over a range
      ;; that is not a multiple of bound makes the low residues more likely, which
      ;; is a real bias in something callers reach for to pick tokens and salts.
      (let loop ()
        (let* ((u (bitwise-and (sr-uint 4) #x7fffffff))
               (r (modulo u bound)))
          (if (<= (- u r) (- 2147483648 bound)) (->num r) (loop))))))

(for-each
  (lambda (nm)
    (register-class-ctor! nm
      ;; (SecureRandom. seed) is accepted and the seed ignored: the JVM's seeded
      ;; ctor SUPPLEMENTS entropy rather than replacing it, so ignoring it cannot
      ;; make the output weaker than the caller asked for.
      (lambda args (make-jhost "securerandom" #f)))
    (register-class-statics! nm
      (list (cons "getInstance" (lambda args (make-jhost "securerandom" #f)))
            (cons "getInstanceStrong" (lambda args (make-jhost "securerandom" #f))))))
  '("SecureRandom" "java.security.SecureRandom"))

(register-host-methods! "securerandom"
  (list
    (cons "nextBytes" (lambda (self ba)
                        (let* ((v (jolt-array-vec ba))
                               (n (vector-length v))
                               (bv (jolt-random-bytes n)))
                          (let loop ((i 0))
                            (when (fx<? i n)
                              (vector-set! v i (na-byte-of (bytevector-u8-ref bv i)))
                              (loop (fx+ i 1))))
                          jolt-nil)))
    (cons "nextInt" (lambda (self . a)
                      (if (pair? a)
                          (sr-next-int-bound (exact (truncate (car a))))
                          (->num (random-u32->s32 (sr-uint 4))))))
    (cons "nextLong" (lambda (self)
                       (let ((u (sr-uint 8)))
                         (->num (if (>= u #x8000000000000000) (- u #x10000000000000000) u)))))
    (cons "nextDouble" (lambda (self)
                         (* (bitwise-and (sr-uint 7) (- (expt 2 53) 1)) (/ 1.0 (expt 2 53)))))
    (cons "nextFloat" (lambda (self)
                        (/ (bitwise-and (sr-uint 3) (- (expt 2 24) 1))
                           (exact->inexact (expt 2 24)))))
    (cons "nextBoolean" (lambda (self) (fx=? 1 (bitwise-and (sr-uint 1) 1))))
    (cons "generateSeed" (lambda (self n)
                           (na-bv->bytearray (jolt-random-bytes (exact (truncate n))))))
    ;; setSeed supplements on the JVM and never replaces; with an OS source there
    ;; is nothing to supplement, so it is a no-op rather than a weakening.
    (cons "setSeed" (lambda (self . _) jolt-nil))))

;; --- java.util.Optional -----------------------------------------------------
;; Returned by getters across java.time / java.net.http (e.g. HttpRequest.timeout,
;; HttpClient.connectTimeout). Value-equal so (= (Optional/of x) (Optional/of x)).
(define (jt-optional present? value) (make-jhost "optional" (vector present? value)))
(define jt-optional-empty (jt-optional #f jolt-nil))
(define (opt? x) (and (jhost? x) (string=? (jhost-tag x) "optional")))
(define (opt-present? o) (vector-ref (jhost-state o) 0))
(define (opt-value o) (vector-ref (jhost-state o) 1))
(let ((statics (list (cons "of" (lambda (v) (if (jolt-nil? v) (throw-jvm 'NullPointerException "Optional.of(null)") (jt-optional #t v))))
                     (cons "ofNullable" (lambda (v) (if (jolt-nil? v) jt-optional-empty (jt-optional #t v))))
                     (cons "empty" (lambda _ jt-optional-empty)))))
  (register-class-statics! "Optional" statics)
  (register-class-statics! "java.util.Optional" statics))
(register-host-methods! "optional"
  (list (cons "isPresent" (lambda (o) (opt-present? o)))
        (cons "isEmpty" (lambda (o) (not (opt-present? o))))
        (cons "get" (lambda (o) (if (opt-present? o) (opt-value o) (throw-jvm 'NoSuchElementException "No value present"))))
        (cons "orElse" (lambda (o d) (if (opt-present? o) (opt-value o) d)))
        (cons "orElseGet" (lambda (o f) (if (opt-present? o) (opt-value o) (jolt-invoke f))))
        (cons "ifPresent" (lambda (o f) (when (opt-present? o) (jolt-invoke f (opt-value o))) jolt-nil))
        (cons "toString" (lambda (o) (if (opt-present? o)
                                         (string-append "Optional[" (jolt-str-render-one (opt-value o)) "]")
                                         "Optional.empty")))))
(register-eq-arm! (lambda (a b) (or (opt? a) (opt? b)))
                  (lambda (a b) (and (opt? a) (opt? b) (eq? (opt-present? a) (opt-present? b))
                                     (or (not (opt-present? a)) (jolt=2 (opt-value a) (opt-value b))))))

;; class hierarchy lives in class-hierarchy.ss (jvm-class-parents).
;; fn classes (ns$name) inherit AFunction (handled by jch-direct-supers).

;; (instance? Class e) on a throwable tagged-table carrying a JVM :class matches the
;; carried class or any of its ancestors (full name or last segment), so a library's
;; (catch UnknownHostException e …) / (catch IOException e …) matches the ex-info
;; envelope it threw. Mirrors the (class e) arm (host-table.ss) for catch dispatch,
;; which lowers to (instance? C e). Non-match returns 'pass so other arms still run.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (htable? val) (string? (hashtable-ref (htable-h val) "class" #f)))
        (let* ((cls (hashtable-ref (htable-h val) "class" #f))
               (want (symbol-t-name type-sym))
               (want-seg (hsc-last-segment want)))
          (let loop ((names (cons cls (jch-closure cls))))
            (cond ((null? names) 'pass)
                  ((or (string=? want (car names))
                       (string=? want-seg (hsc-last-segment (car names)))) #t)
                  (else (loop (cdr names))))))
        'pass)))

;; JVM class assignability for isa? (20-coll): true when child and parent are both
;; class values and parent is child, java.lang.Object (every class's root), or a
;; modeled ancestor of child (full name or last segment). nil for non-class args, so
;; isa? falls through to its hierarchy/vector logic.
(def-var! "jolt.host" "class-isa?"
  (lambda (child parent)
    (let ((cc (class-key child)) (pp (class-key parent)))
      (if (and cc pp)
          (let ((pseg (hsc-last-segment pp)))
            (if (let loop ((names (cons cc (jch-closure cc))))
                  (cond ((string=? pp "java.lang.Object") #t)
                        ((null? names) #f)
                        ((or (string=? pp (car names))
                             (string=? pseg (hsc-last-segment (car names)))) #t)
                        (else (loop (cdr names)))))
                #t jolt-nil))
          jolt-nil))))

;; is NAME a class the host models (registered in the class graph, or a fn class)?
;; Object itself is modeled.
(define (hsc-class-known? name)
  (or (string=? name "java.lang.Object")
      (jch-known? name)
      (str-has-dollar? name)))

;; (jolt.host/class-supers name) / (jolt.host/class-ancestors name) — a jolt seq of
;; super / ancestor class-name strings (transitive, Object-rooted), or nil when
;; jolt models no hierarchy for it. class-bases is the DIRECT supers (clojure.core
;; `bases` / the class arm of `parents`). Each result element is an interned jclass
;; so (= (first (parents Long)) Number) and contains? work against class tokens.
(def-var! "jolt.host" "class-supers"
  (lambda (x)
    (let ((name (class-key x)))
      (if name
          (let ((as (jch-ancestors-rooted name)))
            (if (null? as) jolt-nil (list->cseq (map jolt-class-for as))))
          jolt-nil))))
(def-var! "jolt.host" "class-ancestors"
  (lambda (x)
    (let ((name (class-key x)))
      (if name
          (let ((as (jch-ancestors-rooted name)))
            (if (null? as) jolt-nil (list->cseq (map jolt-class-for as))))
          jolt-nil))))
;; The direct bases of a class as Class objects, superclass first the way the
;; JVM's `bases` orders them: a concrete class whose row names no concrete super
;; extends Object, so Object leads its list (interfaces have no superclass and
;; Object itself none at all). This is clojure.core/bases too — it answered name
;; STRINGS where supers answered Class objects, so (.getName (first (bases c)))
;; failed on every class, and typed.clojure's RClass ancestry (Class->symbol
;; over (bases cls)) with it.
(define (jolt-class-bases x)
  (let ((name (class-key x)))
    (if name
        (let* ((ds (jch-direct-supers name))
               ;; jch-superclass is "java.lang.Object" exactly for a known
               ;; concrete class with no modeled concrete super; #f for Object,
               ;; an interface, or a name the graph does not model (a fn class
               ;; keeps its AFunction row and nothing else, as on the JVM).
               (ds (if (and (equal? (jch-superclass name) "java.lang.Object")
                            (not (member "java.lang.Object" ds)))
                       (cons "java.lang.Object" ds)
                       ds)))
          (if (null? ds) jolt-nil (list->cseq (map jolt-class-for ds))))
        jolt-nil)))
(def-var! "jolt.host" "class-bases" jolt-class-bases)
(def-var! "clojure.core" "bases" jolt-class-bases)
;; is X a class value — a jclass, a deftype ctor, or a name string the host
;; graph models?
(def-var! "jolt.host" "class-value?"
  (lambda (x)
    (if (jclass? x)
        #t
        (let ((n (class-key x)))
          (if (and n (hsc-class-known? n)) #t jolt-nil)))))

;; ---- (class x) for host-shim values ------------------------------------------
;; jhost-backed shims report their JVM class instead of falling through to the
;; opaque :object rendering, so class-driven dispatch (and type-classification
;; libraries reading (type x)) see the real name.
;; jhost value class names derive from the single jhost-tag->fqn registry
;; (class-hierarchy.ss) — one row per shim tag, shared with value-host-tags and
;; the instance? arm below so all three stay in agreement.
(register-class-arm!
  (lambda (x) (and (jhost? x) (jhost-fqn (jhost-tag x)) #t))
  (lambda (x) (jhost-fqn (jhost-tag x))))
;; sorted collections and transients report their JVM classes. jolt's one
;; transient-map representation reports TransientHashMap (the JVM also has
;; PersistentArrayMap$TransientArrayMap for small maps).
(register-class-arm! htable-sorted-map? (lambda (x) "clojure.lang.PersistentTreeMap"))
(register-class-arm! htable-sorted-set? (lambda (x) "clojure.lang.PersistentTreeSet"))
(register-class-arm!
  (lambda (x) (and (jolt-transient? x) #t))
  (lambda (x)
    (case (jolt-transient-kind x)
      ((vec) "clojure.lang.PersistentVector$TransientVector")
      ;; a transient over an array-mode map carries its insertion order
      ((map) (if (jolt-transient-array-map? x)
                 "clojure.lang.PersistentArrayMap$TransientArrayMap"
                 "clojure.lang.PersistentHashMap$TransientHashMap"))
      ((set) "clojure.lang.PersistentHashSet$TransientHashSet")
      (else "clojure.lang.ATransientCollection"))))
;; instance? for these shims derives from the class graph: the value's class name
;; (jhost-fqn) walked through jch-isa? answers interface questions —
;; (instance? java.util.List an-ArrayList), Deque/Queue/Collection/Iterable chains.
;; Widening only: an unknown pairing passes to the other arms, never denies.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (jhost? val) (symbol-t? type-sym))
        (let ((fqn (jhost-fqn (jhost-tag val))))
          (if fqn
              (let* ((tname (symbol-t-name type-sym))
                     (q (or (resolve-class-hint tname) tname)))
                (if (jch-isa? fqn q) #t 'pass))
              'pass))
        'pass)))
;; count over the mutable collection shims, like RT.count over a java.util
;; Collection/Map on the JVM. Each shim registers a count arm (the hashmap arm
;; is registered with its get/contains arms above); jolt-count walks the arms
;; newest-first for jhost values, so these replace an earlier set!-wrap of
;; jolt-count that duplicated the hashmap arm.
(register-count-arm! al-family? (lambda (c) (al-cnt c)))
(register-count-arm! hs-hashset? (lambda (c) (hashtable-size (hm-tbl c))))
;; IEditableCollection / ITransient* answer from the representation: the
;; transient-able persistent collections are editable; a transient reports its
;; kind's interfaces. Widening only — anything else passes to the other arms.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (symbol-t? type-sym)
        (let* ((tn (symbol-t-name type-sym))
               (short (let loop ((i (- (string-length tn) 1)))
                        (cond ((< i 0) tn)
                              ((char=? (string-ref tn i) #\.) (substring tn (+ i 1) (string-length tn)))
                              (else (loop (- i 1)))))))
          (cond
             ((string=? short "IEditableCollection")
              ;; a MapEntry is pvec-backed but not editable on the JVM
              (if (or (and (pvec? val) (not (jolt-map-entry? val))) (pset? val)
                      (pmap? val))
                 #t 'pass))
            ((string=? short "ITransientCollection")
             (if (jolt-transient? val) #t 'pass))
            ((string=? short "ITransientVector")
             (if (and (jolt-transient? val) (eq? 'vec (jolt-transient-kind val))) #t 'pass))
            ((or (string=? short "ITransientMap") (string=? short "ITransientAssociative"))
             (if (and (jolt-transient? val) (eq? 'map (jolt-transient-kind val))) #t 'pass))
            ((string=? short "ITransientSet")
             (if (and (jolt-transient? val) (eq? 'set (jolt-transient-kind val))) #t 'pass))
            (else 'pass)))
        'pass)))
;; (seq a-HashMap) walks its entries, like RT.seqFrom over a java.util.Map.
(register-seq-arm! hm-hashmap? (lambda (x) (jolt-seq (hm->pmap x))))
;; The single place that knows which java.util shims are Iterable/seqable on the
;; JVM (ArrayList/LinkedList/ArrayDeque via al-family?, HashSet, HashMap);
;; post-prelude's clojure.core/seqable? patch consults this instead of carrying
;; its own tag list.
(define (jhost-seqable-shim? x)
  (or (al-family? x) (hs-hashset? x) (hm-hashmap? x)))
;; a MapEntry does not carry meta on the JVM (AMapEntry); deny IObj/IMeta so the
;; pvec backing doesn't claim it.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (symbol-t? type-sym) (jolt-map-entry? val))
        (let ((tn (symbol-t-name type-sym)))
          (if (or (string=? tn "IObj") (string=? tn "clojure.lang.IObj")
                  (string=? tn "IMeta") (string=? tn "clojure.lang.IMeta"))
              #f 'pass))
        'pass)))
;; a reader-conditional value reports its JVM class, not its tagged-map backing.
(define (reader-conditional-value? x)
  (jolt-reader-conditional-record? x))
(register-class-arm! reader-conditional-value? (lambda (x) "clojure.lang.ReaderConditional"))
;; a multimethod reports its JVM class.
(register-class-arm! (lambda (x) (jolt-multifn? x)) (lambda (x) "clojure.lang.MultiFn"))
;; exact-own-class fallback: (instance? C x) is true when C names x's own class —
;; covers checks against a captured (class y) value (transient classes, MultiFn)
;; that no interface arm models. Widening only. The class's ANCESTRY needs no
;; arm of its own: the interface arm above reads value-host-tags, which derives
;; from the class graph for every value whose class arm names a modeled class,
;; so a multimethod is an AFn and a transient is Counted the moment the graph
;; says so. (An ancestry walk here, ahead of the base, was measured at 1.26x on
;; an (instance? IPersistentMap a-vector) miss — the value's class name is the
;; expensive part, and this arm already pays it once.)
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (symbol-t? type-sym)
        (let* ((tn (symbol-t-name type-sym))
               (q (or (resolve-class-hint tn) tn))
               (cn (jolt-class-name val)))
          (if (and (string? cn) (string=? cn q)) #t 'pass))
        'pass)))
;; (instance? Class x) / (instance? java.lang.Class x): a jclass value IS a Class.
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tn (symbol-t-name type-sym)))
      (if (member tn '("Class" "java.lang.Class"))
          (if (jclass? val) #t #f)
          'pass))))
;; a Class OBJECT specifically ((class x) result) — narrower than class-value?,
;; which also admits deftype ctors and modeled name strings. The instance?
;; macro needs exactly this: evaluate a var-held Class, keep quoting record names.
;; class? is true for a modeled host Class value AND for a deftype/defrecord type
;; token — jolt represents a record type by its make-deftype-ctor closure (the
;; same value instance?/ancestors dispatch on), so (class? Bar) holds like the JVM.
;; (jolt unifies Bar with ->Bar, so (class? ->Bar) also holds — a record's name and
;; its positional ctor are one value here.)
(def-var! "jolt.host" "class-object?"
  (lambda (x) (if (or (jclass? x)
                      (and (procedure? x) (deftype-ctor-tag x) #t))
                  #t #f)))
;; nth over the java.util List shims, like RT.nth on a java.util.List.
;; This stays a set!-wrap of jolt-nth rather than a registered arm: unlike
;; jolt-count (which has a register-count-arm! registry), jolt-nth is an inline
;; case-lambda with no nth-arm registry, so there is no arm mechanism to move
;; this into. Converting an al-family List to a cseq here and delegating to the
;; base jolt-nth is the only way nth reaches these shims. See the note by
;; register-count-arm! in collections.ss — jolt-nth's migration to an arm
;; registry is deferred until it gets its own bench guard.
;; A StringBuilder rides the same wrap: RT.nth reads a CharSequence by charAt, and
;; delegating its content string reuses the base nth's own bounds reporting.
(define %shim-nth jolt-nth)
(define (shim-nth-target coll)
  (cond ((al-family? coll) (list->cseq (al->list coll)))
        ((sb-jhost? coll) (sb-str coll))
        (else coll)))
(set! jolt-nth
  (case-lambda
    ((coll i) (%shim-nth (shim-nth-target coll) i))
    ((coll i d) (%shim-nth (shim-nth-target coll) i d))))
(def-var! "clojure.core" "nth" jolt-nth)

;; --- java.nio.charset ---------------------------------------------------------
;; Charset as a jhost "charset" over #(canonical-name encode-max): encode-max is
;; the largest char code the charset encodes (#x10FFFF for UTF-*, i.e. every
;; char — Chez strings are full Unicode). forName is case-insensitive and
;; accepts the JVM's common aliases; unknown names throw
;; UnsupportedCharsetException, malformed ones IllegalCharsetNameException (the
;; JVM's distinction: IllegalCharsetNameException for names that can't be a
;; charset name, UnsupportedCharsetException for well-formed but unknown).
(define (charset-legal-name? s)
  (and (> (string-length s) 0)
       (let loop ((i 0))
         (cond ((= i (string-length s)) #t)
               ((let ((c (string-ref s i)))
                  (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\- #\_ #\. #\:))))
                (loop (+ i 1)))
               (else #f)))))
;; (canonical-name encode-max alias …). The alias lists are the JVM's own —
;; java.nio.charset.Charset/aliases, read off OpenJDK 21 — because that is what
;; libraries pass: clj-http-lite hands (Charset/forName body-encoding) whatever
;; the caller wrote, and "ASCII" is how everyone spells US-ASCII. Matching only
;; the canonical names threw UnsupportedCharsetException on names the JVM
;; resolves. The UTF-32 family is here too; the encode/decode paths below
;; already handled it, but forName rejected it.
(define charset-table
  '(("US-ASCII" 127
     "ASCII" "ascii7" "646" "ANSI_X3.4-1968" "ANSI_X3.4-1986" "IBM367"
     "ISO646-US" "ISO_646.irv:1991" "iso_646.irv:1983" "cp367" "csASCII"
     "iso-ir-6" "us")
    ("ISO-8859-1" 255
     "latin1" "l1" "819" "8859_1" "IBM-819" "IBM819" "ISO8859-1" "ISO8859_1"
     "ISO_8859-1" "ISO_8859-1:1987" "ISO_8859_1" "cp819" "csISOLatin1"
     "iso-ir-100")
    ("UTF-8"    #x10FFFF "UTF8" "unicode-1-1-utf-8")
    ("UTF-16"   #x10FFFF "UTF_16" "UnicodeBig" "unicode" "utf16")
    ("UTF-16BE" #x10FFFF "UTF_16BE" "ISO-10646-UCS-2" "UnicodeBigUnmarked" "X-UTF-16BE")
    ("UTF-16LE" #x10FFFF "UTF_16LE" "UnicodeLittleUnmarked" "X-UTF-16LE")
    ("UTF-32"   #x10FFFF "UTF32" "UTF_32")
    ("UTF-32BE" #x10FFFF "UTF_32BE" "X-UTF-32BE")
    ("UTF-32LE" #x10FFFF "UTF_32LE" "X-UTF-32LE")))

(define (charset-lookup s)
  (let loop ((t charset-table))
    (cond ((null? t) #f)
          ;; the canonical name and every alias, case-insensitively (the JVM's
          ;; forName is case-insensitive over both).
          ((let names ((ns (cons (caar t) (cddar t))))
             (cond ((null? ns) #f)
                   ((string-ci=? s (car ns)) #t)
                   (else (names (cdr ns)))))
           (cons (caar t) (cadar t)))
          (else (loop (cdr t))))))

;; A charset name (canonical or alias) lowercased to its CANONICAL form, so the
;; encode/decode dispatches below match one spelling each instead of carrying
;; their own partial alias lists — which is how (.getBytes s "l1") used to
;; silently produce UTF-8 bytes. An unknown name passes through unchanged and
;; still hits each dispatch's UTF-8 fallback.
(define (charset-canonical-down s)
  (let ((hit (charset-lookup s)))
    (ascii-string-down (if hit (car hit) s))))
(define (charset-for-name s)
  (let ((s (jolt-str-render-one s)))
    (if (not (charset-legal-name? s))
        (throw-jvm 'java.nio.charset.IllegalCharsetNameException s)
        (let ((hit (charset-lookup s)))
          (cond
            (hit (make-jhost "charset" (vector (car hit) (cdr hit))))
            ;; the table lists the charsets Chez encodes natively; the rest come
            ;; from the system iconv, so forName must accept whatever encoding
            ;; through it will accept or the two disagree about the same name.
            ((iconv-known? s) (make-jhost "charset" (vector s #x10FFFF)))
            (else (throw-jvm 'java.nio.charset.UnsupportedCharsetException s)))))))
(define (charset-name c) (vector-ref (jhost-state c) 0))
(define (charset-encode-max c) (vector-ref (jhost-state c) 1))
;; One charset ARGUMENT — a name string or a Charset object — as its name string.
;; A Charset jhost does not render to its name (jolt-str-render-one gives
;; "#object[java.nio.charset.Charset]"), so encoding through an object used to
;; match no arm and fall through to the UTF-8 default: (.getBytes "õ" (Charset/
;; forName "ISO-8859-1")) returned the UTF-8 bytes [195 181] instead of [245].
;; ASCII-only text and an explicit UTF-8 request both hid it.
(define (charset-arg-name c)
  (cond ((string? c) c)
        ((and (jhost? c) (string=? (jhost-tag c) "charset")) (charset-name c))
        (else (jolt-str-render-one c))))
(register-class-statics! "java.nio.charset.Charset"
  (list (cons "forName" charset-for-name)
        (cons "defaultCharset" (lambda () (make-jhost "charset" (vector "UTF-8" #x10FFFF))))
        (cons "isSupported" (lambda (s) (if (charset-lookup (jolt-str-render-one s)) #t #f)))))
(register-host-methods! "charset"
  (list (cons "name" (lambda (c) (charset-name c)))
        (cons "displayName" (lambda (c) (charset-name c)))
        (cons "toString" (lambda (c) (charset-name c)))
        (cons "newEncoder" (lambda (c) (make-jhost "charset-encoder" (vector c))))
        (cons "newDecoder" (lambda (c) (make-jhost "charset-decoder" (vector c))))
        (cons "canEncode" (lambda (c) #t))
        (cons "equals" (lambda (c o) (and (jhost? o) (string=? (jhost-tag o) "charset")
                                          (string=? (charset-name c) (charset-name o)))))))
;; CharsetEncoder.canEncode: over a string, every char must fit; over a char the
;; char itself. A char is a Scheme char in jolt; a string a string.
(register-host-methods! "charset-encoder"
  (list (cons "canEncode"
              (lambda (e x)
                (let ((lim (charset-encode-max (vector-ref (jhost-state e) 0))))
                  (cond ((char? x) (if (<= (char->integer x) lim) #t #f))
                        ((string? x) (let loop ((i 0))
                                       (cond ((= i (string-length x)) #t)
                                             ((> (char->integer (string-ref x i)) lim) #f)
                                             (else (loop (+ i 1))))))
                        (else #f)))))
        (cons "charset" (lambda (e) (vector-ref (jhost-state e) 0)))))
(register-host-methods! "charset-decoder"
  (list (cons "charset" (lambda (d) (vector-ref (jhost-state d) 0)))))
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "charset"))) (lambda (x) "java.nio.charset.Charset"))
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "charset-encoder"))) (lambda (x) "java.nio.charset.CharsetEncoder"))
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "charset-decoder"))) (lambda (x) "java.nio.charset.CharsetDecoder"))

;; --- class-token def-vars as Class objects -----------------------------------
;; Short names (String, Long, HashMap) and FQN value-class names (java.lang.Long,
;; clojure.lang.Atom) evaluate to interned Class objects via the global interner
;; (jolt-class-for), so (= (class x) String) and (instance? Long x) work without
;; a string=class bridge. class-token-alist and class-fqn-list come from
;; host-class.ss (loaded earlier).
(for-each
  (lambda (pair) (def-var! "clojure.core" (car pair) (jolt-class-for (cdr pair))))
  class-token-alist)
(for-each
  (lambda (nm) (def-var! "clojure.core" nm (jolt-class-for nm)))
  class-fqn-list)
;; ...and the 96 auto-imports get theirs pinned to the canonical name, last, so
;; the mapping every namespace has cannot be decided by which class the graph
;; happened to enumerate first: class-token-alist is first-simple-name-wins over
;; an unordered hashtable walk, so a same-named class anywhere else in the graph
;; could otherwise take `Process` or `Package` from java.lang.
(for-each
  (lambda (n)
    (let ((fqn (jolt-default-import-canonical n)))
      (when (jch-known? fqn) (def-var! "clojure.core" n (jolt-class-for fqn)))))
  jolt-default-import-names)

;; --- resolve/ns-resolve return the Class for a class mapping ------------------
;; A JVM ns mapping is a var or a Class, and resolve hands back whichever the
;; symbol names. jolt's base resolve (ns.ss) is var-only, so it re-registers here
;; with two class-aware layers on top:
;;   - a resolved var that IS the class model's registration for its own name —
;;     a class-token var (String, java.lang.Long) or a deftype/defrecord name var
;;     holding its ctor — resolves through to the class value. A user var that
;;     merely holds a class ((def MyCls String)) keeps resolving to the var,
;;     which is what the JVM's def would produce. The name must match the class
;;     (FQN or simple), so ->Rec ctor vars stay vars.
;;   - an unresolved symbol classifies as a class the same way, through the same
;;     jolt-class-for interner, so (= (resolve 'X) X) holds for every class form
;;     the asking namespace maps: dotted names (java.util.Map) and the java.lang
;;     auto-imports.
;;
;; Both layers answer the JVM's question, which is per-NAMESPACE: "does this ns
;; map this name to a class", not "does this class exist". The distinction is not
;; academic — jolt keeps a class-token var in clojure.core for EVERY class it
;; models (above) so a bare `Pattern` self-evaluates, and reading those as
;; mappings made (resolve 'Path) answer in a namespace that never imported
;; java.nio.file.Path. typedclojure's (u/def-object Path ...) guards on exactly
;; that resolve before emitting its deftype, so the guard fired, the deftype was
;; skipped, and the Path-maker it also emits never existed (jolt-17w2).
(define (rsv-registration-class-name c)
  (let ((root (var-cell-root c)))
    (cond ((jclass? root) (jclass-name root))
          ;; a deftype/defrecord NAME var holds its ctor, which carries the tag
          ((procedure? root) (deftype-ctor-tag root))
          (else #f))))
(define (rsv-registration-class c)
  (let ((cn (rsv-registration-class-name c)))
    (and cn
         (let ((nm (var-cell-name c)))
           (and (or (string=? nm cn) (string=? nm (hsc-last-segment cn)))
                (var-cell-root c))))))
;; Is that registration a mapping the asking namespace HAS? jolt holds a ns's own
;; class mappings as cells in that ns — (:import ...) binds the short name there
;; (natives-str.ss), deftype/defrecord binds its own name there (protocols.ss) —
;; so a cell found in the asking ns is one by construction. Beyond that only the
;; two kinds the JVM makes visible everywhere answer: a fully-qualified name, and
;; the java.lang auto-imports. The clojure.core class tokens for everything else
;; are the class MODEL and stay invisible to resolve.
(define (rsv-mapping-visible? c ns)
  (let ((nm (var-cell-name c)))
    (or (string=? (var-cell-ns c) ns)
        (hc-fq-class-name? nm)
        (and (jolt-default-import-fqn nm) #t))))
(define (rsv-class-for-name nm)
  ;; Only a class the host actually MODELS answers, and the syntactic dotted-name
  ;; shape is deliberately not enough: resolve is how tooling feature-detects a
  ;; class (compliment gates its JDK9 module scanner on resolving
  ;; java.lang.module.ModuleFinder), and a token for a class jolt cannot back
  ;; sends such code down paths that then die on the missing pieces — the
  ;; instance? macro reads a class answer here as "the symbol evaluates to that
  ;; class" and emits it unquoted, so answering for an unbacked java.lang name
  ;; (ExceptionInInitializerError, which nothing registers) turns fipp's
  ;; (catch ExceptionInInitializerError _) into an unresolved symbol.
  ;; A symbol in CODE keeps the wider syntactic model (hc-resolve-global).
  ;; A bare name needs no arm here: every one a namespace maps and jolt models is
  ;; a cell — an :import, a deftype, or the clojure.core token the class model
  ;; registers for each auto-import it knows — so jolt-resolve already found it.
  ;; jch-known-exact?, not jch-known?: the latter falls back to the last dotted
  ;; segment, so fake.pkg.String answered java.lang.String's registration and
  ;; resolve handed back a token for a class that exists nowhere — the opposite
  ;; of the feature-detection answer this is here to give.
  (and (hc-fq-class-name? nm)
       (or (jch-known-exact? nm) (host-class-registered? nm))
       (jolt-class-for nm)))
(define (rsv-through v sym ns)
  (cond ((jolt-nil? v)
         (or (and (symbol-t? sym) (not (string? (symbol-t-ns sym)))
                  (rsv-class-for-name (symbol-t-name sym)))
             jolt-nil))
        ;; a class registration the ns does not map is NOT the var either — the
        ;; JVM has no such var to hand back, so the answer is nil.
        ((rsv-registration-class v)
         => (lambda (root) (if (rsv-mapping-visible? v ns) root jolt-nil)))
        (else v)))
(def-var! "clojure.core" "resolve"
  (case-lambda
    ((sym) (rsv-through (jolt-resolve sym) sym (chez-current-ns)))
    ;; the &env arity: a local named sym answers nil, never a class
    ((env sym) (if (and (pmap? env) (pmap-contains? env sym))
                   jolt-nil
                   (rsv-through (jolt-resolve env sym) sym (chez-current-ns))))))
(def-var! "clojure.core" "ns-resolve"
  (lambda (ns-desig sym)
    (rsv-through (jolt-ns-resolve ns-desig sym) sym
                 (jns-name (jolt-the-ns ns-desig)))))

;; --- ns-imports reports the namespace's own class mappings --------------------
;; A JVM namespace maps class names as well as vars: the java.lang auto-imports
;; everywhere, an (:import ...) into the ns that asked for it, a deftype/defrecord
;; into the ns that defines it. ns.ss returns the auto-imports alone because the
;; class model does not exist yet where it is defined; this reads the other two
;; back off the namespace itself rather than keeping a second table that could
;; drift out of step with the cells resolve answers from.
(define hsc-default-imports
  (pmap-fold jolt-default-imports
             (lambda (k v acc) (jolt-assoc acc k (jolt-class-for v)))
             (jolt-hash-map)))
(define (hsc-ns-import-class c)
  ;; the cell is the ns's mapping FOR a class only when it is bound under that
  ;; class's own simple name, so Foo counts and ->Foo / map->Foo do not.
  (and (var-cell-defined? c)
       (let ((cn (rsv-registration-class-name c)))
         (and cn (string=? (var-cell-name c) (hsc-last-segment cn))
              (jolt-class-for cn)))))
(define (hsc-ns-imports . desig)
  (let ((cns (if (pair? desig) (ns-desig->name (car desig)) (chez-current-ns))))
    (fold-left (lambda (m c)
                 (let ((cls (hsc-ns-import-class c)))
                   (if cls (jolt-assoc m (jolt-symbol #f (var-cell-name c)) cls) m)))
               hsc-default-imports
               (ns-cells-list cns))))
(set! jolt-ns-imports hsc-ns-imports)   ; so ns-map (ns.ss) sees them too
(def-var! "clojure.core" "ns-imports" hsc-ns-imports)
;; ...and the other half of that: a cell that IS a class mapping is an import, so
;; ns-interns / ns-publics / the refers built from clojure.core's publics must not
;; report it as a var. Without this the class tokens above are public vars of
;; clojure.core, every namespace refers them, and the refer shadows the very
;; ns-imports entry this section just built ((ns-map 'user) answered the token var
;; for String where the JVM answers the class).
(set! ns-cell-class-mapping? (lambda (c) (and (hsc-ns-import-class c) #t)))
