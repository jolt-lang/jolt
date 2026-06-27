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
(define al-min-cap 8)
(define (al-vec self) (vector-ref (jhost-state self) 0))
(define (al-cnt self) (vector-ref (jhost-state self) 1))
(define (al-cnt! self n) (vector-set! (jhost-state self) 1 n))
(define (make-arraylist xs)               ; xs: a Scheme list of initial elements
  (let* ((n (length xs)) (cap (fxmax al-min-cap n)) (v (make-vector cap jolt-nil)))
    (let loop ((i 0) (xs xs)) (when (pair? xs) (vector-set! v i (car xs)) (loop (fx+ i 1) (cdr xs))))
    (make-jhost "arraylist" (vector v n))))
(define (al-ensure! self need)            ; grow the backing vector (doubling) to fit `need`
  (let ((v (al-vec self)))
    (when (fx>? need (vector-length v))
      (let grow ((cap (fxmax al-min-cap (vector-length v))))
        (if (fx<? cap need) (grow (fx* cap 2))
            (let ((nv (make-vector cap jolt-nil)))
              (let cp ((i 0)) (when (fx<? i (al-cnt self)) (vector-set! nv i (vector-ref v i)) (cp (fx+ i 1))))
              (vector-set! (jhost-state self) 0 nv)))))))
(define (al-push! self x)
  (let ((n (al-cnt self))) (al-ensure! self (fx+ n 1)) (vector-set! (al-vec self) n x) (al-cnt! self (fx+ n 1))))
(define (al-insert-at! self i x)
  (let ((n (al-cnt self)))
    (al-ensure! self (fx+ n 1))
    (let ((v (al-vec self)))
      (let shift ((j n)) (when (fx>? j i) (vector-set! v j (vector-ref v (fx- j 1))) (shift (fx- j 1))))
      (vector-set! v i x) (al-cnt! self (fx+ n 1)))))
(define (al-remove-at! self i)
  (let ((n (al-cnt self)) (v (al-vec self)))
    (let shift ((j i)) (when (fx<? j (fx- n 1)) (vector-set! v j (vector-ref v (fx+ j 1))) (shift (fx+ j 1))))
    (vector-set! v (fx- n 1) jolt-nil) (al-cnt! self (fx- n 1))))
(define (al->list self)                   ; first `count` elements as a Scheme list
  (let ((v (al-vec self)))
    (let loop ((i (fx- (al-cnt self) 1)) (acc '())) (if (fx<? i 0) acc (loop (fx- i 1) (cons (vector-ref v i) acc))))))
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
    (cons "get" (lambda (self i) (vector-ref (al-vec self) (jnum->exact i))))
    (cons "set" (lambda (self i x)
                  (let* ((idx (jnum->exact i)) (old (vector-ref (al-vec self) idx)))
                    (vector-set! (al-vec self) idx x) old)))
    (cons "size" (lambda (self) (->num (al-cnt self))))
    (cons "isEmpty" (lambda (self) (fx=? 0 (al-cnt self))))
    (cons "remove" (lambda (self i)
                     (let* ((idx (jnum->exact i)) (old (vector-ref (al-vec self) idx)))
                       (al-remove-at! self idx) old)))
    (cons "clear" (lambda (self) (vector-set! (jhost-state self) 0 (make-vector al-min-cap jolt-nil)) (al-cnt! self 0) jolt-nil))
    (cons "contains" (lambda (self x) (and (memp (lambda (e) (jolt=2 e x)) (al->list self)) #t)))
    (cons "toArray" (lambda (self . _) (apply jolt-vector (al->list self))))
    (cons "iterator" (lambda (self) (make-jiterator (list->cseq (al->list self)))))
    (cons "toString" (lambda (self) (jolt-pr-str (list->cseq (al->list self)))))))
(register-host-methods! "arraylist" arraylist-methods)

;; java.util.LinkedList: the ArrayList backing plus the Deque surface
;; (addFirst/addLast/removeFirst/removeLast/getFirst/getLast/peek/push/pop).
;; tools.reader holds pending splice forms in one and (seq)s / .remove(0)s it.
(define (al-first self) (vector-ref (al-vec self) 0))
(define (al-last self) (vector-ref (al-vec self) (fx- (al-cnt self) 1)))
(define linkedlist-methods
  (append arraylist-methods
    (list
      (cons "addFirst" (lambda (self x) (al-insert-at! self 0 x) jolt-nil))
      (cons "addLast" (lambda (self x) (al-push! self x) jolt-nil))
      (cons "offer" (lambda (self x) (al-push! self x) #t))
      (cons "removeFirst" (lambda (self) (let ((o (al-first self))) (al-remove-at! self 0) o)))
      (cons "removeLast" (lambda (self) (let ((o (al-last self))) (al-remove-at! self (fx- (al-cnt self) 1)) o)))
      (cons "getFirst" al-first) (cons "getLast" al-last)
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

;; ArrayList / LinkedList are Iterable: (seq al) walks the elements (nil if empty),
;; so (seq pending-forms) and reduce/into over one work like the JVM.
(define %al-seq jolt-seq)
(set! jolt-seq
  (lambda (x)
    (if (and (jhost? x) (or (string=? (jhost-tag x) "arraylist") (string=? (jhost-tag x) "linkedlist")))
        (list->cseq (al->list x))
        (%al-seq x))))

;; Appendable.append text: append(x) renders x; append(csq,start,end) appends the
;; subsequence csq[start,end) (data.json's writer appends string runs this way).
(define (append-text x rest)
  (if (null? rest)
      (render-piece x)
      (substring (render-piece x) (jnum->exact (car rest)) (jnum->exact (cadr rest)))))

(register-class-ctor! "StringBuilder"
  (lambda args (make-jhost "string-builder"
    ;; a numeric first arg is a CAPACITY hint, not content.
    (vector (if (and (pair? args) (not (number? (car args)))) (render-piece (car args)) "")))))
(register-host-methods! "string-builder"
  (list (cons "append" (lambda (self x . rest) (sb-set! self (string-append (sb-str self) (append-text x rest))) self))
        (cons "toString" (lambda (self) (sb-str self)))
        (cons "length" (lambda (self) (->num (string-length (sb-str self)))))
        (cons "charAt" (lambda (self i) (string-ref (sb-str self) (jnum->exact i))))
        (cons "setLength" (lambda (self n)
                            (let ((cur (sb-str self)) (n (jnum->exact n)))
                              (sb-set! self (if (< n (string-length cur))
                                                (substring cur 0 n)
                                                (string-append cur (make-string (- n (string-length cur)) #\nul)))))
                            jolt-nil))))
;; (str sb) / print a StringBuilder -> its accumulated content, like the JVM
;; (str calls toString). Without this str renders the opaque host object.
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "string-builder"))) sb-str)

;; ---- StringWriter -----------------------------------------------------------
;; Writer.write(int) writes the CHAR for that code; append(char) appends the char.
(define (writer-piece x) (if (number? x) (string (integer->char (jnum->exact x))) (render-piece x)))
(register-class-ctor! "StringWriter" (lambda args (make-jhost "writer" (vector ""))))
(register-host-methods! "writer"
  (list (cons "write" (lambda (self x) (sb-set! self (string-append (sb-str self) (writer-piece x))) jolt-nil))
        (cons "append" (lambda (self x . rest) (sb-set! self (string-append (sb-str self) (append-text x rest))) self))
        (cons "flush" (lambda (self) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) (sb-str self)))))

;; a file-backed writer (clojure.java.io/writer of a File/path): accumulates like
;; StringWriter, then persists to the path on flush/close, so
;; (with-open [w (io/writer "f")] (.write w …)) writes the file. State #(path buf).
(define (fw-path self) (vector-ref (jhost-state self) 0))
(define (fw-buf self) (vector-ref (jhost-state self) 1))
(define (fw-append! self s) (vector-set! (jhost-state self) 1 (string-append (fw-buf self) s)))
(define (fw-flush! self) (jolt-spit (fw-path self) (fw-buf self)))  ; jolt-spit: io.ss
(register-host-methods! "file-writer"
  (list (cons "write" (lambda (self x) (fw-append! self (writer-piece x)) jolt-nil))
        (cons "append" (lambda (self x . rest) (fw-append! self (append-text x rest)) self))
        (cons "flush" (lambda (self) (fw-flush! self) jolt-nil))
        (cons "close" (lambda (self) (fw-flush! self) jolt-nil))
        (cons "toString" (lambda (self) (fw-buf self)))))

;; a writer over a real Chez port — the values *out* / *err* hold. write/append
;; push to the port (so (.write *out* s) and (binding [*out* *err*] …) work);
;; it isn't a buffer, so toString is empty. Lets libraries that touch *out*/*err*
;; (tools.logging, selmer) compile and run.
(register-host-methods! "port-writer"
  (list (cons "write" (lambda (self x) (display (writer-piece x) (vector-ref (jhost-state self) 0)) jolt-nil))
        (cons "append" (lambda (self x . rest) (display (append-text x rest) (vector-ref (jhost-state self) 0)) self))
        (cons "flush" (lambda (self) (flush-output-port (vector-ref (jhost-state self) 0)) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) ""))))
(def-var! "clojure.core" "*out*" (make-jhost "port-writer" (vector (current-output-port))))
(def-var! "clojure.core" "*err*" (make-jhost "port-writer" (vector (current-error-port))))

;; PrintWriter — a thin wrapper over a target writer. write/append/print forward
;; the rendered text to the target. clojure.data.json's pretty printer builds
;; (PrintWriter. *out*) where *out* is bound to clojure.pprint's pretty-writer (a
;; jolt record), so forwarding routes column-aware through clojure.pprint/-write;
;; for a host writer target it falls back to that writer's own write.
(define (pw-forward target s)
  (cond
    ((and (jhost? target) (string=? (jhost-tag target) "port-writer"))
     (display s (vector-ref (jhost-state target) 0)))
    ((and (jhost? target) (memv #t (list (string=? (jhost-tag target) "writer")
                                         (string=? (jhost-tag target) "string-builder"))))
     (sb-set! target (string-append (sb-str target) s)))
    (else
     (jolt-invoke (var-deref "clojure.pprint" "-write") target s))))
(register-class-ctor! "PrintWriter"
  (lambda args (make-jhost "print-writer" (vector (if (pair? args) (car args) jolt-nil)))))
(register-class-ctor! "java.io.PrintWriter"
  (lambda args (make-jhost "print-writer" (vector (if (pair? args) (car args) jolt-nil)))))
(register-host-methods! "print-writer"
  (list (cons "write" (lambda (self x . rest) (pw-forward (vector-ref (jhost-state self) 0) (append-text x rest)) jolt-nil))
        (cons "print" (lambda (self x) (pw-forward (vector-ref (jhost-state self) 0) (render-piece x)) jolt-nil))
        (cons "append" (lambda (self x . rest) (pw-forward (vector-ref (jhost-state self) 0) (append-text x rest)) self))
        (cons "flush" (lambda (self) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) ""))))

;; ---- java.util.HashMap ------------------------------------------------------
;; A mutable map keyed by jolt values (jolt-hash / jolt=2). State #(chez-hashtable).
;; Constructors: () | (capacity) | (capacity load-factor) [sizing args ignored] |
;; (Map m) [copy]. Enough of the Map surface for libraries that build a fast lookup
;; (malli's fast-registry: (doto (HashMap. 1024 0.25) (.putAll m)) then .get).
(define (hm-hash k) (let ((h (jolt-hash k)))
                      (bitwise-and (if (and (integer? h) (exact? h)) (abs h) 0) #x3FFFFFFF)))
(define (hm-tbl self) (vector-ref (jhost-state self) 0))
(define (hm-hashmap? x) (and (jhost? x) (string=? (jhost-tag x) "hashmap")))
(define (hm-copy-into! ht src)            ; src: a jolt map or another hashmap
  (if (hm-hashmap? src)
      (vector-for-each (lambda (k) (hashtable-set! ht k (hashtable-ref (hm-tbl src) k jolt-nil)))
                       (hashtable-keys (hm-tbl src)))
      (for-each (lambda (e) (hashtable-set! ht (jolt-nth e 0) (jolt-nth e 1)))
                (seq->list (jolt-seq src)))))
(register-class-ctor! "HashMap"
  (lambda args
    (let ((ht (make-hashtable hm-hash jolt=2)))
      (when (and (pair? args) (or (pmap? (car args)) (hm-hashmap? (car args))))
        (hm-copy-into! ht (car args)))
      (make-jhost "hashmap" (vector ht)))))
(define (hm->pmap self)
  (let ((m (jolt-hash-map)))
    (vector-for-each (lambda (k) (set! m (jolt-assoc m k (hashtable-ref (hm-tbl self) k jolt-nil))))
                     (hashtable-keys (hm-tbl self)))
    m))
(register-host-methods! "hashmap"
  (list (cons "put" (lambda (self k v) (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                          (hashtable-set! (hm-tbl self) k v) old)))
        (cons "get" (lambda (self k) (hashtable-ref (hm-tbl self) k jolt-nil)))
        (cons "getOrDefault" (lambda (self k d) (hashtable-ref (hm-tbl self) k d)))
        (cons "containsKey" (lambda (self k) (if (hashtable-contains? (hm-tbl self) k) #t #f)))
        (cons "containsValue" (lambda (self v)
          (let ((found #f))
            (vector-for-each (lambda (k) (when (jolt=2 v (hashtable-ref (hm-tbl self) k jolt-nil)) (set! found #t)))
                             (hashtable-keys (hm-tbl self))) found)))
        (cons "size" (lambda (self) (hashtable-size (hm-tbl self))))
        (cons "isEmpty" (lambda (self) (= 0 (hashtable-size (hm-tbl self)))))
        (cons "remove" (lambda (self k) (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                           (hashtable-delete! (hm-tbl self) k) old)))
        (cons "clear" (lambda (self) (hashtable-clear! (hm-tbl self)) jolt-nil))
        (cons "putAll" (lambda (self m) (hm-copy-into! (hm-tbl self) m) jolt-nil))
        (cons "keySet" (lambda (self) (apply jolt-hash-set (vector->list (hashtable-keys (hm-tbl self))))))
        (cons "values" (lambda (self) (apply jolt-vector
                          (map (lambda (k) (hashtable-ref (hm-tbl self) k jolt-nil))
                               (vector->list (hashtable-keys (hm-tbl self)))))))
        (cons "entrySet" (lambda (self) (jolt-seq (hm->pmap self))))
        (cons "toString" (lambda (self) (jolt-pr-str (hm->pmap self))))))
;; java.util.concurrent.ConcurrentHashMap — one shared heap, so the mutable
;; HashMap shim serves. (get a-hashmap k) reads the map (clojure.core/get).
(define (make-hashmap-jhost . args)
  (let ((ht (make-hashtable hm-hash jolt=2)))
    (when (and (pair? args) (or (pmap? (car args)) (hm-hashmap? (car args)))) (hm-copy-into! ht (car args)))
    (make-jhost "hashmap" (vector ht))))
(register-class-ctor! "ConcurrentHashMap" make-hashmap-jhost)
(register-class-ctor! "java.util.concurrent.ConcurrentHashMap" make-hashmap-jhost)
(register-get-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "hashmap")))
                   (lambda (coll k d) (hashtable-ref (hm-tbl coll) k d)))
;; count / contains? over the mutable map shim (clojure.core/count + contains?,
;; which core.cache's SoftCache uses on its backing ConcurrentHashMap).
(define (jhost-hashmap? x) (and (jhost? x) (string=? (jhost-tag x) "hashmap")))
(let ((prev-count jolt-count) (prev-contains jolt-contains?))
  (set! jolt-count (lambda (c) (if (jhost-hashmap? c) (hashtable-size (hm-tbl c)) (prev-count c))))
  (set! jolt-contains? (lambda (c k) (if (jhost-hashmap? c)
                                         (if (hashtable-contains? (hm-tbl c) k) #t #f)
                                         (prev-contains c k)))))

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
;; ref-queue state: #(guardian pending-list); reference state: #(weak-pair queue enqueued?).
(define (rq-guardian-of q) (vector-ref (jhost-state q) 0))
(define (rq-add! q ref)
  (let ((st (jhost-state q))) (vector-set! st 1 (append (vector-ref st 1) (list ref)))))
(define (rq-pump! q)                                  ; drain GC-reclaimed refs onto the list
  (let loop ()
    (let ((rep ((rq-guardian-of q)))) (when rep (rq-add! q rep) (loop)))))
(define (rq-poll q)
  (rq-pump! q)
  (let* ((st (jhost-state q)) (l (vector-ref st 1)))
    (if (null? l) jolt-nil (begin (vector-set! st 1 (cdr l)) (car l)))))
(define (a-ref-queue? x) (and (jhost? x) (string=? (jhost-tag x) "ref-queue")))
(define (make-reference v rest)
  (let* ((rq (if (pair? rest) (car rest) jolt-nil))
         (ref (make-jhost "weak-ref" (vector (weak-cons v #f) rq #f))))
    (when (a-ref-queue? rq) ((rq-guardian-of rq) v ref))   ; fire on the referent's collection
    ref))
(for-each (lambda (nm) (register-class-ctor! nm (lambda (v . rest) (make-reference v rest))))
          '("SoftReference" "java.lang.ref.SoftReference" "WeakReference" "java.lang.ref.WeakReference"))
(register-host-methods! "weak-ref"
  (list (cons "get" (lambda (self) (let ((r (car (vector-ref (jhost-state self) 0))))
                                     (if (bwp-object? r) jolt-nil r))))
        (cons "clear" (lambda (self) (set-car! (vector-ref (jhost-state self) 0) jolt-nil) jolt-nil))
        (cons "isEnqueued" (lambda (self) (vector-ref (jhost-state self) 2)))
        (cons "enqueue" (lambda (self)
          (let* ((st (jhost-state self)) (rq (vector-ref st 1)))
            (if (vector-ref st 2) #f
                (begin (vector-set! st 2 #t) (when (a-ref-queue? rq) (rq-add! rq self)) #t)))))))
(for-each (lambda (nm) (register-class-ctor! nm (lambda _ (make-jhost "ref-queue" (vector (make-guardian) '())))))
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
;; state: a vector #(wrapped-reader pushed-list)
(register-class-ctor! "PushbackReader"
  (lambda (rdr . _) (make-jhost "pushback-reader" (vector rdr '()))))
;; LineNumberingPushbackReader: a pushback-reader (jolt doesn't track line
;; numbers; getLineNumber is a stub for error-reporting paths that read it).
(register-class-ctor! "LineNumberingPushbackReader"
  (lambda (rdr . _) (make-jhost "pushback-reader" (vector rdr '()))))
(register-class-ctor! "clojure.lang.LineNumberingPushbackReader"
  (lambda (rdr . _) (make-jhost "pushback-reader" (vector rdr '()))))
(define (read-unit r)        ; read one code unit (flonum) from any reader, -1 at EOF
  (record-method-dispatch r "read" jolt-nil))
(register-host-methods! "pushback-reader"
  (list (cons "read"
          (lambda (self . rest)
            (define (read1)
              (let ((pushed (vector-ref (jhost-state self) 1)))
                (if (pair? pushed)
                    (begin (vector-set! (jhost-state self) 1 (cdr pushed)) (car pushed))
                    (read-unit (vector-ref (jhost-state self) 0)))))
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
        (cons "getLineNumber" (lambda (self) 0))))

;; ---- StringTokenizer --------------------------------------------------------
;; state: a vector #(tokens-list pos)
(define (tokenize s delims)
  (let ((dset (string->list delims)))
    (let loop ((chars (string->list s)) (cur '()) (toks '()))
      (cond ((null? chars) (reverse (if (null? cur) toks (cons (list->string (reverse cur)) toks))))
            ((memv (car chars) dset)
             (loop (cdr chars) '() (if (null? cur) toks (cons (list->string (reverse cur)) toks))))
            (else (loop (cdr chars) (cons (car chars) cur) toks))))))
(register-class-ctor! "StringTokenizer"
  (lambda (s . delims) (make-jhost "string-tokenizer"
    (vector (tokenize (if (string? s) s (jolt-str-render-one s))
                      (if (null? delims) " \t\n\r\f" (car delims))) 0))))
(register-host-methods! "string-tokenizer"
  (list (cons "hasMoreTokens" (lambda (self) (< (vector-ref (jhost-state self) 1) (length (vector-ref (jhost-state self) 0)))))
        (cons "countTokens" (lambda (self) (->num (- (length (vector-ref (jhost-state self) 0)) (vector-ref (jhost-state self) 1)))))
        (cons "nextToken" (lambda (self)
                            (let ((toks (vector-ref (jhost-state self) 0)) (p (vector-ref (jhost-state self) 1)))
                              (if (< p (length toks))
                                  (begin (vector-set! (jhost-state self) 1 (+ p 1)) (list-ref toks p))
                                  (error #f "NoSuchElementException")))))))

;; ---- String / BigInteger / MapEntry constructors ----------------------------
;; (String. bytes [charset]) decodes bytes (a bytevector OR a jolt byte-array)
;; with the named charset (UTF-8 default; ISO-8859-1/latin1/ascii = one byte per
;; char); else stringify. clj-http-lite's body coercion is (String. ^[B body cs).
(define (string-charset-name rest)
  (if (pair? rest)
      (let ((c (car rest)))
        (cond ((string? c) c)
              ((and (jhost? c) (string=? (jhost-tag c) "charset"))
               (let ((p (assq 'name (jhost-state c)))) (if p (jolt-str-render-one (cdr p)) "UTF-8")))
              (else "UTF-8")))
      "UTF-8"))
(define (decode-bytevector bv rest)
  (let ((cs (ascii-string-down (string-charset-name rest))))
    (cond
      ((or (string=? cs "utf-8") (string=? cs "utf8")) (utf8->string bv))
      ((or (string=? cs "iso-8859-1") (string=? cs "latin1") (string=? cs "iso8859-1")
           (string=? cs "us-ascii") (string=? cs "ascii"))
       (list->string (map integer->char (bytevector->u8-list bv))))
      ((or (string=? cs "utf-16") (string=? cs "utf16") (string=? cs "utf-16be") (string=? cs "unicode"))
       (utf16->string bv (endianness big)))   ; respects a leading BOM
      ((string=? cs "utf-16le") (utf16->string bv (endianness little)))
      ((or (string=? cs "utf-32") (string=? cs "utf32") (string=? cs "utf-32be"))
       (utf32->string bv (endianness big)))
      ((string=? cs "utf-32le") (utf32->string bv (endianness little)))
      (else (guard (e (#t (list->string (map integer->char (bytevector->u8-list bv))))) (utf8->string bv))))))
(register-class-ctor! "String"
  (lambda (x . rest)
    (cond ((bytevector? x) (decode-bytevector x rest))
          ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (decode-bytevector (na-bytearray->bv x) rest))
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
(register-class-ctor! "BigInteger"
  (lambda (v . r) (parse-int-or-throw v (if (null? r) 10 (jnum->exact (car r))) "BigInteger")))
(register-class-ctor! "java.math.BigInteger"
  (lambda (v . r) (parse-int-or-throw v (if (null? r) 10 (jnum->exact (car r))) "BigInteger")))
(register-class-ctor! "MapEntry" (lambda (k v) (make-map-entry k v)))
;; JVM exception ctors -> a typed host throwable carrying the canonical :jolt/class
;; (so class / instance? / getMessage / ex-message reflect the real type) and the
;; message. Supports (E. msg), (E. msg cause), (E. cause), and (E.).
(for-each
  (lambda (nm)
    (let ((canonical (or (resolve-class-hint nm) nm)))
      (register-class-ctor! nm
        (lambda args
          (let* ((a0 (if (pair? args) (car args) jolt-nil))
                 (rest (if (pair? args) (cdr args) '()))
                 (cause (if (pair? rest) (car rest) jolt-nil)))
            (cond
              ((string? a0) (jolt-host-throwable canonical a0 cause))
              ((jolt-nil? a0) (jolt-host-throwable canonical jolt-nil))
              ;; (E. cause): a lone throwable arg is the cause, message nil.
              ((and (null? rest) (ex-info-map? a0)) (jolt-host-throwable canonical jolt-nil a0))
              (else (jolt-host-throwable canonical (jolt-str-render-one a0) cause))))))))
  '("Throwable" "Exception" "RuntimeException" "IllegalArgumentException" "IllegalStateException"
    "InterruptedException" "UnsupportedOperationException" "IOException" "NumberFormatException"
    "ArithmeticException" "NullPointerException" "ClassCastException" "IndexOutOfBoundsException"
    "FileNotFoundException" "UnsupportedEncodingException" "EOFException" "java.io.EOFException"
    "Error" "AssertionError"))

;; ---- URLEncoder / URLDecoder (www-form-urlencoded) --------------------------
(define (url-unreserved? b)
  (or (and (>= b 48) (<= b 57)) (and (>= b 65) (<= b 90)) (and (>= b 97) (<= b 122))
      (= b 46) (= b 42) (= b 95) (= b 45)))
(define hex-digits "0123456789ABCDEF")
(define (url-encode s . _)
  (let ((bs (string->utf8 (if (string? s) s (jolt-str-render-one s)))) (out '()))
    (let loop ((i 0))
      (if (= i (bytevector-length bs)) (list->string (reverse out))
          (let ((b (bytevector-u8-ref bs i)))
            (cond ((url-unreserved? b) (set! out (cons (integer->char b) out)))
                  ((= b 32) (set! out (cons #\+ out)))
                  (else (set! out (cons (string-ref hex-digits (bitwise-and b 15))
                                   (cons (string-ref hex-digits (bitwise-arithmetic-shift-right b 4))
                                     (cons #\% out))))))
            (loop (+ i 1)))))))
(define (hexv c)
  (cond ((and (char<=? #\0 c) (char<=? c #\9)) (- (char->integer c) 48))
        ((and (char<=? #\A c) (char<=? c #\F)) (- (char->integer c) 55))
        ((and (char<=? #\a c) (char<=? c #\f)) (- (char->integer c) 87))
        (else (error #f "URLDecoder: malformed escape"))))
(define (url-decode s . _)
  (let* ((str (if (string? s) s (jolt-str-render-one s))) (n (string-length str)) (out '()))
    (let loop ((i 0))
      (if (>= i n) (utf8->string (u8-list->bytevector (reverse out)))
          (let ((c (string-ref str i)))
            (cond ((char=? c #\+) (set! out (cons 32 out)) (loop (+ i 1)))
                  ((char=? c #\%)
                   (set! out (cons (+ (* 16 (hexv (string-ref str (+ i 1)))) (hexv (string-ref str (+ i 2)))) out))
                   (loop (+ i 3)))
                  (else (set! out (cons (char->integer c) out)) (loop (+ i 1)))))))))
(define (u8-list->bytevector lst)
  (let ((bv (make-bytevector (length lst))))
    (let loop ((l lst) (i 0)) (if (null? l) bv (begin (bytevector-u8-set! bv i (car l)) (loop (cdr l) (+ i 1)))))))
(register-class-statics! "URLEncoder" (list (cons "encode" url-encode)))
(register-class-statics! "URLDecoder" (list (cons "decode" url-decode)))
;; Charset/forName yields the canonical name STRING (not an opaque object) so it
;; threads straight into (.getBytes s cs) / (String. bytes cs), which take a name.
(register-class-statics! "Charset" (list (cons "forName" (lambda (nm) (jolt-str-render-one nm)))))

;; ---- Base64 (RFC 4648) ------------------------------------------------------
(define b64-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define (->bytevector x)
  (cond ((bytevector? x) x)
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (na-bytearray->bv x))
        ((string? x) (string->utf8 x))
        (else (string->utf8 (jolt-str-render-one x)))))
(define (b64-encode x)
  (let* ((bs (->bytevector x)) (n (bytevector-length bs)) (out '()))
    (let loop ((i 0))
      (if (>= i n) (list->string (reverse out))
          (let* ((b0 (bytevector-u8-ref bs i))
                 (b1 (if (< (+ i 1) n) (bytevector-u8-ref bs (+ i 1)) #f))
                 (b2 (if (< (+ i 2) n) (bytevector-u8-ref bs (+ i 2)) #f)))
            (set! out (cons (string-ref b64-alphabet (bitwise-arithmetic-shift-right b0 2)) out))
            (set! out (cons (string-ref b64-alphabet (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b0 3) 4)
                                                                  (bitwise-arithmetic-shift-right (or b1 0) 4))) out))
            (set! out (cons (if b1 (string-ref b64-alphabet (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b1 15) 2)
                                                                         (bitwise-arithmetic-shift-right (or b2 0) 6))) #\=) out))
            (set! out (cons (if b2 (string-ref b64-alphabet (bitwise-and b2 63)) #\=) out))
            (loop (+ i 3)))))))
(define (b64-char-val c)
  (let loop ((i 0)) (cond ((= i 64) (error #f "Base64: illegal character")) ((char=? (string-ref b64-alphabet i) c) i) (else (loop (+ i 1))))))
(define (b64-decode x)
  (let* ((str (let ((s (if (string? x) x (utf8->string (->bytevector x)))))
                (list->string (filter (lambda (c) (not (char=? c #\=))) (string->list s)))))
         (out '()) (acc 0) (bits 0))
    (for-each (lambda (c)
                (set! acc (bitwise-ior (bitwise-arithmetic-shift-left acc 6) (b64-char-val c)))
                (set! bits (+ bits 6))
                (when (>= bits 8)
                  (set! bits (- bits 8))
                  (set! out (cons (bitwise-and (bitwise-arithmetic-shift-right acc bits) 255) out))))
              (string->list str))
    (u8-list->bytevector (reverse out))))
(register-host-methods! "b64-encoder"
  (list (cons "encode" (lambda (self bs) (string->utf8 (b64-encode bs))))
        (cons "encodeToString" (lambda (self bs) (b64-encode bs)))))
(register-host-methods! "b64-decoder"
  (list (cons "decode" (lambda (self s) (b64-decode s)))))
(register-class-statics! "Base64"
  (list (cons "getEncoder" (lambda () (make-jhost "b64-encoder" '())))
        (cons "getDecoder" (lambda () (make-jhost "b64-decoder" '())))))

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
(register-class-statics! "Pattern"
  (list (cons "compile" (lambda (s . flags)
                          (if (and (pair? flags) (= (bitwise-and (jnum->exact (car flags)) 8) 8))
                              (jolt-regex (string-append "(?m)" s))
                              (jolt-regex s))))
        (cons "quote" (lambda (s) (pattern-quote s)))
        (cons "MULTILINE" pattern-multiline)))
;; record-method-dispatch already routes string? -> jolt-string-method. Add a
;; regex-t arm (Pattern .split / .matcher-less surface used by corpus) by wrapping
;; once more — a regex-t isn't a jhost.
(define %hs-rmd2 record-method-dispatch)
(set! record-method-dispatch
  (lambda (obj method-name rest-args)
    (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
      (cond
        ((regex-t? obj)
         (cond ((string=? method-name "split")
                ;; .split returns a String[] — a seq (prints
                ;; (a b c), not a vector). re-split with no limit; drop trailing
                ;; empties (JVM default).
                (let ((parts (re-split (regex-t-irx obj) (car rest) #f)))
                  (list->cseq (str-split-drop-trailing parts))))
               ((string=? method-name "pattern") (regex-t-source obj))
               ((or (string=? method-name "toString")) (regex-t-source obj))
               ;; (.matcher pattern s) -> a Matcher (matcher-t) for stepping matches.
               ((string=? method-name "matcher") (jolt-re-matcher obj (car rest)))
               (else (error #f (string-append "No method " method-name " on Pattern")))))
        ;; java.util.regex.Matcher: .matches (anchored whole-region), .find
        ;; (next match), .group [n], .groupCount.
        ((jolt-matcher? obj)
         (cond ((string=? method-name "matches") (jolt-matcher-matches obj))
               ((string=? method-name "find") (not (jolt-nil? (jolt-re-find obj))))
               ((string=? method-name "group") (apply jolt-matcher-group obj rest))
               ((string=? method-name "groupCount") (jolt-matcher-group-count obj))
               (else (error #f (string-append "No method " method-name " on Matcher")))))
        (else (%hs-rmd2 obj method-name rest-args))))))

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
  (lambda (name proc) (register-class-ctor! name proc) jolt-nil))
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
(define (register-tagged-methods! tag members)
  (let* ((key (tag->method-key tag))
         (h (or (hashtable-ref tagged-methods-tbl key #f)
                (let ((nh (make-hashtable string-hash string=?)))
                  (hashtable-set! tagged-methods-tbl key nh) nh))))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members)))

;; htable arm: dispatch (.method obj a*) through the table's tag method registry;
;; an unregistered method falls through (sorted colls are htables too).
(define %hs-rmd-htable record-method-dispatch)
(set! record-method-dispatch
  (lambda (obj method-name rest-args)
    (let ((tag (and (htable? obj) (hashtable-ref (htable-h obj) "jolt/type" #f))))
      (let* ((mh (and tag (hashtable-ref tagged-methods-tbl (tag->method-key tag) #f)))
             (f  (and mh (hashtable-ref mh method-name #f))))
        (if f
            (apply f obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))
            (%hs-rmd-htable obj method-name rest-args))))))

(def-var! "clojure.core" "__register-class-methods!"
  (lambda (tag members) (register-tagged-methods! tag (jmap->static-alist members)) jolt-nil))

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

;; (instance? clojure.lang.IFoo x) for the core clojure.lang interfaces libraries
;; branch on — jolt's value model satisfies them, so report it. Matched by the
;; interface's last dotted segment, so "clojure.lang.IObj" and "IObj" both hit.
(define (hsc-last-segment s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))
;; values that carry metadata (mirrors jolt-with-meta's set in natives-meta.ss).
(define (hsc-imeta? x)
  (or (pvec? x) (pmap? x) (pset? x) (cseq? x) (empty-list-t? x)
      (jolt-lazyseq? x) (jrec? x) (jreify? x) (procedure? x) (symbol-t? x)))
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((iface (hsc-last-segment (symbol-t-name type-sym))))
      (let ((hit (cond
                   ((or (string=? iface "IObj") (string=? iface "IMeta")) (hsc-imeta? val))
                   ((or (string=? iface "IMapEntry") (string=? iface "MapEntry")) (jolt-map-entry? val))
                   ((string=? iface "IRecord") (jrec? val))
                   ((string=? iface "IPersistentMap") (or (pmap? val) (htable-sorted-map? val)))
                   ((string=? iface "IPersistentVector") (and (pvec? val) (not (jolt-map-entry? val))))
                   ((string=? iface "IPersistentSet") (or (pset? val) (htable-sorted-set? val)))
                   ((string=? iface "ISeq")
                    (or (cseq? val) (empty-list-t? val) (jolt-lazyseq? val)))
                   ;; Seqable is anything (seq x) works on — every persistent
                   ;; collection, not just seqs (a vector IS Seqable, not an ISeq).
                   ((string=? iface "Seqable")
                    (or (cseq? val) (empty-list-t? val) (jolt-lazyseq? val)
                        (pvec? val) (pmap? val) (pset? val)
                        (htable-sorted-map? val) (htable-sorted-set? val)))
                   ((string=? iface "Sequential")
                    (or (pvec? val) (cseq? val) (empty-list-t? val) (jolt-lazyseq? val)))
                   ((string=? iface "IFn")
                    (or (procedure? val) (keyword? val) (symbol-t? val)
                        (pmap? val) (pset? val) (pvec? val)))
                   ;; host-class interfaces libraries branch on (data.json, etc.).
                   ;; Matched by last segment, so java.util.Map and Map both hit.
                   ((string=? iface "Named") (or (keyword? val) (symbol-t? val)))
                   ((string=? iface "CharSequence") (string? val))
                   ((string=? iface "Number") (number? val))
                   ((string=? iface "Map") (or (pmap? val) (htable-sorted-map? val)))
                   ((string=? iface "Set") (or (pset? val) (htable-sorted-set? val)))
                   ;; a Java List is a vector or a seq/list — not a set or map.
                   ((string=? iface "List")
                    (or (and (pvec? val) (not (jolt-map-entry? val)))
                        (cseq? val) (empty-list-t? val) (jolt-lazyseq? val)))
                   ;; a Java Collection is any of those plus a set — but NOT a map.
                   ((string=? iface "Collection")
                    (or (pvec? val) (pset? val) (cseq? val) (empty-list-t? val)
                        (jolt-lazyseq? val) (htable-sorted-set? val)))
                   ((string=? iface "Associative")
                    (or (pmap? val) (htable-sorted-map? val)
                        (and (pvec? val) (not (jolt-map-entry? val)))))
                   ;; ILookup (valAt): maps and vectors; Indexed (nth): vectors;
                   ;; Counted: the counted collections. A deftype that declares one
                   ;; is matched by type-satisfies? in instance-check-base.
                   ((string=? iface "ILookup")
                    (or (pmap? val) (htable-sorted-map? val)
                        (and (pvec? val) (not (jolt-map-entry? val)))))
                   ((string=? iface "Indexed")
                    (and (pvec? val) (not (jolt-map-entry? val))))
                   ((string=? iface "Counted")
                    (or (pmap? val) (pset? val) (pvec? val)
                        (htable-sorted-map? val) (htable-sorted-set? val)))
                   ;; reader jhosts — data.json re-wraps a reader in a new
                   ;; PushbackReader unless (instance? PushbackReader r), so this
                   ;; must hold for repeated reads from one reader to work.
                   ((string=? iface "PushbackReader")
                    (and (jhost? val) (string=? (jhost-tag val) "pushback-reader")))
                   ((string=? iface "StringReader")
                    (and (jhost? val) (string=? (jhost-tag val) "string-reader")))
                   ((or (string=? iface "Reader") (string=? iface "BufferedReader"))
                    (reader-jhost? val))
                   (else 'none))))
        (if (eq? hit 'none) 'pass (if hit #t #f))))))

;; java.lang.Class value: (class x) / (.getClass x) return one. It renders like
;; the JVM — str/.toString -> "class <name>", pr -> "<name>", .getName -> "<name>"
;; — but stays = and hash equal to its name STRING, so (= (class x) String),
;; class-keyed maps/sets, multimethod dispatch on class, and instance? all keep
;; working against the bare class-name tokens.
(define (make-class-obj name) (make-jhost "class" (vector name)))
(define (jclass? x) (and (jhost? x) (string=? (jhost-tag x) "class")))
(define (jclass-name x) (vector-ref (jhost-state x) 0))
(define (class-key x) (cond ((jclass? x) (jclass-name x)) ((string? x) x) (else #f)))
(register-eq-arm! (lambda (a b) (or (jclass? a) (jclass? b)))
                  (lambda (a b) (let ((ka (class-key a)) (kb (class-key b)))
                                  (and ka kb (string=? ka kb) #t))))
(register-hash-arm! jclass? (lambda (x) (jolt-hash (jclass-name x))))
(register-str-render! jclass? (lambda (x) (string-append "class " (jclass-name x))))
(register-pr-arm! jclass? (lambda (x) (jclass-name x)))
(register-host-methods! "class"
  (list (cons "getName" (lambda (self) (jclass-name self)))
        (cons "getCanonicalName" (lambda (self) (jclass-name self)))
        (cons "getSimpleName" (lambda (self) (hsc-last-segment (jclass-name self))))
        (cons "toString" (lambda (self) (string-append "class " (jclass-name self))))
        (cons "isArray" (lambda (self) (let ((n (jclass-name self)))
                                         (and (fx>? (string-length n) 0) (char=? (string-ref n 0) #\[)))))
        ;; Class.isInstance(o) == (instance? class o); core.logic's deftype .equals
        ;; uses (.. this getClass (isInstance o)).
        (cons "isInstance" (lambda (self o) (if (instance-check self o) #t #f)))
        (cons "getClass" (lambda (self) (make-class-obj "java.lang.Class")))))

;; (jolt.host/table? x) — is x a host tagged-table?
(def-var! "jolt.host" "table?" (lambda (x) (if (htable? x) #t #f)))

;; --- java.util.Arrays -------------------------------------------------------
(let ((arrays-statics
       (list
         (cons "equals" (lambda (a b)
                          (cond ((and (jolt-nil? a) (jolt-nil? b)) #t)
                                ((or (jolt-nil? a) (jolt-nil? b)) #f)
                                (else (equal? (jolt-array-vec a) (jolt-array-vec b))))))
         (cons "fill" (lambda (a v) (vector-fill! (jolt-array-vec a) v) jolt-nil))
         (cons "copyOf" (lambda (a n)
                          (let* ((src (jolt-array-vec a)) (len (jnum->exact n))
                                 (out (make-vector len 0)))
                            (do ((i 0 (fx+ i 1))) ((fx=? i (min len (vector-length src))))
                              (vector-set! out i (vector-ref src i)))
                            (make-jolt-array out (jolt-array-kind a)))))
         (cons "copyOfRange" (lambda (a from to)
                               (let* ((src (jolt-array-vec a)) (f (jnum->exact from)) (tt (jnum->exact to))
                                      (len (- tt f)) (out (make-vector len 0)))
                                 (do ((i 0 (fx+ i 1))) ((fx=? i len))
                                   (vector-set! out i (vector-ref src (+ f i))))
                                 (make-jolt-array out (jolt-array-kind a)))))
         (cons "toString" (lambda (a) (jolt-pr-str (apply jolt-vector (vector->list (jolt-array-vec a)))))))))
  (register-class-statics! "Arrays" arrays-statics)
  (register-class-statics! "java.util.Arrays" arrays-statics))

;; --- java.util.Random -------------------------------------------------------
;; A non-cryptographic PRNG over Chez's `random`. A seed argument is accepted but
;; not honored for reproducibility (jolt has no seedable Random state); callers
;; that need determinism use SecureRandom or their own generator.
(for-each
  (lambda (nm) (register-class-ctor! nm (lambda args (make-jhost "random" (vector)))))
  '("Random" "java.util.Random"))
(register-host-methods! "random"
  (list
    (cons "nextBytes" (lambda (self ba)
                        (let ((v (jolt-array-vec ba)))
                          (do ((i 0 (fx+ i 1))) ((fx=? i (vector-length v)))
                            (vector-set! v i (random 256))))
                        jolt-nil))
    (cons "nextInt" (lambda (self . a)
                      (->num (if (pair? a) (random (jnum->exact (car a))) (- (random 4294967296) 2147483648)))))
    (cons "nextLong" (lambda (self) (->num (- (random 18446744073709551616) 9223372036854775808))))
    (cons "nextDouble" (lambda (self) (random 1.0)))
    (cons "nextFloat" (lambda (self) (random 1.0)))
    (cons "nextBoolean" (lambda (self) (fx=? 0 (random 2))))))

;; --- java.util.Optional -----------------------------------------------------
;; Returned by getters across java.time / java.net.http (e.g. HttpRequest.timeout,
;; HttpClient.connectTimeout). Value-equal so (= (Optional/of x) (Optional/of x)).
(define (jt-optional present? value) (make-jhost "optional" (vector present? value)))
(define jt-optional-empty (jt-optional #f jolt-nil))
(define (opt? x) (and (jhost? x) (string=? (jhost-tag x) "optional")))
(define (opt-present? o) (vector-ref (jhost-state o) 0))
(define (opt-value o) (vector-ref (jhost-state o) 1))
(let ((statics (list (cons "of" (lambda (v) (if (jolt-nil? v) (error #f "Optional.of(null)") (jt-optional #t v))))
                     (cons "ofNullable" (lambda (v) (if (jolt-nil? v) jt-optional-empty (jt-optional #t v))))
                     (cons "empty" (lambda _ jt-optional-empty)))))
  (register-class-statics! "Optional" statics)
  (register-class-statics! "java.util.Optional" statics))
(register-host-methods! "optional"
  (list (cons "isPresent" (lambda (o) (opt-present? o)))
        (cons "isEmpty" (lambda (o) (not (opt-present? o))))
        (cons "get" (lambda (o) (if (opt-present? o) (opt-value o) (error #f "Optional.get() on empty Optional"))))
        (cons "orElse" (lambda (o d) (if (opt-present? o) (opt-value o) d)))
        (cons "orElseGet" (lambda (o f) (if (opt-present? o) (opt-value o) (jolt-invoke f))))
        (cons "ifPresent" (lambda (o f) (when (opt-present? o) (jolt-invoke f (opt-value o))) jolt-nil))
        (cons "toString" (lambda (o) (if (opt-present? o)
                                         (string-append "Optional[" (jolt-str-render-one (opt-value o)) "]")
                                         "Optional.empty")))))
(register-eq-arm! (lambda (a b) (or (opt? a) (opt? b)))
                  (lambda (a b) (and (opt? a) (opt? b) (eq? (opt-present? a) (opt-present? b))
                                     (or (not (opt-present? a)) (jolt=2 (opt-value a) (opt-value b))))))

;; --- minimal JVM class/interface ancestry -----------------------------------
;; A handful of libraries reflect over the class hierarchy — e.g. core.memoize
;; validates its first argument with (some #{IFn AFn Runnable Callable}
;; (ancestors (class f))). jolt models a class as its name string and has no
;; reflection, so supers/ancestors return nothing on their own. This table gives
;; the common interfaces the direct supers the JVM reports, and the overlay's
;; supers/ancestors fold it in. Keyed by canonical class name; value = direct
;; supers. Extend as more interfaces are exercised.
(define class-supers-tbl (make-hashtable string-hash string=?))
(define (reg-class-supers! name supers) (hashtable-set! class-supers-tbl name supers))
(reg-class-supers! "clojure.lang.IFn" '("java.lang.Runnable" "java.util.concurrent.Callable"))
(reg-class-supers! "clojure.lang.AFn" '("clojure.lang.IFn" "java.lang.Runnable" "java.util.concurrent.Callable"))
(reg-class-supers! "clojure.lang.AFunction" '("clojure.lang.AFn" "clojure.lang.IFn" "clojure.lang.Fn"
                                              "java.lang.Runnable" "java.util.concurrent.Callable"))
;; common exception hierarchy, so (instance? IOException e) / (catch IOException e)
;; match a more specific throwable a library threw (e.g. http-client's
;; UnknownHostException, caught by clj-http-lite's :ignore-unknown-host?).
(reg-class-supers! "java.lang.Throwable" '("java.lang.Object"))
(reg-class-supers! "java.lang.Exception" '("java.lang.Throwable" "java.lang.Object"))
(reg-class-supers! "java.lang.RuntimeException" '("java.lang.Exception" "java.lang.Throwable" "java.lang.Object"))
(reg-class-supers! "java.io.IOException" '("java.lang.Exception" "java.lang.Throwable" "java.lang.Object"))
(reg-class-supers! "java.io.InterruptedIOException" '("java.io.IOException" "java.lang.Exception" "java.lang.Throwable" "java.lang.Object"))
(reg-class-supers! "java.net.SocketException" '("java.io.IOException" "java.lang.Exception" "java.lang.Throwable" "java.lang.Object"))
(reg-class-supers! "java.net.UnknownHostException" '("java.io.IOException" "java.lang.Exception" "java.lang.Throwable" "java.lang.Object"))
(reg-class-supers! "java.net.ConnectException" '("java.net.SocketException" "java.io.IOException" "java.lang.Exception" "java.lang.Throwable" "java.lang.Object"))
(reg-class-supers! "java.net.SocketTimeoutException" '("java.io.InterruptedIOException" "java.io.IOException" "java.lang.Exception" "java.lang.Throwable" "java.lang.Object"))
;; clojure.lang / java.util ancestry for the builtins (class) reports, so a
;; class-keyed multimethod / (isa? (class x) SomeClass) dispatches like the JVM.
;; (Object is supplied universally by class-isa?, so it need not be listed.)
(reg-class-supers! "clojure.lang.IFn" '("clojure.lang.Fn" "java.lang.Runnable" "java.util.concurrent.Callable"))
;; Keyword and Symbol implement IFn (they are callable: (:k m) / ('s m)), so a
;; (class x)-dispatched multimethod with an IFn method matches them, like the JVM.
(reg-class-supers! "clojure.lang.Keyword" '("clojure.lang.Named" "java.lang.Comparable"
                                            "clojure.lang.IFn" "clojure.lang.Fn"
                                            "java.lang.Runnable" "java.util.concurrent.Callable"))
(reg-class-supers! "clojure.lang.Symbol" '("clojure.lang.Named" "java.lang.Comparable"
                                           "clojure.lang.IFn" "clojure.lang.Fn"
                                           "java.lang.Runnable" "java.util.concurrent.Callable"))
(reg-class-supers! "java.lang.String" '("java.lang.CharSequence" "java.lang.Comparable"))
(reg-class-supers! "clojure.lang.PersistentHashSet" '("clojure.lang.APersistentSet" "clojure.lang.IPersistentSet" "clojure.lang.IPersistentCollection" "java.util.Set" "java.util.Collection" "java.lang.Iterable"))
(reg-class-supers! "clojure.lang.PersistentTreeSet" '("clojure.lang.APersistentSet" "clojure.lang.IPersistentSet" "clojure.lang.IPersistentCollection" "java.util.Set" "java.util.Collection" "java.lang.Iterable"))
(reg-class-supers! "clojure.lang.PersistentVector" '("clojure.lang.APersistentVector" "clojure.lang.IPersistentVector" "clojure.lang.IPersistentCollection" "clojure.lang.Sequential" "clojure.lang.Associative" "java.util.List" "java.util.Collection" "java.lang.Iterable"))
(reg-class-supers! "clojure.lang.PersistentArrayMap" '("clojure.lang.APersistentMap" "clojure.lang.IPersistentMap" "clojure.lang.IPersistentCollection" "clojure.lang.Associative" "java.util.Map" "java.lang.Iterable"))
(reg-class-supers! "clojure.lang.PersistentHashMap" '("clojure.lang.APersistentMap" "clojure.lang.IPersistentMap" "clojure.lang.IPersistentCollection" "clojure.lang.Associative" "java.util.Map" "java.lang.Iterable"))
(reg-class-supers! "clojure.lang.PersistentList" '("clojure.lang.ASeq" "clojure.lang.ISeq" "clojure.lang.IPersistentCollection" "clojure.lang.Sequential" "clojure.lang.Seqable" "java.util.List" "java.util.Collection" "java.lang.Iterable"))
(reg-class-supers! "clojure.lang.LazySeq" '("clojure.lang.ISeq" "clojure.lang.IPersistentCollection" "clojure.lang.Sequential" "clojure.lang.Seqable" "java.lang.Iterable"))
(reg-class-supers! "clojure.lang.Cons" '("clojure.lang.ASeq" "clojure.lang.ISeq" "clojure.lang.Sequential" "clojure.lang.Seqable" "java.lang.Iterable"))

;; transitive closure of direct supers (set semantics via an accumulator list)
(define (class-ancestors-list name)
  (let loop ((pending (hashtable-ref class-supers-tbl name '())) (seen '()))
    (cond ((null? pending) (reverse seen))
          ((member (car pending) seen) (loop (cdr pending) seen))
          (else (loop (append (hashtable-ref class-supers-tbl (car pending) '()) (cdr pending))
                      (cons (car pending) seen))))))

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
          (let loop ((names (cons cls (class-ancestors-list cls))))
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
            (if (let loop ((names (cons cc (class-ancestors-list cc))))
                  (cond ((string=? pp "java.lang.Object") #t)
                        ((null? names) #f)
                        ((or (string=? pp (car names))
                             (string=? pseg (hsc-last-segment (car names)))) #t)
                        (else (loop (cdr names)))))
                #t jolt-nil))
          jolt-nil))))

;; (jolt.host/class-supers name) / (jolt.host/class-ancestors name) — a jolt seq of
;; super / ancestor class-name strings, or nil when jolt models no hierarchy for it.
(def-var! "jolt.host" "class-supers"
  (lambda (x)
    (let ((name (class-key x)))
      (if (and name (hashtable-contains? class-supers-tbl name))
          (list->cseq (hashtable-ref class-supers-tbl name '()))
          jolt-nil))))
(def-var! "jolt.host" "class-ancestors"
  (lambda (x)
    (let ((name (class-key x)))
      (if name
          (let ((as (class-ancestors-list name)))
            (if (null? as) jolt-nil (list->cseq as)))
          jolt-nil))))
