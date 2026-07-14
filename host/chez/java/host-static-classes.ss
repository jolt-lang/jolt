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
  (list (cons "write" (lambda (self x) (fw-append! self (writer-piece x)) jolt-nil))
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
(define (port-writer-port self)
  (let ((p (vector-ref (jhost-state self) 0)))
    (cond ((eq? p 'out) (current-output-port))
          ((eq? p 'err) (current-error-port))
          (else p))))
(register-host-methods! "port-writer"
  (list (cons "write" (lambda (self x) (display (writer-piece x) (port-writer-port self)) jolt-nil))
        (cons "append" (lambda (self x . rest) (display (append-text x rest) (port-writer-port self)) self))
        (cons "flush" (lambda (self) (flush-output-port (port-writer-port self)) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) ""))))
(def-var! "clojure.core" "*out*" (make-jhost "port-writer" (vector 'out)))
(def-var! "clojure.core" "*err*" (make-jhost "port-writer" (vector 'err)))

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
  (list (cons "write" (lambda (self x) (set-box! (pwo-buf self)
                                  (string-append (unbox (pwo-buf self)) (writer-piece x))) jolt-nil))
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
(define (hm-hashmap? x) (and (jhost? x) (string=? (jhost-tag x) "hashmap")))
(define (hm-copy-into! ht src)            ; src: a jolt map or another hashmap
  (if (hm-hashmap? src)
      (vector-for-each (lambda (k) (hashtable-set! ht k (hashtable-ref (hm-tbl src) k jolt-nil)))
                       (hashtable-keys (hm-tbl src)))
      (for-each (lambda (e) (hashtable-set! ht (jolt-nth e 0) (jolt-nth e 1)))
                (seq->list (jolt-seq src)))))
(define (hm-copy-into-ordered! self src)  ; like hm-copy-into!, keeping insertion order
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
(register-host-methods! "hashmap"
  (list (cons "put" (lambda (self k v) (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                          (hm-note-key! self k)
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
(define (make-atomic init)
  (make-jhost "atomic" (vector (box init) (make-mutex))))
(define (atomic-box self) (vector-ref (jhost-state self) 0))
(define (atomic-lock self) (vector-ref (jhost-state self) 1))
(let ((ref-ctor (lambda args (make-atomic (if (pair? args) (car args) jolt-nil))))
      (num-ctor (lambda args (make-atomic (if (pair? args) (car args) 0))))
      (bool-ctor (lambda args (make-atomic (if (pair? args) (car args) #f)))))
  (for-each (lambda (n) (register-class-ctor! n ref-ctor))
            '("AtomicReference" "java.util.concurrent.atomic.AtomicReference"))
  (for-each (lambda (n) (register-class-ctor! n num-ctor))
            '("AtomicInteger" "java.util.concurrent.atomic.AtomicInteger"
              "AtomicLong" "java.util.concurrent.atomic.AtomicLong"))
  (for-each (lambda (n) (register-class-ctor! n bool-ctor))
            '("AtomicBoolean" "java.util.concurrent.atomic.AtomicBoolean")))
(register-host-methods! "atomic"
  (list (cons "get" (lambda (self) (unbox (atomic-box self))))
        (cons "set" (lambda (self v) (set-box! (atomic-box self) v) jolt-nil))
        (cons "getAndSet" (lambda (self v) (with-mutex (atomic-lock self)
                            (let ((o (unbox (atomic-box self)))) (set-box! (atomic-box self) v) o))))
        (cons "compareAndSet" (lambda (self o n) (with-mutex (atomic-lock self)
                                (if (jolt=2 (unbox (atomic-box self)) o)
                                    (begin (set-box! (atomic-box self) n) #t) #f))))
        (cons "updateAndGet" (lambda (self f) (with-mutex (atomic-lock self)
                               (let ((n (jolt-invoke f (unbox (atomic-box self)))))
                                 (set-box! (atomic-box self) n) n))))
        (cons "getAndUpdate" (lambda (self f) (with-mutex (atomic-lock self)
                               (let ((o (unbox (atomic-box self))))
                                 (set-box! (atomic-box self) (jolt-invoke f o)) o))))
        (cons "incrementAndGet" (lambda (self) (with-mutex (atomic-lock self)
                                  (let ((n (+ (unbox (atomic-box self)) 1))) (set-box! (atomic-box self) n) n))))
        (cons "decrementAndGet" (lambda (self) (with-mutex (atomic-lock self)
                                  (let ((n (- (unbox (atomic-box self)) 1))) (set-box! (atomic-box self) n) n))))
        (cons "getAndIncrement" (lambda (self) (with-mutex (atomic-lock self)
                                  (let ((o (unbox (atomic-box self)))) (set-box! (atomic-box self) (+ o 1)) o))))
        (cons "getAndDecrement" (lambda (self) (with-mutex (atomic-lock self)
                                  (let ((o (unbox (atomic-box self)))) (set-box! (atomic-box self) (- o 1)) o))))
        (cons "addAndGet" (lambda (self d) (with-mutex (atomic-lock self)
                            (let ((n (+ (unbox (atomic-box self)) (jnum->exact d)))) (set-box! (atomic-box self) n) n))))
        (cons "getAndAdd" (lambda (self d) (with-mutex (atomic-lock self)
                            (let ((o (unbox (atomic-box self)))) (set-box! (atomic-box self) (+ o (jnum->exact d))) o))))
        (cons "intValue" (lambda (self) (jnum->exact (unbox (atomic-box self)))))
        (cons "longValue" (lambda (self) (jnum->exact (unbox (atomic-box self)))))
        (cons "toString" (lambda (self) (jolt-str-render-one (unbox (atomic-box self)))))))
;; java.util.Collections/synchronizedMap|List|Set wrap a collection for
;; thread-safe access. The shared-heap HashMap/ArrayList shims already serialize
;; individual ops adequately for these uses, so the wrapper returns its argument.
(let ((ident (lambda (c . _) c)))
  (register-class-statics! "Collections"
    (list (cons "synchronizedMap" ident) (cons "synchronizedList" ident)
          (cons "synchronizedSet" ident) (cons "unmodifiableMap" ident)
          (cons "unmodifiableList" ident) (cons "unmodifiableSet" ident)
          (cons "emptyList" (lambda _ (jolt-vector))) (cons "emptyMap" (lambda _ (jolt-hash-map)))))
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
(register-get-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "hashmap")))
                   (lambda (coll k d) (hashtable-ref (hm-tbl coll) k d)))
;; count / contains? over the mutable map shim (clojure.core/count + contains?,
;; which core.cache's SoftCache uses on its backing ConcurrentHashMap).
(define (jhost-hashmap? x) (and (jhost? x) (string=? (jhost-tag x) "hashmap")))
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
;; Fully-qualified aliases so (java.io.PushbackReader. …) / (java.io.StringReader. …)
;; resolve to these built-ins even when a library defines a deftype of the same
;; simple name (tools.reader), which would otherwise take the bare-name slot.
(register-class-ctor! "java.io.PushbackReader" (lookup-class class-ctors-tbl "PushbackReader"))
(register-class-ctor! "java.io.StringReader" (lookup-class class-ctors-tbl "StringReader"))
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
                                  (jolt-throw (jolt-host-throwable "java.util.NoSuchElementException" "no more tokens"))))))
        ;; StringTokenizer implements java.util.Enumeration — enumeration-seq drives
        ;; it through these, so alias them onto the token methods.
        (cons "hasMoreElements" (lambda (self) (< (vector-ref (jhost-state self) 1) (length (vector-ref (jhost-state self) 0)))))
        (cons "nextElement" (lambda (self)
                              (let ((toks (vector-ref (jhost-state self) 0)) (p (vector-ref (jhost-state self) 1)))
                                (if (< p (length toks))
                                    (begin (vector-set! (jhost-state self) 1 (+ p 1)) (list-ref toks p))
                                    (jolt-throw (jolt-host-throwable "java.util.NoSuchElementException" "no more tokens"))))))))

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
(register-class-ctor! "BigInteger"
  (lambda (v . r) (parse-int-or-throw v (if (null? r) 10 (jnum->exact (car r))) "BigInteger")))
(register-class-ctor! "java.math.BigInteger"
  (lambda (v . r) (parse-int-or-throw v (if (null? r) 10 (jnum->exact (car r))) "BigInteger")))
(register-class-ctor! "MapEntry" (lambda (k v) (make-map-entry k v)))
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
(register-method-arm! 42
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
               ;; start/end of the last successful find (whole match, or group n)
               ((string=? method-name "start")
                (let ((mm (matcher-t-last obj)))
                  (if mm (irregex-match-start-index mm (if (pair? rest) (jnum->exact (car rest)) 0))
                      (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "No match available")))))
               ((string=? method-name "end")
                (let ((mm (matcher-t-last obj)))
                  (if mm (irregex-match-end-index mm (if (pair? rest) (jnum->exact (car rest)) 0))
                      (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "No match available")))))
               (else (error #f (string-append "No method " method-name " on Matcher")))))
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
(register-method-arm! 43
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
                   ((string=? iface "IRecord") (jrec? val))
                   ;; IFn — maps/sets/vectors are callable in jolt beyond the JVM
                   ;; class hierarchy, so jch-tags doesn't cover them for these types.
                   ((string=? iface "IFn")
                    (or (procedure? val) (keyword? val) (symbol-t? val)
                        (pmap? val) (pset? val) (pvec? val)))
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
(define (jolt-class-for name)
  (let ((existing (hashtable-ref jolt-class-for-tbl name #f)))
    (if existing
        existing
        (let ((obj (make-class-obj name)))
          (hashtable-set! jolt-class-for-tbl name obj)
          obj))))
(def-var! "jolt.host" "jolt-class-for" jolt-class-for)

(define (class-key x)
  (cond ((jclass? x) (jclass-name x))
        ((string? x) x)
        ;; a deftype/defrecord NAME var holds its ctor; treat it as the class
        ((procedure? x) (hashtable-ref chez-deftype-ctor-tag x #f))
        (else #f)))
;; = compares jclass values by name (stable interning makes this eq?-level);
;; strings are no longer = to a jclass — class-key survives for internal
;; dispatch boundaries only (multimethod tables, catch dispatch, isa?).
(register-eq-arm! (lambda (a b) (and (jclass? a) (jclass? b)))
                  (lambda (a b) (let ((ka (class-key a)) (kb (class-key b)))
                                  (and ka kb (string=? ka kb) #t))))
(register-hash-arm! jclass? (lambda (x) (jolt-hash (jclass-name x))))
(register-str-render! jclass? (lambda (x) (string-append "class " (jclass-name x))))
(register-pr-arm! jclass? (lambda (x) (jclass-name x)))
;; print/println of a Class prints the bare name (getName), like pr — the JVM's
;; print-method for Class ignores *print-readably*. Only str is "class <name>".
(let ((prev (var-deref "clojure.core" "__print1")))
  (def-var! "clojure.core" "__print1"
    (lambda (x) (if (jclass? x) (jclass-name x) (jolt-invoke1 prev x)))))
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
;; (class x) on a jclass value returns java.lang.Class, so (instance? Class
;; (class y)) and class-based dispatch see the correct JVM class.
(register-class-arm! jclass? (lambda (x) "java.lang.Class"))

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
        (let ((given (if (pair? args) (car args) (exact (truncate (current-time))))))
          (make-jhost "random" (vector (random-init-seed given)))))))
  '("Random" "java.util.Random"))
(register-host-methods! "random"
  (list
    (cons "nextBytes" (lambda (self ba)
                        (let ((v (jolt-array-vec ba))
                              (st (jhost-state self)))
                          (do ((i 0 (fx+ i 1))) ((fx=? i (vector-length v)))
                            (vector-set! v i (random-next 8 st)))
                          jolt-nil)))
    (cons "nextInt" (lambda (self . a)
                      (let ((st (jhost-state self)))
                        (if (pair? a)
                            (let ((bound (exact (truncate (car a)))))
                              (if (<= bound 0)
                                  (error #f "Random.nextInt: bound must be positive")
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
(def-var! "jolt.host" "class-bases"
  (lambda (x)
    (let ((name (class-key x)))
      (if name
          (let* ((ds (jch-direct-supers name))
                 ;; a concrete class's bases include its superclass — Object when
                 ;; nothing more specific is modeled (interfaces have none).
                 (ds (if (or (string=? name "java.lang.Object")
                             (jch-interface? name)
                             (member "java.lang.Object" ds))
                         ds
                         (append ds '("java.lang.Object")))))
            (if (null? ds) jolt-nil (list->cseq (map jolt-class-for ds))))
          jolt-nil))))
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
(define jhost-class-names
  '(("instant" . "java.time.Instant")
    ("local-date" . "java.time.LocalDate")
    ("local-time" . "java.time.LocalTime")
    ("local-date-time" . "java.time.LocalDateTime")
    ("zoned-dt" . "java.time.ZonedDateTime")
    ("zoned-date-time" . "java.time.ZonedDateTime")
    ("offset-date-time" . "java.time.OffsetDateTime")
    ("offset-time" . "java.time.OffsetTime")
    ("duration" . "java.time.Duration")
    ("period" . "java.time.Period")
    ("year" . "java.time.Year")
    ("year-month" . "java.time.YearMonth")
    ("zone-id" . "java.time.ZoneId")
    ("zone-offset" . "java.time.ZoneOffset")
    ("zone-rules" . "java.time.zone.ZoneRules")
    ("chrono-unit" . "java.time.temporal.ChronoUnit")
    ("chrono-field" . "java.time.temporal.ChronoField")
    ("month-enum" . "java.time.Month")
    ("dow-enum" . "java.time.DayOfWeek")
    ("clock" . "java.time.Clock")
    ("dt-formatter" . "java.time.format.DateTimeFormatter")
    ("sdf" . "java.text.SimpleDateFormat")
    ("calendar" . "java.util.GregorianCalendar")
    ("locale" . "java.util.Locale")
    ("timezone" . "java.util.TimeZone")
    ("arraylist" . "java.util.ArrayList")
    ("linkedlist" . "java.util.LinkedList")
    ("arraydeque" . "java.util.ArrayDeque")
    ("hashmap" . "java.util.HashMap")
    ("hashset" . "java.util.HashSet")
    ;; io writer/reader shims: *out* is a PrintWriter like the JVM REPL's
    ("port-writer" . "java.io.PrintWriter")
    ("print-writer" . "java.io.PrintWriter")
    ("file-writer" . "java.io.FileWriter")
    ("writer" . "java.io.StringWriter")
    ("string-reader" . "java.io.StringReader")
    ("pushback-reader" . "java.io.PushbackReader")
    ("char-writer" . "java.io.OutputStreamWriter")
    ("char-reader" . "java.io.InputStreamReader")))
(register-class-arm!
  (lambda (x) (and (jhost? x) (assoc (jhost-tag x) jhost-class-names) #t))
  (lambda (x) (cdr (assoc (jhost-tag x) jhost-class-names))))
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
      ((map) (if (jolt-transient-ord x)
                 "clojure.lang.PersistentArrayMap$TransientArrayMap"
                 "clojure.lang.PersistentHashMap$TransientHashMap"))
      ((set) "clojure.lang.PersistentHashSet$TransientHashSet")
      (else "clojure.lang.ATransientCollection"))))
;; instance? for these shims derives from the class graph: the value's class name
;; (jhost-class-names) walked through jch-isa? answers interface questions —
;; (instance? java.util.List an-ArrayList), Deque/Queue/Collection/Iterable chains.
;; Widening only: an unknown pairing passes to the other arms, never denies.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (jhost? val) (symbol-t? type-sym))
        (let ((p (assoc (jhost-tag val) jhost-class-names)))
          (if p
              (let* ((tname (symbol-t-name type-sym))
                     (q (or (resolve-class-hint tname) tname)))
                (if (jch-isa? (cdr p) q) #t 'pass))
              'pass))
        'pass)))
;; count over the mutable collection shims, like RT.count over a java.util
;; Collection/Map on the JVM.
(define %shim-count jolt-count)
(set! jolt-count
  (lambda (x)
    (if (jhost? x)
        (let ((tag (jhost-tag x)))
          (cond ((or (string=? tag "arraylist") (string=? tag "linkedlist")
                     (string=? tag "arraydeque"))
                 (al-cnt x))
                ((or (string=? tag "hashmap") (string=? tag "hashset"))
                 (hashtable-size (hm-tbl x)))
                (else (%shim-count x))))
        (%shim-count x))))
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
(define kw-rc-jtype (keyword "jolt" "type"))
(define kw-rc (keyword "jolt" "reader-conditional"))
(define (reader-conditional-value? x)
  (jolt-reader-conditional-record? x))
(register-class-arm! reader-conditional-value? (lambda (x) "clojure.lang.ReaderConditional"))
;; a multimethod reports its JVM class.
(register-class-arm! (lambda (x) (jolt-multifn? x)) (lambda (x) "clojure.lang.MultiFn"))
;; exact-own-class fallback: (instance? C x) is true when C names x's own class —
;; covers checks against a captured (class y) value (transient classes, MultiFn)
;; that no interface arm models. Widening only.
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
                      (and (procedure? x) (hashtable-ref chez-deftype-ctor-tag x #f) #t))
                  #t #f)))
;; nth over the java.util List shims, like RT.nth on a java.util.List.
(define %shim-nth jolt-nth)
(set! jolt-nth
  (case-lambda
    ((coll i) (if (al-family? coll) (%shim-nth (list->cseq (al->list coll)) i) (%shim-nth coll i)))
    ((coll i d) (if (al-family? coll) (%shim-nth (list->cseq (al->list coll)) i d) (%shim-nth coll i d)))))
(def-var! "clojure.core" "nth" jolt-nth)

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
