;; post-prelude-str.ss — Chez-only native overrides over clojure.string.
;; Loaded right after post-prelude.ss on the CHEZ boot (cli.ss / bootstrap /
;; build prologues) but NOT part of the Gambit cross-mint boot: these bind
;; natives from java/natives-str.ss, which the Gambit boot excludes along with
;; the rest of the java/ tail. Gambit keeps the overlay versions.

;; --- hot-path natives over prelude-baked Clojure wrappers --------------------
;; clojure.string's wrappers are compiled into the seed prelude as chained
;; overlay calls (to-str -> count -> subs -> =, ~400-500ns/call, allocating a
;; substring per invocation) over the ~40ns natives. Replace the vars with
;; single-proc natives (natives-str.ss) carrying the same semantics. Calls
;; already compiled against these vars pick the new values up through the var
;; cells, so no remint is needed.
(def-var! "clojure.string" "starts-with?" str-starts-with?)
(def-var! "clojure.string" "ends-with?" str-ends-with?)
(def-var! "clojure.string" "includes?" str-includes?)
(def-var! "clojure.string" "index-of" str-index-of*)
(def-var! "clojure.string" "upper-case" str-upper-c)
(def-var! "clojure.string" "lower-case" str-lower-c)
;; clojure.string/replace with a STRING pattern and STRING replacement is a
;; literal replace-all (JVM semantics) — the common "-" -> "_" / quoting case.
;; The overlay replace routes through to-str + str-replace-all's pattern-type
;; dispatch (~400ns); route the literal case straight to the native scan.
(define jolt-string-replace
  (let ((ov-replace (var-deref "clojure.string" "replace")))
    (case-lambda
      ((s match replacement)
       (if (and (string? match) (string? replacement))
           (str-replace-literal (str-coerce s) match replacement)
           (jolt-invoke ov-replace s match replacement))))))
(def-var! "clojure.string" "replace" jolt-string-replace)

;; These limits bound the work until a fiber, signal or GC can run again.
;; Only scalar hashing/equality, literal string copies and intern hits run
;; inside the region. A missed intern can take a lock, so it ends the region.
(define pmap-builder-max-entries 256)
(define pmap-builder-stock-timer (timer-interrupt-handler))
(define pmap-builder-stop (make-jolt-reduced #f))
(define (pmap-builder-short-string? x)
  (and (string? x) (fx<=? (string-length x) 128)))
(define (pmap-builder-key? k)
  (or (keyword-t? k) (pmap-builder-short-string? k) (fixnum? k) (char? k) (jolt-nil? k)))

;; The source is a built-in immutable map. Finding the first unhandled
;; entry again only reads its slots and trie nodes. Keep the skip counter
;; in the continuation too: this walk may be interrupted or re-entered.
(define (reduce-kv-builder-resume f source prefix count missed retry)
  (let ((step (kv-step f)))
    (let ((r (walk-pmap-kv source
               (lambda (acc k v)
                 (cond ((not (fixnum? acc)) (step acc k v))
                       ((fx>? acc 0) (fx- acc 1))
                       (missed (jolt-site-kept (retry prefix k v (car missed))))
                       (else (step prefix k v))))
               count)))
      (if (jolt-reduced? r) (jolt-reduced-val r) r))))

(define (reduce-kv-build f seed source attempt retry)
  (cond
    ((not (pmap? source)) (jolt-reduce-kv f seed source))
    ((fx=? 0 (pmap-cnt source)) seed)
    ;; For one or two writes the ordinary array copies cost less than setup.
    ((or (fx<=? (pmap-cnt source) 2)
         (fx>? (pmap-cnt source) pmap-builder-max-entries)
         ;; A custom engine handler can observe or change an in-flight fold.
         (let ((handler (timer-interrupt-handler)))
           (not (or (eq? handler pmap-builder-stock-timer) (eq? handler jolt-tick-fire)))))
     (jolt-reduce-kv f seed source))
    (else
     ;; The live builder never crosses a host delivery point or a call into
     ;; user code. On refusal publish the persistent prefix before performing
     ;; the first ordinary step. A continuation from that step cannot get
     ;; back to the builder, even if it resumes before the fold returns.
     (let-values (((prefix count missed)
                   (dynamic-wind
                     disable-interrupts
                     (lambda ()
                       (let ((t (pmap-builder-new (pmap-cnt source))) (count 0) (missed #f))
                         (let ((r (walk-pmap-kv source
                                    (lambda (_ k v)
                                      (let ((decision (attempt t k v)))
                                        (if (eq? decision #t)
                                            (begin (set! count (fx+ count 1)) #t)
                                            (begin (set! missed decision) pmap-builder-stop))))
                                    #t)))
                           (values (if (fx=? count 0) seed (jolt-persistent! t))
                                   (and (eq? r pmap-builder-stop) count) missed))))
                     enable-interrupts)))
       (cond ((not count) prefix)
             ((and (fx=? count 0) (not missed)) (jolt-reduce-kv f seed source))
             (else (reduce-kv-builder-resume f source prefix count missed retry)))))))

(define (pmap-builder-write! t k v)
  (pmap-builder-put! t k v)
  #t)
(define (jolt-reduce-kv-build-copy f seed source)
  (reduce-kv-build f seed source
    (lambda (t k v) (and (pmap-builder-key? k) (pmap-builder-write! t k v))) #f))

(define (builder-interned-keyword ns name)
  (if ns
      (hashtable-ref keyword-table (keyword-intern-key ns name) #f)
      (hashtable-ref keyword-table-bare name #f)))
(define (builder-symbol-key k)
  (if (symbol-t? k)
      (let ((ns (jolt-namespace k)) (name (symbol-t-name k)))
        (and (pmap-builder-short-string? name)
             (if (jolt-nil? ns)
                 (keyword-from-string name builder-interned-keyword)
                 (and (pmap-builder-short-string? ns) (builder-interned-keyword ns name)))))
      (and (pmap-builder-key? k) k)))
(define (jolt-reduce-kv-build-symbol-keys f seed source)
  (reduce-kv-build f seed source
    (lambda (t k v) (let ((key (builder-symbol-key k))) (and key (pmap-builder-write! t key v)))) #f))

(define (jolt-reduce-kv-build-string-replace f seed source replace-root match replacement retry)
  (reduce-kv-build f seed source
    (lambda (t k v)
      (and (pmap-builder-key? k) (string? v) (fx<=? (string-length v) 1024)
           (let ((proc (replace-root)))
             ;; Read the linked callable at this step. If it has changed, hand
             ;; that very value to the ordinary application for this entry.
             (if (eq? proc jolt-string-replace)
                 (pmap-builder-write! t k (str-replace-literal v match replacement))
                 (list proc)))))
    retry))
