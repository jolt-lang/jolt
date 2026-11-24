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

;; A confined reduce-kv whose callback only hands its accumulator to a final
;; (assoc acc K V); jolt.passes.facts/reducer-confinement-at proves that shape.
;; The emitter passes `step`, the same callback with that assoc re-targeted
;; to jolt-reduce-kv-collect, and the whole fold runs through it. It evaluates
;; every key and value expression in order, as the ordinary fold does, and
;; accumulates the computed entries as an immutable list, so a continuation
;; that re-enters the fold carries its own entries. A builder then writes them
;; with interrupts off; nothing in that region calls user code. The first key
;; that cannot be hashed natively freezes the entries so far, and from there
;; the collector is the ordinary step's assoc on a persistent map.

;; The builder region is bounded so a fiber, signal or GC can run again soon.
(define pmap-builder-max-entries 256)
(define (pmap-builder-key? k)
  (or (keyword-t? k) (and (string? k) (fx<=? (string-length k) 128))
      (fixnum? k) (char? k) (jolt-nil? k)))

(define (jolt-reduce-kv-builder-source? source)
  (and (pmap? source)
       ;; For one or two writes the ordinary array copies cost less than setup.
       (fx<=? 3 (pmap-cnt source) pmap-builder-max-entries)))

(define (reduce-kv-entries? acc) (or (pair? acc) (null? acc)))

(define (reduce-kv-freeze entries)
  (if (null? entries)
      empty-pmap
      (let ((entries (reverse entries)))
        (dynamic-wind
          disable-interrupts
          (lambda ()
            (let ((t (pmap-builder-new (length entries))))
              (for-each (lambda (e) (pmap-builder-put! t (car e) (cdr e))) entries)
              (jolt-persistent! t)))
          enable-interrupts))))

(define (jolt-reduce-kv-collect acc k v)
  (cond ((not (reduce-kv-entries? acc)) (jolt-assoc3 acc k v))
        ((pmap-builder-key? k) (cons (cons k v) acc))
        (else (jolt-assoc3 (reduce-kv-freeze acc) k v))))

(define (jolt-reduce-kv-build seed source step)
  (let ((r (walk-pmap-kv source (kv-step step) '())))
    (if (reduce-kv-entries? r) (reduce-kv-freeze r) r)))
