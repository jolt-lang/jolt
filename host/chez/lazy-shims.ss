;; lazy-shims.ss — host shims that load on first reference (#1517).
;;
;; Most of the java.util / java.lang shims under host/chez/java are part of every
;; boot: their code is placed into the heap and their top level runs, class
;; registrations and all, before the program's first form. For a class most
;; programs never name — Vector, ConcurrentSkipListMap, ProcessBuilder — that is
;; startup paid for nothing. Boot cost tracks the code the image carries (about
;; 10ms per MB of runtime source on x86_64 Linux), and the 0.8.24 window grew it
;; by 1.3MB.
;;
;; So rt.ss names such a file with (lazy-load "PATH") instead of (load "PATH").
;; In a boot that leaves the file out, what it would have registered is known
;; ahead of time — lazy-shims-index.ss, generated from the file itself by
;; tools/lazy-shims-gen.ss and held to it by `make lazyshims` — and the first reference
;; to any of it loads the file:
;;
;;   - a class it registers (a constructor, statics, an enum): lib-try-autoload!,
;;     the miss every class lookup already takes for a library provider;
;;   - a static member it adds to a class some other file registers
;;     (Runtime/getRuntime, whose Runtime is zip-file.ss's): host-static-ref's
;;     member miss;
;;   - an instance method it adds to a tag some other file owns: dispatch-miss
;;     (records-dispatch.ss), the end of every method dispatch.
;;
;; Its class-hierarchy rows (jch-register-supers!) are registered where the
;; (lazy-load …) form stands, so instance?, isa?, ancestors and Class/forName see
;; the class from the start, exactly as when the file was part of the boot.
;;
;; WHAT A LAZY FILE MAY DO is narrower than what a file in the boot may do, and
;; tools/lazy-shims-gen.ss checks it: it may not replace a member some other file
;; registered (a boot that has not loaded it would answer the old member, one that
;; has the new — behavior that depends on what ran earlier), nothing outside it
;; may name its definitions except in code that only runs once it has loaded, and
;; no other file may use its macros. A file that cannot meet that stays (load …).
;;
;; WHERE THE CODE COMES FROM when it loads:
;;   - the jolt binary: a fasl build-jolt.ss compiled from the file, in the same
;;     embedded blob as the stdlib namespaces (entry "lazy:PATH");
;;   - a source checkout (bin/jolt, the gates, build-boot): the file on disk,
;;     relative to the directory the boot ran in;
;;   - a `jolt build` program: the file inlined where the (lazy-load …) form stands
;;     when the program names something it provides (build.ss
;;     bld-lazy-shims-for-app), and nothing otherwise — a reference the build could
;;     not see then reports the class as left out, with the way to bring it in.
;;
;; JOLT_LAZY_SHIMS=eager loads each file where its form stands, as (load …) would.
;; It is how tools/lazy-shims-gen.ss reads what a file registers, and the way to rule
;; laziness out when chasing a problem in a source checkout.

;; path -> #(status deps abs-path) — status: pending, loading or loaded; abs-path is
;; where a source boot reads the file, #f in a built binary
(define lazy-shim-units (make-hashtable string-hash string=?))
(define lazy-shim-class-tbl  (make-hashtable string-hash string=?))  ; class name -> path
(define lazy-shim-static-tbl (make-hashtable string-hash string=?))  ; "Class#member" -> path
(define lazy-shim-method-tbl (make-hashtable string-hash string=?))  ; method name -> (path ...)
(define lazy-shim-tag-tbl    (make-hashtable string-hash string=?))  ; host tag -> path
(define lazy-shim-index-tbl  (make-hashtable string-hash string=?))  ; path -> its index row
;; How many declared units have not loaded. The triggers read it without the lock:
;; a stale non-zero only sends a miss through the table lookups, and it never goes
;; back up, so a zero is final.
(define lazy-shims-pending 0)
(define lazy-shims-mu (make-mutex))

(define (lazy-shims-eager?)
  (let ((v (getenv "JOLT_LAZY_SHIMS"))) (and v (string=? v "eager"))))

(define lazy-shim-order '())   ; declared paths, newest first
;; The index rows, one per file: (PATH (deps P ...) (classes C ...)
;; (statics (C M) ...) (methods M ...) (tags T ...) (supers (C S ...) ...)). Applied before
;; the first (lazy-load …); a row for a file no form declares is inert.
(define (lazy-shims-index! rows)
  (for-each
    (lambda (row)
      (let ((path (car row)))
        (hashtable-set! lazy-shim-index-tbl path row)
        (for-each (lambda (c) (hashtable-set! lazy-shim-class-tbl c path))
                  (lazy-shim-row-field row 'classes))
        (for-each (lambda (cm) (hashtable-set! lazy-shim-static-tbl (string-append (car cm) "#" (cadr cm)) path))
                  (lazy-shim-row-field row 'statics))
        (for-each (lambda (m)
                    (hashtable-update! lazy-shim-method-tbl m (lambda (ps) (append ps (list path))) '()))
                  (lazy-shim-row-field row 'methods))
        (for-each (lambda (t) (hashtable-set! lazy-shim-tag-tbl t path))
                  (lazy-shim-row-field row 'tags))))
    rows))
(define (lazy-shim-row-field row key)
  (let ((f (assq key (cdr row)))) (if f (cdr f) '())))

;; (lazy-load PATH): rt.ss's form for a file that loads on first reference.
;; (lazy-load PATH 'built): the same in a built binary (build.ss
;; bld-emit-lazy-load!), which reads the file from its embedded fasls or not at
;; all — never from a directory that happens to hold a jolt checkout.
(define (lazy-load path . built)
  (let ((row (hashtable-ref lazy-shim-index-tbl path #f)))
    (cond
      ((and (lazy-shims-eager?) (null? built))
       (load path)
       (set! lazy-shim-order (cons path lazy-shim-order))
       (hashtable-set! lazy-shim-units path (vector 'loaded '() path)))
      ((not row)
       ;; declared lazy but missing from the index: the index is stale, and a file
       ;; nothing can trigger would simply never load. Load it here instead, and
       ;; say so — `make lazyshims` is the fix.
       (fprintf (current-error-port)
                "jolt: ~a is declared lazy-load but is not in lazy-shims-index.ss; ~a (regenerate it: chez --script tools/lazy-shims-gen.ss)\n"
                path (if (null? built) "loading it at boot" "nothing can load it"))
       (when (null? built) (load path))
       (hashtable-set! lazy-shim-units path (vector 'loaded '() path)))
      (else
       ;; its place in the class graph, now, as when it was part of the boot
       (for-each (lambda (r) (jch-register-supers! (car r) (cdr r)))
                 (lazy-shim-row-field row 'supers))
       (unless (hashtable-ref lazy-shim-units path #f)
         (set! lazy-shim-order (cons path lazy-shim-order))
         (hashtable-set! lazy-shim-units path
                         (vector 'pending (lazy-shim-row-field row 'deps)
                                 (and (null? built) (string-append (current-directory) "/" path))))
         (set! lazy-shims-pending (fx+ lazy-shims-pending 1)))))))

;; A `jolt build` program carries a file it needs inlined where its form stood;
;; the build follows the inlined text with this.
(define (lazy-shim-inlined! path)
  (let ((u (hashtable-ref lazy-shim-units path #f)))
    (cond
      ((not u) (hashtable-set! lazy-shim-units path (vector 'loaded '() path)))
      ((eq? (vector-ref u 0) 'pending)
       (vector-set! u 0 'loaded)
       (set! lazy-shims-pending (fx- lazy-shims-pending 1))))))

;; Load PATH's unit (its deps first). #t when the unit was not loaded when asked
;; and is loaded now — by this call, or by another thread this one waited for,
;; since either way the caller's lookup will now find what it missed. #f when it was
;; loaded already (a second miss is a real one), is loading on this thread (a
;; trigger from inside its own top level), or is not a unit at all. A unit whose
;; code this binary does not carry raises, naming what wanted it.
(define (lazy-shim-load! path wanted)
  (let ((u (hashtable-ref lazy-shim-units path #f)))
    (and u
         ;; 'loading too: another thread's load holds the mutex until it is done,
         ;; so taking it waits for that load; this thread's own (a trigger from
         ;; inside the file's top level) re-enters the recursive mutex and finds
         ;; it still loading
         (memq (vector-ref u 0) '(pending loading))
         (begin
           (jolt-with-mutex lazy-shims-mu
             (when (eq? (vector-ref u 0) 'pending)
               (vector-set! u 0 'loading)
               (for-each (lambda (d) (lazy-shim-load! d wanted)) (vector-ref u 1))
               (let ((ok #f))
                 (dynamic-wind
                   (lambda () #f)
                   (lambda () (lazy-shim-load-code! path (vector-ref u 2) wanted) (set! ok #t))
                   (lambda ()
                     (if ok
                         (begin (vector-set! u 0 'loaded)
                                (set! lazy-shims-pending (fx- lazy-shims-pending 1)))
                         ;; a load that raised leaves it pending: the next
                         ;; reference tries again and reports the same way
                         (vector-set! u 0 'pending)))))))
           (eq? (vector-ref u 0) 'loaded)))))

(define (lazy-shim-load-code! path abs wanted)
  ;; io.ss's reader of the embedded fasl blob, resolved at the call: this file
  ;; loads before io.ss does
  (let ((bv (let ((fetch (sa-baked-global 'jolt-embedded-fasl)))
              (and fetch (fetch (string-append "lazy:" path))))))
    (cond
      (bv (load-compiled-from-port (open-bytevector-input-port bv)))
      ((and abs (file-exists? abs)) (load abs))
      (else (lazy-shim-absent! path wanted)))))

(define (lazy-shim-absent! path wanted)
  (throw-jvm 'NoClassDefFoundError
    (string-append
      (if wanted (string-append wanted ": ") "")
      "its host shim " path " is not in this program. jolt build links a host shim "
      "only when the program's source names one of its classes or members; one named "
      "only at run time (a computed Class/forName, a method found by reflection) needs "
      "a reference the build can see, such as an :import of the class.")))

;; ---- the triggers ----------------------------------------------------------------
;; Each answers #t when it loaded something, so the caller looks again.

;; A class name, resolved the way the registries resolve one (lookup-class): as
;; given, then by its last dotted segment.
(define (lazy-shim-class-path name)
  (and (string? name)
       (or (hashtable-ref lazy-shim-class-tbl name #f)
           (hashtable-ref lazy-shim-class-tbl (short-class-name name) #f))))
(define (lazy-shims-class! name)
  (and (fx>? lazy-shims-pending 0)
       (let ((p (lazy-shim-class-path name)))
         (and p (lazy-shim-load! p name)))))

;; Class/member, for a member a lazy file adds to a class it does not register.
(define (lazy-shims-static! class member)
  (and (fx>? lazy-shims-pending 0)
       (or (lazy-shims-class! class)
           (let ((p (or (hashtable-ref lazy-shim-static-tbl (string-append class "#" member) #f)
                        (hashtable-ref lazy-shim-static-tbl
                                       (string-append (short-class-name class) "#" member) #f))))
             (and p (lazy-shim-load! p (string-append class "/" member)))))))

;; An instance method a lazy file adds to a tag another file owns. Every unit
;; that adds a member of that name loads: the miss does not say which tag asked.
(define (lazy-shims-method! name)
  (and (fx>? lazy-shims-pending 0)
       (let loop ((ps (hashtable-ref lazy-shim-method-tbl name '())) (any #f))
         (if (null? ps)
             any
             (loop (cdr ps) (or (lazy-shim-load! (car ps) (string-append "." name)) any))))))

;; A host object of a tag only a lazy file makes, arriving some other way than
;; through that file: an image read (state-image.ss), whose objects are rebuilt
;; from their tag and state with no constructor run. The file's read checks and
;; methods must be there before the object is.
(define (lazy-shims-tag! tag)
  (and (fx>? lazy-shims-pending 0)
       (let ((p (hashtable-ref lazy-shim-tag-tbl tag #f)))
         (and p (lazy-shim-load! p tag)))))

;; Does a lazy file provide NAME, spelled exactly so? Asked where the answer
;; matters before anything is loaded: runtime-provides-class? (a library's claim on a class the runtime
;; provides is dropped) and host-class-registered? (the analyzer's "is this a
;; class"). A loaded file's classes are in the registries themselves.
(define (lazy-shims-provides? name)
  (and (string? name) (hashtable-ref lazy-shim-class-tbl name #f) #t))

;; For tests and tools: the declared units and their status.
(define (lazy-shims-status)
  (let-values (((ks vs) (hashtable-entries lazy-shim-units)))
    (list-sort (lambda (a b) (string<? (car a) (car b)))
               (map (lambda (k v) (cons k (vector-ref v 0))) (vector->list ks) (vector->list vs)))))
(define (lazy-shims-load-all!)
  (for-each (lambda (p) (lazy-shim-load! p #f)) (reverse lazy-shim-order)))
