;; lazy-shims-gen.ss — write (or check) host/chez/lazy-shims-index.ss.
;;
;;   chez --script tools/lazy-shims-gen.ss           rewrite the index
;;   chez --script tools/lazy-shims-gen.ss --check   fail if it is stale or a
;;                                                       lazy file breaks a rule
;;
;; Boots the runtime from source with every (lazy-load …) loading in place
;; (JOLT_LAZY_SHIMS=eager), the boot those files were written for, and reads off
;; what each one did by diffing the registries around every file the boot loads.
;; See lazy-shims.ss for what the index is for. A lazy file:
;;
;;   - may register classes, statics and constructors of its own, and members on
;;     its own tags (a tag is a file's when that file names its class) — those
;;     can only be reached through a class lookup, which is a trigger;
;;   - may ADD a static to another file's class or a method to another file's tag
;;     — the index lists them, and a miss on one is a trigger;
;;   - may NOT replace anything another file registered: a member, a constructor,
;;     a tag's class, an interface default (rule "override");
;;   - may NOT be named by any other runtime file, except files that only call
;;     into it from code running on its own values (rule "eager-ref",
;;     lazy-shims-ref-allow below);
;;   - may NOT define a macro another file uses (rule "macro");
;;   - may NOT have a class another runtime file reaches by name (rule
;;     "eager-name": a "java.util.Vector" in code that builds or calls one) —
;;     a `jolt build` program carries a lazy file only when ITS code names the
;;     class, so the runtime's own uses would go unseen there. A name used as
;;     data (a class-graph row, an image codec's kind) is listed in
;;     lazy-shims-name-allow;
;;   - may NOT put a dispatch arm where a later file's arm would now run ahead of
;;     it, or behind it (rule "arm-order"), unless the two predicates cannot both
;;     answer one value (lazy-shims-arm-allow below).
(import (chezscheme))

(define args (cdr (command-line)))
(define check? (member "--check" args))
;; --lazy-digest FILE: the second boot (see "lazy boot" below), run as a child
(define lazy-digest-out (let ((m (member "--lazy-digest" args))) (and m (pair? (cdr m)) (cadr m))))
(define index-path "host/chez/lazy-shims-index.ss")
(if lazy-digest-out
    (putenv "JOLT_LAZY_SHIMS" "")
    (putenv "JOLT_LAZY_SHIMS" "eager"))

;; Files that name a lazy file's definitions only inside code that runs on that
;; file's own values (an image codec's encoder sees a Vector only once Vector has
;; loaded; its decoder triggers the load by class name first).
(define lazy-shims-ref-allow '("host/chez/java/image-codecs.ss"))
;; (file name): a lazy class's name in another file as data, not a use. A file
;; listed alone holds only such data.
(define lazy-shims-name-allow
  '(("host/chez/lazy-shims-index.ss")                ; the index itself
    ("host/chez/java/class-hierarchy.ss")              ; class-graph rows
    ("host/chez/java/jdk-interfaces.ss")               ; class-graph rows (generated)
    ("host/chez/java/image-codecs.ss")                 ; codec kinds
    ("host/chez/ns.ss" "ProcessBuilder")               ; the default java.lang imports
    ("host/chez/java/host-static.ss" "EnumSet")        ; an arity row: EnumSet/of
    ;; a ListIterator's class for a Vector or Stack it is handed
    ("host/chez/java/coll-defaults.ss" "java.util.Vector")
    ("host/chez/java/coll-defaults.ss" "java.util.Stack")))
(define (name-allowed? f name)
  (exists (lambda (a) (and (string=? (car a) f) (or (null? (cdr a)) (string=? (cadr a) name))))
          lazy-shims-name-allow))
;; (lazy-file chain later-file): the two files' arms on that chain may both answer
;; one value and their order is known not to matter. Each entry says why. Pairs the
;; sample pool (arm-samples below) shows disjoint need no entry.
(define lazy-shims-arm-allow '())

;; ---- snapshots ---------------------------------------------------------------------
(define (tbl sym) (and (top-level-bound? sym) (let ((t (top-level-value sym))) (and (hashtable? t) t))))
(define (keys-of sym) (let ((t (tbl sym))) (if t (vector->list (hashtable-keys t)) '())))
;; ((key . member) . value) for the two-level tables
(define (members-of sym)
  (let ((t (tbl sym)) (acc '()))
    (when t
      (vector-for-each
        (lambda (k)
          (let ((v (hashtable-ref t k #f)))
            (when (hashtable? v)
              (vector-for-each (lambda (m) (set! acc (cons (cons (cons k (if (string? m) m (format "~a" m))) (hashtable-ref v m #f)) acc)))
                               (hashtable-keys v)))))
        (hashtable-keys t)))
    acc))
(define (flat-of sym)   ; (key . value) for a one-level table
  (let ((t (tbl sym))) (if t (let-values (((ks vs) (hashtable-entries t))) (map cons (vector->list ks) (vector->list vs))) '())))
(define (chains)
  (filter (lambda (p) (list? (cdr p)))
          (map (lambda (s) (cons s (top-level-value s)))
               (filter (lambda (s)
                         (and (top-level-bound? s)
                              (let ((n (symbol->string s)))
                                (or (string-suffix? n "-arms") (string-suffix? n "-registry")))))
                       (environment-symbols (interaction-environment))))))
(define (string-suffix? s suf)
  (let ((n (string-length s)) (m (string-length suf)))
    (and (>= n m) (string=? (substring s (- n m) n) suf))))
(define (bound-syms)
  (let ((h (make-eq-hashtable)))
    (for-each (lambda (s) (when (or (top-level-bound? s) (top-level-syntax? s)) (hashtable-set! h s #t)))
              (environment-symbols (interaction-environment)))
    h))

(define (snapshot)
  (vector (members-of 'class-statics-tbl) (flat-of 'class-ctors-tbl) (flat-of 'enum-class-tbl)
          (members-of 'host-methods-tbl) (flat-of 'jhost-tag->fqn) (flat-of 'cd-default-tbl)
          (flat-of 'jvm-class-parents) (chains) (bound-syms)
          (keys-of 'class-statics-tbl) (keys-of 'host-methods-tbl)))

(define (pair-table alist eqv)   ; key -> value, with keys compared by equal?
  (let ((h (make-hashtable equal-hash equal?))) (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) alist) h))

;; per loaded file: #(path before after) in load order
(define file-log '())
(define real-load load)
;; a file whose load ran another load is a PARENT: its diff holds its children's
(define load-depth-children '())   ; stack: did the load at this depth run another?
(define parents '())
(define (gen-load path . rest)
  (unless (null? load-depth-children) (set-car! load-depth-children #t))
  (set! load-depth-children (cons #f load-depth-children))
  (let ((before (snapshot)))
    (apply real-load path rest)
    (when (car load-depth-children) (set! parents (cons path parents)))
    (set! load-depth-children (cdr load-depth-children))
    (set! file-log (cons (vector path before (snapshot)) file-log))))
(define load gen-load)

;; the boot jolt itself carries (build.ss bld-runtime-manifest)
(load "host/chez/scheme-adapter-runtime.ss")
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(load "host/chez/post-prelude-str.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/cli-core.ss")
(load "host/chez/png.ss")
(load "host/chez/loader.ss")
(load "host/chez/diagnostic-render.ss")
(load "host/chez/java/ffi.ss")
(set! file-log (reverse file-log))

;; ---- the boot's shape ----------------------------------------------------------------
;; One line per registered thing, with a procedure named by its source form: two
;; boots that registered the same things from the same code read the same. The
;; lazy boot (lazy files loaded on reference, here all at once after the boot) must
;; read the same as the eager one, or something depended on load order.
(define (value-sig v)
  (cond
    ((procedure? v)
     (guard (e (#t "proc"))
       (let* ((c ((inspect/object v) 'code)) (src (c 'source)))
         (format "proc:~a:~a" (or (c 'name) "") (if src (equal-hash (ungensym (src 'value))) "")))))
    ((string? v) (string-append "str:" v))
    ((symbol? v) (format "sym:~a" v))
    ((number? v) (format "num:~a" v))
    ((boolean? v) (format "bool:~a" v))
    ((list? v) (format "list:~a" (length v)))
    (else "obj")))
;; a macro's expansion carries gensyms whose unique names differ run to run
(define (ungensym x)
  (cond ((gensym? x) (string->symbol (symbol->string x)))
        ((pair? x) (cons (ungensym (car x)) (ungensym (cdr x))))
        ((vector? x) (vector-map ungensym x))
        (else x)))
(define (digest)
  (let ((lines '()))
    (define (emit! . xs) (set! lines (cons (apply string-append xs) lines)))
    (for-each (lambda (p) (emit! "statics " (caar p) " " (cdar p) " " (value-sig (cdr p)))) (members-of 'class-statics-tbl))
    (for-each (lambda (p) (emit! "method " (caar p) " " (cdar p) " " (value-sig (cdr p)))) (members-of 'host-methods-tbl))
    (for-each (lambda (p) (emit! "ctor " (car p) " " (value-sig (cdr p)))) (flat-of 'class-ctors-tbl))
    (for-each (lambda (p) (emit! "enum " (car p))) (flat-of 'enum-class-tbl))
    (for-each (lambda (p) (emit! "fqn " (car p) " " (value-sig (cdr p)))) (flat-of 'jhost-tag->fqn))
    (for-each (lambda (p) (emit! "iface " (car p) " " (value-sig (cdr p)))) (flat-of 'cd-default-tbl))
    (for-each (lambda (p) (emit! "supers " (car p) (apply string-append (map (lambda (x) (string-append " " x)) (cdr p)))))
              (flat-of 'jvm-class-parents))
    ;; a chain as a multiset: its ORDER is the arm-order rule's
    (for-each (lambda (c)
                (for-each (lambda (x) (emit! "arm " (symbol->string (car c)) " "
                                             (if (pair? x) (value-sig (car x)) (value-sig x))))
                          (cdr c)))
              (chains))
    (list-sort string<? lines)))

(when lazy-digest-out
  (lazy-shims-load-all!)
  (call-with-output-file lazy-digest-out
    (lambda (o) (for-each (lambda (l) (put-string o l) (newline o)) (digest)))
    'replace)
  (exit 0))

;; A nested load shows up twice: once for itself and again inside its parent's
;; diff. A file's own contribution is its diff minus its children's, so walk the
;; log innermost-first: inner loads finish (and are logged) before their parent.
(define lazy-paths (map car (lazy-shims-status)))
(define (lazy? p) (member p lazy-paths))

;; who first created / last changed each key, across the whole boot, attributed to
;; the innermost file that did it
(define owner (make-hashtable equal-hash equal?))     ; (kind . key) -> path that created it
(define last-writer (make-hashtable equal-hash equal?))
(define violations '())
(define (violate! rule path what)
  (set! violations (cons (format "~a: ~a: ~a" rule path what) violations)))

(define (diff-alist kind path b a)
  ;; new keys and changed values between before (b) and after (a)
  (let ((bt (pair-table b equal?)))
    (for-each
      (lambda (p)
        (let* ((k (cons kind (car p))) (old (hashtable-ref bt (car p) 'absent)))
          (cond
            ((eq? old 'absent)
             (unless (hashtable-ref owner k #f) (hashtable-set! owner k path))
             (hashtable-set! last-writer k path))
            ((not (eq? old (cdr p)))
             (hashtable-set! last-writer k path)
             (unless (equal? (hashtable-ref owner k path) path)
               (hashtable-set! owner (cons 'override k) (cons path (hashtable-ref owner k #f))))))))
      a)))

;; innermost-first attribution: process entries in log order (children are logged
;; before their parents), and let a parent's diff only claim what no child did
(for-each
  (lambda (e)
    (let ((path (vector-ref e 0)) (b (vector-ref e 1)) (a (vector-ref e 2)))
      (diff-alist 'statics path (vector-ref b 0) (vector-ref a 0))
      (diff-alist 'ctor path (vector-ref b 1) (vector-ref a 1))
      (diff-alist 'enum path (vector-ref b 2) (vector-ref a 2))
      (diff-alist 'method path (vector-ref b 3) (vector-ref a 3))
      (diff-alist 'tag-fqn path (vector-ref b 4) (vector-ref a 4))
      (diff-alist 'iface-default path (vector-ref b 5) (vector-ref a 5))))
  file-log)

;; the innermost entry for each lazy path
(define (entry-of path) (find (lambda (e) (string=? (vector-ref e 0) path)) file-log))

;; ---- what each lazy file registered ------------------------------------------------
(define (sorted-strings xs) (list-sort string<? (dedupe xs)))
(define (dedupe xs) (let loop ((xs xs) (acc '())) (cond ((null? xs) (reverse acc)) ((member (car xs) acc) (loop (cdr xs) acc)) (else (loop (cdr xs) (cons (car xs) acc))))))

(define (keys-owned-by kind path)
  (let-values (((ks vs) (hashtable-entries owner)))
    (let loop ((i 0) (acc '()))
      (if (= i (vector-length ks)) acc
          (loop (+ i 1)
                (let ((k (vector-ref ks i)))
                  (if (and (pair? k) (eq? (car k) kind) (equal? (vector-ref vs i) path))
                      (cons (cdr k) acc)
                      acc)))))))

;; a tag's owner: the file that named its class, else the file that made its table.
;; A lazy file also owns every tag whose class is one of its own classes (or one
;; nested in one): only it makes such objects, whatever made the table first (the
;; views file registers spliterators on ArrayDeque's tag ahead of ArrayDeque).
(define (tag-owner tag)
  (or (lazy-tag-owner tag)
      (hashtable-ref owner (cons 'tag-fqn tag) #f)
      (hashtable-ref owner (cons 'tag tag) #f)))
(define (lazy-tag-owner tag)
  (or
    (let ((fqn (let ((t (tbl 'jhost-tag->fqn))) (and t (hashtable-ref t tag #f)))))
      (and (string? fqn)
           (let ((outer (let loop ((i 0)) (cond ((= i (string-length fqn)) fqn)
                                                ((char=? (string-ref fqn i) #\$) (substring fqn 0 i))
                                                (else (loop (+ i 1)))))))
             (find (lambda (p) (member outer (classes-of* p))) lazy-paths))))
    ;; ...or the only file that builds one (a Process comes from nowhere but
    ;; ProcessBuilder.start, whatever file names java.lang.Process)
    (let ((makers (hashtable-ref tag-makers tag '())))
      (and (= (length makers) 1) (lazy? (car makers)) (car makers)))))
;; tag -> the files with a (make-jhost "TAG" …) / (make-jutil-iterator "TAG" …)
(define tag-makers (make-hashtable string-hash string=?))
(define (note-tag-makers! f)
  (guard (e (#t #f))
    (let ((p (open-input-file f)))
      (let loop ()
        (let ((x (read p)))
          (unless (eof-object? x)
            (let walk ((x x))
              (when (pair? x)
                (when (and (memq (car x) '(make-jhost make-jutil-iterator))
                           (pair? (cdr x)) (string? (cadr x)))
                  (hashtable-update! tag-makers (cadr x) (lambda (fs) (if (member f fs) fs (cons f fs))) '()))
                (walk (car x)) (walk (cdr x))))
            (loop))))
      (close-port p))))
(for-each note-tag-makers! (dedupe (map (lambda (e) (vector-ref e 0)) file-log)))
(for-each (lambda (e)
            (let ((path (vector-ref e 0)) (b (vector-ref e 1)) (a (vector-ref e 2)))
              (for-each (lambda (t) (unless (member t (vector-ref b 10))
                                      (unless (hashtable-ref owner (cons 'tag t) #f)
                                        (hashtable-set! owner (cons 'tag t) path))))
                        (vector-ref a 10))))
          file-log)

;; statics keys are (class . member); a class is "mine" when this file created
;; its table — read it off the class-statics key set
(define class-owner (make-hashtable string-hash string=?))
(for-each (lambda (e)
            (let ((path (vector-ref e 0)) (b (vector-ref e 1)) (a (vector-ref e 2)))
              (for-each (lambda (c) (unless (or (member c (vector-ref b 9)) (hashtable-ref class-owner c #f))
                                      (hashtable-set! class-owner c path)))
                        (vector-ref a 9))))
          file-log)
(define (classes-of* path)
  (sorted-strings
    (append (filter (lambda (c) (equal? (hashtable-ref class-owner c #f) path))
                    (map car (keys-owned-by 'statics path)))
            (keys-owned-by 'ctor path)
            (keys-owned-by 'enum path))))
(define (statics-of* path)
  (list-sort (lambda (a b) (string<? (string-append (car a) "#" (cadr a)) (string-append (car b) "#" (cadr b))))
             (dedupe
               (filter values
                 (map (lambda (cm)
                        (and (not (equal? (hashtable-ref class-owner (car cm) #f) path))
                             (list (car cm) (cdr cm))))
                      (keys-owned-by 'statics path))))))
(define (methods-of path)       ; members on a tag another file owns
  (sorted-strings
    (filter values
      (map (lambda (tm) (and (not (equal? (tag-owner (car tm)) path)) (cdr tm)))
           (keys-owned-by 'method path)))))
(define (tags-of path)          ; the tags only it makes values of
  (sorted-strings
    (filter (lambda (t) (equal? (tag-owner t) path))
            (dedupe (append (keys-of 'host-methods-tbl) (keys-of 'jhost-tag->fqn))))))
(define (supers-of path)        ; parents this file added, per class, in order
  (let* ((e (entry-of path))
         (b (pair-table (vector-ref (vector-ref e 1) 6) equal?)))
    (list-sort (lambda (x y) (string<? (car x) (car y)))
      (filter (lambda (r) (pair? (cdr r)))
        (map (lambda (p)
               (let ((old (hashtable-ref b (car p) '())))
                 (cons (car p) (filter (lambda (s) (not (member s old))) (cdr p)))))
             (vector-ref (vector-ref e 2) 6))))))
(define (defs-of path)          ; the top-level names this file bound
  (let* ((e (entry-of path)) (b (vector-ref (vector-ref e 1) 8)) (a (vector-ref (vector-ref e 2) 8)))
    (filter (lambda (s) (not (hashtable-ref b s #f))) (vector->list (hashtable-keys a)))))

;; ---- overrides -------------------------------------------------------------------
(let-values (((ks vs) (hashtable-entries owner)))
  (vector-for-each
    (lambda (k v)
      ;; v is (writer . first-owner). Loading a lazy file late flips its order
      ;; with every other writer of the key, whichever of the two came first.
      (when (and (pair? k) (eq? (car k) 'override)
                 (or (lazy? (car v)) (and (cdr v) (lazy? (cdr v)))))
        (let* ((what (cdr k)) (kind (car what)) (key (cdr what))
               (lazy-one (if (lazy? (car v)) (car v) (cdr v))))
          ;; a lazy file replacing a member on one of its OWN tags is its own business
          (unless (and (eq? kind 'method) (lazy? (car v)) (equal? (tag-owner (car key)) (car v)))
            (violate! "override" lazy-one
                      (format "~a ~s is registered by both ~a and ~a" kind key (or (cdr v) "an earlier file") (car v)))))))
    ks vs))

;; Two arms can swap safely when no value satisfies both predicates. The pool is
;; what the arms on these chains are about: every java.util class built with no
;; arguments (the lazy files' own among them) and the Clojure values. A chain whose
;; elements are not (predicate . handler) pairs cannot be checked this way, so any
;; reorder on it is reported.
(define (added-arm-objs e chain)
  (let* ((b (let ((p (assq chain (vector-ref (vector-ref e 1) 7)))) (if p (cdr p) '())))
         (a (let ((p (assq chain (vector-ref (vector-ref e 2) 7)))) (if p (cdr p) '()))))
    (filter (lambda (x) (not (memq x b))) a)))
(define arm-samples
  (let ((ctors (filter (lambda (c) (let ((n (car c)))
                                     (and (> (string-length n) 10)
                                          (string=? (substring n 0 10) "java.util.")
                                          (not (member n '("java.util.Timer" "java.util.Scanner"))))))
                       (flat-of 'class-ctors-tbl))))
    (append
      (filter values
        (map (lambda (c) (guard (e (#t #f)) (let ((v ((cdr c)))) (and v (cons (car c) v))))) ctors))
      (list (cons "nil" jolt-nil) (cons "long" 1) (cons "string" "s") (cons "char" #\c)
            (cons "keyword" (keyword #f "k")) (cons "list" (jolt-list 1 2)) (cons "vector" (jolt-vector 1 2))
            (cons "map" (jolt-hash-map 1 2)) (cons "set" (jolt-hash-set 1 2))
            (cons "empty-list" jolt-empty-list)))))
(define (arm-pred x) (and (pair? x) (procedure? (car x)) (car x)))
(define (arms-overlap mine theirs)
  (let ((ps (map arm-pred mine)) (qs (map arm-pred theirs)))
    (if (or (memq #f ps) (memq #f qs))
        "values it cannot sample (an arm that is not a predicate pair)"
        (let loop ((vs arm-samples))
          (cond
            ((null? vs) #f)
            ((and (exists (lambda (p) (guard (e (#t #f)) (p (cdar vs)))) ps)
                  (exists (lambda (q) (guard (e (#t #f)) (q (cdar vs)))) qs))
             (caar vs))
            (else (loop (cdr vs))))))))

;; ---- arm order -----------------------------------------------------------------------
;; For each chain: the arms each file added, front (newest-first) or back (-last).
;; Loading a lazy file late moves its front arms ahead of, and its back arms behind,
;; every arm a later file added to the same end.
(define (added-arms e chain)
  (let* ((b (let ((p (assq chain (vector-ref (vector-ref e 1) 7)))) (if p (cdr p) '())))
         (a (let ((p (assq chain (vector-ref (vector-ref e 2) 7)))) (if p (cdr p) '())))
         (new (filter (lambda (x) (not (memq x b))) a)))
    ;; an added arm is at the front if it precedes every old one
    (map (lambda (x) (if (and (pair? b) (memq (car b) (memq x a))) 'front 'back)) new)))
(define chain-names (map car (vector-ref (vector-ref (car (last-pair file-log)) 2) 7)))
(for-each
  (lambda (path)
    (let ((pos (let loop ((l file-log) (i 0)) (if (string=? (vector-ref (car l) 0) path) i (loop (cdr l) (+ i 1))))))
      (for-each
        (lambda (chain)
          (let ((mine (added-arms (list-ref file-log pos) chain)))
            (when (pair? mine)
              (let loop ((l (list-tail file-log (+ pos 1))))
                (unless (null? l)
                  (let* ((e (car l)) (other (vector-ref e 0)))
                    (unless (or (lazy? other) (member other parents)
                                (member (list path chain other) lazy-shims-arm-allow))
                      (let ((theirs (added-arms e chain)))
                        (when (or (and (memq 'front mine) (memq 'front theirs))
                                  (and (memq 'back mine) (memq 'back theirs)))
                          (let ((v (arms-overlap (added-arm-objs (list-ref file-log pos) chain)
                                                 (added-arm-objs e chain))))
                            (when v
                              (violate! "arm-order" path
                                        (format "~a: its arms and ~a's both answer ~a, and loading it late reorders them"
                                                chain other v))))))))
                  (loop (cdr l)))))))
        chain-names)))
  lazy-paths)

;; ---- references from other files ---------------------------------------------------
(define (file-symbols path)
  (let ((h (make-eq-hashtable)))
    (guard (e (#t (violate! "read" path (condition-message* e))))
      (let ((p (open-input-file path)))
        (let loop ()
          (let ((x (read p)))
            (unless (eof-object? x)
              (let walk ((x x))
                (cond ((symbol? x) (hashtable-set! h x #t))
                      ((pair? x) (walk (car x)) (walk (cdr x)))
                      ((vector? x) (vector-for-each walk x))))
              (loop))))
        (close-port p)))
    h))
(define (condition-message* e)
  (if (message-condition? e) (condition-message e) (format "~s" e)))
;; a keyword, not a variable (top-level-syntax? answers #t for both)
(define (top-level-macro? s) (and (top-level-syntax? s) (not (top-level-bound? s))))
(define all-files (dedupe (map (lambda (e) (vector-ref e 0)) file-log)))
(define lazy-defs (map (lambda (p) (cons p (defs-of p))) lazy-paths))
(define deps (make-hashtable string-hash string=?))
(for-each
  (lambda (f)
    (let ((syms (file-symbols f)))
      (for-each
        (lambda (ld)
          (let ((used (filter (lambda (s) (hashtable-ref syms s #f)) (cdr ld))))
            (when (and (pair? used) (not (string=? f (car ld))))
              (let ((macros (filter top-level-macro? used)))
                (cond
                  ((lazy? f) (hashtable-update! deps f (lambda (ds) (if (member (car ld) ds) ds (cons (car ld) ds))) '()))
                  ;; a macro is expanded where the file is COMPILED, which for a
                  ;; boot file is before the lazy one loads: the use compiles to a
                  ;; call of an unbound variable, and stays one. No file may.
                  ((pair? macros)
                   (violate! "macro" (car ld) (format "~a uses its macro(s) ~s" f macros)))
                  ((member f lazy-shims-ref-allow) #f)
                  (else
                   (violate! "eager-ref" (car ld)
                             (format "~a names ~s" f (list-head used (min 6 (length used)))))))))))
        lazy-defs)))
  all-files)
;; the Clojure side reaches host names too (stdlib/clojure/core/async.clj names jolt-sm-*)
(define (clj-files dir)
  (let loop ((ds (list dir)) (acc '()))
    (if (null? ds) acc
        (let ((d (car ds)))
          (let-values (((subs fs)
                        (partition (lambda (x) (file-directory? (string-append d "/" x))) (directory-list d))))
            (loop (append (cdr ds) (map (lambda (x) (string-append d "/" x)) subs))
                  (append acc (map (lambda (x) (string-append d "/" x))
                                   (filter (lambda (x) (or (string-suffix? x ".clj") (string-suffix? x ".cljc"))) fs)))))))))
(define (text-tokens text)
  (let ((h (make-hashtable string-hash string=?)) (n (string-length text)))
    (let loop ((i 0) (start #f))
      (cond
        ((= i n) (when start (hashtable-set! h (substring text start n) #t)))
        ((token-char? (string-ref text i)) (loop (+ i 1) (or start i)))
        (else (when start (hashtable-set! h (substring text start i) #t)) (loop (+ i 1) #f))))
    h))
(define (token-char? c) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\! #\? #\* #\> #\< #\= #\$))))
(for-each
  (lambda (f)
    (let ((toks (text-tokens (call-with-input-file f get-string-all))))
      (for-each
        (lambda (ld)
          (for-each (lambda (s)
                      (when (hashtable-ref toks (symbol->string s) #f)
                        (violate! "eager-ref" (car ld) (format "~a names ~a" f s))))
                    (cdr ld)))
        lazy-defs)))
  (append (clj-files "jolt-core") (clj-files "stdlib")))

;; ---- objects of a lazy tag made elsewhere -----------------------------------------------
;; A file that builds an object of a lazy file's tag hands out a value whose
;; methods are not there until something else loads that file. Look for the
;; direct constructions: a tag string as the first argument of make-jhost /
;; make-jutil-iterator.
(define lazy-owned-tags
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (t) (let ((o (tag-owner t))) (when (and o (lazy? o)) (hashtable-set! h t o))))
              (keys-of 'host-methods-tbl))
    h))
(for-each
  (lambda (f)
    (unless (or (lazy? f) (member f lazy-shims-ref-allow))
      (guard (e (#t #f))
        (let ((p (open-input-file f)))
          (let loop ()
            (let ((x (read p)))
              (unless (eof-object? x)
                (let walk ((x x))
                  (when (pair? x)
                    (when (and (memq (car x) '(make-jhost make-jutil-iterator))
                               (pair? (cdr x)) (string? (cadr x))
                               (hashtable-ref lazy-owned-tags (cadr x) #f))
                      (violate! "lazy-tag" (hashtable-ref lazy-owned-tags (cadr x) #f)
                                (format "~a builds a ~s, whose methods only it registers" f (cadr x))))
                    (walk (car x)) (walk (cdr x))))
                (loop))))
          (close-port p)))))
  all-files)

;; ---- a lazy class reached by name from elsewhere ----------------------------------------
(define lazy-class-names
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (p) (for-each (lambda (c) (hashtable-set! h c p)) (classes-of* p))) lazy-paths)
    h))
(for-each
  (lambda (f)
    (unless (lazy? f)
      (guard (e (#t #f))
        (let ((p (open-input-file f)) (seen (make-hashtable string-hash string=?)))
          (let loop ()
            (let ((x (read p)))
              (unless (eof-object? x)
                (let walk ((x x))
                  (cond ((string? x)
                         (let ((owner (hashtable-ref lazy-class-names x #f)))
                           (when (and owner (not (name-allowed? f x)) (not (hashtable-ref seen x #f)))
                             (hashtable-set! seen x #t)
                             (violate! "eager-name" owner (format "~a names its class ~s" f x)))))
                        ((pair? x) (walk (car x)) (walk (cdr x)))
                        ((vector? x) (vector-for-each walk x))))
                (loop))))
          (close-port p)))))
  all-files)

;; ---- the lazy boot ------------------------------------------------------------------
;; The same files, but each lazy one loaded on reference — all of them, after the
;; boot. Run in a child (one Chez cannot boot twice) and compare shapes.
(define (chez-exe)
  (or (let ((c (getenv "CHEZ"))) (and c (> (string-length c) 0) c))
      (let ((d ".cache/local"))
        (and (file-directory? d)
             (let ((hit (find (lambda (x) (and (> (string-length x) 11) (string=? (substring x 0 11) "chezscheme-")
                                               (file-exists? (string-append d "/" x "/bin/scheme"))))
                              (directory-list d))))
               (and hit (string-append d "/" hit "/bin/scheme")))))
      "scheme"))
(let* ((tmp (format "/tmp/jolt-lazy-digest-~a" (random 1000000000)))
       (rc (system (format "'~a' --script tools/lazy-shims-gen.ss --lazy-digest '~a'" (chez-exe) tmp))))
  (if (not (= rc 0))
      (violate! "lazy-boot" "(all)" (format "the lazy boot failed (exit ~a)" rc))
      (let* ((lazy-lines (call-with-input-file tmp
                           (lambda (i) (let loop ((acc '()))
                                         (let ((l (get-line i))) (if (eof-object? l) (reverse acc) (loop (cons l acc))))))))
             (eager-lines (digest))
             (lh (make-hashtable string-hash string=?))
             (eh (make-hashtable string-hash string=?)))
        (delete-file tmp)
        (for-each (lambda (l) (hashtable-update! lh l (lambda (n) (+ n 1)) 0)) lazy-lines)
        (for-each (lambda (l) (hashtable-update! eh l (lambda (n) (+ n 1)) 0)) eager-lines)
        (let ((only-eager (filter (lambda (l) (not (= (hashtable-ref eh l 0) (hashtable-ref lh l 0)))) (dedupe eager-lines)))
              (only-lazy (filter (lambda (l) (= 0 (hashtable-ref eh l 0))) (dedupe lazy-lines))))
          (for-each (lambda (l) (violate! "lazy-boot" "(all)" (string-append "eager boot has, lazy boot lacks: " l)))
                    (list-head only-eager (min 15 (length only-eager))))
          (for-each (lambda (l) (violate! "lazy-boot" "(all)" (string-append "lazy boot has, eager boot lacks: " l)))
                    (list-head only-lazy (min 15 (length only-lazy))))))))

;; ---- the index -------------------------------------------------------------------------
(define (render)
  (let ((o (open-output-string)))
    (put-string o ";; lazy-shims-index.ss — GENERATED by tools/lazy-shims-gen.ss; do not edit.\n")
    (put-string o ";; What each (lazy-load …) file in rt.ss registers, read off a boot that loads\n")
    (put-string o ";; them in place: the names whose first reference loads it (lazy-shims.ss).\n")
    (put-string o ";; Regenerate with `chez --script tools/lazy-shims-gen.ss`; `make lazyshims`\n")
    (put-string o ";; fails while it is stale.\n")
    (put-string o "(lazy-shims-index!\n  '(")
    (let ((first #t))
      (for-each
        (lambda (path)
          (unless first (put-string o "\n    "))
          (set! first #f)
          (put-string o (format "(~s" path))
          (put-string o (format "\n     (deps~{ ~s~})" (sorted-strings (hashtable-ref deps path '()))))
          (put-string o (format "\n     (classes~{ ~s~})" (classes-of* path)))
          (put-string o (format "\n     (statics~{ ~s~})" (statics-of* path)))
          (put-string o (format "\n     (methods~{ ~s~})" (methods-of path)))
          (put-string o (format "\n     (tags~{ ~s~})" (tags-of path)))
          (put-string o "\n     (supers")
          (for-each (lambda (r) (put-string o (format "\n       ~s" r))) (supers-of path))
          (put-string o "))"))
        (list-sort string<? lazy-paths)))
    (put-string o "))\n")
    (get-output-string o)))

(define text (render))
(define (fail! msg) (display msg (current-error-port)) (newline (current-error-port)) (exit 1))
(when (pair? violations)
  (for-each (lambda (v) (display (string-append "lazy-shims: " v "\n") (current-error-port))) (reverse violations))
  (fail! (format "lazy-shims: ~a violation(s); a file that breaks these rules must stay (load …) — see lazy-shims-gen.ss"
                 (length violations))))
(if check?
    (let ((cur (guard (e (#t #f)) (call-with-input-file index-path get-string-all))))
      (if (equal? cur text)
          (printf "lazy-shims: index current, ~a lazy file(s)\n" (length lazy-paths))
          (fail! (string-append "lazy-shims: " index-path " is stale; regenerate with: chez --script tools/lazy-shims-gen.ss"))))
    (begin
      (call-with-output-file index-path (lambda (p) (put-string p text)) 'replace)
      (printf "lazy-shims: wrote ~a (~a lazy file(s))\n" index-path (length lazy-paths))))
