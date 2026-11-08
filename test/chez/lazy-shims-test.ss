;; lazy-shims-test.ss — host shims that load on first reference (#1517).
;;
;;   chez --script test/chez/lazy-shims-test.ss
;;
;; rt.ss names some java shims (lazy-load "PATH") rather than (load "PATH"): the
;; boot leaves them out, and the first reference to anything one registers loads
;; it (lazy-shims.ss). `make lazyshims` runs tools/lazy-shims-gen.ss --check first,
;; which holds the index to the files and every lazy file to the rules a late
;; load needs; this gate holds the triggers to what the index promises, in a
;; boot that has loaded none of them.
;;
;; One boot, so each case reaches for a DIFFERENT file and checks that exactly
;; that one loaded: a trigger that loaded everything would pass a weaker test.
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (if pred (printf "PASS: ~a\n" name)
      (begin (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name))))
(define (ev s) (jolt-compile-eval s "user"))
;; Clojure equality: a vector the test builds is not equal? to one jolt returns
(define (same? a b) (eq? #t (jolt=2 a b)))
(define (st path) (let ((p (assoc (string-append "host/chez/java/" path ".ss") (lazy-shims-status)))) (and p (cdr p))))
(define units '("array-deque" "atomic-arrays" "concurrency-sync" "enum-colls" "jutil-concurrent" "process" "zip-gzip"))
(define (loaded) (filter (lambda (u) (eq? (st u) 'loaded)) units))

;; ---- 1. the boot leaves every one of them out -----------------------------------
(ok "1a. every declared lazy file is in the status table" (for-all st units))
(ok "1b. none has loaded by the end of the boot" (null? (loaded)))
;; their classes are known before they load: the class graph has their rows, and a
;; library cannot claim a class the runtime provides
(ok "1c. instance?/isa? see a lazy class before it loads"
    (eq? #t (ev "(isa? java.util.ArrayDeque java.util.Deque)")))
(ok "1d. ...without loading it" (null? (loaded)))
(ok "1e. runtime-provides-class? answers for a lazy class" (runtime-provides-class? "java.util.concurrent.Semaphore"))
(ok "1f. host-class-registered? too (the analyzer's is-it-a-class)" (host-class-registered? "java.util.PriorityQueue"))
(ok "1g. still nothing loaded" (null? (loaded)))
;; a library's :jolt/provides claim on a class a lazy file provides is dropped, as
;; it is for a class the boot registers: which implementation answers must not
;; depend on whether the file has loaded yet
(register-class-provider! "lazy.skiplist.install" #f '("java.util.concurrent.ConcurrentSkipListMap"))
(ok "1h. a :jolt/provides claim on an unloaded lazy class is dropped"
    (and (not (lib-provider-for "java.util.concurrent.ConcurrentSkipListMap"))
         (not (lib-provider-for "ConcurrentSkipListMap"))))
(ok "1i. ...without loading it" (null? (loaded)))

;; ---- 2. each kind of reference loads its file, and only its file ----------------
(ok "2a. a constructor: (java.util.ArrayDeque.)"
    (same? (ev "(let [d (java.util.ArrayDeque.)] (.addLast d 1) (.addFirst d 0) (vec d))") (ev "[0 1]")))
(ok "2b. ...loaded array-deque.ss alone" (equal? (loaded) '("array-deque")))
(ok "2c. a static on the class: EnumSet/noneOf"
    (eqv? 0 (ev "(.size (java.util.EnumSet/noneOf java.util.concurrent.TimeUnit))")))
(ok "2d. ...loaded enum-colls.ss" (equal? (loaded) '("array-deque" "enum-colls")))
(ok "2e. a static another file's class carries: Runtime/getRuntime"
    (ev "(pos? (.availableProcessors (Runtime/getRuntime)))"))
(ok "2f. ...loaded process.ss" (equal? (loaded) '("array-deque" "enum-colls" "process")))
(ok "2g. Class/forName"
    (equal? "java.util.concurrent.atomic.AtomicLongArray"
            (ev "(.getName (Class/forName \"java.util.concurrent.atomic.AtomicLongArray\"))")))
(ok "2h. ...loaded atomic-arrays.ss" (member "atomic-arrays" (loaded)))
(ok "2i. reflection on a class literal: .getConstructors"
    (ev "(pos? (alength (.getConstructors java.util.concurrent.Semaphore)))"))
(ok "2j. ...loaded concurrency-sync.ss" (member "concurrency-sync" (loaded)))
;; Several threads reaching for one unloaded class at once: one loads it, the
;; rest wait for it, and every one of them gets the class — a waiter that found
;; it loaded by someone else must look again, not report the class missing.
(ok "2j'. eight threads construct a GZIPOutputStream at once, before zip-gzip.ss loads"
    (and (eq? (st "zip-gzip") 'pending)
         (eq? #t (ev "(let [go (promise) fs (doall (for [_ (range 8)] (future @go (let [b (java.io.ByteArrayOutputStream.)] (with-open [z (java.util.zip.GZIPOutputStream. b)] (.write z (.getBytes \"hi\"))) (pos? (.size b))))))] (deliver go true) (every? true? (map deref fs)))"))))
(ok "2k. an imported short name"
    (equal? (ev "(do (import '[java.util.zip GZIPOutputStream]) (let [b (java.io.ByteArrayOutputStream.)] (with-open [z (GZIPOutputStream. b)] (.write z (.getBytes \"hi\"))) (pos? (.size b))))")
            #t))
(ok "2l. ...loaded zip-gzip.ss" (member "zip-gzip" (loaded)))
(ok "2m. jutil-concurrent.ss is still out" (eq? (st "jutil-concurrent") 'pending))
;; An image another process wrote, holding a value of a class this one has not
;; loaded: reading it loads the class's file before the value is rebuilt.
(define (chez-exe)
  (or (let ((c (getenv "CHEZ"))) (and c (> (string-length c) 0) c))
      ".cache/local/chezscheme-10.4.1/bin/scheme"))
(define img (string-append (host-temp-dir) "/jolt-lazy-shims-" (number->string (get-process-id)) ".jimg"))
(define child (string-append (host-temp-dir) "/jolt-lazy-shims-" (number->string (get-process-id)) ".ss"))
(call-with-output-file child
  (lambda (o)
    (write '(import (chezscheme)) o)
    (write '(load "host/chez/gate-boot.ss") o)
    (write `(jolt-compile-eval ,(string-append "(jolt.host/image-write! \"" img "\" (java.util.PriorityQueue. [3 1 2]))") "user") o))
  'replace)
(ok "2n. (child) an image holding a PriorityQueue is written"
    (= 0 (system (string-append "'" (chez-exe) "' --script '" child "' >/dev/null 2>&1"))))
(ok "2o. reading it rebuilds the PriorityQueue"
    (same? (ev (string-append "(let [q (jolt.host/image-read \"" img "\")] [(.getName (class q)) (.poll q) (.poll q)])"))
           (ev "[\"java.util.PriorityQueue\" 1 2]")))
(ok "2p. ...which loaded jutil-concurrent.ss" (eq? (st "jutil-concurrent") 'loaded))
(guard (e (#t #f)) (delete-file child) (delete-file img))
(ok "2q. a class it registers, by its short name through an import: Vector"
    (same? (ev "(do (import '[java.util Vector]) (let [v (Vector.)] (.add v :a) (.addElement v :b) (vec v)))")
            (ev "[:a :b]")))
(ok "2r. every lazy file has now loaded" (= (length (loaded)) (length units)))

;; ---- 3. after the load, the class is what the boot used to make it -------------
(ok "3a. a Vector's reversed() view (registered across the split views file)"
    (same? (ev "(let [v (java.util.Vector. [1 2 3])] (vec (.reversed v)))") (ev "[3 2 1]")))
(ok "3b. a LinkedList's reversed() view answers get (its methods are copied at boot)"
    (eqv? 3 (ev "(.get (.reversed (java.util.LinkedList. [1 2 3])) 0)")))
(ok "3c. an ArrayList's reversed() view"
    (eqv? 3 (ev "(.get (.reversed (java.util.ArrayList. [1 2 3])) 0)")))
(ok "3d. ConcurrentSkipListMap's sequencedKeySet (moved after the class)"
    (same? (ev "(vec (.sequencedKeySet (doto (java.util.concurrent.ConcurrentSkipListMap.) (.put 2 :b) (.put 1 :a))))")
            (ev "[1 2]")))
(ok "3e. Comparator/naturalOrder is the JDK's enum constant"
    (same? (ev "[(.name (java.util.Comparator/naturalOrder)) (.isEnum (class (java.util.Comparator/naturalOrder)))]")
            (ev "[\"INSTANCE\" true]")))

;; ---- 4. a file this program does not carry ---------------------------------------
;; What a `jolt build` program sees for a class whose shim the build left out:
;; the class names itself and says how to bring it in.
(lazy-shims-index! '(("lazy-shims-test/absent.ss" (classes "zz.lazy.Absent"))))
(lazy-load "lazy-shims-test/absent.ss")
(ok "4a. a reference to a class whose shim is absent raises NoClassDefFoundError naming the class"
    (let ((r (ev "(try (zz.lazy.Absent.) :no-error (catch Throwable e [(.getName (class e)) (.getMessage e)]))")))
      (and (pvec? r)
           (equal? (pvec-nth-d r 0 #f) "java.lang.NoClassDefFoundError")
           (let ((m (pvec-nth-d r 1 "")))
             (and (string? m)
                  (let ((has (lambda (needle) (let loop ((i 0)) (and (<= (+ i (string-length needle)) (string-length m))
                                                                    (or (string=? (substring m i (+ i (string-length needle))) needle)
                                                                        (loop (+ i 1))))))))
                    (and (has "zz.lazy.Absent") (has "lazy-shims-test/absent.ss") (has ":import"))))))))
(ok "4b. ...and stays pending, so the next reference says the same"
    (eq? 'pending (cdr (assoc "lazy-shims-test/absent.ss" (lazy-shims-status)))))
;; Class/forName of a class that is not there is a ClassNotFoundException on the
;; JVM, an Exception, so feature detection (try (Class/forName x) (catch Exception
;; _ fallback)) falls back. A constructor is the NoClassDefFoundError above.
(define (forname-absent expr)
  (ev (string-append "(try " expr " :no-error (catch Exception e [(.getName (class e)) (.getMessage e)]))")))
(define (has-all? m needles)
  (and (string? m)
       (for-all (lambda (needle)
                  (let loop ((i 0)) (and (<= (+ i (string-length needle)) (string-length m))
                                         (or (string=? (substring m i (+ i (string-length needle))) needle)
                                             (loop (+ i 1))))))
                needles)))
(ok "4c. Class/forName of a class whose shim is absent raises ClassNotFoundException, keeping the hint"
    (let ((r (forname-absent "(Class/forName (str \"zz.lazy.\" \"Absent\"))")))
      (and (pvec? r)
           (equal? (pvec-nth-d r 0 #f) "java.lang.ClassNotFoundException")
           (has-all? (pvec-nth-d r 1 "") '("zz.lazy.Absent" ":import")))))
(ok "4d. ...and so does clojure.lang.RT/classForName"
    (let ((r (forname-absent "(clojure.lang.RT/classForName (str \"zz.lazy.\" \"Absent\"))")))
      (and (pvec? r) (equal? (pvec-nth-d r 0 #f) "java.lang.ClassNotFoundException"))))
(ok "4e. the constructor is still a NoClassDefFoundError after a forName"
    (let ((r (ev "(try (zz.lazy.Absent.) :no-error (catch Throwable e (.getName (class e))))")))
      (equal? r "java.lang.NoClassDefFoundError")))

;; ---- 4'. a load another thread finished between a lookup and its trigger ---------
;; A lookup that misses and then finds the unit loaded may have looked before the
;; load landed (or, on arm64, before its writes were visible here): the first time
;; a thread finds a unit loaded, the trigger says look again; after that, a miss is
;; real. lazy-shim-inlined! stands in for the other thread's finished load.
(lazy-shims-index! '(("lazy-shims-test/done.ss" (classes "zz.lazy.Done"))))
(lazy-load "lazy-shims-test/done.ss")
(lazy-shim-inlined! "lazy-shims-test/done.ss")
(ok "4f. a thread's first sight of a unit loaded elsewhere answers look-again"
    (eq? #t (lazy-shims-class! "zz.lazy.Done")))
(ok "4g. ...and the next miss is a real one" (not (lazy-shims-class! "zz.lazy.Done")))
(lazy-shims-index! '(("lazy-shims-test/done2.ss" (classes "zz.lazy.Done2"))))
(lazy-load "lazy-shims-test/done2.ss")
(lazy-shim-inlined! "lazy-shims-test/done2.ss")
(ok "4h. each thread gets its own first sight: another thread's does not count here"
    (let ((m (make-mutex)) (c (make-condition)) (r 'unset))
      (fork-thread (lambda () (let ((v (lazy-shims-class! "zz.lazy.Done2")))
                                (with-mutex m (set! r v) (condition-broadcast c)))))
      (with-mutex m (let loop () (when (eq? r 'unset) (condition-wait c m) (loop))))
      (and (eq? r #t) (eq? #t (lazy-shims-class! "zz.lazy.Done2")))))

;; ---- 5. a record sharing a lazy class's short name ---------------------------------
;; babashka.process defines a ProcessBuilder record and builds the JDK's with
;; (java.lang.ProcessBuilder. cmd). The record's constructor is registered under
;; its short name, and a class lookup falls back to the short name; the JDK class's
;; exact name must be tried — loading its file — before that fallback, as it was
;; when the file was part of the boot. And the record must not take the short name
;; while the file is still out, or a bare (ProcessBuilder. cmd) in any other ns
;; builds the record. The bare one runs first: loading process.ss re-registers the
;; short name and would hide it. A fresh boot, so process.ss is still out.
(define child2 (string-append (host-temp-dir) "/jolt-lazy-shims-pb-" (number->string (get-process-id)) ".ss"))
(define out2 (string-append child2 ".out"))
(call-with-output-file child2
  (lambda (o)
    (write '(import (chezscheme)) o)
    (write '(load "host/chez/gate-boot.ss") o)
    (write `(call-with-output-file ,out2
              (lambda (p)
                (jolt-compile-eval "(do (ns lazy.pb) (defrecord ProcessBuilder [pb opts prev]))" "user")
                (display (jolt-compile-eval
                           "(do (in-ns 'user) (.getName (class (ProcessBuilder. [\"echo\"]))))" "user")
                         p)
                (display " " p)
                (display (jolt-compile-eval
                           "(do (in-ns 'lazy.pb) (.getName (class (java.lang.ProcessBuilder. [\"echo\"]))))" "user")
                         p))
              'replace)
           o))
  'replace)
(ok "5a. beside a ProcessBuilder record, a bare (ProcessBuilder. cmd) elsewhere and (java.lang.ProcessBuilder. cmd) build the JDK's"
    (and (= 0 (system (string-append "'" (chez-exe) "' --script '" child2 "' >/dev/null 2>&1")))
         (equal? "java.lang.ProcessBuilder java.lang.ProcessBuilder" (call-with-input-file out2 get-string-all))))
(guard (e (#t #f)) (delete-file child2) (delete-file out2))

(printf "\nlazy-shims: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
