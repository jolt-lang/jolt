;; build-scaling-test.ss — what keeps `jolt build` from scaling badly with the
;; size of the app (jolt-lang/jolt#1059).
;;
;; Profiling a test-heavy app of 233 namespaces put 70% of a 120s build in Chez's
;; back end: compile-file over the app half (44s, 62% of it collecting) and the
;; vfasl conversion (40s, superlinear in the image). This gate pins the build-side
;; shapes those costs depend on:
;;
;;   a. the app's init bodies are split into SMALL procedures. Chez's passes over
;;      one lambda body grow faster than the body; 100 forms per procedure
;;      compiled the 28MB app half in 49.6s, 10 per procedure in 31.2s
;;   b. the back-end steps run under a larger collect trip, and restore it after
;;   c. a vfasl conversion that fails outright says so — the in-process path used
;;      to keep the plain boot silently, after spending the time
;;   d. the vfasl image is converted in pieces: the runtime prefix once (cached),
;;      the app per unit. The whole-boot conversion re-imaged ~40MB that never
;;      changes, and is superlinear in the image (40s for a 28MB app half)
;;   e. the app half is one compile unit per namespace, cached on its text; a
;;      miss compiles in a worker (in parallel, in a real build)
;;   f. the require scan parses each file once, and the ns prelude stops at the
;;      ns form
;;
;;   chez --script test/chez/build-scaling-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")
(load "host/chez/cli-core.ss")
(load "host/chez/png.ss")
(load "host/chez/loader.ss")
(load "host/chez/java/ffi.ss")
(load "host/chez/build.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (if pred (printf "PASS: ~a\n" name)
      (begin (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name))))

(define tmp "target/build-scaling-gate")
(bld-mkdir-p tmp)
(define (at name) (string-append tmp "/" name))

(define (substring? needle hay)
  (let ((n (string-length needle)) (h (string-length hay)))
    (let loop ((i 0))
      (cond ((> (+ i n) h) #f)
            ((string=? needle (substring hay i (+ i n))) #t)
            (else (loop (+ i 1)))))))

(define (read-all-string s)
  (let ((ip (open-input-string s)))
    (let loop ((acc '()))
      (let ((f (read ip)))
        (if (eof-object? f) (reverse acc) (loop (cons f acc)))))))

;; --- a: init procedures stay small -------------------------------------------
(define init-forms
  (read-all-string
    (with-output-to-string
      (lambda ()
        (bld-emit-app-init (current-output-port)
                           (let loop ((i 0) (acc '()))
                             (if (= i 95) acc
                                 (loop (+ i 1) (cons (format "(display ~a)" i) acc)))))))))
(define chunk-procs
  (filter (lambda (f) (and (pair? f) (eq? (car f) 'define) (pair? (cadr f))
                           (not (eq? (car (cadr f)) 'jolt-app-init!))))
          init-forms))
(ok "every app body lands in exactly one init procedure"
    (= 95 (apply + (map (lambda (f) (length (cddr f))) chunk-procs))))
(ok "no init procedure holds more than 10 app forms"
    (for-all (lambda (f) (<= (length (cddr f)) 10)) chunk-procs))
(ok "jolt-app-init! calls every chunk, in order"
    (let ((main (find (lambda (f) (and (pair? f) (eq? (car f) 'define) (pair? (cadr f))
                                       (eq? (car (cadr f)) 'jolt-app-init!)))
                      init-forms)))
      (equal? (map car (cddr main)) (map (lambda (f) (car (cadr f))) chunk-procs))))

;; --- b: the back end's collect trip -------------------------------------------
(define before (collect-trip-bytes))
(define inside (bld-with-backend-gc (lambda () (collect-trip-bytes))))
(ok "back-end steps run under a collect trip of at least 64MB"
    (>= inside (* 64 1024 1024)))
(ok "the collect trip is restored afterwards" (= before (collect-trip-bytes)))

;; --- c: a failed in-process conversion reports itself --------------------------
(define junk (at "junk.boot"))
(let ((p (open-file-output-port junk (file-options no-fail))))
  (put-bytevector p (u8-list->bytevector '(200 201 202 203)))
  (close-port p))
(define junk-out (at "junk.vfasl"))
(define junk-result 'unset)
(define note-text
  (with-output-to-string (lambda () (set! junk-result (bld-vfasl-convert! junk junk-out)))))
(ok "an unconvertible boot answers #f" (eq? junk-result #f))
(ok "…and prints the no-vfasl note"
    (let ((needle "could not be converted to a vfasl"))
      (let loop ((i 0))
        (cond ((> (+ i (string-length needle)) (string-length note-text)) #f)
              ((string=? needle (substring note-text i (+ i (string-length needle)))) #t)
              (else (loop (+ i 1)))))))

;; --- d: split vfasl conversion ------------------------------------------------
;; The prefix (Chez's boots + the runtime unit) converts once and is cached; each
;; app unit converts on its own; the image is their concatenation. What has to
;; hold is that the result BOOTS, and that the app unit's code shares the
;; runtime unit's objects — a record made by the runtime is an instance of the
;; type the app unit's code names, across two separately converted entries.
(define csv bld-host-csv-dir)
(define (write-text! path s)
  (let ((p (open-output-file path 'replace))) (put-string p s) (close-port p)))
(define rt-ss (at "rt-unit.ss")) (define rt-so (at "rt-unit.so"))
(define app-ss (at "app-unit.ss")) (define app-so (at "app-unit.so"))
(write-text! rt-ss
  (string-append
    "(define-record-type gate-point (nongenerative gate-point-v1) (fields x))\n"
    "(define rt-made (make-gate-point 41))\n"))
(write-text! app-ss
  "(define app-says (if (gate-point? rt-made) (+ 1 (gate-point-x rt-made)) 'not-a-point))\n")
(parameterize ((optimize-level 2)) (compile-file rt-ss rt-so) (compile-file app-ss app-so))
(define units (list (list rt-ss rt-so 'runtime) (list app-ss app-so 'app)))
(define base-boots (list (string-append csv "/petite.boot") (string-append csv "/scheme.boot")))
(define rt-key (at "rt-key.so"))
(define rt-key-dir tmp)
(define (prefix-images)             ; the prefix images cached under rt-key
  (filter (lambda (f) (and (> (string-length f) 10) (string=? (substring f 0 10) "rt-key.so.")
                           (bld-suffix? f ".default.vfasl")))
          (directory-list rt-key-dir)))
(for-each (lambda (f) (delete-file (string-append rt-key-dir "/" f))) (prefix-images))
(define vboot (at "split.boot"))
(ok "a split conversion produces an image"
    (and (bld-vfasl-split! tmp base-boots units rt-key #f vboot) (file-exists? vboot)))
(ok "the runtime prefix image is cached under the runtime's key"
    (= 1 (length (prefix-images))))
(define probe (at "probe.ss"))
(write-text! probe "(display app-says)\n")
(define (boot-output boot)
  (let* ((p (process (string-append "'" bld-chez "' -b '" (current-directory) "/" boot "' --script '" probe "' 2>&1")))
         (in (car p)))
    (let loop ((acc '()))
      (let ((c (read-char in)))
        (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc)))))))
(ok "the split image boots, and app code sees the runtime's record as its own type"
    (string=? (boot-output vboot) "42"))
;; second build: the prefix comes from the cache, nothing re-converts it
(define vboot2 (at "split2.boot"))
(define real-convert sa-vfasl-convert-file)
(define conversions 0)
(set! sa-vfasl-convert-file (lambda args (set! conversions (+ conversions 1)) (apply real-convert args)))
(ok "a second build reuses the cached prefix"
    (and (bld-vfasl-split! tmp base-boots units rt-key #f vboot2)
         (= conversions 0)
         (string=? (boot-output vboot2) "42")))
(set! sa-vfasl-convert-file real-convert)
;; the prefix image is laid out for the exact kernel, and a patched Chez can keep
;; its version string, so the key reads the boot files themselves
(define fake-boot (at "fake.boot"))
(write-text! fake-boot "one kernel")
(define k1 (bld-files-key (list fake-boot)))
(write-text! fake-boot "one kerneL")
(ok "the prefix key moves with the Chez boot files' content"
    (not (string=? k1 (bld-files-key (list fake-boot)))))

;; --- e: one compile unit per namespace, cached on its text -------------------------
(define grouped (bld-group-app-strs '("a1" "a2" "b1" "c1" "c2" "c3") '(("a" . 2) ("b" . 1) ("c" . 3)) "entry"))
(ok "app strings regroup by namespace, in order"
    (equal? grouped '(("a" "a1" "a2") ("b" "b1") ("c" "c1" "c2" "c3"))))
(ok "a namespace that emitted nothing makes no unit"
    (equal? (bld-group-app-strs '("a1" "c1") '(("a" . 1) ("b" . 0) ("c" . 1)) "entry")
            '(("a" "a1") ("c" "c1"))))
(ok "strings the sizes do not account for (a shaken app) stay one unit"
    (equal? (bld-group-app-strs '("x" "y") '(("a" . 3)) "entry") '(("entry" "x" "y"))))
(ok "chunk procedures are named for their namespace, not their position"
    (let ((s (with-output-to-string
               (lambda () (bld-emit-app-chunks (current-output-port) (bld-unit-tag "my.ns-x/y?") '("(f)"))))))
      (and (substring? "jolt-app-init$my.ns-x_y_$" s) (substring? "$0!" s))))
(ok "names that differ only in rewritten characters get different chunk names"
    (let ((tags (map bld-unit-tag '("app.db?" "app.db!" "app.db_" "app.db"))))
      (and (string=? (list-ref tags 2) "app.db_") (string=? (list-ref tags 3) "app.db")
           (let distinct ((ts tags))
             (or (null? ts) (and (not (member (car ts) (cdr ts))) (distinct (cdr ts))))))))
(ok "the unit key moves with the text"
    (not (string=? (bld-unit-key "release" "(define x 1)") (bld-unit-key "release" "(define x 2)"))))
(ok "…and with the compile parameters"
    (not (string=? (bld-unit-key "release" "(define x 1)") (bld-unit-key "optimized" "(define x 1)"))))
(ok "…and is stable for the same inputs"
    (string=? (bld-unit-key "release" "(define x 1)") (bld-unit-key "release" "(define x 1)")))

;; the cache: a miss compiles and stores, a hit copies without compiling
(define ucache (at "unit-cache"))
(when (file-exists? ucache)
  (for-each (lambda (f) (delete-file (string-append ucache "/" f))) (directory-list ucache)))
(putenv "JOLT_BUILD_CACHE_DIR" ucache)
(putenv "JOLT_BUILD_JOBS" "1")
(define u1-ss (at "u1.ss")) (define u2-ss (at "u2.ss"))
(write-text! u1-ss "(define gate-u1 1)\n")
(write-text! u2-ss "(define gate-u2 2)\n")
(define uunits (list (list u1-ss (at "u1.so") 'app) (list u2-ss (at "u2.so") 'app)))
(bld-compile-app-units! tmp "release" uunits #f)
(ok "a cold build compiles every unit and caches it"
    (and (file-exists? (at "u1.so")) (file-exists? (at "u2.so"))
         (= 2 (length (filter (lambda (f) (bld-suffix? f ".so")) (directory-list ucache))))))
(delete-file (at "u1.so")) (delete-file (at "u2.so"))
(define real-compile bld-chez-compile-file)
(define compiled-again 0)
(set! bld-chez-compile-file (lambda args (set! compiled-again (+ compiled-again 1)) (apply real-compile args)))
(bld-compile-app-units! tmp "release" uunits #f)
(ok "a warm build compiles nothing and still produces every unit"
    (and (= compiled-again 0) (file-exists? (at "u1.so")) (file-exists? (at "u2.so"))))
(write-text! u2-ss "(define gate-u2 3)\n")
(bld-compile-app-units! tmp "release" uunits #f)
(ok "changing one unit recompiles that unit only" (= compiled-again 1))
(set! bld-chez-compile-file real-compile)

;; --dev must retain named locals even when the runtime's optional source-only
;; mode is active. Reuse exactly the same native unit cache after switching to
;; full: a source-only dev fasl used to poison that later build's cache hit.
(define source-info-env (getenv "JOLT_SOURCE_INFO"))
(define dev-ss (at "dev.ss"))
(define dev-so (at "dev.so"))
(define dev-text
  "(define (gate-dev-worker capture) (let ((kept (list 'kept-value))) (let ((answer (capture))) (cons kept answer))))\n")
(write-text! dev-ss dev-text)
(define dev-units (list (list dev-ss dev-so 'app)))
(define (dev-named-local?)
  (load dev-so)
  (let loop ((io (inspect/object (cdr (gate-dev-worker (lambda () (call/cc values)))))))
    (cond
      ((not (eq? (io 'type) 'continuation)) #f)
      ((equal? ((io 'code) 'name) "gate-dev-worker")
       (exists (lambda (i)
                 (let ((v (io 'ref i)))
                   (and (v 'name) (equal? ((v 'ref) 'value) '(kept-value)))))
               (iota (io 'length))))
      (else (loop (io 'link))))))
(putenv "JOLT_SOURCE_INFO" "source")
(define dev-source-key (bld-unit-key "dev" dev-text))
(bld-compile-app-units! tmp "dev" dev-units #f)
(ok "source-only override keeps named locals in dev units" (dev-named-local?))
(putenv "JOLT_SOURCE_INFO" "full")
(delete-file dev-so)
(set! compiled-again 0)
(set! bld-chez-compile-file (lambda args (set! compiled-again (+ compiled-again 1)) (apply real-compile args)))
(bld-compile-app-units! tmp "dev" dev-units #f)
(set! bld-chez-compile-file real-compile)
(ok "full dev reuses the source-override cache with named locals"
    (and (= compiled-again 0) (dev-named-local?)
         (string=? dev-source-key (bld-unit-key "dev" dev-text))))
(putenv "JOLT_SOURCE_INFO" "off")
(delete-file dev-so)
(bld-run-job! (vector dev-ss dev-so #f "dev" 'default #f))
(ok "off override keeps named locals in dev worker jobs" (dev-named-local?))
(for-each (lambda (form) (eval form))
          (read-all-string (bld-units-compile-forms dev-units "dev")))
(ok "spawned-Chez dev compile forms keep named locals" (dev-named-local?))
(putenv "JOLT_SOURCE_INFO" (or source-info-env ""))

;; The prune drops the least recently USED entries: a hit marks its entry, so an
;; entry every build reads is not evicted just for having been created first.
(define lru (at "lru-cache"))
(bld-mkdir-p lru)
(for-each (lambda (f) (delete-file (string-append lru "/" f))) (directory-list lru))
(define lru-a (string-append lru "/a.so")) (define lru-b (string-append lru "/b.so"))
(write-text! lru-a (make-string 1000 #\a))
(write-text! lru-b (make-string 1000 #\b))
(set-file-mtime-millis! lru-a 1000000000000)      ; a: created long ago
(set-file-mtime-millis! lru-b 1500000000000)      ; b: newer, never read
(ok "a cache hit marks its entry as used"
    (and (bld-cache-fetch! lru-a (at "lru-out.so"))
         (> (sa-file-mtime-ms lru-a) 1500000000000)))
(putenv "JOLT_BUILD_CACHE_DIR" lru)
(putenv "JOLT_BUILD_CACHE_MB" (number->string (/ 1500.0 1024 1024)))
(bld-prune-build-cache!)
(ok "…so over budget the prune keeps it and drops the older-used entry"
    (and (file-exists? lru-a) (not (file-exists? lru-b))))
(putenv "JOLT_BUILD_CACHE_MB" "2048")
(putenv "JOLT_BUILD_CACHE_DIR" ucache)

;; an image an earlier build left in the build dir is never taken as this build's
(write-text! (at "u1.so.vfasl") "stale bytes from an earlier build")
(bld-compile-app-units! tmp "release" uunits #t)
(ok "a unit image left over from an earlier build is not reused"
    (and (bld-vfasl-unit! (at "u1.so") (at "u1.so.vfasl"))
         (not (string=? (read-file-string (at "u1.so.vfasl")) "stale bytes from an earlier build"))))

;; a worker manifest compiles its jobs
(define wm (at "jobs.edn"))
(let ((op (open-output-file wm 'replace)))
  (write (vector u1-ss (at "w1.so") (at "w1.so.vfasl") "release" 'default #f) op)
  (close-port op))
(bld-compile-worker wm)
(ok "a worker compiles and converts each job in its manifest"
    (and (file-exists? (at "w1.so")) (file-exists? (at "w1.so.vfasl"))))

;; The parent takes any output that exists as finished and caches it, so a job
;; that dies part way must leave none: a child raising after it has written some
;; of the fasl (a heap ceiling, a backend fault) stands in for one killed mid-write.
(for-each (lambda (f) (when (file-exists? f) (delete-file f)))
          (list (at "w2.so") (at "w2.so.part") (at "w2.so.vfasl")))
(set! bld-chez-compile-file
  (lambda (mode src so) (write-text! so "half a fasl") (error 'gate "compile died part way")))
(ok "a job whose compile dies part way raises"
    (guard (e (#t #t))
      (bld-run-job! (vector u1-ss (at "w2.so") (at "w2.so.vfasl") "release" 'default #f))
      #f))
(set! bld-chez-compile-file real-compile)
(ok "…and leaves no output for the parent to take as finished"
    (not (file-exists? (at "w2.so"))))
(write-text! (at "w3.so") "not a fasl")
(when (file-exists? (at "w3.so.vfasl")) (delete-file (at "w3.so.vfasl")))
(ok "a unit image that will not convert is left unwritten, not half written"
    (and (not (sa-vfasl-convert-object-file (at "w3.so") (at "w3.so.vfasl")))
         (not (file-exists? (at "w3.so.vfasl")))))

;; --- f: each source file is parsed once by the require scan ------------------------
;; The scan read every file twice (its requires, then the classes it names), and
;; the emit's ns prelude parsed a whole file to find its first form.
(define sroot (at "scan-src"))
(bld-mkdir-p (string-append sroot "/scan"))
(write-text! (string-append sroot "/scan/a.clj")
  "(ns scan.a (:require [scan.b :as b]))\n(defn f [] (b/g))\n")
(write-text! (string-append sroot "/scan/b.clj")
  ";; leading comment\n(ns scan.b)\n(defn g [] 1)\n")
(define saved-roots (get-source-roots))
(set-source-roots!* (list sroot))
(define reads 0)
(define real-read-source ldr-read-source)
(set! ldr-read-source (lambda (f) (set! reads (+ reads 1)) (real-read-source f)))
(define closure (bld-require-closure (list "scan.a")))
(set! ldr-read-source real-read-source)
(ok "the require scan still finds the closure, deps first"
    (equal? (map car closure) '("scan.b" "scan.a")))
(ok "…reading each file once" (= reads 2))
(set-source-roots!* saved-roots)
(ok "the ns prelude reads the ns form even when it is not the first form"
    (equal? (bld-ns-prelude "scan.b" ";; c\n(def early 1)\n(ns scan.b (:require [clojure.string :as str]))\n(def x 2)\n")
            (bld-ns-prelude "scan.b" "(ns scan.b (:require [clojure.string :as str]))\n")))
(ok "…and a file with no ns form yields only the ns switch"
    (equal? (bld-ns-prelude "scan.c" "(def x 1)\n") (list "(set-chez-ns! \"scan.c\")")))

(printf "\nbuild scaling gate: ~a/~a passed~a\n"
        (- total fails) total (if (= fails 0) "" (format " (~a failed)" fails)))
(exit (if (= fails 0) 0 1))
