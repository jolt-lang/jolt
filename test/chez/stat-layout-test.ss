;; The struct-stat layout resolution (jolt-lang/jolt#798). Run:
;;   chez --script test/chez/stat-layout-test.ss
;;
;; nio-file reads st_mode and st_uid at offsets that are a per-platform ABI, and
;; it used to pick them from the host's IDENTITY alone. A portable-bytecode
;; build has no identity to read — its machine tag names neither OS nor arch —
;; so every pb build fell through to "unverified struct stat layout" and refused
;; getPosixFilePermissions and getOwner, on hosts whose layout is one this file
;; has always known. Native aarch64 Linux refused for the same reason.
;;
;; The layout is measured now, so the interesting case is the one this host
;; cannot be: resolution with NO identity to go on. That is what the middle
;; block does — it runs the measurement alone and requires it to land on the row
;; identity would have proposed, which is exactly what a pb build depends on.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

;; ---- the host resolves a layout at all -------------------------------------
(define resolved (nio-stat-layout))
(ok "this host resolves a struct stat layout" (and resolved #t))
(ok (format "resolved layout is ~a" (and resolved (nio-layout-name resolved)))
    (memq (and resolved (nio-layout-name resolved)) '(darwin linux-x86-64 linux-arm64)))

;; ---- measurement alone, which is the pb case -------------------------------
;; No identity input: just a real stat and the candidate table.
(define probe-buf (nio-probe-root-stat))
(ok "a root stat is available to measure with" (and probe-buf #t))
(when probe-buf
  (let ((measured (nio-sole-verifying-layout probe-buf)))
    (ok "measurement alone identifies exactly one layout" (and measured #t))
    (ok "measurement alone agrees with what identity proposes here"
        (eq? measured (nio-proposed-stat-layout)))
    (ok "measurement alone agrees with what the host actually resolved"
        (eq? measured resolved))
    ;; The discrimination has to be sharp, not lucky: every OTHER row must fail.
    (for-each
      (lambda (l)
        (unless (eq? l measured)
          (ok (format "~a does not also verify here" (nio-layout-name l))
              (not (nio-layout-verifies? l probe-buf)))))
      nio-stat-layouts)))

;; ---- and the offsets it lands on actually read the file --------------------
;; A file whose mode we just set: the layout is right only if st_mode reads back
;; the mode we chose, which no offset that merely happens to carry S_IFDIR would.
(define tmp (string-append (host-temp-dir) "/jolt-stat-layout-" (number->string (sa-real-time-ms))))
(when probe-buf
  (close-port (open-file-output-port tmp (file-options no-fail)))
  (guard (e (#t #f))
    (let ((chmod (jolt-foreign-proc-safe "chmod" '(string int) 'int)))
      (when chmod
        (chmod tmp #o640)
        (let ((mode (nio-stat-mode tmp)))
          (ok (format "st_mode of a 0640 file reads back 0640 (got ~a)"
                      (and mode (number->string (bitwise-and mode #o777) 8)))
              (and mode (= #o640 (bitwise-and mode #o777))))
          (ok "and its format bits say regular file, not directory"
              (and mode (= #x8000 (bitwise-and mode #xF000))))))))
  ;; st_uid of a file this process just created is this process's own uid.
  ;; The rest of the row is read from the attribute snapshot's one buffer: each
  ;; field has to read back what this process just made it.
  (let* ((snap (nio-snapshot tmp #t tmp #f))
         (ref (lambda (nm) (nio-snap-ref snap nm)))
         (uid (ref "uid"))
         (c-getuid (jolt-foreign-proc-safe "getuid" '() 'int))
         (c-getgid (jolt-foreign-proc-safe "getgid" '() 'int)))
    (when c-getuid
      (ok (format "st_uid of a file we created is our own uid (~a)" uid)
          (and uid (= uid (c-getuid)))))
    (when c-getgid
      (ok (format "st_gid of a file we created is our own gid or its directory's (~a)" (ref "gid"))
          (integer? (ref "gid"))))
    (ok (format "st_size of an empty file is 0 (got ~a)" (ref "size")) (eqv? 0 (ref "size")))
    (ok (format "st_nlink of a new file is 1 (got ~a)" (ref "nlink")) (eqv? 1 (ref "nlink")))
    (ok (format "st_rdev of a regular file is 0 (got ~a)" (ref "rdev")) (eqv? 0 (ref "rdev")))
    (ok "the snapshot's mode is st_mode" (eqv? (ref "mode") (nio-stat-mode tmp)))
    (ok "st_ctime is not in the future"
        (<= (file-time-ns (ref "ctime")) (* 1000000000 (+ 1 (time-second (current-time)))))))
  (let ((port (open-file-output-port tmp (file-options no-fail))))
    (put-bytevector port (string->utf8 "hello")) (close-port port))
  (ok "st_size reads back the five bytes written"
      (eqv? 5 (nio-snap-ref (nio-snapshot tmp #t tmp #f) "size")))
;; The time columns (jolt-ow0x) read back what utimes(2) wrote, each from its own
  ;; offset: the access time and the mtime are set apart, so a reader on the wrong
  ;; timespec answers the other one. The birth time is not settable everywhere;
  ;; it only has to be a time at or before now.
  (let ((c-utimes (jolt-foreign-proc-safe "utimes" '(string u8*) 'int))
        (tv (make-bytevector 32 0)))
    (when c-utimes
      (bytevector-s64-set! tv 0 1100000000 (native-endianness))
      (bytevector-s64-set! tv 8 250000 (native-endianness))
      (bytevector-s64-set! tv 16 1600000000 (native-endianness))
      (bytevector-s64-set! tv 24 0 (native-endianness))
      (c-utimes tmp tv)
      (let ((ns (lambda (nm) (let ((t (nio-read-time tmp nm #t))) (and t (file-time-ns t))))))
        (ok (format "st_atime reads back the access time (got ~a)" (ns "lastAccessTime"))
            (eqv? 1100000000250000000 (ns "lastAccessTime")))
        (ok (format "st_mtime reads back the mtime (got ~a)" (ns "lastModifiedTime"))
            (eqv? 1600000000000000000 (ns "lastModifiedTime")))
        (let ((b (ns "creationTime")))
          (ok (format "the birth time, where there is one, is not in the future (got ~a)" b)
              (or (not b) (<= b (* 1000000000 (+ 1 (time-second (current-time))))))))
        (ok "utimensat moves the access time and leaves the mtime"
            (and (nio-set-access-time! tmp 1200000000000000000 #t)
                 (eqv? 1200000000000000000 (ns "lastAccessTime"))
                 (eqv? 1600000000000000000 (ns "lastModifiedTime"))))
        ;; the timespec's nanoseconds survive both ways, and setting the mtime
        ;; leaves the access time where it was
        (ok "an mtime set to the nanosecond reads back to the nanosecond"
            (and (nio-set-mtime! tmp 1600000000123456789 #t)
                 (eqv? 1600000000123456789 (ns "lastModifiedTime"))
                 (eqv? 1200000000000000000 (ns "lastAccessTime")))))))
  ;; st_dev and st_ino: the file key of one file read twice is one key.
  (ok "the file key is (st_dev, st_ino) and stable"
      (let ((a (nio-file-key tmp #t)) (b (nio-file-key tmp #t)))
        (and (file-key? a) (equal? (jhost-state a) (jhost-state b))
             (eqv? (cdr (jhost-state a)) (nio-stat-ino tmp)))))
  (delete-file tmp))

(printf "stat-layout: ~a checks, ~a failures\n" total fails)
(when (> fails 0) (exit 1))
