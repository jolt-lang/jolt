;; gate-boot-fresh.ss — the staleness predicate for the gate boot image.
;;
;; Split out of gate-boot.ss (which loads it, then acts on it) so it can be
;; loaded and exercised on its own, without booting a runtime: this predicate
;; deciding "fresh" when it isn't would mean a gate silently testing code that is
;; no longer on disk, the one genuinely dangerous failure mode of the boot cache.
;; test/chez/gateboot-smoke.sh drives it over synthetic input lists.
;;
;; SO is fresh when it exists and every input listed in INPUTS still has the
;; CONTENT it had when the image was built. Each line is "<hash> <path>".
;; Anything unclear — no image, no list, a listed file deleted, a line in the old
;; bare-path format — is NOT fresh, so the caller falls back to source.
;;
;; The comparison was mtime: is the image newer than every input. That is not the
;; question. It answers "was the image written after this file was last touched",
;; and the two come apart exactly when a build overlaps an edit — the compiler
;; reads a file at T0, the edit lands at T1, the image is written at T2 > T1, and
;; the image is forever "newer than" a file whose content it does not contain.
;; Editing while a gate runs is ordinary, and the result was a gate testing code
;; that is no longer on disk. It cost a session: `make test` went red on a state
;; image row while CI passed on identical sources, and the image was the only
;; difference. A false GREEN is the same mechanism with the arguments swapped,
;; and that one says nothing at all.
;;
;; Content answers the real question and cannot be fooled by clock order, a
;; preserved mtime (cp -p, tar -x, rsync --times), or a checkout that restores an
;; older revision. It costs ~17ms over the ~5.4MB of a full preamble against a
;; ~220ms cached boot, which is the cache still being worth having.
;; Standalone script: self-load the runtime adapter for sa-file-mtime-ms. A
;; second load by gate-boot.ss is a harmless redefinition.
(load "host/chez/scheme-adapter-runtime.ss")

;; FNV-1a 32-bit over the file's BYTES. Bytes, not characters: an input can be
;; any encoding, and decoding one as text to hash it would raise on the first
;; file that is not. The writer of the input list uses THIS function, by loading
;; this file, so the two can never drift apart.
(define (gate-boot-content-hash path)
  (let* ((bv (call-with-port (open-file-input-port path)
               (lambda (in) (get-bytevector-all in))))
         (bv (if (eof-object? bv) (bytevector) bv))
         (n  (bytevector-length bv)))
    (let loop ((i 0) (h 2166136261))
      (if (fx=? i n)
          h
          (loop (fx+ i 1)
                (fxlogand (fx* (fxlogxor h (bytevector-u8-ref bv i)) 16777619)
                          #xFFFFFFFF))))))

(define (gate-boot-hash-string path)
  (number->string (gate-boot-content-hash path) 16))

;; "<hash> <path>" -> (hash . path); #f for any line that is not that shape,
;; which includes a list written by an older build (bare paths).
(define (gate-boot-parse-line line)
  (let loop ((i 0))
    (cond ((fx=? i (string-length line)) #f)
          ((char=? (string-ref line i) #\space)
           (and (fx>? i 0)
                (fx<? (fx+ i 1) (string-length line))
                (cons (substring line 0 i)
                      (substring line (fx+ i 1) (string-length line)))))
          (else (loop (fx+ i 1))))))

;; Never raises: a parallel rebuild can remove the list or an input between the
;; existence checks and the reads, and a gate that cannot tell is a stale image.
(define (gate-boot-image-fresh? so inputs)
  (and (file-exists? so) (file-exists? inputs)
       (guard (e (#t #f))
         (call-with-input-file inputs
           (lambda (p)
             (let loop ()
               (let ((line (get-line p)))
                 (cond ((eof-object? line) #t)
                       ((string=? line "") (loop))
                       (else
                        (let ((e (gate-boot-parse-line line)))
                          (cond ((not e) #f)          ; old format, or malformed
                                ((not (file-exists? (cdr e))) #f)
                                ((not (string=? (car e) (gate-boot-hash-string (cdr e)))) #f)
                                (else (loop)))))))))))))
