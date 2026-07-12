;; regex-translate.ss — Java regex pattern → irregex SRE translator.
;;
;; Parses a Java/Clojure regex pattern string ONCE via recursive descent and emits
;; an irregex SRE (s-expression AST). The current string-rewriting pipeline in
;; regex.ss is the fallback until V2 is proven >= V1 on all inputs.
;;
;; Loaded before regex.ss; exports (java-pattern->sre pat-string) → values(sre opts).
;; The SRE is passed directly to (irregex sre . opts), bypassing irregex's PCRE
;; string reader entirely.
;;
;; STAGE 1: skeleton — delegates to irregex's string->sre for now.
;; The recursive-descent parser is built up incrementally in Stage 2.

;; ── helpers ───────────────────────────────────────────────────────────────────

(define (hex-value c)
  (cond ((and (char<=? #\0 c) (char<=? c #\9)) (- (char->integer c) 48))
        ((and (char<=? #\a c) (char<=? c #\f)) (- (char->integer c) 87))
        ((and (char<=? #\A c) (char<=? c #\F)) (- (char->integer c) 55))
        (else #f)))

(define (string-scan-char s c start end)
  (let loop ((i start))
    (cond ((>= i end) #f)
          ((char=? (string-ref s i) c) i)
          (else (loop (+ i 1))))))

(define (flag-set? flags f) (memq f flags))

;; ── (?x) whitespace/comment stripping (from regex.ss, duplicated for standalone use)

(define (regex-x-strip s start)
  (let ((n (string-length s)) (out (open-output-string)))
    (let loop ((i start))
      (if (>= i n)
          (get-output-string out)
          (let ((c (string-ref s i)))
            (cond
             ((and (char=? c #\\) (< (+ i 1) n))
              (write-char c out) (write-char (string-ref s (+ i 1)) out)
              (loop (+ i 2)))
             ((char=? c #\#)
              (let skip ((j (+ i 1)))
                (if (>= j n) (loop j)
                    (if (char=? (string-ref s j) #\newline)
                        (loop (+ j 1))
                        (skip (+ j 1))))))
             ((memv c '(#\space #\tab #\newline #\return #\x0B #\x0C))
              (loop (+ i 1)))
             (else (write-char c out) (loop (+ i 1)))))))))

(define (apply-global-x src)
  (let ((n (string-length src)))
    (let loop ((i 0) (in-class #f))
      (if (>= (+ i 2) n)
          src
          (let ((c (string-ref src i)))
            (cond
             ((and (char=? c #\\) (< (+ i 1) n)) (loop (+ i 2) in-class))
             ((and (not in-class) (char=? c #\[)) (loop (+ i 1) #t))
             ((and in-class (char=? c #\])) (loop (+ i 1) #f))
             ((and (not in-class) (char=? c #\()
                   (char=? (string-ref src (+ i 1)) #\?))
              (let scan ((j (+ i 2)) (fs '()))
                (if (>= j n) src
                    (let ((fc (string-ref src j)))
                      (cond
                       ((memv fc '(#\s #\i #\m #\x #\u))
                        (scan (+ j 1) (cons fc fs)))
                       ((and (char=? fc #\)) (pair? fs) (memv #\x fs))
                        (let ((others (reverse (remv #\x fs))))
                          (string-append
                           (substring src 0 i)
                           (apply string-append
                                  (map (lambda (f) (string #\( #\? f #\))) others))
                           (regex-x-strip src (+ j 1)))))
                       (else (loop (+ i 1) in-class)))))))
             (else (loop (+ i 1) in-class))))))))

;; ── Leading flags ─────────────────────────────────────────────────────────────

(define (regex-flag->opt c)
  (cond ((char=? c #\s) 'single-line)
        ((char=? c #\i) 'case-insensitive)
        ((char=? c #\m) 'multi-line)
        (else #f)))

(define (parse-leading-flags src i end)
  (let loop ((i i) (opts '()))
    (if (>= (+ i 3) end)
        (values (reverse opts) i)
        (let ((c0 (string-ref src i))
              (c1 (string-ref src (+ i 1))))
          (if (and (char=? c0 #\() (char=? c1 #\?))
              (let scan ((j (+ i 2)) (fs '()))
                (if (>= j end)
                    (values (reverse opts) i)
                    (let ((c (string-ref src j)))
                      (cond
                       ((char=? c #\))
                        (let ((mapped (map regex-flag->opt fs)))
                          (if (and (pair? fs) (for-all (lambda (x) x) mapped))
                              (loop (+ j 1) (append opts mapped))
                              (values (reverse opts) i))))
                       ((char=? c #\:) (values (reverse opts) i))
                       (else (scan (+ j 1) (cons c fs)))))))
              (values (reverse opts) i))))))

;; ── Entry point ───────────────────────────────────────────────────────────────
;; Stage 1: delegates to irregex's string->sre.
;; Returns two values: (sre . opts-list)

(define (java-pattern->sre source)
  (let* ((len (string-length source))
         (source (apply-global-x source)))
    (let-values (((opts start) (parse-leading-flags source 0 len)))
      ;; For now, build the SRE by having irregex parse the (preprocessed) string.
      ;; This is a placeholder; the recursive-descent parser replaces it in Stage 2.
      (let ((pat (substring source start len)))
        ;; Build arguments for irregex string->sre: the pattern string + options
        (values `(string->sre ,pat) opts)))))
