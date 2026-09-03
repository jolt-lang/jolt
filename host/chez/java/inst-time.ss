;; #inst values + the java.util / java.text date layer.
;;
;; A #inst literal lowers (analyzer :inst node -> emit) to (jolt-inst-from-string
;; "…"); this file parses the RFC3339 string to epoch-ms and models the value as a
;; `jinst` record (one flonum field, ms). Equality / map-key hashing are by the
;; INSTANT (offset-normalized). The overlay inst?/inst-ms read (get x :jolt/type)/(get x :ms),
;; so jolt-get answers those off a jinst — the overlay fns then work unchanged.
;;
;; This file owns the always-available java.util / java.text layer: java.util.Date,
;; java.sql.Date / Timestamp, Calendar, TimeZone, java.text.SimpleDateFormat,
;; and the UTC/GMT format-ms / parse-ms pattern engine that backs them (and HTTP
;; date headers). The java.time.* API is the jolt-lang/time base — portable Clojure
;; under stdlib/jolt/time/, autoloaded on first use (host-static.ss, RFC 0008) — so
;; it is NOT registered here; the one bridge is .toInstant, which routes a Date /
;; #inst / FileTime to the base Instant through set-instant-ctor! + mk-instant.
;; Loads LAST in rt.ss, after host-static.ss and io.ss.

;; --- civil <-> days since the Unix epoch (Howard Hinnant's algorithms) -------
;; No portable UTC mktime on Chez, so compute epoch days directly from y/m/d.
(define (days-from-civil y m d)
  (let* ((y2 (if (<= m 2) (- y 1) y))
         (era (quotient (if (>= y2 0) y2 (- y2 399)) 400))
         (yoe (- y2 (* era 400)))
         (doy (+ (quotient (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (- d 1)))
         (doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(define (civil-from-days z)            ; -> (values year month day)
  (let* ((z2 (+ z 719468))
         (era (quotient (if (>= z2 0) z2 (- z2 146096)) 146097))
         (doe (- z2 (* era 146097)))
         (yoe (quotient (+ doe (- (quotient doe 1460)) (quotient doe 36524) (- (quotient doe 146096))) 365))
         (y (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (quotient yoe 4) (- (quotient yoe 100)))))
         (mp (quotient (+ (* 5 doy) 2) 153))
         (d (+ (- doy (quotient (+ (* 153 mp) 2) 5)) 1))
         (m (+ mp (if (< mp 10) 3 -9))))
    (values (if (<= m 2) (+ y 1) y) m d)))

;; --- RFC3339 parse: yyyy[-MM[-dd[Thh[:mm[:ss[.fff]]]]]][Z|±hh:mm] -> ms -------
(define-record-type jinst (fields ms) (nongenerative chez-jinst-v1))

(define (digit? c) (and (char>=? c #\0) (char<=? c #\9)))
(define (digits-at s i n)               ; n digits from i -> integer, or #f
  (and (<= (+ i n) (string-length s))
       (let loop ((j i) (acc 0))
         (if (= j (+ i n))
             acc
             (and (digit? (string-ref s j))
                  (loop (+ j 1) (+ (* acc 10) (- (char->integer (string-ref s j)) 48))))))))

(define (jolt-inst-from-string ts0)
  ;; a leading '-' marks a negative (proleptic) year; the rest of the field may be
  ;; more than 4 digits (java.time prints -999999999-…). Read the year up to the
  ;; first '-' that separates it from the month.
  (define neg-year (and (> (string-length ts0) 0) (char=? (string-ref ts0 0) #\-)))
  (define ts (if neg-year (substring ts0 1 (string-length ts0)) ts0))
  (define len (string-length ts))
  (define (fail) (throw-jvm (quote RuntimeException) (string-append "Unrecognized #inst timestamp: " ts0)))
  (define (read-year)
    ;; >=4 digits up to a non-digit; java.time uses min-4 but allows more.
    (let loop ((j 0) (acc 0) (n 0))
      (if (and (< j len) (digit? (string-ref ts j)))
          (loop (+ j 1) (+ (* acc 10) (- (char->integer (string-ref ts j)) 48)) (+ n 1))
          (if (>= n 4) (cons acc j) #f))))
  (let* ((yr (or (read-year) (fail)))
         (year (if neg-year (- (car yr)) (car yr)))
         (i (cdr yr)) (month 1) (day 1) (hh 0) (mm 0) (ss 0) (frac-ms 0) (off-s 0))
    ;; -MM
    (when (and (< i len) (char=? (string-ref ts i) #\-) (digits-at ts (+ i 1) 2))
      (set! month (digits-at ts (+ i 1) 2)) (set! i (+ i 3)))
    ;; -dd
    (when (and (< i len) (char=? (string-ref ts i) #\-) (digits-at ts (+ i 1) 2))
      (set! day (digits-at ts (+ i 1) 2)) (set! i (+ i 3)))
    ;; Thh
    (when (and (< i len) (or (char=? (string-ref ts i) #\T) (char=? (string-ref ts i) #\t))
               (digits-at ts (+ i 1) 2))
      (set! hh (digits-at ts (+ i 1) 2)) (set! i (+ i 3))
      ;; :mm
      (when (and (< i len) (char=? (string-ref ts i) #\:) (digits-at ts (+ i 1) 2))
        (set! mm (digits-at ts (+ i 1) 2)) (set! i (+ i 3))
        ;; :ss
        (when (and (< i len) (char=? (string-ref ts i) #\:) (digits-at ts (+ i 1) 2))
          (set! ss (digits-at ts (+ i 1) 2)) (set! i (+ i 3))
          ;; .fff (truncate beyond 3)
          (when (and (< i len) (char=? (string-ref ts i) #\.))
            (let loop ((j (+ i 1)) (k 0) (acc 0))
              (if (and (< j len) (digit? (string-ref ts j)))
                  (loop (+ j 1) (+ k 1) (if (< k 3) (+ (* acc 10) (- (char->integer (string-ref ts j)) 48)) acc))
                  (begin
                    (set! frac-ms (* acc (expt 10 (max 0 (- 3 k)))))
                    (set! i j))))))))
    ;; offset Z | ±hh:mm
    (when (< i len)
      (let ((c (string-ref ts i)))
        (cond
          ((or (char=? c #\Z) (char=? c #\z)) (set! i (+ i 1)))
          ((or (char=? c #\+) (char=? c #\-))
           (let ((oh (digits-at ts (+ i 1) 2)) (om (digits-at ts (+ i 4) 2)))
             (unless (and oh om (char=? (string-ref ts (+ i 3)) #\:)) (fail))
             (set! off-s (* (if (char=? c #\-) -1 1) (+ (* oh 3600) (* om 60))))
             (set! i (+ i 6))))
          (else (fail)))))
    (unless (= i len) (fail))
    (let ((base-s (+ (* (days-from-civil year month day) 86400) (* hh 3600) (* mm 60) ss)))
      (make-jinst (- (+ (* base-s 1000) frac-ms) (* off-s 1000))))))

;; --- canonical print form: yyyy-MM-ddThh:mm:ss.fff-00:00 (UTC) ---------------
(define (pad2 n) (if (< n 10) (string-append "0" (number->string n)) (number->string n)))
(define (pad4 n) (if (< n 0)
    (string-append "-" (pad4 (- n)))
    (let ((s (number->string n))) (string-append (make-string (max 0 (- 4 (string-length s))) #\0) s))))
(define (pad3 n) (let ((s (number->string n))) (string-append (make-string (max 0 (- 3 (string-length s))) #\0) s)))
(define (inst-floor-div a b) (let ((q (quotient a b)) (r (remainder a b))) (if (and (not (= r 0)) (< (* a b) 0)) (- q 1) q)))
(define (inst-floor-mod a b) (- a (* (inst-floor-div a b) b)))

(define (inst-fields ms)                ; -> list (y mo d hh mm ss frac dow)
  (let* ((total-s (inst-floor-div (exact (truncate ms)) 1000))
         (frac (- (exact (truncate ms)) (* total-s 1000)))
         (days (inst-floor-div total-s 86400))
         (sod (inst-floor-mod total-s 86400))
         (hh (quotient sod 3600)) (mm (quotient (remainder sod 3600) 60)) (ss (remainder sod 60))
         (dow (inst-floor-mod (+ days 4) 7)))   ; 1970-01-01 = Thursday; 0=Sunday
    (call-with-values (lambda () (civil-from-days days))
      (lambda (y mo d) (list y mo d hh mm ss frac dow)))))

(define (inst-rfc3339 inst)
  (let ((f (inst-fields (jinst-ms inst))))
    (string-append (pad4 (list-ref f 0)) "-" (pad2 (list-ref f 1)) "-" (pad2 (list-ref f 2))
                   "T" (pad2 (list-ref f 3)) ":" (pad2 (list-ref f 4)) ":" (pad2 (list-ref f 5))
                   "." (pad3 (list-ref f 6)) "-00:00")))

;; --- DateTimeFormatter pattern engine -----
;; month-names backs .parse's month-name matching only; format-time names come
;; from the :date-names extension point (sdf-date-names below).
(define month-names (vector "January" "February" "March" "April" "May" "June" "July"
                            "August" "September" "October" "November" "December"))

;; names is sdf-date-names' vector: #(months months-short days days-short), the
;; days Monday-first (CLDR order); inst-fields' dow is 0=Sunday, hence dow-mon.
;; format-ms renders UTC unless a zone (a TimeZone jhost) is given, in which
;; case the fields are that zone's clock and z / Z / X render its name and
;; offset. The zone-less form is what the HTTP date formats use.
(define (offset-string off-s style)   ; style: 'rfc822 ±hhmm | 'iso-h ±hh | 'iso-hm ±hhmm | 'iso-hcm ±hh:mm
  (let* ((sign (if (< off-s 0) "-" "+")) (a (abs off-s))
         (h (pad2 (quotient a 3600))) (m (pad2 (quotient (remainder a 3600) 60))))
    (case style
      ((rfc822 iso-hm) (string-append sign h m))
      ((iso-h) (string-append sign h))
      (else (string-append sign h ":" m)))))
(define (format-ms pattern ms names . opt)
  (let* ((zone (and (pair? opt) (car opt)))
         (off-ms (if zone (tz-offset-ms zone ms) 0))
         (off-s (quotient off-ms 1000))
         (f (inst-fields (+ ms off-ms))) (n (string-length pattern)) (out (open-output-string)))
    ;; let* — dow-mon is derived from dow, so the bindings must be sequential.
    (let* ((y (list-ref f 0)) (mo (list-ref f 1)) (d (list-ref f 2))
           (hh (list-ref f 3)) (mi (list-ref f 4)) (se (list-ref f 5)) (dow (list-ref f 7))
           (months (vector-ref names 0)) (months-short (vector-ref names 1))
           (days (vector-ref names 2)) (days-short (vector-ref names 3))
           (dow-mon (modulo (+ dow 6) 7)))
      (define (run-len i c) (let loop ((j i)) (if (and (< j n) (char=? (string-ref pattern j) c)) (loop (+ j 1)) (- j i))))
      (let loop ((i 0))
        (when (< i n)
          (let* ((c (string-ref pattern i)) (k (run-len i c)))
            (cond
              ((char=? c #\')
               (if (and (< (+ i 1) n) (char=? (string-ref pattern (+ i 1)) #\'))
                   (begin (write-char #\' out) (loop (+ i 2)))
                   (let close ((j (+ i 1)))
                     (cond ((>= j n) (loop j))
                           ((char=? (string-ref pattern j) #\') (loop (+ j 1)))
                           (else (write-char (string-ref pattern j) out) (close (+ j 1)))))))
              ((char=? c #\y) (display (if (>= k 4) (number->string y) (pad2 (modulo y 100))) out) (loop (+ i k)))
              ((char=? c #\M)
               (display (cond ((= k 1) (number->string mo)) ((= k 2) (pad2 mo))
                              ((= k 3) (vector-ref months-short (- mo 1)))
                              (else (vector-ref months (- mo 1)))) out)
               (loop (+ i k)))
              ((char=? c #\d) (display (if (= k 1) (number->string d) (pad2 d)) out) (loop (+ i k)))
              ((char=? c #\E)
               (display (vector-ref (if (>= k 4) days days-short) dow-mon) out)
               (loop (+ i k)))
              ((char=? c #\H) (display (if (= k 1) (number->string hh) (pad2 hh)) out) (loop (+ i k)))
              ((char=? c #\h)
               (let ((h12 (let ((h (modulo hh 12))) (if (= h 0) 12 h))))
                 (display (if (= k 1) (number->string h12) (pad2 h12)) out)) (loop (+ i k)))
              ((char=? c #\m) (display (if (= k 1) (number->string mi) (pad2 mi)) out) (loop (+ i k)))
              ((char=? c #\s) (display (if (= k 1) (number->string se) (pad2 se)) out) (loop (+ i k)))
              ((char=? c #\a) (display (if (< hh 12) "AM" "PM") out) (loop (+ i k)))
              ;; timezone: with no zone format-ms renders UTC and the HTTP zone is
              ;; GMT (z/zzz -> GMT, Z -> +0000, X -> Z); with one, z is its short
              ;; name, Z its RFC822 offset, X/XX/XXX its ISO offset (Z at zero).
              ((char=? c #\z) (display (if zone (tz-abbrev zone ms) "GMT") out) (loop (+ i k)))
              ((char=? c #\Z) (display (if zone (offset-string off-s 'rfc822) "+0000") out) (loop (+ i k)))
              ((char=? c #\X) (display (cond ((or (not zone) (= off-s 0)) "Z")
                                              ((= k 1) (offset-string off-s 'iso-h))
                                              ((= k 2) (offset-string off-s 'iso-hm))
                                              (else (offset-string off-s 'iso-hcm)))
                                        out)
               (loop (+ i k)))
              (else (write-char c out) (loop (+ i 1)))))))
      (get-output-string out))))

;; --- SimpleDateFormat .parse: pattern-driven parse to epoch-ms (UTC/GMT) ------
(define (month-from-name s)
  (let ((m3 (ascii-string-down (substring s 0 (min 3 (string-length s))))))
    (let loop ((i 0))
      (cond ((= i 12) #f)
            ((string=? (ascii-string-down (substring (vector-ref month-names i) 0 3)) m3) (+ i 1))
            (else (loop (+ i 1)))))))
;; parse-ms reads the civil fields as UTC unless the input carries a zone (a
;; z/Z/X directive consumed one) or a zone is given, in which case a zone-less
;; input is that zone's clock, as SimpleDateFormat.parse reads it.
(define (parse-ms pattern input . opt)
  (let ((pn (string-length pattern)) (inn (string-length input))
        (zone (and (pair? opt) (car opt))) (tz-seen #f)
        (y 1970) (mo 1) (d 1) (hh 0) (mi 0) (ss 0) (frac-ms 0) (pm 'none) (off-s 0))
    ;; a parse failure is a java.time.format.DateTimeParseException (typed, so a
    ;; (catch DateTimeParseException …) over a bad date matches), like the JVM.
    (define (pfail)
      (jolt-throw (jolt-host-throwable "java.time.format.DateTimeParseException"
                                       (string-append "unparseable date \"" input "\"") jolt-nil)))
    (define (run-len i c) (let loop ((j i)) (if (and (< j pn) (char=? (string-ref pattern j) c)) (loop (+ j 1)) (- j i))))
    ;; read up to `maxw` digits (#f = unbounded). A fixed-width field (k>=2, e.g.
    ;; HHmm) caps the read at its run length so adjacent numeric fields split.
    (define (read-digits-w ii maxw)      ; -> (val . next), pfail if none
      (let loop ((j ii) (acc 0) (n 0) (any #f))
        (if (and (< j inn) (digit? (string-ref input j)) (or (not maxw) (< n maxw)))
            (loop (+ j 1) (+ (* acc 10) (- (char->integer (string-ref input j)) 48)) (+ n 1) #t)
            (if any (cons acc j) (pfail)))))
    (define (read-digits ii) (read-digits-w ii #f))
    (define (read-alpha ii)              ; -> (str . next)
      (let loop ((j ii)) (if (and (< j inn) (char-alphabetic? (string-ref input j))) (loop (+ j 1))
                             (cons (substring input ii j) j))))
    (define (read-tz ii)                 ; consume GMT/UTC/Z or ±hh[:]mm; -> next, sets off-s
      (cond ((>= ii inn) ii)
            ((char-alphabetic? (string-ref input ii)) (set! tz-seen #t) (cdr (read-alpha ii)))
            ((or (char=? (string-ref input ii) #\+) (char=? (string-ref input ii) #\-))
             (let* ((sign (if (char=? (string-ref input ii) #\-) -1 1))
                    (oh (or (and (< (+ ii 1) inn) (digits-at input (+ ii 1) 2)) 0))
                    (has-colon (and (> oh 0) (< (+ ii 3) inn) (char=? (string-ref input (+ ii 3)) #\:)))
                    (om (if has-colon
                            (or (digits-at input (+ ii 4) 2) 0)
                            (or (digits-at input (+ ii 3) 2) 0)))
                    (end (+ ii (if (> oh 0) (if has-colon 6 (if (> om 0) 5 3)) 0))))
               (set! off-s (* sign (+ (* oh 3600) (* om 60))))
               (set! tz-seen #t)
               end))
            (else ii)))
    ;; skip a '[' optional section to just past its matching ']' (nestable;
    ;; ']' inside a '…' literal is not a delimiter). `j` is just past the '['.
    (define (skip-optional pj)
      (let loop ((j pj) (depth 0) (inq #f))
        (cond ((>= j pn) j)
              (inq (loop (+ j 1) depth (not (char=? (string-ref pattern j) #\'))))
              ((char=? (string-ref pattern j) #\') (loop (+ j 1) depth #t))
              ((char=? (string-ref pattern j) #\[) (loop (+ j 1) (+ depth 1) #f))
              ((char=? (string-ref pattern j) #\]) (if (= depth 0) (+ j 1) (loop (+ j 1) (- depth 1) #f)))
              (else (loop (+ j 1) depth #f)))))
    (let loop ((pi 0) (ii 0))
      (if (>= pi pn)
          (begin
            (when (eq? pm 'pm) (when (< hh 12) (set! hh (+ hh 12))))
            (when (eq? pm 'am) (when (= hh 12) (set! hh 0)))
            (let ((local (- (+ (* 1000 (+ (* (days-from-civil y mo d) 86400) (* hh 3600) (* mi 60) ss)) frac-ms) (* off-s 1000))))
              (make-jinst (if (and zone (not tz-seen)) (local->utc zone local) local))))
          (let ((c (string-ref pattern pi)))
            (cond
              ((char-alphabetic? c)
               (let ((k (run-len pi c)))
                 (cond
                   ((char=? c #\y) (let ((r (read-digits-w ii (if (>= k 3) #f k))))
                                     ;; 2-digit year (value < 100): JVM sliding window — 00-68 -> 20xx,
                                     ;; 69-99 -> 19xx (rfc1036 HTTP dates). A full year stays as-is.
                                     (set! y (let ((v (car r))) (if (and (= k 2) (< v 100)) (if (< v 69) (+ 2000 v) (+ 1900 v)) v)))
                                     (loop (+ pi k) (cdr r))))
                   ((char=? c #\M) (if (>= k 3)
                                       (let ((r (read-alpha ii))) (set! mo (or (month-from-name (car r)) (pfail))) (loop (+ pi k) (cdr r)))
                                       (let ((r (read-digits-w ii (if (>= k 2) k #f)))) (set! mo (car r)) (loop (+ pi k) (cdr r)))))
                   ((char=? c #\d) (let ((r (read-digits-w ii (if (>= k 2) k #f)))) (set! d (car r)) (loop (+ pi k) (cdr r))))
                   ((or (char=? c #\H) (char=? c #\h)) (let ((r (read-digits-w ii (if (>= k 2) k #f)))) (set! hh (car r)) (loop (+ pi k) (cdr r))))
                   ((char=? c #\m) (let ((r (read-digits-w ii (if (>= k 2) k #f)))) (set! mi (car r)) (loop (+ pi k) (cdr r))))
                   ((char=? c #\s) (let ((r (read-digits-w ii (if (>= k 2) k #f))))
                                     (set! ss (car r))
                                     ;; an ISO formatter (modeled here as an ss-pattern with no S
                                     ;; field) still accepts an optional fractional second; consume
                                     ;; .fff -> millis from the input. Skip when the pattern carries
                                     ;; the fraction itself (a following '.'/S handles it).
                                     (let ((j (cdr r)) (pnext (if (< (+ pi k) pn) (string-ref pattern (+ pi k)) #\nul)))
                                       (if (and (not (char=? pnext #\.)) (not (char=? pnext #\S))
                                                (< j inn) (char=? (string-ref input j) #\.)
                                                (< (+ j 1) inn) (digit? (string-ref input (+ j 1))))
                                           (let frac ((p (+ j 1)) (kk 0) (acc 0))
                                             (if (and (< p inn) (digit? (string-ref input p)))
                                                 (frac (+ p 1) (+ kk 1) (if (< kk 3) (+ (* acc 10) (- (char->integer (string-ref input p)) 48)) acc))
                                                 (begin (set! frac-ms (* acc (expt 10 (max 0 (- 3 kk))))) (loop (+ pi k) p))))
                                           (loop (+ pi k) j)))))
                   ((char=? c #\S) (let frac ((p ii) (kk 0) (acc 0))
                                     (if (and (< p inn) (< kk k) (digit? (string-ref input p)))
                                         (frac (+ p 1) (+ kk 1) (+ (* acc 10) (- (char->integer (string-ref input p)) 48)))
                                         (begin (set! frac-ms (* acc (expt 10 (max 0 (- 3 kk))))) (loop (+ pi k) p)))))
                   ((char=? c #\E) (loop (+ pi k) (cdr (read-alpha ii))))
                   ((char=? c #\a) (let ((r (read-alpha ii)))
                                     (set! pm (if (string=? (ascii-string-down (car r)) "pm") 'pm 'am))
                                     (loop (+ pi k) (cdr r))))
                   ((or (char=? c #\z) (char=? c #\Z) (char=? c #\X) (char=? c #\x) (char=? c #\V) (char=? c #\v)) (loop (+ pi k) (read-tz ii)))
                   (else (loop (+ pi k) ii)))))
              ((char=? c #\')
               (if (and (< (+ pi 1) pn) (char=? (string-ref pattern (+ pi 1)) #\'))
                   (loop (+ pi 2) (if (and (< ii inn) (char=? (string-ref input ii) #\')) (+ ii 1) ii))
                   (let lit ((pj (+ pi 1)) (ij ii))
                     (cond ((>= pj pn) (loop pj ij))
                           ((char=? (string-ref pattern pj) #\') (loop (+ pj 1) ij))
                           ((and (< ij inn) (char=? (string-ref input ij) (string-ref pattern pj))) (lit (+ pj 1) (+ ij 1)))
                           (else (pfail))))))
              ;; [ ] Java optional section: if the input is exhausted, skip the
              ;; whole section to its matching ] (absent fields keep defaults —
              ;; a bare yyyy-MM-dd yields midnight UTC); otherwise parse through.
              ((char=? c #\[) (if (>= ii inn) (loop (skip-optional (+ pi 1)) ii) (loop (+ pi 1) ii)))
              ((char=? c #\]) (loop (+ pi 1) ii))
              ;; literal: match it; a pattern space tolerates missing/extra spaces.
              ((char=? c #\space)
               (let skip ((ij ii)) (if (and (< ij inn) (char=? (string-ref input ij) #\space)) (skip (+ ij 1)) (loop (+ pi 1) ij))))
              ((and (< ii inn) (char=? (string-ref input ii) c)) (loop (+ pi 1) (+ ii 1)))
              (else (pfail))))))))

;; --- value integration: get / = / hash / pr / type / instance? --------------
(define kw-jolt-type (keyword "jolt" "type"))
(define kw-ms (keyword #f "ms"))
(define inst-type-kw (keyword "jolt" "inst"))

(register-get-arm! jinst?
  (lambda (coll k d)
    (cond ((jolt=2 k kw-jolt-type) inst-type-kw)
          ((jolt=2 k kw-ms) (jinst-ms coll))
          (else d))))

(register-eq-arm! (lambda (a b) (or (jinst? a) (jinst? b)))
                  (lambda (a b) (and (jinst? a) (jinst? b) (= (jinst-ms a) (jinst-ms b)))))

(register-hash-arm! jinst? (lambda (x) (jolt-hash (jinst-ms x))))

;; a java.util.Date is Comparable (compareTo / clojure.core compare), by epoch ms.
(register-compare-arm! (lambda (a b) (and (jinst? a) (jinst? b)))
                       (lambda (a b) (let ((x (jinst-ms a)) (y (jinst-ms b)))
                                       (cond ((< x y) -1) ((> x y) 1) (else 0)))))

;; #inst is a java.util.Date — (class x) / (type x) report that, not the internal
;; :jolt/inst tag (which print-method still dispatches on via __type-tag).
(register-class-arm! jinst? (lambda (x) "java.util.Date"))

;; java.time.Instant is the jolt-lang/time base (stdlib/jolt/time/instant.clj), an
;; opaque tagged-table with its own equality/hash/compare — core needs no arm for it.

;; java.sql.Date shim values (mk-sql-date jhosts) are equal by kind + epoch-ms.
(define (time-jhost? x) (and (jhost? x) (member (jhost-tag x) '("sql-date")) #t))
(register-eq-arm! (lambda (a b) (or (time-jhost? a) (time-jhost? b)))
                  (lambda (a b) (and (time-jhost? a) (time-jhost? b)
                                     (string=? (jhost-tag a) (jhost-tag b))
                                     (= (ms-of a) (ms-of b)))))
(register-hash-arm! time-jhost? (lambda (x) (jolt-hash (ms-of x))))

(define (inst-pr i) (string-append "#inst \"" (inst-rfc3339 i) "\""))
(register-pr-arm! jinst? inst-pr)
(register-str-render! jinst? inst-rfc3339)

(register-type-arm! jinst? (lambda (x) inst-type-kw))

;; instance? java.util.Date -> a jinst; java.time.Instant/LocalDateTime -> the
;; matching jhost tag. The instance? macro passes the class-name symbol.
(define (class-short tn) (let loop ((i (- (string-length tn) 1)))
                           (cond ((< i 0) tn) ((char=? (string-ref tn i) #\.) (substring tn (+ i 1) (string-length tn))) (else (loop (- i 1))))))
(register-instance-check-arm!
  (lambda (type-sym val)
    (let* ((full (symbol-t-name type-sym))
           (tn (class-short full)))
      (cond
        ;; a #inst / (Date.) is a java.util.Date. It is NOT a java.sql.Date or a
        ;; java.sql.Timestamp — both are SUBCLASSES on the JVM, so a plain Date is
        ;; an instance of neither. Comparing only the SHORT name made (instance?
        ;; java.sql.Date (java.util.Date. 0)) true, and Selmer's date filter tests
        ;; java.sql.Date before java.util.Date, so every date went down the
        ;; .toLocalDate branch and blew up.
        ((jinst? val) (cond ((or (string=? full "java.util.Date") (string=? full "Date")) #t)
                            ((or (string=? tn "Timestamp") (string=? full "java.sql.Date")) #f)
                            (else 'pass)))
        ;; java.time.Instant is the base library's own type (instance? handled there).
        ;; A java.sql.Date IS a java.util.Date (its superclass), but not a Timestamp.
        ((and (jhost? val) (string=? (jhost-tag val) "sql-date"))
         (cond ((or (string=? full "java.sql.Date") (string=? full "java.util.Date")
                    (string=? full "Date")) #t)
               ((string=? tn "Timestamp") #f)
               (else 'pass)))
        (else 'pass)))))

;; inst-ms* is a seed native (the overlay inst-ms reads (get x :ms), now answered).
(def-var! "clojure.core" "inst-ms*" (lambda (i) (jinst-ms i)))

;; --- java.time bridge from the #inst / java.util layer -----------------------
;; ms-of projects a core date value (a #inst, a Calendar, a java.sql.Date) to
;; epoch-ms for the java.util / java.text layer. The java.time value types are the
;; jolt-lang/time base (Clojure tagged-tables with their own arithmetic, autoloaded
;; on first use) and do not pass through here.
(define (ms-of d)
  (cond ((number? d) d)
        ((jinst? d) (jinst-ms d))
        ((and (jhost? d) (string=? (jhost-tag d) "calendar")) (cal-ms d))
        ((and (jhost? d) (string=? (jhost-tag d) "sql-date")) (vector-ref (jhost-state d) 0))
        (else (throw-jvm (quote IllegalArgumentException) (string-append "not a date value: " (jolt-final-str d))))))
;; coerce a user-supplied ms (exact or flonum) to an exact integer for storage.
(define (ms->exact ms) (exact (round ms)))
(define (now-ms) (now-millis))   ; exact ms (= JVM long); now-millis from host-static.ss

;; .toInstant on a java.util.Date / #inst / java.sql.Date / FileTime yields the
;; base java.time.Instant — ONE representation. The base (stdlib/jolt/time/instant.clj)
;; installs its epoch-nanos ctor through set-instant-ctor! when it loads; autoload
;; the base the first time the bridge is used, so .toInstant works with no
;; dependency (RFC 0008). mk-instant takes epoch-ms (its many ms-based call sites).
(define jt-instant-hook #f)
(def-var! "jolt.host" "set-instant-ctor!"
  (lambda (f) (set! jt-instant-hook (lambda (nanos) (jolt-invoke f nanos))) jolt-nil))
(define (mk-instant ms)
  (unless jt-instant-hook (load-namespace "jolt.time.base"))
  (jt-instant-hook (* (ms->exact ms) 1000000)))
;; java.util.Locale is only meaningful for formatting, which is the jolt-lang/time
;; library (DateTimeFormatter and localized names). The library owns the single
;; Locale registration; core does not carry a second one (RFC 0008). Where a core
;; ctor takes a Locale (SimpleDateFormat below), the argument is rendered to its
;; id through jolt-str-render-one and resolved via the :date-names extension
;; point — core never names the class, so referencing Locale with no dependency
;; still errors, naming the library.

;; java.util.Date / java.sql.Timestamp: #inst's classes. (Date.) = now, (Date. ms)
;; or (Date. another-date) -> a jinst (ms-of accepts a number / jinst / instant), so
;; .getTime / inst? / instance? Date|Timestamp work.
(define (date-ctor . args)
  (cond
    ((null? args) (make-jinst (now-ms)))
    ((null? (cdr args)) (make-jinst (ms->exact (ms-of (car args)))))
    ;; deprecated (Date. year-1900 month0 date [hrs min sec]): civil fields on
    ;; the default zone's clock, as the JVM reads them.
    (else
     (let* ((y  (+ 1900 (jnum->exact (list-ref args 0))))
            (mo (+ 1 (jnum->exact (list-ref args 1))))
            (d  (jnum->exact (list-ref args 2)))
            (hh (if (> (length args) 3) (jnum->exact (list-ref args 3)) 0))
            (mm (if (> (length args) 4) (jnum->exact (list-ref args 4)) 0))
            (ss (if (> (length args) 5) (jnum->exact (list-ref args 5)) 0)))
       (make-jinst (local->utc (default-timezone)
                               (* 1000 (+ (* (days-from-civil y mo d) 86400) (* hh 3600) (* mm 60) ss))))))))
(register-class-ctor! "Date" date-ctor)
(register-class-ctor! "java.util.Date" date-ctor)
(register-class-ctor! "Timestamp" date-ctor)
(register-class-ctor! "java.sql.Timestamp" date-ctor)
;; Date/from(Instant) is owned by the base java.time (stdlib/jolt/time/instant.clj):
;; it takes a java.time.Instant, so the base — which defines Instant — is loaded
;; whenever it is reachable, and it converts through (java.util.Date. ms) back to a
;; jinst here. (A Scheme copy would need ms-of to understand the base Instant, which
;; it deliberately does not; the base owns the whole java.time side.)
;; java.sql.Date: a distinct class from java.util.Date (a "sql-date" jhost over
;; epoch-ms) so a protocol extended to both routes a sql.Date to its own impl.
;; (Date. year-1900 month0 day) builds midnight of that civil date on the
;; default zone's clock; valueOf parses "yyyy-MM-dd" to the same instant, and
;; toLocalDate reads the civil date back on that clock (so the three agree).
(define (mk-sql-date ms) (make-jhost "sql-date" (vector (ms->exact ms))))
(define (sql-date-midnight y mo d)
  (mk-sql-date (local->utc (default-timezone) (* 1000 (* (days-from-civil y mo d) 86400)))))
(register-class-ctor! "java.sql.Date"
  (case-lambda
    ((ms) (mk-sql-date (ms-of ms)))   ; (Date. epoch-ms)
    ((y m d) (sql-date-midnight (+ 1900 (jnum->exact y)) (+ 1 (jnum->exact m)) (jnum->exact d)))))
(register-class-statics! "java.sql.Date"
  (list (cons "valueOf" (lambda (s) (mk-sql-date (jinst-ms (parse-ms "yyyy-MM-dd" (if (string? s) s (jolt-str-render-one s)) (default-timezone))))))))
(register-host-methods! "sql-date"
  (list (cons "getTime" (lambda (self) (ms-of self)))
        (cons "toInstant" (lambda (self) (mk-instant (ms-of self))))
        (cons "toLocalDate" (lambda (self)
                              (let ((f (date-local-fields (ms-of self))))
                                (host-static-call "java.time.LocalDate" "of" (list-ref f 0) (list-ref f 1) (list-ref f 2)))))
        (cons "toString" (lambda (self) (inst-rfc3339 (make-jinst (ms-of self)))))))

;; java.util.TimeZone: an id, and the offset that id stands for. A custom id —
;; GMT+05:30 and the forms around it — is parsed here; a named IANA zone resolves
;; through libc (tz-primitives.ss), which is also where its DST transitions come
;; from. Where there is no tzdata a named zone reads as UTC, the same answer the
;; JVM gives for an id it does not know.

;; A leading GMT / UTC / UT is a prefix only when the rest is empty or an offset.
(define (tz-strip-gmt s)
  (let ((n (string-length s)))
    (define (try k tag)
      (and (>= n k) (string-ci=? (substring s 0 k) tag)
           (or (= n k) (memv (string-ref s k) '(#\+ #\-)))
           (substring s k n)))
    (or (try 3 "GMT") (try 3 "UTC") (try 2 "UT") s)))

;; ±H | ±HH | ±HMM | ±HHMM | ±HH:MM | ±HH:MM:SS -> seconds east of UTC, else #f.
(define (tz-parse-offset s)
  (let ((n (string-length s)))
    (and (>= n 2)
         (let ((sign (case (string-ref s 0) ((#\+) 1) ((#\-) -1) (else #f))))
           (and sign
                (let loop ((i 1) (ds '()))
                  (if (= i n)
                      (let* ((d (list->vector (reverse ds))) (k (vector-length d)))
                        (define (num lo hi)
                          (let l ((j lo) (acc 0))
                            (if (= j hi) acc (l (+ j 1) (+ (* acc 10) (vector-ref d j))))))
                        (and (memv k '(1 2 3 4 6))
                             (let ((h (if (= k 3) (num 0 1) (num 0 (min k 2))))
                                   (m (cond ((= k 3) (num 1 3)) ((>= k 4) (num 2 4)) (else 0)))
                                   (sec (if (= k 6) (num 4 6) 0)))
                               (and (<= h 23) (<= m 59) (<= sec 59)
                                    (* sign (+ (* h 3600) (* m 60) sec))))))
                      (let ((c (string-ref s i)))
                        (cond ((char=? c #\:) (loop (+ i 1) ds))
                              ((and (char>=? c #\0) (char<=? c #\9))
                               (loop (+ i 1) (cons (- (char->integer c) 48) ds)))
                              (else #f))))))))))

(define (tz-custom-offset id)            ; seconds | #f when the id is a named zone
  (let ((body (tz-strip-gmt id)))
    (cond ((string=? body "") 0)
          ((string-ci=? body "Z") 0)
          (else (tz-parse-offset body)))))

(define tz-probe-jan 1767225600)         ; 2026-01-01T00:00:00Z
(define tz-probe-jul 1782864000)         ; 2026-07-01T00:00:00Z

(define (tz-offset-seconds id epoch-secs)
  (or (tz-custom-offset id) (tzp-tz-offset id epoch-secs) 0))

;; The JVM's raw offset is standard time. DST only ever adds, so the smaller of a
;; midwinter and a midsummer probe is standard time in either hemisphere.
(define (tz-raw-offset-seconds id)
  (or (tz-custom-offset id)
      (let ((a (tzp-tz-offset id tz-probe-jan)) (b (tzp-tz-offset id tz-probe-jul)))
        (and a b (min a b)))
      0))

;; state: #(id last), last = (epoch-second . offset-seconds) of the most recent
;; lookup or #f. A SimpleDateFormat formatting the same instant repeatedly (a
;; loop over one date, a log line's timestamp) reads its offset off the pair
;; instead of taking the libc probe's mutex and memo table each time; the pair
;; is one immutable cons written in one slot, so a reader on another thread
;; sees a whole (second . offset) or the previous one, never a torn mix.
(define (timezone-of id) (make-jhost "timezone" (vector (if (string? id) id (jolt-str-render-one id)) #f)))
(define (timezone? x) (and (jhost? x) (string=? (jhost-tag x) "timezone")))
(define (tz-id tz) (if (timezone? tz) (vector-ref (jhost-state tz) 0) (jolt-str-render-one tz)))
(define (tz-offset-ms tz ms)
  (let ((sec (inst-floor-div (ms->exact ms) 1000)))
    (if (timezone? tz)
        (let* ((st (jhost-state tz)) (last (vector-ref st 1)))
          (if (and last (eqv? (car last) sec))
              (* 1000 (cdr last))
              (let ((off (tz-offset-seconds (vector-ref st 0) sec)))
                (vector-set! st 1 (cons sec off))
                (* 1000 off))))
        (* 1000 (tz-offset-seconds (tz-id tz) sec)))))

;; Two TimeZones with one id are equal and hash alike, as on the JVM (the JVM
;; also compares rules, which one id fixes here). The cached-offset slot is
;; never part of the comparison.
(register-eq-arm! (lambda (a b) (or (timezone? a) (timezone? b)))
                  (lambda (a b) (and (timezone? a) (timezone? b) (string=? (tz-id a) (tz-id b)))))
(register-hash-arm! timezone? (lambda (x) (jolt-hash (tz-id x))))
(register-host-methods! "timezone"
  (list (cons "getID" (lambda (self) (tz-id self)))
        (cons "toString" (lambda (self) (tz-id self)))
        (cons "getOffset" (lambda (self ms) (tz-offset-ms self ms)))
        (cons "getRawOffset" (lambda (self) (* 1000 (tz-raw-offset-seconds (tz-id self)))))
        (cons "hasSameRules" (lambda (self o) (string=? (tz-id self) (tz-id o))))
        (cons "equals" (lambda (self o) (and (timezone? o) (string=? (tz-id self) (tz-id o)))))
        (cons "hashCode" (lambda (self) (jolt-hash (tz-id self))))
        (cons "useDaylightTime"
              (lambda (self)
                (let ((id (tz-id self)))
                  (not (= (tz-offset-seconds id tz-probe-jan)
                          (tz-offset-seconds id tz-probe-jul))))))
        (cons "inDaylightTime"
              (lambda (self d)
                (> (tz-offset-ms self (ms-of d))
                   (* 1000 (tz-raw-offset-seconds (tz-id self))))))))

;; The default zone: TZ from the process's own environment (a leading colon is
;; the POSIX file form; a zoneinfo path is read for the zone name it ends in),
;; else what a registered provider answers, else UTC. Core reads no system file
;; for this. Which zone a machine is in lives in /etc/localtime or
;; /etc/timezone, and looking there is I/O the program never asked for and a
;; layout assumption core does not make; jolt.time knows how to find it (its
;; ZoneId/systemDefault) and registers that lookup as the provider when it
;; loads, so with the library present core and java.time name one zone, and
;; without it a zone-less format is UTC. Not cached: a dumped image must not
;; bake a build machine's zone in, and this is asked on the way into a format,
;; not in a loop.
(define default-zone-provider #f)
(def-var! "jolt.host" "set-default-zone-provider!"
  (lambda (f) (set! default-zone-provider (if (jolt-nil? f) #f f)) jolt-nil))
(define (tz-env-zone)
  (define (after-zoneinfo p)
    (let ((i (substring-index "zoneinfo/" p)))
      (and i (let ((z (substring p (+ i 9) (string-length p))))
               (and (> (string-length z) 0) z)))))
  (let ((tz (getenv "TZ")))
    (and tz (> (string-length tz) 0)
         (let ((tz (if (char=? (string-ref tz 0) #\:) (substring tz 1 (string-length tz)) tz)))
           (and (> (string-length tz) 0)
                (if (char=? (string-ref tz 0) #\/) (after-zoneinfo tz) tz))))))
(define (system-tz-id)
  (or (tz-env-zone)
      (and default-zone-provider
           (let ((z (guard (e (#t #f)) (jolt-invoke default-zone-provider))))
             (and (string? z) (> (string-length z) 0) z)))
      "UTC"))
(define (default-timezone) (timezone-of (system-tz-id)))
;; The zone's short name at an instant, as SimpleDateFormat's z renders it:
;; UTC and GMT are themselves, a custom GMT±hh:mm id is its own name, a named
;; zone answers through libc ("EST", "BST"), and a zone libc cannot name
;; falls back to its id.
(define (tz-abbrev zone ms)
  (let ((id (tz-id zone)))
    (cond ((string=? id "Z") "UTC")
          ((member id '("UTC" "GMT" "UT")) id)
          ((tz-custom-offset id) id)
          ((tzp-tz-abbrev id (inst-floor-div (exact (truncate ms)) 1000)) => values)
          (else id))))
;; A local-clock reading in `zone` (a civil time taken as if it were UTC) to
;; the instant it names: subtract the offset, then re-read the offset AT that
;; instant so a reading across a DST step lands on the right side of it.
(define (local->utc zone local)
  (let* ((o1 (tz-offset-ms zone local))
         (ms1 (- local o1))
         (o2 (tz-offset-ms zone ms1)))
    (- local o2)))
(define timezone-statics
  (list (cons "getTimeZone" timezone-of)
        (cons "getDefault" (lambda () (default-timezone)))))
(register-class-statics! "TimeZone" timezone-statics)
(register-class-statics! "java.util.TimeZone" timezone-statics)

;; java.util.Calendar / GregorianCalendar: a broken-down time plus the zone it is
;; read in. Either the instant or the fields are authoritative, never both, and the
;; other is derived on demand. That is what makes
;;   (doto (GregorianCalendar. y m d h mi s) (.set Calendar/MILLISECOND n)
;;                                           (.setTimeZone tz))
;; mean what it does on the JVM: the fields are resolved in whichever zone is
;; current when the instant is finally asked for, so setting the zone last still
;; decides what the fields meant.
;; Field constants are Java's int values so .set/.get dispatch on the right field.
(define cal-ERA 0) (define cal-YEAR 1) (define cal-MONTH 2)
(define cal-WEEK_OF_YEAR 3) (define cal-WEEK_OF_MONTH 4) (define cal-DAY_OF_MONTH 5)
(define cal-DAY_OF_YEAR 6) (define cal-DAY_OF_WEEK 7) (define cal-DAY_OF_WEEK_IN_MONTH 8)
(define cal-AM_PM 9) (define cal-HOUR 10)
(define cal-HOUR_OF_DAY 11) (define cal-MINUTE 12) (define cal-SECOND 13)
(define cal-MILLISECOND 14) (define cal-ZONE_OFFSET 15) (define cal-DST_OFFSET 16)
(define (cal-ms->fields ms)            ; -> vector [y mo0 d hh mi ss frac] (MONTH 0-based, JVM)
  (let ((f (inst-fields ms)))
    (vector (list-ref f 0) (- (list-ref f 1) 1) (list-ref f 2)
            (list-ref f 3) (list-ref f 4) (list-ref f 5) (list-ref f 6))))
(define (cal-fields->ms v)
  (+ (* 1000 (+ (* (days-from-civil (vector-ref v 0) (+ 1 (vector-ref v 1)) (vector-ref v 2)) 86400)
                (* (vector-ref v 3) 3600) (* (vector-ref v 4) 60) (vector-ref v 5)))
     (vector-ref v 6)))
(define (cal-field-index fld)
  (cond ((= fld cal-YEAR) 0) ((= fld cal-MONTH) 1) ((= fld cal-DAY_OF_MONTH) 2)
        ((= fld cal-HOUR_OF_DAY) 3) ((= fld cal-MINUTE) 4) ((= fld cal-SECOND) 5)
        ((= fld cal-MILLISECOND) 6) (else #f)))
;; state: #(instant-ms | #f, field-vector | #f, TimeZone). At most one of the
;; first two is #f, and asking for it fills it in from the other.
(define (cal-zone c) (vector-ref (jhost-state c) 2))

(define (cal-ms c)
  (let ((st (jhost-state c)))
    (or (vector-ref st 0)
        (let* ((local (cal-fields->ms (vector-ref st 1)))
               (id (tz-id (vector-ref st 2)))
               ;; the offset depends on the instant and the instant on the offset,
               ;; so probe with the local reading and correct once.
               (o (* 1000 (tz-offset-seconds id (inst-floor-div local 1000))))
               (ms (- local (* 1000 (tz-offset-seconds id (inst-floor-div (- local o) 1000))))))
          (vector-set! st 0 ms)
          ms))))

(define (cal-fields c)
  (let ((st (jhost-state c)))
    (or (vector-ref st 1)
        (let* ((ms (vector-ref st 0))
               (f (cal-ms->fields (+ ms (tz-offset-ms (vector-ref st 2) ms)))))
          (vector-set! st 1 f)
          f))))

(define (cal-set-ms! c ms)
  (let ((st (jhost-state c)))
    (vector-set! st 0 (ms->exact ms)) (vector-set! st 1 #f) jolt-nil))

;; Reading or editing a field completes the calendar first, as the JVM does:
;; pending fields resolve to an instant in the current zone and are then re-derived
;; from it, so an out-of-range MONTH 13 or DAY_OF_MONTH 32 reads back normalized.
(define (cal-complete! c)
  (let ((st (jhost-state c)))
    (unless (vector-ref st 0)
      (cal-ms c)
      (vector-set! st 1 #f)))
  (cal-fields c))

;; hand back the fields for mutation: the instant they described is now stale.
(define (cal-edit-fields! c)
  (let ((f (cal-complete! c))) (vector-set! (jhost-state c) 0 #f) f))

(define (cal-days-in-month y m)          ; m is 1-12
  (- (days-from-civil (if (= m 12) (+ y 1) y) (if (= m 12) 1 (+ m 1)) 1)
     (days-from-civil y m 1)))

;; Derived fields are computed, not stored, so they cannot go stale under .set.
(define (cal-get c fld)
  (let* ((f (cal-complete! c))
         (y (vector-ref f 0)) (mo0 (vector-ref f 1))
         (d (vector-ref f 2)) (h (vector-ref f 3)))
    (cond
      ((cal-field-index fld) => (lambda (i) (vector-ref f i)))
      ((= fld cal-ERA) (if (> y 0) 1 0))
      ((= fld cal-DAY_OF_WEEK) (+ 1 (inst-floor-mod (+ (days-from-civil y (+ mo0 1) d) 4) 7)))
      ((= fld cal-DAY_OF_YEAR) (+ 1 (- (days-from-civil y (+ mo0 1) d) (days-from-civil y 1 1))))
      ((= fld cal-AM_PM) (if (< h 12) 0 1))
      ((= fld cal-HOUR) (modulo h 12))
      ((= fld cal-ZONE_OFFSET) (* 1000 (tz-raw-offset-seconds (tz-id (cal-zone c)))))
      ((= fld cal-DST_OFFSET)
       (let ((id (tz-id (cal-zone c))))
         (- (* 1000 (tz-offset-seconds id (inst-floor-div (cal-ms c) 1000)))
            (* 1000 (tz-raw-offset-seconds id)))))
      (else 0))))

;; add moves YEAR and MONTH in calendar steps, clamping the day (Jan 31 plus one
;; month is Feb 28); day-scale fields keep the wall-clock time and shift the date;
;; time-scale fields add real elapsed time, as on the JVM.
(define (cal-add-days! c n)
  (let ((f (cal-edit-fields! c))) (vector-set! f 2 (+ (vector-ref f 2) n)) jolt-nil))

(define (cal-add! c fld amount)
  (cond
    ((= fld cal-MONTH)
     (let* ((f (cal-edit-fields! c))
            (tot (+ (* 12 (vector-ref f 0)) (vector-ref f 1) amount))
            (y (inst-floor-div tot 12)) (mo0 (inst-floor-mod tot 12)))
       (vector-set! f 0 y) (vector-set! f 1 mo0)
       (vector-set! f 2 (min (vector-ref f 2) (cal-days-in-month y (+ mo0 1))))))
    ((= fld cal-YEAR)
     (let* ((f (cal-edit-fields! c)) (y (+ (vector-ref f 0) amount)))
       (vector-set! f 0 y)
       (vector-set! f 2 (min (vector-ref f 2) (cal-days-in-month y (+ (vector-ref f 1) 1))))))
    ((or (= fld cal-DAY_OF_MONTH) (= fld cal-DAY_OF_YEAR) (= fld cal-DAY_OF_WEEK))
     (cal-add-days! c amount))
    ((or (= fld cal-WEEK_OF_YEAR) (= fld cal-WEEK_OF_MONTH) (= fld cal-DAY_OF_WEEK_IN_MONTH))
     (cal-add-days! c (* 7 amount)))
    ((or (= fld cal-HOUR_OF_DAY) (= fld cal-HOUR)) (cal-set-ms! c (+ (cal-ms c) (* 3600000 amount))))
    ((= fld cal-MINUTE) (cal-set-ms! c (+ (cal-ms c) (* 60000 amount))))
    ((= fld cal-SECOND) (cal-set-ms! c (+ (cal-ms c) (* 1000 amount))))
    ((= fld cal-MILLISECOND) (cal-set-ms! c (+ (cal-ms c) amount)))
    (else jolt-nil))
  jolt-nil)

(register-host-methods! "calendar"
  (list (cons "setTime" (lambda (self d) (cal-set-ms! self (ms-of d))))
        (cons "getTime" (lambda (self) (make-jinst (cal-ms self))))
        (cons "getTimeInMillis" (lambda (self) (cal-ms self)))
        (cons "setTimeInMillis" (lambda (self ms) (cal-set-ms! self ms)))
        (cons "getTimeZone" (lambda (self) (cal-zone self)))
        (cons "setTimeZone"
              (lambda (self tz)
                (let ((st (jhost-state self)))
                  ;; the JVM invalidates the fields here, so a calendar that has an
                  ;; instant re-reads its fields in the new zone, while one still
                  ;; holding pending fields resolves them in it instead.
                  (when (vector-ref st 0) (vector-set! st 1 #f))
                  (vector-set! st 2 (if (timezone? tz) tz (timezone-of (jolt-str-render-one tz)))))
                jolt-nil))
        (cons "set"
              (lambda (self . a)
                (let ((n (length a)) (f (cal-edit-fields! self)))
                  (cond
                    ((= n 2)
                     (let ((idx (cal-field-index (jnum->exact (car a)))))
                       (when idx (vector-set! f idx (jnum->exact (cadr a))))))
                    ((memv n '(3 5 6))
                     (let ((v (map jnum->exact a)))
                       (vector-set! f 0 (list-ref v 0))
                       (vector-set! f 1 (list-ref v 1))
                       (vector-set! f 2 (list-ref v 2))
                       ;; the multi-field arities set only the fields they name —
                       ;; the JVM leaves the rest, MILLISECOND included, alone.
                       (when (>= n 5)
                         (vector-set! f 3 (list-ref v 3))
                         (vector-set! f 4 (list-ref v 4)))
                       (when (= n 6) (vector-set! f 5 (list-ref v 5)))))
                    (else (throw-jvm (quote IllegalArgumentException)
                                     "No matching method set for calendar")))
                  jolt-nil)))
        (cons "get" (lambda (self field) (cal-get self (jnum->exact field))))
        (cons "add" (lambda (self field amount) (cal-add! self (jnum->exact field) (jnum->exact amount))))
        (cons "before" (lambda (self o) (< (cal-ms self) (ms-of o))))
        (cons "after" (lambda (self o) (> (cal-ms self) (ms-of o))))))

(define (calendar-arg-zone args)
  (or (let loop ((a args)) (cond ((null? a) #f) ((timezone? (car a)) (car a)) (else (loop (cdr a)))))
      (default-timezone)))

(define (calendar-of-ms ms zone) (make-jhost "calendar" (vector (ms->exact ms) #f zone)))

;; GregorianCalendar(): now. (y month day [hour minute [second]]): those fields in
;; the default zone, with MILLISECOND 0. A TimeZone argument sets the zone; a
;; Locale argument is accepted and ignored — jolt has the one calendar system.
(define (gregorian-calendar-ctor . args)
  (if (and (pair? args) (number? (car args)))
      (let ((n (length args)))
        (when (< n 3)
          (throw-jvm (quote IllegalArgumentException)
                     "No matching ctor found for class GregorianCalendar"))
        (let ((at (lambda (i dflt) (if (> n i) (jnum->exact (list-ref args i)) dflt))))
          (make-jhost "calendar"
                      (vector #f (vector (at 0 0) (at 1 0) (at 2 1) (at 3 0) (at 4 0) (at 5 0) 0)
                              (calendar-arg-zone '())))))
      (calendar-of-ms (now-ms) (calendar-arg-zone args))))

(register-class-ctor! "GregorianCalendar" gregorian-calendar-ctor)
(register-class-ctor! "java.util.GregorianCalendar" gregorian-calendar-ctor)

(define calendar-statics
  (list (cons "getInstance" (lambda args (calendar-of-ms (now-ms) (calendar-arg-zone args))))
        (cons "ERA" cal-ERA) (cons "YEAR" cal-YEAR) (cons "MONTH" cal-MONTH)
        (cons "WEEK_OF_YEAR" cal-WEEK_OF_YEAR) (cons "WEEK_OF_MONTH" cal-WEEK_OF_MONTH)
        (cons "DATE" cal-DAY_OF_MONTH) (cons "DAY_OF_MONTH" cal-DAY_OF_MONTH)
        (cons "DAY_OF_YEAR" cal-DAY_OF_YEAR) (cons "DAY_OF_WEEK" cal-DAY_OF_WEEK)
        (cons "DAY_OF_WEEK_IN_MONTH" cal-DAY_OF_WEEK_IN_MONTH)
        (cons "AM_PM" cal-AM_PM) (cons "HOUR" cal-HOUR)
        (cons "HOUR_OF_DAY" cal-HOUR_OF_DAY) (cons "MINUTE" cal-MINUTE)
        (cons "SECOND" cal-SECOND) (cons "MILLISECOND" cal-MILLISECOND)
        (cons "ZONE_OFFSET" cal-ZONE_OFFSET) (cons "DST_OFFSET" cal-DST_OFFSET)
        (cons "AM" 0) (cons "PM" 1)
        (cons "JANUARY" 0) (cons "FEBRUARY" 1) (cons "MARCH" 2) (cons "APRIL" 3)
        (cons "MAY" 4) (cons "JUNE" 5) (cons "JULY" 6) (cons "AUGUST" 7)
        (cons "SEPTEMBER" 8) (cons "OCTOBER" 9) (cons "NOVEMBER" 10) (cons "DECEMBER" 11)
        (cons "SUNDAY" 1) (cons "MONDAY" 2) (cons "TUESDAY" 3) (cons "WEDNESDAY" 4)
        (cons "THURSDAY" 5) (cons "FRIDAY" 6) (cons "SATURDAY" 7)))
(register-class-statics! "Calendar" calendar-statics)
(register-class-statics! "java.util.Calendar" calendar-statics)
;; GregorianCalendar inherits Calendar's constants and adds the era ones.
(define gregorian-statics (append (list (cons "AD" 1) (cons "BC" 0)) calendar-statics))
(register-class-statics! "GregorianCalendar" gregorian-statics)
(register-class-statics! "java.util.GregorianCalendar" gregorian-statics)

;; java.text.SimpleDateFormat: holds a pattern and an optional locale id;
;; .setTimeZone is accepted (format-ms is UTC); .format(date) renders the date
;; per the pattern via the format-ms engine.
;;
;; Month and day names are per-locale CLDR data core does not carry beyond the
;; ROOT locale, declared as an extension point. :fallback is :default, NOT
;; :strict (the opposite of ::currency-data): the JVM's own contract for an
;; unknown locale is "fall back to ROOT", so the :default mechanism reproduces
;; it exactly, and a locale passed for a purely numeric pattern still gets an
;; answer instead of a raise. ROOT's wide names ARE the abbreviated forms (the
;; JVM renders "Mar" for MMMM at ROOT — verified, not a bug). jolt-lang/time
;; owns java.util.Locale and registers the rest (RFC 0008).
;;
;; The key is a locale ID STRING, not a Locale: core has no Locale class, and
;; rendering the argument through jolt-str-render-one reaches the library's
;; Locale (registered with a :str yielding its id) without core naming the class.
(define sdf-root-months (vector "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(define sdf-root-days (vector "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))  ; Monday-first, CLDR order
(define sdf-root-names (vector sdf-root-months sdf-root-months sdf-root-days sdf-root-days))
(let ((kw (lambda (n) (keyword #f n))))
  (jolt-register-extension-point! (kw "date-names")
    (jolt-hash-map
      (kw "key") (kw "string")
      (kw "root") ""
      (kw "fields") (jolt-hash-map (kw "months") (kw "any")
                                   (kw "months-short") (kw "any")
                                   (kw "days") (kw "any")
                                   (kw "days-short") (kw "any"))
      (kw "default") (jolt-hash-map (kw "months") (apply jolt-vector (vector->list sdf-root-months))
                                    (kw "months-short") (apply jolt-vector (vector->list sdf-root-months))
                                    (kw "days") (apply jolt-vector (vector->list sdf-root-days))
                                    (kw "days-short") (apply jolt-vector (vector->list sdf-root-days)))
      (kw "fallback") (kw "default")
      (kw "hint") "The jolt-lang/time library carries per-locale month and day names.")))

;; Project the point's map for a locale id to format-ms' names vector. A field a
;; provider set to nil (the field type is :any, so it can) falls back to ROOT's.
(define (sdf-date-names id)
  (let ((data (jolt-extension-value (keyword #f "date-names") id)))
    (let ((f (lambda (name i)
               (let* ((v (jolt-get-dispatch data (keyword #f name) jolt-nil))
                      (s (if (jolt-nil? v) jolt-nil (jolt-seq v))))
                 (if (jolt-nil? s) (vector-ref sdf-root-names i) (list->vector (seq->list s)))))))
      (vector (f "months" 0) (f "months-short" 1) (f "days" 2) (f "days-short" 3)))))

;; state: #(pattern locale-id zone). The zone is the machine's at construction,
;; as the JVM's is, and setTimeZone replaces it: format renders the instant on
;; that zone's clock and parse reads a zone-less input on it.
(define (sdf-ctor pat . rest)
  (make-jhost "sdf" (vector (if (string? pat) pat (jolt-str-render-one pat))
                            (and (pair? rest) (jolt-str-render-one (car rest)))
                            (default-timezone))))
(register-class-ctor! "SimpleDateFormat" sdf-ctor)
(register-class-ctor! "java.text.SimpleDateFormat" sdf-ctor)
(register-host-methods! "sdf"
  (list (cons "setTimeZone" (lambda (self tz) (vector-set! (jhost-state self) 2 tz) jolt-nil))
        (cons "getTimeZone" (lambda (self) (vector-ref (jhost-state self) 2)))
        (cons "setLenient" (lambda (self b) jolt-nil))
        (cons "applyPattern" (lambda (self p) (vector-set! (jhost-state self) 0 (jolt-str-render-one p)) jolt-nil))
        (cons "toPattern" (lambda (self) (vector-ref (jhost-state self) 0)))
        (cons "parse" (lambda (self s)
                        (let ((st (jhost-state self)))
                          (parse-ms (vector-ref st 0) (jolt-str-render-one s) (vector-ref st 2)))))
        (cons "format" (lambda (self d)
                         (let ((st (jhost-state self)))
                           (format-ms (vector-ref st 0) (ms-of d)
                                      (sdf-date-names (or (vector-ref st 1) ""))
                                      (vector-ref st 2)))))))

;; The civil fields of an instant on the default zone's clock.
(define (date-local-fields ms)
  (inst-fields (+ ms (tz-offset-ms (default-timezone) ms))))
;; a jinst's java.util.Date method surface (record-method-dispatch arm).
(register-method-arm! arm-priority-date
  (lambda (obj method-name rest-args)
    (cond
      ((jinst? obj)
       (cond ((string=? method-name "getTime") (jinst-ms obj))
             ;; deprecated java.util.Date accessors: the default zone's clock.
             ((string=? method-name "getYear") (- (list-ref (date-local-fields (jinst-ms obj)) 0) 1900))
             ((string=? method-name "getMonth") (- (list-ref (date-local-fields (jinst-ms obj)) 1) 1))
             ((string=? method-name "getDate") (list-ref (date-local-fields (jinst-ms obj)) 2))
             ((string=? method-name "getHours") (list-ref (date-local-fields (jinst-ms obj)) 3))
             ((string=? method-name "getMinutes") (list-ref (date-local-fields (jinst-ms obj)) 4))
             ((string=? method-name "getSeconds") (list-ref (date-local-fields (jinst-ms obj)) 5))
             ((string=? method-name "getDay") (list-ref (date-local-fields (jinst-ms obj)) 7))
             ;; minutes WEST of UTC, as the JVM signs it
             ((string=? method-name "getTimezoneOffset")
              (- (quotient (tz-offset-ms (default-timezone) (jinst-ms obj)) 60000)))
             ((string=? method-name "toInstant") (mk-instant (jinst-ms obj)))
             ((string=? method-name "toString") (inst-rfc3339 obj))
             ((string=? method-name "equals") (and (pair? (if (jolt-nil? rest-args) '() (seq->list rest-args)))
                                                   (jinst? (car (seq->list rest-args)))
                                                   (= (jinst-ms obj) (jinst-ms (car (seq->list rest-args))))))
             ((string=? method-name "before") (< (jinst-ms obj) (ms-of (car (seq->list rest-args)))))
             ((string=? method-name "after") (> (jinst-ms obj) (ms-of (car (seq->list rest-args)))))
             (else (dispatch-miss obj method-name (if (jolt-nil? rest-args) '() (seq->list rest-args))))))
      (else 'pass))))

;; Clojure's built-in data readers, so a library that merges default-data-readers
;; or binds *data-readers* (e.g. aero's reader opts) resolves #inst / #uuid.
;; Keyed by symbol, like Clojure. *data-readers* is the bindable user table.
(def-var! "clojure.core" "default-data-readers"
  (jolt-hash-map (jolt-symbol #f "inst") jolt-inst-from-string
                 (jolt-symbol #f "uuid") jolt-uuid-from-string))
(def-dynvar! "clojure.core" "*data-readers*" empty-pmap)
