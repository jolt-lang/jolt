;; #inst values + a java.time formatting shim.
;;
;; A #inst literal lowers (analyzer :inst node -> emit) to (jolt-inst-from-string
;; "…"); this file parses the RFC3339 string to epoch-ms and models the value as a
;; `jinst` record (one flonum field, ms). Equality / map-key hashing are by the
;; INSTANT (offset-normalized). The overlay inst?/inst-ms read (get x :jolt/type)/(get x :ms),
;; so jolt-get answers those off a jinst — the overlay fns then work unchanged.
;;
;; The java.time surface (DateTimeFormatter/Instant/ZoneId/LocalDateTime/
;; FormatStyle/Locale + the .format/.atZone/.toInstant/… methods) is
;; registered through host-static.ss's class-statics / host-
;; methods registries — so this loads LAST in rt.ss, after host-static.ss and io.ss.

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
  (define (fail) (error #f (string-append "Unrecognized #inst timestamp: " ts0)))
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
(define (pad4 n) (let ((s (number->string n))) (string-append (make-string (max 0 (- 4 (string-length s))) #\0) s)))
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
(define month-names (vector "January" "February" "March" "April" "May" "June" "July"
                            "August" "September" "October" "November" "December"))
(define day-names (vector "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(define (format-ms pattern ms)
  (let ((f (inst-fields ms)) (n (string-length pattern)) (out (open-output-string)))
    (let ((y (list-ref f 0)) (mo (list-ref f 1)) (d (list-ref f 2))
          (hh (list-ref f 3)) (mi (list-ref f 4)) (se (list-ref f 5)) (dow (list-ref f 7)))
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
                              ((= k 3) (substring (vector-ref month-names (- mo 1)) 0 3))
                              (else (vector-ref month-names (- mo 1)))) out)
               (loop (+ i k)))
              ((char=? c #\d) (display (if (= k 1) (number->string d) (pad2 d)) out) (loop (+ i k)))
              ((char=? c #\E)
               (display (if (>= k 4) (vector-ref day-names dow) (substring (vector-ref day-names dow) 0 3)) out)
               (loop (+ i k)))
              ((char=? c #\H) (display (if (= k 1) (number->string hh) (pad2 hh)) out) (loop (+ i k)))
              ((char=? c #\h)
               (let ((h12 (let ((h (modulo hh 12))) (if (= h 0) 12 h))))
                 (display (if (= k 1) (number->string h12) (pad2 h12)) out)) (loop (+ i k)))
              ((char=? c #\m) (display (if (= k 1) (number->string mi) (pad2 mi)) out) (loop (+ i k)))
              ((char=? c #\s) (display (if (= k 1) (number->string se) (pad2 se)) out) (loop (+ i k)))
              ((char=? c #\a) (display (if (< hh 12) "AM" "PM") out) (loop (+ i k)))
              ;; timezone — format-ms renders UTC, the HTTP zone is GMT: z/zzz -> GMT,
              ;; Z (RFC822) -> +0000, X (ISO) -> Z.
              ((char=? c #\z) (display "GMT" out) (loop (+ i k)))
              ((char=? c #\Z) (display "+0000" out) (loop (+ i k)))
              ((char=? c #\X) (display "Z" out) (loop (+ i k)))
              (else (write-char c out) (loop (+ i 1)))))))
      (get-output-string out))))

;; --- SimpleDateFormat .parse: pattern-driven parse to epoch-ms (UTC/GMT) ------
(define (month-from-name s)
  (let ((m3 (ascii-string-down (substring s 0 (min 3 (string-length s))))))
    (let loop ((i 0))
      (cond ((= i 12) #f)
            ((string=? (ascii-string-down (substring (vector-ref month-names i) 0 3)) m3) (+ i 1))
            (else (loop (+ i 1)))))))
(define (parse-ms pattern input)
  (let ((pn (string-length pattern)) (inn (string-length input))
        (y 1970) (mo 1) (d 1) (hh 0) (mi 0) (ss 0) (frac-ms 0) (pm 'none))
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
    (define (read-tz ii)                 ; consume GMT/UTC/Z or ±hhmm; -> next
      (cond ((>= ii inn) ii)
            ((char-alphabetic? (string-ref input ii)) (cdr (read-alpha ii)))
            ((or (char=? (string-ref input ii) #\+) (char=? (string-ref input ii) #\-))
             (let loop ((j (+ ii 1))) (if (and (< j inn) (or (digit? (string-ref input j)) (char=? (string-ref input j) #\:))) (loop (+ j 1)) j)))
            (else ii)))
    (let loop ((pi 0) (ii 0))
      (if (>= pi pn)
          (begin
            (when (eq? pm 'pm) (when (< hh 12) (set! hh (+ hh 12))))
            (when (eq? pm 'am) (when (= hh 12) (set! hh 0)))
            (make-jinst (+ (* 1000 (+ (* (days-from-civil y mo d) 86400) (* hh 3600) (* mi 60) ss)) frac-ms)))
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

;; #inst is a java.util.Date — (class x) / (type x) report that, not the internal
;; :jolt/inst tag (which print-method still dispatches on via __type-tag).
(register-class-arm! jinst? (lambda (x) "java.util.Date"))

;; java.time.Instant is nano-precise: two Instants are = when their epoch-nanos
;; match (so an Instant and one shifted by a single nanosecond differ).
(define (jt-instant-tag? x) (and (jhost? x) (string=? (jhost-tag x) "instant")))
(register-eq-arm! (lambda (a b) (or (jt-instant-tag? a) (jt-instant-tag? b)))
                  (lambda (a b) (and (jt-instant-tag? a) (jt-instant-tag? b)
                                     (= (inst-nanos a) (inst-nanos b)))))
(register-hash-arm! jt-instant-tag? (lambda (x) (jolt-hash (inst-nanos x))))

;; ZonedDateTime / java.sql.Date shim values (mk-zoned/mk-sql-date jhosts) are
;; equal when same kind + same epoch-ms.
(define (time-jhost? x) (and (jhost? x) (member (jhost-tag x) '("zoned-dt" "sql-date")) #t))
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
    (let ((tn (class-short (symbol-t-name type-sym))))
      (cond
        ;; a #inst / (Date.) is a java.util.Date; it is NOT a java.sql.Timestamp
        ;; (on the JVM a Date is not a Timestamp), so answer Timestamp explicitly #f.
        ((jinst? val) (cond ((string=? tn "Date") #t)
                            ((string=? tn "Timestamp") #f)
                            (else 'pass)))
        ((and (jhost? val) (string=? (jhost-tag val) "instant")) (if (string=? tn "Instant") #t 'pass))
        ;; java.sql.Date is a java.util.Date subclass (but not a Timestamp).
        ((and (jhost? val) (string=? (jhost-tag val) "sql-date"))
         (cond ((or (string=? tn "Date")) #t) ((string=? tn "Timestamp") #f) (else 'pass)))
        (else 'pass)))))

;; inst-ms* is a seed native (the overlay inst-ms reads (get x :ms), now answered).
(def-var! "clojure.core" "inst-ms*" (lambda (i) (jinst-ms i)))

;; --- java.time shim values (jhost objects over host-static.ss registries) -----
;; "local-date" stores an epoch-day (java-time.ss owns the type); ms-of projects it
;; to UTC midnight so existing date math keeps working. "local-dt" stores epoch-day +
;; nano-of-day; the others store epoch-ms.
(define (ms-of d)
  (cond ((number? d) d)
        ((jinst? d) (jinst-ms d))
        ((and (jhost? d) (string=? (jhost-tag d) "local-date"))
         (* (vector-ref (jhost-state d) 0) 86400000))
        ((and (jhost? d) (string=? (jhost-tag d) "local-date-time"))
         (+ (* (vector-ref (jhost-state d) 0) 86400000)
            (quotient (vector-ref (jhost-state d) 1) 1000000)))
        ;; "instant" stores epoch-nanos; project to ms (floor) for ms-based callers.
        ((and (jhost? d) (string=? (jhost-tag d) "instant"))
         (inst-floor-div (vector-ref (jhost-state d) 0) 1000000))
        ((and (jhost? d) (member (jhost-tag d) '("zoned-dt" "calendar" "sql-date")))
         (vector-ref (jhost-state d) 0))
        (else (error #f "not a date value" d))))
;; A java.time.Instant stores epoch-nanos (exact integer). mk-instant takes ms,
;; for the many ms-based call sites; mk-instant-nanos is the nano-precise ctor and
;; inst-nanos the nano accessor (java-time.ss owns the nano-aware arithmetic).
(define (mk-instant-nanos n) (make-jhost "instant" (vector (exact (truncate n)))))
(define (inst-nanos x) (vector-ref (jhost-state x) 0))
(define (mk-instant ms) (mk-instant-nanos (* (ms->exact ms) 1000000)))
(define (mk-zoned ms) (make-jhost "zoned-dt" (vector ms)))
;; LocalDateTime from epoch-ms (UTC): the java-time.ss "local-date-time" jhost,
;; state [epoch-day nano-of-day].
(define (mk-local ms)
  (let* ((ems (exact (truncate ms)))
         (ed (inst-floor-div ems 86400000))
         (mod (inst-floor-mod ems 86400000)))
    (make-jhost "local-date-time" (vector ed (* mod 1000000)))))
;; local-date from epoch-ms: the epoch-day of the UTC day containing ms.
(define (mk-local-date ms) (make-jhost "local-date" (vector (inst-floor-div (exact (truncate ms)) 86400000))))
;; a formatter carries its pattern and a locale id (default "en"); the locale
;; selects month/day names in the java-time.ss format engine.
(define (mk-formatter pat . loc) (make-jhost "dt-formatter" (vector pat (if (null? loc) "en" (car loc)))))
(define (fmt-pat f) (vector-ref (jhost-state f) 0))
(define (fmt-locale f) (let ((s (jhost-state f))) (if (> (vector-length s) 1) (vector-ref s 1) "en")))
(define (locale-id l) (if (and (jhost? l) (string=? (jhost-tag l) "locale")) (vector-ref (jhost-state l) 0) "en"))
(define (now-ms) (now-millis))   ; exact ms (= JVM long); now-millis from host-static.ss
;; coerce a user-supplied ms (exact or flonum) to an exact integer for storage.
(define (ms->exact ms) (exact (round ms)))

(register-host-methods! "instant"
  (list (cons "atZone" (lambda (self zone) (mk-zoned (ms-of self))))
        (cons "toEpochMilli" (lambda (self) (ms-of self)))
        (cons "toString" (lambda (self) (inst-rfc3339 (make-jinst (ms-of self)))))))
(register-host-methods! "zoned-dt"
  (list (cons "toLocalDateTime" (lambda (self) (mk-local (ms-of self))))
        (cons "toInstant" (lambda (self) (mk-instant (ms-of self))))))
;; LocalDate.atZone(zone): the UTC layer treats it as a zoned value at midnight.
;; (java-time.ss registers atStartOfDay and the rest of the local-date surface.)
(register-host-methods! "local-date"
  (list (cons "atZone" (lambda (self zone) (mk-zoned (ms-of self))))))
(register-host-methods! "dt-formatter"
  (list (cons "withLocale" (lambda (self locale) (mk-formatter (fmt-pat self) (locale-id locale))))
        (cons "withZone" (lambda (self zone) (mk-formatter (fmt-pat self) (fmt-locale self))))
        (cons "format" (lambda (self d) (format-ms (fmt-pat self) (ms-of d))))
        ;; parse a string per the pattern -> an instant value; Instant/from / the
        ;; LocalDateTime/parse static read its ms back out.
        (cons "parse" (lambda (self s) (mk-instant (jinst-ms (parse-ms (fmt-pat self) (jolt-str-render-one s))))))))

;; FormatStyle approximations (no locale DB on this host).
(define style-patterns
  '((date . ((short . "M/d/yy") (medium . "MMM d, yyyy") (long . "MMMM d, yyyy") (full . "EEEE, MMMM d, yyyy")))
    (time . ((short . "h:mm a") (medium . "h:mm:ss a") (long . "h:mm:ss a") (full . "h:mm:ss a")))
    (datetime . ((short . "M/d/yy, h:mm a") (medium . "MMM d, yyyy, h:mm:ss a")
                 (long . "MMMM d, yyyy, h:mm:ss a") (full . "EEEE, MMMM d, yyyy, h:mm:ss a")))))
(define (style-of fs) (vector-ref (jhost-state fs) 0))       ; a symbol: short/medium/long/full
(define (style-fmt kind fs)
  (mk-formatter (or (let ((row (assq kind style-patterns))) (and row (let ((e (assq (style-of fs) (cdr row)))) (and e (cdr e)))))
                    "yyyy-MM-dd HH:mm:ss")))

(register-class-statics! "FormatStyle"
  (list (cons "SHORT" (make-jhost "format-style" (vector 'short)))
        (cons "MEDIUM" (make-jhost "format-style" (vector 'medium)))
        (cons "LONG" (make-jhost "format-style" (vector 'long)))
        (cons "FULL" (make-jhost "format-style" (vector 'full)))))
(register-class-statics! "DateTimeFormatter"
  (list (cons "ofPattern" (lambda (p . _) (mk-formatter p)))
        (cons "ISO_LOCAL_DATE" (mk-formatter "yyyy-MM-dd"))
        (cons "ISO_LOCAL_DATE_TIME" (mk-formatter "yyyy-MM-dd'T'HH:mm:ss"))
        ;; ISO_INSTANT always renders in UTC with a trailing Z (format-ms is UTC; X -> "Z").
        (cons "ISO_INSTANT" (mk-formatter "yyyy-MM-dd'T'HH:mm:ssX"))
        ;; ISO_ZONED_DATE_TIME: the UTC layer renders/parses it like ISO_INSTANT.
        (cons "ISO_ZONED_DATE_TIME" (mk-formatter "yyyy-MM-dd'T'HH:mm:ssX"))
        (cons "ofLocalizedDate" (lambda (fs) (style-fmt 'date fs)))
        (cons "ofLocalizedTime" (lambda (fs) (style-fmt 'time fs)))
        (cons "ofLocalizedDateTime" (lambda (fs) (style-fmt 'datetime fs)))))
(register-class-statics! "Instant"
  (list (cons "ofEpochMilli" (lambda (ms) (mk-instant (ms->exact ms))))
        (cons "now" (lambda () (mk-instant (now-ms))))
        ;; Instant/parse an ISO-8601 instant ("…T…Z") -> an instant value.
        (cons "parse" (lambda (s) (mk-instant (jinst-ms (jolt-inst-from-string
                                                          (if (string? s) s (jolt-str-render-one s)))))))
        ;; Instant/from a temporal accessor -> an instant at the same epoch-ms.
        (cons "from" (lambda (t) (mk-instant (ms-of t))))))
(register-class-statics! "ZoneId"
  (list (cons "systemDefault" (lambda () (make-jhost "zone-id" (vector "system"))))
        (cons "of" (lambda (id) (make-jhost "zone-id" (vector id))))))
(register-class-statics! "LocalDateTime"
  (list (cons "ofInstant" (lambda (inst zone) (mk-local (ms-of inst))))
        (cons "now" (lambda () (mk-local (now-ms))))
        ;; LocalDateTime/parse text, or text + a formatter (the UTC layer ignores
        ;; the parsed offset) -> a local-dt at the parsed instant.
        (cons "parse" (lambda (s . fmt)
                        (let ((str (if (string? s) s (jolt-str-render-one s))))
                          (mk-local (jinst-ms (if (null? fmt)
                                                  (jolt-inst-from-string str)
                                                  (parse-ms (fmt-pat (car fmt)) str)))))))))
(let ((locale-ctor (lambda (id . _) (make-jhost "locale" (vector (if (string? id) id (jolt-str-render-one id)))))))
  (register-class-ctor! "Locale" locale-ctor)
  (register-class-ctor! "java.util.Locale" locale-ctor))
(register-class-statics! "Locale"
  (list (cons "getDefault" (lambda () (make-jhost "locale" (vector "default"))))
        (cons "setDefault" (lambda (x) jolt-nil))
        (cons "forLanguageTag" (lambda (tag) (make-jhost "locale" (vector (if (string? tag) tag (jolt-str-render-one tag))))))
        (cons "ENGLISH" (make-jhost "locale" (vector "en")))
        (cons "FRENCH" (make-jhost "locale" (vector "fr")))
        (cons "GERMAN" (make-jhost "locale" (vector "de")))
        (cons "ITALIAN" (make-jhost "locale" (vector "it")))
        (cons "JAPANESE" (make-jhost "locale" (vector "ja")))
        (cons "KOREAN" (make-jhost "locale" (vector "ko")))
        (cons "CHINESE" (make-jhost "locale" (vector "zh")))
        (cons "SIMPLIFIED_CHINESE" (make-jhost "locale" (vector "zh-CN")))
        (cons "TRADITIONAL_CHINESE" (make-jhost "locale" (vector "zh-TW")))
        (cons "FRANCE" (make-jhost "locale" (vector "fr-FR")))
        (cons "GERMANY" (make-jhost "locale" (vector "de-DE")))
        (cons "ITALY" (make-jhost "locale" (vector "it-IT")))
        (cons "JAPAN" (make-jhost "locale" (vector "ja-JP")))
        (cons "KOREA" (make-jhost "locale" (vector "ko-KR")))
        (cons "CHINA" (make-jhost "locale" (vector "zh-CN")))
        (cons "PRC" (make-jhost "locale" (vector "zh-CN")))
        (cons "TAIWAN" (make-jhost "locale" (vector "zh-TW")))
        (cons "UK" (make-jhost "locale" (vector "en-GB")))
        (cons "US" (make-jhost "locale" (vector "en-US")))
        (cons "CANADA" (make-jhost "locale" (vector "en-CA")))
        (cons "CANADA_FRENCH" (make-jhost "locale" (vector "fr-CA")))
        (cons "ROOT" (make-jhost "locale" (vector "")))))

;; java.util.Date / java.sql.Timestamp: #inst's classes. (Date.) = now, (Date. ms)
;; or (Date. another-date) -> a jinst (ms-of accepts a number / jinst / instant), so
;; .getTime / inst? / instance? Date|Timestamp work.
(define (date-ctor . args)
  (cond
    ((null? args) (make-jinst (now-ms)))
    ((null? (cdr args)) (make-jinst (ms->exact (ms-of (car args)))))
    ;; deprecated (Date. year-1900 month0 date [hrs min sec]) — civil fields in UTC.
    (else
     (let* ((y  (+ 1900 (jnum->exact (list-ref args 0))))
            (mo (+ 1 (jnum->exact (list-ref args 1))))
            (d  (jnum->exact (list-ref args 2)))
            (hh (if (> (length args) 3) (jnum->exact (list-ref args 3)) 0))
            (mm (if (> (length args) 4) (jnum->exact (list-ref args 4)) 0))
            (ss (if (> (length args) 5) (jnum->exact (list-ref args 5)) 0)))
       (make-jinst (* 1000 (+ (* (days-from-civil y mo d) 86400) (* hh 3600) (* mm 60) ss)))))))
(register-class-ctor! "Date" date-ctor)
(register-class-ctor! "java.util.Date" date-ctor)
(register-class-ctor! "Timestamp" date-ctor)
(register-class-ctor! "java.sql.Timestamp" date-ctor)
;; Date/from(Instant) -> a java.util.Date at the instant's epoch-ms.
(let ((date-statics (list (cons "from" (lambda (inst) (make-jinst (ms->exact (ms-of inst))))))))
  (register-class-statics! "Date" date-statics)
  (register-class-statics! "java.util.Date" date-statics))
;; java.sql.Date: a distinct class from java.util.Date (a "sql-date" jhost over
;; epoch-ms) so a protocol extended to both routes a sql.Date to its own impl.
;; (Date. year-1900 month0 day) builds UTC midnight of that civil date; valueOf
;; parses "yyyy-MM-dd" to the same instant (so the two agree).
(define (mk-sql-date ms) (make-jhost "sql-date" (vector (ms->exact ms))))
(define (sql-date-midnight y mo d) (mk-sql-date (* 1000 (* (days-from-civil y mo d) 86400))))
(register-class-ctor! "java.sql.Date"
  (case-lambda
    ((ms) (mk-sql-date (ms-of ms)))   ; (Date. epoch-ms)
    ((y m d) (sql-date-midnight (+ 1900 (jnum->exact y)) (+ 1 (jnum->exact m)) (jnum->exact d)))))
(register-class-statics! "java.sql.Date"
  (list (cons "valueOf" (lambda (s) (mk-sql-date (jinst-ms (parse-ms "yyyy-MM-dd" (if (string? s) s (jolt-str-render-one s)))))))))
(register-host-methods! "sql-date"
  (list (cons "getTime" (lambda (self) (ms-of self)))
        (cons "toInstant" (lambda (self) (mk-instant (ms-of self))))
        (cons "toLocalDate" (lambda (self) (mk-local-date (ms-of self))))
        (cons "toString" (lambda (self) (inst-rfc3339 (make-jinst (ms-of self)))))))

;; java.util.Calendar: a mutable broken-down UTC time over an epoch-ms. setTime/
;; getTime read/write it; set(field,value) recomputes ms from the field projection.
;; Field constants are Java's int values so .set/.get dispatch on the right field.
(define cal-YEAR 1) (define cal-MONTH 2) (define cal-DAY_OF_MONTH 5)
(define cal-HOUR_OF_DAY 11) (define cal-MINUTE 12) (define cal-SECOND 13)
(define cal-MILLISECOND 14)
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
(register-host-methods! "calendar"
  (list (cons "setTime" (lambda (self d) (vector-set! (jhost-state self) 0 (ms->exact (ms-of d))) jolt-nil))
        (cons "getTime" (lambda (self) (make-jinst (vector-ref (jhost-state self) 0))))
        (cons "getTimeInMillis" (lambda (self) (vector-ref (jhost-state self) 0)))
        (cons "setTimeInMillis" (lambda (self ms) (vector-set! (jhost-state self) 0 (ms->exact ms)) jolt-nil))
        (cons "set" (lambda (self field val)
                      (let ((v (cal-ms->fields (vector-ref (jhost-state self) 0)))
                            (idx (cal-field-index (jnum->exact field))))
                        (when idx (vector-set! v idx (jnum->exact val))
                              (vector-set! (jhost-state self) 0 (cal-fields->ms v)))
                        jolt-nil)))
        (cons "get" (lambda (self field)
                      (let ((v (cal-ms->fields (vector-ref (jhost-state self) 0)))
                            (idx (cal-field-index (jnum->exact field))))
                        (if idx (vector-ref v idx) 0))))))
(define calendar-statics
  (list (cons "getInstance" (lambda _ (make-jhost "calendar" (vector (now-ms)))))
        (cons "YEAR" cal-YEAR) (cons "MONTH" cal-MONTH) (cons "DAY_OF_MONTH" cal-DAY_OF_MONTH)
        (cons "HOUR_OF_DAY" cal-HOUR_OF_DAY) (cons "MINUTE" cal-MINUTE)
        (cons "SECOND" cal-SECOND) (cons "MILLISECOND" cal-MILLISECOND)))
(register-class-statics! "Calendar" calendar-statics)
(register-class-statics! "java.util.Calendar" calendar-statics)

;; java.util.TimeZone: an opaque id holder (format-ms is UTC, so a non-UTC zone is
;; not honored — only the UTC case the corpus uses is exercised).
(define (timezone-of id) (make-jhost "timezone" (vector (if (string? id) id (jolt-str-render-one id)))))
(define timezone-statics
  (list (cons "getTimeZone" timezone-of)
        (cons "getDefault" (lambda () (timezone-of "default")))))
(register-class-statics! "TimeZone" timezone-statics)
(register-class-statics! "java.util.TimeZone" timezone-statics)

;; java.text.SimpleDateFormat: holds a pattern; .setTimeZone is accepted (format-ms
;; is UTC); .format(date) renders the date per the pattern via the format-ms engine.
(define (sdf-ctor pat . _) (make-jhost "sdf" (vector (if (string? pat) pat (jolt-str-render-one pat)))))
(register-class-ctor! "SimpleDateFormat" sdf-ctor)
(register-class-ctor! "java.text.SimpleDateFormat" sdf-ctor)
(register-host-methods! "sdf"
  (list (cons "setTimeZone" (lambda (self tz) jolt-nil))
        (cons "setLenient" (lambda (self b) jolt-nil))
        (cons "applyPattern" (lambda (self p) (vector-set! (jhost-state self) 0 (jolt-str-render-one p)) jolt-nil))
        (cons "toPattern" (lambda (self) (vector-ref (jhost-state self) 0)))
        (cons "parse" (lambda (self s) (parse-ms (vector-ref (jhost-state self) 0) (jolt-str-render-one s))))
        (cons "format" (lambda (self d) (format-ms (vector-ref (jhost-state self) 0) (ms-of d))))))

;; a jinst's java.util.Date method surface (record-method-dispatch arm).
(register-method-arm! 40
  (lambda (obj method-name rest-args)
    (cond
      ((jinst? obj)
       (cond ((string=? method-name "getTime") (jinst-ms obj))
             ;; deprecated java.util.Date accessors (UTC civil fields).
             ((string=? method-name "getYear") (- (list-ref (inst-fields (jinst-ms obj)) 0) 1900))
             ((string=? method-name "getMonth") (- (list-ref (inst-fields (jinst-ms obj)) 1) 1))
             ((string=? method-name "getDate") (list-ref (inst-fields (jinst-ms obj)) 2))
             ((string=? method-name "getHours") (list-ref (inst-fields (jinst-ms obj)) 3))
             ((string=? method-name "getMinutes") (list-ref (inst-fields (jinst-ms obj)) 4))
             ((string=? method-name "getSeconds") (list-ref (inst-fields (jinst-ms obj)) 5))
             ((string=? method-name "getDay") (list-ref (inst-fields (jinst-ms obj)) 7))
             ((string=? method-name "toInstant") (mk-instant (jinst-ms obj)))
             ((string=? method-name "toLocalDate") (mk-local-date (jinst-ms obj)))
             ((string=? method-name "toLocalDateTime") (mk-local (jinst-ms obj)))
             ((string=? method-name "toString") (inst-rfc3339 obj))
             ((string=? method-name "equals") (and (pair? (if (jolt-nil? rest-args) '() (seq->list rest-args)))
                                                   (jinst? (car (seq->list rest-args)))
                                                   (= (jinst-ms obj) (jinst-ms (car (seq->list rest-args))))))
             ((string=? method-name "before") (< (jinst-ms obj) (ms-of (car (seq->list rest-args)))))
             ((string=? method-name "after") (> (jinst-ms obj) (ms-of (car (seq->list rest-args)))))
             (else (error #f (string-append "No method " method-name " on Date")))))
      (else 'pass))))

;; Clojure's built-in data readers, so a library that merges default-data-readers
;; or binds *data-readers* (e.g. aero's reader opts) resolves #inst / #uuid.
;; Keyed by symbol, like Clojure. *data-readers* is the bindable user table.
(def-var! "clojure.core" "default-data-readers"
  (jolt-hash-map (jolt-symbol #f "inst") jolt-inst-from-string
                 (jolt-symbol #f "uuid") jolt-uuid-from-string))
(def-var! "clojure.core" "*data-readers*" empty-pmap)
