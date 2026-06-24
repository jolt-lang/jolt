;; java.time core value types: LocalDate, LocalTime, LocalDateTime, Instant.
;;
;; Each is a tz-free jhost over an integer state:
;;   local-date      (vector epoch-day)                 day number from 1970-01-01
;;   local-time      (vector nano-of-day)               [0, 86_400_000_000_000)
;;   local-dt        (vector epoch-day nano-of-day)
;;   instant         (vector epoch-ms)                  reused from inst-time.ss
;;
;; The cljc.java-time wrappers are thin interop over these — static factories
;; (LocalDate/of …) reach register-class-statics!, instance methods reach the
;; per-tag method tables. Equality / hash / compare / print / instance? / protocol
;; dispatch are wired through the host registries.
;;
;; Precision: Instant is millisecond-granular (it carries epoch-ms only). get-nano
;; reports (ms-sub-second * 1e6); plus-nanos / plus-millis round to the millisecond.
;; LocalTime / LocalDateTime carry full nanosecond precision.

;; --- shared helpers ----------------------------------------------------------
(define nanos-per-day 86400000000000)
(define nanos-per-sec 1000000000)

(define (jt-floor-div a b) (let ((q (quotient a b)) (r (remainder a b)))
                             (if (and (not (= r 0)) (< (* a b) 0)) (- q 1) q)))
(define (jt-floor-mod a b) (- a (* (jt-floor-div a b) b)))
(define (jt->exact n) (cond ((and (number? n) (exact? n) (integer? n)) n)
                            ((number? n) (exact (truncate n)))
                            (else (error #f "java.time: not an integer" n))))

(define (jt-leap? y) (and (= 0 (modulo y 4)) (or (not (= 0 (modulo y 100))) (= 0 (modulo y 400)))))
(define (jt-len-of-month y m)
  (cond ((= m 2) (if (jt-leap? y) 29 28))
        ((memv m '(4 6 9 11)) 30)
        (else 31)))

;; constructors
(define (jt-local-date ed) (make-jhost "local-date" (vector ed)))
(define (jt-local-time nod) (make-jhost "local-time" (vector nod)))
(define (jt-local-dt ed nod) (make-jhost "local-date-time" (vector ed nod)))

;; state accessors
(define (ld-epoch-day d) (vector-ref (jhost-state d) 0))
(define (lt-nano-of-day t) (vector-ref (jhost-state t) 0))
;; the LocalDateTime jhost uses tag "local-date-time" with [epoch-day nano-of-day].
;; inst-time.ss's ms-based "local-dt" tag stays for the #inst formatting shim.
(define (ldt-epoch-day x) (vector-ref (jhost-state x) 0))
(define (ldt-nano-of-day x) (vector-ref (jhost-state x) 1))

;; civil <-> epoch-day (inst-time.ss owns days-from-civil / civil-from-days)
(define (ymd->epoch-day y m d) (days-from-civil y m d))
(define (epoch-day->ymd ed) (civil-from-days ed))   ; -> (values y m d)

;; build a LocalDate, normalizing month/day overflow the way java.time does:
;; the year/month roll, then the day is clamped to the month length.
(define (jt-date-of y m d)
  (let* ((ym (+ (* y 12) (- m 1)))
         (y2 (jt-floor-div ym 12))
         (m2 (+ 1 (jt-floor-mod ym 12)))
         (dom (min d (jt-len-of-month y2 m2))))
    (jt-local-date (ymd->epoch-day y2 m2 dom))))

;; day-of-week: 1970-01-01 is Thursday; java.time DayOfWeek is 1=Mon..7=Sun.
(define (ld-dow ed) (+ 1 (jt-floor-mod (+ ed 3) 7)))
(define jt-day-names (vector "MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY" "SATURDAY" "SUNDAY"))
(define jt-month-names (vector "JANUARY" "FEBRUARY" "MARCH" "APRIL" "MAY" "JUNE" "JULY"
                               "AUGUST" "SEPTEMBER" "OCTOBER" "NOVEMBER" "DECEMBER"))

(define (ld-day-of-year ed)
  (call-with-values (lambda () (epoch-day->ymd ed))
    (lambda (y m d) (+ 1 (- ed (ymd->epoch-day y 1 1))))))

;; nano-of-day <-> (h m s nano)
(define (hmsn->nano h m s nano) (+ (* (+ (* h 3600) (* m 60) s) nanos-per-sec) nano))
(define (lt-hour t) (quotient (lt-nano-of-day t) (* 3600 nanos-per-sec)))
(define (lt-minute t) (modulo (quotient (lt-nano-of-day t) (* 60 nanos-per-sec)) 60))
(define (lt-second t) (modulo (quotient (lt-nano-of-day t) nanos-per-sec) 60))
(define (lt-nano t) (modulo (lt-nano-of-day t) nanos-per-sec))

;; --- ISO-8601 rendering ------------------------------------------------------
(define (iso-date-str ed)
  (call-with-values (lambda () (epoch-day->ymd ed))
    (lambda (y m d) (string-append (pad4 y) "-" (pad2 m) "-" (pad2 d)))))

;; LocalTime: "HH:mm", "HH:mm:ss", or with a fractional second (3/6/9 digits).
(define (frac-digits nano)
  (cond ((= 0 (modulo nano 1000000)) (let ((s (number->string (quotient nano 1000000))))
                                       (string-append (make-string (max 0 (- 3 (string-length s))) #\0) s)))
        ((= 0 (modulo nano 1000)) (let ((s (number->string (quotient nano 1000))))
                                    (string-append (make-string (max 0 (- 6 (string-length s))) #\0) s)))
        (else (let ((s (number->string nano)))
                (string-append (make-string (max 0 (- 9 (string-length s))) #\0) s)))))
(define (iso-time-str nod)
  (let ((h (quotient nod (* 3600 nanos-per-sec)))
        (mi (modulo (quotient nod (* 60 nanos-per-sec)) 60))
        (s (modulo (quotient nod nanos-per-sec) 60))
        (nano (modulo nod nanos-per-sec)))
    (string-append (pad2 h) ":" (pad2 mi)
                   (if (and (= s 0) (= nano 0)) ""
                       (string-append ":" (pad2 s) (if (= nano 0) "" (string-append "." (frac-digits nano))))))))
(define (iso-datetime-str ed nod)
  (string-append (iso-date-str ed) "T" (iso-time-str nod)))
;; Instant: always UTC with a trailing Z; seconds always shown, millis only when nonzero.
(define (iso-instant-str ms)
  (let* ((ems (exact (truncate ms)))
         (secs (jt-floor-div ems 1000))
         (frac (- ems (* secs 1000)))
         (ed (jt-floor-div secs 86400))
         (sod (jt-floor-mod secs 86400))
         (nod (* (+ (* (quotient sod 3600) 3600) (* (quotient (modulo sod 3600) 60) 60) (modulo sod 60))
                 nanos-per-sec)))
    (string-append (iso-date-str ed) "T"
                   (pad2 (quotient sod 3600)) ":" (pad2 (modulo (quotient sod 60) 60)) ":" (pad2 (modulo sod 60))
                   (if (= frac 0) "" (string-append "." (frac-digits (* frac 1000000))))
                   "Z")))

;; --- ISO parsing -------------------------------------------------------------
(define (jt-str x) (if (string? x) x (jolt-str-render-one x)))
;; "yyyy-MM-dd" -> epoch-day
(define (parse-iso-date s)
  (let ((y (digits-at s 0 4)) (m (digits-at s 5 2)) (d (digits-at s 8 2)))
    (if (and y m d (char=? (string-ref s 4) #\-) (char=? (string-ref s 7) #\-))
        (ymd->epoch-day y m d)
        (error #f (string-append "could not parse LocalDate: " s)))))
;; "HH:mm[:ss[.fff…]]" -> nano-of-day
(define (parse-iso-time s)
  (let ((len (string-length s)))
    (let ((h (digits-at s 0 2)) (mi (digits-at s 3 2)))
      (unless (and h mi (char=? (string-ref s 2) #\:))
        (error #f (string-append "could not parse LocalTime: " s)))
      (let ((s2 (and (> len 5) (char=? (string-ref s 5) #\:) (digits-at s 6 2))))
        (if (not s2)
            (hmsn->nano h mi 0 0)
            (let loop ((i 8) (nano 0))   ; optional .fraction
              (if (and (< i len) (char=? (string-ref s 8) #\.))
                  (let frac ((j 9) (k 0) (acc 0))
                    (if (and (< j len) (digit? (string-ref s j)))
                        (frac (+ j 1) (+ k 1) (+ (* acc 10) (- (char->integer (string-ref s j)) 48)))
                        (hmsn->nano h mi s2 (* acc (expt 10 (max 0 (- 9 k)))))))
                  (hmsn->nano h mi s2 0))))))))
;; "yyyy-MM-ddTHH:mm[:ss[.fff]]" -> (values epoch-day nano-of-day)
(define (parse-iso-datetime s)
  (let ((ti (let loop ((i 0)) (cond ((>= i (string-length s)) #f)
                                    ((or (char=? (string-ref s i) #\T) (char=? (string-ref s i) #\t)) i)
                                    (else (loop (+ i 1)))))))
    (unless ti (error #f (string-append "could not parse LocalDateTime: " s)))
    (values (parse-iso-date (substring s 0 ti))
            (parse-iso-time (substring s (+ ti 1) (string-length s))))))

;; --- LocalDate ---------------------------------------------------------------
(define ld-min (jt-local-date (ymd->epoch-day -999999999 1 1)))
(define ld-max (jt-local-date (ymd->epoch-day 999999999 12 31)))

(register-class-statics! "LocalDate"
  (list (cons "of" (lambda (y m d) (jt-date-of (jt->exact y) (jt->exact m) (jt->exact d))))
        (cons "ofEpochDay" (lambda (n) (jt-local-date (jt->exact n))))
        (cons "ofYearDay" (lambda (y doy) (jt-local-date (+ (ymd->epoch-day (jt->exact y) 1 1) (- (jt->exact doy) 1)))))
        (cons "parse" (lambda (s . _) (jt-local-date (parse-iso-date (jt-str s)))))
        (cons "now" (lambda _ (mk-local-date (now-ms))))
        (cons "from" (lambda (t) (cond ((and (jhost? t) (string=? (jhost-tag t) "local-date")) t)
                                       ((and (jhost? t) (string=? (jhost-tag t) "local-date-time")) (jt-local-date (ldt-epoch-day t)))
                                       (else (mk-local-date (ms-of t))))))
        (cons "MIN" ld-min)
        (cons "MAX" ld-max)))

(define (ld-plus-months d n)
  (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d)))
    (lambda (y m dom)
      (let* ((ym (+ (* y 12) (- m 1) n))
             (y2 (jt-floor-div ym 12)) (m2 (+ 1 (jt-floor-mod ym 12))))
        (jt-local-date (ymd->epoch-day y2 m2 (min dom (jt-len-of-month y2 m2))))))))
(define (ld-plus-years d n)
  (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d)))
    (lambda (y m dom) (jt-local-date (ymd->epoch-day (+ y n) m (min dom (jt-len-of-month (+ y n) m)))))))

(define (ld-with-field d which v)
  (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d)))
    (lambda (y m dom)
      (case which
        ((year) (jt-local-date (ymd->epoch-day v m (min dom (jt-len-of-month v m)))))
        ((month) (jt-local-date (ymd->epoch-day y v (min dom (jt-len-of-month y v)))))
        ((day) (jt-local-date (ymd->epoch-day y m v)))
        ((day-of-year) (jt-local-date (+ (ymd->epoch-day y 1 1) (- v 1))))))))

(register-host-methods! "local-date"
  (list (cons "getYear" (lambda (d) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d))) (lambda (y m dd) y))))
        (cons "getMonthValue" (lambda (d) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d))) (lambda (y m dd) m))))
        (cons "getDayOfMonth" (lambda (d) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d))) (lambda (y m dd) dd))))
        (cons "getMonth" (lambda (d) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d)))
                                       (lambda (y m dd) (make-jhost "month-enum" (vector m))))))
        (cons "getDayOfWeek" (lambda (d) (make-jhost "dow-enum" (vector (ld-dow (ld-epoch-day d))))))
        (cons "getDayOfYear" (lambda (d) (ld-day-of-year (ld-epoch-day d))))
        (cons "toEpochDay" (lambda (d) (ld-epoch-day d)))
        (cons "lengthOfMonth" (lambda (d) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d)))
                                            (lambda (y m dd) (jt-len-of-month y m)))))
        (cons "lengthOfYear" (lambda (d) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d)))
                                           (lambda (y m dd) (if (jt-leap? y) 366 365)))))
        (cons "isLeapYear" (lambda (d) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d)))
                                         (lambda (y m dd) (jt-leap? y)))))
        (cons "plusDays" (lambda (d n) (jt-local-date (+ (ld-epoch-day d) (jt->exact n)))))
        (cons "minusDays" (lambda (d n) (jt-local-date (- (ld-epoch-day d) (jt->exact n)))))
        (cons "plusWeeks" (lambda (d n) (jt-local-date (+ (ld-epoch-day d) (* 7 (jt->exact n))))))
        (cons "minusWeeks" (lambda (d n) (jt-local-date (- (ld-epoch-day d) (* 7 (jt->exact n))))))
        (cons "plusMonths" (lambda (d n) (ld-plus-months d (jt->exact n))))
        (cons "minusMonths" (lambda (d n) (ld-plus-months d (- (jt->exact n)))))
        (cons "plusYears" (lambda (d n) (ld-plus-years d (jt->exact n))))
        (cons "minusYears" (lambda (d n) (ld-plus-years d (- (jt->exact n)))))
        (cons "withYear" (lambda (d v) (ld-with-field d 'year (jt->exact v))))
        (cons "withMonth" (lambda (d v) (ld-with-field d 'month (jt->exact v))))
        (cons "withDayOfMonth" (lambda (d v) (ld-with-field d 'day (jt->exact v))))
        (cons "withDayOfYear" (lambda (d v) (ld-with-field d 'day-of-year (jt->exact v))))
        (cons "isBefore" (lambda (d o) (< (ld-epoch-day d) (ld-epoch-day o))))
        (cons "isAfter" (lambda (d o) (> (ld-epoch-day d) (ld-epoch-day o))))
        (cons "isEqual" (lambda (d o) (= (ld-epoch-day d) (ld-epoch-day o))))
        (cons "compareTo" (lambda (d o) (let ((a (ld-epoch-day d)) (b (ld-epoch-day o)))
                                          (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals" (lambda (d o) (and (jhost? o) (string=? (jhost-tag o) "local-date") (= (ld-epoch-day d) (ld-epoch-day o)))))
        (cons "hashCode" (lambda (d) (ld-epoch-day d)))
        (cons "atStartOfDay" (lambda (d . _) (jt-local-dt (ld-epoch-day d) 0)))
        (cons "atTime" (case-lambda
                         ((d t) (if (and (jhost? t) (string=? (jhost-tag t) "local-time"))
                                    (jt-local-dt (ld-epoch-day d) (lt-nano-of-day t))
                                    (error #f "atTime: expected LocalTime")))
                         ((d h m) (jt-local-dt (ld-epoch-day d) (hmsn->nano (jt->exact h) (jt->exact m) 0 0)))
                         ((d h m s) (jt-local-dt (ld-epoch-day d) (hmsn->nano (jt->exact h) (jt->exact m) (jt->exact s) 0)))
                         ((d h m s nano) (jt-local-dt (ld-epoch-day d) (hmsn->nano (jt->exact h) (jt->exact m) (jt->exact s) (jt->exact nano))))))
        (cons "toString" (lambda (d) (iso-date-str (ld-epoch-day d))))))

;; --- LocalTime ---------------------------------------------------------------
(define lt-min (jt-local-time 0))
(define lt-max (jt-local-time (- nanos-per-day 1)))

(register-class-statics! "LocalTime"
  (list (cons "of" (case-lambda
                     ((h m) (jt-local-time (hmsn->nano (jt->exact h) (jt->exact m) 0 0)))
                     ((h m s) (jt-local-time (hmsn->nano (jt->exact h) (jt->exact m) (jt->exact s) 0)))
                     ((h m s nano) (jt-local-time (hmsn->nano (jt->exact h) (jt->exact m) (jt->exact s) (jt->exact nano))))))
        (cons "ofNanoOfDay" (lambda (n) (jt-local-time (jt->exact n))))
        (cons "ofSecondOfDay" (lambda (n) (jt-local-time (* (jt->exact n) nanos-per-sec))))
        (cons "parse" (lambda (s . _) (jt-local-time (parse-iso-time (jt-str s)))))
        (cons "now" (lambda _ (jt-local-time (let ((ms (now-ms))) (* (jt-floor-mod (exact (truncate ms)) 86400000) 1000000)))))
        (cons "from" (lambda (t) (cond ((and (jhost? t) (string=? (jhost-tag t) "local-time")) t)
                                       ((and (jhost? t) (string=? (jhost-tag t) "local-date-time")) (jt-local-time (ldt-nano-of-day t)))
                                       (else (error #f "LocalTime/from: unsupported")))))
        (cons "MIN" lt-min)
        (cons "MAX" lt-max)
        (cons "MIDNIGHT" (jt-local-time 0))
        (cons "NOON" (jt-local-time (* 12 3600 nanos-per-sec)))))

(define (lt-plus t nanos) (jt-local-time (jt-floor-mod (+ (lt-nano-of-day t) nanos) nanos-per-day)))
(define (lt-with t which v)
  (let ((h (lt-hour t)) (mi (lt-minute t)) (s (lt-second t)) (nano (lt-nano t)))
    (case which
      ((hour) (jt-local-time (hmsn->nano v mi s nano)))
      ((minute) (jt-local-time (hmsn->nano h v s nano)))
      ((second) (jt-local-time (hmsn->nano h mi v nano)))
      ((nano) (jt-local-time (hmsn->nano h mi s v))))))

(register-host-methods! "local-time"
  (list (cons "getHour" (lambda (t) (lt-hour t)))
        (cons "getMinute" (lambda (t) (lt-minute t)))
        (cons "getSecond" (lambda (t) (lt-second t)))
        (cons "getNano" (lambda (t) (lt-nano t)))
        (cons "toNanoOfDay" (lambda (t) (lt-nano-of-day t)))
        (cons "toSecondOfDay" (lambda (t) (quotient (lt-nano-of-day t) nanos-per-sec)))
        (cons "plusHours" (lambda (t n) (lt-plus t (* (jt->exact n) 3600 nanos-per-sec))))
        (cons "minusHours" (lambda (t n) (lt-plus t (- (* (jt->exact n) 3600 nanos-per-sec)))))
        (cons "plusMinutes" (lambda (t n) (lt-plus t (* (jt->exact n) 60 nanos-per-sec))))
        (cons "minusMinutes" (lambda (t n) (lt-plus t (- (* (jt->exact n) 60 nanos-per-sec)))))
        (cons "plusSeconds" (lambda (t n) (lt-plus t (* (jt->exact n) nanos-per-sec))))
        (cons "minusSeconds" (lambda (t n) (lt-plus t (- (* (jt->exact n) nanos-per-sec)))))
        (cons "plusNanos" (lambda (t n) (lt-plus t (jt->exact n))))
        (cons "minusNanos" (lambda (t n) (lt-plus t (- (jt->exact n)))))
        (cons "withHour" (lambda (t v) (lt-with t 'hour (jt->exact v))))
        (cons "withMinute" (lambda (t v) (lt-with t 'minute (jt->exact v))))
        (cons "withSecond" (lambda (t v) (lt-with t 'second (jt->exact v))))
        (cons "withNano" (lambda (t v) (lt-with t 'nano (jt->exact v))))
        (cons "truncatedTo" (lambda (t u) (lt-truncate t u)))
        (cons "isBefore" (lambda (t o) (< (lt-nano-of-day t) (lt-nano-of-day o))))
        (cons "isAfter" (lambda (t o) (> (lt-nano-of-day t) (lt-nano-of-day o))))
        (cons "compareTo" (lambda (t o) (let ((a (lt-nano-of-day t)) (b (lt-nano-of-day o)))
                                          (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals" (lambda (t o) (and (jhost? o) (string=? (jhost-tag o) "local-time") (= (lt-nano-of-day t) (lt-nano-of-day o)))))
        (cons "hashCode" (lambda (t) (lt-nano-of-day t)))
        (cons "atDate" (lambda (t d) (jt-local-dt (ld-epoch-day d) (lt-nano-of-day t))))
        (cons "toString" (lambda (t) (iso-time-str (lt-nano-of-day t))))))

;; truncatedTo a ChronoUnit: zero out below the unit. The unit arrives as a
;; chrono-unit jhost (name) or a keyword/string; common units only.
(define (chrono-unit-name u)
  (cond ((and (jhost? u) (string=? (jhost-tag u) "chrono-unit")) (vector-ref (jhost-state u) 0))
        ((string? u) u)
        ((keyword? u) (keyword-t-name u))
        (else #f)))
(define (lt-truncate t u)
  (let* ((nod (lt-nano-of-day t))
         (unit (chrono-unit-name u))
         (div (cond ((not unit) 1)
                    ((string-ci=? unit "NANOS") 1)
                    ((string-ci=? unit "MICROS") 1000)
                    ((string-ci=? unit "MILLIS") 1000000)
                    ((string-ci=? unit "SECONDS") nanos-per-sec)
                    ((string-ci=? unit "MINUTES") (* 60 nanos-per-sec))
                    ((string-ci=? unit "HOURS") (* 3600 nanos-per-sec))
                    ((string-ci=? unit "DAYS") nanos-per-day)
                    (else 1))))
    (jt-local-time (* (quotient nod div) div))))

;; --- LocalDateTime -----------------------------------------------------------
(define ldt-min (jt-local-dt (ymd->epoch-day -999999999 1 1) 0))
(define ldt-max (jt-local-dt (ymd->epoch-day 999999999 12 31) (- nanos-per-day 1)))

;; epoch-seconds at a zero offset (the tz-free layer treats LocalDateTime as UTC).
(define (ldt->epoch-second x) (+ (* (ldt-epoch-day x) 86400) (quotient (ldt-nano-of-day x) nanos-per-sec)))
(define (ldt->ms x) (+ (* (ldt-epoch-day x) 86400000) (quotient (ldt-nano-of-day x) 1000000)))

(register-class-statics! "LocalDateTime"
  (list (cons "of" (case-lambda
                     ((d t) (jt-local-dt (ld-epoch-day d) (lt-nano-of-day t)))   ; (LocalDate LocalTime)
                     ((y mo d h mi) (jt-local-dt (ymd->epoch-day (jt->exact y) (jt->exact mo) (jt->exact d))
                                                 (hmsn->nano (jt->exact h) (jt->exact mi) 0 0)))
                     ((y mo d h mi s) (jt-local-dt (ymd->epoch-day (jt->exact y) (jt->exact mo) (jt->exact d))
                                                   (hmsn->nano (jt->exact h) (jt->exact mi) (jt->exact s) 0)))
                     ((y mo d h mi s nano) (jt-local-dt (ymd->epoch-day (jt->exact y) (jt->exact mo) (jt->exact d))
                                                        (hmsn->nano (jt->exact h) (jt->exact mi) (jt->exact s) (jt->exact nano))))))
        (cons "ofEpochSecond" (lambda (secs nano off)
                                (let ((es (jt->exact secs)))
                                  (jt-local-dt (jt-floor-div es 86400)
                                               (+ (* (jt-floor-mod es 86400) nanos-per-sec) (jt->exact nano))))))
        (cons "ofInstant" (lambda (inst . _) (let ((ms (ms-of inst)))
                                               (jt-local-dt (jt-floor-div (exact (truncate ms)) 86400000)
                                                            (* (jt-floor-mod (exact (truncate ms)) 86400000) 1000000)))))
        (cons "parse" (lambda (s . _) (call-with-values (lambda () (parse-iso-datetime (jt-str s)))
                                        (lambda (ed nod) (jt-local-dt ed nod)))))
        (cons "now" (lambda _ (let ((ms (exact (truncate (now-ms)))))
                                (jt-local-dt (jt-floor-div ms 86400000) (* (jt-floor-mod ms 86400000) 1000000)))))
        (cons "from" (lambda (t) (cond ((and (jhost? t) (string=? (jhost-tag t) "local-date-time")) t)
                                       (else (let ((ms (ms-of t)))
                                               (jt-local-dt (jt-floor-div (exact (truncate ms)) 86400000)
                                                            (* (jt-floor-mod (exact (truncate ms)) 86400000) 1000000)))))))
        (cons "MIN" ldt-min)
        (cons "MAX" ldt-max)))

;; date / time arithmetic on a LocalDateTime: round-trip through the components.
(define (ldt-date x) (jt-local-date (ldt-epoch-day x)))
(define (ldt-time x) (jt-local-time (ldt-nano-of-day x)))
(define (ldt-combine d t) (jt-local-dt (ld-epoch-day d) (lt-nano-of-day t)))
;; add nanos to a LocalDateTime, carrying whole days into the date.
(define (ldt-plus-nanos x nanos)
  (let* ((total (+ (ldt-nano-of-day x) nanos))
         (carry (jt-floor-div total nanos-per-day))
         (nod (jt-floor-mod total nanos-per-day)))
    (jt-local-dt (+ (ldt-epoch-day x) carry) nod)))

(register-host-methods! "local-date-time"
  (list (cons "getYear" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (ldt-epoch-day x))) (lambda (y m d) y))))
        (cons "getMonthValue" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (ldt-epoch-day x))) (lambda (y m d) m))))
        (cons "getMonth" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (ldt-epoch-day x))) (lambda (y m d) (make-jhost "month-enum" (vector m))))))
        (cons "getDayOfMonth" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (ldt-epoch-day x))) (lambda (y m d) d))))
        (cons "getDayOfWeek" (lambda (x) (make-jhost "dow-enum" (vector (ld-dow (ldt-epoch-day x))))))
        (cons "getDayOfYear" (lambda (x) (ld-day-of-year (ldt-epoch-day x))))
        (cons "getHour" (lambda (x) (lt-hour (ldt-time x))))
        (cons "getMinute" (lambda (x) (lt-minute (ldt-time x))))
        (cons "getSecond" (lambda (x) (lt-second (ldt-time x))))
        (cons "getNano" (lambda (x) (lt-nano (ldt-time x))))
        (cons "toLocalDate" (lambda (x) (ldt-date x)))
        (cons "toLocalTime" (lambda (x) (ldt-time x)))
        (cons "toEpochSecond" (lambda (x . _) (ldt->epoch-second x)))
        (cons "plusDays" (lambda (x n) (jt-local-dt (+ (ldt-epoch-day x) (jt->exact n)) (ldt-nano-of-day x))))
        (cons "minusDays" (lambda (x n) (jt-local-dt (- (ldt-epoch-day x) (jt->exact n)) (ldt-nano-of-day x))))
        (cons "plusWeeks" (lambda (x n) (jt-local-dt (+ (ldt-epoch-day x) (* 7 (jt->exact n))) (ldt-nano-of-day x))))
        (cons "minusWeeks" (lambda (x n) (jt-local-dt (- (ldt-epoch-day x) (* 7 (jt->exact n))) (ldt-nano-of-day x))))
        (cons "plusMonths" (lambda (x n) (ldt-combine (ld-plus-months (ldt-date x) (jt->exact n)) (ldt-time x))))
        (cons "minusMonths" (lambda (x n) (ldt-combine (ld-plus-months (ldt-date x) (- (jt->exact n))) (ldt-time x))))
        (cons "plusYears" (lambda (x n) (ldt-combine (ld-plus-years (ldt-date x) (jt->exact n)) (ldt-time x))))
        (cons "minusYears" (lambda (x n) (ldt-combine (ld-plus-years (ldt-date x) (- (jt->exact n))) (ldt-time x))))
        (cons "plusHours" (lambda (x n) (ldt-plus-nanos x (* (jt->exact n) 3600 nanos-per-sec))))
        (cons "minusHours" (lambda (x n) (ldt-plus-nanos x (- (* (jt->exact n) 3600 nanos-per-sec)))))
        (cons "plusMinutes" (lambda (x n) (ldt-plus-nanos x (* (jt->exact n) 60 nanos-per-sec))))
        (cons "minusMinutes" (lambda (x n) (ldt-plus-nanos x (- (* (jt->exact n) 60 nanos-per-sec)))))
        (cons "plusSeconds" (lambda (x n) (ldt-plus-nanos x (* (jt->exact n) nanos-per-sec))))
        (cons "minusSeconds" (lambda (x n) (ldt-plus-nanos x (- (* (jt->exact n) nanos-per-sec)))))
        (cons "plusNanos" (lambda (x n) (ldt-plus-nanos x (jt->exact n))))
        (cons "minusNanos" (lambda (x n) (ldt-plus-nanos x (- (jt->exact n)))))
        (cons "withYear" (lambda (x v) (ldt-combine (ld-with-field (ldt-date x) 'year (jt->exact v)) (ldt-time x))))
        (cons "withMonth" (lambda (x v) (ldt-combine (ld-with-field (ldt-date x) 'month (jt->exact v)) (ldt-time x))))
        (cons "withDayOfMonth" (lambda (x v) (ldt-combine (ld-with-field (ldt-date x) 'day (jt->exact v)) (ldt-time x))))
        (cons "withDayOfYear" (lambda (x v) (ldt-combine (ld-with-field (ldt-date x) 'day-of-year (jt->exact v)) (ldt-time x))))
        (cons "withHour" (lambda (x v) (ldt-combine (ldt-date x) (lt-with (ldt-time x) 'hour (jt->exact v)))))
        (cons "withMinute" (lambda (x v) (ldt-combine (ldt-date x) (lt-with (ldt-time x) 'minute (jt->exact v)))))
        (cons "withSecond" (lambda (x v) (ldt-combine (ldt-date x) (lt-with (ldt-time x) 'second (jt->exact v)))))
        (cons "withNano" (lambda (x v) (ldt-combine (ldt-date x) (lt-with (ldt-time x) 'nano (jt->exact v)))))
        (cons "truncatedTo" (lambda (x u) (ldt-combine (ldt-date x) (lt-truncate (ldt-time x) u))))
        (cons "isBefore" (lambda (x o) (ldt<? x o)))
        (cons "isAfter" (lambda (x o) (ldt<? o x)))
        (cons "isEqual" (lambda (x o) (ldt=? x o)))
        (cons "compareTo" (lambda (x o) (ldt-cmp x o)))
        (cons "equals" (lambda (x o) (and (jhost? o) (string=? (jhost-tag o) "local-date-time") (ldt=? x o))))
        (cons "hashCode" (lambda (x) (+ (* (ldt-epoch-day x) 31) (ldt-nano-of-day x))))
        (cons "atZone" (lambda (x zone) (mk-zoned (ldt->ms x))))
        (cons "atOffset" (lambda (x off) (mk-zoned (ldt->ms x))))
        (cons "toInstant" (lambda (x . _) (mk-instant (ldt->ms x))))
        (cons "toString" (lambda (x) (iso-datetime-str (ldt-epoch-day x) (ldt-nano-of-day x))))))

(define (ldt-cmp x o)
  (cond ((< (ldt-epoch-day x) (ldt-epoch-day o)) -1)
        ((> (ldt-epoch-day x) (ldt-epoch-day o)) 1)
        ((< (ldt-nano-of-day x) (ldt-nano-of-day o)) -1)
        ((> (ldt-nano-of-day x) (ldt-nano-of-day o)) 1)
        (else 0)))
(define (ldt<? x o) (< (ldt-cmp x o) 0))
(define (ldt=? x o) (= (ldt-cmp x o) 0))

;; --- Instant (extend the existing inst-time.ss "instant" jhost) --------------
;; ms-granular: nanos plus/minus and getNano round to the millisecond.
(define (inst-ms x) (vector-ref (jhost-state x) 0))
(register-class-statics! "Instant"
  (list (cons "ofEpochSecond" (case-lambda
                                ((s) (mk-instant (* (jt->exact s) 1000)))
                                ((s nano) (mk-instant (+ (* (jt->exact s) 1000) (quotient (jt->exact nano) 1000000))))))
        (cons "EPOCH" (mk-instant 0))
        ;; java.time Instant MIN/MAX are -1e9..1e9 yrs; the ms model can't hold those
        ;; exactly, so use the broadest range the ms layer represents safely.
        (cons "MIN" (mk-instant (* (ymd->epoch-day -999999999 1 1) 86400000)))
        (cons "MAX" (mk-instant (+ (* (ymd->epoch-day 999999999 12 31) 86400000) 86399999)))))

(register-host-methods! "instant"
  (list (cons "getEpochSecond" (lambda (x) (jt-floor-div (exact (truncate (inst-ms x))) 1000)))
        (cons "getNano" (lambda (x) (* (jt-floor-mod (exact (truncate (inst-ms x))) 1000) 1000000)))
        (cons "plusMillis" (lambda (x n) (mk-instant (+ (inst-ms x) (jt->exact n)))))
        (cons "minusMillis" (lambda (x n) (mk-instant (- (inst-ms x) (jt->exact n)))))
        (cons "plusSeconds" (lambda (x n) (mk-instant (+ (inst-ms x) (* (jt->exact n) 1000)))))
        (cons "minusSeconds" (lambda (x n) (mk-instant (- (inst-ms x) (* (jt->exact n) 1000)))))
        (cons "plusNanos" (lambda (x n) (mk-instant (+ (inst-ms x) (quotient (jt->exact n) 1000000)))))
        (cons "minusNanos" (lambda (x n) (mk-instant (- (inst-ms x) (quotient (jt->exact n) 1000000)))))
        (cons "isBefore" (lambda (x o) (< (inst-ms x) (inst-ms o))))
        (cons "isAfter" (lambda (x o) (> (inst-ms x) (inst-ms o))))
        (cons "compareTo" (lambda (x o) (let ((a (inst-ms x)) (b (inst-ms o)))
                                          (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals" (lambda (x o) (and (jhost? o) (string=? (jhost-tag o) "instant") (= (inst-ms x) (inst-ms o)))))
        (cons "hashCode" (lambda (x) (jt->exact (inst-ms x))))
        (cons "truncatedTo" (lambda (x u) (let ((unit (chrono-unit-name u)))
                                            (mk-instant (* (quotient (exact (truncate (inst-ms x)))
                                                                     (cond ((and unit (string-ci=? unit "DAYS")) 86400000)
                                                                           ((and unit (string-ci=? unit "HOURS")) 3600000)
                                                                           ((and unit (string-ci=? unit "MINUTES")) 60000)
                                                                           ((and unit (string-ci=? unit "SECONDS")) 1000)
                                                                           (else 1)))
                                                           (cond ((and unit (string-ci=? unit "DAYS")) 86400000)
                                                                 ((and unit (string-ci=? unit "HOURS")) 3600000)
                                                                 ((and unit (string-ci=? unit "MINUTES")) 60000)
                                                                 ((and unit (string-ci=? unit "SECONDS")) 1000)
                                                                 (else 1)))))))
        (cons "atOffset" (lambda (x off) (mk-zoned (inst-ms x))))
        (cons "toString" (lambda (x) (iso-instant-str (inst-ms x))))))

;; --- Month / DayOfWeek enums (returned by getMonth / getDayOfWeek) -----------
;; minimal: name / getValue / toString, plus the static value fields cljc.java-time
;; might def at load (java.time.Month/JANUARY etc. — not needed by the four core nses
;; but harmless to provide).
(register-host-methods! "month-enum"
  (list (cons "getValue" (lambda (e) (vector-ref (jhost-state e) 0)))
        (cons "name" (lambda (e) (vector-ref jt-month-names (- (vector-ref (jhost-state e) 0) 1))))
        (cons "toString" (lambda (e) (vector-ref jt-month-names (- (vector-ref (jhost-state e) 0) 1))))))
(register-host-methods! "dow-enum"
  (list (cons "getValue" (lambda (e) (vector-ref (jhost-state e) 0)))
        (cons "name" (lambda (e) (vector-ref jt-day-names (- (vector-ref (jhost-state e) 0) 1))))
        (cons "toString" (lambda (e) (vector-ref jt-day-names (- (vector-ref (jhost-state e) 0) 1))))))

;; --- equality / hash / compare / print / instance? --------------------------
(define (jt-date? x) (and (jhost? x) (string=? (jhost-tag x) "local-date")))
(define (jt-time? x) (and (jhost? x) (string=? (jhost-tag x) "local-time")))
(define (jt-dt? x) (and (jhost? x) (string=? (jhost-tag x) "local-date-time")))

(register-eq-arm! (lambda (a b) (or (jt-date? a) (jt-date? b)))
                  (lambda (a b) (and (jt-date? a) (jt-date? b) (= (ld-epoch-day a) (ld-epoch-day b)))))
(register-hash-arm! jt-date? (lambda (x) (jolt-hash (ld-epoch-day x))))
(register-eq-arm! (lambda (a b) (or (jt-time? a) (jt-time? b)))
                  (lambda (a b) (and (jt-time? a) (jt-time? b) (= (lt-nano-of-day a) (lt-nano-of-day b)))))
(register-hash-arm! jt-time? (lambda (x) (jolt-hash (lt-nano-of-day x))))
(register-eq-arm! (lambda (a b) (or (jt-dt? a) (jt-dt? b)))
                  (lambda (a b) (and (jt-dt? a) (jt-dt? b) (ldt=? a b))))
(register-hash-arm! jt-dt? (lambda (x) (jolt-hash (+ (* (ldt-epoch-day x) 31) (ldt-nano-of-day x)))))

(register-str-render! jt-date? (lambda (x) (iso-date-str (ld-epoch-day x))))
(register-pr-arm! jt-date? (lambda (x) (iso-date-str (ld-epoch-day x))))
(register-str-render! jt-time? (lambda (x) (iso-time-str (lt-nano-of-day x))))
(register-pr-arm! jt-time? (lambda (x) (iso-time-str (lt-nano-of-day x))))
(register-str-render! jt-dt? (lambda (x) (iso-datetime-str (ldt-epoch-day x) (ldt-nano-of-day x))))
(register-pr-arm! jt-dt? (lambda (x) (iso-datetime-str (ldt-epoch-day x) (ldt-nano-of-day x))))

;; the "instant" jhost prints as a java.time ISO instant (…Z), not the bare record.
(define (jt-instant? x) (and (jhost? x) (string=? (jhost-tag x) "instant")))
(register-str-render! jt-instant? (lambda (x) (iso-instant-str (inst-ms x))))
(register-pr-arm! jt-instant? (lambda (x) (iso-instant-str (inst-ms x))))

;; compare: same-type java.time values compare on their canonical state.
(define %jt-prev-compare jolt-compare)
(set! jolt-compare
  (lambda (a b)
    (cond
      ((and (jt-date? a) (jt-date? b)) (cond ((< (ld-epoch-day a) (ld-epoch-day b)) -1) ((> (ld-epoch-day a) (ld-epoch-day b)) 1) (else 0)))
      ((and (jt-time? a) (jt-time? b)) (cond ((< (lt-nano-of-day a) (lt-nano-of-day b)) -1) ((> (lt-nano-of-day a) (lt-nano-of-day b)) 1) (else 0)))
      ((and (jt-dt? a) (jt-dt? b)) (ldt-cmp a b))
      ((and (jt-instant? a) (jt-instant? b)) (cond ((< (inst-ms a) (inst-ms b)) -1) ((> (inst-ms a) (inst-ms b)) 1) (else 0)))
      (else (%jt-prev-compare a b)))))
(def-var! "clojure.core" "compare" jolt-compare)

;; instance? for the three new tags (inst-time.ss already answers "instant").
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tn (short-class-name (symbol-t-name type-sym))))
      (cond
        ((jt-date? val) (if (string=? tn "LocalDate") #t 'pass))
        ((jt-time? val) (if (string=? tn "LocalTime") #t 'pass))
        ((jt-dt? val) (if (string=? tn "LocalDateTime") #t 'pass))
        (else 'pass)))))
