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

;; --- libc locale/strftime FFI (graceful-degradation, mutex-guarded) ----------
;; LC_TIME varies by platform: 5 on macOS, 2 on Linux/glibc.
(define LC_TIME
  (case (machine-type)
    ((a6osx t3osx i3osx ta6osx tarm64osx arm64osx) 5)
    (else 2)))
(define tz-mutex (make-mutex))

;; Guard-wrapped FFI via jolt-foreign-proc-safe (deferred symbol lookup so a
;; missing entry doesn't abort boot). nil on failure → graceful fallback.
(define %setlocale (jolt-foreign-proc-safe "setlocale" '(int string) 'void*))
(define %strftime  (jolt-foreign-proc-safe "strftime"  '(u8* size_t string void*) 'size_t))
(define libc-locale-available?
  (and %setlocale %strftime
       (guard (e (#t #f))
         (let ((r (%setlocale LC_TIME "en_US.UTF-8")))
           (and r (not (eq? r 0)))))))
(define libc-locale-checked? #t) ; let runtime inspect the probe

;; --- libc zone-offset FFI (setenv/tzset/localtime, mutex-guarded) ------------
;; setenv("TZ", zone, 1) + tzset() tell libc which timezone to use for localtime.
;; localtime fills a struct tm (56 bytes) with tm_gmtoff at byte 40 (long, 8 bytes).
;; All guarded by tz-mutex — setenv/tzset mutate process-global state.
(define %setenv    (jolt-foreign-proc-safe "setenv"    '(string string int) 'int))
(define %tzset     (jolt-foreign-proc-safe "tzset"     '() 'void))
(define %localtime (jolt-foreign-proc-safe "localtime" '(void*) 'void*))

;; FFI available check only — zone-offset-seconds is defined later.
(define libc-tz-available?* (and %setenv %tzset %localtime))

;; zone offset in seconds east of UTC at an epoch-second instant, via libc.
;; Returns #f when libc is unavailable or the zone is unknown.
;; struct tm layout (64-bit macOS/glibc): 9 ints(36) + pad(4) + long tm_gmtoff(8).
;; localtime returns a STATIC buffer — do NOT foreign-free it.
(define (zone-offset-seconds zone epoch)
  (and libc-tz-available?*
       (with-mutex tz-mutex
         (%setenv "TZ" zone 1)
         (%tzset)
         (let ((tp (foreign-alloc 8)))
           (foreign-set! 'long tp 0 epoch)
           (let ((tm (%localtime tp)))
             (foreign-free tp)
              (and tm (not (eq? tm 0))
                    (foreign-ref 'long tm 40)))))))

;; locale-id → libc locale string (for setlocale).
(define locale->libc-table
  '(("de" . "de_DE.UTF-8") ("fr" . "fr_FR.UTF-8") ("it" . "it_IT.UTF-8")
    ("ja" . "ja_JP.UTF-8") ("es" . "es_ES.UTF-8") ("ko" . "ko_KR.UTF-8")
    ("zh" . "zh_CN.UTF-8") ("pt" . "pt_BR.UTF-8") ("ru" . "ru_RU.UTF-8")
    ("en" . "en_US.UTF-8") ("und" . "en_US.UTF-8")))
(define (locale->libc loc)
  (cond ((not (string? loc)) "C")
        ((let ((e (assoc loc locale->libc-table))) (and e (cdr e))) => values)
        ((>= (string-length loc) 2)
         (let ((prefix (ascii-string-down (substring loc 0 2))))
           (or (let ((e (assoc prefix locale->libc-table))) (and e (cdr e))) "C")))
        (else "C")))

;; strftime-based locale name lookup. Builds a struct tm, calls strftime under
;; the tz-mutex, decodes the UTF-8 bytevector to a jolt string. Falls back to
;; English (C locale) when libc is unavailable or setlocale returns NULL.
(define (locale-name-via-strftime locale tm-mon tm-wday fmt full?)
  (if libc-locale-available?
      (let* ((libc-loc (locale->libc locale))
             (buf (make-bytevector 128))
             (tm (foreign-alloc 56)))      ; struct tm
        (foreign-set! 'integer-32 tm 0 0)    ; tm_sec
        (foreign-set! 'integer-32 tm 4 0)    ; tm_min
        (foreign-set! 'integer-32 tm 8 0)    ; tm_hour
        (foreign-set! 'integer-32 tm 12 1)   ; tm_mday
        (foreign-set! 'integer-32 tm 16 tm-mon) ; tm_mon
        (foreign-set! 'integer-32 tm 20 70)  ; tm_year (1970)
        (foreign-set! 'integer-32 tm 24 tm-wday) ; tm_wday
        (foreign-set! 'integer-32 tm 28 0)   ; tm_yday
        (foreign-set! 'integer-32 tm 32 -1)  ; tm_isdst (unknown)
        (let ((result
               (with-mutex tz-mutex
                 (let ((saved (%setlocale LC_TIME libc-loc)))
                   (if saved
                       (let ((n (%strftime buf 128 fmt tm)))
                         (%setlocale LC_TIME "C")
                         (and (> n 0) n))
                       (begin
                         (when saved (%setlocale LC_TIME saved))
                         #f))))))
          (let ((r result))
            (foreign-free tm)
            (if r
                (let* ((bv (make-bytevector r)))
                  (bytevector-copy! buf 0 bv 0 r)
                  (utf8->string bv))
                (begin
                  (english-locale-name tm-mon tm-wday fmt full?))))))
      (english-locale-name tm-mon tm-wday fmt full?)))

(define (english-locale-name tm-mon tm-wday fmt full?)
  (define en-months
    (if full?
        (vector "January" "February" "March" "April" "May" "June" "July"
                "August" "September" "October" "November" "December")
        (vector "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
  (define en-days
    (if full?
        (vector "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
        (vector "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))
  (cond ((string=? fmt "%B") (vector-ref en-months tm-mon))
        ((string=? fmt "%b") (vector-ref en-months tm-mon))
        ((string=? fmt "%A") (vector-ref en-days tm-wday))
        ((string=? fmt "%a") (vector-ref en-days tm-wday))
        (else "???")))

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
;; nano-precise ISO instant. The fraction is shown in groups of 3 digits (millis,
;; micros, or nanos), matching DateTimeFormatter.ISO_INSTANT.
(define (iso-instant-str-nanos en)
  (let* ((secs (jt-floor-div en nanos-per-sec))
         (nano (jt-floor-mod en nanos-per-sec))
         (ed (jt-floor-div secs 86400))
         (sod (jt-floor-mod secs 86400)))
    (string-append (iso-date-str ed) "T"
                   (pad2 (quotient sod 3600)) ":" (pad2 (modulo (quotient sod 60) 60)) ":" (pad2 (modulo sod 60))
                   (cond ((= nano 0) "")
                         ((= 0 (modulo nano 1000000)) (string-append "." (frac-fixed nano 3)))
                         ((= 0 (modulo nano 1000)) (string-append "." (frac-fixed nano 6)))
                         (else (string-append "." (frac-fixed nano 9))))
                   "Z")))
;; nano (0..1e9) -> the leading `digits` of its 9-digit zero-padded form.
(define (frac-fixed nano digits)
  (let ((s9 (let ((s (number->string nano))) (string-append (make-string (max 0 (- 9 (string-length s))) #\0) s))))
    (substring s9 0 digits)))

;; --- ISO parsing -------------------------------------------------------------
(define (jt-str x) (if (string? x) x (jolt-str-render-one x)))
(define (jt-str-replace s old new)
  (let* ((oldn (string-length old)) (newn (string-length new))
         (out (open-output-string)))
    (let loop ((i 0))
      (if (>= i (string-length s))
          (get-output-string out)
          (let ((j (string-index s old i)))
            (if j
                (begin (display (substring s i j) out)
                       (display new out)
                       (loop (+ j oldn)))
                (begin (display (substring s i (string-length s)) out)
                       (get-output-string out))))))))
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

;; the offset/zone suffix start in an ISO instant/offset string: the first
;; Z/z/+/- after the 'T', or #f if local-only. (parse-zone-offset / the [zone]
;; bracket handle the suffix itself.)
(define (iso-offset-pos s)
  (let ((tpos (let loop ((i 0)) (cond ((>= i (string-length s)) 0)
                                      ((memv (string-ref s i) '(#\T #\t)) i)
                                      (else (loop (+ i 1)))))))
    (let loop ((i (+ tpos 1)))
      (cond ((>= i (string-length s)) #f)
            ((memv (string-ref s i) '(#\Z #\z #\+ #\-)) i)
            (else (loop (+ i 1)))))))
;; parse an ISO-8601 instant ("…T…[.fffffffff](Z|±HH:mm)") -> UTC epoch-nanos,
;; preserving full fractional-second precision.
(define (parse-iso-instant-nanos s)
  (let* ((opos (iso-offset-pos s))
         (local (if opos (substring s 0 opos) s))
         (osuf (if opos (substring s opos (string-length s)) "Z"))
         (off (if (or (string=? osuf "Z") (string=? osuf "z")) 0 (parse-zone-offset osuf))))
    (call-with-values (lambda () (parse-iso-datetime local))
      (lambda (ed nod) (- (+ (* ed nanos-per-day) nod) (* off nanos-per-sec))))))

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
        (cons "atZone" (lambda (x zone) (zoned-of-ldt x zone)))
        (cons "atOffset" (lambda (x off) (offset-of-ldt x off)))
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
;; nano-precise: state is epoch-nanos (inst-nanos). inst-ms projects to ms (floor)
;; for the ms-based zone/Date call sites; mk-instant-nanos / inst-nanos own the
;; nano arithmetic.
(define (inst-ms x) (jt-floor-div (inst-nanos x) 1000000))
;; truncatedTo unit -> nanos, or #f for an unsupported unit (no-op).
(define (instant-unit-nanos u)
  (let ((unit (chrono-unit-name u)))
    (cond ((and unit (string-ci=? unit "DAYS")) (* 86400 nanos-per-sec))
          ((and unit (string-ci=? unit "HALF_DAYS")) (* 43200 nanos-per-sec))
          ((and unit (string-ci=? unit "HOURS")) (* 3600 nanos-per-sec))
          ((and unit (string-ci=? unit "MINUTES")) (* 60 nanos-per-sec))
          ((and unit (string-ci=? unit "SECONDS")) nanos-per-sec)
          ((and unit (string-ci=? unit "MILLIS")) 1000000)
          ((and unit (string-ci=? unit "MICROS")) 1000)
          (else 1))))
(register-class-statics! "Instant"
  (list (cons "ofEpochSecond" (case-lambda
                                ((s) (mk-instant-nanos (* (jt->exact s) nanos-per-sec)))
                                ((s nano) (mk-instant-nanos (+ (* (jt->exact s) nanos-per-sec) (jt->exact nano))))))
        (cons "EPOCH" (mk-instant-nanos 0))
        (cons "MIN" (mk-instant-nanos (* (ymd->epoch-day -999999999 1 1) 86400 nanos-per-sec)))
        (cons "MAX" (mk-instant-nanos (+ (* (ymd->epoch-day 999999999 12 31) 86400 nanos-per-sec)
                                         (* 86399 nanos-per-sec) 999999999)))
        ;; nano-precise ISO parse, overriding the ms-granular inst-time.ss parse.
        ;; Falls back to the ms parser for extended-year (±999999999) MIN/MAX strings
        ;; the fixed-width date parser can't read.
        (cons "parse" (lambda (s . _)
                        (let ((str (if (string? s) s (jolt-str-render-one s))))
                          (guard (e (#t (mk-instant (jinst-ms (jolt-inst-from-string str)))))
                            (mk-instant-nanos (parse-iso-instant-nanos str))))))))

(register-host-methods! "instant"
  (list (cons "getEpochSecond" (lambda (x) (jt-floor-div (inst-nanos x) nanos-per-sec)))
        (cons "getNano" (lambda (x) (jt-floor-mod (inst-nanos x) nanos-per-sec)))
        (cons "toEpochMilli" (lambda (x) (jt-floor-div (inst-nanos x) 1000000)))
        (cons "plusMillis" (lambda (x n) (mk-instant-nanos (+ (inst-nanos x) (* (jt->exact n) 1000000)))))
        (cons "minusMillis" (lambda (x n) (mk-instant-nanos (- (inst-nanos x) (* (jt->exact n) 1000000)))))
        (cons "plusSeconds" (lambda (x n) (mk-instant-nanos (+ (inst-nanos x) (* (jt->exact n) nanos-per-sec)))))
        (cons "minusSeconds" (lambda (x n) (mk-instant-nanos (- (inst-nanos x) (* (jt->exact n) nanos-per-sec)))))
        (cons "plusNanos" (lambda (x n) (mk-instant-nanos (+ (inst-nanos x) (jt->exact n)))))
        (cons "minusNanos" (lambda (x n) (mk-instant-nanos (- (inst-nanos x) (jt->exact n)))))
        (cons "isBefore" (lambda (x o) (< (inst-nanos x) (inst-nanos o))))
        (cons "isAfter" (lambda (x o) (> (inst-nanos x) (inst-nanos o))))
        (cons "compareTo" (lambda (x o) (let ((a (inst-nanos x)) (b (inst-nanos o)))
                                          (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals" (lambda (x o) (and (jhost? o) (string=? (jhost-tag o) "instant") (= (inst-nanos x) (inst-nanos o)))))
        (cons "hashCode" (lambda (x) (inst-nanos x)))
        (cons "truncatedTo" (lambda (x u) (let ((d (instant-unit-nanos u)))
                                            (mk-instant-nanos (* (jt-floor-div (inst-nanos x) d) d)))))
        (cons "atOffset" (lambda (x off) (offset-of-instant-nanos (inst-nanos x) off)))
        (cons "atZone" (lambda (x zone) (zoned-of-instant-nanos (inst-nanos x) zone)))
        (cons "toString" (lambda (x) (iso-instant-str-nanos (inst-nanos x))))))

;; --- Month / DayOfWeek enums (returned by getMonth / getDayOfWeek) -----------
(define (jt-month n) (make-jhost "month-enum" (vector n)))
(define (jt-dow n) (make-jhost "dow-enum" (vector n)))
(define (month-val e) (vector-ref (jhost-state e) 0))
(define (dow-val e) (vector-ref (jhost-state e) 0))
(define (month-name n) (vector-ref jt-month-names (- n 1)))
(define (dow-name n) (vector-ref jt-day-names (- n 1)))
;; quarter starts: Jan/Apr/Jul/Oct.
(define (month-quarter-start m) (+ 1 (* 3 (quotient (- m 1) 3))))
;; day-of-year of the first day of month m (non-leap base; +1 from March on a leap year).
(define (month-first-day-of-year m leap)
  (let ((cum (vector 0 0 31 59 90 120 151 181 212 243 273 304 334)))
    (+ 1 (vector-ref cum m) (if (and leap (> m 2)) 1 0))))

(register-host-methods! "month-enum"
  (list (cons "getValue" (lambda (e) (month-val e)))
        (cons "ordinal" (lambda (e) (- (month-val e) 1)))
        (cons "name" (lambda (e) (month-name (month-val e))))
        (cons "toString" (lambda (e) (month-name (month-val e))))
        (cons "getDisplayName" (lambda (e . _) (month-name (month-val e))))
        (cons "plus" (lambda (e n) (jt-month (+ 1 (jt-floor-mod (+ (- (month-val e) 1) (jt->exact n)) 12)))))
        (cons "minus" (lambda (e n) (jt-month (+ 1 (jt-floor-mod (- (- (month-val e) 1) (jt->exact n)) 12)))))
        (cons "length" (lambda (e leap) (jt-len-of-month (if (jolt-truthy? leap) 4 1) (month-val e))))
        (cons "minLength" (lambda (e) (if (= (month-val e) 2) 28 (jt-len-of-month 1 (month-val e)))))
        (cons "maxLength" (lambda (e) (if (= (month-val e) 2) 29 (jt-len-of-month 1 (month-val e)))))
        (cons "firstMonthOfQuarter" (lambda (e) (jt-month (month-quarter-start (month-val e)))))
        (cons "firstDayOfYear" (lambda (e leap) (month-first-day-of-year (month-val e) (jolt-truthy? leap))))
        (cons "compareTo" (lambda (e o) (- (month-val e) (month-val o))))
        (cons "equals" (lambda (e o) (and (jhost? o) (string=? (jhost-tag o) "month-enum") (= (month-val e) (month-val o)))))
        (cons "hashCode" (lambda (e) (month-val e)))))
(register-host-methods! "dow-enum"
  (list (cons "getValue" (lambda (e) (dow-val e)))
        (cons "ordinal" (lambda (e) (- (dow-val e) 1)))
        (cons "name" (lambda (e) (dow-name (dow-val e))))
        (cons "toString" (lambda (e) (dow-name (dow-val e))))
        (cons "getDisplayName" (lambda (e . _) (dow-name (dow-val e))))
        (cons "plus" (lambda (e n) (jt-dow (+ 1 (jt-floor-mod (+ (- (dow-val e) 1) (jt->exact n)) 7)))))
        (cons "minus" (lambda (e n) (jt-dow (+ 1 (jt-floor-mod (- (- (dow-val e) 1) (jt->exact n)) 7)))))
        (cons "compareTo" (lambda (e o) (- (dow-val e) (dow-val o))))
        (cons "equals" (lambda (e o) (and (jhost? o) (string=? (jhost-tag o) "dow-enum") (= (dow-val e) (dow-val o)))))
        (cons "hashCode" (lambda (e) (dow-val e)))))

(define (month-from-temporal t)
  (cond ((and (jhost? t) (string=? (jhost-tag t) "month-enum")) t)
        ((jt-date? t) (jt-month (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day t))) (lambda (y m d) m))))
        ((jt-dt? t) (jt-month (call-with-values (lambda () (epoch-day->ymd (ldt-epoch-day t))) (lambda (y m d) m))))
        (else (error #f "Month/from: unsupported"))))
(register-class-statics! "Month"
  (append
   (list (cons "of" (lambda (n) (jt-month (jt->exact n))))
         (cons "valueOf" (lambda (s) (let ((nm (jt-str s)))
                                       (let loop ((i 0)) (cond ((= i 12) (error #f (string-append "No enum constant Month." nm)))
                                                               ((string=? (vector-ref jt-month-names i) nm) (jt-month (+ i 1)))
                                                               (else (loop (+ i 1))))))))
         (cons "from" (lambda (t) (month-from-temporal t)))
         (cons "values" (lambda () (make-pvec (list->vector (map jt-month '(1 2 3 4 5 6 7 8 9 10 11 12)))))))
   (map (lambda (i) (cons (vector-ref jt-month-names (- i 1)) (jt-month i))) '(1 2 3 4 5 6 7 8 9 10 11 12))))
(define (dow-from-temporal t)
  (cond ((and (jhost? t) (string=? (jhost-tag t) "dow-enum")) t)
        ((jt-date? t) (jt-dow (ld-dow (ld-epoch-day t))))
        ((jt-dt? t) (jt-dow (ld-dow (ldt-epoch-day t))))
        (else (error #f "DayOfWeek/from: unsupported"))))
(register-class-statics! "DayOfWeek"
  (append
   (list (cons "of" (lambda (n) (jt-dow (jt->exact n))))
         (cons "valueOf" (lambda (s) (let ((nm (jt-str s)))
                                       (let loop ((i 0)) (cond ((= i 7) (error #f (string-append "No enum constant DayOfWeek." nm)))
                                                               ((string=? (vector-ref jt-day-names i) nm) (jt-dow (+ i 1)))
                                                               (else (loop (+ i 1))))))))
         (cons "from" (lambda (t) (dow-from-temporal t)))
         (cons "values" (lambda () (make-pvec (list->vector (map jt-dow '(1 2 3 4 5 6 7)))))))
   (map (lambda (i) (cons (vector-ref jt-day-names (- i 1)) (jt-dow i))) '(1 2 3 4 5 6 7))))

;; --- Duration: (vector seconds nanos), nanos in [0, 1e9) ----------------------
(define (dur-normalize secs nanos)
  (let* ((carry (jt-floor-div nanos nanos-per-sec))
         (n (jt-floor-mod nanos nanos-per-sec)))
    (make-jhost "duration" (vector (+ secs carry) n))))
(define (dur-secs d) (vector-ref (jhost-state d) 0))
(define (dur-nanos d) (vector-ref (jhost-state d) 1))
(define (dur-total-nanos d) (+ (* (dur-secs d) nanos-per-sec) (dur-nanos d)))
(define (dur-of-total-nanos tn) (dur-normalize (jt-floor-div tn nanos-per-sec) (jt-floor-mod tn nanos-per-sec)))
(define dur-zero (make-jhost "duration" (vector 0 0)))

;; ISO-8601: PTnHnMnS. Components come from the normalized (secs,nanos>=0) state;
;; each H/M/S field carries its own sign (java.time prints them per-component). The
;; seconds field folds in the fractional nanos: a negative second with nanos shows
;; e.g. "-0.5" (rem-sec -1 + nanos 5e8 -> -0.5).
(define (dur->string d)
  (let ((secs (dur-secs d)) (nanos (dur-nanos d)))
    (if (and (= secs 0) (= nanos 0)) "PT0S"
        (let* ((hours (quotient secs 3600))
               (mins (quotient (remainder secs 3600) 60))
               (rem-secs (remainder secs 60))
               (out (open-output-string)))
          (display "PT" out)
          (unless (= hours 0) (display (number->string hours) out) (write-char #\H out))
          (unless (= mins 0) (display (number->string mins) out) (write-char #\M out))
          (when (or (not (= rem-secs 0)) (not (= nanos 0)) (and (= hours 0) (= mins 0)))
            (if (= nanos 0)
                (display (number->string rem-secs) out)
                ;; whole second + fraction; a negative rem-sec with positive nanos
                ;; rolls toward zero (rem+1) and shows fraction as 1e9-nanos.
                (let* ((neg (< rem-secs 0))
                       (whole (if neg (+ rem-secs 1) rem-secs))
                       (frac (if neg (- nanos-per-sec nanos) nanos)))
                  (when (and neg (= whole 0)) (write-char #\- out))
                  (display (number->string whole) out)
                  (write-char #\. out)
                  (display (dur-frac-digits frac) out)))
            (write-char #\S out))
          (get-output-string out)))))
;; nanos -> a 9-digit fraction with all trailing zeros stripped (Duration shows the
;; minimal fraction, e.g. .5, unlike LocalTime which pads to 3/6/9).
(define (dur-frac-digits nano)
  (let ((s9 (let ((s (number->string nano))) (string-append (make-string (max 0 (- 9 (string-length s))) #\0) s))))
    (let loop ((i 9)) (cond ((<= i 1) (substring s9 0 1))
                            ((char=? (string-ref s9 (- i 1)) #\0) (loop (- i 1)))
                            (else (substring s9 0 i))))))

(define (dur-temporal-nanos t)   ; instant/ldt/lt/zoned/offset -> a nanos count
  (cond ((jt-instant? t) (inst-nanos t))
        ((jt-dt? t) (+ (* (ldt-epoch-day t) nanos-per-day) (ldt-nano-of-day t)))
        ((jt-time? t) (lt-nano-of-day t))
        ((jt-date? t) (* (ld-epoch-day t) nanos-per-day))
        ;; zoned/offset date-times measure on the instant timeline (UTC nanos).
        ((jt-zoned-dt? t) (zdt->nanos t))
        ((jt-offset-dt? t) (odt->nanos t))
        (else (error #f "Duration/between: unsupported temporal"))))
(register-class-statics! "Duration"
  (list (cons "ZERO" dur-zero)
        (cons "of" (lambda (n unit) (dur-of-total-nanos (* (jt->exact n) (chrono-unit-nanos unit)))))
        (cons "ofDays" (lambda (n) (dur-normalize (* (jt->exact n) 86400) 0)))
        (cons "ofHours" (lambda (n) (dur-normalize (* (jt->exact n) 3600) 0)))
        (cons "ofMinutes" (lambda (n) (dur-normalize (* (jt->exact n) 60) 0)))
        (cons "ofSeconds" (case-lambda
                            ((s) (dur-normalize (jt->exact s) 0))
                            ((s na) (dur-normalize (jt->exact s) (jt->exact na)))))
        (cons "ofMillis" (lambda (n) (dur-of-total-nanos (* (jt->exact n) 1000000))))
        (cons "ofNanos" (lambda (n) (dur-of-total-nanos (jt->exact n))))
        (cons "between" (lambda (a b) (dur-of-total-nanos (- (dur-temporal-nanos b) (dur-temporal-nanos a)))))
        (cons "from" (lambda (t) (if (and (jhost? t) (string=? (jhost-tag t) "duration")) t (error #f "Duration/from"))))
        (cons "parse" (lambda (s) (parse-iso-duration (jt-str s))))))

(define (dur-plus a b) (dur-of-total-nanos (+ (dur-total-nanos a) (dur-total-nanos b))))
(register-host-methods! "duration"
  (list (cons "getSeconds" (lambda (d) (dur-secs d)))
        (cons "getNano" (lambda (d) (dur-nanos d)))
        (cons "toDays" (lambda (d) (quotient (dur-secs d) 86400)))
        (cons "toHours" (lambda (d) (quotient (dur-secs d) 3600)))
        (cons "toMinutes" (lambda (d) (quotient (dur-secs d) 60)))
        ;; toMillis/toSeconds truncate the TOTAL toward zero (JVM rounds the whole
        ;; value, not per-field): -1001 micros -> -1 ms, not -2.
        (cons "toMillis" (lambda (d) (quotient (dur-total-nanos d) 1000000)))
        (cons "toNanos" (lambda (d) (dur-total-nanos d)))
        (cons "plus" (lambda (d o) (cond ((and (jhost? o) (string=? (jhost-tag o) "duration")) (dur-plus d o))
                                         (else (error #f "Duration.plus: expected Duration")))))
        (cons "minus" (lambda (d o) (dur-of-total-nanos (- (dur-total-nanos d) (dur-total-nanos o)))))
        (cons "plusDays" (lambda (d n) (dur-of-total-nanos (+ (dur-total-nanos d) (* (jt->exact n) 86400 nanos-per-sec)))))
        (cons "plusHours" (lambda (d n) (dur-of-total-nanos (+ (dur-total-nanos d) (* (jt->exact n) 3600 nanos-per-sec)))))
        (cons "plusMinutes" (lambda (d n) (dur-of-total-nanos (+ (dur-total-nanos d) (* (jt->exact n) 60 nanos-per-sec)))))
        (cons "plusSeconds" (lambda (d n) (dur-of-total-nanos (+ (dur-total-nanos d) (* (jt->exact n) nanos-per-sec)))))
        (cons "plusMillis" (lambda (d n) (dur-of-total-nanos (+ (dur-total-nanos d) (* (jt->exact n) 1000000)))))
        (cons "plusNanos" (lambda (d n) (dur-of-total-nanos (+ (dur-total-nanos d) (jt->exact n)))))
        (cons "minusDays" (lambda (d n) (dur-of-total-nanos (- (dur-total-nanos d) (* (jt->exact n) 86400 nanos-per-sec)))))
        (cons "minusHours" (lambda (d n) (dur-of-total-nanos (- (dur-total-nanos d) (* (jt->exact n) 3600 nanos-per-sec)))))
        (cons "minusMinutes" (lambda (d n) (dur-of-total-nanos (- (dur-total-nanos d) (* (jt->exact n) 60 nanos-per-sec)))))
        (cons "minusSeconds" (lambda (d n) (dur-of-total-nanos (- (dur-total-nanos d) (* (jt->exact n) nanos-per-sec)))))
        (cons "minusMillis" (lambda (d n) (dur-of-total-nanos (- (dur-total-nanos d) (* (jt->exact n) 1000000)))))
        (cons "minusNanos" (lambda (d n) (dur-of-total-nanos (- (dur-total-nanos d) (jt->exact n)))))
        (cons "multipliedBy" (lambda (d n) (dur-of-total-nanos (* (dur-total-nanos d) (jt->exact n)))))
        (cons "dividedBy" (lambda (d n) (dur-of-total-nanos (quotient (dur-total-nanos d) (jt->exact n)))))
        (cons "negated" (lambda (d) (dur-of-total-nanos (- (dur-total-nanos d)))))
        (cons "abs" (lambda (d) (dur-of-total-nanos (abs (dur-total-nanos d)))))
        (cons "withSeconds" (lambda (d s) (dur-normalize (jt->exact s) (dur-nanos d))))
        (cons "withNanos" (lambda (d na) (dur-normalize (dur-secs d) (jt->exact na))))
        (cons "isZero" (lambda (d) (and (= (dur-secs d) 0) (= (dur-nanos d) 0))))
        (cons "isNegative" (lambda (d) (< (dur-total-nanos d) 0)))
        (cons "compareTo" (lambda (d o) (let ((a (dur-total-nanos d)) (b (dur-total-nanos o)))
                                          (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals" (lambda (d o) (and (jhost? o) (string=? (jhost-tag o) "duration") (= (dur-total-nanos d) (dur-total-nanos o)))))
        (cons "hashCode" (lambda (d) (jolt-hash (dur-total-nanos d))))
        ;; TemporalAmount: addTo/subtractFrom apply this duration to a temporal.
        (cons "addTo" (lambda (d t) (temporal-plus-nanos t (dur-total-nanos d))))
        (cons "subtractFrom" (lambda (d t) (temporal-plus-nanos t (- (dur-total-nanos d)))))
        ;; TemporalAmount: a Duration is measured in SECONDS + NANOS.
        (cons "getUnits" (lambda (d) (make-pvec (vector (jt-chrono-unit "SECONDS") (jt-chrono-unit "NANOS")))))
        (cons "get" (lambda (d u) (let ((nm (string-upcase (arg-unit-name u))))
                                    (cond ((string=? nm "SECONDS") (dur-secs d))
                                          ((string=? nm "NANOS") (dur-nanos d))
                                          (else (error #f (string-append "Duration has no unit " nm)))))))
        (cons "toString" (lambda (d) (dur->string d)))))

;; "PT…" parse: PnDTnHnMn.nS (we accept the time part; days fold into hours).
(define (parse-iso-duration s)
  (let ((len (string-length s)) (neg #f) (i 0) (total 0))
    (define (sign-at j) (cond ((>= j len) (values 1 j))
                              ((char=? (string-ref s j) #\-) (values -1 (+ j 1)))
                              ((char=? (string-ref s j) #\+) (values 1 (+ j 1)))
                              (else (values 1 j))))
    (when (and (< i len) (char=? (string-ref s i) #\-)) (set! neg #t) (set! i (+ i 1)))
    (unless (and (< i len) (or (char=? (string-ref s i) #\P) (char=? (string-ref s i) #\p)))
      (error #f (string-append "could not parse Duration: " s)))
    (set! i (+ i 1))
    (let loop ((in-time #f))
      (when (< i len)
        (let ((c (string-ref s i)))
          (cond
            ((or (char=? c #\T) (char=? c #\t)) (set! i (+ i 1)) (loop #t))
            (else
             (call-with-values (lambda () (sign-at i))
               (lambda (sg j)
                 ;; read number (with optional fraction)
                 (let num ((k j) (acc 0) (frac 0) (fdigits 0) (in-frac #f) (any #f))
                   (cond
                     ((and (< k len) (digit? (string-ref s k)))
                      (if in-frac
                          (num (+ k 1) acc (+ (* frac 10) (- (char->integer (string-ref s k)) 48)) (+ fdigits 1) #t #t)
                          (num (+ k 1) (+ (* acc 10) (- (char->integer (string-ref s k)) 48)) frac fdigits #f #t)))
                     ((and (< k len) (char=? (string-ref s k) #\.)) (num (+ k 1) acc frac fdigits #t any))
                     ((and any (< k len))
                      (let* ((unit (char-upcase (string-ref s k)))
                             (nanos (* (+ (* acc nanos-per-sec) (* frac (expt 10 (max 0 (- 9 fdigits))))) sg))
                             (mult (cond ((char=? unit #\D) 86400) ((char=? unit #\H) 3600)
                                         ((char=? unit #\M) (if in-time 60 (error #f "Duration months unsupported")))
                                         ((char=? unit #\S) 1)
                                         (else (error #f (string-append "bad Duration unit: " s))))))
                        (set! total (+ total (* nanos mult)))
                        (set! i (+ k 1))
                        (loop in-time)))
                     (else (error #f (string-append "could not parse Duration: " s))))))))))))
    (dur-of-total-nanos (if neg (- total) total))))

;; --- Period: (vector years months days) --------------------------------------
(define (jt-period y m d) (make-jhost "period" (vector y m d)))
(define (per-years p) (vector-ref (jhost-state p) 0))
(define (per-months p) (vector-ref (jhost-state p) 1))
(define (per-days p) (vector-ref (jhost-state p) 2))
(define per-zero (jt-period 0 0 0))
(define (per->string p)
  (let ((y (per-years p)) (m (per-months p)) (d (per-days p)))
    (if (and (= y 0) (= m 0) (= d 0)) "P0D"
        (let ((out (open-output-string)))
          (write-char #\P out)
          (unless (= y 0) (display (number->string y) out) (write-char #\Y out))
          (unless (= m 0) (display (number->string m) out) (write-char #\M out))
          (unless (= d 0) (display (number->string d) out) (write-char #\D out))
          (get-output-string out)))))
;; Period/between counts whole years/months/days from a to b (java.time semantics).
(define (per-between a b)
  (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day a)))
    (lambda (y1 m1 d1)
      (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day b)))
        (lambda (y2 m2 d2)
          (let* ((total-months (- (+ (* y2 12) m2) (+ (* y1 12) m1)))
                 (days (- d2 d1)))
            ;; if days go negative, borrow a month (using the prior month length of b)
            (let-values (((tm dd)
                          (if (and (> total-months 0) (< days 0))
                              (let* ((tm (- total-months 1))
                                     (bm (+ (* y1 12) (- m1 1) tm))
                                     (by (jt-floor-div bm 12)) (bmo (+ 1 (jt-floor-mod bm 12)))
                                     (len (jt-len-of-month by bmo)))
                                (values tm (+ days len)))
                              (if (and (< total-months 0) (> days 0))
                                  (values (+ total-months 1) (- days (jt-len-of-month y2 m2)))
                                  (values total-months days)))))
              (jt-period (jt-floor-div tm 12) (jt-floor-mod tm 12) dd))))))))
(define (per-normalize p)
  (let ((tm (+ (* (per-years p) 12) (per-months p))))
    (jt-period (jt-floor-div tm 12) (jt-floor-mod tm 12) (per-days p))))
(register-class-statics! "Period"
  (list (cons "ZERO" per-zero)
        (cons "of" (lambda (y m d) (jt-period (jt->exact y) (jt->exact m) (jt->exact d))))
        (cons "ofYears" (lambda (y) (jt-period (jt->exact y) 0 0)))
        (cons "ofMonths" (lambda (m) (jt-period 0 (jt->exact m) 0)))
        (cons "ofWeeks" (lambda (w) (jt-period 0 0 (* 7 (jt->exact w)))))
        (cons "ofDays" (lambda (d) (jt-period 0 0 (jt->exact d))))
        (cons "between" (lambda (a b) (per-between a b)))
        (cons "from" (lambda (t) (if (and (jhost? t) (string=? (jhost-tag t) "period")) t (error #f "Period/from"))))
        (cons "parse" (lambda (s) (parse-iso-period (jt-str s))))))
(register-host-methods! "period"
  (list (cons "getYears" (lambda (p) (per-years p)))
        (cons "getMonths" (lambda (p) (per-months p)))
        (cons "getDays" (lambda (p) (per-days p)))
        (cons "toTotalMonths" (lambda (p) (+ (* (per-years p) 12) (per-months p))))
        (cons "plusYears" (lambda (p n) (jt-period (+ (per-years p) (jt->exact n)) (per-months p) (per-days p))))
        (cons "plusMonths" (lambda (p n) (jt-period (per-years p) (+ (per-months p) (jt->exact n)) (per-days p))))
        (cons "plusDays" (lambda (p n) (jt-period (per-years p) (per-months p) (+ (per-days p) (jt->exact n)))))
        (cons "minusYears" (lambda (p n) (jt-period (- (per-years p) (jt->exact n)) (per-months p) (per-days p))))
        (cons "minusMonths" (lambda (p n) (jt-period (per-years p) (- (per-months p) (jt->exact n)) (per-days p))))
        (cons "minusDays" (lambda (p n) (jt-period (per-years p) (per-months p) (- (per-days p) (jt->exact n)))))
        (cons "withYears" (lambda (p n) (jt-period (jt->exact n) (per-months p) (per-days p))))
        (cons "withMonths" (lambda (p n) (jt-period (per-years p) (jt->exact n) (per-days p))))
        (cons "withDays" (lambda (p n) (jt-period (per-years p) (per-months p) (jt->exact n))))
        (cons "plus" (lambda (p o) (jt-period (+ (per-years p) (per-years o)) (+ (per-months p) (per-months o)) (+ (per-days p) (per-days o)))))
        (cons "minus" (lambda (p o) (jt-period (- (per-years p) (per-years o)) (- (per-months p) (per-months o)) (- (per-days p) (per-days o)))))
        (cons "multipliedBy" (lambda (p n) (jt-period (* (per-years p) (jt->exact n)) (* (per-months p) (jt->exact n)) (* (per-days p) (jt->exact n)))))
        (cons "negated" (lambda (p) (jt-period (- (per-years p)) (- (per-months p)) (- (per-days p)))))
        (cons "normalized" (lambda (p) (per-normalize p)))
        (cons "isZero" (lambda (p) (and (= (per-years p) 0) (= (per-months p) 0) (= (per-days p) 0))))
        (cons "isNegative" (lambda (p) (or (< (per-years p) 0) (< (per-months p) 0) (< (per-days p) 0))))
        (cons "equals" (lambda (p o) (and (jhost? o) (string=? (jhost-tag o) "period")
                                          (= (per-years p) (per-years o)) (= (per-months p) (per-months o)) (= (per-days p) (per-days o)))))
        (cons "hashCode" (lambda (p) (+ (per-years p) (bitwise-arithmetic-shift-left (per-months p) 8) (bitwise-arithmetic-shift-left (per-days p) 16))))
        ;; TemporalAmount: a Period adds its y/m/d to a date-bearing temporal.
        (cons "addTo" (lambda (p t) (temporal-plus-period t p 1)))
        (cons "subtractFrom" (lambda (p t) (temporal-plus-period t p -1)))
        ;; TemporalAmount: a Period is measured in YEARS + MONTHS + DAYS.
        (cons "getUnits" (lambda (p) (make-pvec (vector (jt-chrono-unit "YEARS") (jt-chrono-unit "MONTHS") (jt-chrono-unit "DAYS")))))
        (cons "get" (lambda (p u) (let ((nm (string-upcase (arg-unit-name u))))
                                    (cond ((string=? nm "YEARS") (per-years p))
                                          ((string=? nm "MONTHS") (per-months p))
                                          ((string=? nm "DAYS") (per-days p))
                                          (else (error #f (string-append "Period has no unit " nm)))))))
        (cons "toString" (lambda (p) (per->string p)))))
;; "PnYnMnWnD" -> a Period (weeks fold into days).
(define (parse-iso-period s)
  (let ((len (string-length s)) (sign 1) (i 0) (y 0) (m 0) (d 0))
    (when (and (< i len) (char=? (string-ref s i) #\-)) (set! sign -1) (set! i (+ i 1)))
    (unless (and (< i len) (or (char=? (string-ref s i) #\P) (char=? (string-ref s i) #\p)))
      (error #f (string-append "could not parse Period: " s)))
    (set! i (+ i 1))
    (let loop ()
      (when (< i len)
        (let ((vsign (cond ((char=? (string-ref s i) #\-) (set! i (+ i 1)) -1)
                           ((char=? (string-ref s i) #\+) (set! i (+ i 1)) 1)
                           (else 1))))
          (let num ((k i) (acc 0) (any #f))
            (cond
              ((and (< k len) (digit? (string-ref s k))) (num (+ k 1) (+ (* acc 10) (- (char->integer (string-ref s k)) 48)) #t))
              ((and any (< k len))
               (let ((u (char-upcase (string-ref s k))) (val (* vsign acc)))
                 (cond ((char=? u #\Y) (set! y val)) ((char=? u #\M) (set! m val))
                       ((char=? u #\W) (set! d (+ d (* 7 val)))) ((char=? u #\D) (set! d (+ d val)))
                       (else (error #f (string-append "bad Period unit: " s))))
                 (set! i (+ k 1)) (loop)))
              (else (error #f (string-append "could not parse Period: " s))))))))
    (jt-period (* sign y) (* sign m) (* sign d))))

;; --- Year --------------------------------------------------------------------
(define (jt-year y) (make-jhost "year" (vector y)))
(define (year-val y) (vector-ref (jhost-state y) 0))
(define (year-from-temporal t)
  (cond ((and (jhost? t) (string=? (jhost-tag t) "year")) t)
        ((jt-date? t) (jt-year (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day t))) (lambda (y m d) y))))
        ((jt-dt? t) (jt-year (call-with-values (lambda () (epoch-day->ymd (ldt-epoch-day t))) (lambda (y m d) y))))
        ((and (jhost? t) (string=? (jhost-tag t) "year-month")) (jt-year (ym-year t)))
        (else (error #f "Year/from: unsupported"))))
(register-class-statics! "Year"
  (list (cons "of" (lambda (y) (jt-year (jt->exact y))))
        (cons "now" (lambda _ (jt-year (call-with-values (lambda () (epoch-day->ymd (jt-floor-div (exact (truncate (now-ms))) 86400000))) (lambda (y m d) y)))))
        (cons "isLeap" (lambda (y) (jt-leap? (jt->exact y))))
        (cons "parse" (lambda (s . _) (jt-year (or (string->number (jt-str s)) (error #f "could not parse Year")))))
        (cons "from" (lambda (t) (year-from-temporal t)))
        (cons "MIN_VALUE" -999999999)
        (cons "MAX_VALUE" 999999999)))
(register-host-methods! "year"
  (list (cons "getValue" (lambda (y) (year-val y)))
        (cons "isLeap" (lambda (y) (jt-leap? (year-val y))))
        (cons "length" (lambda (y) (if (jt-leap? (year-val y)) 366 365)))
        (cons "plusYears" (lambda (y n) (jt-year (+ (year-val y) (jt->exact n)))))
        (cons "minusYears" (lambda (y n) (jt-year (- (year-val y) (jt->exact n)))))
        (cons "atMonth" (lambda (y m) (jt-year-month (year-val y) (jt->exact m))))
        (cons "atDay" (lambda (y doy) (jt-local-date (+ (ymd->epoch-day (year-val y) 1 1) (- (jt->exact doy) 1)))))
        (cons "atMonthDay" (lambda (y md) (error #f "Year.atMonthDay: MonthDay unsupported")))
        (cons "isBefore" (lambda (y o) (< (year-val y) (year-val o))))
        (cons "isAfter" (lambda (y o) (> (year-val y) (year-val o))))
        (cons "isValidMonthDay" (lambda (y md) #f))
        (cons "compareTo" (lambda (y o) (let ((a (year-val y)) (b (year-val o))) (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals" (lambda (y o) (and (jhost? o) (string=? (jhost-tag o) "year") (= (year-val y) (year-val o)))))
        (cons "hashCode" (lambda (y) (year-val y)))
        (cons "toString" (lambda (y) (number->string (year-val y))))))

;; --- YearMonth: (vector year month) ------------------------------------------
(define (jt-year-month y m)
  (let* ((ym (+ (* y 12) (- m 1))) (y2 (jt-floor-div ym 12)) (m2 (+ 1 (jt-floor-mod ym 12))))
    (make-jhost "year-month" (vector y2 m2))))
(define (ym-year x) (vector-ref (jhost-state x) 0))
(define (ym-month x) (vector-ref (jhost-state x) 1))
(define (ym-plus-months x n)
  (let ((tm (+ (* (ym-year x) 12) (- (ym-month x) 1) n)))
    (make-jhost "year-month" (vector (jt-floor-div tm 12) (+ 1 (jt-floor-mod tm 12))))))
(define (ym->string x) (string-append (pad4 (ym-year x)) "-" (pad2 (ym-month x))))
(define (ym-from-temporal t)
  (cond ((and (jhost? t) (string=? (jhost-tag t) "year-month")) t)
        ((jt-date? t) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day t))) (lambda (y m d) (jt-year-month y m))))
        ((jt-dt? t) (call-with-values (lambda () (epoch-day->ymd (ldt-epoch-day t))) (lambda (y m d) (jt-year-month y m))))
        (else (error #f "YearMonth/from: unsupported"))))
(register-class-statics! "YearMonth"
  (list (cons "of" (lambda (y m) (jt-year-month (jt->exact y) (jt->exact m))))
        (cons "now" (lambda _ (call-with-values (lambda () (epoch-day->ymd (jt-floor-div (exact (truncate (now-ms))) 86400000)))
                                (lambda (y m d) (jt-year-month y m)))))
        (cons "parse" (lambda (s . fmt)
                        (if (null? fmt)
                            (let ((str (jt-str s)))
                              (jt-year-month (or (digits-at str 0 4) (error #f "could not parse YearMonth"))
                                             (or (digits-at str 5 2) (error #f "could not parse YearMonth"))))
                            (call-with-values (lambda () (formatter-parse-fields (car fmt) s))
                              (lambda (ed nod) (call-with-values (lambda () (epoch-day->ymd ed)) (lambda (y m d) (jt-year-month y m))))))))
        (cons "from" (lambda (t) (ym-from-temporal t)))))
(register-host-methods! "year-month"
  (list (cons "getYear" (lambda (x) (ym-year x)))
        (cons "getMonthValue" (lambda (x) (ym-month x)))
        (cons "getMonth" (lambda (x) (jt-month (ym-month x))))
        (cons "lengthOfMonth" (lambda (x) (jt-len-of-month (ym-year x) (ym-month x))))
        (cons "lengthOfYear" (lambda (x) (if (jt-leap? (ym-year x)) 366 365)))
        (cons "isLeapYear" (lambda (x) (jt-leap? (ym-year x))))
        (cons "plusMonths" (lambda (x n) (ym-plus-months x (jt->exact n))))
        (cons "minusMonths" (lambda (x n) (ym-plus-months x (- (jt->exact n)))))
        (cons "plusYears" (lambda (x n) (jt-year-month (+ (ym-year x) (jt->exact n)) (ym-month x))))
        (cons "minusYears" (lambda (x n) (jt-year-month (- (ym-year x) (jt->exact n)) (ym-month x))))
        (cons "withYear" (lambda (x v) (jt-year-month (jt->exact v) (ym-month x))))
        (cons "withMonth" (lambda (x v) (jt-year-month (ym-year x) (jt->exact v))))
        (cons "atDay" (lambda (x d) (jt-local-date (ymd->epoch-day (ym-year x) (ym-month x) (jt->exact d)))))
        (cons "atEndOfMonth" (lambda (x) (jt-local-date (ymd->epoch-day (ym-year x) (ym-month x) (jt-len-of-month (ym-year x) (ym-month x))))))
        (cons "isValidDay" (lambda (x d) (let ((dd (jt->exact d))) (and (>= dd 1) (<= dd (jt-len-of-month (ym-year x) (ym-month x)))))))
        (cons "isBefore" (lambda (x o) (< (ym-cmp x o) 0)))
        (cons "isAfter" (lambda (x o) (> (ym-cmp x o) 0)))
        (cons "compareTo" (lambda (x o) (ym-cmp x o)))
        (cons "equals" (lambda (x o) (and (jhost? o) (string=? (jhost-tag o) "year-month") (= (ym-cmp x o) 0))))
        (cons "hashCode" (lambda (x) (+ (* (ym-year x) 13) (ym-month x))))
        (cons "toString" (lambda (x) (ym->string x)))))
(define (ym-cmp x o)
  (cond ((< (ym-year x) (ym-year o)) -1) ((> (ym-year x) (ym-year o)) 1)
        ((< (ym-month x) (ym-month o)) -1) ((> (ym-month x) (ym-month o)) 1) (else 0)))

;; --- ChronoUnit / ChronoField ------------------------------------------------
;; Each ChronoUnit is a jhost (vector name nanos-or-#f) — nanos is the fixed
;; duration in nanoseconds for time-based units, or an estimate for date-based ones.
;; The conversion lets Duration/of and unit-based between/plus/until work.
(define seconds-per-year (* (/ 146097 400) 86400))   ; 365.2425 days
(define chrono-unit-table
  ;; (name . nanos-per-unit)   month/year use the average-length estimate java.time uses.
  (list (cons "NANOS" 1)
        (cons "MICROS" 1000)
        (cons "MILLIS" 1000000)
        (cons "SECONDS" nanos-per-sec)
        (cons "MINUTES" (* 60 nanos-per-sec))
        (cons "HOURS" (* 3600 nanos-per-sec))
        (cons "HALF_DAYS" (* 43200 nanos-per-sec))
        (cons "DAYS" (* 86400 nanos-per-sec))
        (cons "WEEKS" (* 7 86400 nanos-per-sec))
        (cons "MONTHS" (exact (round (* (/ seconds-per-year 12) nanos-per-sec))))
        (cons "YEARS" (exact (round (* seconds-per-year nanos-per-sec))))
        (cons "DECADES" (exact (round (* 10 seconds-per-year nanos-per-sec))))
        (cons "CENTURIES" (exact (round (* 100 seconds-per-year nanos-per-sec))))
        (cons "MILLENNIA" (exact (round (* 1000 seconds-per-year nanos-per-sec))))
        (cons "ERAS" (exact (round (* 1000000000 seconds-per-year nanos-per-sec))))
        (cons "FOREVER" #f)))
(define (jt-chrono-unit name) (make-jhost "chrono-unit" (vector name)))
(define (cu-name u) (vector-ref (jhost-state u) 0))
(define (chrono-unit-nanos u)
  (let* ((nm (chrono-unit-name u)) (row (and nm (assoc (string-upcase nm) chrono-unit-table))))
    (if (and row (cdr row)) (cdr row) (error #f (string-append "no fixed duration for unit " (or nm "?"))))))
;; register the 16 unit constants under ChronoUnit statics.
(register-class-statics! "ChronoUnit"
  (append
   (map (lambda (row) (cons (car row) (jt-chrono-unit (car row)))) chrono-unit-table)
   (list (cons "valueOf" (lambda (s) (jt-chrono-unit (jt-str s))))
         (cons "values" (lambda () (make-pvec (list->vector (map (lambda (r) (jt-chrono-unit (car r))) chrono-unit-table))))))))
(register-host-methods! "chrono-unit"
  (list (cons "name" (lambda (u) (cu-name u)))
        (cons "toString" (lambda (u) (cu-name u)))
        (cons "ordinal" (lambda (u) (let loop ((t chrono-unit-table) (i 0))
                                      (cond ((null? t) -1) ((string=? (caar t) (cu-name u)) i) (else (loop (cdr t) (+ i 1)))))))
        (cons "getDuration" (lambda (u) (dur-of-total-nanos (chrono-unit-nanos u))))
        (cons "isDateBased" (lambda (u) (and (member (cu-name u) '("DAYS" "WEEKS" "MONTHS" "YEARS" "DECADES" "CENTURIES" "MILLENNIA" "ERAS")) #t)))
        (cons "isTimeBased" (lambda (u) (and (member (cu-name u) '("NANOS" "MICROS" "MILLIS" "SECONDS" "MINUTES" "HOURS" "HALF_DAYS")) #t)))
        (cons "isDurationEstimated" (lambda (u) (and (member (cu-name u) '("DAYS" "WEEKS" "MONTHS" "YEARS" "DECADES" "CENTURIES" "MILLENNIA" "ERAS" "FOREVER")) #t)))
        (cons "between" (lambda (u a b) (unit-between (cu-name u) a b)))
        (cons "addTo" (lambda (u t n) (temporal-plus-unit t (jt->exact n) (cu-name u))))
        (cons "equals" (lambda (u o) (and (jhost? o) (string=? (jhost-tag o) "chrono-unit") (string=? (cu-name u) (cu-name o)))))
        (cons "hashCode" (lambda (u) (string-hash (cu-name u))))))

;; ChronoField: a jhost (vector name). get/getLong/with project the field onto a
;; temporal via the field-projection table below.
;; the full ChronoField enum (the cljc wrapper defs every constant at load). Only
;; the common ones are projected by temporal-get-field; the rest exist as tokens.
(define chrono-field-names
  '("NANO_OF_SECOND" "NANO_OF_DAY" "MICRO_OF_SECOND" "MICRO_OF_DAY" "MILLI_OF_SECOND" "MILLI_OF_DAY"
    "SECOND_OF_MINUTE" "SECOND_OF_DAY" "MINUTE_OF_HOUR" "MINUTE_OF_DAY"
    "HOUR_OF_AMPM" "CLOCK_HOUR_OF_AMPM" "HOUR_OF_DAY" "CLOCK_HOUR_OF_DAY" "AMPM_OF_DAY"
    "DAY_OF_WEEK" "ALIGNED_DAY_OF_WEEK_IN_MONTH" "ALIGNED_DAY_OF_WEEK_IN_YEAR"
    "DAY_OF_MONTH" "DAY_OF_YEAR" "EPOCH_DAY"
    "ALIGNED_WEEK_OF_MONTH" "ALIGNED_WEEK_OF_YEAR"
    "MONTH_OF_YEAR" "PROLEPTIC_MONTH" "YEAR_OF_ERA" "YEAR" "ERA"
    "INSTANT_SECONDS" "OFFSET_SECONDS"))
(define (jt-chrono-field name) (make-jhost "chrono-field" (vector name)))
(define (cf-name f) (vector-ref (jhost-state f) 0))
(register-class-statics! "ChronoField"
  (append
   (map (lambda (n) (cons n (jt-chrono-field n))) chrono-field-names)
   (list (cons "valueOf" (lambda (s) (jt-chrono-field (jt-str s))))
         (cons "values" (lambda () (make-pvec (list->vector (map jt-chrono-field chrono-field-names))))))))
(register-host-methods! "chrono-field"
  (list (cons "name" (lambda (f) (cf-name f)))
        (cons "toString" (lambda (f) (cf-name f)))
        (cons "getDisplayName" (lambda (f . _) (cf-name f)))
        (cons "isDateBased" (lambda (f) (and (member (cf-name f) '("DAY_OF_WEEK" "DAY_OF_MONTH" "DAY_OF_YEAR" "EPOCH_DAY" "MONTH_OF_YEAR" "PROLEPTIC_MONTH" "YEAR_OF_ERA" "YEAR" "ERA")) #t)))
        (cons "isTimeBased" (lambda (f) (not (member (cf-name f) '("DAY_OF_WEEK" "DAY_OF_MONTH" "DAY_OF_YEAR" "EPOCH_DAY" "MONTH_OF_YEAR" "PROLEPTIC_MONTH" "YEAR_OF_ERA" "YEAR" "ERA" "INSTANT_SECONDS" "OFFSET_SECONDS")))))
        (cons "getFrom" (lambda (f t) (temporal-get-field t (cf-name f))))
        (cons "equals" (lambda (f o) (and (jhost? o) (string=? (jhost-tag o) "chrono-field") (string=? (cf-name f) (cf-name o)))))
        (cons "hashCode" (lambda (f) (string-hash (cf-name f))))))

;; --- ValueRange (minimal min/max holder) -------------------------------------
(define (jt-value-range smin lmin smax lmax) (make-jhost "value-range" (vector smin lmin smax lmax)))
(register-host-methods! "value-range"
  (list (cons "getMinimum" (lambda (r) (vector-ref (jhost-state r) 1)))
        (cons "getLargestMinimum" (lambda (r) (vector-ref (jhost-state r) 1)))
        (cons "getSmallestMaximum" (lambda (r) (vector-ref (jhost-state r) 2)))
        (cons "getMaximum" (lambda (r) (vector-ref (jhost-state r) 3)))
        (cons "isFixed" (lambda (r) (and (= (vector-ref (jhost-state r) 0) (vector-ref (jhost-state r) 1))
                                         (= (vector-ref (jhost-state r) 2) (vector-ref (jhost-state r) 3)))))
        (cons "isValidValue" (lambda (r v) (let ((x (jt->exact v))) (and (>= x (vector-ref (jhost-state r) 1)) (<= x (vector-ref (jhost-state r) 3))))))
        (cons "toString" (lambda (r) (string-append (number->string (vector-ref (jhost-state r) 1)) " - " (number->string (vector-ref (jhost-state r) 3)))))))

;; --- temporal field/unit machinery: plus/minus/until/get/with on core types --
;; These power the generic cljc.java-time.temporal dispatchers, which forward to
;; the receiver's plus/minus/until/get/getLong/with/range/isSupported.

;; add n of `unit` to a temporal (local-date / local-time / local-date-time / instant).
(define (temporal-plus-unit t n unit)
  (let ((u (string-upcase unit)))
    (cond
      ((jt-date? t)
       (cond ((string=? u "DAYS") (jt-local-date (+ (ld-epoch-day t) n)))
             ((string=? u "WEEKS") (jt-local-date (+ (ld-epoch-day t) (* 7 n))))
             ((string=? u "MONTHS") (ld-plus-months t n))
             ((string=? u "YEARS") (ld-plus-years t n))
             ((string=? u "DECADES") (ld-plus-years t (* 10 n)))
             ((string=? u "CENTURIES") (ld-plus-years t (* 100 n)))
             ((string=? u "MILLENNIA") (ld-plus-years t (* 1000 n)))
             (else (error #f (string-append "LocalDate plus unsupported unit " u)))))
      ((jt-time? t) (lt-plus t (* n (chrono-unit-nanos (jt-chrono-unit u)))))
      ((jt-dt? t)
       (cond ((string=? u "DAYS") (jt-local-dt (+ (ldt-epoch-day t) n) (ldt-nano-of-day t)))
             ((string=? u "WEEKS") (jt-local-dt (+ (ldt-epoch-day t) (* 7 n)) (ldt-nano-of-day t)))
             ((string=? u "MONTHS") (ldt-combine (ld-plus-months (ldt-date t) n) (ldt-time t)))
             ((string=? u "YEARS") (ldt-combine (ld-plus-years (ldt-date t) n) (ldt-time t)))
             ((string=? u "DECADES") (ldt-combine (ld-plus-years (ldt-date t) (* 10 n)) (ldt-time t)))
             ((string=? u "CENTURIES") (ldt-combine (ld-plus-years (ldt-date t) (* 100 n)) (ldt-time t)))
             ((string=? u "MILLENNIA") (ldt-combine (ld-plus-years (ldt-date t) (* 1000 n)) (ldt-time t)))
             (else (ldt-plus-nanos t (* n (chrono-unit-nanos (jt-chrono-unit u)))))))
      ((jt-instant? t)
       (cond ((string=? u "DAYS") (mk-instant-nanos (+ (inst-nanos t) (* n 86400 nanos-per-sec))))
             (else (mk-instant-nanos (+ (inst-nanos t) (* n (chrono-unit-nanos (jt-chrono-unit u))))))))
      (else (error #f "plus: unsupported temporal")))))

;; add raw nanos (for Duration.addTo).
(define (temporal-plus-nanos t nanos)
  (cond ((jt-time? t) (lt-plus t nanos))
        ((jt-dt? t) (ldt-plus-nanos t nanos))
        ((jt-instant? t) (mk-instant-nanos (+ (inst-nanos t) nanos)))
        ((jt-date? t) (jt-local-date (+ (ld-epoch-day t) (quotient nanos (* 86400 nanos-per-sec)))))
        (else (error #f "plus(Duration): unsupported temporal"))))

;; add a Period (scaled by sign) to a date-bearing temporal.
(define (temporal-plus-period t p sign)
  (let ((y (* sign (per-years p))) (m (* sign (per-months p))) (d (* sign (per-days p))))
    (cond ((jt-date? t) (jt-local-date (+ (ld-epoch-day (ld-plus-months (ld-plus-years t y) m)) d)))
          ((jt-dt? t) (let ((nd (ld-plus-months (ld-plus-years (ldt-date t) y) m)))
                        (jt-local-dt (+ (ld-epoch-day nd) d) (ldt-nano-of-day t))))
          (else (error #f "plus(Period): unsupported temporal")))))

;; count whole `unit`s from a to b.
(define (unit-between unit a b)
  (let ((u (string-upcase unit)))
    (cond
      ((and (jt-date? a) (jt-date? b))
       (let ((da (ld-epoch-day a)) (db (ld-epoch-day b)))
         (cond ((string=? u "DAYS") (- db da))
               ((string=? u "WEEKS") (quotient (- db da) 7))
               ((string=? u "MONTHS") (date-months-between da db))
               ((string=? u "YEARS") (quotient (date-months-between da db) 12))
               ((string=? u "DECADES") (quotient (date-months-between da db) 120))
               ((string=? u "CENTURIES") (quotient (date-months-between da db) 1200))
               (else (error #f (string-append "between unsupported unit " u))))))
      ((and (jt-time? a) (jt-time? b)) (quotient (- (lt-nano-of-day b) (lt-nano-of-day a)) (chrono-unit-nanos (jt-chrono-unit u))))
      ((and (jt-dt? a) (jt-dt? b))
       (cond ((member u '("YEARS" "MONTHS" "DECADES" "CENTURIES" "MILLENNIA"))
              (unit-between u (ldt-date a) (ldt-date b)))   ; date-based: ignore time-of-day below months on LDT? use date months adjusted
             (else (quotient (- (+ (* (ldt-epoch-day b) nanos-per-day) (ldt-nano-of-day b))
                                 (+ (* (ldt-epoch-day a) nanos-per-day) (ldt-nano-of-day a)))
                             (chrono-unit-nanos (jt-chrono-unit u))))))
      ((and (jt-instant? a) (jt-instant? b))
       (quotient (- (inst-nanos b) (inst-nanos a)) (chrono-unit-nanos (jt-chrono-unit u))))
      (else (error #f "between: unsupported temporals")))))
;; whole months between two epoch-days (java.time: months, then -1 if day-of-month
;; of b hasn't reached a's).
(define (date-months-between da db)
  (call-with-values (lambda () (epoch-day->ymd da))
    (lambda (y1 m1 d1)
      (call-with-values (lambda () (epoch-day->ymd db))
        (lambda (y2 m2 d2)
          (let ((months (- (+ (* y2 12) m2) (+ (* y1 12) m1))))
            (cond ((and (> months 0) (< d2 d1)) (- months 1))
                  ((and (< months 0) (> d2 d1)) (+ months 1))
                  (else months))))))))

;; field get: project a ChronoField onto a temporal.
(define (temporal-get-field t field)
  (let ((f (string-upcase field)))
    (cond
      ((jt-date? t)
       (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day t)))
         (lambda (y m d)
           (cond ((string=? f "YEAR") y) ((string=? f "MONTH_OF_YEAR") m) ((string=? f "DAY_OF_MONTH") d)
                 ((string=? f "DAY_OF_WEEK") (ld-dow (ld-epoch-day t)))
                 ((string=? f "DAY_OF_YEAR") (ld-day-of-year (ld-epoch-day t)))
                 ((string=? f "EPOCH_DAY") (ld-epoch-day t))
                 ((string=? f "PROLEPTIC_MONTH") (+ (* y 12) (- m 1)))
                 ((string=? f "YEAR_OF_ERA") (if (>= y 1) y (- 1 y)))
                 ((string=? f "ERA") (if (>= y 1) 1 0))
                 ;; aligned-* group the day-of-month/year into 7-day blocks from the
                 ;; 1st (java.time): the within-block weekday is ((n-1) mod 7)+1, the
                 ;; block number is ((n-1) quotient 7)+1.
                 ((string=? f "ALIGNED_DAY_OF_WEEK_IN_MONTH") (+ (modulo (- d 1) 7) 1))
                 ((string=? f "ALIGNED_WEEK_OF_MONTH") (+ (quotient (- d 1) 7) 1))
                 ((string=? f "ALIGNED_DAY_OF_WEEK_IN_YEAR")
                  (+ (modulo (- (ld-day-of-year (ld-epoch-day t)) 1) 7) 1))
                 ((string=? f "ALIGNED_WEEK_OF_YEAR")
                  (+ (quotient (- (ld-day-of-year (ld-epoch-day t)) 1) 7) 1))
                 (else (error #f (string-append "LocalDate has no field " f)))))))
      ((jt-time? t)
       (cond ((string=? f "HOUR_OF_DAY") (lt-hour t)) ((string=? f "MINUTE_OF_HOUR") (lt-minute t))
             ((string=? f "SECOND_OF_MINUTE") (lt-second t)) ((string=? f "NANO_OF_SECOND") (lt-nano t))
             ((string=? f "NANO_OF_DAY") (lt-nano-of-day t))
             ((string=? f "MILLI_OF_DAY") (quotient (lt-nano-of-day t) 1000000))
             ((string=? f "MICRO_OF_DAY") (quotient (lt-nano-of-day t) 1000))
             ((string=? f "SECOND_OF_DAY") (quotient (lt-nano-of-day t) nanos-per-sec))
             ((string=? f "MINUTE_OF_DAY") (quotient (lt-nano-of-day t) (* 60 nanos-per-sec)))
             ((string=? f "MILLI_OF_SECOND") (quotient (lt-nano t) 1000000))
             ((string=? f "MICRO_OF_SECOND") (quotient (lt-nano t) 1000))
             ;; CLOCK_HOUR_OF_DAY is 1..24 (midnight is 24), HOUR_OF_AMPM 0..11,
             ;; CLOCK_HOUR_OF_AMPM 1..12, AMPM_OF_DAY 0 (AM) / 1 (PM).
             ((string=? f "CLOCK_HOUR_OF_DAY") (let ((h (lt-hour t))) (if (= h 0) 24 h)))
             ((string=? f "HOUR_OF_AMPM") (modulo (lt-hour t) 12))
             ((string=? f "CLOCK_HOUR_OF_AMPM") (let ((h (modulo (lt-hour t) 12))) (if (= h 0) 12 h)))
             ((string=? f "AMPM_OF_DAY") (quotient (lt-hour t) 12))
             (else (error #f (string-append "LocalTime has no field " f)))))
      ((jt-dt? t)
       ;; route a field to whichever part supports it (date fields incl. the
       ;; aligned-* group to the date, the rest to the time).
       (if (temporal-supports-field? (ldt-date t) f)
           (temporal-get-field (ldt-date t) f)
           (temporal-get-field (ldt-time t) f)))
      ((jt-instant? t)
       (cond ((string=? f "INSTANT_SECONDS") (jt-floor-div (inst-nanos t) nanos-per-sec))
             ((string=? f "NANO_OF_SECOND") (jt-floor-mod (inst-nanos t) nanos-per-sec))
             ((string=? f "MILLI_OF_SECOND") (jt-floor-div (jt-floor-mod (inst-nanos t) nanos-per-sec) 1000000))
             ((string=? f "MICRO_OF_SECOND") (jt-floor-div (jt-floor-mod (inst-nanos t) nanos-per-sec) 1000))
             (else (error #f (string-append "Instant has no field " f)))))
      ((and (jhost? t) (string=? (jhost-tag t) "year"))
       (let ((y (year-val t)))
         (cond ((string=? f "YEAR") y) ((string=? f "YEAR_OF_ERA") (if (>= y 1) y (- 1 y)))
               ((string=? f "ERA") (if (>= y 1) 1 0))
               (else (error #f (string-append "Year has no field " f))))))
      ((and (jhost? t) (string=? (jhost-tag t) "year-month"))
       (let ((y (ym-year t)) (m (ym-month t)))
         (cond ((string=? f "YEAR") y) ((string=? f "MONTH_OF_YEAR") m)
               ((string=? f "PROLEPTIC_MONTH") (+ (* y 12) (- m 1)))
               ((string=? f "YEAR_OF_ERA") (if (>= y 1) y (- 1 y))) ((string=? f "ERA") (if (>= y 1) 1 0))
               (else (error #f (string-append "YearMonth has no field " f))))))
      (else (error #f "get(field): unsupported temporal")))))

;; field set: (with temporal ChronoField value) -> a new temporal.
(define (temporal-with-field t field v)
  (let ((f (string-upcase field)))
    (cond
      ((jt-date? t)
       (cond ((string=? f "YEAR") (ld-with-field t 'year v)) ((string=? f "MONTH_OF_YEAR") (ld-with-field t 'month v))
             ((string=? f "DAY_OF_MONTH") (ld-with-field t 'day v)) ((string=? f "DAY_OF_YEAR") (ld-with-field t 'day-of-year v))
             ((string=? f "EPOCH_DAY") (jt-local-date v))
             (else (error #f (string-append "LocalDate.with unsupported field " f)))))
      ((jt-time? t)
       (cond ((string=? f "HOUR_OF_DAY") (lt-with t 'hour v)) ((string=? f "MINUTE_OF_HOUR") (lt-with t 'minute v))
             ((string=? f "SECOND_OF_MINUTE") (lt-with t 'second v)) ((string=? f "NANO_OF_SECOND") (lt-with t 'nano v))
             ((string=? f "NANO_OF_DAY") (jt-local-time v))
             (else (error #f (string-append "LocalTime.with unsupported field " f)))))
      ((jt-dt? t)
       (if (member f '("YEAR" "MONTH_OF_YEAR" "DAY_OF_MONTH" "DAY_OF_YEAR" "EPOCH_DAY"))
           (ldt-combine (temporal-with-field (ldt-date t) f v) (ldt-time t))
           (ldt-combine (ldt-date t) (temporal-with-field (ldt-time t) f v))))
      (else (error #f "with(field): unsupported temporal")))))

(define (temporal-supports-unit? t unit)
  (let ((u (string-upcase unit)))
    (cond ((jt-date? t) (and (member u '("DAYS" "WEEKS" "MONTHS" "YEARS" "DECADES" "CENTURIES" "MILLENNIA" "ERAS")) #t))
          ((jt-time? t) (and (member u '("NANOS" "MICROS" "MILLIS" "SECONDS" "MINUTES" "HOURS" "HALF_DAYS")) #t))
          ((or (jt-dt? t) (jt-instant? t)) (not (member u '("FOREVER"))))
          (else #f))))
(define (temporal-supports-field? t field)
  (let ((f (string-upcase field)))
    (cond ((jt-date? t) (and (member f '("YEAR" "MONTH_OF_YEAR" "DAY_OF_MONTH" "DAY_OF_WEEK" "DAY_OF_YEAR" "EPOCH_DAY" "PROLEPTIC_MONTH" "YEAR_OF_ERA" "ERA"
                                         "ALIGNED_DAY_OF_WEEK_IN_MONTH" "ALIGNED_DAY_OF_WEEK_IN_YEAR" "ALIGNED_WEEK_OF_MONTH" "ALIGNED_WEEK_OF_YEAR")) #t))
          ((jt-time? t) (and (member f '("HOUR_OF_DAY" "CLOCK_HOUR_OF_DAY" "HOUR_OF_AMPM" "CLOCK_HOUR_OF_AMPM" "AMPM_OF_DAY"
                                         "MINUTE_OF_HOUR" "MINUTE_OF_DAY" "SECOND_OF_MINUTE" "SECOND_OF_DAY"
                                         "MILLI_OF_SECOND" "MILLI_OF_DAY" "MICRO_OF_SECOND" "MICRO_OF_DAY"
                                         "NANO_OF_SECOND" "NANO_OF_DAY")) #t))
          ((jt-dt? t) (or (temporal-supports-field? (ldt-date t) field) (temporal-supports-field? (ldt-time t) field)))
          ((jt-instant? t) (and (member f '("INSTANT_SECONDS" "NANO_OF_SECOND" "MILLI_OF_SECOND" "MICRO_OF_SECOND")) #t))
          ((and (jhost? t) (string=? (jhost-tag t) "year")) (and (member f '("YEAR" "YEAR_OF_ERA" "ERA")) #t))
          ((and (jhost? t) (string=? (jhost-tag t) "year-month"))
           (and (member f '("YEAR" "MONTH_OF_YEAR" "PROLEPTIC_MONTH" "YEAR_OF_ERA" "ERA")) #t))
          (else #f))))

(define (arg-is-unit? x) (and (jhost? x) (string=? (jhost-tag x) "chrono-unit")))
(define (arg-is-field? x) (and (jhost? x) (string=? (jhost-tag x) "chrono-field")))
(define (arg-is-amount? x) (and (jhost? x) (member (jhost-tag x) '("duration" "period"))))
(define (arg-unit-name x) (cond ((arg-is-unit? x) (cu-name x)) ((string? x) x) ((keyword? x) (keyword-t-name x)) (else #f)))
(define (arg-field-name x) (cond ((arg-is-field? x) (cf-name x)) ((string? x) x) ((keyword? x) (keyword-t-name x)) (else #f)))

;; the generic plus/minus/until/get/getLong/with/range/isSupported, shared by all
;; four core tags. plus/minus accept (n unit) or (amount); with accepts (field val).
(define (mk-temporal-methods)
  (list
   (cons "plus" (case-lambda
                  ((t a) (cond ((arg-is-amount? a) (if (string=? (jhost-tag a) "duration")
                                                       (temporal-plus-nanos t (dur-total-nanos a))
                                                       (temporal-plus-period t a 1)))
                               (else (error #f "plus: bad amount"))))
                  ((t n u) (temporal-plus-unit t (jt->exact n) (arg-unit-name u)))))
   (cons "minus" (case-lambda
                   ((t a) (cond ((arg-is-amount? a) (if (string=? (jhost-tag a) "duration")
                                                        (temporal-plus-nanos t (- (dur-total-nanos a)))
                                                        (temporal-plus-period t a -1)))
                                (else (error #f "minus: bad amount"))))
                   ((t n u) (temporal-plus-unit t (- (jt->exact n)) (arg-unit-name u)))))
   (cons "until" (case-lambda
                   ((t o) (per-between t o))                                ; (.until start end) -> Period
                   ((t o u) (unit-between (arg-unit-name u) t o))))
   (cons "get" (lambda (t f) (temporal-get-field t (arg-field-name f))))
   (cons "getLong" (lambda (t f) (temporal-get-field t (arg-field-name f))))
   (cons "with" (case-lambda
                  ((t adj) (apply-adjuster t adj))                         ; (.with t adjuster)
                  ((t f v) (temporal-with-field t (arg-field-name f) (jt->exact v)))))
   (cons "isSupported" (lambda (t x) (cond ((arg-is-unit? x) (temporal-supports-unit? t (cu-name x)))
                                           ((arg-is-field? x) (temporal-supports-field? t (cf-name x)))
                                           (else #f))))
   (cons "range" (lambda (t f) (temporal-range t (arg-field-name f))))))
(register-host-methods! "local-date" (mk-temporal-methods))
(register-host-methods! "local-time" (mk-temporal-methods))
(register-host-methods! "local-date-time" (mk-temporal-methods))
;; Year/YearMonth answer the field accessors too (a fields-over-all-temporals walk
;; queries them); their own plus/minus/with stay the specific methods above.
(let ((field-methods
       (list (cons "isSupported" (lambda (t x) (cond ((arg-is-unit? x) (temporal-supports-unit? t (cu-name x)))
                                                     ((arg-is-field? x) (temporal-supports-field? t (cf-name x)))
                                                     (else #f))))
             (cons "get" (lambda (t f) (temporal-get-field t (arg-field-name f))))
             (cons "getLong" (lambda (t f) (temporal-get-field t (arg-field-name f)))))))
  (register-host-methods! "year" field-methods)
  (register-host-methods! "year-month" field-methods))
(register-host-methods! "instant" (mk-temporal-methods))

;; --- TemporalAdjuster: a date->date transform applied via (.with t adjuster) --
;; Stored as a jhost over a procedure that takes/returns a LocalDate; for a
;; LocalDateTime the date part is adjusted and the time kept.
(define (jt-adjuster proc) (make-jhost "temporal-adjuster" (vector proc)))
(define (adjuster-proc a) (vector-ref (jhost-state a) 0))
(define (apply-adjuster t adj)
  (let ((proc (cond ((and (jhost? adj) (string=? (jhost-tag adj) "temporal-adjuster")) (adjuster-proc adj))
                    ((procedure? adj) adj)
                    (else (error #f "with: expected a TemporalAdjuster")))))
    (cond ((jt-date? t) (proc t))
          ((jt-dt? t) (ldt-combine (proc (ldt-date t)) (ldt-time t)))
          (else (proc t)))))
;; the next/previous day-of-week adjusters (dow target 1=Mon..7=Sun).
(define (adj-next-dow d target same?)
  (let* ((cur (ld-dow (ld-epoch-day d)))
         (delta (jt-floor-mod (- target cur) 7))
         (step (if (and same? (= delta 0)) 0 (if (= delta 0) 7 delta))))
    (jt-local-date (+ (ld-epoch-day d) step))))
(define (adj-prev-dow d target same?)
  (let* ((cur (ld-dow (ld-epoch-day d)))
         (delta (jt-floor-mod (- cur target) 7))
         (step (if (and same? (= delta 0)) 0 (if (= delta 0) 7 delta))))
    (jt-local-date (- (ld-epoch-day d) step))))
(define (dow-target dow) (cond ((and (jhost? dow) (string=? (jhost-tag dow) "dow-enum")) (dow-val dow))
                               (else (jt->exact dow))))
(define (adj-first-day-of-month d)
  (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d))) (lambda (y m dd) (jt-local-date (ymd->epoch-day y m 1)))))
(define (adj-last-day-of-month d)
  (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d))) (lambda (y m dd) (jt-local-date (ymd->epoch-day y m (jt-len-of-month y m))))))
(define (adj-day-of-week-in-month d ordinal target)
  ;; the ordinal-th (1-based) `target` weekday in d's month (negative counts from end).
  (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d)))
    (lambda (y m dd)
      (if (>= ordinal 0)
          (let* ((first (jt-local-date (ymd->epoch-day y m 1)))
                 (first-match (adj-next-dow first target #t)))
            (jt-local-date (+ (ld-epoch-day first-match) (* 7 (- (max 1 ordinal) 1)))))
          (let* ((last (adj-last-day-of-month d))
                 (last-match (adj-prev-dow last target #t)))
            (jt-local-date (- (ld-epoch-day last-match) (* 7 (- (- ordinal) 1)))))))))
(register-class-statics! "TemporalAdjusters"
  (list (cons "firstDayOfMonth" (lambda () (jt-adjuster adj-first-day-of-month)))
        (cons "lastDayOfMonth" (lambda () (jt-adjuster adj-last-day-of-month)))
        (cons "firstDayOfNextMonth" (lambda () (jt-adjuster (lambda (d) (jt-local-date (+ (ld-epoch-day (adj-last-day-of-month d)) 1))))))
        (cons "firstDayOfYear" (lambda () (jt-adjuster (lambda (d) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d))) (lambda (y m dd) (jt-local-date (ymd->epoch-day y 1 1))))))))
        (cons "lastDayOfYear" (lambda () (jt-adjuster (lambda (d) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d))) (lambda (y m dd) (jt-local-date (ymd->epoch-day y 12 31))))))))
        (cons "firstDayOfNextYear" (lambda () (jt-adjuster (lambda (d) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day d))) (lambda (y m dd) (jt-local-date (ymd->epoch-day (+ y 1) 1 1))))))))
        (cons "dayOfWeekInMonth" (lambda (ordinal dow) (jt-adjuster (lambda (d) (adj-day-of-week-in-month d (jt->exact ordinal) (dow-target dow))))))
        (cons "firstInMonth" (lambda (dow) (jt-adjuster (lambda (d) (adj-day-of-week-in-month d 1 (dow-target dow))))))
        (cons "lastInMonth" (lambda (dow) (jt-adjuster (lambda (d) (adj-day-of-week-in-month d -1 (dow-target dow))))))
        (cons "next" (lambda (dow) (jt-adjuster (lambda (d) (adj-next-dow d (dow-target dow) #f)))))
        (cons "nextOrSame" (lambda (dow) (jt-adjuster (lambda (d) (adj-next-dow d (dow-target dow) #t)))))
        (cons "previous" (lambda (dow) (jt-adjuster (lambda (d) (adj-prev-dow d (dow-target dow) #f)))))
        (cons "previousOrSame" (lambda (dow) (jt-adjuster (lambda (d) (adj-prev-dow d (dow-target dow) #t)))))
        (cons "ofDateAdjuster" (lambda (f) (jt-adjuster (lambda (d) (jolt-invoke f d)))))))
(register-host-methods! "temporal-adjuster"
  (list (cons "adjustInto" (lambda (a t) (apply-adjuster t a)))))

;; range(field): a ValueRange. A small set of common fields; others fall back to a
;; generous range so callers that only read min/max don't crash.
(define (temporal-range t field)
  (let ((f (string-upcase field)))
    (cond
      ((and (jt-date? t) (string=? f "DAY_OF_MONTH"))
       (jt-value-range 1 1 28 (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day t))) (lambda (y m d) (jt-len-of-month y m)))))
      ((string=? f "MONTH_OF_YEAR") (jt-value-range 1 1 12 12))
      ((string=? f "DAY_OF_WEEK") (jt-value-range 1 1 7 7))
      ((string=? f "HOUR_OF_DAY") (jt-value-range 0 0 23 23))
      ((string=? f "MINUTE_OF_HOUR") (jt-value-range 0 0 59 59))
      ((string=? f "SECOND_OF_MINUTE") (jt-value-range 0 0 59 59))
      ((string=? f "NANO_OF_SECOND") (jt-value-range 0 0 999999999 999999999))
      (else (jt-value-range 0 0 999999999999 999999999999)))))

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
(register-str-render! jt-instant? (lambda (x) (iso-instant-str-nanos (inst-nanos x))))
(register-pr-arm! jt-instant? (lambda (x) (iso-instant-str-nanos (inst-nanos x))))

;; Phase-2 value types: amounts, enums, and the chrono-unit/field tokens. Each
;; prints as its java.time toString and is = / hashed on its canonical state.
(define (jt-tagged? tag) (lambda (x) (and (jhost? x) (string=? (jhost-tag x) tag))))
(define (register-jt-value! tag str-fn hash-fn)
  (let ((pred (jt-tagged? tag)))
    (register-str-render! pred str-fn)
    (register-pr-arm! pred str-fn)
    (register-eq-arm! (lambda (a b) (or (pred a) (pred b)))
                      (lambda (a b) (and (pred a) (pred b) (equal? (jhost-state a) (jhost-state b)))))
    (register-hash-arm! pred hash-fn)))
(register-jt-value! "duration" dur->string (lambda (x) (jolt-hash (dur-total-nanos x))))
(register-jt-value! "period" per->string (lambda (x) (jolt-hash (+ (per-years x) (per-months x) (per-days x)))))
(register-jt-value! "month-enum" (lambda (x) (month-name (month-val x))) (lambda (x) (jolt-hash (month-val x))))
(register-jt-value! "dow-enum" (lambda (x) (dow-name (dow-val x))) (lambda (x) (jolt-hash (dow-val x))))
(register-jt-value! "year" (lambda (x) (number->string (year-val x))) (lambda (x) (jolt-hash (year-val x))))
(register-jt-value! "year-month" ym->string (lambda (x) (jolt-hash (+ (* (ym-year x) 13) (ym-month x)))))
(register-jt-value! "chrono-unit" cu-name (lambda (x) (jolt-hash (cu-name x))))
(register-jt-value! "chrono-field" cf-name (lambda (x) (jolt-hash (cf-name x))))

;; compare: same-type java.time values compare on their canonical state.
(define %jt-prev-compare jolt-compare)
(set! jolt-compare
  (lambda (a b)
    (cond
      ((and (jt-date? a) (jt-date? b)) (cond ((< (ld-epoch-day a) (ld-epoch-day b)) -1) ((> (ld-epoch-day a) (ld-epoch-day b)) 1) (else 0)))
      ((and (jt-time? a) (jt-time? b)) (cond ((< (lt-nano-of-day a) (lt-nano-of-day b)) -1) ((> (lt-nano-of-day a) (lt-nano-of-day b)) 1) (else 0)))
      ((and (jt-dt? a) (jt-dt? b)) (ldt-cmp a b))
      ((and (jt-instant? a) (jt-instant? b)) (cond ((< (inst-nanos a) (inst-nanos b)) -1) ((> (inst-nanos a) (inst-nanos b)) 1) (else 0)))
      ((and (jt-tagged-as? a "duration") (jt-tagged-as? b "duration"))
       (let ((x (dur-total-nanos a)) (y (dur-total-nanos b))) (cond ((< x y) -1) ((> x y) 1) (else 0))))
      ((and (jt-tagged-as? a "month-enum") (jt-tagged-as? b "month-enum")) (- (month-val a) (month-val b)))
      ((and (jt-tagged-as? a "dow-enum") (jt-tagged-as? b "dow-enum")) (- (dow-val a) (dow-val b)))
      ((and (jt-tagged-as? a "year") (jt-tagged-as? b "year"))
       (let ((x (year-val a)) (y (year-val b))) (cond ((< x y) -1) ((> x y) 1) (else 0))))
      ((and (jt-tagged-as? a "year-month") (jt-tagged-as? b "year-month")) (ym-cmp a b))
      (else (%jt-prev-compare a b)))))
(define (jt-tagged-as? x tag) (and (jhost? x) (string=? (jhost-tag x) tag)))
(def-var! "clojure.core" "compare" jolt-compare)

;; instance? for the three new tags (inst-time.ss already answers "instant").
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tn (short-class-name (symbol-t-name type-sym))))
      (cond
        ((jt-date? val) (if (string=? tn "LocalDate") #t 'pass))
        ((jt-time? val) (if (string=? tn "LocalTime") #t 'pass))
        ((jt-dt? val) (if (string=? tn "LocalDateTime") #t 'pass))
        ((jt-tagged-as? val "duration") (if (member tn '("Duration" "TemporalAmount")) #t 'pass))
        ((jt-tagged-as? val "period") (if (member tn '("Period" "TemporalAmount")) #t 'pass))
        ((jt-tagged-as? val "month-enum") (if (string=? tn "Month") #t 'pass))
        ((jt-tagged-as? val "dow-enum") (if (string=? tn "DayOfWeek") #t 'pass))
        ((jt-tagged-as? val "year") (if (string=? tn "Year") #t 'pass))
        ((jt-tagged-as? val "year-month") (if (string=? tn "YearMonth") #t 'pass))
        ((jt-tagged-as? val "chrono-unit") (if (member tn '("ChronoUnit" "TemporalUnit")) #t 'pass))
        ((jt-tagged-as? val "chrono-field") (if (member tn '("ChronoField" "TemporalField")) #t 'pass))
        (else 'pass)))))

;; ====================================================================
;; Phase 3: zones, offset/zoned date-times, clocks, formatter integration.
;; ====================================================================
;;
;; Chez exposes local + UTC + fixed offsets, not an IANA tz database. Fixed-offset
;; and UTC zones are exact. A named zone (America/New_York) maps to a representative
;; fixed offset from a small table; arbitrary-instant DST transitions are NOT modeled.
;;
;;   zone-offset       (vector total-seconds)
;;   zone-id           (vector id offset-seconds)        offset-seconds: resolved fixed offset
;;   offset-time       (vector nano-of-day offset-seconds)
;;   offset-date-time  (vector epoch-day nano-of-day offset-seconds)
;;   zoned-date-time   (vector epoch-day nano-of-day offset-seconds zone-id)
;;   clock             (vector kind fixed-ms zone-id [base-clock])   kind: 'system | 'fixed | 'offset | 'tick

;; --- ZoneOffset --------------------------------------------------------------
(define (jt-zone-offset secs) (make-jhost "zone-offset" (vector secs)))
(define (zo-secs z) (vector-ref (jhost-state z) 0))
(define (jt-zone-offset? x) (and (jhost? x) (string=? (jhost-tag x) "zone-offset")))
;; ZoneOffset id: "Z" for 0, else ±HH:mm[:ss] with the seconds field elided at :00.
(define (zo-id secs)
  (if (= secs 0) "Z"
      (let* ((neg (< secs 0)) (a (abs secs))
             (h (quotient a 3600)) (m (quotient (modulo a 3600) 60)) (s (modulo a 60)))
        (string-append (if neg "-" "+") (pad2 h) ":" (pad2 m)
                       (if (= s 0) "" (string-append ":" (pad2 s)))))))
;; parse a ZoneOffset id: "Z"/"+00:00" -> 0, "+HH:mm[:ss]" / "+HHmm" / "+HH".
(define (parse-zone-offset s)
  (let ((str (jt-str s)))
    (cond
      ((or (string=? str "Z") (string=? str "z") (string=? str "UTC") (string=? str "GMT") (string=? str "+00:00")) 0)
      (else
       (let* ((sign (cond ((char=? (string-ref str 0) #\-) -1) (else 1)))
              (body (if (memv (string-ref str 0) '(#\+ #\-)) (substring str 1 (string-length str)) str))
              (parts (let loop ((i 0) (cur '()) (acc '()))
                       (cond ((>= i (string-length body))
                              (reverse (cons (list->string (reverse cur)) acc)))
                             ((char=? (string-ref body i) #\:) (loop (+ i 1) '() (cons (list->string (reverse cur)) acc)))
                             (else (loop (+ i 1) (cons (string-ref body i) cur) acc))))))
         (if (and (= (length parts) 1) (> (string-length (car parts)) 2))
             ;; "+HHmm" / "+HHmmss" compact form
             (let ((b (car parts)))
               (* sign (+ (* (or (string->number (substring b 0 2)) 0) 3600)
                          (* (if (>= (string-length b) 4) (or (string->number (substring b 2 4)) 0) 0) 60)
                          (if (>= (string-length b) 6) (or (string->number (substring b 4 6)) 0) 0))))
             (* sign (+ (* (or (string->number (list-ref parts 0)) 0) 3600)
                        (* (if (> (length parts) 1) (or (string->number (list-ref parts 1)) 0) 0) 60)
                        (if (> (length parts) 2) (or (string->number (list-ref parts 2)) 0) 0)))))))))

(register-class-statics! "ZoneOffset"
  (list (cons "of" (lambda (s) (jt-zone-offset (parse-zone-offset s))))
        (cons "ofTotalSeconds" (lambda (n) (jt-zone-offset (jt->exact n))))
        (cons "ofHours" (lambda (h) (jt-zone-offset (* (jt->exact h) 3600))))
        ;; java.time requires h/m/s to share a sign; the total is the plain sum.
        (cons "ofHoursMinutes" (lambda (h m) (jt-zone-offset (+ (* (jt->exact h) 3600) (* (jt->exact m) 60)))))
        (cons "ofHoursMinutesSeconds" (lambda (h m s) (jt-zone-offset (+ (* (jt->exact h) 3600) (* (jt->exact m) 60) (jt->exact s)))))
        (cons "from" (lambda (t) (cond ((jt-zone-offset? t) t)
                                       ((jt-offset-dt? t) (jt-zone-offset (odt-offset t)))
                                       ((jt-zoned-dt? t) (jt-zone-offset (zdt-offset t)))
                                       (else (error #f "ZoneOffset/from: unsupported")))))
        (cons "UTC" (jt-zone-offset 0))
        (cons "MIN" (jt-zone-offset (* -18 3600)))
        (cons "MAX" (jt-zone-offset (* 18 3600)))))
(register-host-methods! "zone-offset"
  (list (cons "getId" (lambda (z) (zo-id (zo-secs z))))
        (cons "getTotalSeconds" (lambda (z) (zo-secs z)))
        (cons "getRules" (lambda (z) (make-jhost "zone-rules" (vector (zo-secs z)))))
        (cons "normalized" (lambda (z) z))
        (cons "compareTo" (lambda (z o) (let ((a (zo-secs z)) (b (zo-secs o))) (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals" (lambda (z o) (and (jt-zone-offset? o) (= (zo-secs z) (zo-secs o)))))
        (cons "hashCode" (lambda (z) (zo-secs z)))
        (cons "toString" (lambda (z) (zo-id (zo-secs z))))))

;; --- ZoneId ------------------------------------------------------------------
;; Fixed-offset zone ids are parsed directly. Named zones (containing "/" or
;; matching known short ids) route through libc localtime → tm_gmtoff, which
;; reads /usr/share/zoneinfo and correctly handles DST for all regions.
(define (jt-zone-id id off) (make-jhost "zone-id" (vector id off)))
(define (zid-id z) (vector-ref (jhost-state z) 0))
(define (zid-off z) (vector-ref (jhost-state z) 1))
(define (jt-zone-id? x) (and (jhost? x) (string=? (jhost-tag x) "zone-id")))
;; system zone offset. The inst/SimpleDateFormat layer is UTC-centric (tz-free,
;; machine-independent by design), so systemDefault resolves to UTC: a LocalDateTime
;; round-tripped through atZone/toInstant stays aligned with the UTC #inst model.
(define (system-zone-offset-secs) 0)
;; java.time ZoneId.SHORT_IDS: 3-letter ids -> region/offset ids.
(define short-ids-pairs
  '(("ACT" . "Australia/Darwin") ("AET" . "Australia/Sydney") ("AGT" . "America/Argentina/Buenos_Aires")
    ("ART" . "Africa/Cairo") ("AST" . "America/Anchorage") ("BET" . "America/Sao_Paulo")
    ("BST" . "Asia/Dhaka") ("CAT" . "Africa/Harare") ("CNT" . "America/St_Johns")
    ("CST" . "America/Chicago") ("CTT" . "Asia/Shanghai") ("EAT" . "Africa/Addis_Ababa")
    ("ECT" . "Europe/Paris") ("IET" . "America/Indiana/Indianapolis") ("IST" . "Asia/Kolkata")
    ("JST" . "Asia/Tokyo") ("MIT" . "Pacific/Apia") ("NET" . "Asia/Yerevan")
    ("NST" . "Pacific/Auckland") ("PLT" . "Asia/Karachi") ("PNT" . "America/Phoenix")
    ("PRT" . "America/Puerto_Rico") ("PST" . "America/Los_Angeles") ("SST" . "Pacific/Guadalcanal")
    ("VST" . "Asia/Ho_Chi_Minh") ("EST" . "-05:00") ("MST" . "-07:00") ("HST" . "-10:00")))
;; resolve a short id or return the original.
(define (resolve-short-id id)
  (let ((e (assoc id short-ids-pairs))) (if e (cdr e) id)))
;; is the zone id a named (IANA) zone (contains "/")?
(define (zone-has-slash? id)
  (let ((len (string-length id)))
    (let loop ((i 0))
      (and (< i len)
           (or (char=? (string-ref id i) #\/)
               (loop (+ i 1)))))))
(define (fixed-offset-zone? id)
  (and (> (string-length id) 0) (memv (string-ref id 0) '(#\+ #\-))))

;; offset for a zone at a UTC instant (epoch-seconds). Named zones route through
;; libc localtime; fixed offsets are parsed directly.
(define (zone-offset-at-instant id std secs)
  (cond ((fixed-offset-zone? id) (parse-zone-offset id))
        ((zone-has-slash? id)
         (let ((o (zone-offset-seconds id secs)))
           (if o o std)))
        (else std)))
;; offset for a zone at a local wall-time (epoch-seconds).
(define (zone-offset-at-local id std lsecs)
  (let ((o (zone-offset-at-instant id std lsecs)))
    (zone-offset-at-instant id std (- lsecs o))))

;; resolve any zone designator (string / ZoneId / ZoneOffset) to a (id . offset).
(define (resolve-zone z)
  (cond
    ((jt-zone-offset? z) (cons (zo-id (zo-secs z)) (zo-secs z)))
    ((jt-zone-id? z) (cons (zid-id z) (zid-off z)))
    (else
     (let ((id (jt-str z)))
       (cond
         ((string=? id "system") (cons (zo-id (system-zone-offset-secs)) (system-zone-offset-secs)))
         ((or (string=? id "Z") (string=? id "UTC") (string=? id "GMT") (string=? id "Etc/UTC") (string=? id "Etc/GMT"))
          (cons "Z" 0))
         ((fixed-offset-zone? id) (let ((s (parse-zone-offset id))) (cons (zo-id s) s)))
         (else (let ((rid (resolve-short-id id)))
                 (if (zone-has-slash? rid)
                     (let ((off (zone-offset-seconds rid 0)))
                       (cons rid (or off 0)))
                     (cons id 0)))))))))
(define (zone-id-of z)
  (let ((r (resolve-zone z))) (jt-zone-id (car r) (cdr r))))

;; --- DST rule table (fallback when libc is unavailable) ----------------------
;; Standard-offset table for known zones (base offset, no DST).
(define zone-offset-table
  '(("UTC" . 0) ("GMT" . 0) ("Z" . 0) ("Etc/UTC" . 0) ("Etc/GMT" . 0)
    ("America/New_York" . -18000) ("America/Chicago" . -21600) ("America/Denver" . -25200)
    ("America/Los_Angeles" . -28800) ("America/Toronto" . -18000) ("America/Sao_Paulo" . -10800)
    ("America/Mexico_City" . -21600) ("America/Argentina/Buenos_Aires" . -10800)
    ("Europe/London" . 0) ("Europe/Paris" . 3600) ("Europe/Berlin" . 3600)
    ("Europe/Madrid" . 3600) ("Europe/Rome" . 3600) ("Europe/Amsterdam" . 3600)
    ("Europe/Stockholm" . 3600) ("Europe/Zurich" . 3600) ("Europe/Moscow" . 10800)
    ("Asia/Tokyo" . 32400) ("Asia/Shanghai" . 28800) ("Asia/Kolkata" . 19800)
    ("Asia/Singapore" . 28800) ("Asia/Dubai" . 14400) ("Asia/Hong_Kong" . 28800)
    ("Australia/Sydney" . 36000) ("Australia/Melbourne" . 36000) ("Australia/Brisbane" . 36000)
    ("Australia/Perth" . 28800) ("Australia/Adelaide" . 34200)
    ("Pacific/Auckland" . 43200) ("Pacific/Fiji" . 43200)
    ("Africa/Johannesburg" . 7200) ("Africa/Cairo" . 7200) ("Africa/Lagos" . 3600)))

;; DST rule families: US (2nd Sun Mar→1st Sun Nov), EU (last Sun Mar→last Sun Oct),
;; AU (1st Sun Oct→1st Sun Apr), NZ (last Sun Sep→1st Sun Apr).
(define dst-zone-table
  '(("America/New_York" -18000 . us) ("America/Toronto" -18000 . us)
    ("America/Chicago" -21600 . us) ("America/Denver" -25200 . us)
    ("America/Los_Angeles" -28800 . us)
    ("Europe/London" 0 . eu) ("Europe/Paris" 3600 . eu)
    ("Europe/Berlin" 3600 . eu) ("Europe/Madrid" 3600 . eu)
    ("Europe/Rome" 3600 . eu) ("Europe/Amsterdam" 3600 . eu)
    ("Europe/Stockholm" 3600 . eu) ("Europe/Zurich" 3600 . eu)
    ("Australia/Sydney" 36000 . au) ("Australia/Melbourne" 36000 . au)
    ("Australia/Adelaide" 34200 . au) ("Pacific/Auckland" 43200 . nz)))
(define (dst-entry id) (assoc id dst-zone-table))
(define dst-saving 3600)
(define (secs->year secs)
  (call-with-values (lambda () (civil-from-days (jt-floor-div secs 86400)))
    (lambda (y m d) y)))

;; epoch-day of the nth (1-based) weekday `dow` in (year, month); n<0 counts from end.
(define (nth-dow-epoch-day year month dow n)
  (if (> n 0)
      (let* ((first-ed (ymd->epoch-day year month 1))
             (shift (modulo (- dow (epoch-day-dow first-ed)) 7)))
        (+ first-ed shift (* (- n 1) 7)))
      (let* ((last-ed (- (ymd->epoch-day year (+ month 1) 1) 1))
             (shift (modulo (- (epoch-day-dow last-ed) dow) 7)))
        (- last-ed shift (* (- (- n) 1) 7)))))
(define (epoch-day-dow ed) (modulo (+ ed 4) 7))

(define (dst-offset-at-instant id std rule secs)
  (let ((year (secs->year secs)))
    (case rule
      ((us)
       (let ((spring (- (+ (* (nth-dow-epoch-day year 3 0 2) 86400) (* 2 3600)) std))
             (fall   (- (+ (* (nth-dow-epoch-day year 11 0 1) 86400) (* 2 3600)) (+ std dst-saving))))
         (if (and (<= spring secs) (< secs fall)) (+ std dst-saving) std)))
      ((eu)
       (let ((spring (+ (* (nth-dow-epoch-day year 3 0 -1) 86400) 3600))
             (fall   (+ (* (nth-dow-epoch-day year 10 0 -1) 86400) 3600)))
         (if (and (<= spring secs) (< secs fall)) (+ std dst-saving) std)))
      ;; AU: DST from 1st Sun Oct 02:00 local → 1st Sun Apr 03:00 local.
      ((au)
       (let ((spring (- (+ (* (nth-dow-epoch-day year 10 0 1) 86400) (* 2 3600)) std))
             (fall   (- (+ (* (nth-dow-epoch-day year 4 0 1) 86400) (* 3 3600)) (+ std dst-saving))))
         (if (and (<= spring secs) (< secs fall)) (+ std dst-saving) std)))
      ;; NZ: DST from last Sun Sep 02:00 local → 1st Sun Apr 03:00 local.
      ((nz)
       (let ((spring (- (+ (* (nth-dow-epoch-day year 9 0 -1) 86400) (* 2 3600)) std))
             (fall   (- (+ (* (nth-dow-epoch-day year 4 0 1) 86400) (* 3 3600)) (+ std dst-saving))))
         (if (and (<= spring secs) (< secs fall)) (+ std dst-saving) std)))
      (else std))))
;; offset for a zone at a UTC instant (epoch-seconds). Named zones route through
;; libc localtime; fixed offsets are parsed directly.
(define (zone-offset-at-instant id std secs)
  (cond ((fixed-offset-zone? id) (parse-zone-offset id))
        ((zone-has-slash? id)
         (let ((o (zone-offset-seconds id secs)))
           (if o o std)))
        (else std)))
;; offset for a zone at a local wall-time (epoch-seconds).
(define (zone-offset-at-local id std lsecs)
  ;; approximate: treat the local time as UTC, get the offset, then re-derive.
  ;; libc localtime can handle this more precisely but the simple approach
  ;; covers all current use cases.
  (let ((o (zone-offset-at-instant id std lsecs)))
    (zone-offset-at-instant id std (- lsecs o))))

;; Expose which tz resolution backend is active: :libc when the capability probe
;; passed, :fallback when using the built-in rule table. Guard-wrapped so a
;; tree-shake or static build (which may not have the libc symbol) doesn't crash.
(def-var! "jolt.host" "tz-backend"
  (guard (e (#t ':fallback))
    (if libc-tz-available?* ':libc ':fallback)))

(register-class-statics! "ZoneId"
  (list (cons "of" (lambda (id . _) (zone-id-of id)))
        (cons "systemDefault" (lambda () (let ((s (system-zone-offset-secs))) (jt-zone-id (zo-id s) s))))
        (cons "getAvailableZoneIds" (lambda () (fold-left pset-conj empty-pset (map car short-ids-pairs))))
        (cons "SHORT_IDS" (apply jolt-hash-map (apply append (map (lambda (p) (list (car p) (cdr p))) short-ids-pairs))))
        (cons "from" (lambda (t) (cond ((jt-zone-id? t) t) ((jt-zone-offset? t) (jt-zone-id (zo-id (zo-secs t)) (zo-secs t)))
                                       ((jt-zoned-dt? t) (zdt-zone t)) (else (error #f "ZoneId/from: unsupported")))))))
(register-host-methods! "zone-id"
  (list (cons "getId" (lambda (z) (zid-id z)))
        (cons "getRules" (lambda (z) (make-jhost "zone-rules" (vector (zid-id z) (zid-off z)))))
        (cons "normalized" (lambda (z) (if (and (> (string-length (zid-id z)) 0) (memv (string-ref (zid-id z) 0) '(#\+ #\- #\Z)))
                                           (jt-zone-offset (zid-off z)) z)))
        (cons "getDisplayName" (lambda (z . _) (zid-id z)))
        (cons "equals" (lambda (z o) (and (jt-zone-id? o) (string=? (zid-id z) (zid-id o)))))
        (cons "hashCode" (lambda (z) (string-hash (zid-id z))))
        (cons "toString" (lambda (z) (zid-id z)))))
;; ZoneRules carries the zone id + standard offset. getOffset is DST-aware: given
;; an Instant it resolves the offset at that instant, given a LocalDateTime at that
;; local wall time; with no argument it yields the standard offset.
(define (zr-id r) (vector-ref (jhost-state r) 0))
(define (zr-std r) (vector-ref (jhost-state r) 1))
(register-host-methods! "zone-rules"
  (list (cons "getOffset"
              (lambda (r . args)
                (if (null? args)
                    (jt-zone-offset (zr-std r))
                    (let ((a (car args)))
                      (cond
                        ((jt-instant-tag? a)
                         (jt-zone-offset (zone-offset-at-instant (zr-id r) (zr-std r) (jt-floor-div (inst-nanos a) nanos-per-sec))))
                        ((jinst? a)
                         (jt-zone-offset (zone-offset-at-instant (zr-id r) (zr-std r) (jt-floor-div (exact (truncate (jinst-ms a))) 1000))))
                        ((and (jhost? a) (string=? (jhost-tag a) "local-date-time"))
                         (jt-zone-offset (zone-offset-at-local (zr-id r) (zr-std r)
                                          (+ (* (ldt-epoch-day a) 86400) (jt-floor-div (ldt-nano-of-day a) nanos-per-sec)))))
                        (else (jt-zone-offset (zr-std r))))))))
        (cons "isFixedOffset" (lambda (r) (not (zone-has-slash? (zr-id r)))))
        (cons "getStandardOffset" (lambda (r . _) (jt-zone-offset (zr-std r))))
        (cons "toString" (lambda (r) (if (zone-has-slash? (zr-id r)) "ZoneRules" "ZoneRules[fixed]")))))

;; --- OffsetTime --------------------------------------------------------------
(define (jt-offset-time nod off) (make-jhost "offset-time" (vector nod off)))
(define (ot-nod x) (vector-ref (jhost-state x) 0))
(define (ot-offset x) (vector-ref (jhost-state x) 1))
(define (jt-offset-time? x) (and (jhost? x) (string=? (jhost-tag x) "offset-time")))
(define (ot->string x) (string-append (iso-time-str (ot-nod x)) (offset-suffix (ot-offset x))))
;; the offset suffix used in ISO offset/zoned rendering: "Z" for 0 else ±HH:mm[:ss].
(define (offset-suffix secs) (zo-id secs))

;; --- OffsetDateTime ----------------------------------------------------------
(define (jt-offset-dt ed nod off) (make-jhost "offset-date-time" (vector ed nod off)))
(define (odt-epoch-day x) (vector-ref (jhost-state x) 0))
(define (odt-nano-of-day x) (vector-ref (jhost-state x) 1))
(define (odt-offset x) (vector-ref (jhost-state x) 2))
(define (jt-offset-dt? x) (and (jhost? x) (string=? (jhost-tag x) "offset-date-time")))
(define (odt-ldt x) (jt-local-dt (odt-epoch-day x) (odt-nano-of-day x)))
;; epoch-ms of the instant this offset-dt denotes (subtract the offset to reach UTC).
(define (odt->ms x) (- (ldt->ms (odt-ldt x)) (* (odt-offset x) 1000)))
(define (odt->nanos x) (- (+ (* (odt-epoch-day x) nanos-per-day) (odt-nano-of-day x)) (* (odt-offset x) nanos-per-sec)))
(define (odt->string x) (string-append (iso-datetime-str (odt-epoch-day x) (odt-nano-of-day x)) (offset-suffix (odt-offset x))))

;; --- ZonedDateTime -----------------------------------------------------------
(define (jt-zoned-dt ed nod off zone) (make-jhost "zoned-date-time" (vector ed nod off zone)))
(define (zdt-epoch-day x) (vector-ref (jhost-state x) 0))
(define (zdt-nano-of-day x) (vector-ref (jhost-state x) 1))
(define (zdt-offset x) (vector-ref (jhost-state x) 2))
(define (zdt-zone x) (vector-ref (jhost-state x) 3))
(define (jt-zoned-dt? x) (and (jhost? x) (string=? (jhost-tag x) "zoned-date-time")))
(define (zdt-ldt x) (jt-local-dt (zdt-epoch-day x) (zdt-nano-of-day x)))
(define (zdt->ms x) (- (ldt->ms (zdt-ldt x)) (* (zdt-offset x) 1000)))
(define (zdt->nanos x) (- (+ (* (zdt-epoch-day x) nanos-per-day) (zdt-nano-of-day x)) (* (zdt-offset x) nanos-per-sec)))
;; ISO zoned: local + offset, then [zone-id] unless the zone IS the offset.
(define (zdt->string x)
  (let* ((zid (zdt-zone x)) (id (zid-id zid)))
    (string-append (iso-datetime-str (zdt-epoch-day x) (zdt-nano-of-day x)) (offset-suffix (zdt-offset x))
                   (if (or (string=? id (offset-suffix (zdt-offset x)))
                           (and (> (string-length id) 0) (memv (string-ref id 0) '(#\+ #\- #\Z))))
                       "" (string-append "[" id "]")))))

;; build a ZonedDateTime/OffsetDateTime from a LocalDateTime + zone designator. The
;; offset is the zone's fixed offset (no DST resolution).
(define (zoned-of-ldt ldt zone)
  (let* ((r (resolve-zone zone))
         (lsecs (+ (* (ldt-epoch-day ldt) 86400) (jt-floor-div (ldt-nano-of-day ldt) 1000000000)))
         (off (zone-offset-at-local (car r) (cdr r) lsecs)))
    (jt-zoned-dt (ldt-epoch-day ldt) (ldt-nano-of-day ldt) off (jt-zone-id (car r) (cdr r)))))
(define (offset-of-ldt ldt off)
  (let ((secs (cond ((jt-zone-offset? off) (zo-secs off)) ((jt-zone-id? off) (zid-off off)) (else (parse-zone-offset off)))))
    (jt-offset-dt (ldt-epoch-day ldt) (ldt-nano-of-day ldt) secs)))
;; from an epoch-ms instant + zone: apply the zone offset to get the local fields.
(define (zoned-of-instant-ms ms zone)
  (let* ((r (resolve-zone zone))
         (off (zone-offset-at-instant (car r) (cdr r) (jt-floor-div (exact (truncate ms)) 1000)))
         (local-ms (+ (exact (truncate ms)) (* off 1000)))
         (ed (jt-floor-div local-ms 86400000)) (nod (* (jt-floor-mod local-ms 86400000) 1000000)))
    (jt-zoned-dt ed nod off (jt-zone-id (car r) (cdr r)))))
(define (offset-of-instant-ms ms off-or-zone)
  (let* ((secs (cond ((jt-zone-offset? off-or-zone) (zo-secs off-or-zone))
                     ((jt-zone-id? off-or-zone) (zid-off off-or-zone))
                     (else (let ((r (resolve-zone off-or-zone)))
                             (zone-offset-at-instant (car r) (cdr r) (jt-floor-div (exact (truncate ms)) 1000))))))
         (local-ms (+ (exact (truncate ms)) (* secs 1000)))
         (ed (jt-floor-div local-ms 86400000)) (nod (* (jt-floor-mod local-ms 86400000) 1000000)))
    (jt-offset-dt ed nod secs)))
;; nano-precise instant -> zoned/offset (Instant carries epoch-nanos; ms versions
;; above stay for the ms-based Date/Calendar callers).
(define (zoned-of-instant-nanos nanos zone)
  (let* ((r (resolve-zone zone))
         (off (zone-offset-at-instant (car r) (cdr r) (jt-floor-div nanos nanos-per-sec)))
         (local-nanos (+ nanos (* off nanos-per-sec)))
         (ed (jt-floor-div local-nanos nanos-per-day)) (nod (jt-floor-mod local-nanos nanos-per-day)))
    (jt-zoned-dt ed nod off (jt-zone-id (car r) (cdr r)))))
(define (offset-of-instant-nanos nanos off-or-zone)
  (let* ((off (cond ((jt-zone-offset? off-or-zone) (zo-secs off-or-zone))
                    ((jt-zone-id? off-or-zone) (zid-off off-or-zone))
                    (else (let ((r (resolve-zone off-or-zone)))
                            (zone-offset-at-instant (car r) (cdr r) (jt-floor-div nanos nanos-per-sec))))))
         (local-nanos (+ nanos (* off nanos-per-sec)))
         (ed (jt-floor-div local-nanos nanos-per-day)) (nod (jt-floor-mod local-nanos nanos-per-day)))
    (jt-offset-dt ed nod off)))

;; redefine mk-zoned (used by Phase-1/2 atZone/atOffset) to yield a real
;; ZonedDateTime at UTC. Older inst-time.ss "zoned-dt" callers route through here.
(set! mk-zoned (lambda (ms) (zoned-of-instant-ms ms (jt-zone-id "Z" 0))))

;; --- now / Clock-aware statics -----------------------------------------------
;; A Clock yields an instant (epoch-ms) and a zone. now-from-clock reads ms.
(define (clock-now-ms clk)
  (cond ((not (jhost? clk)) (now-ms))
        ((string=? (jhost-tag clk) "clock") (clk-millis clk))
        (else (now-ms))))
(define (clock-zone clk)
  (if (and (jhost? clk) (string=? (jhost-tag clk) "clock")) (vector-ref (jhost-state clk) 2)
      (jt-zone-id "Z" 0)))
;; now-ms-arg: an optional first arg may be a Clock or a ZoneId; pick the ms.
(define (now-ms* args)
  (if (and (pair? args) (jhost? (car args)) (string=? (jhost-tag (car args)) "clock"))
      (clock-now-ms (car args))
      (now-ms)))

;; rewire the Phase-1/2 now statics to accept an optional Clock/ZoneId argument.
(register-class-statics! "Instant"
  (list (cons "now" (lambda args (mk-instant (now-ms* args))))))
(register-class-statics! "LocalDate"
  (list (cons "now" (lambda args (mk-local-date (now-ms* args))))))
(register-class-statics! "LocalTime"
  (list (cons "now" (lambda args (jt-local-time (* (jt-floor-mod (exact (truncate (now-ms* args))) 86400000) 1000000))))))
(register-class-statics! "LocalDateTime"
  (list (cons "now" (lambda args (mk-local (now-ms* args))))))

;; --- Clock -------------------------------------------------------------------
;; (vector kind fixed-ms zone [base])  kind 'system | 'fixed | 'offset | 'tick
(define (jt-clock kind ms zone . base) (make-jhost "clock" (vector kind ms zone (and (pair? base) (car base)))))
(define (clk-millis clk)
  (let ((kind (vector-ref (jhost-state clk) 0)))
    (case kind
      ((fixed) (vector-ref (jhost-state clk) 1))
      ((offset) (+ (clk-millis (vector-ref (jhost-state clk) 3)) (vector-ref (jhost-state clk) 1)))
      ((tick) (let* ((base (vector-ref (jhost-state clk) 3)) (dur-ms (vector-ref (jhost-state clk) 1))
                     (m (clk-millis base)))
                (if (> dur-ms 0) (* (jt-floor-div m dur-ms) dur-ms) m)))
      (else (now-ms)))))
(register-class-statics! "Clock"
  (list (cons "systemUTC" (lambda () (jt-clock 'system 0 (jt-zone-id "Z" 0))))
        (cons "systemDefaultZone" (lambda () (let ((s (system-zone-offset-secs))) (jt-clock 'system 0 (jt-zone-id (zo-id s) s)))))
        (cons "system" (lambda (zone) (jt-clock 'system 0 (zone-id-of zone))))
        (cons "fixed" (lambda (inst zone) (jt-clock 'fixed (exact (truncate (ms-of inst))) (zone-id-of zone))))
        (cons "offset" (lambda (clk dur) (jt-clock 'offset (quotient (dur-total-nanos dur) 1000000) (clock-zone clk) clk)))
        (cons "tick" (lambda (clk dur) (jt-clock 'tick (quotient (dur-total-nanos dur) 1000000) (clock-zone clk) clk)))
        (cons "tickMinutes" (lambda (zone) (jt-clock 'tick 60000 (zone-id-of zone) (jt-clock 'system 0 (zone-id-of zone)))))
        (cons "tickSeconds" (lambda (zone) (jt-clock 'tick 1000 (zone-id-of zone) (jt-clock 'system 0 (zone-id-of zone)))))))
(register-host-methods! "clock"
  (list (cons "instant" (lambda (clk) (mk-instant (clk-millis clk))))
        (cons "millis" (lambda (clk) (clk-millis clk)))
        (cons "getZone" (lambda (clk) (vector-ref (jhost-state clk) 2)))
        (cons "withZone" (lambda (clk zone) (make-jhost "clock" (vector (vector-ref (jhost-state clk) 0)
                                                                        (vector-ref (jhost-state clk) 1)
                                                                        (zone-id-of zone)
                                                                        (vector-ref (jhost-state clk) 3)))))
        (cons "equals" (lambda (clk o) (and (jhost? o) (string=? (jhost-tag o) "clock") (equal? (jhost-state clk) (jhost-state o)))))
        (cons "toString" (lambda (clk) "Clock"))))

;; --- ZonedDateTime statics + methods -----------------------------------------
(register-class-statics! "ZonedDateTime"
  (list (cons "of" (case-lambda
                     ((ldt zone) (zoned-of-ldt ldt zone))
                     ((d t zone) (zoned-of-ldt (jt-local-dt (ld-epoch-day d) (lt-nano-of-day t)) zone))
                     ((y mo d h mi s nano zone)
                      (zoned-of-ldt (jt-local-dt (ymd->epoch-day (jt->exact y) (jt->exact mo) (jt->exact d))
                                                 (hmsn->nano (jt->exact h) (jt->exact mi) (jt->exact s) (jt->exact nano))) zone))))
        (cons "ofInstant" (case-lambda
                            ((inst zone) (zoned-of-instant-ms (ms-of inst) zone))
                            ((ldt off zone) (zoned-of-ldt ldt zone))))
        (cons "ofLocal" (lambda (ldt zone . _) (zoned-of-ldt ldt zone)))
        (cons "now" (lambda args (let ((ms (now-ms* args)))
                                   (zoned-of-instant-ms ms (if (and (pair? args) (jhost? (car args)) (string=? (jhost-tag (car args)) "clock"))
                                                               (clock-zone (car args)) (jt-zone-id "Z" 0))))))
        (cons "parse" (lambda (s . fmt)
                        (if (null? fmt) (parse-zoned-date-time (jt-str s))
                            (call-with-values (lambda () (formatter-parse-fields (car fmt) s))
                              (lambda (ed nod) (jt-zoned-dt ed nod 0 (jt-zone-id "Z" 0)))))))
        (cons "from" (lambda (t) (cond ((jt-zoned-dt? t) t)
                                       ((jt-offset-dt? t) (jt-zoned-dt (odt-epoch-day t) (odt-nano-of-day t) (odt-offset t) (jt-zone-id (zo-id (odt-offset t)) (odt-offset t))))
                                       (else (zoned-of-instant-ms (ms-of t) (jt-zone-id "Z" 0))))))))

;; apply nano arithmetic to a zoned/offset value, keeping its offset & zone.
(define (zdt-plus-nanos x nanos)
  (let ((nldt (ldt-plus-nanos (zdt-ldt x) nanos)))
    (jt-zoned-dt (ldt-epoch-day nldt) (ldt-nano-of-day nldt) (zdt-offset x) (zdt-zone x))))
(define (zdt-with-ldt x ldt) (jt-zoned-dt (ldt-epoch-day ldt) (ldt-nano-of-day ldt) (zdt-offset x) (zdt-zone x)))

(register-host-methods! "zoned-date-time"
  (list (cons "getYear" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (zdt-epoch-day x))) (lambda (y m d) y))))
        (cons "getMonthValue" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (zdt-epoch-day x))) (lambda (y m d) m))))
        (cons "getMonth" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (zdt-epoch-day x))) (lambda (y m d) (jt-month m)))))
        (cons "getDayOfMonth" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (zdt-epoch-day x))) (lambda (y m d) d))))
        (cons "getDayOfWeek" (lambda (x) (jt-dow (ld-dow (zdt-epoch-day x)))))
        (cons "getDayOfYear" (lambda (x) (ld-day-of-year (zdt-epoch-day x))))
        (cons "getHour" (lambda (x) (lt-hour (zdt-ldt x))))
        (cons "getMinute" (lambda (x) (lt-minute (zdt-ldt x))))
        (cons "getSecond" (lambda (x) (lt-second (zdt-ldt x))))
        (cons "getNano" (lambda (x) (lt-nano (zdt-ldt x))))
        (cons "getOffset" (lambda (x) (jt-zone-offset (zdt-offset x))))
        (cons "getZone" (lambda (x) (zdt-zone x)))
        (cons "toInstant" (lambda (x) (mk-instant-nanos (zdt->nanos x))))
        (cons "toLocalDate" (lambda (x) (jt-local-date (zdt-epoch-day x))))
        (cons "toLocalTime" (lambda (x) (jt-local-time (zdt-nano-of-day x))))
        (cons "toLocalDateTime" (lambda (x) (zdt-ldt x)))
        (cons "toOffsetDateTime" (lambda (x) (jt-offset-dt (zdt-epoch-day x) (zdt-nano-of-day x) (zdt-offset x))))
        (cons "toEpochSecond" (lambda (x) (jt-floor-div (zdt->ms x) 1000)))
        (cons "plusDays" (lambda (x n) (zdt-with-ldt x (jt-local-dt (+ (zdt-epoch-day x) (jt->exact n)) (zdt-nano-of-day x)))))
        (cons "minusDays" (lambda (x n) (zdt-with-ldt x (jt-local-dt (- (zdt-epoch-day x) (jt->exact n)) (zdt-nano-of-day x)))))
        (cons "plusWeeks" (lambda (x n) (zdt-with-ldt x (jt-local-dt (+ (zdt-epoch-day x) (* 7 (jt->exact n))) (zdt-nano-of-day x)))))
        (cons "minusWeeks" (lambda (x n) (zdt-with-ldt x (jt-local-dt (- (zdt-epoch-day x) (* 7 (jt->exact n))) (zdt-nano-of-day x)))))
        (cons "plusMonths" (lambda (x n) (zdt-with-ldt x (ldt-combine (ld-plus-months (ldt-date (zdt-ldt x)) (jt->exact n)) (ldt-time (zdt-ldt x))))))
        (cons "minusMonths" (lambda (x n) (zdt-with-ldt x (ldt-combine (ld-plus-months (ldt-date (zdt-ldt x)) (- (jt->exact n))) (ldt-time (zdt-ldt x))))))
        (cons "plusYears" (lambda (x n) (zdt-with-ldt x (ldt-combine (ld-plus-years (ldt-date (zdt-ldt x)) (jt->exact n)) (ldt-time (zdt-ldt x))))))
        (cons "minusYears" (lambda (x n) (zdt-with-ldt x (ldt-combine (ld-plus-years (ldt-date (zdt-ldt x)) (- (jt->exact n))) (ldt-time (zdt-ldt x))))))
        (cons "plusHours" (lambda (x n) (zdt-plus-nanos x (* (jt->exact n) 3600 nanos-per-sec))))
        (cons "minusHours" (lambda (x n) (zdt-plus-nanos x (- (* (jt->exact n) 3600 nanos-per-sec)))))
        (cons "plusMinutes" (lambda (x n) (zdt-plus-nanos x (* (jt->exact n) 60 nanos-per-sec))))
        (cons "minusMinutes" (lambda (x n) (zdt-plus-nanos x (- (* (jt->exact n) 60 nanos-per-sec)))))
        (cons "plusSeconds" (lambda (x n) (zdt-plus-nanos x (* (jt->exact n) nanos-per-sec))))
        (cons "minusSeconds" (lambda (x n) (zdt-plus-nanos x (- (* (jt->exact n) nanos-per-sec)))))
        (cons "plusNanos" (lambda (x n) (zdt-plus-nanos x (jt->exact n))))
        (cons "minusNanos" (lambda (x n) (zdt-plus-nanos x (- (jt->exact n)))))
        (cons "withYear" (lambda (x v) (zdt-with-ldt x (ldt-combine (ld-with-field (ldt-date (zdt-ldt x)) 'year (jt->exact v)) (ldt-time (zdt-ldt x))))))
        (cons "withMonth" (lambda (x v) (zdt-with-ldt x (ldt-combine (ld-with-field (ldt-date (zdt-ldt x)) 'month (jt->exact v)) (ldt-time (zdt-ldt x))))))
        (cons "withDayOfMonth" (lambda (x v) (zdt-with-ldt x (ldt-combine (ld-with-field (ldt-date (zdt-ldt x)) 'day (jt->exact v)) (ldt-time (zdt-ldt x))))))
        (cons "withDayOfYear" (lambda (x v) (zdt-with-ldt x (ldt-combine (ld-with-field (ldt-date (zdt-ldt x)) 'day-of-year (jt->exact v)) (ldt-time (zdt-ldt x))))))
        (cons "withHour" (lambda (x v) (zdt-with-ldt x (ldt-combine (ldt-date (zdt-ldt x)) (lt-with (ldt-time (zdt-ldt x)) 'hour (jt->exact v))))))
        (cons "withMinute" (lambda (x v) (zdt-with-ldt x (ldt-combine (ldt-date (zdt-ldt x)) (lt-with (ldt-time (zdt-ldt x)) 'minute (jt->exact v))))))
        (cons "withSecond" (lambda (x v) (zdt-with-ldt x (ldt-combine (ldt-date (zdt-ldt x)) (lt-with (ldt-time (zdt-ldt x)) 'second (jt->exact v))))))
        (cons "withNano" (lambda (x v) (zdt-with-ldt x (ldt-combine (ldt-date (zdt-ldt x)) (lt-with (ldt-time (zdt-ldt x)) 'nano (jt->exact v))))))
        (cons "truncatedTo" (lambda (x u) (zdt-with-ldt x (ldt-combine (ldt-date (zdt-ldt x)) (lt-truncate (ldt-time (zdt-ldt x)) u)))))
        (cons "withZoneSameInstant" (lambda (x zone) (zoned-of-instant-nanos (zdt->nanos x) zone)))
        (cons "withZoneSameLocal" (lambda (x zone) (zoned-of-ldt (zdt-ldt x) zone)))
        (cons "withFixedOffsetZone" (lambda (x) (jt-zoned-dt (zdt-epoch-day x) (zdt-nano-of-day x) (zdt-offset x) (jt-zone-id (zo-id (zdt-offset x)) (zdt-offset x)))))
        (cons "plus" (case-lambda ((x a) (zdt-plus-amount x a 1)) ((x n u) (zdt-with-ldt x (temporal-plus-unit (zdt-ldt x) (jt->exact n) (arg-unit-name u))))))
        (cons "minus" (case-lambda ((x a) (zdt-plus-amount x a -1)) ((x n u) (zdt-with-ldt x (temporal-plus-unit (zdt-ldt x) (- (jt->exact n)) (arg-unit-name u))))))
        (cons "until" (lambda (x o u) (unit-between (arg-unit-name u) (zdt-ldt x) (zdt-ldt o))))
        (cons "get" (lambda (x f) (zdt-get-field x (arg-field-name f))))
        (cons "getLong" (lambda (x f) (zdt-get-field x (arg-field-name f))))
        (cons "with" (case-lambda ((x adj) (zdt-with-ldt x (apply-adjuster (zdt-ldt x) adj)))
                                  ((x f v) (zdt-with-ldt x (temporal-with-field (zdt-ldt x) (arg-field-name f) (jt->exact v))))))
        (cons "isSupported" (lambda (x a) (cond ((arg-is-unit? a) (not (string-ci=? (cu-name a) "FOREVER")))
                                                ((arg-is-field? a) #t) (else #f))))
        (cons "range" (lambda (x f) (temporal-range (zdt-ldt x) (arg-field-name f))))
        (cons "isBefore" (lambda (x o) (< (zdt->ms x) (time-ms o))))
        (cons "isAfter" (lambda (x o) (> (zdt->ms x) (time-ms o))))
        (cons "isEqual" (lambda (x o) (= (zdt->ms x) (time-ms o))))
        (cons "compareTo" (lambda (x o) (let ((a (zdt->ms x)) (b (zdt->ms o))) (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals" (lambda (x o) (and (jt-zoned-dt? o) (equal? (jhost-state x) (jhost-state o)))))
        (cons "hashCode" (lambda (x) (jolt-hash (zdt->ms x))))
        (cons "format" (lambda (x fmt) (fmt-format fmt x)))
        (cons "toString" (lambda (x) (zdt->string x)))))
(define (zdt-plus-amount x a sign)
  (cond ((arg-is-amount? a) (if (string=? (jhost-tag a) "duration")
                                (zdt-plus-nanos x (* sign (dur-total-nanos a)))
                                (zdt-with-ldt x (temporal-plus-period (zdt-ldt x) a sign))))
        (else (error #f "ZonedDateTime.plus: bad amount"))))
(define (zdt-get-field x field)
  (let ((f (string-upcase field)))
    (cond ((string=? f "OFFSET_SECONDS") (zdt-offset x))
          ((string=? f "INSTANT_SECONDS") (jt-floor-div (zdt->ms x) 1000))
          (else (temporal-get-field (zdt-ldt x) f)))))
;; epoch-ms of any time-bearing value (for cross-type before/after).
(define (time-ms o)
  (cond ((jt-zoned-dt? o) (zdt->ms o)) ((jt-offset-dt? o) (odt->ms o)) ((jt-instant? o) (inst-ms o))
        ((jt-dt? o) (ldt->ms o)) (else (ms-of o))))

;; --- OffsetDateTime statics + methods ----------------------------------------
(register-class-statics! "OffsetDateTime"
  (list (cons "of" (case-lambda
                     ((ldt off) (offset-of-ldt ldt off))
                     ((d t off) (offset-of-ldt (jt-local-dt (ld-epoch-day d) (lt-nano-of-day t)) off))
                     ((y mo d h mi s nano off)
                      (offset-of-ldt (jt-local-dt (ymd->epoch-day (jt->exact y) (jt->exact mo) (jt->exact d))
                                                  (hmsn->nano (jt->exact h) (jt->exact mi) (jt->exact s) (jt->exact nano))) off))))
        (cons "ofInstant" (lambda (inst zone) (offset-of-instant-ms (ms-of inst) zone)))
        (cons "now" (lambda args (offset-of-instant-ms (now-ms* args) (jt-zone-offset 0))))
        (cons "parse" (lambda (s . fmt)
                        (if (null? fmt) (parse-offset-date-time (jt-str s))
                            (call-with-values (lambda () (formatter-parse-fields (car fmt) s))
                              (lambda (ed nod) (jt-offset-dt ed nod 0))))))
        (cons "from" (lambda (t) (cond ((jt-offset-dt? t) t)
                                       ((jt-zoned-dt? t) (jt-offset-dt (zdt-epoch-day t) (zdt-nano-of-day t) (zdt-offset t)))
                                       (else (offset-of-instant-ms (ms-of t) (jt-zone-offset 0))))))
        (cons "MIN" (jt-offset-dt (ymd->epoch-day -999999999 1 1) 0 (* 18 3600)))
        (cons "MAX" (jt-offset-dt (ymd->epoch-day 999999999 12 31) (- nanos-per-day 1) (* -18 3600)))))
(define (odt-with-ldt x ldt) (jt-offset-dt (ldt-epoch-day ldt) (ldt-nano-of-day ldt) (odt-offset x)))
(define (odt-plus-nanos x nanos) (odt-with-ldt x (ldt-plus-nanos (odt-ldt x) nanos)))
(register-host-methods! "offset-date-time"
  (list (cons "getYear" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (odt-epoch-day x))) (lambda (y m d) y))))
        (cons "getMonthValue" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (odt-epoch-day x))) (lambda (y m d) m))))
        (cons "getMonth" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (odt-epoch-day x))) (lambda (y m d) (jt-month m)))))
        (cons "getDayOfMonth" (lambda (x) (call-with-values (lambda () (epoch-day->ymd (odt-epoch-day x))) (lambda (y m d) d))))
        (cons "getDayOfWeek" (lambda (x) (jt-dow (ld-dow (odt-epoch-day x)))))
        (cons "getDayOfYear" (lambda (x) (ld-day-of-year (odt-epoch-day x))))
        (cons "getHour" (lambda (x) (lt-hour (odt-ldt x))))
        (cons "getMinute" (lambda (x) (lt-minute (odt-ldt x))))
        (cons "getSecond" (lambda (x) (lt-second (odt-ldt x))))
        (cons "getNano" (lambda (x) (lt-nano (odt-ldt x))))
        (cons "getOffset" (lambda (x) (jt-zone-offset (odt-offset x))))
        (cons "toInstant" (lambda (x) (mk-instant-nanos (odt->nanos x))))
        (cons "toLocalDate" (lambda (x) (jt-local-date (odt-epoch-day x))))
        (cons "toLocalTime" (lambda (x) (jt-local-time (odt-nano-of-day x))))
        (cons "toLocalDateTime" (lambda (x) (odt-ldt x)))
        (cons "toOffsetTime" (lambda (x) (jt-offset-time (odt-nano-of-day x) (odt-offset x))))
        (cons "toZonedDateTime" (lambda (x) (jt-zoned-dt (odt-epoch-day x) (odt-nano-of-day x) (odt-offset x) (jt-zone-id (zo-id (odt-offset x)) (odt-offset x)))))
        (cons "toEpochSecond" (lambda (x) (jt-floor-div (odt->ms x) 1000)))
        (cons "atZoneSameInstant" (lambda (x zone) (zoned-of-instant-nanos (odt->nanos x) zone)))
        (cons "atZoneSimilarLocal" (lambda (x zone) (zoned-of-ldt (odt-ldt x) zone)))
        (cons "withOffsetSameInstant" (lambda (x off) (offset-of-instant-ms (odt->ms x) off)))
        (cons "withOffsetSameLocal" (lambda (x off) (offset-of-ldt (odt-ldt x) off)))
        (cons "plusDays" (lambda (x n) (odt-with-ldt x (jt-local-dt (+ (odt-epoch-day x) (jt->exact n)) (odt-nano-of-day x)))))
        (cons "minusDays" (lambda (x n) (odt-with-ldt x (jt-local-dt (- (odt-epoch-day x) (jt->exact n)) (odt-nano-of-day x)))))
        (cons "plusWeeks" (lambda (x n) (odt-with-ldt x (jt-local-dt (+ (odt-epoch-day x) (* 7 (jt->exact n))) (odt-nano-of-day x)))))
        (cons "minusWeeks" (lambda (x n) (odt-with-ldt x (jt-local-dt (- (odt-epoch-day x) (* 7 (jt->exact n))) (odt-nano-of-day x)))))
        (cons "plusMonths" (lambda (x n) (odt-with-ldt x (ldt-combine (ld-plus-months (ldt-date (odt-ldt x)) (jt->exact n)) (ldt-time (odt-ldt x))))))
        (cons "minusMonths" (lambda (x n) (odt-with-ldt x (ldt-combine (ld-plus-months (ldt-date (odt-ldt x)) (- (jt->exact n))) (ldt-time (odt-ldt x))))))
        (cons "plusYears" (lambda (x n) (odt-with-ldt x (ldt-combine (ld-plus-years (ldt-date (odt-ldt x)) (jt->exact n)) (ldt-time (odt-ldt x))))))
        (cons "minusYears" (lambda (x n) (odt-with-ldt x (ldt-combine (ld-plus-years (ldt-date (odt-ldt x)) (- (jt->exact n))) (ldt-time (odt-ldt x))))))
        (cons "plusHours" (lambda (x n) (odt-plus-nanos x (* (jt->exact n) 3600 nanos-per-sec))))
        (cons "minusHours" (lambda (x n) (odt-plus-nanos x (- (* (jt->exact n) 3600 nanos-per-sec)))))
        (cons "plusMinutes" (lambda (x n) (odt-plus-nanos x (* (jt->exact n) 60 nanos-per-sec))))
        (cons "minusMinutes" (lambda (x n) (odt-plus-nanos x (- (* (jt->exact n) 60 nanos-per-sec)))))
        (cons "plusSeconds" (lambda (x n) (odt-plus-nanos x (* (jt->exact n) nanos-per-sec))))
        (cons "minusSeconds" (lambda (x n) (odt-plus-nanos x (- (* (jt->exact n) nanos-per-sec)))))
        (cons "plusNanos" (lambda (x n) (odt-plus-nanos x (jt->exact n))))
        (cons "minusNanos" (lambda (x n) (odt-plus-nanos x (- (jt->exact n)))))
        (cons "withYear" (lambda (x v) (odt-with-ldt x (ldt-combine (ld-with-field (ldt-date (odt-ldt x)) 'year (jt->exact v)) (ldt-time (odt-ldt x))))))
        (cons "withMonth" (lambda (x v) (odt-with-ldt x (ldt-combine (ld-with-field (ldt-date (odt-ldt x)) 'month (jt->exact v)) (ldt-time (odt-ldt x))))))
        (cons "withDayOfMonth" (lambda (x v) (odt-with-ldt x (ldt-combine (ld-with-field (ldt-date (odt-ldt x)) 'day (jt->exact v)) (ldt-time (odt-ldt x))))))
        (cons "withDayOfYear" (lambda (x v) (odt-with-ldt x (ldt-combine (ld-with-field (ldt-date (odt-ldt x)) 'day-of-year (jt->exact v)) (ldt-time (odt-ldt x))))))
        (cons "withHour" (lambda (x v) (odt-with-ldt x (ldt-combine (ldt-date (odt-ldt x)) (lt-with (ldt-time (odt-ldt x)) 'hour (jt->exact v))))))
        (cons "withMinute" (lambda (x v) (odt-with-ldt x (ldt-combine (ldt-date (odt-ldt x)) (lt-with (ldt-time (odt-ldt x)) 'minute (jt->exact v))))))
        (cons "withSecond" (lambda (x v) (odt-with-ldt x (ldt-combine (ldt-date (odt-ldt x)) (lt-with (ldt-time (odt-ldt x)) 'second (jt->exact v))))))
        (cons "withNano" (lambda (x v) (odt-with-ldt x (ldt-combine (ldt-date (odt-ldt x)) (lt-with (ldt-time (odt-ldt x)) 'nano (jt->exact v))))))
        (cons "truncatedTo" (lambda (x u) (odt-with-ldt x (ldt-combine (ldt-date (odt-ldt x)) (lt-truncate (ldt-time (odt-ldt x)) u)))))
        (cons "plus" (case-lambda ((x a) (odt-plus-amount x a 1)) ((x n u) (odt-with-ldt x (temporal-plus-unit (odt-ldt x) (jt->exact n) (arg-unit-name u))))))
        (cons "minus" (case-lambda ((x a) (odt-plus-amount x a -1)) ((x n u) (odt-with-ldt x (temporal-plus-unit (odt-ldt x) (- (jt->exact n)) (arg-unit-name u))))))
        (cons "until" (lambda (x o u) (unit-between (arg-unit-name u) (odt-ldt x) (odt-ldt o))))
        (cons "get" (lambda (x f) (odt-get-field x (arg-field-name f))))
        (cons "getLong" (lambda (x f) (odt-get-field x (arg-field-name f))))
        (cons "with" (case-lambda ((x adj) (odt-with-ldt x (apply-adjuster (odt-ldt x) adj)))
                                  ((x f v) (odt-with-ldt x (temporal-with-field (odt-ldt x) (arg-field-name f) (jt->exact v))))))
        (cons "isSupported" (lambda (x a) (cond ((arg-is-unit? a) (not (string-ci=? (cu-name a) "FOREVER"))) ((arg-is-field? a) #t) (else #f))))
        (cons "range" (lambda (x f) (temporal-range (odt-ldt x) (arg-field-name f))))
        (cons "isBefore" (lambda (x o) (< (odt->ms x) (time-ms o))))
        (cons "isAfter" (lambda (x o) (> (odt->ms x) (time-ms o))))
        (cons "isEqual" (lambda (x o) (= (odt->ms x) (time-ms o))))
        (cons "compareTo" (lambda (x o) (let ((a (odt->ms x)) (b (odt->ms o))) (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals" (lambda (x o) (and (jt-offset-dt? o) (equal? (jhost-state x) (jhost-state o)))))
        (cons "hashCode" (lambda (x) (jolt-hash (odt->ms x))))
        (cons "format" (lambda (x fmt) (fmt-format fmt x)))
        (cons "toString" (lambda (x) (odt->string x)))))
(define (odt-plus-amount x a sign)
  (cond ((arg-is-amount? a) (if (string=? (jhost-tag a) "duration")
                                (odt-plus-nanos x (* sign (dur-total-nanos a)))
                                (odt-with-ldt x (temporal-plus-period (odt-ldt x) a sign))))
        (else (error #f "OffsetDateTime.plus: bad amount"))))
(define (odt-get-field x field)
  (let ((f (string-upcase field)))
    (cond ((string=? f "OFFSET_SECONDS") (odt-offset x))
          ((string=? f "INSTANT_SECONDS") (jt-floor-div (odt->ms x) 1000))
          (else (temporal-get-field (odt-ldt x) f)))))

;; --- OffsetTime statics + methods --------------------------------------------
(register-class-statics! "OffsetTime"
  (list (cons "of" (case-lambda
                     ((t off) (jt-offset-time (lt-nano-of-day t) (zo-secs* off)))
                     ((h m s nano off) (jt-offset-time (hmsn->nano (jt->exact h) (jt->exact m) (jt->exact s) (jt->exact nano)) (zo-secs* off)))))
        (cons "ofInstant" (lambda (inst zone) (let ((od (offset-of-instant-ms (ms-of inst) zone)))
                                                (jt-offset-time (odt-nano-of-day od) (odt-offset od)))))
        (cons "now" (lambda args (jt-offset-time (* (jt-floor-mod (exact (truncate (now-ms* args))) 86400000) 1000000) 0)))
        (cons "parse" (lambda (s . _) (parse-offset-time (jt-str s))))
        (cons "from" (lambda (t) (cond ((jt-offset-time? t) t)
                                       ((jt-offset-dt? t) (jt-offset-time (odt-nano-of-day t) (odt-offset t)))
                                       (else (error #f "OffsetTime/from: unsupported")))))
        (cons "MIN" (jt-offset-time 0 (* 18 3600)))
        (cons "MAX" (jt-offset-time (- nanos-per-day 1) (* -18 3600)))))
(define (zo-secs* off) (cond ((jt-zone-offset? off) (zo-secs off)) ((jt-zone-id? off) (zid-off off)) (else (parse-zone-offset off))))
(define (ot-with x nod) (jt-offset-time nod (ot-offset x)))
(register-host-methods! "offset-time"
  (list (cons "getHour" (lambda (x) (lt-hour (jt-local-time (ot-nod x)))))
        (cons "getMinute" (lambda (x) (lt-minute (jt-local-time (ot-nod x)))))
        (cons "getSecond" (lambda (x) (lt-second (jt-local-time (ot-nod x)))))
        (cons "getNano" (lambda (x) (lt-nano (jt-local-time (ot-nod x)))))
        (cons "getOffset" (lambda (x) (jt-zone-offset (ot-offset x))))
        (cons "toLocalTime" (lambda (x) (jt-local-time (ot-nod x))))
        (cons "atDate" (lambda (x d) (jt-offset-dt (ld-epoch-day d) (ot-nod x) (ot-offset x))))
        (cons "plusHours" (lambda (x n) (ot-with x (jt-floor-mod (+ (ot-nod x) (* (jt->exact n) 3600 nanos-per-sec)) nanos-per-day))))
        (cons "minusHours" (lambda (x n) (ot-with x (jt-floor-mod (- (ot-nod x) (* (jt->exact n) 3600 nanos-per-sec)) nanos-per-day))))
        (cons "plusMinutes" (lambda (x n) (ot-with x (jt-floor-mod (+ (ot-nod x) (* (jt->exact n) 60 nanos-per-sec)) nanos-per-day))))
        (cons "minusMinutes" (lambda (x n) (ot-with x (jt-floor-mod (- (ot-nod x) (* (jt->exact n) 60 nanos-per-sec)) nanos-per-day))))
        (cons "plusSeconds" (lambda (x n) (ot-with x (jt-floor-mod (+ (ot-nod x) (* (jt->exact n) nanos-per-sec)) nanos-per-day))))
        (cons "minusSeconds" (lambda (x n) (ot-with x (jt-floor-mod (- (ot-nod x) (* (jt->exact n) nanos-per-sec)) nanos-per-day))))
        (cons "plusNanos" (lambda (x n) (ot-with x (jt-floor-mod (+ (ot-nod x) (jt->exact n)) nanos-per-day))))
        (cons "minusNanos" (lambda (x n) (ot-with x (jt-floor-mod (- (ot-nod x) (jt->exact n)) nanos-per-day))))
        (cons "withHour" (lambda (x v) (ot-with x (lt-nano-of-day (lt-with (jt-local-time (ot-nod x)) 'hour (jt->exact v))))))
        (cons "withMinute" (lambda (x v) (ot-with x (lt-nano-of-day (lt-with (jt-local-time (ot-nod x)) 'minute (jt->exact v))))))
        (cons "withSecond" (lambda (x v) (ot-with x (lt-nano-of-day (lt-with (jt-local-time (ot-nod x)) 'second (jt->exact v))))))
        (cons "withNano" (lambda (x v) (ot-with x (lt-nano-of-day (lt-with (jt-local-time (ot-nod x)) 'nano (jt->exact v))))))
        (cons "truncatedTo" (lambda (x u) (ot-with x (lt-nano-of-day (lt-truncate (jt-local-time (ot-nod x)) u)))))
        (cons "withOffsetSameLocal" (lambda (x off) (jt-offset-time (ot-nod x) (zo-secs* off))))
        (cons "isBefore" (lambda (x o) (< (- (ot-nod x) (* (ot-offset x) nanos-per-sec)) (- (ot-nod o) (* (ot-offset o) nanos-per-sec)))))
        (cons "isAfter" (lambda (x o) (> (- (ot-nod x) (* (ot-offset x) nanos-per-sec)) (- (ot-nod o) (* (ot-offset o) nanos-per-sec)))))
        (cons "isEqual" (lambda (x o) (= (- (ot-nod x) (* (ot-offset x) nanos-per-sec)) (- (ot-nod o) (* (ot-offset o) nanos-per-sec)))))
        (cons "compareTo" (lambda (x o) (let ((a (- (ot-nod x) (* (ot-offset x) nanos-per-sec))) (b (- (ot-nod o) (* (ot-offset o) nanos-per-sec))))
                                          (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals" (lambda (x o) (and (jt-offset-time? o) (equal? (jhost-state x) (jhost-state o)))))
        (cons "hashCode" (lambda (x) (jolt-hash (ot-nod x))))
        (cons "format" (lambda (x fmt) (fmt-format fmt x)))
        (cons "toString" (lambda (x) (ot->string x)))))

;; --- ISO parse for the offset/zoned types ------------------------------------
;; split off an offset/zone suffix at the end of an ISO datetime string; -> (values
;; local-part offset-secs zone-id-or-#f). offset starts at the first +/-/Z after T.
(define (split-offset-zone s)
  (let ((len (string-length s))
        (ti (let loop ((i 0)) (cond ((>= i (string-length s)) #f)
                                    ((or (char=? (string-ref s i) #\T) (char=? (string-ref s i) #\t)) i)
                                    (else (loop (+ i 1)))))))
    ;; bracket zone "[Region/City]" at the very end
    (let* ((zone-id (and (> len 0) (char=? (string-ref s (- len 1)) #\])
                         (let loop ((i (- len 2))) (cond ((< i 0) #f)
                                                         ((char=? (string-ref s i) #\[) (substring s (+ i 1) (- len 1)))
                                                         (else (loop (- i 1)))))))
           (s2 (if zone-id (let loop ((i (- len 1))) (if (char=? (string-ref s i) #\[) (substring s 0 i) (loop (- i 1)))) s))
           (len2 (string-length s2))
           ;; find the offset start (after the T)
           (oi (let loop ((i (if ti (+ ti 1) 0)))
                 (cond ((>= i len2) #f)
                       ((or (char=? (string-ref s2 i) #\Z) (char=? (string-ref s2 i) #\z)) i)
                       ((and (> i (if ti (+ ti 1) 0)) (or (char=? (string-ref s2 i) #\+) (char=? (string-ref s2 i) #\-))) i)
                       (else (loop (+ i 1)))))))
      (if oi
          (values (substring s2 0 oi) (parse-zone-offset (substring s2 oi len2)) zone-id)
          (values s2 0 zone-id)))))
(define (parse-zoned-date-time s)
  (call-with-values (lambda () (split-offset-zone s))
    (lambda (local off zone-id)
      (call-with-values (lambda () (parse-iso-datetime local))
        (lambda (ed nod)
          (let ((zid (if zone-id (zone-id-of zone-id) (jt-zone-id (zo-id off) off))))
            (jt-zoned-dt ed nod off zid)))))))
(define (parse-offset-date-time s)
  (call-with-values (lambda () (split-offset-zone s))
    (lambda (local off zone-id)
      (call-with-values (lambda () (parse-iso-datetime local)) (lambda (ed nod) (jt-offset-dt ed nod off))))))
(define (parse-offset-time s)
  ;; "HH:mm[:ss]±HH:mm" / "...Z"
  (let ((oi (let loop ((i 0)) (cond ((>= i (string-length s)) #f)
                                    ((or (char=? (string-ref s i) #\Z) (char=? (string-ref s i) #\z)) i)
                                    ((and (> i 0) (or (char=? (string-ref s i) #\+) (char=? (string-ref s i) #\-))) i)
                                    (else (loop (+ i 1)))))))
    (if oi (jt-offset-time (parse-iso-time (substring s 0 oi)) (parse-zone-offset (substring s oi (string-length s))))
        (jt-offset-time (parse-iso-time s) 0))))

;; --- formatter integration ---------------------------------------------------
;; format any java.time value through a dt-formatter pattern. Builds the broken-down
;; fields from the value's local representation; X/Z/x render its offset, z its zone.
(define (jt-value-format-parts v)         ; -> (vector y mo d hh mi se nano dow offset-secs zone-id)
  (define (from-ed-nod ed nod off zid)
    (call-with-values (lambda () (epoch-day->ymd ed))
      (lambda (y m d)
        (let ((t (jt-local-time nod)))
          (vector y m d (lt-hour t) (lt-minute t) (lt-second t) (lt-nano t) (jt-floor-mod (+ ed 4) 7) off zid)))))
  (cond
    ((jt-date? v) (call-with-values (lambda () (epoch-day->ymd (ld-epoch-day v)))
                    (lambda (y m d) (vector y m d 0 0 0 0 (jt-floor-mod (+ (ld-epoch-day v) 4) 7) 0 #f))))
    ((jt-time? v) (let ((t v)) (vector 1970 1 1 (lt-hour t) (lt-minute t) (lt-second t) (lt-nano t) 4 0 #f)))
    ((jt-dt? v) (from-ed-nod (ldt-epoch-day v) (ldt-nano-of-day v) 0 #f))
    ((jt-zoned-dt? v) (from-ed-nod (zdt-epoch-day v) (zdt-nano-of-day v) (zdt-offset v) (zid-id (zdt-zone v))))
    ((jt-offset-dt? v) (from-ed-nod (odt-epoch-day v) (odt-nano-of-day v) (odt-offset v) #f))
    ((jt-offset-time? v) (let ((t (jt-local-time (ot-nod v)))) (vector 1970 1 1 (lt-hour t) (lt-minute t) (lt-second t) (lt-nano t) 4 (ot-offset v) #f)))
    ((jt-instant? v) #f)                  ; instants render via format-ms (UTC)
    (else #f)))
;; Locale month/day names via libc strftime (primary) with English fallback.
;; The locale id (e.g. "de", "fr") is mapped to a libc locale string and strftime
;; with %B/%b/%A/%a produces localized names in UTF-8. Mutex-guarded (setlocale
;; mutates process-global state). Falls back to English on unavailable locale.
(define (locale-month-name loc mo full?)
  (locale-name-via-strftime loc (- mo 1) 0 (if full? "%B" "%b") full?))
(define (locale-day-name loc dow full?)
  (locale-name-via-strftime loc 0 dow (if full? "%A" "%a") full?))

;; the pattern engine for java.time values (extends inst-time.ss's format-ms letters
;; with fractional S and real X/x/Z/z from the value's own offset/zone).
(define (jt-format-pattern pattern v . loc)
  (define locale (if (null? loc) "en" (car loc)))
  (let ((parts (jt-value-format-parts v)))
    (if (not parts)
        (format-ms pattern (ms-of v))     ; instant: UTC engine
        (let ((y (vector-ref parts 0)) (mo (vector-ref parts 1)) (d (vector-ref parts 2))
              (hh (vector-ref parts 3)) (mi (vector-ref parts 4)) (se (vector-ref parts 5))
              (nano (vector-ref parts 6)) (dow (vector-ref parts 7))
              (off (vector-ref parts 8)) (zid (vector-ref parts 9))
              (n (string-length pattern)) (out (open-output-string)))
          (define (run-len i c) (let loop ((j i)) (if (and (< j n) (char=? (string-ref pattern j) c)) (loop (+ j 1)) (- j i))))
          (define (off-iso secs colon allow-z)
            (if (and allow-z (= secs 0)) "Z"
                (let* ((neg (< secs 0)) (a (abs secs)) (h (quotient a 3600)) (m (quotient (modulo a 3600) 60)) (s (modulo a 60)))
                  (string-append (if neg "-" "+") (pad2 h) (if colon ":" "") (pad2 m)
                                 (if (= s 0) "" (string-append (if colon ":" "") (pad2 s)))))))
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
                  ;; y = year-of-era, Y = week-based-year; for whole dates they agree,
                  ;; so render Y like y (no ISO-week calendar here).
                  ((or (char=? c #\y) (char=? c #\Y)) (display (if (>= k 4) (pad4 y) (pad2 (modulo y 100))) out) (loop (+ i k)))
                  ((char=? c #\M)
                   (display (cond ((= k 1) (number->string mo)) ((= k 2) (pad2 mo))
                                  ((= k 3) (locale-month-name locale mo #f))
                                  (else (locale-month-name locale mo #t))) out) (loop (+ i k)))
                  ((char=? c #\d) (display (if (= k 1) (number->string d) (pad2 d)) out) (loop (+ i k)))
                  ((char=? c #\E) (display (locale-day-name locale dow (>= k 4)) out) (loop (+ i k)))
                  ((char=? c #\H) (display (if (= k 1) (number->string hh) (pad2 hh)) out) (loop (+ i k)))
                  ((char=? c #\h) (let ((h12 (let ((h (modulo hh 12))) (if (= h 0) 12 h)))) (display (if (= k 1) (number->string h12) (pad2 h12)) out)) (loop (+ i k)))
                  ((char=? c #\m) (display (if (= k 1) (number->string mi) (pad2 mi)) out) (loop (+ i k)))
                  ((char=? c #\s) (display (if (= k 1) (number->string se) (pad2 se)) out) (loop (+ i k)))
                  ((char=? c #\S) (let ((str (let ((p (number->string (quotient nano (expt 10 (max 0 (- 9 k))))))) p)))
                                    (display (string-append (make-string (max 0 (- k (string-length str))) #\0) str) out)) (loop (+ i k)))
                  ((char=? c #\a) (display (if (< hh 12) "AM" "PM") out) (loop (+ i k)))
                  ((char=? c #\X) (display (off-iso off (>= k 3) #t) out) (loop (+ i k)))
                  ((char=? c #\x) (display (off-iso off (>= k 3) #f) out) (loop (+ i k)))
                  ((char=? c #\Z) (display (if (>= k 3) (off-iso off #t #f) (off-iso off #f #f)) out) (loop (+ i k)))
                  ((char=? c #\V) (display (or zid (off-iso off #t #t)) out) (loop (+ i k)))
                  ((char=? c #\z) (display (or zid (off-iso off #t #t)) out) (loop (+ i k)))
                  (else (write-char c out) (loop (+ i 1)))))))
          (get-output-string out)))))

;; .format on a dt-formatter applied to a java.time value. The pattern comes from the
;; formatter; ISO_* constants carry a literal pattern that round-trips through here.
(define (fmt-format fmt v)
  (cond ((and (jhost? fmt) (string=? (jhost-tag fmt) "dt-formatter")) (jt-format-pattern (fmt-pat fmt) v (fmt-locale fmt)))
        ((string? fmt) (jt-format-pattern fmt v))
        (else (jt-value->iso v))))
(define (jt-value->iso v)
  (cond ((jt-date? v) (iso-date-str (ld-epoch-day v))) ((jt-time? v) (iso-time-str (lt-nano-of-day v)))
        ((jt-dt? v) (iso-datetime-str (ldt-epoch-day v) (ldt-nano-of-day v)))
        ((jt-zoned-dt? v) (zdt->string v)) ((jt-offset-dt? v) (odt->string v))
        ((jt-offset-time? v) (ot->string v)) ((jt-instant? v) (iso-instant-str-nanos (inst-nanos v)))
        (else (jolt-str-render-one v))))

;; .format on the new value types reaches the per-tag method tables above. Add
;; .format to local-date/time/date-time too (they previously had none).
(register-host-methods! "local-date" (list (cons "format" (lambda (x fmt) (fmt-format fmt x)))))
(register-host-methods! "local-time" (list (cons "format" (lambda (x fmt) (fmt-format fmt x)))))
(register-host-methods! "local-date-time" (list (cons "format" (lambda (x fmt) (fmt-format fmt x)))))
(register-host-methods! "instant" (list (cons "format" (lambda (x fmt) (fmt-format fmt x)))))

;; extend the dt-formatter method table: .format routes java.time values through the
;; richer engine; .parse picks the value type from the parsed fields.
(register-host-methods! "dt-formatter"
  (list (cons "format" (lambda (self d) (jt-format-pattern (fmt-pat self) d (fmt-locale self))))
        (cons "parse" (lambda (self s) (mk-instant (jinst-ms (parse-ms (fmt-pat self) (jt-str s))))))
        (cons "withLocale" (lambda (self locale) (mk-formatter (fmt-pat self) (locale-id locale))))
        (cons "withZone" (lambda (self zone) (mk-formatter (fmt-pat self) (fmt-locale self))))
        (cons "getZone" (lambda (self) (jt-zone-id "Z" 0)))
        (cons "getLocale" (lambda (self) (make-jhost "locale" (vector (fmt-locale self)))))
        (cons "toString" (lambda (self) (fmt-pat self)))))

;; DateTimeFormatter ISO constants (richer engine; the pattern strings drive parse +
;; format). Re-registers ofPattern so the format-aware Local*/parse statics see them.
(register-class-statics! "DateTimeFormatter"
  (list (cons "ofPattern" (lambda (p . rest) (mk-formatter (jt-str p) (if (pair? rest) (locale-id (car rest)) "en"))))
        (cons "ISO_LOCAL_DATE" (mk-formatter "yyyy-MM-dd"))
        (cons "ISO_LOCAL_TIME" (mk-formatter "HH:mm:ss"))
        (cons "ISO_LOCAL_DATE_TIME" (mk-formatter "yyyy-MM-dd'T'HH:mm:ss"))
        (cons "ISO_DATE" (mk-formatter "yyyy-MM-dd"))
        (cons "ISO_TIME" (mk-formatter "HH:mm:ss"))
        (cons "ISO_DATE_TIME" (mk-formatter "yyyy-MM-dd'T'HH:mm:ss"))
        (cons "ISO_INSTANT" (mk-formatter "yyyy-MM-dd'T'HH:mm:ssX"))
        (cons "ISO_OFFSET_DATE_TIME" (mk-formatter "yyyy-MM-dd'T'HH:mm:ssXXX"))
        (cons "ISO_OFFSET_TIME" (mk-formatter "HH:mm:ssXXX"))
        (cons "ISO_OFFSET_DATE" (mk-formatter "yyyy-MM-ddXXX"))
        (cons "ISO_ZONED_DATE_TIME" (mk-formatter "yyyy-MM-dd'T'HH:mm:ssXXX"))
        (cons "ISO_ORDINAL_DATE" (mk-formatter "yyyy-DDD"))
        (cons "ISO_WEEK_DATE" (mk-formatter "yyyy-'W'ww-e"))
        (cons "BASIC_ISO_DATE" (mk-formatter "yyyyMMdd"))
        (cons "RFC_1123_DATE_TIME" (mk-formatter "EEE, dd MMM yyyy HH:mm:ss Z"))))

;; Local*/parse with a formatter: parse via the pattern, then build the right value.
;; The parse path reuses inst-time.ss parse-ms (UTC) and reads back the fields.
(define (formatter-parse-fields fmt s)
  (let ((ms (jinst-ms (parse-ms (fmt-pat fmt) (jt-str s)))))
    (let ((ed (jt-floor-div (exact (truncate ms)) 86400000)) (nod (* (jt-floor-mod (exact (truncate ms)) 86400000) 1000000)))
      (values ed nod))))
(register-class-statics! "LocalDate"
  (list (cons "parse" (lambda (s . fmt)
                        (if (null? fmt) (jt-local-date (parse-iso-date (jt-str s)))
                            (call-with-values (lambda () (formatter-parse-fields (car fmt) s)) (lambda (ed nod) (jt-local-date ed))))))))
(register-class-statics! "LocalTime"
  (list (cons "parse" (lambda (s . fmt)
                        (if (null? fmt) (jt-local-time (parse-iso-time (jt-str s)))
                            (call-with-values (lambda () (formatter-parse-fields (car fmt) s)) (lambda (ed nod) (jt-local-time nod))))))))
(register-class-statics! "LocalDateTime"
  (list (cons "parse" (lambda (s . fmt)
                        (if (null? fmt)
                            (call-with-values (lambda () (parse-iso-datetime (jt-str s))) (lambda (ed nod) (jt-local-dt ed nod)))
                            (call-with-values (lambda () (formatter-parse-fields (car fmt) s)) (lambda (ed nod) (jt-local-dt ed nod))))))))

;; --- ms-of / equality / hash / compare / print / instance? for Phase-3 types --
;; extend ms-of so ZonedDateTime/OffsetDateTime/OffsetTime route through to an epoch-ms.
(define %p3-ms-of ms-of)
(set! ms-of (lambda (d)
              (cond ((jt-zoned-dt? d) (zdt->ms d))
                    ((jt-offset-dt? d) (odt->ms d))
                    (else (%p3-ms-of d)))))

(register-jt-value! "zone-offset" (lambda (x) (zo-id (zo-secs x))) (lambda (x) (jolt-hash (zo-secs x))))
(register-jt-value! "zone-id" (lambda (x) (zid-id x)) (lambda (x) (jolt-hash (string-hash (zid-id x)))))
(register-str-render! jt-zoned-dt? zdt->string) (register-pr-arm! jt-zoned-dt? zdt->string)
(register-str-render! jt-offset-dt? odt->string) (register-pr-arm! jt-offset-dt? odt->string)
(register-str-render! jt-offset-time? ot->string) (register-pr-arm! jt-offset-time? ot->string)
(register-hash-arm! jt-zoned-dt? (lambda (x) (jolt-hash (zdt->ms x))))
(register-hash-arm! jt-offset-dt? (lambda (x) (jolt-hash (odt->ms x))))
(register-hash-arm! jt-offset-time? (lambda (x) (jolt-hash (ot-nod x))))
;; ZonedDateTime equality is local-date-time + offset + zone id (java.time
;; ZonedDateTime.equals). Compare fields, not the raw state: the state holds a
;; nested zone-id record that Chez equal? compares by identity, not contents.
(register-eq-arm! (lambda (a b) (or (jt-zoned-dt? a) (jt-zoned-dt? b)))
                  (lambda (a b) (and (jt-zoned-dt? a) (jt-zoned-dt? b)
                                     (= (zdt-epoch-day a) (zdt-epoch-day b))
                                     (= (zdt-nano-of-day a) (zdt-nano-of-day b))
                                     (= (zdt-offset a) (zdt-offset b))
                                     (string=? (zid-id (zdt-zone a)) (zid-id (zdt-zone b))))))
(register-eq-arm! (lambda (a b) (or (jt-offset-dt? a) (jt-offset-dt? b)))
                  (lambda (a b) (and (jt-offset-dt? a) (jt-offset-dt? b) (equal? (jhost-state a) (jhost-state b)))))
(register-eq-arm! (lambda (a b) (or (jt-offset-time? a) (jt-offset-time? b)))
                  (lambda (a b) (and (jt-offset-time? a) (jt-offset-time? b) (equal? (jhost-state a) (jhost-state b)))))

;; compare for Phase-3 types (same-type only).
(define %p3-prev-compare jolt-compare)
(set! jolt-compare
  (lambda (a b)
    (cond
      ((and (jt-zoned-dt? a) (jt-zoned-dt? b)) (let ((x (zdt->ms a)) (y (zdt->ms b))) (cond ((< x y) -1) ((> x y) 1) (else 0))))
      ((and (jt-offset-dt? a) (jt-offset-dt? b)) (let ((x (odt->ms a)) (y (odt->ms b))) (cond ((< x y) -1) ((> x y) 1) (else 0))))
      ((and (jt-zone-offset? a) (jt-zone-offset? b)) (let ((x (zo-secs a)) (y (zo-secs b))) (cond ((< x y) -1) ((> x y) 1) (else 0))))
      ;; #inst (java.util.Date) values compare by epoch-ms.
      ((and (jinst? a) (jinst? b)) (let ((x (jinst-ms a)) (y (jinst-ms b))) (cond ((< x y) -1) ((> x y) 1) (else 0))))
      (else (%p3-prev-compare a b)))))
(def-var! "clojure.core" "compare" jolt-compare)

;; instance? for the Phase-3 tags.
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tn (short-class-name (symbol-t-name type-sym))))
      (cond
        ((jt-zone-offset? val) (if (member tn '("ZoneOffset" "ZoneId")) #t 'pass))
        ((jt-zone-id? val) (if (string=? tn "ZoneId") #t 'pass))
        ((jt-zoned-dt? val) (if (string=? tn "ZonedDateTime") #t 'pass))
        ((jt-offset-dt? val) (if (string=? tn "OffsetDateTime") #t 'pass))
        ((jt-offset-time? val) (if (string=? tn "OffsetTime") #t 'pass))
        ((and (jhost? val) (string=? (jhost-tag val) "clock")) (if (string=? tn "Clock") #t 'pass))
        ((and (jhost? val) (string=? (jhost-tag val) "dt-formatter")) (if (string=? tn "DateTimeFormatter") #t 'pass))
        ((and (jhost? val) (string=? (jhost-tag val) "temporal-adjuster")) (if (member tn '("TemporalAdjuster")) #t 'pass))
        (else 'pass)))))

;; DateTimeFormatterBuilder — accumulates a pattern and defaults; toFormatter
;; builds a dt-formatter from the accumulated pattern via mk-formatter (the same
;; engine DateTimeFormatter/ofPattern uses). When no pattern was appended the
;; builder falls back to the lenient-ISO formatter, preserving the spec-tools path.
(define (builder-pat self) (vector-ref (jhost-state self) 0))
(define (builder-defs self) (vector-ref (jhost-state self) 1))
(define (builder-pat-set! self v) (vector-set! (jhost-state self) 0 v))
(define (builder-defs-set! self v) (vector-set! (jhost-state self) 1 v))
(define (append-pattern-str self p)
  (builder-pat-set! self (string-append (builder-pat self) (jt-str p)))
  self)
(define (builder-append-literal self lit)
  (let ((s (jt-str lit)))
    (builder-pat-set! self
      (string-append (builder-pat self) "'" (jt-str-replace s "'" "''") "'"))
    self))
(register-class-ctor! "DateTimeFormatterBuilder"
  (lambda _ (make-jhost "dtf-builder" (vector "" '()))))
(register-host-methods! "dtf-builder"
  (list (cons "appendPattern" append-pattern-str)
        (cons "appendLiteral" builder-append-literal)
        (cons "appendOptional"
              (lambda (self x)
                (let ((p (if (and (jhost? x) (string=? (jhost-tag x) "dt-formatter"))
                             (fmt-pat x)
                             (jt-str x))))
                  (builder-pat-set! self (string-append (builder-pat self) "[" p "]"))
                  self)))
        (cons "appendValue" (lambda (self . _) self))
        (cons "parseDefaulting"
              (lambda (self f v)
                (builder-defs-set! self (cons (cons f v) (builder-defs self)))
                self))
        (cons "parseCaseInsensitive" (lambda (self) self))
        (cons "toFormatter"
              (lambda (self . _)
                (let ((p (builder-pat self)))
                  (if (string=? p "")
                      (make-jhost "lenient-iso-dtf" (vector #f))
                      (mk-formatter p)))))))
(register-host-methods! "lenient-iso-dtf"
  (list (cons "parse" (lambda (self s . _)
          (let ((str (jt-str s)))
            (if (and (>= (string-length str) 10)
                     (or (= 10 (string-length str))
                         (char=? (string-ref str 10) #\T))
                     (char=? (string-ref str 4) #\-))
                (if (= 10 (string-length str))
                    (mk-instant-nanos (* (parse-iso-date str) 86400 nanos-per-sec))
                    (guard (e (#t (mk-instant (jinst-ms (jolt-inst-from-string str)))))
                      (mk-instant-nanos (parse-iso-instant-nanos
                                         (if (char=? (string-ref str (- (string-length str) 1)) #\Z)
                                             str (string-append str "Z"))))))
                (error #f (string-append "could not parse: " str))))))
        (cons "format" (lambda (self t . _) (jolt-str-render-one t)))))
