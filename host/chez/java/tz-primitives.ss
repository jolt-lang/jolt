;; tz-primitives.ss — the irreducible libc time primitives, exposed to Clojure.
;;
;; The java.time.* implementation lives in the jolt-lang/time library as portable
;; Clojure, but two operations need libc and can't be expressed there: the offset
;; of a named IANA zone at an instant (localtime/tzset reading /usr/share/zoneinfo)
;; and locale-specific month/day names (strftime). Both degrade gracefully — the
;; library falls back to its own rule tables / English names when these return nil.
;;
;; jolt.host/tz-offset-seconds  (zone-id epoch-seconds) -> seconds east of UTC | nil
;; jolt.host/locale-name        (locale-id tm-mon tm-wday fmt) -> string | nil
;; jolt.host/tz-backend         :libc | :fallback

;; LC_TIME varies by platform: 5 on macOS, 2 on Linux/glibc.
(define tzp-LC_TIME
  (case (machine-type)
    ((a6osx t3osx i3osx ta6osx tarm64osx arm64osx) 5)
    (else 2)))
(define tzp-mutex (make-mutex))

;; Guard-wrapped FFI via jolt-foreign-proc-safe (deferred symbol lookup so a
;; missing entry doesn't abort boot). nil on failure -> graceful fallback.
(define tzp-setlocale (jolt-foreign-proc-safe "setlocale" '(int string) 'void*))
(define tzp-strftime  (jolt-foreign-proc-safe "strftime"  '(u8* size_t string void*) 'size_t))
(define tzp-setenv    (jolt-foreign-proc-safe "setenv"    '(string string int) 'int))
(define tzp-tzset     (jolt-foreign-proc-safe "tzset"     '() 'void))
(define tzp-localtime (jolt-foreign-proc-safe "localtime" '(void*) 'void*))

(define tzp-locale-available?
  (and tzp-setlocale tzp-strftime
       (guard (e (#t #f))
         (let ((r (tzp-setlocale tzp-LC_TIME "en_US.UTF-8"))) (and r (not (eq? r 0)))))))

;; FFI symbols present? (setenv is POSIX-only, so this is #f on Windows).
(define tzp-tz-symbols? (and tzp-setenv tzp-tzset tzp-localtime))

;; zone offset in seconds east of UTC at an epoch-second instant, via libc.
;; struct tm layout (64-bit): 9 ints(36) + pad(4) + long tm_gmtoff at byte 40.
;; localtime returns a STATIC buffer — do NOT foreign-free it.
(define (tzp-offset-raw zone epoch)
  (and tzp-tz-symbols?
       (with-mutex tzp-mutex
         (tzp-setenv "TZ" zone 1)
         (tzp-tzset)
         (let ((tp (foreign-alloc 8)))
           (foreign-set! 'long tp 0 epoch)
           (let ((tm (tzp-localtime tp)))
             (foreign-free tp)
             (and tm (not (eq? tm 0)) (foreign-ref 'long tm 40)))))))

;; Capability probe: trust libc only if it returns known-correct offsets for known
;; zones/instants. Rejects Windows (garbage tm_gmtoff) and missing tzdata.
(define tzp-tz-usable?
  (and tzp-tz-symbols?
       (guard (e (#t #f))
         (let ((jan 1768478400))  ; 2026-01-15T12:00:00Z
           (and (eqv? (tzp-offset-raw "America/New_York" jan) -18000)
                (eqv? (tzp-offset-raw "Australia/Sydney" jan) 39600)
                (eqv? (tzp-offset-raw "UTC" jan) 0))))))

(define (tzp-tz-offset zone epoch)
  (and tzp-tz-usable? (tzp-offset-raw zone epoch)))

;; locale-id -> libc locale string.
(define tzp-locale-table
  '(("de" . "de_DE.UTF-8") ("fr" . "fr_FR.UTF-8") ("it" . "it_IT.UTF-8")
    ("ja" . "ja_JP.UTF-8") ("es" . "es_ES.UTF-8") ("ko" . "ko_KR.UTF-8")
    ("zh" . "zh_CN.UTF-8") ("pt" . "pt_BR.UTF-8") ("ru" . "ru_RU.UTF-8")
    ("en" . "en_US.UTF-8") ("und" . "en_US.UTF-8")))
(define (tzp-locale->libc loc)
  (cond ((not (string? loc)) "C")
        ((let ((e (assoc loc tzp-locale-table))) (and e (cdr e))) => values)
        ((>= (string-length loc) 2)
         (let ((prefix (ascii-string-down (substring loc 0 2))))
           (or (let ((e (assoc prefix tzp-locale-table))) (and e (cdr e))) "C")))
        (else "C")))

;; strftime-based locale name (libc only; nil when unavailable so the library uses
;; its own English fallback). fmt is a strftime spec: "%B"/"%b"/"%A"/"%a".
(define (tzp-locale-name locale tm-mon tm-wday fmt)
  (and tzp-locale-available?
       (let ((libc-loc (tzp-locale->libc locale))
             (buf (make-bytevector 128))
             (tm (foreign-alloc 56)))
         (foreign-set! 'integer-32 tm 0 0) (foreign-set! 'integer-32 tm 4 0)
         (foreign-set! 'integer-32 tm 8 0) (foreign-set! 'integer-32 tm 12 1)
         (foreign-set! 'integer-32 tm 16 tm-mon) (foreign-set! 'integer-32 tm 20 70)
         (foreign-set! 'integer-32 tm 24 tm-wday) (foreign-set! 'integer-32 tm 28 0)
         (foreign-set! 'integer-32 tm 32 -1)
         (let ((result (with-mutex tzp-mutex
                         (let ((saved (tzp-setlocale tzp-LC_TIME libc-loc)))
                           (if saved
                               (let ((n (tzp-strftime buf 128 fmt tm)))
                                 (tzp-setlocale tzp-LC_TIME "C")
                                 (and (> n 0) n))
                               #f)))))
           (foreign-free tm)
           (and result
                (let ((bv (make-bytevector result)))
                  (bytevector-copy! buf 0 bv 0 result)
                  (utf8->string bv)))))))

;; Clojure-facing seam. tz-offset returns nil (jolt-nil) when libc is unusable.
(define (tzp->jolt x) (if x x jolt-nil))
(def-var! "jolt.host" "tz-offset-seconds"
  (lambda (zone epoch) (tzp->jolt (tzp-tz-offset (jolt-str-render-one zone) epoch))))
(def-var! "jolt.host" "locale-name"
  (lambda (locale tm-mon tm-wday fmt)
    (tzp->jolt (tzp-locale-name (jolt-str-render-one locale) tm-mon tm-wday (jolt-str-render-one fmt)))))
(def-var! "jolt.host" "tz-backend" (guard (e (#t ':fallback)) (if tzp-tz-usable? ':libc ':fallback)))
