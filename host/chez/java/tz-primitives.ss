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
(define tzp-LC_TIME (if (eq? (sa-os-family) 'macos) 5 2))
(define tzp-mutex (make-mutex))

;; Guard-wrapped FFI via jolt-foreign-proc-safe (deferred symbol lookup so a
;; missing entry doesn't abort boot). nil on failure -> graceful fallback.
;; setlocale's result is read as `string`, not void*: it is a locale NAME, and the
;; two things this file does with it both need it as one. NULL (the failure answer,
;; and what an uninstalled locale returns) then arrives as #f rather than the
;; address 0 — which is TRUTHY in Scheme, so a void* result made every failure read
;; as success. And `string` COPIES out of libc's static buffer, which the next
;; setlocale call is free to overwrite, so a saved name survives its own restore.
(define tzp-setlocale (jolt-foreign-proc-safe "setlocale" '(int string) 'string))
(define tzp-strftime  (jolt-foreign-proc-safe "strftime"  '(u8* size_t string void*) 'size_t))
(define tzp-setenv    (jolt-foreign-proc-safe "setenv"    '(string string int) 'int))
(define tzp-unsetenv  (jolt-foreign-proc-safe "unsetenv"  '(string) 'int))
(define tzp-getenv    (jolt-foreign-proc-safe "getenv"    '(string) 'string))
(define tzp-tzset     (jolt-foreign-proc-safe "tzset"     '() 'void))
(define tzp-localtime (jolt-foreign-proc-safe "localtime" '(void*) 'void*))

;; LC_TIME is process-global, exactly like TZ below, so every probe that writes it
;; puts it back. setlocale(cat, NULL) queries without setting, which is how the
;; previous value is read; a #f query answer means libc would not tell us, and
;; there is nothing safe to restore to, so that case leaves the category alone
;; rather than guessing at "C".
(define (tzp-locale-restore! saved)
  (when saved (tzp-setlocale tzp-LC_TIME saved)))

;; Capability probe: has libc a real en_US locale to name months from?
;;
;; It RESTORES LC_TIME, for the same reason tzp-offset-probe restores TZ. This runs
;; at boot, unconditionally, so leaving it set left every jolt process in
;; en_US.UTF-8 — a C program starts in "C" — and any later strftime, localtime or
;; nl_langinfo, jolt's own or a user's through jolt.ffi, silently answered from a
;; locale nobody selected. An embedder that had chosen its own locale before
;; calling into jolt lost it.
;;
;; dynamic-wind, not straight-line code, so the throw the guard is here to catch
;; cannot leave the category clobbered on its way out.
(define tzp-locale-available?
  (and tzp-setlocale tzp-strftime
       (guard (e (#t #f))
         (let ((saved (tzp-setlocale tzp-LC_TIME #f)))
           (dynamic-wind
             (lambda () #f)
             (lambda () (and (tzp-setlocale tzp-LC_TIME "en_US.UTF-8") #t))
             (lambda () (tzp-locale-restore! saved)))))))

;; FFI symbols present? (setenv/getenv/unsetenv are POSIX-only, so this is #f on
;; Windows). getenv and unsetenv are required here too: tzp-offset-raw needs both
;; to save/restore TZ around its write, and without that guarantee a resolve
;; failure on either would silently reintroduce the TZ leak this file exists to
;; prevent, rather than falling back to the DST-table backend.
(define tzp-tz-symbols? (and tzp-setenv tzp-tzset tzp-localtime tzp-getenv tzp-unsetenv))

;; zone offset in seconds east of UTC at an epoch-second instant, via libc.
;; struct tm layout (64-bit): 9 ints(36) + pad(4) + long tm_gmtoff at byte 40.
;; localtime returns a STATIC buffer — do NOT foreign-free it.
;; TZ is process-global, so it is saved and restored around the write. Without
;; that, this leaves the process in whichever zone it last probed: the capability
;; check below ends on "UTC", so every jolt process was left in UTC and any later
;; localtime() — jolt's own, or a user's through jolt.ffi — answered UTC while
;; the machine was somewhere else. getenv returns #f when TZ was unset, which is
;; the common case and restores with unsetenv rather than an empty TZ (an empty
;; TZ means UTC, not "unset").
;;
;; Chez's `string` return copies the char* into a Scheme string, so `saved`
;; survives the setenv that immediately follows it; a raw pointer into environ
;; would not.
;;
;; dynamic-wind rather than straight-line code so a throw between the two cannot
;; leave TZ clobbered — the same shape jolt-with-mutex itself uses.
(define (tzp-restore-tz! saved)
  (if saved
      (tzp-setenv "TZ" saved 1)
      (tzp-unsetenv "TZ"))
  (tzp-tzset))

(define (tzp-offset-probe zone epoch)   ; caller holds tzp-mutex
  (let ((saved (tzp-getenv "TZ")))
    (dynamic-wind
      (lambda ()
        (tzp-setenv "TZ" zone 1)
        (tzp-tzset))
      (lambda ()
        (let ((tp (sa-foreign-alloc 8)))
          (sa-foreign-set! 'long tp 0 epoch)
          (let ((tm (tzp-localtime tp)))
            (sa-foreign-free tp)
            (and tm (not (eq? tm 0)) (sa-foreign-ref 'long tm 40)))))
      (lambda () (tzp-restore-tz! saved)))))

;; Memo over that probe. The offset of a zone at an instant is a pure function of
;; the two, and the probe is expensive for a reason the restore above introduced:
;; leaving TZ clobbered meant libc had already loaded that zone, so the NEXT
;; call's tzset had nothing to re-read. Restoring TZ — which it must — means every
;; call now performs two real zone loads instead of none. Measured on
;; darwin-arm64, one probe call in a repeated same-zone loop:
;;
;;   leaving TZ clobbered   ~0.9-1.6 us
;;   restoring TZ           ~1.1-2.4 ms      (~1000x, four runs)
;;
;; A tzset that re-reads is what costs: with TZ unchanged it is ~240ns, and even
;; a fixed-offset TZ string with no tzfile to read is ~215us on this platform.
;;
;; This does not make a first lookup cheaper; it stops repeated ones paying again.
;; That covers the common shape: jolt-lang/time's resolve-zone asks every named
;; zone for its offset at epoch 0, so id -> offset resolution hit libc every time
;; a ZoneId was built. A query about a LIVE instant still pays, since its epoch
;; differs on every call — answering those from memory needs the zone's
;; transition table (i.e. reading tzfiles directly), which is a bigger change
;; than this one.
;;
;; Bounded and cleared wholesale on overflow rather than evicted one at a time:
;; live-instant queries would otherwise grow it for the process's lifetime, and
;; an LRU is not worth the code here. Inside the mutex because Chez hashtables
;; are not thread-safe; the probe is called with it already held.
;;
;; A tzdata change mid-process is not picked up for an already-answered
;; (zone, instant). The JVM caches zone rules per process too, and a running
;; process seeing a tzdata upgrade is not a case worth the invalidation
;; machinery.
(define tzp-offset-memo-cap 1024)
(define tzp-offset-memo (make-hashtable equal-hash equal?))

(define (tzp-offset-raw zone epoch)
  (and tzp-tz-symbols?
       (jolt-with-mutex tzp-mutex
         (let* ((key (cons zone epoch))
                (hit (hashtable-ref tzp-offset-memo key #f)))
           ;; `or` is safe: a real offset of 0 (UTC) is not #f in Scheme.
           (or hit
               (let ((off (tzp-offset-probe zone epoch)))
                 (when off
                   (when (> (hashtable-size tzp-offset-memo) tzp-offset-memo-cap)
                     (hashtable-clear! tzp-offset-memo))
                   (hashtable-set! tzp-offset-memo key off))
                 off))))))

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
;;
;; The requested locale has to be REQUESTED, not assumed: setlocale answers NULL
;; for one the OS does not have installed, which is the common case (most images
;; carry only C and en_US). Read as void* that NULL was the address 0 and so
;; truthy, and strftime then ran under whatever locale happened to be in effect
;; and its answer was returned as if it were the requested one's — English on a
;; stock box, or genuinely the wrong language wherever some other locale was
;; current. nil is what the caller wants: jolt.time.fmt's `or` falls through to
;; its own bundled tables, which are right everywhere.
;;
;; LC_TIME is restored to the name that was in effect, not to "C". Restoring to a
;; constant is its own clobber — it just happens to be a tidy-looking one — and
;; this ran on any locale-aware format call, so the previous value did not survive
;; the first one.
(define (tzp-locale-name locale tm-mon tm-wday fmt)
  (and tzp-locale-available?
       (let ((libc-loc (tzp-locale->libc locale))
             (buf (make-bytevector 128))
             (tm (sa-foreign-alloc 56)))
         ;; tm is freed on the way out however the body leaves, throw included.
         (dynamic-wind
           (lambda () #f)
           (lambda ()
             (sa-foreign-set! 'integer-32 tm 0 0) (sa-foreign-set! 'integer-32 tm 4 0)
             (sa-foreign-set! 'integer-32 tm 8 0) (sa-foreign-set! 'integer-32 tm 12 1)
             (sa-foreign-set! 'integer-32 tm 16 tm-mon) (sa-foreign-set! 'integer-32 tm 20 70)
             (sa-foreign-set! 'integer-32 tm 24 tm-wday) (sa-foreign-set! 'integer-32 tm 28 0)
             (sa-foreign-set! 'integer-32 tm 32 -1)
             (let ((result (jolt-with-mutex tzp-mutex
                             (let ((saved (tzp-setlocale tzp-LC_TIME #f)))
                               (dynamic-wind
                                 (lambda () #f)
                                 (lambda ()
                                   (and (tzp-setlocale tzp-LC_TIME libc-loc)
                                        (let ((n (tzp-strftime buf 128 fmt tm)))
                                          (and (> n 0) n))))
                                 (lambda () (tzp-locale-restore! saved)))))))
               (and result
                    (let ((bv (make-bytevector result)))
                      (bytevector-copy! buf 0 bv 0 result)
                      (utf8->string bv)))))
           (lambda () (sa-foreign-free tm))))))

;; Clojure-facing seam. tz-offset returns nil (jolt-nil) when libc is unusable.
(define (tzp->jolt x) (if x x jolt-nil))
(def-var! "jolt.host" "tz-offset-seconds"
  (lambda (zone epoch) (tzp->jolt (tzp-tz-offset (jolt-str-render-one zone) epoch))))
(def-var! "jolt.host" "locale-name"
  (lambda (locale tm-mon tm-wday fmt)
    (tzp->jolt (tzp-locale-name (jolt-str-render-one locale) tm-mon tm-wday (jolt-str-render-one fmt)))))
(def-var! "jolt.host" "tz-backend" (guard (e (#t ':fallback)) (if tzp-tz-usable? ':libc ':fallback)))
