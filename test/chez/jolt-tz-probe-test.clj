;; jolt.host tz gate — the libc timezone probe must not leave TZ behind.
;;
;; tz-offset-seconds sets TZ, calls tzset and reads tm_gmtoff. TZ is process
;; global, so without a restore the process is left in whichever zone was probed
;; last — and the capability check at boot ends on "UTC". Every jolt process then
;; answered UTC from localtime(), including user code calling it through jolt.ffi,
;; while the machine was somewhere else entirely.
;;
;; Run: bin/jolt run test/chez/jolt-tz-probe-test.clj (smoke.sh greps for
;; "JOLT-TZ-PROBE-TEST OK").
(ns jolt-tz-probe-test)

(require '[jolt.ffi :as ffi])

(def failures (atom []))

(defmacro check-eq [label got want]
  `(do
     (print (str "  .. " ~label "\n"))
     (flush)
     (let [g# ~got w# ~want]
       (when-not (= g# w#)
         (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#)))))))

(ffi/defcfn c-getenv "getenv" [:string] :string)
(ffi/defcfn c-setenv "setenv" [:string :string :int] :int)
(ffi/defcfn c-unsetenv "unsetenv" [:string] :int)
(ffi/defcfn c-time "time" [:pointer] :long)
(ffi/defcfn c-localtime "localtime" [:pointer] :pointer)
(ffi/defcfn c-gmtime "gmtime" [:pointer] :pointer)

(defn- hour-of [f epoch-buf]
  (ffi/read (f epoch-buf) :int 8))     ; tm_hour is the third int

;; The boot-time capability probe has already run by the time this loads, so an
;; unrestored TZ would be observable right here — but only in a process that did
;; not INHERIT one. A TZ in the environment is an ordinary setup (a container's
;; ENV TZ=UTC, a shell that exports it), and from in here it is indistinguishable
;; from a leak, so asserting nil unconditionally failed the gate on the
;; environment rather than on the code. smoke.sh runs this with TZ scrubbed so
;; the assertion below is what the gate actually reports; run by hand with TZ
;; set, it says so and moves on to the checks that carry their own TZ.
(let [inherited (c-getenv "TZ")]
  (if (nil? inherited)
    (check-eq "TZ is not left set by the boot probe" inherited nil)
    (println (str "  .. (skipped: TZ=" (pr-str inherited)
                  " was inherited, so a leak is not observable here)"))))

;; A named zone under a KNOWN TZ must come back unchanged. This is the case the
;; boot probe broke: it left TZ=UTC, so a caller who had set TZ themselves lost it.
(c-setenv "TZ" "America/New_York" 1)
(check-eq "a zone query answers correctly"
          (jolt.host/tz-offset-seconds "Australia/Sydney" 1768478400) 39600)
(check-eq "TZ survives a zone query" (c-getenv "TZ") "America/New_York")
(c-unsetenv "TZ")

;; ...and an UNSET TZ must stay unset rather than become an empty string, which
;; libc reads as UTC rather than as "use the system zone".
(check-eq "a zone query still answers with TZ unset"
          (jolt.host/tz-offset-seconds "America/New_York" 1768478400) -18000)
(check-eq "TZ is still unset afterwards" (c-getenv "TZ") nil)

;; The end-to-end symptom: with the system zone in effect, localtime and gmtime
;; agree only if the machine really is on UTC. On a UTC machine this check cannot
;; discriminate, so it is skipped there rather than asserted as a pass.
(let [b (ffi/alloc 8)]
  (try
    (c-time b)                          ; one shared epoch for both calls below
    (let [local  (hour-of c-localtime b)
          utc    (hour-of c-gmtime b)
          sys-tz (c-getenv "TZ")]
      (if (= local utc)
        (println (str "  .. (skipped: this machine's local time IS UTC, TZ=" (pr-str sys-tz) ")"))
        (check-eq "localtime is not silently gmtime" (= local utc) false)))
    (finally (ffi/free b))))

;; The probe is memoized per (zone, epoch), so the gate has to prove the memo
;; distinguishes both parts of that key rather than answering the first offset it
;; ever computed. A same-zone DST pair is the sharp case: identical zone string,
;; different instants, genuinely different offsets.
(check-eq "memo: same zone, southern summer" (jolt.host/tz-offset-seconds "Australia/Sydney" 1768478400) 39600)
(check-eq "memo: same zone, southern winter" (jolt.host/tz-offset-seconds "Australia/Sydney" 1784203200) 36000)
(check-eq "memo: re-asking summer still answers summer"
          (jolt.host/tz-offset-seconds "Australia/Sydney" 1768478400) 39600)
(check-eq "memo: same instant, different zone" (jolt.host/tz-offset-seconds "Europe/Paris" 1768478400) 3600)

;; A cached 0 must read as a hit, not as "nothing cached": UTC's offset IS 0, and
;; only #f is false in Scheme, so a memo written with `or` has to keep answering
;; from the table rather than re-probing (or, worse, returning nil).
(check-eq "memo: a zero offset caches" (jolt.host/tz-offset-seconds "UTC" 1768478400) 0)
(check-eq "memo: a zero offset stays cached" (jolt.host/tz-offset-seconds "UTC" 1768478400) 0)

;; Memo hits must not skip the TZ restore, which they cannot — they never touch
;; TZ at all — but the whole point of the file is that this stays true.
(check-eq "TZ is still unset after the memo checks" (c-getenv "TZ") nil)

(if (empty? @failures)
  (println "JOLT-TZ-PROBE-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "JOLT-TZ-PROBE-TEST FAILED:" (count @failures))))
