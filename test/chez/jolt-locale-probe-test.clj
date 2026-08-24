;; jolt.host's libc locale probe — the sibling of jolt-tz-probe-test.clj.
;;
;; LC_TIME is process global exactly like TZ, and tz-primitives.ss writes it in
;; two places: a boot capability probe asking whether libc has a real en_US to
;; name months from, and locale-name itself around each strftime. Neither put it
;; back, so every jolt process ran in en_US.UTF-8 (a C program starts in "C") and
;; the first locale-name call moved it to "C" for good — visible to jolt's own
;; strftime/localtime and to user code calling either through jolt.ffi, and
;; destroying an embedder's own setlocale choice.
;;
;; The other half is that setlocale answers NULL for a locale the OS does not
;; have, which read as the address 0 — truthy in Scheme — so a failed request was
;; taken for a success and strftime answered from whatever locale was current.
;;
;; Run: bin/jolt run test/chez/jolt-locale-probe-test.clj (smoke.sh greps for
;; "JOLT-LOCALE-PROBE-TEST OK").
(ns jolt-locale-probe-test)

(require '[jolt.ffi :as ffi])

(def failures (atom []))

(defmacro check-eq [label got want]
  `(do
     (print (str "  .. " ~label "\n"))
     (flush)
     (let [g# ~got w# ~want]
       (when-not (= g# w#)
         (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#)))))))

;; LC_TIME is 2 on glibc, 5 on macOS — the same split tz-primitives.ss keys on.
(def LC_TIME (if (= "Mac OS X" (System/getProperty "os.name")) 5 2))

;; NULL queries the current locale instead of setting it, so this reads LC_TIME
;; without disturbing it (jolt.ffi carries nil across :string as of 0.7.24).
(ffi/defcfn c-setlocale "setlocale" [:int :string] :string)

;; A C process starts every category in "C" and jolt must hand it back that way.
(check-eq "the boot probe leaves LC_TIME at the process default"
          (c-setlocale LC_TIME nil) "C")

;; A caller's own choice must survive a locale-aware format call — and the choice
;; has to be something other than "C" for the check to have teeth: restoring to a
;; hardcoded "C", which is what the code did, is indistinguishable from a real
;; restore when "C" is what was there anyway. locale-name is inert unless the boot
;; probe found en_US.UTF-8, so wherever it answers at all, that locale is installed
;; and serves as the non-"C" witness.
(if (some? (jolt.host/locale-name "en" 0 1 "%B"))
  (do (c-setlocale LC_TIME "en_US.UTF-8")
      (jolt.host/locale-name "en" 0 1 "%B")
      (check-eq "a caller's own LC_TIME survives a locale-name call"
                (c-setlocale LC_TIME nil) "en_US.UTF-8"))
  (println "  .. (skipped: libc has no en_US.UTF-8, so locale-name is inert here)"))
(c-setlocale LC_TIME "C")

;; An UNINSTALLED locale must read as nil, not as a name borrowed from whichever
;; locale was current. Every box has "C"; essentially none has all of these, and
;; a name coming back for one that setlocale refused is the bug. Where a locale
;; IS installed a real name is correct, so each check accepts nil or a name and
;; the gate is that the two agree — a name only when setlocale took the locale.
(doseq [[id libc] [["fr" "fr_FR.UTF-8"] ["ja" "ja_JP.UTF-8"]
                   ["de" "de_DE.UTF-8"] ["ru" "ru_RU.UTF-8"]]]
  (let [installed? (some? (do (c-setlocale LC_TIME "C")
                             (c-setlocale LC_TIME libc)))]
    (c-setlocale LC_TIME "C")
    (check-eq (str "locale-name \"" id "\" answers only if " libc " is installed")
              (some? (jolt.host/locale-name id 0 1 "%B"))
              installed?)))

;; ...and none of those calls may have left the category somewhere of their own.
(check-eq "LC_TIME is where the test left it after the uninstalled-locale checks"
          (c-setlocale LC_TIME nil) "C")

(if (empty? @failures)
  (println "JOLT-LOCALE-PROBE-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "JOLT-LOCALE-PROBE-TEST FAILED:" (count @failures))))
