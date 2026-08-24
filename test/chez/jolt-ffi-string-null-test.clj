;; jolt.ffi :string <-> NULL gate. Chez's `string` foreign type carries NULL in
;; both directions as #f; jolt's nil is a separate sentinel, so without the
;; backend's wrapping the boundary leaked Scheme — passing nil raised "invalid
;; foreign-procedure argument #[jolt-nil-v1]", and a NULL return arrived as false.
;; Run: bin/jolt run test/chez/jolt-ffi-string-null-test.clj (smoke.sh greps for
;; "JOLT-FFI-STRING-NULL-TEST OK").
(ns jolt-ffi-string-null-test)

(require '[jolt.ffi :as ffi])
(require '[clojure.string :as str])

(def failures (atom []))

(defmacro check-eq [label got want]
  `(do
     (print (str "  .. " ~label "\n"))
     (flush)
     (let [g# ~got w# ~want]
       (when-not (= g# w#)
         (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#)))))))

;; setlocale(int, const char *): NULL QUERIES the current locale instead of
;; setting it, so it exercises "NULL is a meaningful argument" without needing a
;; helper library. LC_ALL is 0 on both platforms jolt targets.
(ffi/defcfn c-setlocale "setlocale" [:int :string] :string)

;; getenv returns NULL for a name that is not set — the return direction.
(ffi/defcfn c-getenv "getenv" [:string] :string)

;; --- argument direction ------------------------------------------------------
(let [queried (c-setlocale 0 nil)]
  (check-eq "nil argument reaches C as NULL (setlocale queries)"
            (string? queried) true)
  (check-eq "the queried locale is non-empty" (pos? (count queried)) true))

;; A real string still marshals; nil support must not disturb the common path.
(check-eq "a string argument still marshals" (string? (c-setlocale 0 "C")) true)

;; --- return direction --------------------------------------------------------
(check-eq "a NULL return arrives as nil, not false"
          (c-getenv "JOLT_FFI_STRING_NULL_TEST_DEFINITELY_UNSET")
          nil)
(check-eq "a NULL return is not false"
          (false? (c-getenv "JOLT_FFI_STRING_NULL_TEST_DEFINITELY_UNSET"))
          false)
(check-eq "a non-NULL return is still a string" (string? (c-getenv "PATH")) true)

;; --- nil is only special for :string -----------------------------------------
;; :pointer keeps taking ffi/null as the integer 0; nothing here changes that.
(check-eq "ffi/null is still 0" (ffi/null? ffi/null) true)

;; --- and nil is the ONLY spelling of NULL ------------------------------------
;; `string` is the one foreign type Chez takes a non-string on: it rejects #t, an
;; integer, a symbol and a bytevector there, and rejects #f in every other
;; position, but accepts #f in a `string` position as NULL. So false was the
;; single value that could cross this boundary silently, and it landed on exactly
;; the value C reads as "absent" — a `when` that did not fire, a predicate
;; result. There is no :bool foreign type for it to have legitimately come from.
;; Asserted on the MESSAGE, not merely on "something threw": the other non-strings
;; did already throw before this, but as a raw Chez condition — class :object,
;; reading "invalid foreign-procedure argument #[keyword-v1 #f \"kw\" 1158308175]"
;; — so a bare threw? check would pass either way and prove nothing about them.
(defn- threw-message [f]
  (try (f) nil (catch Throwable t (str (.getMessage t)))))

(defn- rejected? [f]
  (let [m (threw-message f)]
    (and (some? m) (str/includes? m "jolt.ffi: :string got"))))

;; "<what> must be a string, got nil" — the naming positions further down.
(defn- rejected-name? [f]
  (let [m (threw-message f)]
    (and (some? m) (str/includes? m "must be a string, got nil"))))

(check-eq "false as a :string argument is rejected, not sent as NULL"
          (rejected? #(c-setlocale 0 false)) true)
(check-eq "false's rejection is an IllegalArgumentException"
          (try (c-setlocale 0 false) false
               (catch IllegalArgumentException _ true))
          true)
(check-eq "true as a :string argument is rejected in jolt's own words"
          (rejected? #(c-setlocale 0 true)) true)
(check-eq "a non-string :string argument is rejected in jolt's own words"
          (rejected? #(c-setlocale 0 42)) true)

;; The rejection must not have cost the two values that ARE legal, which is the
;; property a validating conversion is most likely to break.
(check-eq "nil still reaches C as NULL after the check" (string? (c-setlocale 0 nil)) true)
(check-eq "a string still marshals after the check" (string? (c-setlocale 0 "C")) true)

;; --- where NULL is NOT available, nil is an error ----------------------------
;; The positions above can say "absent" — NULL is exactly that. The ones below
;; cannot: they use the string to NAME something. nil reached the `str` coercion
;; there, which renders it "", so an absent name became an empty one and the
;; boundary acted on it — a foreign type called "", a dlopen of "".
(check-eq "a nil foreign type is rejected, not read as the type named \"\""
          (rejected-name? #(ffi/sizeof nil)) true)
(check-eq "a real foreign type still resolves" (pos? (ffi/sizeof :int32)) true)
(check-eq "a nil library name is rejected, not dlopen'd as \"\""
          (rejected-name? #(ffi/loaded? nil)) true)
;; load-library keeps its documented nil: no name means "the process's own
;; symbols are already resolvable", which is an answer, not an absent name.
(check-eq "(load-library nil) is still the documented no-op"
          (nil? (ffi/load-library nil)) true)

(if (empty? @failures)
  (println "JOLT-FFI-STRING-NULL-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "JOLT-FFI-STRING-NULL-TEST FAILED:" (count @failures))))
