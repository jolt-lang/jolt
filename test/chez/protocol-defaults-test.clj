;; protocol-defaults-test.clj — default method bodies in defprotocol (issue #740).
;;
;; ask.clojure.org 1538 asked for this in 2014 and it was never accepted; the
;; community answer is "extend the protocol to Object", which works but is
;; GLOBAL — an Object extension puts the default on every value in the system.
;; Java 8 shipped interface default methods for exactly this evolution problem.
;;
;; The syntax is fn's: a bare vector is a signature, a LIST starting with a
;; vector is a signature plus a default body. Unambiguous against the docstring
;; a clause may already carry, and per-arity for free.
;;
;;   (defprotocol P
;;     (a [this])                       ; no default, exactly as today
;;     (b ([this] :fallback))           ; defaulted
;;     (c [this] ([this x] :two)))      ; one arity defaulted, one not
;;
;; SCOPE IS THE WHOLE POINT. A default reaches a type that EXTENDS the protocol
;; and omits that method — Java 8's semantics. It does NOT reach a value that
;; never extended the protocol, because that is the Object-wide extension this
;; feature exists to avoid. satisfies? is unchanged. The rows below pin both
;; halves; the second is the one that keeps this from being a leak in disguise.

(ns protocol-defaults-test)

(def failures (atom []))
(defn check [label pred]
  (when-not pred (swap! failures conj label)))
(defn raises? [f] (try (f) false (catch Throwable _ true)))

;; --- the shape -----------------------------------------------------------------

(defprotocol Greeter
  (greet [this])
  (greet-loudly ([this] (str (greet this) "!")))
  (greet-many
    [this n]
    ([this n sep] (clojure.string/join sep (repeat n (greet this)))))
  (farewell ([this] "bye") "say goodbye"))

(defrecord Polite [name]
  Greeter
  (greet [this] (str "hello " name)))

(defrecord Shouty [name]
  Greeter
  (greet [this] (str "HELLO " name))
  (greet-loudly [this] (str (greet this) "!!!")))

(check "an omitted method falls to the protocol default"
       (= "hello ann!" (greet-loudly (->Polite "ann"))))

(check "an inline implementation still wins over the default"
       (= "HELLO bo!!!" (greet-loudly (->Shouty "bo"))))

(check "the method with no default at all still works"
       (= "hello ann" (greet (->Polite "ann"))))

(check "a default with no reference to other methods"
       (= "bye" (farewell (->Polite "ann"))))

(check "a docstring alongside a default is still just a docstring"
       (= "bye" (farewell (->Shouty "bo"))))

;; --- per-arity -----------------------------------------------------------------

(check "the defaulted arity resolves"
       (= "hello ann-hello ann" (greet-many (->Polite "ann") 2 "-")))

(check "an arity with NO default still raises"
       (raises? #(greet-many (->Polite "ann") 2)))

;; --- scope: extenders only ------------------------------------------------------
;; The half that matters. A default must not become an Object extension.

(defrecord Unrelated [x])

(check "a type that never extended the protocol does not get the default"
       (raises? #(greet-loudly (->Unrelated 1))))

(check "a plain value does not get the default"
       (raises? #(greet-loudly 42)))

(check "nil does not get the default"
       (raises? #(greet-loudly nil)))

(check "a string does not get the default"
       (raises? #(greet-loudly "not a greeter")))

(check "satisfies? is unchanged for a non-extender"
       (false? (satisfies? Greeter (->Unrelated 1))))

(check "satisfies? is still true for an extender that relies on the default"
       (true? (satisfies? Greeter (->Polite "ann"))))

;; --- extend-type and extend reach defaults too ----------------------------------

(defrecord ViaExtend [name])
(extend-type ViaExtend
  Greeter
  (greet [this] (str "hi " (:name this))))

(check "extend-type gets the default for an omitted method"
       (= "hi cy!" (greet-loudly (->ViaExtend "cy"))))

(deftype ViaDeftype [name]
  Greeter
  (greet [this] (str "yo " name)))

(check "deftype gets the default for an omitted method"
       (= "yo di!" (greet-loudly (->ViaDeftype "di"))))

(check "reify gets the default for an omitted method"
       (= "sup ed!" (greet-loudly (reify Greeter (greet [this] "sup ed")))))

;; extending a host type keeps the default scoped to that type, not to Object
(extend-type String
  Greeter
  (greet [this] (str "str:" this)))

(check "a host type that extends gets the default"
       (= "str:x!" (greet-loudly "x")))

(check "extending String did not leak the default onto other values"
       (raises? #(greet-loudly 42)))

;; --- a default calling another method of its own protocol -----------------------
;; greet-loudly's default calls greet, which dispatches normally — so the default
;; sees the RECEIVER's implementation, not the protocol's.

(check "a default dispatches back through the protocol"
       (= ["hello ann!" "hi cy!"]
          [(greet-loudly (->Polite "ann")) (greet-loudly (->ViaExtend "cy"))]))

;; --- redefinition ----------------------------------------------------------------

(defprotocol Redef
  (rm ([this] :first)))
(defrecord RedefRec [])
(extend-type RedefRec Redef)
(check "default before redefinition" (= :first (rm (->RedefRec))))

(defprotocol Redef
  (rm ([this] :second)))
(extend-type RedefRec Redef)
(check "a redefined protocol installs the new default"
       (= :second (rm (->RedefRec))))

;; --- protocols with no defaults are untouched -------------------------------------

(defprotocol Plain
  (only [this])
  (missing [this]))
(defrecord PlainRec []
  Plain
  (only [this] :ok))

(check "a protocol with no defaults still answers its implemented method"
       (= :ok (only (->PlainRec))))
(check "a protocol with no defaults still raises on an omitted method"
       (raises? #(missing (->PlainRec))))

(if (empty? @failures)
  (do (println "PROTOCOL-DEFAULTS-TEST OK") (flush) (System/exit 0))
  (do (doseq [failure @failures] (println "FAIL:" failure))
      (flush)
      (System/exit 1)))
