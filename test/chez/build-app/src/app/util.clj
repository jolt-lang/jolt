(ns app.util
  (:require [clojure.string :as str]))

(defn shout [s]
  (str/upper-case (str s "!")))

;; A hintless double fn: with wp-infer now the release default, its r param is
;; seeded :double from the (area 2.0) call site in app.core, so the built binary
;; lowers * to fl*. build-smoke greps flat.ss for the fl-op (proves wp-infer ran).
(defn area [r] (* r r))

;; Unhinted string interop: the target (str x) types :str per-form (the str-ret
;; table), so the built binary lowers .startsWith/.indexOf to the string natives
;; inline — build-smoke greps flat.ss for str-starts-with? and the ABSENCE of a
;; record-method-dispatch "startsWith" (proves the :str stamp emitted directly).
(defn strd-prefix [x] (.startsWith (str x) "s"))
(defn strd-find [x] (.indexOf (str x) (int 45)))

;; The per-ns ret table: clojure.string/replace ALWAYS returns a string (so the
;; target types :str and lowers directly), while clojure.core/replace is
;; polymorphic and must NOT stamp. This call site is the replace row — if the
;; table lookup regressed to generic dispatch, build-smoke's negative grep for
;; record-method-dispatch "startsWith" fails on this fn.
(defn strd-rep [x] (.startsWith (clojure.string/replace x "a" "b") "b"))

;; Never called: only its PRESENCE matters. Referencing a jolt-lang/time class
;; must make the build scan pull the provider's install ns (src-provider/jolt/
;; time.clj, a stand-in on the roots) into flat.ss — a built binary has no
;; source roots, so the runtime class-miss autoload can't fire there.
(defn zdt-class [] java.time.ZonedDateTime)

;; Proven-keyword interop: honeysql's kw->sym is (.sym ^clojure.lang.Keyword k)
;; on its :clj branch. The hint proves the target a keyword, so the built binary
;; must lower .sym to the inline (jolt-symbol (keyword-t-ns t) (keyword-t-name t))
;; — no record-method-dispatch walk, no jolt-vector rest-args. build-smoke greps
;; flat.ss for that emission shape and the ABSENCE of a keyword .sym dispatch.
(defn kwsym [^clojure.lang.Keyword k] (.sym k))

;; Proven-StringBuilder interop: the shape honey.sql.util/join has, and every
;; string-building loop in Clojure — a let-bound (StringBuilder.) with NO hint in
;; the source, appended to in a reduce. init-proves-hint supplies the :sb stamp, so
;; the built binary must lower .append/.toString to the inline sb-append!/sb-str
;; instead of the jhost method-table walk. build-smoke greps flat.ss for that shape
;; and for the ABSENCE of a record-method-dispatch "append".
(defn sbjoin [sep parts]
  (let [sb (StringBuilder.)]
    (reduce (fn [first? p]
              (when-not first? (.append sb sep))
              (.append sb p)
              false)
            true parts)
    (.toString sb)))

;; ^:redef / ^:dynamic opt out of direct-linking even with it on by default (the
;; release default now), so the built binary can still redef/bind them at runtime.
(def ^:redef redef-fn (fn [] :original))
(def ^:dynamic *config* :default)

;; jolt#1009: a PLAIN def — no ^:redef, no ^:dynamic — is direct-linked, and a
;; direct-linked def is also LINKED: a root write reaches the jv$ binding that
;; compiled value-position reads go to. Before that the binding froze at load,
;; so in a built binary `(var-get #'root-val)` answered the new root while a
;; compiled `root-val` kept answering the old one, forever. `nil` is the exact
;; shape the issue was filed against (a plain `(def x nil)` holding no value yet
;; — a config slot, a test fixture, an injected dependency).
(def root-val nil)
(defn root-fn [] :original)

;; A two-deep non-tail call chain that throws — exercises native stack traces in a
;; direct-link build (build-smoke runs -main with a --boom sentinel arg). deep-boom
;; is defined through a USER macro: its source registration only gets a real line
;; if the reader position survives macroexpansion (so the trace frame maps).
(defmacro defguarded [name args & body]
  `(defn ~name ~args (assert (number? ~(first args)) "needs a number") ~@body))

(defguarded deep-boom [x]
  (* x 2))

(defn mid-boom [x]
  (inc (deep-boom x)))

(defmacro twice [x]
  `(do ~x ~x))

;; A multimethod with a :default method. The AOT build must set the per-ns
;; current ns before these forms run, or the defmethod registers app.util/greet
;; under the wrong ns and a dispatch to :default crashes (not a fn nil). app.core
;; adds an aliased method (util/greet :loud) — see there.
(defmulti greet (fn [kind] kind))
(defmethod greet :default [_] "greet:default")

;; A var defined TWICE, with a caller compiled between the two defs. Plain
;; Clojure, legal, and last-def-wins in every other jolt mode — but a stashed
;; inline body froze the FIRST definition into dd-caller while dd-late, compiled
;; after the second def, saw the second. One binary, two answers (jolt-rtjm).
;; Both call paths must report "second".
(defn dd-target [] "first")
(defn dd-caller [] (dd-target))
(defn dd-target [] "second")
(defn dd-late [] (dd-target))

;; A spliceable callee holding a NAMED inner fn. The splicer alpha-renames the
;; inner fn's own name for hygiene, so its emitted procedure — and its backtrace
;; frame — is `step-boom__ilN`. Two things this pins (jolt-pzos): the mangled
;; suffix must not reach the user, and inner-boom must appear ONCE, not twice —
;; the inner fn has its own runtime frame, so stamping the inline chain through
;; the fn boundary made the reporter expand that frame as spliced code too.
(defn inner-boom [n]
  (let [step (fn step-boom [y] (throw (ex-info "inner boom" {:y y})))]
    (step n)))

;; A spliceable callee that RETURNS a closure. An anonymous fn travels in a state
;; image as its source form plus its captured values, and the splicer used to drop
;; that registration — so `jolt run` wrote this closure and the built binary
;; refused it (jolt-giqc). The two captures are the two shapes: closure-base is
;; private to THIS namespace, so the registration has to record app.util as the ns
;; the form was written in, and n arrives as a folded constant at one call site
;; and as a renamed live local at the other.
(def ^:private closure-base 100)

(defn make-closure [n]
  (fn [y] (+ closure-base n y)))

;; A multimethod and a lazy sequence, for the built-binary image case in
;; app.core. Both are things a real program holds in its state, and both used to
;; refuse in an image: a multimethod had its dispatch tables walked, and a lazy
;; seq clojure.core built had no recorded source for its thunk.
(defmulti image-mm (fn [x] x))
(defmethod image-mm :a [_] :got-a)
(defmethod image-mm :default [_] :dflt)

;; A bare deftype that DECLARES clojure.lang.ILookup. On the JVM such a type has
;; no key lookup but the one it declares, so its valAt answers for a field-named
;; key too — that is the whole point when the slot holds something valAt is there
;; to transform (typed.clojure keeps a lazy thunk in one). The runtime honours
;; that; the BUILD used to fold past it, because every deftype is registered as a
;; record shape and nothing told the passes this one has a lookup of its own
;; (jolt-fpp3.1). Three doors, each a different pass:
;;   (:a (->Lk 1))        — scalar replacement's (:k <ctor>) fold
;;   (lk-read (->Lk n))   — whole-program inference proving the param a struct,
;;                          which drops the guard and reads the slot
;;   (:a @lk-box)         — opaque, the control: it was right all along
(deftype Lk [a]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [_ k d] (if (= k :a) :from-valat d)))

;; Forward-reference resolution (issue #451): fwd-get is compiled BEFORE this
;; namespace redefines `get`, so its bare `get` must resolve to
;; clojure.core/get — in `jolt run` (in-order load) AND in a built binary
;; (whose emit walk re-analyzes the source against the fully-loaded process,
;; where app.util/get already exists). If the binary's second pass resolved
;; the ns-local redefinition instead, (fwd-get env "K") would call
;; (get env "K") = (assoc "K" :url env :method :get) — a ClassCastException
;; (String cannot be cast to Associative) in the built binary only.
;; The same shape through `first` (redefined further down) pins that the
;; redef-visible window is "at-or-after the def", not file-wide.
(defn fwd-get [env k] (get env k))

;; compiled BEFORE the redefinition below: bare `first` is clojure.core/first.
(defn fwd-first [coll] (first coll))

;; The ns-local redefinitions, http-client / ring style: a helper library that
;; shadows a core fn with a request-shaped one.
(defn get [url opts] (assoc opts :url url :method :get))
(defn first [req] (assoc req :seen-first true))

;; a caller AFTER both redefs: the ns-local fns must win here in every mode
;; (arguments in the redef's [url opts] order).
(defn fwd-late [env k] [(get k env) (first {:req k})])

(defn lk-read [t] (:a t))
(def lk-box (atom nil))
