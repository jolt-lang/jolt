;; dynamic-var-defaults.ss — default values for the handful of clojure.core dynamic
;; vars that aren't emitted into the prelude (*clojure-version*, *assert*, …). Plain
;; constant def-var!s; *ns* (a namespace object) needs a value type with
;; get-see-through and map?=false and is tracked separately. The binding-stack
;; machinery (binding / var-set / thread-bound?) lives in dyn-binding.ss. Loaded
;; from rt.ss after the value model + def-var!.

;; *clojure-version* — a map {:major 1 :minor 11 :incremental 0 :qualifier nil}.
(def-var! "clojure.core" "*clojure-version*"
  (jolt-hash-map (keyword #f "major") 1
                 (keyword #f "minor") 11
                 (keyword #f "incremental") 0
                 (keyword #f "qualifier") jolt-nil))

;; *unchecked-math* — jolt does no unchecked-math elision; the var reads false.
(def-var! "clojure.core" "*unchecked-math*" #f)

;; *warn-on-reflection* — jolt has no reflection, so the var reads false; (set!
;; *warn-on-reflection* …) resolves and updates it (a no-op effect).
(def-var! "clojure.core" "*warn-on-reflection*" #f)

;; *assert* — gates `assert`; settable/bindable (malli.assert toggles it). Default
;; true, like the JVM.
(def-var! "clojure.core" "*assert*" #t)

;; *print-readably* — bound by pr-family / with-out-str-style code; default true.
(def-var! "clojure.core" "*print-readably*" #t)

;; *print-meta* — when true, pr prints metadata with a ^ prefix; default false.
(def-var! "clojure.core" "*print-meta*" #f)

;; Portable clojure.core dynamic vars whose DEFAULT already matches jolt's
;; behaviour, so exposing them is sound (resolve/binding work, reads return the
;; right value) — not a silent divergence. The ones that change observable output
;; if set non-default (*print-length* / *print-level* / *default-data-reader-fn*)
;; are deliberately NOT interned until the printer/reader honour them.
;;
;; *read-eval* — gates #=() read-eval. jolt's reader has no #=, so it reads true
;; (no eval-on-read happens regardless); a lib can (binding [*read-eval* false] …).
(def-var! "clojure.core" "*read-eval*" #t)
;; *print-dup* — gates print-dup (a multimethod that exists); default false.
(def-var! "clojure.core" "*print-dup*" #f)
;; *print-namespace-maps* — jolt never prints the #:ns{…} map shorthand, so the
;; var reads false (accurate); settable for code that toggles it.
(def-var! "clojure.core" "*print-namespace-maps*" #f)
;; *flush-on-newline* — jolt flushes line output; default true.
(def-var! "clojure.core" "*flush-on-newline*" #t)
;; *compile-files* — jolt has no separate compile phase that emits .class files.
(def-var! "clojure.core" "*compile-files*" #f)
;; *math-context* — BigDecimal rounding context; nil = unlimited, jolt's default.
(def-var! "clojure.core" "*math-context*" jolt-nil)
;; *command-line-args* — the args after the script/-main; nil outside a -m run.
(def-var! "clojure.core" "*command-line-args*" jolt-nil)
;; *file* — the source file being loaded; "NO_SOURCE_PATH" when none, like the JVM.
(def-var! "clojure.core" "*file*" "NO_SOURCE_PATH")

;; REPL result/exception history. Bound by the REPL after each evaluation; nil
;; outside a REPL, which is what reading them returns here.
(def-var! "clojure.core" "*1" jolt-nil)
(def-var! "clojure.core" "*2" jolt-nil)
(def-var! "clojure.core" "*3" jolt-nil)
(def-var! "clojure.core" "*e" jolt-nil)
