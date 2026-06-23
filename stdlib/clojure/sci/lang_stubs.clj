; Minimal SCI type stubs for Jolt bootstrap
; Provides the deftype definitions that SCI's Clojure source references
; before the full SCI runtime is loaded.

; Protocol stubs for SCI's type system
(defprotocol IBox (setVal [this v]) (getVal [this]))
(defprotocol HasName (getName [this]))
(defprotocol IVar (bindRoot [this v]) (getRawRoot [this]) (toSymbol [this])
  (isMacro [this]) (setThreadBound [this v]) (unbind [this]) (hasRoot [this]))
(defprotocol DynVar (dynamic? [this]))
(defprotocol IReified (getInterfaces [this]) (getMethods [this])
  (getProtocols [this]) (getFields [this]))

; Unbound sentinel for vars
(deftype SciUnbound [the-var])

; Core SCI types — keep minimal for bootstrap
(deftype Namespace [name mappings aliases imports])
(deftype Var [root name meta macro dynamic ns])

; Store stub (sci.ctx-store provides a global context atom)
(def ctx-store (atom nil))

; Macro helpers from sci.impl.macros
(defn deftime [& body] nil)
(defn usetime [& body] (eval (first body)))
(defmacro ? [& args]
  (if (contains? &env (quote &env))
    (let [form (first args)]
      (if (= :clj (first form))
        (second form)
        (if (= :cljs (first form)) nil)))))
