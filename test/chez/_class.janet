# jolt-13zk — bare class-name tokens + (class x) on Chez. A class name (String,
# Keyword, File...) evaluates to its JVM canonical-name STRING — the same value
# (class instance) returns — so (= String (class "x")) holds and a (defmethod m
# String ...) keys against a (class ...) dispatch. host-class.ss ports
# src/jolt/eval_resolve.janet's class-canonical-names + core-class (scalar arms).
# Oracle = build/jolt. Collection (class ...) is host-taxonomy-dependent (the seed
# leaks the Janet host type "table"/"struct") and is NOT compared here.
#
#   janet test/chez/_class.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/jolt-chez"))

(def cases
  [# --- bare class tokens evaluate to canonical strings ---
   ["String"            "java.lang.String"]
   ["Number"            "java.lang.Number"]
   ["Keyword"           "clojure.lang.Keyword"]
   ["File"              "java.io.File"]
   ["Exception"         "java.lang.Exception"]
   ["MapEntry"          "clojure.lang.MapEntry"]
   # --- (class x) on scalars matches core-class ---
   ["(class 1)"         "java.lang.Number"]
   ["(class 1.5)"       "java.lang.Number"]
   ["(class \"s\")"     "java.lang.String"]
   ["(class :k)"        "clojure.lang.Keyword"]
   ["(class true)"      "java.lang.Boolean"]
   ["(class false)"     "java.lang.Boolean"]
   ["(class nil)"       ""]
   # --- token <-> class equality ---
   ["(= String (class \"abc\"))"               "true"]
   ["(= Number (class 7))"                      "true"]
   ["(= String (class 7))"                      "false"]
   # --- defmulti dispatch on class ---
   ["(do (defmulti cm (fn [x] (class x))) (defmethod cm String [x] :str) (cm \"a\"))" ":str"]
   ["(do (defmulti cn (fn [x] (class x))) (defmethod cn nil [x] :nil) (defmethod cn String [x] :str) (cn nil))" ":nil"]
   ["(do (defmulti cn (fn [x] (class x))) (defmethod cn nil [x] :nil) (defmethod cn String [x] :str) (cn \"z\"))" ":str"]])

(defn run-capture [bin expr]
  (def proc (os/spawn [bin "-e" expr] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (def err (ev/read (proc :err) 0x100000))
  (def code (os/proc-wait proc))
  (def lines (filter (fn [l] (not (empty? l)))
                     (string/split "\n" (string/trim (if out (string out) "")))))
  [code (if (empty? lines) "" (last lines)) (string/trim (if err (string err) ""))])

(var pass 0)
(def fails @[])
(each [expr expected] cases
  (def [ocode oracle _] (run-capture "build/jolt" expr))
  (def [code got err] (run-capture jolt-bin expr))
  (cond
    (not= ocode 0) (array/push fails [expr (string "ORACLE FAILED exit " ocode)])
    (not= oracle expected) (array/push fails [expr (string "ORACLE MISMATCH want `" expected "` got `" oracle "`")])
    (not= code 0) (array/push fails [expr (string "exit " code "; err: " err)])
    (= got expected) (++ pass)
    (array/push fails [expr (string "want `" expected "`, got `" got "`")])))

(printf "\n_class parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
