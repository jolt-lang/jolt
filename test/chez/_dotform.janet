# jolt-kuic — the `.` special form + `.-field` field-access desugar on Chez. The
# analyzer lowers (. target member arg*) and (.-field target) to a :host-call;
# the Chez emit routes a non-shimmed :host-call through record-method-dispatch,
# which dot-forms.ss extends with field access + map/vector member dispatch.
# Expectations are the build/jolt (seed) oracle, captured per case.
#
#   janet test/chez/_dotform.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/jolt-chez"))

(def exprs
  [# strings: method calls via the String surface
   "(. \"HI\" toLowerCase)"
   "(. \"abc\" length)"
   "(. \"abc\" toUpperCase)"
   # vectors / maps: collection interop (count/nth/get/containsKey)
   "(. [1 2 3] count)"
   "(. [10 20 30] nth 1)"
   "(. {:a 1 :b 2} count)"
   "(. {:a 1 :b 2} get :b)"
   "(. {:a 1} containsKey :a)"
   "(. {:count 99} count)"
   # map member: stored fn called with self (+ args), else field value
   "(. {:value 41 :describe (fn [self] (str \"v=\" (:value self)))} describe)"
   "(. {:greet (fn [self n] (str \"Hello \" n))} greet \"Alice\")"
   "(. {:value 41} value)"
   # field access via .-field head
   "(.-value {:value 41})"
   "(.-x {:x 7 :y 9})"
   # field access via (. obj -field)
   "(. {:value 41} -value)"
   # records: .-field reads a field, .method dispatches the protocol method
   "(do (defrecord Rf [x]) (.-x (->Rf 7)))"
   "(do (defrecord Rg [a b]) (.-b (->Rg 1 2)))"
   "(do (defprotocol Greet (hi [_])) (defrecord Rh [nm] Greet (hi [_] (str \"hi \" nm))) (. (->Rh \"x\") hi))"
   # universal object-methods on a non-record map win over a field lookup
   "(try (throw (ex-info \"bad\" {})) (catch Throwable e (.getMessage e)))"
   "(try (throw (ex-info \"bad\" {:k 1})) (catch Throwable e (.getMessage e)))"
   "(try (throw \"boom\") (catch Throwable e (.getMessage e)))"
   "(try (throw (Exception. \"boom\")) (catch Throwable e (.getMessage e)))"
   "(try (throw (IllegalArgumentException. \"bad\")) (catch Exception e (.getMessage e)))"
   "(.equals \"a\" \"a\")"
   "(.equals \"a\" \"b\")"
   "(. {:value 41 :describe (fn [self] (str \"v=\" (:value self)))} describe)"])

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
(each expr exprs
  (def [ocode oracle _] (run-capture "build/jolt" expr))
  (def [code got err] (run-capture jolt-bin expr))
  (cond
    (not= ocode 0) (array/push fails [expr (string "ORACLE FAILED exit " ocode)])
    (not= code 0) (array/push fails [expr (string "exit " code "; err: " err)])
    (= got oracle) (++ pass)
    (array/push fails [expr (string "want `" oracle "`, got `" got "`")])))

(printf "\n_dotform parity [%s]: %d/%d passed" jolt-bin pass (length exprs))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
