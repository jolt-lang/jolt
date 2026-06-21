# jolt-kuic — the `.` special form + `.-field` field-access desugar on Chez. The
# analyzer lowers (. target member arg*) and (.-field target) to a :host-call;
# the Chez emit routes a non-shimmed :host-call through record-method-dispatch,
# which dot-forms.ss extends with field access + map/vector member dispatch.
# Each case carries its expected printed value.
#
#   janet test/chez/_dotform.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

(def cases
  [# strings: method calls via the String surface
   ["(. \"HI\" toLowerCase)" "hi"]
   ["(. \"abc\" length)" "3"]
   ["(. \"abc\" toUpperCase)" "ABC"]
   # vectors / maps: collection interop (count/nth/get/containsKey)
   ["(. [1 2 3] count)" "3"]
   ["(. [10 20 30] nth 1)" "20"]
   ["(. {:a 1 :b 2} count)" "2"]
   ["(. {:a 1 :b 2} get :b)" "2"]
   ["(. {:a 1} containsKey :a)" "true"]
   ["(. {:count 99} count)" "1"]
   # map member: stored fn called with self (+ args), else field value
   ["(. {:value 41 :describe (fn [self] (str \"v=\" (:value self)))} describe)" "v=41"]
   ["(. {:greet (fn [self n] (str \"Hello \" n))} greet \"Alice\")" "Hello Alice"]
   ["(. {:value 41} value)" "41"]
   # field access via .-field head
   ["(.-value {:value 41})" "41"]
   ["(.-x {:x 7 :y 9})" "7"]
   # field access via (. obj -field)
   ["(. {:value 41} -value)" "41"]
   # records: .-field reads a field, .method dispatches the protocol method
   ["(do (defrecord Rf [x]) (.-x (->Rf 7)))" "7"]
   ["(do (defrecord Rg [a b]) (.-b (->Rg 1 2)))" "2"]
   ["(do (defprotocol Greet (hi [_])) (defrecord Rh [nm] Greet (hi [_] (str \"hi \" nm))) (. (->Rh \"x\") hi))" "hi x"]
   # universal object-methods on a non-record map win over a field lookup
   ["(try (throw (ex-info \"bad\" {})) (catch Throwable e (.getMessage e)))" "bad"]
   ["(try (throw (ex-info \"bad\" {:k 1})) (catch Throwable e (.getMessage e)))" "bad"]
   ["(try (throw \"boom\") (catch Throwable e (.getMessage e)))" "boom"]
   ["(try (throw (Exception. \"boom\")) (catch Throwable e (.getMessage e)))" "boom"]
   ["(try (throw (IllegalArgumentException. \"bad\")) (catch Exception e (.getMessage e)))" "bad"]
   ["(.equals \"a\" \"a\")" "true"]
   ["(.equals \"a\" \"b\")" "false"]
   ["(. {:value 41 :describe (fn [self] (str \"v=\" (:value self)))} describe)" "v=41"]])

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
  (def [code got err] (run-capture jolt-bin expr))
  (cond
    (not= code 0) (array/push fails [expr (string "exit " code "; err: " err)])
    (= got expected) (++ pass)
    (array/push fails [expr (string "want `" expected "`, got `" got "`")])))

(printf "\n_dotform parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
