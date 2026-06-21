# jolt-2o7x — dynamic var binding (binding / with-bindings* / var-set /
# thread-bound? / with-local-vars / with-redefs / bound-fn* / get-thread-bindings).
# Expectations are the JVM-canonical values. TDD harness: bin/joltc
# -e per case, last non-empty line == expected.
#
#   janet test/chez/_dynbind.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

(def cases
  [# --- binding: install / restore / seen across a fn call ---
   ["binding rebinds"        "(do (def ^:dynamic *bx* 10) (binding [*bx* 99] *bx*))"                "99"]
   ["binding restores"       "(do (def ^:dynamic *by* 10) (binding [*by* 99] *by*) *by*)"            "10"]
   ["binding seen by fn"     "(do (def ^:dynamic *bz* 0) (defn rdz [] *bz*) (binding [*bz* 7] (rdz)))" "7"]
   ["binding both: vec"      "(do (def ^:dynamic *bv* 1) [(binding [*bv* 2] *bv*) *bv*])"            "[2 1]"]
   ["nested binding"         "(do (def ^:dynamic *bn* 1) (binding [*bn* 2] (binding [*bn* 3] *bn*)))" "3"]
   ["nested binding outer"   "(do (def ^:dynamic *bo* 1) (binding [*bo* 2] (binding [*bo* 3] nil) *bo*))" "2"]
   # (a macro reading a dynamic var — corpus 606/607 — needs top-level defmacro
   # in the -e path, which the Chez driver can't compile yet; out of scope here.)

   # --- var-set inside a binding targets the frame; restored on exit ---
   ["var-set in binding"     "(do (def ^:dynamic *z* 1) (binding [*z* 0] (var-set (var *z*) 5) *z*))" "5"]
   ["var-set frame restores" "(do (def ^:dynamic *zr* 1) (binding [*zr* 0] (var-set (var *zr*) 5)) *zr*)" "1"]

   # --- thread-bound? ---
   ["thread-bound? unbound"  "(do (def ^:dynamic *tb* 1) (thread-bound? (var *tb*)))"                "false"]
   ["thread-bound? in scope" "(do (def ^:dynamic *tc* 1) (binding [*tc* 2] (thread-bound? (var *tc*))))" "true"]

   # --- with-bindings* / bound-fn* / get-thread-bindings ---
   # Binding frames look up by var cell identity, so a var-keyed binding map
   # resolves: the Clojure-correct value is 7.
   ["with-bindings*"         "(do (def ^:dynamic *wb* 1) (with-bindings* {(var *wb*) 7} (fn [] *wb*)))" "7"]
   ["bound-fn* conveys"      "(do (def ^:dynamic *cf* 1) (let [g (binding [*cf* 3] (bound-fn* (fn [] *cf*)))] (g)))" "3"]
   ["get-thread-bindings"    "(do (def ^:dynamic *gb* 1) (binding [*gb* 9] (get (get-thread-bindings) (var *gb*))))" "9"]

   # --- with-local-vars ---
   ["local-var get"          "(with-local-vars [x 1] (var-get x))"                                  "1"]
   ["local-var set"          "(with-local-vars [x 1] (var-set x 2) (var-get x))"                    "2"]
   ["local-var two"          "(with-local-vars [a 1 b 2] [(var-get a) (var-get b)])"                "[1 2]"]
   ["local-var as value"     "(with-local-vars [x 0] (let [bump (fn [v] (var-set v (+ 5 (var-get v))))] (bump x) (var-get x)))" "5"]
   ["local-var init outer"   "(let [y 3] (with-local-vars [x y] (var-get x)))"                      "3"]
   ["local-var body result"  "(with-local-vars [x 1] :done)"                                        ":done"]

   # --- with-redefs ---
   ["with-redefs rebinds"    "(do (defn wrf [] 1) (with-redefs [wrf (fn [] 42)] (wrf)))"             "42"]
   ["with-redefs restores"   "(do (defn wrg [] 1) (with-redefs [wrg (fn [] 42)]) (wrg))"             "1"]
   ["with-redefs on throw"   "(do (defn wrh [] 1) (try (with-redefs [wrh (fn [] 42)] (throw (ex-info \"x\" {}))) (catch :default e nil)) (wrh))" "1"]
   ["with-redefs-fn"         "(do (defn wri [] 1) (with-redefs-fn {(var wri) (fn [] 42)} (fn [] (wri))))" "42"]

   # --- alter-var-root ---
   ["alter-var-root"         "(do (def av 1) (alter-var-root (var av) inc) av)"                      "2"]])

(defn run-capture [expr]
  (def proc (os/spawn [jolt-bin "-e" expr] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (def err (ev/read (proc :err) 0x100000))
  (def code (os/proc-wait proc))
  (def lines (filter (fn [l] (not (empty? l)))
                     (string/split "\n" (string/trim (if out (string out) "")))))
  [code (if (empty? lines) "" (last lines)) (if err (string err) "")])

(var pass 0)
(def fails @[])
(each [label expr expected] cases
  (def [code got err] (run-capture expr))
  (cond
    (not= code 0) (array/push fails [label (string "exit " code "; err: " (string/trim err))])
    (= got expected) (++ pass)
    (array/push fails [label (string "want `" expected "`, got `" got "`")])))

(printf "\n_dynbind parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [l m] fails (printf "  FAIL [%s] %s" l m)))
(flush)
(os/exit (if (empty? fails) 0 1))
