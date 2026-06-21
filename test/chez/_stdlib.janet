# jolt-j5vg / jolt-22vo / clojure.pprint — Phase-2 stdlib parity closeout.
#
#   clojure.set    — pure Clojure (src/jolt/clojure/set.clj) added to the Chez
#                    prelude tier (driver.janet stdlib-ns-files).
#   clojure.math   — native flonum-math shims (host/chez/math.ss) def-var!'d into
#                    the clojure.math ns; the analyzer already knows the ns exists
#                    (api.janet install-clojure-math!), so refs lower to var-deref.
#   clojure.pprint — minimal shim on the prelude; pprint's 2-arity no longer uses
#                    (binding [*out* writer] ...) (uncompilable on the no-fallback
#                    Chez back end; *out* isn't a bindable var — output always goes
#                    through the host seam).
#
# Outputs are order-stable (value-equality / scalars) so set/map iteration order
# — which is host-dependent — never masquerades as a divergence.
#
#
#   janet test/chez/_stdlib.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

(def cases
  [# --- clojure.math (jolt-22vo / jolt-h79) ---
   ["(< 1.4142 (clojure.math/sqrt 2) 1.4143)"                       "true"]
   ["(long (clojure.math/pow 2 10))"                                "1024"]
   ["(long (clojure.math/tan 0))"                                   "0"]
   ["(clojure.math/round 2.6)"                                      "3"]
   ["(clojure.math/floor 2.9)"                                      "2"]
   ["(clojure.math/signum -7.2)"                                    "-1"]
   ["(< 3.14 (clojure.math/to-radians 180) 3.15)"                   "true"]
   ["(< 3.14 clojure.math/PI 3.15)"                                 "true"]
   ["(< 2.71 clojure.math/E 2.72)"                                  "true"]
   ["(long (clojure.math/cbrt 27))"                                 "3"]
   ["(< 4.6 (clojure.math/log 100) 4.7)"                            "true"]
   # Chez has no native log10 (computed as log(x)/log(10)), so it can differ from
   # C log10 in the last ulp (3 vs 2.9999…); range-check, don't pin.
   ["(< 2.99 (clojure.math/log10 1000) 3.01)"                       "true"]
   ["(do (require (quote [clojure.math :as m])) (long (m/hypot 3 4)))" "5"]
   ["(mapv (comp long clojure.math/sqrt) [1 4])"                    "[1 2]"]
   ["(long (clojure.math/atan2 0 1))"                               "0"]
   # --- clojure.set (jolt-j5vg) ---
   ["(do (require (quote [clojure.set :as s])) (= #{1 2 3 4} (s/union #{1 2} #{3 4})))"        "true"]
   ["(do (require (quote [clojure.set :as s])) (= #{2} (s/intersection #{1 2} #{2 3})))"       "true"]
   ["(do (require (quote [clojure.set :as s])) (= #{1} (s/difference #{1 2} #{2 3})))"         "true"]
   ["(do (require (quote [clojure.set :as s])) (s/subset? #{1} #{1 2}))"                       "true"]
   ["(do (require (quote [clojure.set :as s])) (s/superset? #{1 2} #{1}))"                     "true"]
   ["(do (require (quote [clojure.set :as s])) (= {1 :a 2 :b} (s/map-invert {:a 1 :b 2})))"    "true"]
   ["(do (require (quote [clojure.set :as s])) (= #{:a} (s/select keyword? #{:a})))"           "true"]
   ["(do (require (quote [clojure.set :as s])) (= #{{:a 1 :b 2}} (s/join #{{:a 1}} #{{:b 2}})))" "true"]
   ["(do (require (quote [clojure.set :as s])) (= {:b 1} (s/rename-keys {:a 1} {:a :b})))"     "true"]
   ["(do (require (quote [clojure.set :as s])) (= 2 (count (s/index #{{:k 1} {:k 2}} [:k]))))" "true"]
   # --- clojure.pprint (minimal shim) ---
   ["(do (require (quote [clojure.pprint :as pp])) (= \"[1 2 3]\\n\" (with-out-str (pp/pprint [1 2 3]))))" "true"]
   ["(do (require (quote [clojure.pprint :as pp])) (pp/with-pprint-dispatch pp/code-dispatch 42))" "42"]])

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

(printf "\n_stdlib parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
