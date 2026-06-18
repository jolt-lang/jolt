# jolt-fmm4 — (type x) on Chez: :type meta override, record class-name symbol,
# and a comprehensive value->taxonomy mapping (no value type crashes -> must be
# total, the recorded gotcha). Expectations are the build/jolt (seed) oracle.
# Producers that the seed makes eager (range) are avoided: (type (range 3)) is
# :vector on the seed (eager) but :seq on chez (lazy) — a range-container
# divergence unrelated to `type`, covered elsewhere.
#
#   janet test/chez/_type.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/jolt-chez"))

(def cases
  [# --- scalars ---
   ["int"          "(type 5)"                                  ":number"]
   ["float"        "(type 5.0)"                                ":number"]
   ["ratio-ish"    "(type (/ 10 2))"                           ":number"]
   ["string"       "(type \"s\")"                              ":string"]
   ["keyword"      "(type :k)"                                 ":keyword"]
   ["symbol"       "(type 'x)"                                 ":symbol"]
   ["true"         "(type true)"                               ":boolean"]
   ["false"        "(type false)"                              ":boolean"]
   ["nil"          "(type nil)"                                ""]
   ["char"         "(type \\a)"                                ":char"]

   # --- collections ---
   ["vector"       "(type [1 2])"                              ":vector"]
   ["empty vector" "(type [])"                                 ":vector"]
   ["map"          "(type {:a 1})"                             ":map"]
   ["set"          "(type #{1})"                               ":set"]
   ["list"         "(type '(1 2))"                             ":seq"]
   ["empty list"   "(type '())"                                ":seq"]
   ["map entry"    "(type (first {:a 1}))"                     ":vector"]
   ["lazy map"     "(type (map inc [1 2]))"                    ":seq"]
   ["lazy filter"  "(type (filter odd? [1 2 3]))"             ":seq"]
   ["lazy-seq"     "(type (lazy-seq (cons 1 nil)))"           ":seq"]
   ["take iterate" "(type (take 2 (iterate inc 0)))"          ":seq"]
   ["fn"           "(type inc)"                                ":fn"]
   ["sorted-map"   "(type (sorted-map :a 1))"                  ":map"]
   ["sorted-set"   "(type (sorted-set 1))"                     ":jolt/sorted-set"]

   # --- :type meta override (the headline jolt-fmm4 case) ---
   ["meta override"     "(type (with-meta [1] {:type :custom}))" ":custom"]
   ["meta override map" "(type (with-meta {:a 1} {:type :rec}))" ":rec"]
   ["meta no :type"     "(type (with-meta [1] {:other 9}))"      ":vector"]

   # --- record -> ns-qualified class-name symbol ---
   ["record symbol"   "(do (defrecord TyR [a]) (type (->TyR 1)))"                       "user.TyR"]
   ["record roundtrip" "(do (defrecord TyR [a]) (= (symbol (str (type (->TyR 1)))) (type (->TyR 1))))" "true"]
   ["record is symbol" "(do (defrecord TyR [a]) (symbol? (type (->TyR 1))))"            "true"]

   # --- exotic host wrappers (seed :jolt/* tags; total, never crash) ---
   ["atom"        "(type (atom 1))"                            ":jolt/atom"]
   ["volatile"    "(type (volatile! 1))"                       ":jolt/volatile"]
   ["regex"       "(type #\"re\")"                             ":jolt/regex"]
   ["var"         "(do (def vx 1) (type (var vx)))"            ":jolt/var"]
   ["transient"   "(type (transient []))"                      ":jolt/transient"]
   ["uuid"        "(type (random-uuid))"                       ":jolt/uuid"]
   ["ex-info"     "(type (ex-info \"x\" {}))"                  ":jolt/ex-info"]])

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

(printf "\n_type parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [l m] fails (printf "  FAIL [%s] %s" l m)))
(flush)
(os/exit (if (empty? fails) 0 1))
