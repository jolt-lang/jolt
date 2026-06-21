# jolt-yxqm — namespace value model (find-ns/ns-name/all-ns/resolve/ns-publics/
# in-ns/*ns* …). TDD harness: drives bin/joltc -e per case (fresh subprocess
# = per-case isolation), checks the LAST printed line == expected. Expected
# values are the JVM-canonical reference, baked per case.
#
#   janet test/chez/_ns.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

# [label expr expected-last-line]
(def cases
  [["find-ns existing"      "(some? (find-ns 'clojure.core))"                         "true"]
   ["find-ns missing"       "(nil? (find-ns 'does.not.exist))"                        "true"]
   ["resolve native-op +"   "(var? (resolve '+))"                                     "true"]
   ["resolve undefined"     "(nil? (resolve 'totally-undefined-xyz))"                 "true"]
   ["ns-publics has def"    "(do (def npv 1) (some? (get (ns-publics 'user) 'npv)))"  "true"]
   ["ns-map has def"        "(do (def nmv 1) (some? (get (ns-map 'user) 'nmv)))"      "true"]
   ["ns-aliases is a map"   "(map? (ns-aliases 'clojure.core))"                       "true"]
   ["ns-interns is a map"   "(map? (ns-interns 'user))"                               "true"]
   ["ns-interns count pos"  "(do (def q 1) (pos? (count (ns-interns 'user))))"        "true"]
   ["all-ns count pos"      "(pos? (count (all-ns)))"                                 "true"]
   ["ns-name *ns* = user"   "(= (ns-name *ns*) (ns-name (find-ns 'user)))"            "true"]
   ["in-ns returns ns str"  "(str (in-ns 'jolt.test-ns-b))"                           "jolt.test-ns-b"]
   ["in-ns updates *ns*"    "(do (in-ns 'jolt.test-ns-a) (str *ns*))"                 "jolt.test-ns-a"]
   ["ns-unmap clears var"   "(do (def nuv 1) (ns-unmap 'user 'nuv) (nil? (resolve 'nuv)))" "true"]
   ["in-ns no error"        "(do (in-ns 'my.ns) (symbol? 'x))"                        "true"]
   ["str ns-name *ns*"      "(str (ns-name *ns*))"                                    "user"]
   ["find-var qualified"    "(var? (find-var 'clojure.core/map))"                     "true"]
   ["the-ns ns-name"        "(= 'user (ns-name (the-ns 'user)))"                      "true"]
   ["create-ns ns-name"     "(= 'foo.bar (ns-name (create-ns 'foo.bar)))"            "true"]])

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

(printf "\n_ns parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [l m] fails (printf "  FAIL [%s] %s" l m)))
(flush)
(os/exit (if (empty? fails) 0 1))
