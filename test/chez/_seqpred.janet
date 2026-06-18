# sequential? / seq? on lazy seqs (jolt-2o7x follow-up). The inc M fix made the
# native seq? var recognize a lazy-seq (re-def-var!, not just set!). sequential?
# is overlay (`(defn sequential? [x] (or (vector? x) (seq? x)))`), so it inherits
# the fix transitively; this pins that both predicates agree with the JVM oracle
# over every lazy-seq-producing form (and the native =/hash path via set!).
# Expectations are the build/jolt (JVM-canonical) values.
#
#   janet test/chez/_seqpred.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/jolt-chez"))

(def cases
  [# --- seq? over lazy seqs ---
   # (NB: not (seq? (range 3)) — the seed makes range an eager vector, chez a lazy
   #  seq; a range-container divergence, not the predicate. sequential? agrees on it.)
   ["seq? map"              "(seq? (map inc [1 2 3]))"               "true"]
   ["seq? filter"           "(seq? (filter odd? [1 2 3]))"          "true"]
   ["seq? lazy-seq"         "(seq? (lazy-seq (cons 1 nil)))"        "true"]
   ["seq? take iterate"     "(seq? (take 3 (iterate inc 0)))"       "true"]
   ["seq? cons onto lazy"   "(seq? (cons 0 (range 3)))"             "true"]
   ["seq? vector false"     "(seq? [1 2 3])"                        "false"]
   ["seq? map-coll false"   "(seq? {:a 1})"                         "false"]
   ["seq? nil false"        "(seq? nil)"                            "false"]

   # --- sequential? over lazy seqs (overlay, delegates to seq?) ---
   ["sequential? range"     "(sequential? (range 3))"               "true"]
   ["sequential? map"       "(sequential? (map inc [1 2 3]))"       "true"]
   ["sequential? filter"    "(sequential? (filter odd? [1 2 3]))"   "true"]
   ["sequential? lazy-seq"  "(sequential? (lazy-seq (cons 1 nil)))" "true"]
   ["sequential? infinite"  "(sequential? (take 2 (repeat 9)))"     "true"]
   ["sequential? vector"    "(sequential? [1 2 3])"                 "true"]
   ["sequential? list"      "(sequential? '(1 2 3))"                "true"]
   ["sequential? map false" "(sequential? {:a 1})"                  "false"]
   ["sequential? set false" "(sequential? #{1 2})"                  "false"]
   ["sequential? nil false" "(sequential? nil)"                     "false"]

   # --- native =/hash path (jolt-sequential? via set!) over a raw lazy seq ---
   ["= vec lazyseq"         "(= [0 1 2] (range 3))"                 "true"]
   ["= lazyseq vec"         "(= (range 3) [0 1 2])"                 "true"]
   ["= lazyseq list"        "(= (map inc [0 1]) '(1 2))"            "true"]
   ["set contains lazyseq"  "(contains? #{[0 1 2]} (vec (range 3)))" "true"]])

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

(printf "\n_seqpred parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [l m] fails (printf "  FAIL [%s] %s" l m)))
(flush)
(os/exit (if (empty? fails) 0 1))
