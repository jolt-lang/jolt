# jolt-zikh — var def-time metadata capture (^:private / ^Type tag / docstring).
# (meta (var v)) must carry the def-time reader metadata + :ns/:name, matching the
# JVM-canonical reference. TDD harness: bin/joltc -e per case, last line ==
# expected.
#
#   janet test/chez/_var_meta.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

(def cases
  # NOTE: ^{:map} metadata on a def name (e.g. (def ^{:doc "hi"} dv 1)) reads as
  # (def (with-meta name m) v) and is uncompilable for the COMPILER generally
  # (analyzer.clj rejects it) — out of subset, not a meta-capture gap. Shorthand
  # ^:kw / ^Type
  # and the docstring form keep the name a plain symbol, so they're in scope.
  [["^:private on var"   "(do (def ^:private pv 1) (:private (meta (var pv))))"      "true"]
   ["^Type tag on var"   "(do (def ^String tv \"a\") (:tag (meta (var tv))))"        "String"]
   ["(def name doc val)"  "(do (def dv2 \"hi\" 1) (:doc (meta (var dv2))))"          "hi"]
   ["meta carries :name"  "(do (def mv 1) (:name (meta (var mv))))"                  "mv"]
   ["meta carries :ns"    "(do (def nv 1) (:ns (meta (var nv))))"                    "user"]
   ["plain def: no user meta" "(do (def pl 1) (nil? (:private (meta (var pl)))))"    "true"]])

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

(printf "\n_var_meta parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [l m] fails (printf "  FAIL [%s] %s" l m)))
(flush)
(os/exit (if (empty? fails) 0 1))
