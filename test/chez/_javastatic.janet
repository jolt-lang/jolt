# jolt-avt6 — host class statics + constructors on Chez. The analyzer lowers
# Class/member to :host-static and (Class. ...) to :host-new; the Chez emit lowers
# them to host-static-ref/host-static-call/host-new (host-static.ss registry).
# Expectations are the build/jolt (seed) oracle, captured per case.
#
#   janet test/chez/_javastatic.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/jolt-chez"))

# [label expr] — expected is whatever build/jolt prints (captured at runtime).
(def exprs
  ["(Math/sqrt 16)"
   "(Math/abs -3)"
   "(Math/max 2 7)"
   "(pos? Long/MAX_VALUE)"
   "(String/valueOf 42)"
   "(String/valueOf \"hi\")"
   "(String/valueOf :k)"
   "(String/valueOf nil)"
   "(Long/parseLong \"42\")"
   "(Long/valueOf \"42\")"
   "(Integer/parseInt \"ff\" 16)"
   "(.byteValue (Integer/valueOf \"ff\" 16))"
   "(Boolean/parseBoolean \"true\")"
   "(Boolean/parseBoolean \"yes\")"
   "(Character/isUpperCase \\A)"
   "(Character/isLowerCase \\a)"
   "(Character/isUpperCase \\a)"
   "(Thread/interrupted)"
   "(System/getProperty \"os.name\")"
   "(string? (get (System/getenv) \"HOME\"))"
   "(string? (System/getenv \"HOME\"))"
   "(fn? System/exit)"
   "(string? (get (System/getProperties) \"os.name\"))"
   "(pos? (count (seq (System/getenv))))"
   "(let [es (map (fn [[k v]] [k v]) (System/getenv))] (and (pos? (count es)) (every? vector? es)))"
   # constructors + their methods
   "(.toString (StringBuilder. \"x\"))"
   "(.toString (-> (StringBuilder.) (.append \"a\") (.append \\b) (.append 1)))"
   "(.toString (.append (StringBuilder. 16) \"x\"))"
   "(let [sb (StringBuilder.)] (.append sb \"abcd\") (.setLength sb 2) (.toString sb))"
   "(let [w (StringWriter.)] (.write w \"a\") (.append w \\b) (.toString w))"
   "(let [r (StringReader. \"ab\")] [(.read r) (.read r) (.read r)])"
   "(let [r (StringReader. \"ab\")] (.mark r 1) [(.read r) (do (.reset r) (.read r))])"
   "(let [r (java.io.PushbackReader. (java.io.StringReader. \"ab\"))] [(.read r) (.read r)])"
   "(let [r (PushbackReader. (StringReader. \"ab\")) a (.read r)] (.unread r a) [a (.read r) (.read r)])"
   "(let [r (PushbackReader. (StringReader. \"a\"))] (.unread r \\x) [(.read r) (.read r)])"
   "(BigInteger. \"123\")"
   "(let [m (HashMap. {:a 1 :b 2})] (.get m :b))"
   "(let [m (HashMap. {})] (.put m :x 1) (.put m :y 2) (.size m))"
   "(let [t (StringTokenizer. \"a=1&b=2\" \"&\")] [(.nextToken t) (.nextToken t)])"
   "(.toString (StringBuilder. \"x\"))"
   # ring-codec surface
   "(URLEncoder/encode \"a b=c\")"
   "(URLDecoder/decode (URLEncoder/encode \"x &=%?\"))"
   "(String. (.encode (Base64/getEncoder) (.getBytes \"hello\")))"
   "(String. (.decode (Base64/getDecoder) (String. (.encode (Base64/getEncoder) (.getBytes \"hello\")))))"
   "(Integer/parseInt \"ff\" 16)"
   # Pattern statics
   "(regex? (Pattern/compile \"a.c\"))"
   "(.split (Pattern/compile \",\") \"a,b,c\")"
   "(do (require '[clojure.string :as s]) (s/replace \"a1b2\" (Pattern/compile \"[0-9]\") \"\"))"
   "(boolean (re-find (Pattern/compile \"^x\" Pattern/MULTILINE) \"y\\nx\"))"
   "(boolean (re-find (re-pattern (Pattern/quote \"a.c\")) \"za.cy\"))"
   "(boolean (re-find (re-pattern (Pattern/quote \"a.c\")) \"zabcy\"))"])

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

(printf "\n_javastatic parity [%s]: %d/%d passed" jolt-bin pass (length exprs))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
