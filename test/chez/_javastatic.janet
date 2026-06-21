# jolt-avt6 — host class statics + constructors on Chez. The analyzer lowers
# Class/member to :host-static and (Class. ...) to :host-new; the Chez emit lowers
# them to host-static-ref/host-static-call/host-new (host-static.ss registry).
# Each case carries its expected printed value. Env-dependent values (os.name) are
# asserted via a predicate so the case stays portable across machines.
#
#   janet test/chez/_javastatic.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

(def cases
  [["(Math/sqrt 16)" "4"]
   ["(Math/abs -3)" "3"]
   ["(Math/max 2 7)" "7"]
   ["(pos? Long/MAX_VALUE)" "true"]
   ["(String/valueOf 42)" "42"]
   ["(String/valueOf \"hi\")" "hi"]
   ["(String/valueOf :k)" ":k"]
   ["(String/valueOf nil)" "null"]
   ["(Long/parseLong \"42\")" "42"]
   ["(Long/valueOf \"42\")" "42"]
   ["(Integer/parseInt \"ff\" 16)" "255"]
   ["(.byteValue (Integer/valueOf \"ff\" 16))" "-1"]
   ["(Boolean/parseBoolean \"true\")" "true"]
   ["(Boolean/parseBoolean \"yes\")" "false"]
   ["(Character/isUpperCase \\A)" "true"]
   ["(Character/isLowerCase \\a)" "true"]
   ["(Character/isUpperCase \\a)" "false"]
   ["(Thread/interrupted)" "false"]
   ["(string? (System/getProperty \"os.name\"))" "true"]
   ["(string? (get (System/getenv) \"HOME\"))" "true"]
   ["(string? (System/getenv \"HOME\"))" "true"]
   ["(fn? System/exit)" "true"]
   ["(string? (get (System/getProperties) \"os.name\"))" "true"]
   ["(pos? (count (seq (System/getenv))))" "true"]
   ["(let [es (map (fn [[k v]] [k v]) (System/getenv))] (and (pos? (count es)) (every? vector? es)))" "true"]
   # constructors + their methods
   ["(.toString (StringBuilder. \"x\"))" "x"]
   ["(.toString (-> (StringBuilder.) (.append \"a\") (.append \\b) (.append 1)))" "ab1"]
   ["(.toString (.append (StringBuilder. 16) \"x\"))" "x"]
   ["(let [sb (StringBuilder.)] (.append sb \"abcd\") (.setLength sb 2) (.toString sb))" "ab"]
   ["(let [w (StringWriter.)] (.write w \"a\") (.append w \\b) (.toString w))" "ab"]
   ["(let [r (StringReader. \"ab\")] [(.read r) (.read r) (.read r)])" "[97 98 -1]"]
   ["(let [r (StringReader. \"ab\")] (.mark r 1) [(.read r) (do (.reset r) (.read r))])" "[97 97]"]
   ["(let [r (java.io.PushbackReader. (java.io.StringReader. \"ab\"))] [(.read r) (.read r)])" "[97 98]"]
   ["(let [r (PushbackReader. (StringReader. \"ab\")) a (.read r)] (.unread r a) [a (.read r) (.read r)])" "[97 97 98]"]
   ["(let [r (PushbackReader. (StringReader. \"a\"))] (.unread r \\x) [(.read r) (.read r)])" "[120 97]"]
   ["(BigInteger. \"123\")" "123"]
   ["(let [m (HashMap. {:a 1 :b 2})] (.get m :b))" "2"]
   ["(let [m (HashMap. {})] (.put m :x 1) (.put m :y 2) (.size m))" "2"]
   ["(let [t (StringTokenizer. \"a=1&b=2\" \"&\")] [(.nextToken t) (.nextToken t)])" "[a=1 b=2]"]
   ["(.toString (StringBuilder. \"x\"))" "x"]
   # ring-codec surface
   ["(URLEncoder/encode \"a b=c\")" "a+b%3Dc"]
   ["(URLDecoder/decode (URLEncoder/encode \"x &=%?\"))" "x &=%?"]
   ["(String. (.encode (Base64/getEncoder) (.getBytes \"hello\")))" "aGVsbG8="]
   ["(String. (.decode (Base64/getDecoder) (String. (.encode (Base64/getEncoder) (.getBytes \"hello\")))))" "hello"]
   ["(Integer/parseInt \"ff\" 16)" "255"]
   # Pattern statics
   ["(regex? (Pattern/compile \"a.c\"))" "true"]
   ["(.split (Pattern/compile \",\") \"a,b,c\")" "(a b c)"]
   ["(do (require '[clojure.string :as s]) (s/replace \"a1b2\" (Pattern/compile \"[0-9]\") \"\"))" "ab"]
   ["(boolean (re-find (Pattern/compile \"^x\" Pattern/MULTILINE) \"y\\nx\"))" "true"]
   ["(boolean (re-find (re-pattern (Pattern/quote \"a.c\")) \"za.cy\"))" "true"]
   ["(boolean (re-find (re-pattern (Pattern/quote \"a.c\")) \"zabcy\"))" "false"]])

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

(printf "\n_javastatic parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
