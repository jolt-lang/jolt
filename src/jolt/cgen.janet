# Native code generation: jolt IR -> C, for hot numeric-leaf fns (jolt-ihdp, the
# lever-1 native-codegen tier of epic jolt-5vsp). The spike
# (docs/foundational-runtime-lever1-native-codegen.md) showed native-C compute
# beats the Janet-VM floor (~18-22x faster than bytecode, edges out JVM Clojure),
# and that compiling a hot LEAF fn to C — called from a bytecode loop — captures
# the win because the forward (bytecode -> C) crossing is nearly free.
#
# This module is the IR -> C translator + a cc/load driver. It is a standalone
# library: nothing wires it into the default compile path yet (that — detecting
# hot fns and installing the C version onto the var cell — is the next step). A
# fn is a candidate only if its whole body is numeric (params/locals/return are
# numbers, all calls are native-op arithmetic): then everything stays in C
# `double`s, unboxed at entry and reboxed at return, with no Janet in the hot loop.
(import ./core_types :as ct)
(import ./phm :as phm)

(defn- norm-node [n] (if (phm/phm? n) (phm/phm-to-struct n) n))

# jolt core fn name -> {:c "<C operator>" :kind :infix|:incdec}. The subset of
# backend/native-ops that maps to a C double operator with matching semantics.
# (mod/rem/bit-ops/min/max deliberately omitted for now — they need helper calls
# or differ from C operators; add them as the candidate grammar grows.)
(def- c-ops
  {"+" {:c "+" :kind :infix}   "-" {:c "-" :kind :infix}
   "*" {:c "*" :kind :infix}   "/" {:c "/" :kind :infix}
   "<" {:c "<" :kind :infix}   ">" {:c ">" :kind :infix}
   "<=" {:c "<=" :kind :infix} ">=" {:c ">=" :kind :infix}
   "=" {:c "==" :kind :infix}
   "inc" {:c "+" :kind :incdec} "dec" {:c "-" :kind :incdec}})

(defn- args-of [node] (ct/vview (node :args)))

# A native-op invoke -> its c-ops entry, else nil (the head must be a clojure.core
# ref to a supported primitive at the right arity).
(defn- op-for [node]
  (def n (norm-node node))
  (def f (and (= :invoke (n :op)) (norm-node (n :fn))))
  (and f (= :var (f :op)) (= "clojure.core" (f :ns))
       (let [op (get c-ops (f :name))
             nargs (length (args-of n))]
         (and op
              (if (= (op :kind) :incdec) (= 1 nargs) (= 2 nargs))
              op))))

# --- candidate check: every node in the body is C-translatable ---
(var- numeric-ok? nil)
(defn- bindings-ok? [node]
  (all (fn [pair] (numeric-ok? (in (ct/vview pair) 1))) (ct/vview (node :bindings))))
(set numeric-ok?
  (fn numeric-ok? [raw]
    (def node (norm-node raw))
    (case (node :op)
      :const (number? (node :val))
      :local true
      :if (and (numeric-ok? (node :test)) (numeric-ok? (node :then)) (numeric-ok? (node :else)))
      :do (and (all numeric-ok? (ct/vview (node :statements))) (numeric-ok? (node :ret)))
      :let (and (bindings-ok? node) (numeric-ok? (node :body)))
      :loop (and (bindings-ok? node) (numeric-ok? (node :body)))
      :recur (all numeric-ok? (args-of node))
      :invoke (and (op-for node) (all numeric-ok? (args-of node)))
      false)))

(defn fn-ir-of
  "Unwrap a :def-of-fn or a bare :fn IR node to the :fn node, or nil."
  [ir]
  (def n (norm-node ir))
  (def init (norm-node (get n :init n)))
  (and (= :fn (init :op)) init))

(defn numeric-leaf?
  "True when ir is a single-fixed-arity fn whose entire body is C-translatable
  (numeric in, numeric out, only native-op arithmetic)."
  [ir]
  (def fnode (fn-ir-of ir))
  (and fnode
       (let [ars (ct/vview (fnode :arities))]
         (and (= 1 (length ars))
              (not ((first ars) :rest))
              (numeric-ok? ((first ars) :body))))))

# --- C generation ---
# scope: jolt-local-name (string) -> C var name. Fresh C names sidestep charset
# and keyword collisions. rctx (recur target): {:vars [c-names] :flag flag-name}.

(defn- new-namer []
  (def counter @[0])
  (fn [] (def n (in counter 0)) (put counter 0 (+ n 1)) (string "v" n)))

(defn- fmtnum [x]
  (def s (string/format "%.17g" (* 1.0 x)))
  (if (or (string/find "." s) (string/find "e" s) (string/find "n" s)) s (string s ".0")))

(var- c-expr nil)
(set c-expr
  (fn c-expr [raw scope]
    (def node (norm-node raw))
    (case (node :op)
      :const (fmtnum (node :val))
      :local (or (get scope (node :name)) (error (string "cgen: unbound local " (node :name))))
      :invoke (let [op (op-for node) as (args-of node)]
                (if (= (op :kind) :incdec)
                  (string "(" (c-expr (in as 0) scope) " " (op :c) " 1)")
                  (string "(" (c-expr (in as 0) scope) " " (op :c) " " (c-expr (in as 1) scope) ")")))
      (error (string "cgen: non-pure op in expr position: " (node :op))))))

(defn- pure? [node]
  (case (node :op) :const true :local true :invoke (truthy? (op-for node)) false))

# emit-to: append C statements that compute node's (double) value into dest.
# Control-flow forms (if/do/let/loop/recur) lower to statements; pure nodes to a
# single assignment. A comparison yields C int 0/1, which is a fine double here.
(var- emit-to nil)
(set emit-to
  (fn emit-to [raw dest scope rctx out namer]
    (def node (norm-node raw))
    (case (node :op)
      :if (let [t (namer)]
            (buffer/push out "double " t ";\n")
            (emit-to (node :test) t scope rctx out namer)
            (buffer/push out "if (" t " != 0.0) {\n")
            (emit-to (node :then) dest scope rctx out namer)
            (buffer/push out "} else {\n")
            (emit-to (node :else) dest scope rctx out namer)
            (buffer/push out "}\n"))
      :do (do (each s (ct/vview (node :statements))
                (def junk (namer)) (buffer/push out "double " junk ";\n")
                (emit-to s junk scope rctx out namer))
              (emit-to (node :ret) dest scope rctx out namer))
      :let (let [scope2 (table/clone scope)]
             (each pair (ct/vview (node :bindings))
               (def p (ct/vview pair))
               (def cv (namer))
               (buffer/push out "double " cv ";\n")
               (emit-to (in p 1) cv scope2 rctx out namer)
               (put scope2 (string (in p 0)) cv))
             (emit-to (node :body) dest scope2 rctx out namer))
      :loop (let [scope2 (table/clone scope) cvars @[] flag (namer)]
              (each pair (ct/vview (node :bindings))
                (def p (ct/vview pair))
                (def cv (namer))
                (buffer/push out "double " cv ";\n")
                (emit-to (in p 1) cv scope2 rctx out namer)
                (put scope2 (string (in p 0)) cv)
                (array/push cvars cv))
              (buffer/push out "int " flag " = 1;\n")
              (buffer/push out "while (" flag ") {\n" flag " = 0;\n")
              (emit-to (node :body) dest scope2 {:vars cvars :flag flag} out namer)
              (buffer/push out "}\n"))
      # recur: compute new values into temps first (avoid clobbering loop vars
      # mid-update), then assign and set the continue flag. Produces no value —
      # the enclosing while re-runs.
      :recur (let [as (args-of node) tmps @[]]
               (each a as
                 (def tv (namer))
                 (buffer/push out "double " tv ";\n")
                 (emit-to a tv scope rctx out namer)
                 (array/push tmps tv))
               (for i 0 (length tmps)
                 (buffer/push out (in (rctx :vars) i) " = " (in tmps i) ";\n"))
               (buffer/push out (rctx :flag) " = 1;\n"))
      (if (pure? node)
        (buffer/push out dest " = " (c-expr node scope) ";\n")
        (error (string "cgen: unsupported op " (node :op)))))))

(defn- sanitize [name]
  (def b @"")
  (each c name
    (buffer/push b (if (or (and (>= c 97) (<= c 122)) (and (>= c 65) (<= c 90))
                           (and (>= c 48) (<= c 57)) (= c 95)) c 95)))
  (string b))

(defn gen-c-fn
  "ir (a numeric-leaf :def-of-fn or :fn node), c-name -> a C source string for a
  Janet native module exporting `c-name` as a cfunction. Assumes (numeric-leaf? ir)."
  [ir c-name]
  (def fnode (fn-ir-of ir))
  (def ar (first (ct/vview (fnode :arities))))
  (def params (map string (ct/vview (ar :params))))
  (def namer (new-namer))
  (def scope @{})
  (def pre @"")
  (buffer/push pre "#include <janet.h>\n")
  (buffer/push pre "static Janet cfun_" c-name "(int32_t argc, Janet *argv) {\n")
  (buffer/push pre "janet_fixarity(argc, " (string (length params)) ");\n")
  (eachp [i pn] params
    (def cv (namer))
    (buffer/push pre "double " cv " = janet_getnumber(argv, " (string i) ");\n")
    (put scope pn cv))
  (def out @"")
  (def ret (namer))
  (buffer/push out "double " ret ";\n")
  (emit-to (ar :body) ret scope nil out namer)
  (string pre out
          "return janet_wrap_number(" ret ");\n}\n"
          "static const JanetReg cfuns[] = {{\"" c-name "\", cfun_" c-name
          ", NULL}, {NULL, NULL, NULL}};\n"
          "JANET_MODULE_ENTRY(JanetTable *env) { janet_cfuns(env, \"cg\", cfuns); }\n"))

# --- toolchain ---
(defn header-dir
  "Directory containing janet.h, probed from syspath + common prefixes, or nil."
  []
  (def sp (dyn :syspath))
  (def cands @[(string sp "/../../include") "/opt/homebrew/include"
               "/usr/local/include" "/usr/include"])
  (var found nil)
  (each c cands (when (and (not found) (os/stat (string c "/janet.h"))) (set found c)))
  found)

(defn toolchain-available?
  "True when a C compiler and janet.h are present (so compile-fn can work)."
  []
  (and (header-dir)
       (= 0 (os/execute ["cc" "--version"] :px
                        {:out (file/open "/dev/null" :w) :err (file/open "/dev/null" :w)}))))

(defn- mkdirs [path]
  (def parts (filter |(not (empty? $)) (string/split "/" path)))
  (var acc (if (string/has-prefix? "/" path) "" "."))
  (each p parts (set acc (string acc "/" p)) (os/mkdir acc)))

(defn- cache-dir []
  # Persistent across runs (like the ctx image cache, which also defaults to
  # TMPDIR). Override with JOLT_CGEN_CACHE_DIR.
  (string (or (os/getenv "JOLT_CGEN_CACHE_DIR") (os/getenv "TMPDIR") "/tmp") "/jolt-cgen"))

(defn- cc-compile [cpath sopath]
  # macOS needs -undefined dynamic_lookup so the module's janet_* refs resolve
  # against the host at load time; ELF (Linux/BSD) resolves them then anyway.
  (def cmd @["cc" "-shared" "-fPIC" "-O2" (string "-I" (header-dir))])
  (when (= :macos (os/which)) (array/push cmd "-undefined") (array/push cmd "dynamic_lookup"))
  (array/push cmd cpath "-o" sopath)
  (def r (os/execute cmd :p))
  (unless (= 0 r) (error (string "cgen: cc failed (" r ") for " cpath))))

(defn compile-fn
  "Compile a numeric-leaf fn IR to a native Janet cfunction. Returns the
  cfunction, or nil if ir isn't a candidate or the toolchain is unavailable.
  opts: :dir (cache dir, default a persistent jolt-cgen under TMPDIR), :name (C
  identifier base). The .so is content-addressed by a hash of the generated C +
  the Janet ABI + platform, so cc only runs on the first build of a given fn;
  later runs (same source) reuse the cached .so. JOLT_CGEN_NO_CACHE=1 forces a
  rebuild. cc errors propagate (a compiler crash is a cgen bug, not a punt)."
  [ir &opt opts]
  (default opts {})
  (when (and (numeric-leaf? ir) (header-dir))
    (def c-name (sanitize (get opts :name "jolt_cfn")))
    (def src (gen-c-fn ir c-name))
    # content address: the C (semantics + symbol name) + ABI + platform fully
    # determine the .so, so a matching cache entry is always valid.
    (def stamp (string src "|" janet/version "-" janet/build "|" (os/which)))
    (def key (string (band (hash stamp) 0x7FFFFFFF) "-" (length src)))
    (def dir (get opts :dir (cache-dir)))
    (mkdirs dir)
    (def base (string dir "/cg-" key))
    (def sopath (string base ".so"))
    (def abs (if (string/has-prefix? "/" sopath) sopath (string (os/cwd) "/" sopath)))
    (unless (and (not (os/getenv "JOLT_CGEN_NO_CACHE")) (os/stat abs))
      (def cpath (string base ".c"))
      (spit cpath src)
      (cc-compile cpath sopath))
    (def env @{})
    (native abs env)
    ((get env (symbol c-name)) :value)))
