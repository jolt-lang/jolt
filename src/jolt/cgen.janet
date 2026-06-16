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

# Emit just the `static Janet cfun_<c-name>(...) { ... }` definition for one fn
# into buf. Shared by the single-fn and multi-fn (AOT module) generators.
(defn- emit-cfun [buf ir c-name]
  (def fnode (fn-ir-of ir))
  (def ar (first (ct/vview (fnode :arities))))
  (def params (map string (ct/vview (ar :params))))
  (def namer (new-namer))
  (def scope @{})
  (buffer/push buf "static Janet cfun_" c-name "(int32_t argc, Janet *argv) {\n")
  (buffer/push buf "janet_fixarity(argc, " (string (length params)) ");\n")
  (eachp [i pn] params
    (def cv (namer))
    (buffer/push buf "double " cv " = janet_getnumber(argv, " (string i) ");\n")
    (put scope pn cv))
  (def ret (namer))
  (buffer/push buf "double " ret ";\n")
  (emit-to (ar :body) ret scope nil buf namer)
  (buffer/push buf "return janet_wrap_number(" ret ");\n}\n"))

(defn gen-c-module
  "entries: [{:sym <C identifier> :ir <numeric-leaf fn IR>} ...] -> C source for
  ONE Janet native module exporting every entry's :sym as a cfunction. This is
  the AOT/build-time shape (jolt-a7ds): all of an app's hot fns in a single .so,
  compiled once at build time. Assumes each :ir is a numeric leaf."
  [entries]
  (def buf @"")
  (buffer/push buf "#include <janet.h>\n")
  (each e entries (emit-cfun buf (e :ir) (e :sym)))
  (buffer/push buf "static const JanetReg cfuns[] = {\n")
  (each e entries
    (buffer/push buf "  {\"" (e :sym) "\", cfun_" (e :sym) ", NULL},\n"))
  (buffer/push buf "  {NULL, NULL, NULL}};\n")
  (buffer/push buf "JANET_MODULE_ENTRY(JanetTable *env) { janet_cfuns(env, \"cg\", cfuns); }\n")
  (string buf))

(defn gen-c-fn
  "ir (a numeric-leaf :def-of-fn or :fn node), c-name -> C source for a Janet
  native module exporting `c-name` as a cfunction. Assumes (numeric-leaf? ir)."
  [ir c-name]
  (gen-c-module [{:sym c-name :ir ir}]))

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
  "True when a C compiler and janet.h are present (so compile-fn can work). Never
  throws: a missing cc (not on PATH) makes os/execute raise, so guard it — a
  toolchain-less deploy target must get false, not a crash."
  []
  (and (header-dir)
       (let [r (protect (os/execute ["cc" "--version"] :p
                                    {:out (file/open "/dev/null" :w)
                                     :err (file/open "/dev/null" :w)}))]
         (and (r 0) (= 0 (r 1))))))

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

(defn- abspath [p] (if (string/has-prefix? "/" p) p (string (os/cwd) "/" p)))

# Build a .so from C source, content-addressed and cached. The C + Janet ABI +
# platform fully determine the .so, so a matching cache entry is always valid.
# Returns the absolute .so path. JOLT_CGEN_NO_CACHE=1 forces a rebuild.
(defn- build-so [src dir]
  (def stamp (string src "|" janet/version "-" janet/build "|" (os/which)))
  (def key (string (band (hash stamp) 0x7FFFFFFF) "-" (length src)))
  (mkdirs dir)
  (def base (string dir "/cg-" key))
  (def sopath (string base ".so"))
  (def abs (abspath sopath))
  (unless (and (not (os/getenv "JOLT_CGEN_NO_CACHE")) (os/stat abs))
    (def cpath (string base ".c"))
    (spit cpath src)
    (cc-compile cpath sopath))
  abs)

(defn load-module
  "Load a prebuilt cgen .so (NO cc — works on a target with no toolchain) and
  return a table of symbol-string -> cfunction for every cfunction it exports.
  This is the runtime side of the build-time/AOT path (jolt-a7ds)."
  [sopath]
  (def env @{})
  (native (abspath sopath) env)
  (def out @{})
  (eachp [k v] env
    (when (and (symbol? k) (table? v) (or (cfunction? (v :value)) (function? (v :value))))
      (put out (string k) (v :value))))
  out)

(defn compile-module
  "AOT path: compile MANY numeric-leaf fns into ONE native module, once, at build
  time. entries: [{:sym <C id> :ir <numeric-leaf IR>} ...]. Returns
  {:sopath <abs .so> :fns {sym -> cfunction}}, or nil if the toolchain is absent
  or any entry isn't a numeric leaf. opts: :dir (cache dir)."
  [entries &opt opts]
  (default opts {})
  (when (and (header-dir) (all |(numeric-leaf? ($ :ir)) entries))
    (def src (gen-c-module entries))
    (def sopath (build-so src (get opts :dir (cache-dir))))
    {:sopath sopath :fns (load-module sopath)}))

(defn compile-fn
  "Compile a single numeric-leaf fn IR to a native Janet cfunction. Returns the
  cfunction, or nil if ir isn't a candidate or the toolchain is unavailable.
  opts: :dir (cache dir, default a persistent jolt-cgen under TMPDIR), :name (C
  identifier base). Content-addressed/cached like compile-module."
  [ir &opt opts]
  (default opts {})
  (when (and (numeric-leaf? ir) (header-dir))
    (def c-name (sanitize (get opts :name "jolt_cfn")))
    (def src (gen-c-fn ir c-name))
    (def sopath (build-so src (get opts :dir (cache-dir))))
    (def env @{})
    (native sopath env)
    ((get env (symbol c-name)) :value)))

# --- AOT build/deploy (jolt-a7ds) ---------------------------------------------
# Build phase (needs cc): collect an app's numeric-leaf fns (the backend's
# :cgen-collect? mode fills (ctx :env) :cgen-collected with [{:ns :name :ir}]),
# compile them all into ONE native module, and write a manifest. Deploy phase
# (NO cc, target needs no toolchain): load the prebuilt module + manifest into a
# qname->cfunction map that the backend installs as var roots (:cgen-prebuilt).

(defn aot-build
  "collected: [{:ns :name :ir} ...] (numeric-leaf fns). Compile them into one
  native module and return {:sopath <abs .so> :entries [{:ns :name :sym}]}, or
  nil (toolchain absent / a non-leaf / empty). opts: :dir (cache dir).
  Syms are positional (f0, f1, ...) so they're collision-free."
  [collected &opt opts]
  (default opts {})
  (when (and (header-dir) (not (empty? collected)))
    (def entries @[])
    (def manifest @[])
    (eachp [i e] collected
      (def sym (string "f" i))
      (array/push entries {:sym sym :ir (e :ir)})
      (array/push manifest {:ns (e :ns) :name (e :name) :sym sym}))
    (def m (compile-module entries opts))
    (and m {:sopath (m :sopath) :entries manifest})))

(defn write-manifest
  "Persist an aot-build result to path as jdn (the deploy side reads it)."
  [path build]
  (spit path (string/format "%j" build)))

(defn load-aot
  "Deploy side (NO cc): read a manifest written by write-manifest, load its
  prebuilt .so, and return a qname (\"ns/name\") -> cfunction map suitable for
  (ctx :env) :cgen-prebuilt. nil if the manifest/.so is missing."
  [manifest-path]
  (when (os/stat manifest-path)
    (def build (parse (slurp manifest-path)))
    (def fns (load-module (build :sopath)))
    (def out @{})
    (each e (build :entries)
      (when-let [f (get fns (e :sym))]
        (put out (string (e :ns) "/" (e :name)) f)))
    out))
