# Phase 1 — jolt IR -> Chez Scheme emitter (jolt-cf1q.2).
#
# The new back end: consumes the SAME host-neutral IR (jolt.ir, see
# jolt-core/jolt/ir.clj) the analyzer produces and the Janet backend consumes,
# but emits Scheme source text instead of Janet. `host/compile` (Chez `eval`)
# turns that into a procedure. This increment covers the pure-functional subset
# (const/local/var/rt/if/do/let/fn/invoke/def/loop/recur) — enough to run
# fib/mandelbrot-shaped code through the REAL IR. Globals are early-bound here;
# var-cell late binding is the next increment.
#
# IR nodes are plain :op-tagged structs/tables (keyword keys), matching ir.clj.

(def rt-map
  # jolt RT primitive name -> Scheme. = is the exactness-aware jolt= from
  # values.ss; inc/dec/quot get preamble shims. Arithmetic/compare are native.
  {"+" "+" "-" "-" "*" "*" "/" "/"
   "<" "<" ">" ">" "<=" "<=" ">=" ">="
   "=" "jolt=" "inc" "jolt-inc" "dec" "jolt-dec"
   "mod" "modulo" "quot" "quotient" "rem" "remainder"})

(var- recur-target nil)
(var- gensym-n 0)
(defn- fresh-label [prefix] (string prefix (++ gensym-n)))

# MVP: jolt local/var names are valid Scheme identifiers (inc, even?, + all are).
(defn- munge [name] name)

(var emit nil)   # forward declaration (mutual recursion with the helpers below)

(defn- emit-const [v]
  (cond
    (nil? v) "jolt-nil"
    (boolean? v) (if v "#t" "#f")
    (number? v) (string v)
    (string? v) (string/format "%j" v)   # quoted+escaped string literal
    (errorf "emit-const: unsupported literal %p" v)))

(defn- emit-binding [b]
  (string "(" (munge (get b 0)) " " (emit (get b 1)) ")"))

(defn- emit-let [node]
  (string "(let* (" (string/join (map emit-binding (get node :bindings)) " ") ") "
          (emit (get node :body)) ")"))

(defn- emit-loop [node]
  (def label (fresh-label "loop"))
  (def bs (string/join (map emit-binding (get node :bindings)) " "))
  (def prev recur-target)
  (set recur-target label)
  (def body (emit (get node :body)))
  (set recur-target prev)
  (string "(let " label " (" bs ") " body ")"))

(defn- emit-recur [node]
  (unless recur-target (error "emit: recur outside a loop/fn target"))
  (string "(" recur-target " " (string/join (map emit (get node :args)) " ") ")"))

(defn- emit-fn [node]
  (def arities (get node :arities))
  (when (not= 1 (length arities)) (error "emit: multi-arity fn not in this increment"))
  (def a (first arities))
  (when (get a :rest) (error "emit: variadic fn not in this increment"))
  (def params (map munge (get a :params)))
  # wrap the body in a named let so fn-level `recur` rebinds the params
  (def label (fresh-label "fnrec"))
  (def prev recur-target)
  (set recur-target label)
  (def body (emit (get a :body)))
  (set recur-target prev)
  (string "(lambda (" (string/join params " ") ") "
          "(let " label " (" (string/join (map (fn [p] (string "(" p " " p ")")) params) " ") ") "
          body "))"))

(set emit (fn emit [node]
  (case (get node :op)
    :const (emit-const (get node :val))
    :local (munge (get node :name))
    :var   (munge (get node :name))           # early-bound (MVP)
    :rt    (or (get rt-map (get node :name))
               (errorf "emit: unmapped rt primitive %s" (get node :name)))
    :host  (get node :name)
    :if    (string "(if (jolt-truthy? " (emit (get node :test)) ") "
                   (emit (get node :then)) " " (emit (get node :else)) ")")
    :do    (string "(begin "
                   (string/join (map emit (get node :statements)) " ")
                   (if (empty? (get node :statements)) "" " ")
                   (emit (get node :ret)) ")")
    :invoke (string "(" (emit (get node :fn)) " "
                    (string/join (map emit (get node :args)) " ") ")")
    :let   (emit-let node)
    :loop  (emit-loop node)
    :recur (emit-recur node)
    :fn    (emit-fn node)
    :def   (string "(define " (munge (get node :name)) " " (emit (get node :init)) ")")
    (errorf "emit: unhandled op %p" (get node :op)))))

# Wrap emitted top-level forms into a runnable Chez program: preamble (value
# model + rt shims) then the forms, then print `final` (a Scheme expr string).
(defn program [forms-scheme final]
  (string
    "(import (chezscheme))\n"
    "(load \"host/chez/values.ss\")\n"
    "(define (jolt-inc x) (+ x 1))\n"
    "(define (jolt-dec x) (- x 1))\n"
    (string/join forms-scheme "\n") "\n"
    "(printf \"~a\\n\" " final ")\n"))
