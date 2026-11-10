;;   Portions of this file are from core.async's src/main/clojure/clojure/core/async.clj,
;;   Copyright (c) Rich Hickey and contributors. All rights reserved.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

;; clojure.core.async — higher-level dataflow API over the channel primitives.
;;
;; The primitives (chan, <!, >!, <!!, >!!, close!, put!, take!, offer!, timeout,
;; promise-chan, buffer/dropping-buffer/sliding-buffer, thread, go-spawn,
;; fiber-spawn, fiber-execute) are provided natively (host/chez/java/async.ss);
;; everything but the two fiber ones (fiber-spawn is io-thread's carrier — see
;; thread-call below; fiber-execute is the same spawn without a result channel,
;; behind clojure.core.async.impl.dispatch's :io Executor) runs on real OS
;; threads. go and go-loop
;; are NOT: they are defined below, because the pass that picks a park's
;; representation per site needs &env, macroexpand and resolve. This overlay
;; adds the portable dataflow operators — alts!, pipe, pipeline, split, reduce,
;; transduce, mult, mix, pub/sub, map, merge, and the deprecated map</map>/… —
;; ported from clojure.core.async over those primitives. Because go blocks are real
;; threads, parking ops are ordinary blocking ops and work anywhere; this is a
;; superset of the JVM model (no fixed thread pool, no pending-op limit).

(ns clojure.core.async
  (:refer-clojure :exclude [reduce transduce into merge map take partition partition-by bounded-count])
  ;; The port protocol the macros and any ported caller ask about, loaded here so
  ;; a bare (require 'clojure.core.async) installs it — upstream's ns form lists
  ;; it the same way. It requires nothing back, so there is no cycle.
  (:require [clojure.core.async.impl.protocols]))

;; --- go, and the cheap park -------------------------------------------------
;; go spawns its body on the backend *go-backend* names, read at spawn time.
;;
;; A fiber parks by capturing a continuation, and Chez represents that as a stack
;; segment that stays live for as long as the process is parked. A park the pass
;; below can SEE does not need one: it rewrites the rest of the body into a
;; closure, and __sm-take / __sm-put store the closure and switch to the
;; scheduler without capturing anything (host/chez/java/sm.ss).
;;
;; The choice is per PARK SITE, not per body. A park the pass cannot see or cannot
;; rewrite — inside a called function, inside a try, inside a nested fn, reached
;; through eval — is left exactly as it is and parks by capturing, which is what
;; every park does today. So the pass is opportunistic: it can cost a park its
;; cheap representation, never its correctness. When it has nothing to gain it
;; returns nil and go expands the way it always has.
;;
;; Not covered: alts! / alt! (threading a continuation through the waiter
;; registration in __do-alts is its own round — the macros below are over those
;; fns, so they exist and park by capturing), and any park inside a try (the
;; rewrite would have to carry the exception frame explicitly).

;; The park ops, by identity. Resolving the caller's symbol and comparing the VAR
;; is the only sound test: a user function named <! must be left alone. A macro
;; that expands to a park is invisible to the pre-scan, which costs it the cheap
;; park and nothing else.
;;
;; <!! and >!! are park ops too, but of their OWN kind: a Thread.interrupt throws
;; out of them and not out of <! / >! (async.ss ac-intr-wait!), so they rewrite to
;; __sm-take!! / __sm-put!!, whose cheap park carries the interrupt arm
;; (sm.ss jolt-sm-commit!/intr), and <! / >! keep the arm-less one.
(def ^:private sm-take-var #'clojure.core.async/<!)
(def ^:private sm-take!!-var #'clojure.core.async/<!!)
(def ^:private sm-put-var #'clojure.core.async/>!)
(def ^:private sm-put!!-var #'clojure.core.async/>!!)

;; park kind -> the op the pass emits for it
(def ^:private sm-park-ops
  {:take 'clojure.core.async/__sm-take
   :take!! 'clojure.core.async/__sm-take!!
   :put 'clojure.core.async/__sm-put
   :put!! 'clojure.core.async/__sm-put!!})

;; Forms the pass does not look inside. A park in one of them stays where it is;
;; a park-free one is emitted whole.
;;
;; This is clojure.core/special-syms minus the five heads the pass DOES rewrite
;; (do let* if loop* recur), plus defmacro, import* and syntax-quote. Listing the
;; rest exhaustively is the point: a special form that falls through to sm-cps's
;; :else arm is rebuilt as an ordinary application, which evaluates its subforms
;; as expressions — a silent miscompile. case* / deftype* / reify* are
;; unreachable today (jolt's `case` expands to let* + if, `reify` to a
;; (make-reified {...} "iface") call), so they are here to make the fallback true
;; by construction rather than by accident of how those macros expand.
;;
;; syntax-quote is the one head special-syms does not name and jolt still lowers
;; as a special form, because jolt's reader leaves a backtick as
;; (syntax-quote datum) for the analyzer while the JVM's reader expands it during
;; read. So the set to complement is jolt's analyzer `handled`, not the portable
;; special-syms, and dropping the difference cost `(go (list `(<! ch) :done))` its
;; meaning: the datum is data, but the :else arm rebuilt it as an application,
;; CPS'd the (<! ch) inside it into a real take, and syntax-quoted the resulting
;; gensym — so the channel was drained and the value came back (user/a__257 :done).
(def ^:private sm-opaque
  '#{quote syntax-quote try catch finally fn* letfn* set! def defmacro throw var
     monitor-enter monitor-exit new . & case* deftype* reify* import*})

;; recur inside one of these targets it, not an enclosing loop.
(def ^:private sm-recur-barrier '#{fn* loop* letfn*})

;; The two heads whose subforms are DATA. sm-children stops at both, so the
;; walkers never descend into a template.
(def ^:private sm-quoting '#{quote syntax-quote})

(defn- sm-bail []
  (throw (ex-info "sm/bail" {::bail true})))

(defn- sm-bail? [e] (boolean (::bail (ex-data e))))

(defn- sm-head [form] (when (seq? form) (first form)))

;; A head as the ANALYZER will read it. A special-form head may arrive
;; clojure.core-QUALIFIED and still dispatch to the special form: analyze-list*
;; builds its sf-name from either spelling, because syntax-quote qualifies a
;; macro like `letfn` whose expansion the analyzer lowers as a special form. So
;; (clojure.core/let* …) and (let* …) are the same form to the compiler, and this
;; pass has to agree — matching only the unqualified spelling drops a qualified
;; let* / try / quote / fn* / recur through to sm-cps's :else arm, where it is
;; rebuilt as an ordinary application and its subforms are evaluated as
;; expressions. That evaluates a binding vector, a catch clause, or a quoted
;; datum, and hoists a park out of a fn body — silently.
(defn- sm-sf-head
  [form]
  (let [h (sm-head form)]
    (if (and (symbol? h) (= "clojure.core" (namespace h))) (symbol (name h)) h)))

(defn- sm-children
  "Subforms to walk for a tree predicate. A quoted form has none, and neither
  does a syntax-quoted one: its subforms are DATA, and the walkers below
  macroexpand what they descend into, so walking a template ran the expander of
  every macro named inside a backtick — code the analyzer never expands and the
  compiler never sees. The predicates would only ever be conservative about it
  (sm-cps emits a syntax-quote whole through the sm-opaque arm either way), so
  nothing is lost by stopping, and an expander that throws or has side effects
  no longer gets to run on a quoted template."
  [form]
  (cond
    (contains? sm-quoting (sm-sf-head form)) nil
    (seq? form) (seq form)
    (vector? form) (seq form)
    (set? form) (seq form)
    (map? form) (concat (keys form) (vals form))
    :else nil))

(defn- sm-tree-any? [pred form]
  (or (pred form)
      (boolean (some (fn [c] (sm-tree-any? pred c)) (sm-children form)))))

;; :env IS the &env map — local symbol to nil, the shape analyzer.clj's
;; amp-env-map builds and the one both `resolve` and a macro expect. It is the
;; pass's only record of what is in scope: sm-bind extends it as continuation
;; parameters and let* bindings come into scope, so it stays true as the pass
;; descends, and sm-expand hands it to the expander so a macro sees the scope the
;; analyzer would have shown it.
(defn- sm-park-kind
  "nil, :take, :take!!, :put or :put!! — and only when sym resolves to the exact
  var. A local of the same name shadows it."
  [ctx sym]
  (when (and (symbol? sym) (not (contains? (:env ctx) sym)))
    (let [v (resolve (:env ctx) sym)]
      (cond
        (nil? v) nil
        (identical? v sm-take-var) :take
        (identical? v sm-take!!-var) :take!!
        (identical? v sm-put-var) :put
        (identical? v sm-put!!-var) :put!!
        :else nil))))

(defn- sm-parks? [ctx form]
  (sm-tree-any? (fn [f] (some? (sm-park-kind ctx (sm-head f)))) form))

;; UNDER THE LOCALS AS &env, which is not a nicety. sm-cps does not merely
;; classify with the expansion, it REBUILDS the body out of it — every one of the
;; do / let* / if / loop* / :else arms emits from `ex`. The one-argument
;; macroexpand has no env and binds &env to {}, so a macro that reads &env
;; expands one way here and another way when the analyzer reaches it, and the
;; program that gets compiled inside a park-bearing go body is not the one the
;; same source compiles to anywhere else. clojure.core/__macroexpand-env
;; (natives-reader.ss) is the seam that passes it; :env below is that map, kept
;; current by sm-bind as the pass introduces continuation parameters, so a macro
;; asking "is this name a local" gets the same answer the analyzer would give.
;;
;; A head that names a local is not expanded at all, whatever it resolves to.
(defn- sm-expand
  "One macroexpansion step chain, skipping a head that names a local."
  [ctx form]
  (if (and (seq? form)
           (symbol? (sm-head form))
           (not (contains? (:env ctx) (sm-head form)))
           (not (contains? sm-opaque (sm-sf-head form))))
    (clojure.core/__macroexpand-env form (:env ctx))
    form))

(defn- sm-targets-recur?
  "A recur that would rebind the loop this pass is rewriting. Expands as it walks:
  the barrier names are the SPECIAL forms (fn*/loop*/letfn*), so testing them
  against unexpanded source would miss `loop` and `fn` and mistake an inner loop's
  recur for this one's — which is a miscompile in either direction."
  [ctx form]
  (let [ex (sm-expand ctx form)
        h (sm-sf-head ex)]
    (cond
      (= h 'quote) false
      (= h 'recur) true
      (contains? sm-recur-barrier h) false
      :else (boolean (some (fn [c] (sm-targets-recur? ctx c)) (sm-children ex))))))

;; A form may be emitted whole only if it neither parks NOR carries a recur this
;; pass owns. The recur half is the trap: in
;;
;;   (loop [] (do (<! ch) (if p (recur) :done)))
;;
;; the tail is park-free, so emitting it whole looks right — but it would land
;; inside a continuation closure, where recur targets THAT fn instead of the loop.
;; Keeping such a form on the spine costs a couple of closures and reaches the
;; recur.
(defn- sm-inline-ok? [ctx form]
  (and (not (sm-parks? ctx form))
       (or (nil? (:rec ctx)) (not (sm-targets-recur? ctx form)))))

;; nil is the &env value for a local, matching analyzer.clj's amp-env-map — the
;; JVM maps a local to a compiler binding object, and the consumers that read
;; &env at all only look at its keys.
(defn- sm-bind [ctx sym] (update ctx :env assoc sym nil))

(defn- sm-bind* [ctx syms]
  (loop [c ctx s (seq syms)]
    (if s (recur (sm-bind c (first s)) (next s)) c)))

(declare sm-cps)

(defn- sm-kont
  "Bind (fn* [p] <body>) to a fresh name and hand that name to f. A continuation
  is always a symbol, so both arms of an if can mention it without duplicating
  its body."
  [ctx p bodyf f]
  (let [ks (gensym "k__")]
    (list 'let* [ks (list 'fn* [p] (bodyf (sm-bind (sm-bind ctx ks) p)))]
          (f (sm-bind ctx ks) ks))))

(defn- sm-cps-body
  "CPS a form sequence as an implicit do."
  [ctx forms k]
  (cond
    (empty? forms) (list k nil)
    (empty? (rest forms)) (sm-cps ctx (first forms) k)
    (sm-inline-ok? ctx (first forms))
    (list 'do (first forms) (sm-cps-body ctx (rest forms) k))
    :else
    (sm-kont ctx (gensym "v__")
             (fn [c] (sm-cps-body c (rest forms) k))
             (fn [c ks] (sm-cps c (first forms) ks)))))

(defn- sm-cps-seq
  "Evaluate forms left to right, binding each result to a symbol, then hand the
  symbols to f. Source order is preserved: a park-free form to the left of a
  parking one is bound BEFORE the park, not after it."
  [ctx forms f]
  (letfn [(step [c done todo]
            (if (empty? todo)
              (f c done)
              (let [a (first todo)
                    s (gensym "a__")]
                (if (sm-inline-ok? c a)
                  (list 'let* [s a] (step (sm-bind c s) (conj done s) (rest todo)))
                  (sm-kont c s
                           (fn [c2] (step c2 (conj done s) (rest todo)))
                           (fn [c2 ks] (sm-cps c2 a ks)))))))]
    (if (every? (fn [a] (sm-inline-ok? ctx a)) forms)
      (f ctx (vec forms))
      (step ctx [] forms))))

(defn- sm-cps-let
  "let* — a park-free init stays a binding; a parking one becomes a continuation
  parameter, which keeps the shadowing the source had."
  [ctx pairs body k]
  (if (empty? pairs)
    (sm-cps-body ctx body k)
    (let [b (first pairs)
          init (second pairs)
          more (drop 2 pairs)]
      (if (sm-inline-ok? ctx init)
        (list 'let* [b init] (sm-cps-let (sm-bind ctx b) more body k))
        (sm-kont ctx b
                 (fn [c] (sm-cps-let c more body k))
                 (fn [c ks] (sm-cps c init ks)))))))

(defn- sm-cps-loop
  "loop* becomes a letfn* function called in tail position, so a recur chain is a
  tail call and does not grow the stack across parks.

  The inits bind SEQUENTIALLY under the loop's own names, exactly as sm-cps-let
  does and as the analyzer reads loop* — analyze-bindings threads each binding
  into the env the next one is analyzed in, so (loop [a 1 b (inc a)] …) is legal
  and b's init means the a above it. Evaluating them as a flat argument list
  instead put every init in the scope OUTSIDE the loop, where that `a` is
  whatever `a` the enclosing scope happens to have: a compile error if there is
  none, and silently the wrong value if there is one.

  :rec carries the loop's TAIL continuation as well as its name and arity. A
  recur compiles to a bare call to lp, which throws away whatever continuation
  sm-cps was handed, and that is only the same thing as a recur where the
  continuation in hand IS this one — see the recur arm in sm-cps."
  [ctx bindings body k]
  (let [names (vec (take-nth 2 bindings))
        lp (gensym "lp__")]
    (letfn [(enter [c]
              (let [c' (assoc (sm-bind* (sm-bind c lp) names)
                              :rec {:name lp :n (count names) :k k})]
                (list 'letfn* [lp (list 'fn* names (sm-cps-body c' body k))]
                      ;; the names are the let*/continuation bindings built above;
                      ;; the fn's params shadow them inside the body
                      (apply list lp names))))
            (step [c pairs]
              (if (empty? pairs)
                (enter c)
                (let [b (first pairs)
                      init (second pairs)
                      more (drop 2 pairs)]
                  (if (sm-inline-ok? c init)
                    (list 'let* [b init] (step (sm-bind c b) more))
                    (sm-kont c b
                             (fn [c2] (step c2 more))
                             (fn [c2 ks] (sm-cps c2 init ks)))))))]
      (step ctx (seq bindings)))))

;; A test that PARKS becomes a continuation parameter; one that does not goes
;; straight into the if. Routing it through a continuation either way was correct
;; and cost a closure per if — and since when / cond / case / and / or / if-let
;; all lower to if, an if-heavy body paid it at every one. Nothing is duplicated
;; by inlining it: both arms name k, they do not carry it, so each is emitted
;; once whichever shape the test takes.
(defn- sm-cps-if
  [ctx form k]
  (let [t (second form)
        arms (drop 2 form)
        arm (fn [c ts]
              (list 'if ts
                    (sm-cps c (first arms) k)
                    (if (> (count arms) 1)
                      (sm-cps c (second arms) k)
                      (list k nil))))]
    (if (sm-inline-ok? ctx t)
      (arm ctx t)
      (let [ts (gensym "t__")]
        (sm-kont ctx ts
                 (fn [c] (arm c ts))
                 (fn [c ks] (sm-cps c t ks)))))))

(defn- sm-cps
  "Rewrite form so that its value is passed to the continuation named by k."
  [ctx form k]
  (if (sm-inline-ok? ctx form)
    (list k form)
    (if-not (seq? form)
      ;; A collection literal holding a park: left whole, parks the old way. Same
      ;; recur guard as the opaque arm below, and for the same reason — emitted
      ;; whole, a recur this pass owns lands inside a continuation fn* and targets
      ;; THAT fn instead of the loop. Only reachable from source the JVM rejects (a
      ;; recur outside tail position), which is exactly why it is a check and not a
      ;; comment: the pass may cost a park its cheap representation, never its
      ;; meaning, and that should hold by construction.
      (if (and (:rec ctx) (sm-targets-recur? ctx form)) (sm-bail) (list k form))
      ;; h is the head as WRITTEN (what a park test and a rebuilt application need)
      ;; and sf is the same head as the analyzer reads it — the two differ only for
      ;; a clojure.core-qualified special form, and every dispatch below turns on
      ;; sf so that spelling cannot slip past into the :else arm.
      (let [ex (sm-expand ctx form)
            h (sm-head ex)
            sf (sm-sf-head ex)
            pk (sm-park-kind ctx h)]
        (cond
          pk
          (sm-cps-seq ctx (vec (rest ex))
                      (fn [_ args]
                        (apply list
                               (sm-park-ops pk)
                               (concat args [k]))))

          (= sf 'do) (sm-cps-body ctx (rest ex) k)
          (= sf 'let*) (sm-cps-let ctx (vec (second ex)) (drop 2 ex) k)
          (= sf 'if) (sm-cps-if ctx ex k)
          (= sf 'loop*) (sm-cps-loop ctx (vec (second ex)) (drop 2 ex) k)

          ;; recur is a bare call to the loop fn, which DISCARDS k. That is the
          ;; right thing only where k is the loop's own tail continuation, and
          ;; every arm that CPSes a subform hands down a fresh one instead: an
          ;; application argument, a let* init, a non-final do statement, an if
          ;; test. Reached through any of those, discarding k threw the rest of
          ;; the computation away — (loop [i 0] (if (< i 2) (inc (recur (inc i)))
          ;; (<! ch))) answered the take where the ordinary expansion answers it
          ;; plus two. Only source the JVM rejects (a recur outside tail
          ;; position) gets here, which is why nothing caught it; jolt accepts
          ;; that source, so the two expansions have to agree on what it means.
          ;; Bail, the same as the collection-literal and opaque arms do for the
          ;; same reason.
          (= sf 'recur)
          (let [rec (:rec ctx)]
            (when (or (nil? rec)
                      (not= (count (rest ex)) (:n rec))
                      (not= k (:k rec)))
              (sm-bail))
            (sm-cps-seq ctx (vec (rest ex))
                        (fn [_ args] (apply list (:name rec) args))))

          ;; A form this pass does not rewrite. Emitting it whole is correct: a
          ;; park inside it captures a continuation, and that capture includes the
          ;; pending (k _) frame, so the resume carries on properly.
          (or (contains? sm-opaque sf) (not (symbol? h)))
          (if (and (:rec ctx) (sm-targets-recur? ctx ex)) (sm-bail) (list k form))

          :else
          (sm-cps-seq ctx (vec (rest ex))
                      (fn [_ args] (list k (apply list h args)))))))))

(defn- sm-cps-go-body
  "The go body rewritten as (fn* [k] ...), or nil to compile it the way it always
  was. nil means: nothing here parks where the pass can see it, or the body did
  something the pass will not guess at."
  [env body]
  ;; The scope starts from the ENCLOSING one, not empty, and it is the go macro's
  ;; own &env — so a `go` written inside a scope whose local shares a name with a
  ;; macro does not have that local's call expanded as the macro, and a macro the
  ;; pass expands sees the same locals the analyzer would have shown it.
  ;; (or env {}) because a top-level go gets nil, and jolt-resolve treats a
  ;; non-map env as "no locals" — the same answer, said once here instead of at
  ;; every use.
  (let [ctx {:env (or env {}) :rec nil}
        form (if (= 1 (count body)) (first body) (cons 'do body))]
    (when (and (sm-parks? ctx form)
               ;; a recur in the body targets the body fn itself, whose arity this
               ;; pass changes
               (not (sm-targets-recur? ctx form)))
      (try
        (let [k (gensym "k__")]
          (list 'fn* [k] (sm-cps (sm-bind ctx k) form k)))
        (catch Throwable e
          (when-not (sm-bail? e) (throw e))
          nil)))))

(defmacro go
  "Spawn body as a process and return a channel carrying its value. Parking ops
  inside the body — including ones in functions it calls — park the process."
  [& body]
  (if-let [f (sm-cps-go-body &env body)]
    (list 'clojure.core.async/__sm-spawn f)
    (list 'clojure.core.async/go-spawn (list* 'fn* [] body))))

(defmacro go-loop
  "(go (loop bindings body...))"
  [bindings & body]
  (list 'clojure.core.async/go (list* 'clojure.core/loop bindings body)))

;; --- alts -------------------------------------------------------------------
;; do-alts uses a per-call handler registered on each channel (no poll loop).
;; The __do-alts host primitive handles the fast pass, registration, wait, and
;; unregistration atomically under per-channel locks.

(defn- alt-attempt [port]
  (if (vector? port)
    (let [ch (nth port 0) v (nth port 1)]
      (assert (some? v) "Can't put nil on channel")
      (let [r (clojure.core.async/__offer! ch v)]
        (when (some? r) [r ch])))
    (let [r (clojure.core.async/__poll! port)]
      (when (not= r ::none) [r port]))))

(defn- do-alts*
  [ports opts blocking?]
  (assert (pos? (count ports)) "alts must have at least one channel operation")
  (let [ports (vec ports)
        n (count ports)
        has-default (contains? opts :default)]
    ;; Validate every put before the readiness scan can mutate any channel.
    (dotimes [i n]
      (let [port (nth ports i)]
        (when (vector? port)
          (assert (some? (nth port 1)) "Can't put nil on channel"))))
    ;; one fast non-blocking scan for :default support
    (let [start (if (:priority opts) 0 (rand-int n))
          hit (loop [k 0]
                (when (< k n)
                  (let [j (+ start k) i (if (< j n) j (- j n))]
                    (or (alt-attempt (nth ports i))
                        (recur (inc k))))))]
      (if hit
        hit
        (if has-default
          [(:default opts) :default]
          (clojure.core.async/__do-alts ports (boolean (:priority opts)) blocking?))))))

(defn do-alts
  "Returns [val port] for the first ready op among ports. ports is a vector of
  take ports and/or [channel val] put specs. opts may include :priority true
  (try in order) and :default val (return [val :default] if none ready)."
  [ports opts]
  (do-alts* ports opts false))

;; alts!! is the blocking one, and the only one a Thread.interrupt reaches: it
;; throws InterruptedException out of the wait with no op completed, as the JVM's
;; promise deref does. alts! parks, and a parked go block holds no thread.
(defn alts!!
  "Completes at most one of several channel operations. ports is a vector of take
  ports and/or [channel val] put specs. Returns [val port]. Blocks until ready."
  [ports & {:as opts}]
  (do-alts* ports opts true))

(defn alts!
  "Like alts!!. Parking and blocking alts are the same operation in jolt: on a
  thread-backed go both block, and on a fiber both park. Only alts!! is
  interruptible."
  [ports & {:as opts}]
  (do-alts ports opts))

;; --- alt! / alt!! -----------------------------------------------------------
;; Upstream's expansion for the two macros, over the alts! above. The pass that
;; gives a park its cheap representation does not thread a continuation through
;; __do-alts, so an alt! parks by capturing — correct, just not cheap, the same
;; fallback every park the pass cannot see takes.
;;
;; The clojure.core/ prefixes are load-bearing, not style: this namespace
;; :refer-clojure :excludes reduce, map and partition, so a bare `reduce` here is
;; clojure.core.async/reduce, over channels. Upstream's file carries the same
;; exclusions and spells them the same way. Nothing else do-alt calls is shadowed.
(defn do-alt [alts clauses]
  (assert (even? (count clauses)) "unbalanced clauses")
  (let [clauses (clojure.core/partition 2 clauses)
        opt? #(keyword? (first %))
        opts (filter opt? clauses)
        clauses (remove opt? clauses)
        [clauses bindings]
        (clojure.core/reduce
         (fn [[clauses bindings] [ports expr]]
           (let [ports (if (vector? ports) ports [ports])
                 [ports bindings]
                 (clojure.core/reduce
                  (fn [[ports bindings] port]
                    (if (vector? port)
                      (let [[port val] port
                            gp (gensym)
                            gv (gensym)]
                        [(conj ports [gp gv]) (conj bindings [gp port] [gv val])])
                      (let [gp (gensym)]
                        [(conj ports gp) (conj bindings [gp port])])))
                  [[] bindings] ports)]
             [(conj clauses [ports expr]) bindings]))
         [[] []] clauses)
        gch (gensym "ch")
        gret (gensym "ret")]
    `(let [~@(mapcat identity bindings)
           [val# ~gch :as ~gret] (~alts [~@(apply concat (clojure.core/map first clauses))]
                                        ~@(apply concat opts))]
       (cond
         ~@(mapcat (fn [[ports expr]]
                     [`(or ~@(clojure.core/map (fn [port]
                                                 `(= ~gch ~(if (vector? port) (first port) port)))
                                               ports))
                      (if (and (seq? expr) (vector? (first expr)))
                        `(let [~(first expr) ~gret] ~@(rest expr))
                        expr)])
                   clauses)
         (= ~gch :default) val#))))

(defmacro alt!!
  "Like alt!, except as if by alts!!, will block until completed, and
  not intended for use in (go ...) blocks."

  [& clauses]
  (do-alt `alts!! clauses))

(defmacro alt!
  "Makes a single choice between one of several channel operations,
  as if by alts!, returning the value of the result expr corresponding
  to the operation completed. Must be called inside a (go ...) block.

  Each clause takes the form of:

  channel-op[s] result-expr

  where channel-ops is one of:

  take-port - a single port to take
  [take-port | [put-port put-val] ...] - a vector of ports as per alts!
  :default | :priority - an option for alts!

  and result-expr is either a list beginning with a vector, whereupon that
  vector will be treated as a binding for the [val port] return of the
  operation, else any other expression.

  (alt!
    [c t] ([val ch] (foo ch val))
    x ([v] v)
    [[out val]] :wrote
    :default 42)

  Each option may appear at most once. The choice and parking
  characteristics are those of alts!."

  [& clauses]
  (do-alt `alts! clauses))

(defn poll!
  "Takes a val from port if possible immediately. Never blocks. Returns the value
  or nil."
  [port]
  (let [r (clojure.core.async/__poll! port)]
    (when (not= r ::none) r)))

;; --- thread variants --------------------------------------------------------

;; Three carriers, three names, no options — the choice is the call you write:
;; thread is a real OS thread, io-thread is a fiber, go is the CPS pass on
;; whatever *go-backend* names. So workload is not a hint here, it selects:
;;
;;   :io       a FIBER. Cheap enough to have thousands of, and a park releases
;;             the carrier — which is what the JVM buys with a virtual thread.
;;   :mixed    a real OS thread (the default, as on the JVM).
;;   :compute  a real OS thread.
;;
;; :compute is a thread rather than a fiber deliberately, even though preemption
;; means a compute-bound fiber no longer starves its carrier's queue: fibers are
;; capped at the carrier count, so N compute bodies on N carriers leave nothing to
;; run the io-thread bodies that are ready. A thread per compute body is what the
;; caller asked for by saying :compute.
;;
;; What a fiber does NOT get is an OS thread of its own, so a body that blocks
;; SOMEWHERE THE RUNTIME DOES NOT KNOW ABOUT pins its carrier for the duration.
;; Channel ops park, and so do jolt.socket's reads and writes and jolt.process's
;; subprocess pipe reads and writes (R8: an EAGAIN registers with the io-poller
;; and parks), and Thread/sleep. A blocking read on a raw fd you opened yourself and
;; an FFI call that blocks do not — they hold the carrier's OS thread. Use thread
;; for those; that is what it is for.
(defn thread-call
  "Executes f elsewhere, returning a channel that receives f's result then closes.
  workload says what f does and picks the carrier: :io runs f on a fiber
  (blocking-shaped I/O, parks instead of holding a thread), :mixed (the default)
  and :compute run it on a real OS thread. No workload honors *go-backend* —
  unlike go/go-loop, thread-call's carrier is fixed by the call site."
  ([f] (thread-call f :mixed))
  ([f workload]
   (case workload
     :io (clojure.core.async/fiber-spawn f)
     (:mixed :compute) (clojure.core.async/thread-spawn f)
     (throw (IllegalArgumentException.
              (str "Invalid workload " (pr-str workload)
                   " — expected :io, :compute or :mixed"))))))

(defmacro io-thread
  "Executes body on a FIBER, returning a channel that receives the result then
  closes. For blocking-shaped I/O: a channel op or a jolt.socket read that would
  block parks the fiber and frees its carrier for other work, so thousands of
  these cost thousands of stacks rather than thousands of OS threads. A park works
  anywhere in the body — inside a called function, a try, a loop — because a fiber
  parks by capturing its continuation, not by being rewritten.

  Use thread instead when the body blocks in a way the runtime cannot see
  (a raw fd read, a blocking FFI call): that pins the fiber's
  carrier, and a real OS thread is the escape."
  [& body]
  `(thread-call (fn [] ~@body) :io))

;; --- pipe / pipeline --------------------------------------------------------

(defn pipe
  "Takes elements from the from channel and supplies them to the to channel.
  Closes to when from closes unless close? is false."
  ([from to] (pipe from to true))
  ([from to close?]
   (go-loop []
     (let [v (<! from)]
       (if (nil? v)
         (when close? (close! to))
         (when (>! to v)
           (recur)))))
   to))

(defn- pipeline*
  [n to xf from close? ex-handler type]
  (assert (pos? n))
  (let [jobs (chan n)
        results (chan n)
        process (fn [job]
                  (if (nil? job)
                    (do (close! results) nil)
                    (let [v (nth job 0) p (nth job 1)
                          res (chan 1 xf ex-handler)]
                      (>!! res v)
                      (close! res)
                      (put! p res)
                      true)))
        afn (fn [job]
              (if (nil? job)
                (do (close! results) nil)
                (let [v (nth job 0) p (nth job 1)
                      res (chan 1)]
                  (xf v res)
                  (put! p res)
                  true)))]
    (dotimes [_ n]
      (case type
        (:blocking :compute) (thread
                               (loop []
                                 (let [job (<!! jobs)]
                                   (when (process job)
                                     (recur)))))
        :async (go-loop []
                 (let [job (<! jobs)]
                   (when (afn job)
                     (recur))))))
    (go-loop []
      (let [v (<! from)]
        (if (nil? v)
          (close! jobs)
          (let [p (chan 1)]
            (>! jobs [v p])
            (>! results p)
            (recur)))))
    (go-loop []
      (let [p (<! results)]
        (if (nil? p)
          (when close? (close! to))
          (let [res (<! p)]
            (loop []
              (let [v (<! res)]
                (when (and (not (nil? v)) (>! to v))
                  (recur))))
            (recur)))))))

(defn pipeline
  "Takes elements from from, applies transducer xf with parallelism n, supplies to
  to. Outputs are ordered relative to inputs."
  ([n to xf from] (pipeline n to xf from true))
  ([n to xf from close?] (pipeline n to xf from close? nil))
  ([n to xf from close? ex-handler] (pipeline* n to xf from close? ex-handler :compute)))

(defn pipeline-blocking
  "Like pipeline, for blocking operations."
  ([n to xf from] (pipeline-blocking n to xf from true))
  ([n to xf from close?] (pipeline-blocking n to xf from close? nil))
  ([n to xf from close? ex-handler] (pipeline* n to xf from close? ex-handler :blocking)))

(defn pipeline-async
  "Like pipeline, for async fns af of two args [input result-channel]."
  ([n to af from] (pipeline-async n to af from true))
  ([n to af from close?] (pipeline* n to af from close? nil :async)))

(defn split
  "Splits ch by predicate p into [true-chan false-chan]."
  ([p ch] (split p ch nil nil))
  ([p ch t-buf-or-n f-buf-or-n]
   (let [tc (chan t-buf-or-n)
         fc (chan f-buf-or-n)]
     (go-loop []
       (let [v (<! ch)]
         (if (nil? v)
           (do (close! tc) (close! fc))
           (when (>! (if (p v) tc fc) v)
             (recur)))))
     [tc fc])))

;; --- reduce / transduce / collection sinks ----------------------------------

(defn reduce
  "Returns a channel with the single result of reducing ch with f from init."
  [f init ch]
  (go-loop [ret init]
    (let [v (<! ch)]
      (if (nil? v)
        ret
        (let [ret' (f ret v)]
          (if (reduced? ret')
            @ret'
            (recur ret')))))))

(defn transduce
  "async/reduces ch with the transformation (xform f), returning a channel with the
  result."
  [xform f init ch]
  (let [f (xform f)]
    (go
      (let [ret (<! (reduce f init ch))]
        (f ret)))))

(defn- bounded-count [n coll]
  (if (counted? coll)
    (min n (count coll))
    (loop [i 0 s (seq coll)]
      (if (and s (< i n))
        (recur (inc i) (next s))
        i))))

(defn onto-chan!
  "Puts the contents of coll into ch, closing ch after unless close? is false.
  Returns a channel that closes when done."
  ([ch coll] (onto-chan! ch coll true))
  ([ch coll close?]
   (go-loop [vs (seq coll)]
     (if (and vs (>! ch (first vs)))
       (recur (next vs))
       (when close?
         (close! ch))))))

(defn to-chan!
  "Returns a channel containing the contents of coll, closing when exhausted."
  [coll]
  (let [c (bounded-count 100 coll)]
    (if (pos? c)
      (let [ch (chan c)]
        (onto-chan! ch coll)
        ch)
      (let [ch (chan)]
        (close! ch)
        ch))))

(defn onto-chan!!
  "Like onto-chan! for use when accessing coll might block."
  ([ch coll] (onto-chan!! ch coll true))
  ([ch coll close?]
   (thread
     (loop [vs (seq coll)]
       (if (and vs (>!! ch (first vs)))
         (recur (next vs))
         (when close?
           (close! ch)))))))

(defn to-chan!!
  "Like to-chan! for use when accessing coll might block."
  [coll]
  (let [c (bounded-count 100 coll)]
    (if (pos? c)
      (let [ch (chan c)]
        (onto-chan!! ch coll)
        ch)
      (let [ch (chan)]
        (close! ch)
        ch))))

(defn onto-chan
  "Deprecated - use onto-chan! or onto-chan!!"
  ([ch coll] (onto-chan! ch coll true))
  ([ch coll close?] (onto-chan! ch coll close?)))

(defn to-chan
  "Deprecated - use to-chan! or to-chan!!"
  [coll]
  (to-chan! coll))

(defn into
  "Returns a channel with the single collection result of conjoining items from ch
  onto coll. ch must close first."
  [coll ch]
  (reduce conj coll ch))

(defn take
  "Returns a channel that returns at most n items from ch, then closes."
  ([n ch] (take n ch nil))
  ([n ch buf-or-n]
   (let [out (chan buf-or-n)]
     (go (loop [x 0]
           (when (< x n)
             (let [v (<! ch)]
               (when (not (nil? v))
                 (>! out v)
                 (recur (inc x))))))
         (close! out))
     out)))

;; --- mult / tap -------------------------------------------------------------

(defprotocol Mux
  (muxch* [_]))

(defprotocol Mult
  (tap* [m ch close?])
  (untap* [m ch])
  (untap-all* [m]))

(defn mult
  "Creates a mult of ch. Copies can be created with tap and removed with untap.
  Each item is distributed to all taps synchronously."
  [ch]
  (let [cs (atom {})
        m (reify
            Mux
            (muxch* [_] ch)
            Mult
            (tap* [_ ch close?] (swap! cs assoc ch close?) nil)
            (untap* [_ ch] (swap! cs dissoc ch) nil)
            (untap-all* [_] (reset! cs {}) nil))
        dchan (chan 1)
        dctr (atom nil)
        done (fn [_] (when (zero? (swap! dctr dec))
                       (put! dchan true)))]
    (go-loop []
      (let [val (<! ch)]
        (if (nil? val)
          (doseq [[c close?] @cs]
            (when close? (close! c)))
          (let [chs (keys @cs)]
            (reset! dctr (count chs))
            (doseq [c chs]
              (when-not (put! c val done)
                (untap* m c)))
            (when (seq chs)
              (<! dchan))
            (recur)))))
    m))

(defn tap
  "Copies the mult source onto ch. Closes ch when the source closes unless close?
  is false."
  ([mult ch] (tap mult ch true))
  ([mult ch close?] (tap* mult ch close?) ch))

(defn untap
  "Disconnects ch from a mult."
  [mult ch]
  (untap* mult ch))

(defn untap-all
  "Disconnects all channels from a mult."
  [mult]
  (untap-all* mult))

;; --- mix --------------------------------------------------------------------

(defprotocol Mix
  (admix* [m ch])
  (unmix* [m ch])
  (unmix-all* [m])
  (toggle* [m state-map])
  (solo-mode* [m mode]))

(defn mix
  "Creates a mix of input channels put onto out. Inputs are added with admix,
  removed with unmix, and toggled (:mute/:pause/:solo) with toggle."
  [out]
  (let [cs (atom {})
        solo-modes #{:mute :pause}
        solo-mode (atom :mute)
        change (chan (sliding-buffer 1))
        changed #(put! change true)
        pick (fn [attr chs]
               (reduce-kv
                (fn [ret c v]
                  (if (attr v) (conj ret c) ret))
                #{} chs))
        calc-state (fn []
                     (let [chs @cs
                           mode @solo-mode
                           solos (pick :solo chs)
                           pauses (pick :pause chs)]
                       {:solos solos
                        :mutes (pick :mute chs)
                        :reads (conj
                                (if (and (= mode :pause) (seq solos))
                                  (vec solos)
                                  (vec (remove pauses (keys chs))))
                                change)}))
        m (reify
            Mux
            (muxch* [_] out)
            Mix
            (admix* [_ ch] (swap! cs assoc ch {}) (changed))
            (unmix* [_ ch] (swap! cs dissoc ch) (changed))
            (unmix-all* [_] (reset! cs {}) (changed))
            (toggle* [_ state-map] (swap! cs (partial merge-with clojure.core/merge) state-map) (changed))
            (solo-mode* [_ mode]
              (assert (solo-modes mode) (str "mode must be one of: " solo-modes))
              (reset! solo-mode mode)
              (changed)))]
    (go-loop [state (calc-state)]
      (let [{:keys [solos mutes reads]} state
            [v c] (alts! reads)]
        (if (or (nil? v) (= c change))
          (do (when (nil? v)
                (swap! cs dissoc c))
              (recur (calc-state)))
          (if (or (solos c)
                  (and (empty? solos) (not (mutes c))))
            (when (>! out v)
              (recur state))
            (recur state)))))
    m))

(defn admix
  "Adds ch as an input to the mix."
  [mix ch]
  (admix* mix ch))

(defn unmix
  "Removes ch as an input to the mix."
  [mix ch]
  (unmix* mix ch))

(defn unmix-all
  "Removes all inputs from the mix."
  [mix]
  (unmix-all* mix))

(defn toggle
  "Atomically sets the state of one or more channels in a mix."
  [mix state-map]
  (toggle* mix state-map))

(defn solo-mode
  "Sets the solo mode of the mix (:mute or :pause)."
  [mix mode]
  (solo-mode* mix mode))

;; --- pub / sub --------------------------------------------------------------

(defprotocol Pub
  (sub* [p v ch close?])
  (unsub* [p v ch])
  (unsub-all* [p] [p v]))

(defn pub
  "Creates a pub of ch partitioned by topic-fn. Subscribe with sub."
  ([ch topic-fn] (pub ch topic-fn (constantly nil)))
  ([ch topic-fn buf-fn]
   (let [mults (atom {})
         ensure-mult (fn [topic]
                       (or (get @mults topic)
                           (get (swap! mults
                                       #(if (% topic) % (assoc % topic (mult (chan (buf-fn topic))))))
                                topic)))
         p (reify
             Mux
             (muxch* [_] ch)
             Pub
             (sub* [_p topic ch close?]
               (let [m (ensure-mult topic)]
                 (tap m ch close?)))
             (unsub* [_p topic ch]
               (when-let [m (get @mults topic)]
                 (untap m ch)))
             (unsub-all* [_] (reset! mults {}))
             (unsub-all* [_ topic] (swap! mults dissoc topic)))]
     (go-loop []
       (let [val (<! ch)]
         (if (nil? val)
           (doseq [m (vals @mults)]
             (close! (muxch* m)))
           (let [topic (topic-fn val)
                 m (get @mults topic)]
             (when m
               (when-not (>! (muxch* m) val)
                 (swap! mults dissoc topic)))
             (recur)))))
     p)))

(defn sub
  "Subscribes ch to a topic of pub p."
  ([p topic ch] (sub p topic ch true))
  ([p topic ch close?] (sub* p topic ch close?)))

(defn unsub
  "Unsubscribes ch from a topic of pub p."
  [p topic ch]
  (unsub* p topic ch))

(defn unsub-all
  "Unsubscribes all channels from a pub, or from a topic."
  ([p] (unsub-all* p))
  ([p topic] (unsub-all* p topic)))

;; --- map / merge ------------------------------------------------------------

(defn map
  "Applies f to the set of first items from each source channel, then second, etc.
  Closes the output channel when any source closes."
  ([f chs] (map f chs nil))
  ([f chs buf-or-n]
   (let [chs (vec chs)
         out (chan buf-or-n)
         cnt (count chs)
         rets (atom (vec (repeat cnt nil)))
         dchan (chan 1)
         dctr (atom nil)
         done (mapv (fn [i]
                      (fn [ret]
                        (swap! rets assoc i ret)
                        (when (zero? (swap! dctr dec))
                          (put! dchan @rets))))
                    (range cnt))]
     (if (zero? cnt)
       (close! out)
       (go-loop []
         (reset! dctr cnt)
         (dotimes [i cnt]
           (take! (nth chs i) (nth done i)))
         (let [rets (<! dchan)]
           (if (some nil? rets)
             (close! out)
             (do (>! out (apply f rets))
                 (recur))))))
     out)))

(defn merge
  "Returns a channel with all values taken from the source channels chs. Closes
  after all sources close."
  ([chs] (merge chs nil))
  ([chs buf-or-n]
   (let [out (chan buf-or-n)]
     (go-loop [cs (vec chs)]
       (if (pos? (count cs))
         (let [[v c] (alts! cs)]
           (if (nil? v)
             (recur (filterv #(not= c %) cs))
             (do (>! out v)
                 (recur cs))))
         (close! out)))
     out)))

;; --- deprecated channel ops (rewritten as go-loops) -------------------------

(defn map<
  "Deprecated - use a transducer. Returns a read-side channel mapping f over ch."
  [f ch]
  (let [out (chan)]
    (go-loop []
      (let [v (<! ch)]
        (if (nil? v) (close! out) (do (>! out (f v)) (recur)))))
    out))

(defn map>
  "Deprecated - use a transducer. Returns a write-side channel mapping f into out."
  [f out]
  (let [in (chan)]
    (go-loop []
      (let [v (<! in)]
        (if (nil? v) (close! out) (do (>! out (f v)) (recur)))))
    in))

(defn filter<
  "Deprecated - use a transducer."
  ([p ch] (filter< p ch nil))
  ([p ch buf-or-n]
   (let [out (chan buf-or-n)]
     (go-loop []
       (let [val (<! ch)]
         (if (nil? val)
           (close! out)
           (do (when (p val) (>! out val))
               (recur)))))
     out)))

(defn remove<
  "Deprecated - use a transducer."
  ([p ch] (remove< p ch nil))
  ([p ch buf-or-n] (filter< (complement p) ch buf-or-n)))

(defn filter>
  "Deprecated - use a transducer."
  [p out]
  (let [in (chan)]
    (go-loop []
      (let [v (<! in)]
        (if (nil? v)
          (close! out)
          (do (when (p v) (>! out v))
              (recur)))))
    in))

(defn remove>
  "Deprecated - use a transducer."
  [p out]
  (filter> (complement p) out))

(defn- mapcat* [f in out]
  (go-loop []
    (let [val (<! in)]
      (if (nil? val)
        (close! out)
        (do (doseq [v (f val)]
              (>! out v))
            (recur))))))

(defn mapcat<
  "Deprecated - use a transducer."
  ([f in] (mapcat< f in nil))
  ([f in buf-or-n]
   (let [out (chan buf-or-n)]
     (mapcat* f in out)
     out)))

(defn mapcat>
  "Deprecated - use a transducer."
  ([f out] (mapcat> f out nil))
  ([f out buf-or-n]
   (let [in (chan buf-or-n)]
     (mapcat* f in out)
     in)))

(defn unique
  "Deprecated - use a transducer. Drops consecutive duplicates."
  ([ch] (unique ch nil))
  ([ch buf-or-n]
   (let [out (chan buf-or-n)]
     (go (loop [last nil]
           (let [v (<! ch)]
             (when (not (nil? v))
               (if (= v last)
                 (recur last)
                 (do (>! out v)
                     (recur v))))))
         (close! out))
     out)))

(defn partition
  "Deprecated - use a transducer. Partitions ch into vectors of n."
  ([n ch] (partition n ch nil))
  ([n ch buf-or-n]
   (let [out (chan buf-or-n)]
     (go-loop [arr [] idx 0]
       (let [v (<! ch)]
         (if (not (nil? v))
           (let [arr (conj arr v) new-idx (inc idx)]
             (if (< new-idx n)
               (recur arr new-idx)
               (do (>! out arr) (recur [] 0))))
           (do (when (> idx 0) (>! out arr))
               (close! out)))))
     out)))

(defn partition-by
  "Deprecated - use a transducer. Partitions ch by runs of (f v)."
  ([f ch] (partition-by f ch nil))
  ([f ch buf-or-n]
   (let [out (chan buf-or-n)]
     (go-loop [lst [] last ::nothing]
       (let [v (<! ch)]
         (if (not (nil? v))
           (let [new-itm (f v)]
             (if (or (= new-itm last) (identical? last ::nothing))
               (recur (conj lst v) new-itm)
               (do (>! out lst) (recur [v] new-itm))))
           (do (when (> (count lst) 0) (>! out lst))
               (close! out)))))
     out)))
