(ns jolt.passes
  "IR optimization passes (nanopass-lite) + the inference/checking
  driver. Façade over three weakly-coupled namespaces, loaded with the compiler:

    jolt.passes.fold    — const-fold (always-on) + the shared const-shape predicate.
    jolt.passes.inline  — inline + flatten-lets + scalar-replace (direct-link only).
    jolt.passes.types   — collection-type inference + success-type checking
                          (RFC 0006) + the inter-procedural driver API.

  run-passes (below) is the single entry the back end applies to every analyzed
  form. The driver/checker fns the back end looks up by name (check-form,
  infer-body, reinfer-def, set-rtenv!, take-diags!, …) are re-exported here via
  :refer, so jolt.passes stays the only namespace the back end imports.

  Portable Clojure: kernel-tier fns + seed primitives only."
  (:require [jolt.host :refer [inline-enabled? record-shapes protocol-methods stash-inline!]]
            [jolt.passes.fold :refer [const-fold]]
            [jolt.passes.numeric :as numeric]
            [jolt.passes.inline :refer [inline-node flatten-lets scalar-replace dirty set-rec-shapes!]]
            [jolt.passes.types :refer [run-inference
                                       check-form infer-body reinfer-def phint-seed
                                       set-rtenv! set-vtypes! join-types
                                       set-record-shapes! set-map-shapes! set-protocol-methods!
                                       reset-escapes! collected-escapes
                                       wp-infer! param-seeds-for
                                       set-check-mode! take-diags!]]))

;; Cap on inline -> flatten -> scalar-replace -> const-fold iterations. Each pass
;; sets `dirty` when it rewrote something; the loop stops at a clean pass or here.
(def ^:private inline-fixpoint-cap 8)

;; A top-level defn the inline pass may splice: a single fixed arity (no rest). The
;; pass itself checks body size + closedness, so any such fn is stashable.
(defn- inline-eligible? [node]
  (and (= :def (:op node)) (:init node) (= :fn (:op (:init node)))
       (= 1 (count (:arities (:init node))))
       (not (:rest (first (:arities (:init node)))))))

(defn- stash-of [node]
  (let [a (first (:arities (:init node)))]
    {:params (:params a) :body (:body a) :nhints (:nhints a) :ret (:ret-nhint a)}))

(defn run-passes
  "All passes, in order. The back end applies this to every analyzed form. When
  inlining is enabled for the unit (user code under direct-linking),
  run inline + flatten + scalar-replace + const-fold to a capped fixpoint —
  inlining exposes map literals to lookups, scalar-replace collapses them, which
  may expose more — then a collection-type inference pass (optionally
  also emitting success diagnostics) that auto-drops the lookup guard where the
  type is proven. Otherwise (core + bootstrap) just const-fold, as before.

  numeric/annotate runs last in both branches (hint-directed fl*/fx* arithmetic);
  it benefits open builds too, so it is not gated on inlining."
  [node ctx]
  ;; stash an inline-eligible defn so later call sites can splice it (closed-world
  ;; optimization only). Done before optimizing, from the analyzed node.
  (when (and (inline-enabled? ctx) (inline-eligible? node))
    (stash-inline! ctx (:ns node) (:name node) (stash-of node)))
  (numeric/annotate
    (if (inline-enabled? ctx)
      (let [_ (set-rec-shapes! (record-shapes ctx))   ;; record ctor fold
            ;; resolve ^Record param hints (incl. defrecord/extend-type method
            ;; `this`) to bare field reads per-form, not only under whole-program.
            ;; Same shapes the inline pass uses.
            _ (set-record-shapes! (record-shapes ctx))
            _ (set-protocol-methods! (protocol-methods ctx))  ;; devirtualization
            opt (loop [i 0 n (const-fold node)]
                  (reset! dirty false)
                  (let [n2 (const-fold (scalar-replace (flatten-lets (inline-node n ctx))))]
                    (if (and @dirty (< i inline-fixpoint-cap))
                      (recur (inc i) n2)
                      n2)))
            ;; a top-level def whose params the whole-program fixpoint typed gets
            ;; reinferred with those seeds (record types flow in from its callers);
            ;; everything else takes the ordinary per-form inference.
            seeds (when (= :def (:op opt)) (param-seeds-for (str (:ns opt) "/" (:name opt))))]
        ;; a final const-fold after inference propagates any predicate folded to a
        ;; constant, collapsing the `if` it gates to the taken branch.
        (const-fold (if seeds (reinfer-def opt seeds) (run-inference opt))))
      (const-fold node))))
