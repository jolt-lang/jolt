(ns jolt.passes
  "IR optimization passes (nanopass-lite, jolt-2om) + the inference/checking
  driver. Façade over three weakly-coupled namespaces, loaded with the compiler:

    jolt.passes.fold    — const-fold (always-on) + the shared const-shape predicate.
    jolt.passes.inline  — inline + flatten-lets + scalar-replace (direct-link only).
    jolt.passes.types   — collection-type inference + success-type checking
                          (RFC 0006) + the inter-procedural driver API (jolt-767).

  run-passes (below) is the single entry the back end applies to every analyzed
  form. The driver/checker fns the back end looks up by name (check-form,
  infer-body, reinfer-def, set-rtenv!, take-diags!, …) are re-exported here via
  :refer, so jolt.passes stays the only namespace the back end imports.

  Portable Clojure: kernel-tier fns + seed primitives only."
  (:require [jolt.host :refer [inline-enabled? record-shapes]]
            [jolt.passes.fold :refer [const-fold]]
            [jolt.passes.inline :refer [inline-node flatten-lets scalar-replace dirty set-rec-shapes!]]
            [jolt.passes.types :refer [run-inference
                                       check-form infer-body reinfer-def phint-seed
                                       set-rtenv! set-vtypes! join-types
                                       set-record-shapes! set-map-shapes! set-protocol-methods!
                                       reset-escapes! collected-escapes
                                       set-check-mode! take-diags!]]))

(defn run-passes
  "All passes, in order. The back end applies this to every analyzed form. When
  inlining is enabled for the unit (user code under direct-linking, jolt-87f),
  run inline + flatten + scalar-replace + const-fold to a capped fixpoint —
  inlining exposes map literals to lookups, scalar-replace collapses them, which
  may expose more — then a collection-type inference pass (jolt-99x, optionally
  also emitting success diagnostics) that auto-drops the lookup guard where the
  type is proven. Otherwise (core + bootstrap) just const-fold, as before."
  [node ctx]
  (if (inline-enabled? ctx)
    (let [_ (set-rec-shapes! (record-shapes ctx))   ;; record ctor fold (jolt-15jq)
          opt (loop [i 0 n (const-fold node)]
                (reset! dirty false)
                (let [n2 (const-fold (scalar-replace (flatten-lets (inline-node n ctx))))]
                  (if (and @dirty (< i 8))
                    (recur (inc i) n2)
                    n2)))]
      ;; a final const-fold after inference propagates any predicate folded to a
      ;; constant (jolt-wcw), collapsing the `if` it gates to the taken branch.
      (const-fold (run-inference opt)))
    (const-fold node)))
