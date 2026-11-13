(ns jolt.analyzer.tree
  "Walks the analyzer makes over its own analyzed IR: the recur tail-position
  check, a fn literal's free names, and the multi-arity clause rules."
  (:require [jolt.ir :refer [reduce-ir-children]]
            [jolt.host :refer [form-position]]
            [jolt.analyzer.diagnostics :refer [diagnostic-data current-form-position]]))

;; `recur` rebinds and jumps, so it may only appear where its value IS the value
;; of the enclosing target. Anywhere else the surrounding expression is silently
;; discarded: (loop [i 0] (+ 1 (recur (inc i)))) looped forever here and never
;; applied the (+ 1 ...), where the reference refuses to compile it.
;;
;; Checked as a pass over the ANALYZED tree rather than threaded through the
;; analyze-* arms, because tail position is a property of the tree and threading
;; it would put a separate rule in every arm — where one wrong rule REJECTS VALID
;; CODE, which is worse than the permissiveness it replaces. Here the default is
;; "children are not tail", so an op this does not name can only be restrictive
;; about a recur nested inside it, never wrong about one in tail position.
;;
;; Only three ops pass tail position down: an `if` to its branches, a `do` to its
;; last form, a `let` to its body. Every other construct that admits a recur in
;; tail position — case, cond, when, or, and, letfn — is a macro over exactly
;; these, so it needs no rule of its own.
;;
;; NOT the back end's tail-transparent-ops (backend_scheme.clj), which is a
;; larger set answering a different question. That one asks "may a tail call sit
;; under this node", so it also names :invoke, :throw and :host-call — the ops
;; that read *tail?* themselves to decide whether to store a frame site — and
;; :loop, whose own body is where its self-call is tail. Widening this set to
;; match it would put a recur back in an argument position: (loop [] (f (recur)))
;; is exactly what an :invoke entry here would re-admit.
;;
;; :fn and :loop are their OWN recur targets: their bodies are checked when those
;; bodies are analyzed, so descending into them here would check them twice
;; against the wrong target. A loop's binding INITS still belong to the enclosing
;; target, though, and are checked as non-tail.
(defn check-recur-tails!
  ([node] (check-recur-tails! node true))
  ([node tail?]
   (when (some? node)
     (let [op (:op node)]
       (cond
         (= op :recur)
           (do (when-not tail?
                 ;; The recur's OWN position, carried on the node, because this
                 ;; pass runs after the body is analyzed — the position box has
                 ;; been restored by then and names the enclosing loop/fn instead.
                 ;; The reference points at the recur (pos.clj:6:12, not the loop
                 ;; on 4:3) and so should this.
                 (throw (ex-info "Can only recur from tail position"
                                 (diagnostic-data :analyze/invalid-recur
                                                  (:pos node) nil))))
               (doseq [a (:args node)] (check-recur-tails! a false)))
         (= op :fn) nil
         (= op :loop)
           (doseq [b (:bindings node)] (check-recur-tails! (nth b 1) false))
         (= op :if)
           (do (check-recur-tails! (:test node) false)
               (check-recur-tails! (:then node) tail?)
               (check-recur-tails! (:else node) tail?))
         (= op :do)
           (do (doseq [s (:statements node)] (check-recur-tails! s false))
               (check-recur-tails! (:ret node) tail?))
         (= op :let)
           (do (doseq [b (:bindings node)] (check-recur-tails! (nth b 1) false))
               (check-recur-tails! (:body node) tail?))
         :else
           (reduce-ir-children (fn [_ c] (check-recur-tails! c false)) nil node))))))

;; The ORIGINAL (pre-munge) names of the locals a fn literal references that are
;; bound OUTSIDE it — its image-registration free-name list. Computed by walking
;; the analyzed arities with the LEXICAL environment (a set of in-scope names):
;; a :local not in the set is free; a binding form extends the set only for the
;; scope it covers, and a binding's init is walked BEFORE its own name binds
;; ((let [y y] …) reports the outer y). A nested :fn's body IS descended, with
;; its own params/rest/self-name added: the outer literal's :src-form textually
;; contains the nested one, so restore evaluates the whole outer form with the
;; outer's free names bound, and a name free only inside the nested literal must
;; be in the outer's list or it would resolve to a same-named global at eval.
;; Sorted so the registration is stable across builds.
;;
;; A nested :fn is READ, not descended. analyze-fn runs bottom-up, so a nested
;; literal already carries the answer for its own subtree: :free-names is exactly
;; the names it references that nothing inside it binds, which is what descending
;; would recompute. Filtering that list against the enclosing scope gives the same
;; set — a name is this literal's iff the nested one leaves it free AND nothing out
;; here binds it — and it costs the nested list rather than the nested subtree.
;;
;; Descending was quadratic, and it was the analyzer's whole cost on nested
;; literals: every literal walked its entire subtree, so a chain of N of them
;; walked O(N^2) nodes. The CPS pass in clojure.core.async builds that chain (one
;; closure per park site) and 240 sites spent 1.4 s here. Reading takes it to 30 ms.
;; A node with no :free-names (one a pass built, rather than analyze-fn) is
;; descended as before, so the fallback cannot silently drop a capture.
(declare walk-free-names)

(defn- note-free! [refs bound nm]
  (when-not (contains? bound nm) (swap! refs conj nm)))

(defn- arity-params [a]
  (if (:rest a) (conj (:params a) (:rest a)) (:params a)))

(defn- walk-free-fn [refs n bound]
  (if-let [inner (:free-names n)]
    (doseq [nm inner] (note-free! refs bound nm))
    (doseq [a (:arities n)]
      (walk-free-names refs (:body a)
                       (into (if (:name n) (conj bound (:name n)) bound) (arity-params a))))))

;; letrec (letfn, and the state-machine loops core.async's CPS
;; transform builds from it) binds EVERY name across EVERY init,
;; including the init's own. Walking those inits with only the
;; earlier names bound — which is right for let* and loop, where
;; an init sees only what precedes it — reports a letrec binding
;; as free in its own init. That over-approximates :free-names,
;; and a name the literal itself binds has no captured value to
;; recover, so the closure refuses at dump for a variable that
;; was never a capture.
(defn- walk-free-let [refs n bound]
  (if (:letrec n)
    (let [b* (into bound (map first (:bindings n)))]
      (doseq [b (:bindings n)] (walk-free-names refs (second b) b*))
      (walk-free-names refs (:body n) b*))
    (let [b* (reduce (fn [b [nm init]]
                       (walk-free-names refs init b)
                       (conj b nm))
                     bound (:bindings n))]
      (walk-free-names refs (:body n) b*))))

(defn- walk-free-try [refs n bound]
  (walk-free-names refs (:body n) bound)
  (when (:catch-body n)
    (walk-free-names refs (:catch-body n)
                     (if (:catch-sym n) (conj bound (:catch-sym n)) bound)))
  (when (:finally n) (walk-free-names refs (:finally n) bound)))

;; Add to refs every :local under n that `bound` (the names in scope) does not
;; bind.
(defn- walk-free-names [refs n bound]
  (let [op (:op n)]
    (cond
      (= op :local) (note-free! refs bound (:name n))
      (= op :fn) (walk-free-fn refs n bound)
      (or (= op :let) (= op :loop)) (walk-free-let refs n bound)
      (= op :try) (walk-free-try refs n bound)
      :else
      (reduce-ir-children (fn [acc c] (walk-free-names refs c bound) acc) nil n))))

(defn fn-free-names [arities fn-name]
  (let [refs (atom #{})
        ;; each arity's params bind only in that arity: (fn ([x] y) ([y] y))
        ;; references y free in the first arity even though the second binds it
        arity-own (fn [a] (into (set (arity-params a))
                                (when fn-name [fn-name])))]
    (doseq [a arities] (walk-free-names refs (:body a) (arity-own a)))
    (vec (sort @refs))))

;; A multi-arity fn's clauses must be distinguishable by argument count, as the
;; JVM's FnExpr.parse requires: one variadic clause at most, no two fixed clauses
;; of the same count, and no fixed clause taking more than the variadic one's
;; required count. Same checks and the same order as the JVM: per clause in
;; declaration order, then the fixed-above-variadic rule. Accepting a second
;; variadic clause was not harmless -- dispatch takes the first clause that
;; accepts the count, so ([a & r] ...) ([a b c & r] ...) never reaches the second,
;; and a first clause that delegates to the second loops forever.
(defn check-fn-arities [clauses arities]
  ;; Each error points at the clause that broke the rule, as the recur check
  ;; points at the recur: the clause is the code the user wrote, where the fn
  ;; form may be a macro's (defn's, or fn's own fn*) and pointing there drew a
  ;; note blaming the macro. The reference reports the fn's position instead.
  (let [arity-error (fn [clause msg]
                      (throw (ex-info msg (diagnostic-data :analyze/invalid-fn-arities
                                                           (or (form-position clause)
                                                               (current-form-position))
                                                           nil))))
        [variadic fixed]
        (reduce (fn [[variadic fixed] [clause a]]
                  (let [n (count (:params a))]
                    (cond
                      (:rest a)
                      (if variadic
                        (arity-error clause "Can't have more than 1 variadic overload")
                        [n fixed])
                      (contains? fixed n)
                      (arity-error clause "Can't have 2 overloads with same arity")
                      :else [variadic (conj fixed n)])))
                [nil #{}] (map vector clauses arities))]
    (when-let [clause (and variadic
                           (some (fn [[clause a]]
                                   (when (and (not (:rest a)) (> (count (:params a)) variadic))
                                     clause))
                                 (map vector clauses arities)))]
      (arity-error clause "Can't have fixed arity function with more params than variadic function"))
    arities))
