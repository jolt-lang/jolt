;; *compiler-options* {:direct-linking true} on the runtime compile spine (#1377):
;; forms compiled under it get the build's closed-world passes (direct calls,
;; inlining, type inference); everything else stays open-world. Run by smoke.sh
;; with `jolt run`; prints REPL-DL OK when every check holds.
(def failures (atom []))
(defn chk [label ok] (when-not ok (swap! failures conj label)))

(defmacro with-dl [& forms]
  `(binding [*compiler-options* {:direct-linking true}]
     ~@(map (fn [f] `(eval '~f)) forms)))

;; A small fn compiled direct-linked splices into a direct-linked caller, so the
;; caller keeps the body it was compiled against, as under the JVM's direct
;; linking. Open-world, the same caller would see the redefinition.
(with-dl
  (defn sq [x] (* x x))
  (defn use-sq [x] (sq x)))
(eval '(defn sq [x] (+ x x)))
(chk "direct-linked caller keeps the body it compiled against" (= 9 (use-sq 3)))
;; ...and the open-world redefinition dropped the stashed body, so a caller
;; compiled direct-linked AFTER it gets the new one.
(with-dl (defn use-sq2 [x] (sq x)))
(chk "a redefinition outside direct-linking clears the stash" (= 6 (use-sq2 3)))

;; Any later root write drops the stash, not just a top-level defn: a defmulti
;; defines its var through a nested def.
(with-dl (defn mm [x] x))
(defmulti mm identity)
(defmethod mm 2 [_] :two)
(with-dl (defn use-mm [x] (mm x)))
(chk "a defmulti over a stashed fn is called, not spliced" (= :two (use-mm 2)))

;; A var made dynamic after its direct-linked def is read through the var by
;; forms compiled after that.
(with-dl (def dyn1 1))
(.setDynamic #'dyn1)
(chk ".setDynamic after a direct-linked def" (= 2 (with-dl (binding [dyn1 2] dyn1))))

;; ns-unmap drops the cell a binding was linked to; the fresh var a later def
;; interns is what direct-linked callers compiled after that must reach, and
;; the unmapped fn's stashed body must not splice into them.
(with-dl (defn um [x] (* 10 x)))
(ns-unmap *ns* 'um)
(defmulti um identity)
(defmethod um 3 [_] :three)
(with-dl (defn use-um [x] (um x)))
(chk "a def after ns-unmap is called, not the stale binding" (= :three (use-um 3)))

;; A name that was a protocol method and is now a multimethod is called as one:
;; the inference's protocol registry still lists it.
(with-dl (defprotocol Sized (sz [s])))
(ns-unmap *ns* 'sz)
(defmulti sz :k)
(defmethod sz :a [_] :a-size)
(with-dl (defn use-sz [m] (sz m)))
(chk "a protocol method redefined as a multimethod" (= :a-size (use-sz {:k :a})))

;; ^:redef opts a var out: its callers stay var-routed.
(with-dl
  (defn ^:redef r [] 1)
  (defn use-r [] (r)))
(eval '(defn r [] 2))
(chk "^:redef stays redefinable" (= 2 (use-r)))

;; A direct-linked call that is not spliced binds the linked jv$ binding, and a
;; later def writes through to it.
(with-dl
  (defn m ([] 1) ([x] x))
  (defn use-m [] (m)))
(eval '(defn m ([] 2) ([x] x)))
(chk "a linked call follows a later def" (= 2 (use-m)))

;; Records and protocols compile and dispatch under the inference.
(with-dl
  (defprotocol Area (area [s]))
  (defrecord Sq [w] Area (area [_] (* w w)))
  (defn total [xs] (reduce + (map area xs))))
(chk "records + protocols under direct-linking" (= 13 (total [(->Sq 2) (->Sq 3)])))

;; A spliced body keeps its fn's semantics: one object per literal site, a
;; ^long return that truncates, and a lazy-seq thunk whose captures read nil
;; when it runs again.
(with-dl
  (defn lit-rx [] #"a+")
  (defn lit-v [] [:k 'a])
  (defn ^long lret [x] x)
  (defn use-lits [] [(identical? (lit-rx) (lit-rx)) (identical? (lit-v) (lit-v))])
  (defn use-lret [] (lret 18446744073709551621N))
  (defn cleared []
    (let [s (let [v [1 2 3]]
              (lazy-seq (if (nil? v) (list :cleared) (throw (ex-info "boom" {})))))]
      [(try (seq s) (catch Exception e (ex-message e)))
       (try (seq s) (catch Exception e (ex-message e)))])))
(chk "one object per literal site across splices" (= [true true] (use-lits)))
(with-dl (defn rx-via [] (lit-rx)))
(chk "a spliced literal is the fn's own" (identical? (lit-rx) (rx-via)))
(chk "a spliced ^long return truncates" (= 5 (use-lret)))
(chk "a lazy-seq's captures read nil on a rerun" (= ["boom" '(:cleared)] (cleared)))

;; .setDynamic or ^:redef meta given after the def opts later callers out of
;; the splice, so binding and with-redefs reach them.
(with-dl
  (defn dyn-later [x] (let [a (str x x x) b (str a a)] (count (str a b))))
  (.setDynamic #'dyn-later)
  (defn use-dyn-later [x] (dyn-later x))
  (defn redef-later [x] (let [a (str x x x) b (str a a)] (count (str a b))))
  (alter-meta! #'redef-later assoc :redef true)
  (defn use-redef-later [x] (redef-later x)))
(chk "a later .setDynamic is honored by later callers"
     (= :bound (binding [dyn-later (fn [_] :bound)] (use-dyn-later 1))))
(chk "a later ^:redef is honored by later callers"
     (= :redef (with-redefs [redef-later (fn [_] :redef)] (use-redef-later 1))))

;; The default is open world.
(defn o [] 1)
(defn use-o [] (o))
(defn o [] 2)
(chk "open world by default" (= 2 (use-o)))

;; alter-var-root turns it on for every form compiled after it, as on the JVM.
(alter-var-root #'*compiler-options* assoc :direct-linking true)
(defn sq3 [x] (* x x x))
(defn use-sq3 [x] (sq3 x))
(alter-var-root #'*compiler-options* dissoc :direct-linking)
(defn sq3 [x] x)
(chk "alter-var-root of *compiler-options*" (= 8 (use-sq3 2)))

(if (empty? @failures)
  (println "REPL-DL OK")
  (doseq [f @failures] (println "FAIL:" f)))
