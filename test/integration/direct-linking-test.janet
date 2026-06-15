# Direct-linking / redefinition matrix (jolt-d9j, jolt-g86).
#
# Direct-linking is a per-compilation-UNIT property (Clojure model). A call
# compiles direct iff the unit has direct-linking on AND the target is not
# ^:redef/^:dynamic AND the target is an already-defined fn. Otherwise indirect
# (live var deref → redefinable). This pins the user-visible semantics:
#   - default user/REPL unit (direct-linking off): redefine anything, callers see it
#   - direct-linked unit: callers don't see a later redef (unless target ^:redef)
#   - :aot-core? gates whether the core tiers compile direct-linked

(use ../../src/jolt/api)

(var failures 0)
(defn- check [label got want]
  (unless (= got want)
    (++ failures)
    (printf "FAIL [%s] got %q want %q" label got want)))

# 1. Default unit (direct-linking OFF): redefinition reaches compiled callers.
(let [ctx (init {:compile? true})]
  (eval-string ctx "(defn add [a b] (+ a b))")
  (eval-string ctx "(defn caller [] (add 1 2))")
  (check "default before redef" (eval-string ctx "(caller)") 3)
  (eval-string ctx "(defn add [a b] (* a b))")
  (check "default sees redef (indirect)" (eval-string ctx "(caller)") 2))

# 2. Direct-linked unit: compiled caller keeps the original target after a redef.
(let [ctx (init {:compile? true :direct-linking? true})]
  (eval-string ctx "(defn add [a b] (+ a b))")
  (eval-string ctx "(defn caller [] (add 1 2))")
  (check "direct before redef" (eval-string ctx "(caller)") 3)
  (eval-string ctx "(defn add [a b] (* a b))")
  (check "direct ignores redef (sealed)" (eval-string ctx "(caller)") 3)
  # the var itself is still redefined; only the direct-linked call is frozen
  (check "direct var still updated" (eval-string ctx "(add 3 4)") 12))

# 3. ^:redef opts a var OUT of direct-linking even in a direct-linked unit.
(let [ctx (init {:compile? true :direct-linking? true})]
  (eval-string ctx "(defn ^:redef add [a b] (+ a b))")
  (eval-string ctx "(defn caller [] (add 1 2))")
  (check "redef-tagged before" (eval-string ctx "(caller)") 3)
  (eval-string ctx "(defn ^:redef add [a b] (* a b))")
  (check "redef-tagged sees redef" (eval-string ctx "(caller)") 2))

# 4. :aot-core? true (default): redefining a core fn (in clojure.core) is still
#    seen by USER code, because user calls are indirect regardless of core being
#    direct-linked internally.
(let [ctx (init {:compile? true})]
  (eval-string ctx "(defn uses-last [] (last [1 2 3]))")
  (check "core call before" (eval-string ctx "(uses-last)") 3)
  (eval-string ctx "(in-ns (quote clojure.core))")
  (eval-string ctx "(def last (fn [coll] :patched))")
  (eval-string ctx "(in-ns (quote user))")
  (check "user sees core redef (indirect)" (eval-string ctx "(uses-last)") :patched))

# 5. :aot-core? false: core compiles indirect too, so even core-internal callers
#    see a redef — the whole language is redefinable.
(let [ctx (init {:compile? true :aot-core? false})]
  (check "aot-core off still correct" (eval-string ctx "(last [1 2 3])") 3))

# 6. Redefining a record with REORDERED fields must rebind the ctor (jolt-wf4).
#    deftype expands to (do (def R (make-deftype-ctor ...)) (def ->R R) ...): the
#    `->R` alias references R in the SAME compiled unit. Direct-link embeds a var's
#    root as a compile-time constant, but R's redefined root hasn't RUN yet when
#    that unit compiles — so ->R must NOT seal to R's stale (pre-redef) ctor.
(let [ctx (init {:compile? true :direct-linking? true})]
  (eval-string ctx "(defrecord R [x a])")
  (eval-string ctx "(defrecord R [a x])")
  (check "record field-reorder redefine: :a reads the new layout"
         (eval-string ctx "(:a (->R 10 20))") 10)
  (check "record field-reorder redefine: :x reads the new layout"
         (eval-string ctx "(:x (->R 10 20))") 20))

(if (pos? failures)
  (do (printf "direct-linking: %d failure(s)" failures) (os/exit 1))
  (print "direct-linking: all matrix cases passed"))
