# Mutable deftype fields under shapes/direct-link (jolt-c3q).
#
# A deftype field tagged ^:unsynchronized-mutable / ^:volatile-mutable is set! at
# runtime. Immutable records are shape-recs (immutable tuples) where shapes are
# active (direct-link), so set! can't mutate them. A deftype with ANY mutable
# field opts out of the shape-rec layout and uses the mutable table form (which
# set! already mutates and field reads route through), regardless of :shapes?.
# Immutable deftypes/records keep the fast shape-rec.

(use ../../src/jolt/api)

(var failures 0)
(defn- check [label got want]
  (unless (= got want)
    (++ failures)
    (printf "FAIL [%s] got %q want %q" label got want)))

# direct-linking? true => :shapes? on (the mode where immutable records are
# tuples and this used to error "Can't set! field on non-deftype: tuple").
(let [ctx (init {:compile? true :direct-linking? true})]
  (eval-string ctx "(deftype Counter [^:unsynchronized-mutable n])")
  (eval-string ctx "(def c (->Counter 0))")
  (check "read initial mutable field" (eval-string ctx "(.-n c)") 0)
  (eval-string ctx "(set! (.-n c) 5)")
  (check "read after set!" (eval-string ctx "(.-n c)") 5)
  (eval-string ctx "(set! (.-n c) (inc (.-n c)))")
  (check "set! using prior value" (eval-string ctx "(.-n c)") 6)
  # keyword access reads the same mutated field
  (check "keyword access sees mutation" (eval-string ctx "(:n c)") 6))

# Mixed mutable + immutable fields: a method reads both, set! touches the mutable.
(let [ctx (init {:compile? true :direct-linking? true})]
  (eval-string ctx "(deftype Cell [label ^:unsynchronized-mutable v] Object (toString [_] (str label \"=\" v)))")
  (eval-string ctx "(def cell (->Cell \"x\" 1))")
  (check "immutable field reads" (eval-string ctx "(.-label cell)") "x")
  (check "custom toString before" (eval-string ctx "(str cell)") "x=1")
  (eval-string ctx "(set! (.-v cell) 42)")
  (check "mutable field after set!" (eval-string ctx "(.-v cell)") 42)
  (check "custom toString after mutation" (eval-string ctx "(str cell)") "x=42")
  (check "immutable field unchanged" (eval-string ctx "(.-label cell)") "x"))

# volatile-mutable is treated the same (also a mutable field).
(let [ctx (init {:compile? true :direct-linking? true})]
  (eval-string ctx "(deftype Box [^:volatile-mutable x])")
  (eval-string ctx "(def b (->Box :a))")
  (eval-string ctx "(set! (.-x b) :z)")
  (check "volatile-mutable set!" (eval-string ctx "(.-x b)") :z))

# An all-immutable deftype/record is unaffected: still a shape-rec, fast reads.
(let [ctx (init {:compile? true :direct-linking? true})]
  (eval-string ctx "(defrecord Pt [x y])")
  (check "immutable record reads" (eval-string ctx "(:x (->Pt 3 4))") 3)
  (check "immutable record reads y" (eval-string ctx "(:y (->Pt 3 4))") 4))

(if (pos? failures)
  (do (printf "mutable-deftype: %d failure(s)" failures) (os/exit 1))
  (print "mutable-deftype: all cases passed"))
