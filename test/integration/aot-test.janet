# AOT image round-trip: compile a namespace, marshal it to bytecode, load it into
# a FRESH context, and run the loaded functions without recompiling.
(use ../../src/jolt/api)
(use ../../src/jolt/aot)
(use ../../src/jolt/types)

(print "AOT image round-trip...")

(def img-path (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-aot-test.jimg"))

# 1. Compile a namespace into ctx1: a constant, a fn over it, a fn using core
#    fns, and a recursive fn.
(def ctx1 (init {:compile? true}))
(ctx-set-current-ns ctx1 "demo")
(eval-string ctx1 "(def base 100)")
(eval-string ctx1 "(defn add-base [x] (+ x base))")
(eval-string ctx1 "(defn sum-sq [xs] (reduce + (map (fn [x] (* x x)) xs)))")
(eval-string ctx1 "(defn fact [n] (if (zero? n) 1 (* n (fact (dec n)))))")

(assert (= 107 (eval-string ctx1 "(add-base 7)")) "ctx1 add-base")
(assert (= 14 (eval-string ctx1 "(sum-sq [1 2 3])")) "ctx1 sum-sq")
(assert (= 120 (eval-string ctx1 "(fact 5)")) "ctx1 fact")

# 2. Save an AOT image of the compiled namespace.
(save-ns ctx1 "demo" img-path)
(assert (os/stat img-path) "image written")

# 3. Load it into a brand-new context — no recompilation of demo.
(def ctx2 (init {:compile? true}))
(load-ns-image ctx2 "demo" img-path)
(ctx-set-current-ns ctx2 "demo")

(assert (= 107 (eval-string ctx2 "(add-base 7)")) "ctx2 add-base from image")
(assert (= 14 (eval-string ctx2 "(sum-sq [1 2 3])")) "ctx2 sum-sq from image")
(assert (= 3628800 (eval-string ctx2 "(fact 10)")) "ctx2 fact from image (new arg)")

# 4. The loaded vars are live: redefining one is visible to callers compiled in
#    ctx2 that reference it.
(eval-string ctx2 "(def base 1000)")
(assert (= 1007 (eval-string ctx2 "(add-base 7)")) "loaded var still redefinable")

(os/rm img-path)
(print "AOT round-trip passed!")
