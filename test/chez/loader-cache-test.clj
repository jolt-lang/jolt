;; jolt.loader's artifact cache for context-private namespaces (#1653).
;;
;; Run by test/chez/loader-cache-smoke.sh, twice over one source tree and one
;; cache directory: phase 1 writes the tree and checks miss/hit/invalidation in
;; one process, phase 2 checks that a second process loads everything from the
;; artifacts phase 1 left. Needs JOLT_AOT_CACHE=1 and JOLT_AOT_ASYNC=0 (a miss
;; must have published its fasl before the next load looks for it).

(require '[jolt.loader :as jl])

(def root (first *command-line-args*))
(def phase (second *command-line-args*))

(def fails (atom 0))

(defn check [label ok? & detail]
  (if ok?
    (println "PASS:" label)
    (do (swap! fails inc)
        (apply println "FAIL:" label detail))))

(defn stats [] @@#'jolt.loader/cache-stats)

(defn delta
  "Run F and answer [its value {:hits n :misses n}] for the loads it made."
  [f]
  (let [before (stats)
        v (f)
        after (stats)]
    [v (merge-with - after before)]))

(defn write! [rel text]
  (let [f (java.io.File. (str root "/" rel))]
    (.mkdirs (.getParentFile f))
    (spit f text)))

(defn var-of [l qname]
  (jl/resolve l {:kind :var :name qname}))

(defn load-ns! [l nm]
  (jl/load l {:kind :ns :name nm}))

(when (= "1" phase)
  (write! "probe/base.clj" "(ns probe.base)\n(def cell (atom :base))\n(defn base-val [] 7)\n")
  (write! "probe/top.clj"
          (str "(ns probe.top (:require [probe.base :as b]))\n"
               "(defn total [] (+ 35 (b/base-val)))\n"
               ;; a require at CALL time: it must load through the loader that
               ;; defined this function, whichever context loaded it last
               "(defn lazy-cell [] (require 'probe.lazy) @(resolve 'probe.lazy/v))\n"
               "(defn reload-mod! [] (require 'probe.mod :reload) ((resolve 'probe.mod/answer)))\n"))
  (write! "probe/lazy.clj" "(ns probe.lazy)\n(def v (atom :lazy))\n")
  (write! "probe/mod.clj" "(ns probe.mod)\n(defn answer [] :one)\n")
  (write! "probe/mac.clj" "(ns probe.mac)\n(defmacro k [] 1)\n")
  (write! "probe/use.clj" "(ns probe.use (:require [probe.mac :as m]))\n(defn f [] (m/k))\n")
  (write! "probe/cfg.txt" "first")
  (write! "probe/rd.clj"
          (str "(ns probe.rd)\n(defmacro cfg [] (slurp \"" root "/probe/cfg.txt\"))\n"))
  (write! "probe/rduse.clj" "(ns probe.rduse (:require [probe.rd :as rd]))\n(defn f [] (rd/cfg))\n"))

(if (= "1" phase)
  (do
    ;; (a) a second context over the same root loads from the artifacts, and
    ;; still gets cells of its own
    (let [l1 (jl/classpath [root])
          [_ d1] (delta #(load-ns! l1 "probe.top"))
          l2 (jl/classpath [root])
          [_ d2] (delta #(load-ns! l2 "probe.top"))
          c1 (var-of l1 "probe.base/cell")
          c2 (var-of l2 "probe.base/cell")]
      (check "(a) first context compiles both namespaces" (= {:hits 0 :misses 2} d1) d1)
      (check "(a) second context loads both from the cache" (= {:hits 2 :misses 0} d2) d2)
      (check "(a) the cached load computes" (= 42 ((deref (var-of l2 "probe.top/total"))))
             ((deref (var-of l2 "probe.top/total"))))
      (check "(a) each context has its own cells"
             (and c1 c2 (not (identical? c1 c2)) (not (identical? @c1 @c2)))
             c1 c2)

      ;; (b) code from a cached artifact keeps its DEFINING context: l1's
      ;; function runs after l2 loaded the same artifact, and still loads
      ;; through l1
      (let [v1 ((deref (var-of l1 "probe.top/lazy-cell")))
            v2 ((deref (var-of l2 "probe.top/lazy-cell")))]
        (check "(b) a call-time require loads through the defining context"
               (and (jl/resolve l1 {:kind :ns :name "probe.lazy"})
                    (jl/resolve l2 {:kind :ns :name "probe.lazy"})
                    (not (identical? v1 v2)))
               v1 v2))

      ;; (c) :reload of unchanged source loads the artifact; an edit recompiles
      (let [reload! (deref (var-of l1 "probe.top/reload-mod!"))
            _ (reload!)
            [v d] (delta reload!)]
        (check "(c) :reload of unchanged source hits" (and (= :one v) (= {:hits 1 :misses 0} d)) v d)
        (write! "probe/mod.clj" "(ns probe.mod)\n(defn answer [] :two)\n")
        (let [[v d] (delta reload!)]
          (check "(c) :reload of edited source recompiles" (and (= :two v) (= {:hits 0 :misses 1} d)) v d)))
      (jl/unload! l1)
      (jl/unload! l2))

    ;; (d) a dependency's macro is baked into its consumer: editing it must
    ;; invalidate the consumer's artifact
    (let [l (jl/classpath [root])]
      (load-ns! l "probe.use")
      (check "(d) consumer sees the macro" (= 1 ((deref (var-of l "probe.use/f")))))
      (jl/unload! l))
    (let [l (jl/classpath [root])
          [_ d] (delta #(load-ns! l "probe.use"))]
      (check "(d) unchanged dependency: both hit" (= {:hits 2 :misses 0} d) d)
      (jl/unload! l))
    (write! "probe/mac.clj" "(ns probe.mac)\n(defmacro k [] 2)\n")
    (let [l (jl/classpath [root])
          [_ d] (delta #(load-ns! l "probe.use"))]
      (check "(d) edited macro reaches the consumer" (= 2 ((deref (var-of l "probe.use/f")))))
      (check "(d) both recompile" (= {:hits 0 :misses 2} d) d)
      (jl/unload! l))

    ;; (e) a file a macro read at compile time is part of the key
    (let [l (jl/classpath [root])]
      (load-ns! l "probe.rduse")
      (check "(e) consumer sees the file" (= "first" ((deref (var-of l "probe.rduse/f")))))
      (jl/unload! l))
    (write! "probe/cfg.txt" "second")
    (let [l (jl/classpath [root])]
      (load-ns! l "probe.rduse")
      (check "(e) an edited file reaches the consumer"
             (= "second" ((deref (var-of l "probe.rduse/f"))))
             ((deref (var-of l "probe.rduse/f"))))
      (jl/unload! l)))

  ;; phase 2: a new process over the unchanged tree compiles nothing
  (let [l (jl/classpath [root])
        [_ d] (delta #(doseq [n ["probe.top" "probe.use" "probe.rduse" "probe.mod"]]
                        (load-ns! l n)))]
    (check "(f) a second process loads every namespace from the cache"
           (= {:hits 7 :misses 0} d) d)
    (check "(f) and the artifacts compute"
           (and (= 42 ((deref (var-of l "probe.top/total"))))
                (= 2 ((deref (var-of l "probe.use/f"))))
                (= "second" ((deref (var-of l "probe.rduse/f"))))
                (= :two ((deref (var-of l "probe.mod/answer"))))))))

(println (str "LOADERCACHE " (if (zero? @fails) "OK" (str @fails " FAILED"))))
(System/exit (if (zero? @fails) 0 1))
