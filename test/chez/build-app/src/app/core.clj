(ns app.core
  (:require [app.util :as util :refer [greet]]
            [clojure.java.io :as io]
            [jolt.image :as img]))

;; An aliased cross-ns defmethod: 'util/greet is passed quoted to defmethod-setup,
;; so the AOT build must register the `util` alias for app.core or it resolves to
;; ns "util" and never reaches app.util/greet (the dispatch falls to :default).
(defmethod util/greet :loud [_] "greet:loud")

;; jolt#1009, the same-namespace half: a plain def and the reads of it are linked
;; app->app within one unit. See --varroot in -main.
(def same-ns-val nil)

;; A defmethod on a REFERRED multifn (bare `greet`): the AOT build must register
;; the :refer so the bare name resolves to app.util/greet, not a shadow.
(defmethod greet :soft [_] "greet:soft")

;; Namespace top-level code that WAITS on another thread. A built binary used to
;; run these forms from the Chez boot file, during Sbuild_heap, where Chez does
;; not schedule a forked thread at all — so both of these answered :TIMED-OUT in
;; a binary while working under `jolt -m` and in the REPL. clojure.java.shell/sh
;; is the shape that found it: it drains the child through two futures and
;; derefs them with no timeout, so a top-level (sh …) hung forever.
;; Bounded waits deliberately: a regression must fail with a wrong value, not by
;; hanging the gate.
(def boot-future (deref (future :ran) 5000 :TIMED-OUT))
(def boot-thread (let [p (promise)]
                   (.start (Thread. (fn [] (deliver p :ran))))
                   (deref p 5000 :TIMED-OUT)))

(defn -main [& args]
  ;; --boom: throw through a two-deep call chain so build-smoke can assert the
  ;; native stack trace. Off the normal path, so default output is unchanged.
  (when (= (first args) "--boom")
    (util/mid-boom "not-a-number"))
  ;; --num: call a hintless double fn so build-smoke can assert wp-infer ran in
  ;; the release default build (the fl-op in flat.ss, not this output).
  (when (= (first args) "--num")
    (println "area:" (util/area 2.0)))
  ;; --strd: unhinted string interop via the str-ret :str stamp (see app.util).
  (when (= (first args) "--strd")
    (println "strd:" (util/strd-prefix "sy") (util/strd-prefix "no") (util/strd-find "a-b")
             (util/strd-rep "aa") (util/strd-rep "cc")))
  ;; --kwsym: proven-keyword interop — (.sym k) on a ^clojure.lang.Keyword param.
  (when (= (first args) "--kwsym")
    (println "kwsym:" (util/kwsym :ns/qual) (util/kwsym :plain)))
  ;; --sbjoin: proven-StringBuilder interop — an unhinted (let [sb (StringBuilder.)]).
  (when (= (first args) "--sbjoin")
    (println "sbjoin:" (util/sbjoin "." ["a" "b" "c"]) (util/sbjoin "-" []) (util/sbjoin "," ["x"])))
  ;; --redef: with direct-link the release default, ^:redef/:dynamic must still
  ;; opt out so runtime redefinition / binding take effect in the built binary.
  (when (= (first args) "--redef")
    (println "redef:" (with-redefs [util/redef-fn (fn [] :patched)]
                        (util/redef-fn)))
    (println "dyn:" (binding [util/*config* :bound]
                      util/*config*)))
  ;; --varroot: jolt#1009. A PLAIN (no ^:redef/^:dynamic) def is direct-linked,
  ;; and a root write must still reach the binding a compiled value read goes to.
  ;; Every line here read the STALE value in a built binary while `jolt run` and
  ;; the REPL read the new one — the split that made a green test suite go red as
  ;; a binary. `same-ns` covers an app->app link inside one namespace; the util/
  ;; ones cover the cross-ns link, a value read of a fn var, and with-redefs.
  (when (= (first args) "--varroot")
    (alter-var-root #'same-ns-val (constantly :set))
    (println "same-ns:" same-ns-val "/" (var-get #'same-ns-val))
    (alter-var-root #'util/root-val (constantly :set))
    (println "cross-ns:" util/root-val "/" (var-get #'util/root-val))
    (alter-var-root #'util/root-fn (constantly (fn [] :patched)))
    (println "fn-value:" ((identity util/root-fn)) "/" ((var-get #'util/root-fn)))
    ;; A DIRECT CALL is the other half of the closed world and is NOT expected to
    ;; follow: the inline pass may have spliced root-fn's old body into this site
    ;; (here it const-folds to :original outright), so there is no read left to
    ;; redirect. ^:redef / ^:dynamic opt a var out of that, as --redef pins.
    (println "fn-direct:" (util/root-fn))
    (println "with-redefs:" (with-redefs [util/root-val :bound] util/root-val))
    (println "after-redefs:" util/root-val))
  ;; --fnid: closure identity in a BUILT binary. Chez returns ONE closure object
  ;; for every evaluation of a lambda with no free variables, where Clojure
  ;; allocates a fresh fn each time, so the back end gives such a lambda
  ;; something to capture. This has to be asserted HERE and not only under
  ;; `jolt -e`: the release default is --direct-link plus whole-program
  ;; inference, which is exactly where a guard that survives the interpreter
  ;; could still be optimized away. build-smoke runs it on the direct-linked
  ;; build and on --no-direct-link.
  (when (= (first args) "--fnid")
    (let [mk (fn [] (fn [x] x))
          f1 (mk)
          f2 (mk)
          cap (fn [n] (fn [] n))]
      (println "fnid-same:" (identical? f1 f2))
      (println "fnid-set:" (count (set [(mk) (mk)])))
      (println "fnid-meta:" (pr-str [(meta (with-meta f1 {:t 1})) (meta f2)]))
      (println "fnid-call:" ((mk) 7))
      (println "fnid-cap:" (identical? (cap 1) (cap 1)))))
  ;; --heap: the heap ceiling, in a BUILT binary. The install is emitted code in
  ;; the app launcher (build.ss), which is a different site from jolt's own
  ;; launcher, and only a built binary runs it — `jolt -e` proves nothing about
  ;; this one. Reports the ceiling, and separately whether exceeding a low one
  ;; arrives as a catchable OutOfMemoryError rather than a kernel SIGKILL.
  (when (= (first args) "--heap")
    (println "heap-max:" (.maxMemory (Runtime/getRuntime))))
  (when (= (first args) "--heap-oom")
    (println "heap-oom:"
             (try
               (loop [acc [] i 0]
                 (if (> i 100000000) :never-tripped (recur (conj acc (object-array 256)) (inc i))))
               (catch OutOfMemoryError _ :caught-oom)
               (catch Throwable t (str "other:" (type t))))))
  ;; --doubledef: a var defined twice must answer the same through every call
  ;; path in the built binary, and the same as `jolt run` (jolt-rtjm). apply
  ;; defeats any direct-call folding, so the two lines exercise different doors
  ;; into dd-caller.
  (when (= (first args) "--doubledef")
    (println "dd-apply:" (apply util/dd-caller nil))
    (println "dd-call: " (util/dd-caller))
    (println "dd-late: " (util/dd-late)))
  ;; --fwdref: a symbol compiled BEFORE a same-ns redefinition must resolve to
  ;; the clojure.core var in the built binary too — the emit walk re-analyzes
  ;; against the fully-loaded process where app.util/get already exists, and
  ;; resolving the ns-local redef made (fwd-get m "K") assoc onto a String
  ;; (issue #451: kmet's built binary died "class java.lang.String cannot be
  ;; cast to class clojure.lang.Associative" while `jolt run` was fine).
  (when (= (first args) "--fwdref")
    (println "fwd-get:  " (util/fwd-get {"K" 41} "K"))
    (println "fwd-first:" (util/fwd-first [7 8]))
    (println "fwd-late: " (util/fwd-late {"K" 5} "K")))
  ;; --closure <path>: write a closure returned by a SPLICED callee to a state
  ;; image, read it back, and call both. `jolt run` and the built binary have to
  ;; agree — they did not, because the splice dropped the fn's source
  ;; registration and only a built binary splices (jolt-giqc).
  (when (= (first args) "--closure")
    (let [path (second args)
          folded (util/make-closure 10)                 ; constant, no capture left
          live   (util/make-closure (+ 3 (count args)))] ; a renamed live local
      (println "closure-scan:" (count (img/scan folded)) (count (img/scan live)))
      (img/dump! path folded)
      (println "closure-folded:" (folded 5) ((img/read-image path) 5))
      (img/dump! path live)
      (println "closure-live:" (live 5) ((img/read-image path) 5))
      ;; the rest of the value kinds an image is supposed to carry, checked HERE
      ;; because a built binary is a different emit path from `jolt run` and
      ;; nothing else exercises images through it
      (let [lazy (map inc (range 4))
            walked (let [s (map inc (range 100))] (first s) s)]
        (println "img-dumpable:" (img/dumpable? lazy) (img/dumpable? util/image-mm))
        (img/dump! path lazy)
        (println "img-lazy:" (vec (img/read-image path)))
        (img/dump! path walked)
        (println "img-walked:" (vec (take 3 (img/read-image path))))
        (img/dump! path util/image-mm)
        (println "img-multi:" ((img/read-image path) :a) ((img/read-image path) :zz))
        (img/dump! path {:p (promise) :n (find-ns (quote app.core))})
        (let [back (img/read-image path)]
          (println "img-misc:" (realized? (:p back))
                   (identical? (:n back) (find-ns (quote app.core))))))))

  ;; --dtlookup: a deftype that declares its own ILookup must answer through
  ;; that valAt for a field-named key in a BUILT binary too. `jolt run` was right
  ;; and the build folded past it, so this is compared against `jolt run` in
  ;; build-smoke rather than pinned here (jolt-fpp3.1).
  (when (= (first args) "--dtlookup")
    (reset! util/lk-box (util/->Lk (count args)))
    (println "dt-ctor:  " (:a (util/->Lk 1)))
    (println "dt-proven:" (util/lk-read (util/->Lk (count args))))
    (println "dt-opaque:" (:a @util/lk-box) (get @util/lk-box :zz :none)))
  ;; --innerfn: a named inner fn inside a spliced callee (jolt-pzos).
  (when (= (first args) "--innerfn")
    (util/inner-boom 1))
  ;; --resloader: the ClassLoader surface must resolve exactly what io/resource
  ;; resolves, INCLUDING a resource baked into this binary. It used to walk the
  ;; source roots on its own and never look at the embedded table, so every
  ;; classpath-probing library that reaches resources through RT/baseLoader
  ;; rather than clojure.java.io found nothing in a built artifact and everything
  ;; in the source tree it was developed against. Only a built binary has an
  ;; embedded resource, so this is the only place the claim can be checked.
  (when (= (first args) "--resloader")
    (let [cl (clojure.lang.RT/baseLoader)]
      (println "resloader:"
               (= (str (io/resource "greeting.txt")) (str (.getResource cl "greeting.txt")))
               (= (slurp (io/resource "greeting.txt"))
                  (slurp (.getResourceAsStream cl "greeting.txt")))
               (count (enumeration-seq (.getResources cl "greeting.txt")))
               (some? (.getResource String "/greeting.txt"))
               (nil? (.getResource cl "no-such-resource.txt")))))
  ;; the resource is baked into the binary (deps.edn :jolt/build :embed), so this
  ;; resolves with no resources/ dir on disk, run from any cwd.
  (println (slurp (io/resource "greeting.txt")))
  (util/twice (println (util/shout "hello from a built binary")))
  (println "args:" (vec args))
  (println "sum:" (reduce + (map count args)))
  (println "greet-default:" (util/greet :unknown))
  (println "greet-loud:" (util/greet :loud))
  (println "greet-soft:" (util/greet :soft))
  (println "boot-threads:" boot-future boot-thread))
