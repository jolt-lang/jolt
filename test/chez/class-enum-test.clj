;; jolt.host/class-names and jolt.host/all-classes (#1663): every class the
;; runtime models, by canonical JVM name, as babashka.classes/all-classes
;; answers on babashka.
;; Run: bin/jolt run test/chez/class-enum-test.clj (smoke.sh greps for
;; "CLASS-ENUM-TEST OK").
(ns class-enum-test
  (:require [jolt.host :as h]
            [jolt.scheme :as scheme]))

(def failures (atom []))

(defmacro check-eq [label got want]
  `(do
     (print (str "  .. " ~label "\n"))
     (flush)
     (let [g# ~got w# ~want]
       (when-not (= g# w#)
         (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#)))))))

;; which lazy shims have loaded, before anything here lists a class
(defn- lazy-status [] (scheme/eval-string "(format \"~s\" (lazy-shims-status))"))
(def lazy-before (lazy-status))

(def names (h/class-names))
(def name-set (set names))

(check-eq "an eager vector" (vector? names) true)
(check-eq "sorted" (= names (vec (sort names))) true)
(check-eq "no duplicates" (= (count names) (count name-set)) true)
(check-eq "every name canonical, with a package" (every? #(re-find #"\." %) names) true)
(check-eq "no short aliases" (some #{"String" "System" "Collections" "Vector"} names) nil)

;; one from each place a class is modeled
(doseq [n ["java.lang.String" "java.util.ArrayList" "clojure.lang.IFn"     ; graph rows
           "clojure.lang.Seqable" "clojure.lang.IMeta"                     ; supers with no row
           "java.util.Collections" "java.util.Objects" "java.nio.file.Files"
           "java.util.concurrent.Executors"                                ; statics only
           "java.net.URL"                                                  ; a constructor only
           "java.nio.file.WatchKey"                                        ; a host tag only
           "java.util.ArrayDeque" "java.util.Vector"                       ; lazy shims
           "clojure.lang.Compiler$CompilerException"                       ; registered short too
           "clojure.lang.LispReader$ReaderException"
           "clojure.lang.LockingTransaction$RetryEx"]]
  (check-eq (str "lists " n) (contains? name-set n) true))

(def classes (h/all-classes))
(check-eq "all-classes is the same set, in the same order"
          (mapv #(.getName %) classes) names)
(check-eq "each Class is the one jolt-class-for interns"
          (every? #(identical? % (h/jolt-class-for (.getName %))) classes) true)
(check-eq "each has a simple name" (every? #(seq (.getSimpleName %)) classes) true)

(check-eq "listing loads no lazy shim" (lazy-status) lazy-before)
(check-eq "two calls with nothing registered between agree" (h/class-names) names)

;; a class registered later is in the next call
(defrecord EnumProbeRecord [a])
(deftype EnumProbeType [b])
(h/register-class-supers! "class.enum.test.Registered" ["java.lang.Object"])
(let [later (set (h/class-names))]
  (check-eq "a defrecord registered since" (contains? later "class_enum_test.EnumProbeRecord") true)
  (check-eq "a deftype registered since" (contains? later "class_enum_test.EnumProbeType") true)
  (check-eq "register-class-supers! since" (contains? later "class.enum.test.Registered") true)
  (check-eq "nothing listed before is gone" (every? later names) true))

(if (empty? @failures)
  (println "CLASS-ENUM-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "CLASS-ENUM-TEST FAILED:" (count @failures))))
