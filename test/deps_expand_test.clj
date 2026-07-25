;; Dependency-expansion unit tests: the fake in-memory coordinate type and the
;; test cases are taken from clojure.tools.deps (test_deps.clj + the faken
;; extension), run against jolt.deps' ported expansion engine — exclusions,
;; top-dep pinning, newest-version selection, orphan cutting, override/default
;; deps, and the Maven version comparator. Run: bin/jolt run test/deps_expand_test.clj

(ns deps-expand-test
  (:require [clojure.string :as str]
            [jolt.deps :as deps]
            [jolt.deps.ext :as ext]))

;;;; faken: fake Maven-style extension over an in-memory repo {lib {coord [deps]}}

(def ^:dynamic repo {})

(defmethod ext/coord-type-keys :fkn [_type] #{:fkn/version})

(defmethod ext/dep-id :fkn [_lib coord] (select-keys coord [:fkn/version]))

(defmethod ext/compare-versions [:fkn :fkn]
  [_lib coord-x coord-y]
  (ext/compare-mvn-versions (:fkn/version coord-x) (:fkn/version coord-y)))

(defmethod ext/coord-deps :fkn
  [lib coord]
  (remove
    (fn [[_lib {:keys [optional]}]] optional)
    (get-in repo [lib (ext/dep-id lib coord)])))

(defmethod ext/coord-info :fkn [_lib _coord] {:root nil :manifest :none})

(defn- libs
  "resolve-deps :libs for a deps map against the fake repo."
  ([deps-map] (libs deps-map nil))
  ([deps-map args] (:libs (deps/resolve-deps deps-map "." args))))

;;;; harness

(def failures (atom 0))
(def checks (atom 0))
(defn is= [label expected actual]
  (swap! checks inc)
  (when-not (= expected actual)
    (swap! failures inc)
    (println "  FAIL:" label)
    (println "    expected:" (pr-str expected))
    (println "    got:     " (pr-str actual))))

;;;; the repo from tools.deps test_deps.clj

(def base-repo
  {'org.clojure/clojure {{:fkn/version "1.9.0"}
                         [['org.clojure/spec.alpha {:fkn/version "0.1.124"}]
                          ['org.clojure/core.specs.alpha {:fkn/version "0.1.10"}]]}
   'org.clojure/spec.alpha {{:fkn/version "0.1.124"} nil
                            {:fkn/version "0.1.1"} nil}
   'org.clojure/core.specs.alpha {{:fkn/version "0.1.10"} nil}

   'e1/a {{:fkn/version "1"} [['e1/b {:fkn/version "1"}]
                              ['e1/c {:fkn/version "2"}]]}
   'e1/b {{:fkn/version "1"} [['e1/c {:fkn/version "1"}]]}
   'e1/c {{:fkn/version "1"} nil
          {:fkn/version "2"} nil}
   'opt/a {{:fkn/version "1"} [['opt/b {:fkn/version "1" :optional true}]
                               ['opt/c {:fkn/version "1"}]]}
   'opt/b {{:fkn/version "1"} nil}
   'opt/c {{:fkn/version "1"} nil}})

;; NOTE: tools.deps uses org.clojure/clojure in its fake repo; jolt treats that
;; lib as intrinsic and drops it, so those cases use a renamed twin.
(def clj-repo
  {'fake/clojure {{:fkn/version "1.9.0"}
                  [['fake/spec.alpha {:fkn/version "0.1.124"}]
                   ['fake/core.specs.alpha {:fkn/version "0.1.10"}]]}
   'fake/spec.alpha {{:fkn/version "0.1.124"} nil
                     {:fkn/version "0.1.1"} nil}
   'fake/core.specs.alpha {{:fkn/version "0.1.10"} nil}})

(defn- lib-ver [libmap]
  (reduce-kv (fn [m lib coord] (assoc m (-> lib name keyword) (:fkn/version coord)))
             {} libmap))

(binding [repo (merge base-repo clj-repo)]

  ;; test-top-optional-included
  (is= "top optional included" #{'opt/b}
       (set (keys (libs {'opt/b {:fkn/version "1"}}))))

  ;; test-transitive-optional-not-included
  (is= "transitive optional not included" #{'opt/a 'opt/c}
       (set (keys (libs {'opt/a {:fkn/version "1"}}))))

  ;; test-basic-expand
  (is= "basic expand" #{'fake/clojure 'fake/spec.alpha 'fake/core.specs.alpha}
       (set (keys (libs {'fake/clojure {:fkn/version "1.9.0"}}))))

  ;; test-top-dominates: dependent dep decides version
  (is= "transitive version selected" "0.1.124"
       (-> (libs {'fake/clojure {:fkn/version "1.9.0"}})
           (get 'fake/spec.alpha) :fkn/version))
  ;; top dep wins
  (is= "top dep wins" "0.1.1"
       (-> (libs {'fake/clojure {:fkn/version "1.9.0"}
                  'fake/spec.alpha {:fkn/version "0.1.1"}})
           (get 'fake/spec.alpha) :fkn/version))

  ;; test-override-deps
  (is= "override dep wins" "0.1.1"
       (-> (libs {'fake/clojure {:fkn/version "1.9.0"}}
                 {:override-deps {'fake/spec.alpha {:fkn/version "0.1.1"}}})
           (get 'fake/spec.alpha) :fkn/version))

  ;; test-default-deps
  (is= "default dep fills nil" "1.9.0"
       (-> (libs {'fake/clojure nil}
                 {:default-deps {'fake/clojure {:fkn/version "1.9.0"}}})
           (get 'fake/clojure) :fkn/version))

  ;; test-dep-choice: +a1 -> +b1 -> -c1 / -> +c2 — newest c wins
  (is= "newer version selected across paths" {:a "1" :b "1" :c "2"}
       (lib-ver (libs {'e1/a {:fkn/version "1"}}))))

;; test-dep-parent-missing: +a1 -> +b1 -> -c1 -> -x2 (x2 addressed only via
;; omitted c1) / -> +c2
(binding [repo {'e2/a {{:fkn/version "1"} [['e2/b {:fkn/version "1"}]
                                           ['e2/c {:fkn/version "2"}]]}
                'e2/b {{:fkn/version "1"} [['e2/c {:fkn/version "1"}]]}
                'e2/c {{:fkn/version "1"} [['e2/x {:fkn/version "2"}]]
                       {:fkn/version "2"} nil}
                'e2/x {{:fkn/version "2"} nil}}]
  (is= "orphaned child of omitted parent is cut" {:a "1" :b "1" :c "2"}
       (lib-ver (libs {'e2/a {:fkn/version "1"}}))))

;; test-dep-choice2: +a1 -> +b1 -> -c1 / -> +c2 -> +d1
(binding [repo {'e3/a {{:fkn/version "1"} [['e3/b {:fkn/version "1"}]
                                           ['e3/c {:fkn/version "2"}]]}
                'e3/b {{:fkn/version "1"} [['e3/c {:fkn/version "1"}]]}
                'e3/c {{:fkn/version "1"} nil
                       {:fkn/version "2"} [['e3/d {:fkn/version "1"}]]}
                'e3/d {{:fkn/version "1"} nil}}]
  (is= "newer version's children included" {:a "1" :b "1" :c "2" :d "1"}
       (lib-ver (libs {'e3/a {:fkn/version "1"}}))))

;; test-circular-deps: a -> b -> a terminates with both included
(binding [repo {'c1/a {{:fkn/version "1"} [['c1/b {:fkn/version "1"}]]}
                'c1/b {{:fkn/version "1"} [['c1/a {:fkn/version "1"}]]}}]
  (is= "circular deps terminate" #{'c1/a 'c1/b}
       (set (keys (libs {'c1/a {:fkn/version "1"}})))))

;; test-cut-previously-selected-child:
;; +a -> +b1 -> +x2 -> +y1
;;    -> +c -> -b2 -> -x3 -> -z1   (b1 top-ish selected first, b2 loses)
(binding [repo {'cut/a {{:fkn/version "1"} [['cut/b {:fkn/version "1"}]
                                            ['cut/c {:fkn/version "1"}]]}
                'cut/b {{:fkn/version "1"} [['cut/x {:fkn/version "2"}]]
                        {:fkn/version "2"} [['cut/x {:fkn/version "3"}]]}
                'cut/c {{:fkn/version "1"} [['cut/b {:fkn/version "2"}]]}
                'cut/x {{:fkn/version "2"} [['cut/y {:fkn/version "1"}]]
                        {:fkn/version "3"} [['cut/z {:fkn/version "1"}]]}
                'cut/y {{:fkn/version "1"} nil}
                'cut/z {{:fkn/version "1"} nil}}]
  (let [lv (lib-ver (libs {'cut/a {:fkn/version "1"}}))]
    (is= "newer transitive b wins" "2" (:b lv))
    (is= "b2's x3 selected" "3" (:x lv))
    (is= "orphaned y cut" nil (:y lv))
    (is= "z included" "1" (:z lv))))

;; exclusions: same lib/version on two paths, one excluding — the exclusion
;; narrows to the intersection (tools.deps test-dep-same-version-different-exclusions)
(binding [repo {'ex/a {{:fkn/version "1"} [['ex/b {:fkn/version "1" :exclusions ['ex/c]}]
                                           ['ex/d {:fkn/version "1"}]]}
                'ex/b {{:fkn/version "1"} [['ex/c {:fkn/version "1"}]]}
                'ex/c {{:fkn/version "1"} nil}
                'ex/d {{:fkn/version "1"} [['ex/b {:fkn/version "1"}]]}}]
  (is= "exclusion narrows when same version seen without it"
       #{'ex/a 'ex/b 'ex/c 'ex/d}
       (set (keys (libs {'ex/a {:fkn/version "1"}})))))

(binding [repo {'ex2/a {{:fkn/version "1"} [['ex2/b {:fkn/version "1" :exclusions ['ex2/c]}]]}
                'ex2/b {{:fkn/version "1"} [['ex2/c {:fkn/version "1"}]
                                            ['ex2/d {:fkn/version "1"}]]}
                'ex2/c {{:fkn/version "1"} nil}
                'ex2/d {{:fkn/version "1"} nil}}]
  (is= "excluded transitive dep is cut" #{'ex2/a 'ex2/b 'ex2/d}
       (set (keys (libs {'ex2/a {:fkn/version "1"}})))))

;;;; Maven version comparator ordering

(defn- lt [a b] (is= (str a " < " b) true (neg? (ext/compare-mvn-versions a b))))
(defn- veq [a b] (is= (str a " = " b) 0 (ext/compare-mvn-versions a b)))

(lt "1.2.3" "1.2.10")
(lt "1.9.0" "1.10.0")
(lt "1.0-alpha" "1.0-beta")
(lt "1.0-beta" "1.0-rc1")
(lt "1.0-rc1" "1.0")
(lt "1.0-SNAPSHOT" "1.0")
(lt "1.0-alpha1" "1.0-alpha2")
(lt "1.0" "1.0.1")
(lt "1.0" "1.0-sp1")
(lt "0.9" "1.0")
(veq "1.0" "1.0.0")
(veq "1.0" "1.0-final")
(veq "1.0-ga" "1.0.0-release")
(lt "1.0a1" "1.0")

(println (str "deps-expand: " (- @checks @failures) "/" @checks " passed"))
(when (pos? @failures)
  (System/exit 1))
