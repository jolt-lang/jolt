;; Dependency-expansion integration tests: the fake in-memory coordinate type
;; and cases are taken from clojure.tools.deps (test_deps.clj + the faken
;; extension). They run through jolt.deps into Grenadine's shared portable
;; expander — exclusions, top-dep pinning, newest-version selection, orphan
;; cutting, override/default deps, and the Maven version comparator.
;; Run: bin/jolt run test/deps_expand_test.clj

(ns deps-expand-test
  (:require [clojure.string :as str]
            [clojurestar.deps :as portable-deps]
            [grenadine.version :as version]
            [jolt.deps :as deps]
            [jolt.deps.ext :as ext]))

;;;; faken: fake Maven-style extension over an in-memory repo {lib {coord [deps]}}

(def ^:dynamic repo {})

(defmethod ext/coord-type-keys :fkn [_type] #{:fkn/version})

(defmethod ext/dep-id :fkn [_lib coord] (select-keys coord [:fkn/version]))

(defmethod ext/compare-versions [:fkn :fkn]
  [_lib coord-x coord-y]
  (version/compare-versions (:fkn/version coord-x) (:fkn/version coord-y)))

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

(is= "portable facade returns nil" nil (portable-deps/add-deps {}))

;;;; native path roots

(let [kind (var jolt.deps/native-path-kind-for)
      absolute (var jolt.deps/abspath-for)]
  ;; POSIX has one root spelling; a backslash and a Windows drive prefix are
  ;; ordinary filename characters there.
  (is= "POSIX slash path is absolute" :absolute (kind false "/project"))
  (is= "POSIX Windows drive path is relative" :relative (kind false "C:/project"))
  (is= "POSIX backslash path is relative" :relative (kind false "\\project"))
  (is= "POSIX absolute path keeps its spelling" "/project/../lib"
       (absolute false "/base" "/project/../lib"))

  ;; Windows accepts either separator in drive roots and UNC/device paths. Keep
  ;; the input bytes: joining roots must not normalize away dot segments or
  ;; rewrite separators behind a caller's back.
  (doseq [p ["C:/project" "d:\\project" "//server/share/project"
             "\\\\server\\share\\project" "\\\\?\\C:\\project"]]
    (is= (str "Windows absolute path kind " (pr-str p)) :absolute (kind true p))
    (is= (str "Windows absolute path spelling " (pr-str p)) p
         (absolute true "D:/base" p)))
  (is= "Windows relative path joins its declaring root" "D:/base/../lib"
       (absolute true "D:/base" "../lib"))

  ;; A leading separator is relative to the current drive, but prefixing the
  ;; project directory would change its meaning. Preserve both accepted Windows
  ;; separator spellings, matching the old leading-slash behavior.
  (doseq [p ["/project" "\\project"]]
    (is= (str "Windows root-relative path kind " (pr-str p)) :root-relative (kind true p))
    (is= (str "Windows root-relative path spelling " (pr-str p)) p
         (absolute true "D:/base" p)))

  ;; C:path resolves against Windows' per-drive current directory, not the
  ;; declaring project's directory. The launcher changes cwd before resolution,
  ;; so fail with the form named instead of manufacturing D:/base/C:path.
  (doseq [p ["C:project" "d:"]]
    (is= (str "Windows drive-relative path kind " (pr-str p))
         :drive-relative (kind true p))
    (is= (str "Windows drive-relative path rejected " (pr-str p))
         {:path p :kind :drive-relative}
         (try (absolute true "D:/base" p)
              nil
              (catch Exception e (ex-data e))))))

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

(defn- cmp
  "Through the :mvn method, so this pins the comparator the resolver really uses."
  [a b]
  (ext/compare-versions 'a/b {:mvn/version a} {:mvn/version b}))
(defn- lt [a b] (is= (str a " < " b) true (neg? (cmp a b))))
(defn- veq [a b] (is= (str a " = " b) 0 (cmp a b)))

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
;; a dash opens a sub-list, which sorts under an integer in the same position
(lt "1.0-1" "1.0.1")

;;;; unresolvable Maven deps abort resolution (jolt-ktiz.3, jolt-ktiz.7)
;;
;; The behaviour change worth pinning: a dep that could not be OBTAINED used to
;; leave procurement with no root, which the expansion treats exactly like a jar
;; that carries no jolt source — contributing nothing, silently. tools.deps
;; aborts ("Error building classpath. The following artifacts could not be
;; resolved:", exit 1) for a missing artifact and an unreachable repository
;; alike, verified against Clojure CLI 1.12.5.1654, and jolt already did it for
;; git deps. Offline: drives the registry directly rather than a real fetch.
(let [note (var jolt.deps/note-unresolvable!)
      fail (var jolt.deps/fail-unresolvable!)
      unres (var jolt.deps/*unresolvable*)]
  ;; nothing recorded: resolution proceeds
  (is= "no unresolvable deps does not throw" true
       (with-bindings {unres (atom [])} (fail) true))
  ;; one recorded: throws, and the message names the coordinate and the reason
  (let [msg (with-bindings {unres (atom [])}
              (note 'org.clojure/spec.alpha "0.5.238" "could not be fetched: reset")
              (try (fail) nil (catch :default e (ex-message e))))]
    (is= "an unresolvable dep aborts resolution" true (some? msg))
    (is= "the message names the coordinate" true
         (str/includes? (str msg) "org.clojure/spec.alpha 0.5.238"))
    (is= "the message keeps the reason, not a generic 'not found'" true
         (str/includes? (str msg) "could not be fetched: reset"))
    (is= "the message is the tools.deps one" true
         (str/includes? (str msg) "could not be resolved")))
  ;; every failure is reported together, not one build at a time
  (let [msg (with-bindings {unres (atom [])}
              (note 'a/one "1.0" "not found in any repository (tried r1)")
              (note 'b/two "2.0" "could not be fetched: 503")
              (try (fail) nil (catch :default e (ex-message e))))]
    (is= "both unresolvable deps are named at once" true
         (and (str/includes? (str msg) "a/one 1.0")
              (str/includes? (str msg) "b/two 2.0")))))

(println (str "deps-expand: " (- @checks @failures) "/" @checks " passed"))
(when (pos? @failures)
  (System/exit 1))
