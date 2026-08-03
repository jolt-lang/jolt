(ns grenadine-test
  (:require [grenadine.graph :as graph]
            [jolt.deps :as deps]))

(defn- coord
  [artifact version]
  {:group "demo" :artifact artifact :version version})

(def poms
  {["a" "1"] {:deps [(coord "c" "1")]}
   ["b" "1"] {:deps [(coord "c" "2")]}
   ["c" "1"] {:deps [(coord "only-old" "1")]}
   ["c" "2"] {:deps []}
   ["only-old" "1"] {:deps []}})

(defn- pom-fn
  [{:keys [artifact version]}]
  (or (get poms [artifact version])
      (throw (ex-info "missing fixture POM"
                      {:artifact artifact :version version}))))

(let [resolution
      (graph/resolve-graph
       {'demo/a {:mvn/version "1"}
        'demo/b {:mvn/version "1"}}
       {:pom-fn pom-fn :mediation :tools-deps})]
  (when-not (= "2" (get-in resolution [:selected ["demo" "c"]
                                       :coords :version]))
    (throw (ex-info "Grenadine selected the wrong version"
                    {:resolution resolution})))
  (when (get-in resolution [:selected ["demo" "only-old"]])
    (throw (ex-info "Grenadine retained a losing-version subtree"
                    {:resolution resolution}))))

(def effective-poms
  {["demo" "parent" "1"]
   "<project>
      <modelVersion>4.0.0</modelVersion>
      <groupId>demo</groupId><artifactId>parent</artifactId><version>1</version>
      <properties><managed.version>2</managed.version></properties>
      <dependencyManagement><dependencies>
        <dependency>
          <groupId>demo</groupId><artifactId>managed</artifactId>
          <version>${managed.version}</version>
        </dependency>
      </dependencies></dependencyManagement>
    </project>"

   ["demo" "child" "1"]
   "<project>
      <modelVersion>4.0.0</modelVersion>
      <parent>
        <groupId>demo</groupId><artifactId>parent</artifactId><version>1</version>
      </parent>
      <artifactId>child</artifactId>
      <dependencies>
        <dependency><groupId>demo</groupId><artifactId>managed</artifactId></dependency>
        <dependency>
          <groupId>org.clojure</groupId><artifactId>clojure</artifactId>
          <version>1.9.0</version>
        </dependency>
      </dependencies>
    </project>"})

(defn- effective-pom-text
  [{:keys [group artifact version]}]
  (get effective-poms [group artifact version]))

(with-redefs-fn
  {(var jolt.deps/pom-text) effective-pom-text}
  (fn []
    (let [raw (@#'deps/effective-pom-deps
               'demo/child {:mvn/version "1"})
          filtered (into {} (@#'deps/filter-deps raw "."))]
      ;; Clojure itself is pruned — jolt IS Clojure, and putting the artifact's
      ;; source on the roots would shadow core. Its spec children are NOT pruned:
      ;; spec.alpha and core.specs.alpha are part of core on neither host, and on
      ;; the JVM they arrive with the Clojure coordinate. Versions are read off the
      ;; declared Clojure's own POM; this fixture redefines pom-text and so has no
      ;; POM for clojure 1.9.0, which exercises the fallback — hence asserting on
      ;; which libs appear rather than on their versions.
      (when-not (and (= {:mvn/version "2"} (get filtered 'demo/managed))
                     (nil? (get filtered 'org.clojure/clojure))
                     (contains? filtered 'org.clojure/spec.alpha)
                     (contains? filtered 'org.clojure/core.specs.alpha))
        (throw
         (ex-info (str "Jolt did not use Grenadine's effective POM, prune Clojure, "
                       "or carry Clojure's spec dependencies")
                  {:raw raw :filtered filtered}))))))

;;;; A POM Grenadine cannot model degrades to the jar's own pom.xml rather than
;;;; failing the whole resolution.

(defn- deps-of
  "effective-pom-deps for a lib whose POM text comes from `poms-by-coords`."
  [poms-by-coords lib]
  (with-redefs-fn
    {(var jolt.deps/pom-text)
     (fn [{:keys [group artifact version]}]
       (or (get poms-by-coords [group artifact version])
           (throw (ex-info (str "POM not found: " group "/" artifact " " version)
                           {:type :jolt.deps/pom-not-found}))))}
    (fn [] (@#'deps/effective-pom-deps lib {:mvn/version "1"}))))

;; A jar sitting in the local repository without its .pom beside it — installed
;; by hand, or fetched before the machine went offline. Its transitive deps are
;; unknown, not a reason to abandon the resolution.
(when-not (nil? (deps-of {} 'demo/unfetchable))
  (throw (ex-info "an unfetchable POM should degrade to nil, not deps" {})))

;; An unresolved ${property} anywhere in the POM — commonly a version defined
;; only in a <profile>, and just as commonly on a test-scoped dependency jolt
;; drops anyway. Grenadine asserts every declared coordinate before jolt gets to
;; filter by scope, so the whole POM degrades.
(when-not (nil? (deps-of
                 {["demo" "unresolved" "1"]
                  "<project>
                     <modelVersion>4.0.0</modelVersion>
                     <groupId>demo</groupId>
                     <artifactId>unresolved</artifactId><version>1</version>
                     <dependencies>
                       <dependency>
                         <groupId>junit</groupId><artifactId>junit</artifactId>
                         <version>${junit.version}</version><scope>test</scope>
                       </dependency>
                     </dependencies>
                   </project>"}
                 'demo/unresolved))
  (throw (ex-info "an unresolvable ${property} should degrade to nil, not deps" {})))

;; ...and a degraded lib still reports whatever pom.xml its jar carries.
(let [pom (str (System/getProperty "java.io.tmpdir") "/jolt-grenadine-fallback.xml")]
  (spit pom "<project><dependencies>
               <dependency>
                 <groupId>demo</groupId><artifactId>packaged</artifactId>
                 <version>3</version>
               </dependency>
             </dependencies></project>")
  (let [children (@#'deps/children-of
                  {:root "." :manifest :mvn :deps nil :pom pom})]
    (when-not (= [['demo/packaged {:mvn/version "3"}]] (vec children))
      (throw (ex-info "a degraded Maven dep should fall back to its jar's pom.xml"
                      {:children children})))))

(println "grenadine gate: passed")
