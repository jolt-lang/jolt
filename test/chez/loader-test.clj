;; Loader gate: require :reload / :reload-all, failed-load rollback, and that a
;; data-reader fn whose var resolves must surface a throw (not silently degrade
;; to a runtime call). Writes its own scratch ns files under a temp dir, prepends
;; that dir to the source roots, then requires them.
;; Run: bin/joltc run test/chez/loader-test.clj (smoke.sh greps for "LOADER OK").
(ns loader-test
  (:require [jolt.fs :as fs]))

(def failures (atom []))
(defn chk [label ok] (when-not ok (swap! failures conj label)))

(defn vof [ns-sym var-sym]
  (when-let [c (ns-resolve ns-sym var-sym)] (deref c)))

;; scratch source root, prepended to the live roots; set-roots! also re-scans for
;; data_readers so the reader sub-test can register a tag mid-run.
(def root (str (fs/create-temp-dir {:prefix "loader-gate-"})))
(defn set-roots! []
  (jolt.host/set-source-roots!
    (vec (distinct (concat [root] (jolt.host/source-roots))))))

;; --- (1) :reload picks up an edit ----------------------------------------
(spit (str root "/a.clj") "(ns a) (def v 1)")
(set-roots!)
(require 'a)
(chk ":reload: initial load defines a/v" (= (vof 'a 'v) 1))
(spit (str root "/a.clj") "(ns a) (def v 2)")
(require 'a :reload)
(chk ":reload: edit is visible after (require 'a :reload)" (= (vof 'a 'v) 2))

;; --- (2) a namespace whose load THROWS is not left marked loaded ----------
(spit (str root "/b.clj") "(ns b) (throw (ex-info \"B-BOOM\" {}))")
(let [e (try (require 'b) (catch :default e (in-ns 'loader-test) e))]
  (chk "rollback: first load surfaces B-BOOM" (= (ex-message e) "B-BOOM")))
(chk "rollback: b/v undefined while b throws" (nil? (vof 'b 'v)))
(spit (str root "/b.clj") "(ns b) (def v :fixed)")
(require 'b)                                   ; plain require, no :reload
(chk "rollback: plain require reloads after the file is fixed" (= (vof 'b 'v) :fixed))

;; --- (3) :reload-all reloads a dependency chain transitively --------------
(spit (str root "/d.clj") "(ns d) (def v 1)")
(spit (str root "/c.clj") "(ns c (:require [d :as d]))")
(require 'c)
(chk ":reload-all: initial load defines d/v" (= (vof 'd 'v) 1))
(spit (str root "/d.clj") "(ns d) (def v 2)")
(require 'c :reload-all)
(chk ":reload-all: dep d reloaded transitively" (= (vof 'd 'v) 2))

;; --- (4) a data-reader fn that resolves but throws surfaces ---------------
;; useboom's #boom sits inside an uncalled fn body, so a silent fallback to a
;; runtime call would let the require SUCCEED. With the guard narrowed, the
;; resolved reader runs at load and its throw surfaces (naming the tag).
(spit (str root "/rdr.clj") "(ns rdr) (defn boom [form] (throw (ex-info \"READER-BOOM\" {})))")
(spit (str root "/data_readers.clj") "{boom rdr/boom}")
(set-roots!)                                    ; re-scan picks up data_readers.clj
(spit (str root "/useboom.clj") "(ns useboom) (defn f [] #boom [:x])")
(let [e (try (require 'useboom) (catch :default e (in-ns 'loader-test) e))
      msg (ex-message e)]
  (chk "reader-throw: load surfaces the tag" (and msg (re-find #"boom" msg)))
  (chk "reader-throw: load surfaces the reader's message" (and msg (re-find #"READER-BOOM" msg))))

;; --- (5) a LIST libspec (jolt superset) loads + refers, not a prefix list ----
;; (use '(e :only [v])) — the list (e :only [v]) has a KEYWORD second element, so
;; it is a libspec (target e, :only [v]), NOT the prefix-list form. On the JVM
;; this shape throws; jolt accepts it. expand-spec used to misread it as a prefix
;; list and try to load e.v.
(spit (str root "/e.clj") "(ns e) (def v :e-v) (def w :e-w)")
(use '(e :only [v]))
(chk "list-libspec: target e is loaded" (= (vof 'e 'v) :e-v))
(chk "list-libspec: :only [v] refers v into the current ns" (= (vof 'loader-test 'v) :e-v))
(chk "list-libspec: :only filters out w (not referred)" (nil? (vof 'loader-test 'w)))

;; --- (6) the prefix-list form still works (JVM parity) ----------------------
;; (require '(pfx [leaf :as lf])) — list whose second element is a VECTOR (not a
;; keyword), so it stays a prefix list: loads pfx.leaf, alias lf.
(.mkdirs (java.io.File. (str root "/pfx")))
(spit (str root "/pfx/leaf.clj") "(ns pfx.leaf) (def x :leaf-x)")
(require '(pfx [leaf :as lf]))
(chk "prefix-list: pfx.leaf is loaded" (= (vof 'pfx.leaf 'x) :leaf-x))
(chk "prefix-list: alias lf resolves pfx.leaf/x" (= lf/x :leaf-x))

(if (empty? @failures)
  (println "LOADER OK")
  (doseq [f @failures] (println "FAIL:" f)))
