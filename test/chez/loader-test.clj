;; Loader gate: require :reload / :reload-all, failed-load rollback, and that a
;; data-reader fn whose var resolves must surface a throw (not silently degrade
;; to a runtime call). Writes its own scratch ns files under a temp dir, prepends
;; that dir to the source roots, then requires them.
;; Run: bin/jolt run test/chez/loader-test.clj (smoke.sh greps for "LOADER OK").
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
  ;; as RT.load does: a CompilerException naming b.clj, the throw its cause
  (chk "rollback: first load surfaces B-BOOM" (= (ex-message (ex-cause e)) "B-BOOM")))
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

;; --- (3b) :reload-all reloads each lib ONCE, however many libs require it ---
;; clojure.core binds a fresh *loaded-libs* for the extent, so a shared dependency
;; reloads once and is then loaded again. jolt turned the dedup off for the whole
;; extent instead, so every require site reloaded it again — a diamond doubled the
;; work per level, and a real dependency graph ran for tens of minutes.
(def reloads (atom 0))
(.mkdirs (java.io.File. (str root "/dmd")))
(spit (str root "/dmd/base.clj") "(ns dmd.base) (swap! loader-test/reloads inc)")
(doseq [x ["l" "r"]]
  (spit (str root "/dmd/" x "1.clj") (str "(ns dmd." x "1 (:require dmd.base))"))
  (spit (str root "/dmd/" x "2.clj") (str "(ns dmd." x "2 (:require dmd.l1 dmd.r1))")))
(spit (str root "/dmd/top.clj") "(ns dmd.top (:require dmd.l2 dmd.r2))")
(require 'dmd.top)
(chk ":reload-all once: initial load runs base once" (= @reloads 1))
(reset! reloads 0)
(require 'dmd.top :reload-all)
(chk (str ":reload-all once: base reloaded exactly once (was " @reloads ")") (= @reloads 1))
(reset! reloads 0)
(require 'dmd.top :reload-all)
(chk ":reload-all once: a second :reload-all reloads base again" (= @reloads 1))

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

;; --- (7) requiring a namespace with no source file surfaces a clean error ----
;; The load-namespace* "else" branch used to pass a stray 3rd arg to throw-jvm
;; (a strictly 2-arg proc), so any un-locatable ns crashed with an uncatchable
;; "incorrect number of arguments 3 to throw-jvm" instead of naming the missing
;; file. A missing transitive dep must surface as a catchable, named error.
(let [e (try (require 'no.such.namespace.xyz)
             (catch :default e (in-ns 'loader-test) e))
      msg (ex-message e)]
  (chk "missing-ns: require surfaces a catchable error" (some? msg))
  (chk "missing-ns: error names the missing file"
       (and msg (re-find #"no/such/namespace/xyz" msg) (re-find #"Could not locate" msg))))

;; --- (8) a nested require under (binding [*ns* ..]) interns into its own ns ----
;; chez-current-ns prefers a live (binding [*ns* ..]) over the thread parameter,
;; but set-chez-ns! used to write only the parameter — so ldr-load-body's restore
;; of the current ns on the way out of a load did nothing under such a binding,
;; and *ns* stayed pointing at the LAST file the require finished. Every def
;; after the :require in the requiring file then interned into the wrong
;; namespace. typedclojure's ann* macroexpands under exactly such a binding, so
;; its whole dependency tree loaded wrong.
;; Every answer below probed against JVM Clojure 1.12 first.
(spit (str root "/nfx.clj") "(ns nfx) (defn nfx-fn [] :ok)")
(spit (str root "/nfy.clj") "(ns nfy (:require [nfx :as d])) (defn nfy-fn [] (d/nfx-fn))")
(set-roots!)
(binding [*ns* (the-ns 'loader-test)]
  (require 'nfy)
  (chk "ns-binding: the load restores *ns* to the bound one" (= "loader-test" (str *ns*))))
(chk "ns-binding: nested require's ns exists" (some? (find-ns 'nfx)))
(chk "ns-binding: nested def interns in its own ns" (some? (ns-resolve 'nfx 'nfx-fn)))
(chk "ns-binding: def did not leak into the bound ns" (nil? (ns-resolve 'loader-test 'nfx-fn)))
(chk "ns-binding: requiring ns's def interns in its own ns" (some? (ns-resolve 'nfy 'nfy-fn)))
(chk "ns-binding: cross-ns call works"
     (= :ok (when-let [v (ns-resolve 'nfy 'nfy-fn)] ((deref v)))))

;; --- (9) load-file and load put *ns* back after the file's ns form ---------
;; Compiler.load binds *ns* for the duration, so a loaded file's `ns` form never
;; leaks into the caller. jolt left *ns* on the loaded file's namespace, and the
;; caller's next form resolved its aliases in the wrong place. Probed against
;; JVM Clojure 1.12: both stay in the caller's namespace.
(spit (str root "/lfa.clj") "(ns lfa) (def v :file)")
(load-file (str root "/lfa.clj"))
(chk "load-file: *ns* is the caller's afterwards" (= "loader-test" (str *ns*)))
(chk "load-file: the file's def landed in its ns" (= :file (vof 'lfa 'v)))
(spit (str root "/lfb.clj") "(ns lfb) (def v :load)")
(load "/lfb")
(chk "load: *ns* is the caller's afterwards" (= "loader-test" (str *ns*)))
(chk "load: the file's def landed in its ns" (= :load (vof 'lfb 'v)))
(spit (str root "/lfc.clj") "(ns lfc) (throw (ex-info \"LFC\" {}))")
(try (load-file (str root "/lfc.clj")) (catch :default _ nil))
(chk "load-file: *ns* is restored when the file throws" (= "loader-test" (str *ns*)))
;; and it answers the value of the file's last form, as Compiler.load does
;; (jolt answered nil); an empty file answers nil
(spit (str root "/lfd.clj") "(def lfd-x 1)\n(+ lfd-x 2)\n(* 3 4)")
(spit (str root "/lfe.clj") "")
(chk "load-file: answers the last form's value" (= 12 (load-file (str root "/lfd.clj"))))
(chk "load-file: an empty file answers nil" (nil? (load-file (str root "/lfe.clj"))))

;; --- (10) :reload-all reloads the image's stdlib into ITS OWN namespaces ----
;; clojure.string, clojure.walk and clojure.set are baked into the seed and had
;; no ns form, so a :reload-all that reached them evaluated their source in the
;; REQUIRING namespace: their defns landed there, and a macro it shadows (a `let`
;; that does not expand in tail position) broke clojure.string's trim-newline
;; with "Can only recur from tail position".
(spit (str root "/shadowlet.clj")
      (str "(ns shadowlet (:refer-clojure :exclude [let])"
           " (:require [clojure.string :as s] [clojure.walk :as w] [clojure.set :as st]))"
           " (defmacro let [bs & body] `(do (clojure.core/let ~bs ~@body) nil))"
           " (defn f [] (s/upper-case \"x\"))"))
(require 'shadowlet)
(let [e (try (require 'shadowlet :reload-all) nil
             (catch :default e (in-ns 'loader-test) e))]
  (chk (str "reload-all-stdlib: reload succeeds (" (when e (ex-message e)) ")") (nil? e)))
(chk "reload-all-stdlib: *ns* is the caller's afterwards" (= "loader-test" (str *ns*)))
(chk "reload-all-stdlib: clojure.string defs did not land in the requirer"
     (nil? (ns-resolve 'shadowlet 'trim-newline)))
(chk "reload-all-stdlib: clojure.walk defs did not land in the requirer"
     (nil? (ns-resolve 'shadowlet 'postwalk)))
(chk "reload-all-stdlib: clojure.set defs did not land in the requirer"
     (nil? (ns-resolve 'shadowlet 'difference)))
(chk "reload-all-stdlib: clojure.string still works"
     (= "ab" (clojure.string/trim-newline "ab\r\n")))
(chk "reload-all-stdlib: the requirer still works"
     (= "X" (when-let [v (ns-resolve 'shadowlet 'f)] ((deref v)))))

;; --- (11) an unqualified `ns` / `in-ns` always means clojure.core's ---------
;; A file's ns form is read in the REQUIRER's namespace. sci.core excludes `ns`
;; and defines its own, so when :reload-all re-ran its ns form and that loaded
;; clojure.tools.reader.reader-types, the dependency's `(ns ...)` resolved to
;; sci.core/ns and failed "Unable to resolve symbol". The JVM's Compiler.lookupVar
;; and resolve special-case both names to clojure.core, whatever the ns maps.
(spit (str root "/ownnsdep.clj") "(ns ownnsdep) (def v :dep)")
(spit (str root "/ownns.clj")
      (str "(ns ownns (:refer-clojure :exclude [ns in-ns]) (:require ownnsdep))"
           " (defn ns [] :own) (defn in-ns [] :own) (def v :ownns)"))
(require 'ownns)
(let [e (try (require 'ownns :reload-all) nil
             (catch :default e (in-ns 'loader-test) e))]
  (chk (str "own-ns: a ns that shadows `ns` reloads its deps (" (when e (ex-message e)) ")")
       (nil? e)))
(chk "own-ns: *ns* is the caller's afterwards" (= "loader-test" (str *ns*)))
(chk "own-ns: its defs are intact" (= :ownns (vof 'ownns 'v)))
(chk "own-ns: the dep reloaded into its own ns" (= :dep (vof 'ownnsdep 'v)))
(chk "own-ns: ns-resolve of ns is clojure.core's" (= #'clojure.core/ns (ns-resolve 'ownns 'ns)))
(chk "own-ns: ns-resolve of in-ns is clojure.core's"
     (= #'clojure.core/in-ns (ns-resolve 'ownns 'in-ns)))

(if (empty? @failures)
  (println "LOADER OK")
  (doseq [f @failures] (println "FAIL:" f)))
