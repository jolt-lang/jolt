;; The loader conformance suite. These cases ARE the specification of
;; jolt.loader — per-context roots, isolation, delegation policy, unload,
;; concurrent loads, and the host root — the way test/chez/corpus.edn is the
;; specification of clojure.core. They are written against the public API only,
;; so a rewrite underneath is free as long as the suite stays green.
;;
;; Run: bin/jolt run test/chez/loaderconf-test.clj  (make loaderconf gates it
;; against test/chez/loaderconf-known-failures.txt).
;;
;; Each case prints one CASE line; a case passes only if every check in it
;; passed and it did not throw. The gate compares the PASS/FAIL set against the
;; recorded baseline, so a case going green fails the gate until the baseline
;; says so, and a case going red fails it always.
(ns loaderconf-test
  (:require [jolt.loader :as l]
            [jolt.fs :as fs]
            [jolt.host :as host]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async]))

(def cases (atom []))
(def failures (atom []))

(defn chk [label ok] (when-not ok (swap! failures conj label)))

(defmacro defcase [n title & body]
  `(swap! cases conj [~n ~title (fn [] ~@body)]))

;; --- scratch roots ----------------------------------------------------------
;; Each case builds its own directories; a "library at two versions" is two
;; directories holding the same namespace name with different contents, which is
;; what a version-qualified extraction dir already gives us on disk.
(def tmp (str (fs/create-temp-dir {:prefix "loaderconf-"})))

(defn root-dir [name]
  (let [d (str tmp "/" name)]
    (.mkdirs (java.io.File. d))
    d))

(defn write! [dir file src]
  (spit (str dir "/" file) src)
  dir)

(defn val-of
  "The value a :var resolution answers with — the cell's current root."
  [cell]
  (when cell (deref cell)))

;; --- 1. two contexts, one library at two versions ---------------------------
(defcase 1 "v1/v2 isolation: two contexts hold one library at two versions"
  (let [d1 (write! (root-dir "v1") "libx.clj" "(ns libx) (defn version [] :v1)")
        d2 (write! (root-dir "v2") "libx.clj" "(ns libx) (defn version [] :v2)")
        c1 (l/classpath [d1] {:parent (l/root)})
        c2 (l/classpath [d2] {:parent (l/root)})]
    (l/load c1 {:kind :ns :name "libx"})
    (l/load c2 {:kind :ns :name "libx"})
    (let [v1 (l/resolve c1 {:kind :var :name "libx/version"})
          v2 (l/resolve c2 {:kind :var :name "libx/version"})]
      (chk "each context links libx/version" (and (some? v1) (some? v2)))
      (chk "the two links are different cells" (not (identical? v1 v2)))
      (chk "context 1 sees v1" (= :v1 ((val-of v1))))
      (chk "context 2 sees v2" (= :v2 ((val-of v2)))))))

;; --- 2. hermetic ------------------------------------------------------------
(defcase 2 "hermetic: an isolated context cannot see what the root has"
  (let [d (write! (root-dir "herm") "libh.clj" "(ns libh) (def marker :own)")
        ctx (l/classpath [d] {:parent (l/isolated)})]
    (chk "a namespace only the root has does not resolve"
         (empty? (l/find ctx {:kind :ns :name "clojure.string"})))
    (chk "a var only the root has does not resolve"
         (empty? (l/find ctx {:kind :var :name "clojure.core/inc"})))
    (chk "the context's own namespace still resolves"
         (seq (l/find ctx {:kind :ns :name "libh"})))))

;; --- 3. shared by reference -------------------------------------------------
;; The cell, not a copy: injecting a namespace into a context by handing it the
;; host's vars depends on this, and identical? is the only way to say it.
(defcase 3 "shared by reference: a delegated var is the same cell"
  (let [d (root-dir "shared")
        ctx (l/classpath [d] {:parent (l/root)})
        req {:kind :var :name "clojure.core/inc"}
        root-cell (:cell (first (l/find (l/root) req)))
        ctx-cell (:cell (first (l/find ctx req)))]
    (chk "the root answers with a cell" (some? root-cell))
    (chk "the delegating context answers with a cell" (some? ctx-cell))
    (chk "both answers are one object" (identical? root-cell ctx-cell))
    (chk "and that object is the live var" (identical? ctx-cell #'clojure.core/inc))))

;; --- 4. deny is not a miss --------------------------------------------------
(defcase 4 "deny is not a miss: a denied name raises and never falls through"
  (let [d (write! (root-dir "denied") "libd.clj" "(ns libd) (def marker :own)")
        ctx (l/deny (l/classpath [d] {:parent (l/root)}) #{'libd})
        req {:kind :ns :name "libd"}
        e (try (l/find ctx req) nil (catch :default e e))]
    (chk "find on a denied name throws" (some? e))
    (chk "the throw carries :loader/denied" (true? (:loader/denied (ex-data e))))
    (chk "the throw names the request"
         (= [:ns "libd"] [(:kind (ex-data e)) (:name (ex-data e))]))
    (chk "load on a denied name throws too"
         (some? (try (l/load ctx req) nil (catch :default e e))))
    (chk "the context's own roots did not answer instead"
         (nil? (l/resolve ctx {:kind :var :name "libd/marker"})))))

;; --- 5. self-first ----------------------------------------------------------
(defcase 5 "self-first: own roots shadow the delegate, for the declared prefix only"
  (let [du (-> (root-dir "sf-up")
               (write! "libs1.clj" "(ns libs1) (defn who [] :delegate)")
               (write! "libs2.clj" "(ns libs2) (defn who [] :delegate)"))
        dd (-> (root-dir "sf-own")
               (write! "libs1.clj" "(ns libs1) (defn who [] :own)")
               (write! "libs2.clj" "(ns libs2) (defn who [] :own)"))
        ctx (l/self-first (l/classpath [dd] {:parent (l/classpath [du])}) #{'libs1})]
    (l/load ctx {:kind :ns :name "libs1"})
    (l/load ctx {:kind :ns :name "libs2"})
    (chk "the declared prefix comes from the context's own roots"
         (= :own ((val-of (l/resolve ctx {:kind :var :name "libs1/who"})))))
    (chk "every other name still comes from the delegate"
         (= :delegate ((val-of (l/resolve ctx {:kind :var :name "libs2/who"})))))
    (chk "the delegate is still consulted for the declared prefix"
         (seq (l/find ctx {:kind :ns :name "libs1"})))))

;; --- 6. composed delegates --------------------------------------------------
(defcase 6 "composed delegates: a host plus a pool, and parent walks the graph"
  (let [da (write! (root-dir "pool-a") "liba.clj" "(ns liba) (def marker :a)")
        db (write! (root-dir "pool-b") "libb.clj" "(ns libb) (def marker :b)")
        dc (write! (root-dir "pool-c") "libc.clj" "(ns libc) (def marker :c)")
        ctx (l/classpath [dc]
                         {:parent (l/delegating (l/root)
                                                (l/pool [(l/classpath [da])
                                                         (l/classpath [db])]))})]
    (chk "a name from the first pool member resolves"
         (seq (l/find ctx {:kind :ns :name "liba"})))
    (chk "a name from the second pool member resolves"
         (seq (l/find ctx {:kind :ns :name "libb"})))
    (chk "a name from the context's own roots resolves"
         (seq (l/find ctx {:kind :ns :name "libc"})))
    (chk "a name from the host still resolves"
         (seq (l/find ctx {:kind :ns :name "clojure.string"})))
    (chk "parent walks to the delegate" (some? (l/parent ctx)))
    (let [chain (take-while some? (iterate l/parent ctx))]
      (chk "the parent chain terminates" (< (count chain) 32))
      (chk "the parent chain reaches the composed delegate" (> (count chain) 1)))))

;; --- 7. unload --------------------------------------------------------------
(defcase 7 "unload: no new loads, resolved definitions stay live, idempotent"
  (let [d (-> (root-dir "unl")
              (write! "libu.clj" "(ns libu) (defn who [] :u)")
              (write! "libu2.clj" "(ns libu2) (def marker :u2)"))
        ctx (l/classpath [d] {:parent (l/root)})]
    (l/load ctx {:kind :ns :name "libu"})
    (let [who (val-of (l/resolve ctx {:kind :var :name "libu/who"}))
          report (l/unload! ctx)]
      (chk "unload! reports the postcondition held" (true? (:unloaded report)))
      (chk "unload! is not reported as a repeat" (false? (:already report)))
      (chk "teardown errors are reported as data" (vector? (:errors report)))
      (chk "unload! reports no teardown errors here" (empty? (:errors report)))
      (chk "unloaded? is the behavioral predicate" (true? (l/unloaded? ctx)))
      (chk "a definition resolved before the unload still works" (= :u (who)))
      (chk "a new load through the loader throws"
           (some? (try (l/load ctx {:kind :ns :name "libu2"}) nil (catch :default e e))))
      (chk "find after unload throws"
           (some? (try (l/find ctx {:kind :ns :name "libu2"}) nil (catch :default e e))))
      (let [again (l/unload! ctx)]
        (chk "the second unload! is a no-op" (true? (:already again)))
        (chk "the second unload! still reports the postcondition" (true? (:unloaded again)))))
    ;; teardown against a root that vanished underneath reports, never throws
    (let [gone (root-dir "unl-gone")
          ctx2 (l/classpath [gone] {:parent (l/root)})]
      (fs/delete-tree gone)
      (chk "teardown over a vanished root is reported, not thrown"
           (map? (l/unload! ctx2))))))

;; --- 8. resources -----------------------------------------------------------
(defcase 8 "resources: find/open-hit and the classloader facade stay in the context"
  (let [d (write! (root-dir "res") "cfg.edn" "{:from :ctx}")
        ctx (l/classpath [d] {:parent (l/root)})
        hits (l/find ctx {:kind :resource :name "cfg.edn"})
        cl (l/as-classloader ctx)]
    (chk "find locates the resource" (seq hits))
    (chk "the hit names a location, not an open stream" (string? (:url (first hits))))
    (chk "open-hit reads it" (= "{:from :ctx}" (slurp (l/open-hit ctx (first hits)))))
    (chk "getResource resolves in the context" (some? (.getResource cl "cfg.edn")))
    (chk "getResources resolves in the context"
         (seq (enumeration-seq (.getResources cl "cfg.edn"))))
    (chk "getResourceAsStream reads it"
         (= "{:from :ctx}" (slurp (.getResourceAsStream cl "cfg.edn"))))
    (chk "getParent is the delegate's facade" (some? (.getParent cl)))
    (chk "a loader has one facade, so getParent chains and identity hold"
         (identical? cl (l/as-classloader ctx)))
    (chk "the 2-arity io/resource honors the loader" (some? (io/resource "cfg.edn" cl)))
    (chk "the root does not see the context's resource" (nil? (io/resource "cfg.edn")))
    (chk "RT/baseLoader inside the context is the context's loader"
         (identical? cl (l/with-loader ctx (clojure.lang.RT/baseLoader))))))

;; --- 9. defining context ----------------------------------------------------
;; The load-bearing rule: a function requires in the context that DEFINED it,
;; whoever calls it, on whatever thread, and across a fiber park.
(defcase 9 "defining context: a fn requires where it was defined, not where it is called"
  (let [d1 (-> (root-dir "def1")
               (write! "dep.clj" "(ns dep) (def which :ctx1)")
               (write! "caller.clj"
                       (str "(ns caller)\n"
                            "(defn peek-dep [] (require 'dep) @(ns-resolve 'dep 'which))\n"
                            "(defn own [] :own)\n"
                            "(defn peek-own [] ((resolve 'own)))\n"
                            "(defn peek-own-var [] ((find-var 'own)))")))
        d2 (write! (root-dir "def2") "dep.clj" "(ns dep) (def which :ctx2)")
        c1 (l/classpath [d1] {:parent (l/root)})
        c2 (l/classpath [d2] {:parent (l/root)})]
    (l/load c1 {:kind :ns :name "caller"})
    (let [f (val-of (l/resolve c1 {:kind :var :name "caller/peek-dep"}))
          own (val-of (l/resolve c1 {:kind :var :name "caller/peek-own"}))
          own-var (val-of (l/resolve c1 {:kind :var :name "caller/peek-own-var"}))]
      (chk "called from the root, the fn sees its own context's dep" (= :ctx1 (f)))
      (chk "called inside another context, it still sees its own"
           (= :ctx1 (l/with-loader c2 (f))))
      (chk "called from a thread in another context, it still sees its own"
           (= :ctx1 @(future (l/with-loader c2 (f)))))
      (chk "called from a fiber in another context, it still sees its own"
           (= :ctx1 (async/<!! (async/go (l/with-loader c2 (f))))))
      (chk "an unqualified (resolve 'sym) resolves in the defining context"
           (= :own (own)))
      (chk "and so does an unqualified (find-var 'sym)"
           (= :own (own-var)))
      (chk "even when called inside another context"
           (= :own (l/with-loader c2 (own)))))))

;; --- 10. dispatch follows the value -----------------------------------------
(defcase 10 "dispatch follows the value: a context-2 value dispatches in context 2"
  (let [ds (write! (root-dir "disp-shared") "shp.clj"
                   "(ns shp) (defprotocol Shape (area [s]))")
        d2 (write! (root-dir "disp2") "sq.clj"
                   (str "(ns sq (:require [shp]))\n"
                        "(defrecord Square [n])\n"
                        "(extend-type Square shp/Shape (area [s] (* (:n s) (:n s))))\n"
                        "(defn make [n] (->Square n))"))
        d1 (write! (root-dir "disp1") "caller1.clj"
                   "(ns caller1 (:require [shp]))\n(defn call-area [v] (shp/area v))")
        shared (l/classpath [ds] {:parent (l/root)})
        c1 (l/classpath [d1] {:parent shared})
        c2 (l/classpath [d2] {:parent shared})]
    (l/load c2 {:kind :ns :name "sq"})
    (l/load c1 {:kind :ns :name "caller1"})
    (let [make (val-of (l/resolve c2 {:kind :var :name "sq/make"}))
          call (val-of (l/resolve c1 {:kind :var :name "caller1/call-area"}))
          v (make 3)]
      (chk "context 1 code dispatches a context 2 value through context 2's tables"
           (= 9 (call v)))
      (chk "the protocol itself is shared by reference"
           (identical? (l/resolve c1 {:kind :var :name "shp/area"})
                       (l/resolve c2 {:kind :var :name "shp/area"}))))))

;; --- 11. hits are data ------------------------------------------------------
(defcase 11 "hits are data: find opens nothing and a hit round-trips into load"
  (let [d (write! (root-dir "hits") "libp.clj" "(ns libp) (def marker :p)")
        ctx (l/classpath [d] {:parent (l/root)})
        req {:kind :ns :name "libp"}
        hit (first (l/find ctx req))]
    (chk "find answers a hit" (some? hit))
    (chk "the hit prints" (string? (pr-str hit)))
    (chk "two finds answer equal hits" (= hit (first (l/find ctx req))))
    ;; the located path, not a normalized or resolved one — a temp dir reaches
    ;; here through a symlink on macOS, so compare the tail
    (chk "the hit names the file it located"
         (and (string? (:file hit)) (str/ends-with? (:file hit) "/libp.clj")))
    (chk "find installed nothing"
         (nil? (l/resolve ctx {:kind :var :name "libp/marker"})))
    (chk "loading a hit is loading the request"
         (= (l/load ctx hit) (l/load ctx req)))))

;; --- 12. eager construction, lazy load --------------------------------------
(defcase 12 "eager construction, lazy load: the constructor validates, load reads"
  (let [missing (str tmp "/absent-root")]
    (chk "a loader over an unreadable root fails at the constructor"
         (some? (try (l/classpath [missing]) nil (catch :default e e))))
    (let [d (write! (root-dir "toctou") "libt.clj" "(ns libt) (def marker :t)")
          ctx (l/classpath [d] {:parent (l/root)})
          hit (first (l/find ctx {:kind :ns :name "libt"}))]
      (chk "find located it" (some? hit))
      (fs/delete (str d "/libt.clj"))
      (chk "a load whose file vanished after find fails at load, not silently"
           (some? (try (l/load ctx hit) nil (catch :default e e)))))))

;; --- 13. concurrent loads of one name ---------------------------------------
;; The evict-evaluate window mutates process-global state, so loads of ONE name
;; are serialized by name across loaders: several contexts racing on the same
;; name must each end up with their own namespace — not a half-built one, and
;; not each other's.
(defcase 13 "concurrent private loads: one name, several contexts, no cross-talk"
  (let [n 6
        ctxs (mapv (fn [i]
                     (l/classpath [(write! (root-dir (str "conc" i)) "libc.clj"
                                           (str "(ns libc) (Thread/sleep 10)"
                                                " (defn who [] " i ")"))]
                                {:parent (l/root)}))
                   (range n))
        futures (mapv (fn [c]
                        (future
                          (l/load c {:kind :ns :name "libc"})
                          ((val-of (l/resolve c {:kind :var :name "libc/who"})))))
                      ctxs)
        vals (mapv deref futures)
        cells (mapv #(l/resolve % {:kind :var :name "libc/who"}) ctxs)]
    (chk "every context loaded and called its own version" (= vals (vec (range n))))
    (chk "the links are per-context cells"
         (not (identical? (nth cells 0) (nth cells 1))))))

;; --- 14. unload releases the global slot ------------------------------------
;; unload! stops new loads AND releases the name this loader installed (while it
;; is still the loader's), and definitions already resolved stay live: compiled
;; code holds direct links to the cells.
(defcase 14 "unload! unmaps what it installed; live definitions stay live"
  (let [d1 (write! (root-dir "unl-one") "libun.clj" "(ns libun) (defn who [] :one)")
        d2 (write! (root-dir "unl-two") "libun.clj" "(ns libun) (defn who [] :two)")]
    (let [c0 (l/classpath [d1] {:parent (l/root)})]
      (l/load c0 {:kind :ns :name "libun"})
      (chk "the namespace was installed" (some? (find-ns 'libun)))
      (let [who (val-of (l/resolve c0 {:kind :var :name "libun/who"}))
            report (l/unload! c0)]
        (chk "unload! reports the unmapped namespace"
             (= 1 (get-in report [:released :namespaces])))
        (chk "the name is no longer registered" (nil? (find-ns 'libun)))
        (chk "a definition resolved before the unload still works" (= :one (who)))))
    (let [c1 (l/classpath [d1] {:parent (l/root)})
          c2 (l/classpath [d2] {:parent (l/root)})]
      (l/load c1 {:kind :ns :name "libun"})
      (l/load c2 {:kind :ns :name "libun"})
      (let [who2 (val-of (l/resolve c2 {:kind :var :name "libun/who"}))
            report (l/unload! c1)
            c3 (l/classpath [d1] {:parent (l/root)})]
        (chk "context 2's version is the installed one" (= :two (who2)))
        (chk "context 1's unload released nothing (its slot was replaced)"
             (= 0 (get-in report [:released :namespaces])))
        (chk "context 2's registration survived" (some? (find-ns 'libun)))
        (l/unload! c2)
        (chk "the last unload unmaps the name" (nil? (find-ns 'libun)))
        (l/load c3 {:kind :ns :name "libun"})
        (chk "a fresh context reloads after the unload"
             (= :one ((val-of (l/resolve c3 {:kind :var :name "libun/who"})))))))))

;; --- 15. a failed load installs nothing -------------------------------------
;; A private load that throws must not leave a partial namespace registered: the
;; next load of the name must behave as if it never ran.
(defcase 15 "a failed private load leaves nothing installed"
  (let [d (root-dir "failed")
        _ (write! d "libfail.clj"
                  "(ns libfail) (def half :defined) (throw (ex-info \"boom\" {}))")
        ctx (l/classpath [d] {:parent (l/root)})
        err (try (l/load ctx {:kind :ns :name "libfail"})
                 nil
                 (catch :default e e))]
    (chk "the load threw" (some? err))
    (chk "no partial namespace was left registered" (nil? (find-ns 'libfail)))
    (chk "the loader linked nothing" (nil? (l/resolve ctx {:kind :ns :name "libfail"})))
    (write! d "libfail.clj" "(ns libfail) (def marker :ok)")
    (l/load ctx {:kind :ns :name "libfail"})
    (chk "a retry after fixing the source loads"
         (= :ok (val-of (l/resolve ctx {:kind :var :name "libfail/marker"}))))))

;; --- 16. private names are invisible through the host -----------------------
;; A context's own namespace is not the host's: while a context owns the name,
;; the root answers neither the namespace nor a var in it.
(defcase 16 "private names are hidden from the host root"
  (let [d (write! (root-dir "priv") "libpriv.clj" "(ns libpriv) (def marker :mine)")
        ctx (l/classpath [d] {:parent (l/root)})]
    (l/load ctx {:kind :ns :name "libpriv"})
    (chk "the context resolves its own namespace"
         (some? (l/resolve ctx {:kind :ns :name "libpriv"})))
    (chk "the root does not see the namespace"
         (empty? (l/find (l/root) {:kind :ns :name "libpriv"})))
    (chk "the root does not see a var in it"
         (empty? (l/find (l/root) {:kind :var :name "libpriv/marker"})))
    (chk "the owner still sees its var"
         (some? (l/resolve ctx {:kind :var :name "libpriv/marker"})))
    (l/unload! ctx)
    (chk "after unload nothing is registered" (nil? (find-ns 'libpriv)))))

;; --- 17. the host loads its own namespaces through the root -----------------
;; A namespace the host can load but has not loaded yet is located by the root
;; without reading, then loaded through the host's own loader — the context
;; links the shared definition instead of the runtime require pulling it in
;; behind the loader's back.
(defcase 17 "the root loads host namespaces on demand, linked in the context"
  (let [roots (jolt.host/source-roots)
        host-dir (write! (root-dir "hostns") "libhost.clj" "(ns libhost) (def v :host)")]
    (jolt.host/set-source-roots! (cons host-dir roots))
    (try
      (let [ctx (l/classpath [(write! (root-dir "hostapp") "apph.clj"
                                      "(ns apph (:require [libhost])) (defn f [] libhost/v)")]
                             {:parent (l/root)})]
        (chk "the host does not have it loaded yet" (nil? (find-ns 'libhost)))
        (chk "the root locates it" (seq (l/find (l/root) {:kind :ns :name "libhost"})))
        (l/load ctx {:kind :ns :name "apph"})
        (chk "the host loaded it through its own loader" (some? (find-ns 'libhost)))
        (chk "the context linked the host's namespace"
             (some? (l/resolve ctx {:kind :ns :name "libhost"})))
        (chk "the context's code calls it"
             (= :host ((val-of (l/resolve ctx {:kind :var :name "apph/f"})))))
        (l/unload! ctx))
      (finally
        (remove-ns 'libhost)
        (jolt.host/set-source-roots! roots)))))

;; --- 18. a requirement must come from the loader -----------------------------
;; Evaluated source resolves through the loader, not through whatever the
;; runtime's global require would reach: a requirement the loader cannot serve
;; fails the load with an actionable error instead of silently compiling against
;; a definition the context cannot even see.
(defcase 18 "a requirement the loader cannot serve fails the load"
  (let [d (write! (root-dir "hermetic-req") "libhq.clj"
                  (str "(ns libhq (:require [clojure.string]))"
                       " (defn f [] (clojure.string/upper-case \"x\"))"))
        ctx (l/classpath [d] {:parent (l/isolated)})
        err (try (l/load ctx {:kind :ns :name "libhq"})
                 nil
                 (catch :default e e))]
    (chk "the load threw" (some? err))
    (chk "the error names the requirement" (= "clojure.string" (:name (ex-data err))))
    (chk "the error is actionable" (string? (ex-message err)))
    (chk "nothing was installed" (nil? (find-ns 'libhq)))
    (chk "and the source was not linked"
         (nil? (l/resolve ctx {:kind :ns :name "libhq"})))))

;; --- 19. dashed namespace names ----------------------------------------------
;; The runtime maps a namespace to a file the way Clojure does: split on '.',
;; munge '-'->'_' per segment, join with '/' (loader.ss ns-seg-munge). A root
;; holding lib_one/core.clj must therefore serve lib-one.core — every namespace
;; the extension system loads is dashed.
(defcase 19 "dashed namespace names resolve at their munged paths"
  (let [d (root-dir "dashed")]
    (fs/create-dirs (str d "/lib_one"))
    (spit (str d "/lib_one/core.clj")
          "(ns lib-one.core) (defn f [] :dashed) (def v :dashed-v)")
    (let [l (l/classpath [d])]
      (chk "the munged path locates the namespace"
           (seq (l/find l {:kind :ns :name "lib-one.core"})))
      (l/load l {:kind :ns :name "lib-one.core"})
      (chk "the source loads" (some? (l/resolve l {:kind :ns :name "lib-one.core"})))
      (chk "its vars link under the dashed name"
           (= :dashed ((val-of (l/resolve l {:kind :var :name "lib-one.core/f"})))))
      (chk "and a var request loads through the link"
           (= :dashed-v (val-of (l/load l {:kind :var :name "lib-one.core/v"})))))
    ;; an unmunged (dashed) FILE name is not a namespace path: the rule is
    ;; the runtime's, and Clojure's
    (fs/create-dirs (str d "/lib_two"))
    (spit (str d "/lib_two/dep-util.clj")
          "(ns lib-two.dep-util) (def x :wrong)")
    (let [l2 (l/classpath [d])]
      (chk "a dashed file name is not a namespace path"
           (empty? (l/find l2 {:kind :ns :name "lib-two.dep-util"}))))))

;; --- 20. resources follow the ambient loader ---------------------------------
;; io/resource's 1-arity is the resource analogue of the TCCL: inside
;; with-loader it resolves in the bound loader's context — an extension's
;; (io/resource "x") must find its own bundled files — and outside one it keeps
;; the host answer.
(defcase 20 "the 1-arity io/resource follows the ambient loader"
  (let [d (root-dir "ambient-res")]
    (spit (str d "/amb.edn") "{:amb true}")
    (let [ctx (l/classpath [d] {:parent (l/isolated)})]
      (chk "outside any context the file is not on the host's roots"
           (nil? (io/resource "amb.edn")))
      (chk "inside the context it resolves against the context's roots"
           (= (str (io/as-url (java.io.File. (str d "/amb.edn"))))
              (str (l/with-loader* ctx (fn [] (io/resource "amb.edn"))))))
      (chk "and the 2-arity agrees"
           (= (str (l/with-loader* ctx (fn [] (io/resource "amb.edn"))))
              (str (io/resource "amb.edn" (l/as-classloader ctx))))))))

;; --- 21. the host root is not unloadable -------------------------------------
;; The root is the host itself; `unload!` there would leave the process with no
;; world to load from, so it is refused (a bad request, not a teardown) and the
;; root stays live.
(defcase 21 "the host root is not unloadable"
  (let [r (l/root)
        err (try (l/unload! r) nil (catch :default e e))]
    (chk "unload! on the root throws" (some? err))
    (chk "with an actionable message" (string? (ex-message err)))
    (chk "and it is a bad request, not a teardown failure"
         (= :loader/bad-request (:type (ex-data err))))
    (chk "the root is still live" (false? (l/unloaded? r)))
    (chk "and still answers"
         (some? (l/find r {:kind :ns :name "clojure.string"})))))

;; --- 22. the thread context classloader --------------------------------------
;; TCCL is the resource analogue's sibling: inside with-loader it is the
;; context's own classloader (a library that finds its resources the Java way
;; must land in the context's roots), outside one the host singleton.
(defcase 22 "the thread context classloader follows the ambient loader"
  (let [d (root-dir "tccl")]
    (spit (str d "/amb.edn") "{:amb true}")
    (let [ctx (l/classpath [d])
          outside (.getContextClassLoader (Thread/currentThread))
          inside (l/with-loader* ctx (fn [] (.getContextClassLoader (Thread/currentThread))))]
      (chk "inside a context it is that context's classloader"
           (identical? (l/as-classloader ctx) inside))
      (chk "and it resolves the context's own roots"
           (some? (.getResource inside "amb.edn")))
      (chk "outside it is not the context's"
           (and (some? outside) (not (identical? outside inside)))))))

;; --- 23. require's options, and :reload -------------------------------------
;; A runtime (require …) is a load IN the defining context: its :as/:refer must
;; materialize there (the rewrite used to preload and drop them), and :reload
;; must re-read through the loader, not the host.
(defcase 23 "require's options apply in the defining context, and :reload re-reads"
  (let [d (root-dir "reqopts")]
    (spit (str d "/libh.clj") "(ns libh) (defn thing [] :one)")
    (spit (str d "/libmain.clj")
          (str "(ns libmain)"
               " (require '[libh :as h])"
               " (defn f [] (h/thing))"
               " (defn reload! [] (require 'libh :reload))"))
    (let [ctx (l/classpath [d])]
      (l/load ctx {:kind :ns :name "libmain"})
      (chk "the alias materialized for later forms"
           (= :one ((val-of (l/resolve ctx {:kind :var :name "libmain/f"})))))
      (spit (str d "/libh.clj") "(ns libh) (defn thing [] :two)")
      ((val-of (l/resolve ctx {:kind :var :name "libmain/reload!"})))
      (chk ":reload re-read the source through the loader"
           (= :two ((val-of (l/resolve ctx {:kind :var :name "libmain/f"})))))
      ;; a FAILED reload keeps the installed namespace — already-linked code
      ;; must not lose the registration under it
      (spit (str d "/libh.clj")
            "(ns libh) (throw (ex-info \"boom\" {})) (defn thing [] :three)")
      (let [err (try ((val-of (l/resolve ctx {:kind :var :name "libmain/reload!"})))
                      nil
                      (catch :default e e))]
        (chk "the failed reload throws" (some? err))
        (chk "and the installed namespace stays registered"
             (some? (find-ns 'libh)))
        (chk "so linked code still answers"
             (= :two ((val-of (l/resolve ctx {:kind :var :name "libmain/f"}))))))
      ;; a requirement the loader cannot serve fails rather than leaking to the
      ;; runtime's global require (the ns-form path's rule, extended to calls)
      (let [d2 (root-dir "reqopts-bad")]
        (spit (str d2 "/libbad.clj") "(ns libbad) (require 'nope.nothing)")
        (let [err (try (l/load (l/classpath [d2]) {:kind :ns :name "libbad"})
                       nil (catch :default e e))]
          (chk "an unservable require fails the load" (some? err))
          (chk "as :loader/unreadable" (= :loader/unreadable (:type (ex-data err)))))))))

;; --- 24. use and refer ------------------------------------------------------
(defcase 24 "use and refer load through the loader and act in the defining namespace"
  (let [d (root-dir "useref")]
    (spit (str d "/libh.clj") "(ns libh) (defn thing [] :helper) (def other :other)")
    (spit (str d "/libu.clj") "(ns libu) (use 'libh) (defn f [] (thing))")
    (spit (str d "/libr.clj")
          "(ns libr) (refer 'libh :only '[other]) (defn g [] other)")
    (let [ctx (l/classpath [d])]
      (l/load ctx {:kind :ns :name "libu"})
      (l/load ctx {:kind :ns :name "libr"})
      (chk "use referred the helper in the defining namespace"
           (= :helper ((val-of (l/resolve ctx {:kind :var :name "libu/f"})))))
      (chk "refer applied its :only filter there"
           (= :other ((val-of (l/resolve ctx {:kind :var :name "libr/g"}))))))))

;; --- 25. load / load-file ---------------------------------------------------
(defcase 25 "load and load-file are refused in evaluated source"
  (let [d (root-dir "refuse")]
    (spit (str d "/libl.clj") "(ns libl) (load \"whatever\")")
    (let [err (try (l/load (l/classpath [d]) {:kind :ns :name "libl"})
                   nil (catch :default e e))]
      (chk "the load is refused" (some? err))
      (chk "with an actionable message"
           (some? (re-find #"host-file" (ex-message err))))
      (chk "as :loader/unreadable" (= :loader/unreadable (:type (ex-data err)))))))

;; --- 26. a context's own data_readers.clj ---------------------------------------
;; Per-context data readers are not supported: the runtime's reader resolves
;; #tag against the host's *data-readers* before the loader sees the form. That
;; is a documented limit, not a silent one — the load fails naming the tag and
;; the reason.
(defcase 26 "a context's own data_readers.clj fails with the tag named"
  (let [d (root-dir "readers")]
    (spit (str d "/data_readers.clj") "{my/tag my.reader/read-tag}")
    (fs/create-dirs (str d "/my"))
    (spit (str d "/my/reader.clj") "(ns my.reader) (defn read-tag [_] :tagged)")
    (spit (str d "/libdr.clj") "(ns libdr) (def v #my/tag 1)")
    (let [err (try (l/load (l/classpath [d]) {:kind :ns :name "libdr"})
                   nil (catch :default e e))]
      (chk "the load fails" (some? err))
      (chk "as :loader/unreadable" (= :loader/unreadable (:type (ex-data err))))
      (chk "the tag is named" (= ["my/tag"] (:tags (ex-data err))))
      (chk "and the data_readers.clj situation is explained"
           (some? (re-find #"data_readers" (ex-message err))))))
  ;; a root without one fails the same way, blaming no file
  (let [d2 (root-dir "readers-plain")]
    (spit (str d2 "/libdr2.clj") "(ns libdr2) (def v #nope/tag 1)")
    (let [err (try (l/load (l/classpath [d2]) {:kind :ns :name "libdr2"})
                   nil (catch :default e e))]
      (chk "an unresolved tag without any data_readers.clj still names the tag"
           (= ["nope/tag"] (:tags (ex-data err))))
      (chk "and does not blame a file that is not there"
           (nil? (re-find #"data_readers" (ex-message err)))))))

;; --- 27. :reload through a delegate -----------------------------------------
;; The reload intent is keyed by NAME, so a namespace the context shares through
;; a delegate re-reads there — in place, because in-place is what already-linked
;; code needs — and the delegate's own link table ends up with the namespace's
;; var links, not just the namespace.
(defcase 27 ":reload reaches a namespace served by a delegate"
  (let [pd (root-dir "reload-parent")
        cd (root-dir "reload-child")]
    (spit (str pd "/libs.clj") "(ns libs) (defn v [] :old)")
    (spit (str cd "/libmain.clj")
          (str "(ns libmain) (require '[libs :as s])"
               " (defn now [] (s/v))"
               " (defn bump [] (require 'libs :reload))"))
    (let [parent (l/classpath [pd] {:parent (l/isolated)})
          child (l/classpath [cd] {:parent parent})]
      (l/load child {:kind :ns :name "libmain"})
      (chk "the delegate serves libs"
           (= :old ((val-of (l/resolve child {:kind :var :name "libmain/now"})))))
      (spit (str pd "/libs.clj") "(ns libs) (defn v [] :new)")
      ((val-of (l/resolve child {:kind :var :name "libmain/bump"})))
      (chk "the reload reached the delegate and re-read in place"
           (= :new ((val-of (l/resolve child {:kind :var :name "libmain/now"})))))
      (chk "the delegate's table has the namespace link"
           (some? (l/resolve parent {:kind :ns :name "libs"})))
      (chk "and its var links"
           (= :new ((val-of (l/resolve parent {:kind :var :name "libs/v"}))))))))

;; --- 28. a local named after a context op is that local -----------------------
;; The context-carrying rewrite is the compiler's, made where the compiler knows
;; what a symbol names: a call is rewritten only when its head resolves to the
;; clojure.core var. A promise-style callback pair binds `resolve`, a parameter
;; can be called `load`, a let can bind `require` — each is that local's call.
;; The source walk this replaced turned the first into an arity error, the
;; second into a refused host-file load and the third into nil.
(defcase 28 "a local named resolve, load or require is a local, not a context op"
  (let [d (root-dir "shadow")]
    (spit (str d "/libsh.clj")
          (str "(ns libsh)\n"
               "(defn run-then [f] (let [r (atom nil)]"
               " (f (fn [v] (reset! r [:ok v])) (fn [e] (reset! r [:err e]))) @r))\n"
               "(defn via-local [] (run-then (fn [resolve reject] (resolve 42))))\n"
               "(defn total [load] (load 3))\n"
               "(defn via-param [] (total (fn [x] (* x 2))))\n"
               "(defn via-let [] (let [require (fn [x] (inc x))] (require 1)))\n"
               "(defn own [] :own)\n"
               "(defn via-var [] ((resolve 'own)))"))
    (let [ctx (l/classpath [d] {:parent (l/root)})]
      (l/load ctx {:kind :ns :name "libsh"})
      (chk "a callback parameter named resolve is the callback"
           (= [:ok 42] ((val-of (l/resolve ctx {:kind :var :name "libsh/via-local"})))))
      (chk "a parameter named load is the parameter"
           (= 6 ((val-of (l/resolve ctx {:kind :var :name "libsh/via-param"})))))
      (chk "a let-bound require is the let binding"
           (= 2 ((val-of (l/resolve ctx {:kind :var :name "libsh/via-let"})))))
      (chk "and the var call next to them still resolves in the defining context"
           (= :own ((val-of (l/resolve ctx {:kind :var :name "libsh/via-var"}))))))))

;; --- 29. a host namespace pulled in at load time stays the host's ----------------
;; The rewrite is bound around a context's evaluation, and a load can compile
;; a HOST namespace inside that extent — requiring-resolve at the top level
;; reaches the runtime's own loader, whose artifact the AOT cache keeps. That
;; code must compile as the host's: a `require` in one of its fns aliases in
;; the CALLER's namespace at call time, never in the context's (which is where
;; the context-carrying form would put it).
(defcase 29 "a host namespace compiled inside a context load is not rewritten"
  (let [roots (jolt.host/source-roots)
        host-dir (write! (root-dir "hosttrans") "libtrans.clj"
                         (str "(ns libtrans)"
                              " (defn r [] (require '[clojure.set :as transalias]) :done)"))]
    (jolt.host/set-source-roots! (cons host-dir roots))
    (try
      (let [ctx (l/classpath [(write! (root-dir "hosttrans-app") "apptrans.clj"
                                      (str "(ns apptrans)"
                                           " (def rv (requiring-resolve 'libtrans/r))"))]
                             {:parent (l/root)})]
        (chk "the host does not have it loaded yet" (nil? (find-ns 'libtrans)))
        (l/load ctx {:kind :ns :name "apptrans"})
        (chk "the load pulled the host namespace in" (some? (find-ns 'libtrans)))
        (chk "the host fn runs" (= :done (@(val-of (l/resolve ctx {:kind :var :name "apptrans/rv"})))))
        (chk "its require aliased in the caller's namespace, as the host's does"
             (some? (get (ns-aliases *ns*) 'transalias)))
        (chk "and not in the context's namespace"
             (nil? (get (ns-aliases 'apptrans) 'transalias)))
        (l/unload! ctx))
      (finally
        (ns-unalias *ns* 'transalias)
        (remove-ns 'libtrans)
        (jolt.host/set-source-roots! roots)))))

;; --- 30. jar roots ----------------------------------------------------------
;; A context's root may be a jar, read in place as the global roots read one
;; (jolt issue #1005): the namespace loads out of it, a resource in it resolves
;; to a jar: URL that opens, and the facade sees both.
(defcase 30 "a jar root serves its namespaces and resources without extraction"
  (let [src-dir (-> (root-dir "jarsrc")
                    (write! "jarlib.clj" "(ns jarlib) (def where :jar) (def here *file*)")
                    (write! "jarcfg.edn" "{:from :jar}"))
        jar (str (root-dir "jarroot") "/lib.jar")]
    (with-open [out (java.util.zip.ZipOutputStream. (java.io.FileOutputStream. jar))]
      (doseq [name ["jarlib.clj" "jarcfg.edn"]]
        (.putNextEntry out (java.util.zip.ZipEntry. name))
        (.write out (.getBytes (slurp (str src-dir "/" name)) "UTF-8"))
        (.closeEntry out)))
    (let [ctx (l/classpath [jar] {:parent (l/root)})
          cl (l/as-classloader ctx)]
      (l/load ctx {:kind :ns :name "jarlib"})
      (chk "the namespace loads from the jar"
           (= :jar (val-of (l/resolve ctx {:kind :var :name "jarlib/where"}))))
      (chk "*file* names the entry inside the jar"
           (= (str "jar:file:" jar "!/jarlib.clj")
              (val-of (l/resolve ctx {:kind :var :name "jarlib/here"}))))
      (let [hits (l/find ctx {:kind :resource :name "jarcfg.edn"})]
        (chk "find locates the resource in the jar" (seq hits))
        (chk "the hit is a jar: location" (str/starts-with? (:url (first hits)) "jar:file:"))
        (chk "open-hit reads it" (= "{:from :jar}" (slurp (l/open-hit ctx (first hits))))))
      (chk "getResource answers a jar: URL"
           (= "jar" (.getProtocol (.getResource cl "jarcfg.edn"))))
      (chk "getResourceAsStream reads it"
           (= "{:from :jar}" (slurp (.getResourceAsStream cl "jarcfg.edn"))))
      (chk "the root does not see the jar's resource" (nil? (io/resource "jarcfg.edn")))
      (chk "nothing was extracted beside the jar"
           (= ["lib.jar"] (mapv fs/file-name (fs/list-dir (fs/parent jar)))))
      (l/unload! ctx))
    (let [err (try (l/classpath [(str (write! (root-dir "notjar") "x.jar" "nope") "/x.jar")]
                                {:parent (l/root)})
                   nil
                   (catch Exception e e))]
      (chk "a root that is a file but not an archive is refused eagerly"
           (= :loader/bad-root (:type (ex-data err)))))))

;; --- 31. one facade per loader, not per id ----------------------------------
;; A context's facade is cached on the loader, never keyed by `:id` — an id may
;; be reused (a reload, a per-request context). Keyed by id, the second context
;; received the first, unloaded context's facade, and every resource read
;; through it threw "loader <id> is unloaded".
(defcase 31 "a reused :id gets a fresh facade, not the unloaded context's"
  (let [d (write! (root-dir "facade-reuse") "fac.edn" "{:from :ctx}")
        old (l/classpath [d] {:id "facade-reused" :parent (l/isolated)})
        old-cl (l/as-classloader old)]
    (chk "one loader has one facade" (identical? old-cl (l/as-classloader old)))
    (chk "the facade reads its context's resource"
         (= "{:from :ctx}" (slurp (.getResourceAsStream old-cl "fac.edn"))))
    (l/unload! old)
    (let [new (l/classpath [d] {:id "facade-reused" :parent (l/isolated)})
          new-cl (l/as-classloader new)]
      (chk "the new context's facade is a different object"
           (not (identical? old-cl new-cl)))
      (chk "and it reads through the new context"
           (= "{:from :ctx}" (slurp (.getResourceAsStream new-cl "fac.edn"))))
      (chk "the 2-arity io/resource agrees" (some? (io/resource "fac.edn" new-cl)))
      (chk "so does the ambient loader inside the context"
           (identical? new-cl (l/with-loader* new (fn [] (clojure.lang.RT/baseLoader)))))
      (l/unload! new))))

;; --- 32. the reader follows the file's own namespace -------------------------
;; A file is read form by form, each after the forms before it ran, so the
;; namespace its ns form switched to — and the aliases and refers that form
;; installed — are in force for the reader that expands its syntax quotes.
;; Reading the whole file up front under the caller's *ns* expanded `v to the
;; CALLER's v and `bb/v by the caller's aliases, so a macro's expansion named
;; a var the defining file never mentioned. The host loader read form-by-form,
;; which is why a plain require from the root never showed this. The caller
;; here is bound to a decoy namespace holding v 999, so the failure mode is a
;; wrong value, not just an error.
(defcase 32 "syntax quotes resolve in the namespace the file declares"
  (let [d (root-dir "syntaxquote")]
    (spit (str d "/sqb.clj") "(ns sqb) (def v 42) (defmacro m [] `v)")
    (spit (str d "/sqa.clj") "(ns sqa (:require [sqb])) (defn f [] (sqb/m))")
    (spit (str d "/sqc.clj") "(ns sqc (:require [sqb :as bb])) (defmacro m [] `bb/v)")
    (spit (str d "/sqd.clj") "(ns sqd (:require [sqc])) (defn f [] (sqc/m))")
    (spit (str d "/sqe.clj") "(ns sqe (:require [sqb :refer [v]])) (defmacro m [] `v)")
    (spit (str d "/sqf.clj") "(ns sqf (:require [sqe])) (defn f [] (sqe/m))")
    (let [ctx (l/classpath [d])
          decoy (create-ns 'loaderconf-decoy)
          _ (intern decoy 'v 999)]
      (binding [*ns* decoy]
        (l/load ctx {:kind :ns :name "sqa"})
        (l/load ctx {:kind :ns :name "sqd"})
        (l/load ctx {:kind :ns :name "sqf"}))
      (chk "a plain syntax quote resolves in the defining namespace"
           (= 42 ((val-of (l/resolve ctx {:kind :var :name "sqa/f"})))))
      (chk "a syntax-quoted alias resolves through the defining namespace's aliases"
           (= 42 ((val-of (l/resolve ctx {:kind :var :name "sqd/f"})))))
      (chk "a syntax-quoted referred symbol resolves through the defining namespace"
           (= 42 ((val-of (l/resolve ctx {:kind :var :name "sqf/f"}))))))))

;; --- 33. a load's compiler-flag set! is scoped to the load -------------------
;; A source load brackets the set!-able compiler flags to their roots for the
;; file's extent, as the host loader does (loader.ss ldr-with-file-vars — the
;; JVM's Compiler.load): a top-level (set! *warn-on-reflection* true) is legal,
;; and its effect ends with the file instead of escaping into the loading
;; context's frame. Without the bracket, the set! wrote the caller's frame and
;; stayed there; this caller watches its own values.
(defcase 33 "a loaded file's compiler-flag set! does not escape into the caller"
  (let [d (write! (root-dir "loadflags") "lf.clj"
                  "(ns lf) (set! *warn-on-reflection* true) (set! *unchecked-math* true) (set! *assert* false) (def v 42)")
        ctx (l/classpath [d])
        w *warn-on-reflection*
        u *unchecked-math*
        a *assert*]
    (binding [*warn-on-reflection* *warn-on-reflection*
              *unchecked-math* *unchecked-math*
              *assert* *assert*]
      (l/load ctx {:kind :ns :name "lf"})
      (chk "the file loaded and its def is live"
           (= 42 (val-of (l/resolve ctx {:kind :var :name "lf/v"}))))
      (chk "warn-on-reflection did not escape the load"
           (identical? w *warn-on-reflection*))
      (chk "unchecked-math did not escape the load"
           (identical? u *unchecked-math*))
      (chk "assert did not escape the load"
           (identical? a *assert*)))))

;; --- 34. embedded roots: the marker, validation, status ---------------------
;; An embedded root is a prefix into the runtime's embedded-resource table, not
;; a path: construction validates the PREFIX itself, so a blank one is refused
;; eagerly while a prefix holding nothing yet is a legal root — the keys are
;; probed per request, never up front.
(defcase 34 "an embedded root is a marker into the embedded table, validated at construction"
  (chk "embedded-root? recognises the marker"
       (and (l/embedded-root? "embed:assets/bundled")
            (l/embedded-root? "embed:")
            (not (l/embedded-root? "src"))
            (not (l/embedded-root? nil))))
  (let [ctx (l/classpath ["embed:no/such/prefix"] {:id "embedded" :parent (l/isolated)})]
    (chk "a prefix holding nothing is still a legal root"
         (= ["embed:no/such/prefix"] (:roots (l/status ctx))))
    (l/unload! ctx))
  (let [err (try (l/classpath ["embed:"]) nil (catch Exception e e))]
    (chk "a blank prefix is refused eagerly" (= :loader/bad-root (:type (ex-data err))))
    (chk "the refusal names the root" (= "embed:" (:root (ex-data err))))))

;; --- 35. an embedded root that holds nothing is a plain miss -----------------
;; Nothing about an embedded root touches the filesystem: `find` answers [],
;; `load` fails with the loader's own miss, and no "path" is ever opened — a
;; prefix that matches nothing degrades exactly like a directory root that is
;; empty.
(defcase 35 "an embedded root over an empty prefix misses without a filesystem error"
  (let [ctx (l/classpath ["embed:no/such/bundle"] {:parent (l/isolated)})
        err (try (l/load ctx {:kind :ns :name "fixture.entry"}) nil (catch Exception e e))]
    (chk "find answers no namespace hits"
         (empty? (l/find ctx {:kind :ns :name "fixture.entry"})))
    (chk "find answers no resource hits"
         (empty? (l/find ctx {:kind :resource :name "fixture/data.txt"})))
    (chk "load fails" (some? err))
    (chk "the failure is the loader's own miss, not a filesystem error"
         (= :loader/miss (:type (ex-data err))))
    (chk "the failure names the namespace"
         (= "fixture.entry" (:name (ex-data err))))
    (l/unload! ctx)))

;; --- 36. a resource hit opens through its own location -----------------------
;; A resource hit carries the LOCATION it was found at; opening must read that,
;; never re-resolve the request name through the host. On a prefixed root the
;; two differ by construction — the request name is relative to the root while
;; the location is the full embedded key — so a fallback to the name would ask
;; for a key that does not exist. `jolt/loader.clj` names this very source, on a
;; host source root under bin/jolt and in the embedded table of a built jolt.
(defcase 36 "a resource hit opens through its own location, not the request name"
  (let [ctx (l/->loader (fn [req]
                          (when (= :resource (:kind req))
                            {:kind :resource
                             :url "jolt/loader.clj"
                             :embedded? true})))
        hit (first (l/find ctx {:kind :resource :name "loaderconf-probe.txt"}))]
    (chk "the resolver's hit comes back" (some? hit))
    (chk "it carries its own location" (= "jolt/loader.clj" (:url hit)))
    (let [s (l/open-hit ctx hit)]
      (chk "open-hit opens the hit's location" (some? s))
      (chk "and reads that resource, not the request name"
           (str/includes? (slurp s) "(ns jolt.loader")))))

;; --- 37. the root never answers a name a context is claiming ----------------
;; The root hides a context's namespace, and "does a context own it" and "is it
;; installed" are two reads. A context claims the name before it creates the
;; namespace, so reading them in the other order let a claim land in between:
;; the root answered the context's half-built namespace as the host's, and a
;; second context loading the same name linked it — with vars missing, since
;; the snapshot ran mid-evaluation (case 13 threw nil-as-IFn once under memory
;; pressure). This parks a root lookup between the two reads through the
;; private seam, so the interleaving is pinned instead of left to the scheduler.
(defcase 37 "a root lookup racing a private load answers nothing of it"
  (let [d (write! (root-dir "claimrace") "libcr.clj" "(ns libcr) (defn who [] :private)")
        orig @#'l/private-ns?]
    (doseq [req [{:kind :ns :name "libcr"} {:kind :var :name "libcr/who"}]]
      (let [ctx (l/classpath [d] {:parent (l/root)})
            spy-thread (promise) parked (promise) resume (promise)]
        (with-redefs [l/private-ns?
                      (fn [nm]
                        (let [r (orig nm)]
                          (when (and (= nm "libcr")
                                     (identical? (Thread/currentThread) @spy-thread)
                                     (not (realized? parked)))
                            (deliver parked true)
                            (deref resume 5000 nil))
                          r))]
          (let [spy (future
                      (deliver spy-thread (Thread/currentThread))
                      (l/find (l/root) req))]
            ;; with a correct ordering the lookup may finish without parking
            (deref parked 2000 nil)
            (l/load ctx {:kind :ns :name "libcr"})
            (deliver resume true)
            (chk (str "the root answers no " (name (:kind req)) " a context owns")
                 (empty? @spy))))
        (l/unload! ctx)))))

;; --- 38. eval-in: a source string evaluates in the context -------------------
;; `eval`/`load-string` never consult the ambient loader, because the
;; context-carrying rewrite is bound only while a namespace source the loader
;; itself reads compiles; a string evaluated under `with-loader` therefore
;; requires from the runtime's global roots. `eval-in` is the loaded-source
;; code path for a string: the hook is bound around the evaluation, so
;; require/resolve/... carry the context.
(defcase 38 "eval-in evaluates a source string in a context and gates requires"
  (let [d (write! (root-dir "evalin") "libev.clj" "(ns libev) (defn v [] :evaluated)")
        ctx (l/classpath [d] {:parent (l/isolated)})]
    (chk "a body answers the last form's value"
         (= 3 (l/eval-in ctx "script.a" "1 2 3")))
    (l/eval-in ctx "script.b" "(def x 7)")
    (chk "a def interns a var that resolves through the context"
         (= 7 (val-of (l/resolve ctx {:kind :var :name "script.b/x"}))))
    (chk "a require loads through the context's roots"
         (= :evaluated (l/eval-in ctx "script.c" "(require '[libev :as e]) (e/v)")))
    (chk "an ns form's requires load through the context too"
         (= :evaluated (l/eval-in ctx "script.d"
                                  "(ns script.d (:require [libev :as e])) (e/v)")))
    (let [err (try (l/eval-in ctx "script.e" "(require '[clojure.string :as s])")
                   nil (catch :default e e))]
      (chk "a dependency the context cannot serve is refused"
           (= :loader/unreadable (:type (ex-data err))))
      (chk "and the dependency is named"
           (some? (re-find #"clojure.string" (ex-message err)))))
    (let [err (try (l/eval-in ctx "script.f" "(ns other.name) 1")
                   nil (catch :default e e))]
      (chk "an ns form for another name is refused"
           (= :loader/bad-request (:type (ex-data err)))))
    (let [err (try (l/eval-in ctx "script.g" "(ns script.g) 1 (ns other.escape) 2")
                   nil (catch :default e e))]
      (chk "an ns form later in the source is refused"
           (= :loader/bad-request (:type (ex-data err))))
      (chk "and did not install its namespace" (nil? (find-ns 'other.escape)))
      (chk "and the fresh namespace rolled back" (nil? (find-ns 'script.g))))
    (let [err (try (l/eval-in ctx "script.h"
                              "(ns script.h) (clojure.core/ns other.esc2) 2")
                   nil (catch :default e e))]
      (chk "the qualified spelling is refused too"
           (= :loader/bad-request (:type (ex-data err))))
      (chk "and did not install its namespace" (nil? (find-ns 'other.esc2))))
    (let [err (try (l/eval-in ctx "script.i"
                              "(eval '(require '[clojure.string :as s])) 1")
                   nil (catch :default e e))]
      (chk "a runtime eval in the source is still gated"
           (= :loader/unreadable (:type (ex-data err)))))
    (let [err (try (l/eval-in ctx "script.l"
                              "(in-ns 'other.left) (require '[clojure.string :as s])")
                   nil (catch :default e e))]
      (chk "a top-level in-ns fails the evaluation"
           (= :loader/bad-request (:type (ex-data err)))))
    (let [err (try (l/eval-in ctx "script.m" "(do (in-ns 'other.mid))")
                   nil (catch :default e e))]
      (chk "a switch nested in a top-level form fails it too"
           (= :loader/bad-request (:type (ex-data err)))))
    (chk "a form without an ns prefix reads in the context's namespace"
         (= :script.n/k (l/eval-in ctx "script.n" "::k")))
    (chk "and a syntax quote resolves there"
         (= 'script.o/foo (l/eval-in ctx "script.o" "`foo")))
    (chk "an alias a require installed is in force for the next form's read"
         (= :libev/x (l/eval-in ctx "script.p" "(require '[libev :as e]) ::e/x")))
    (let [before *warn-on-reflection*]
      (l/eval-in ctx "script.q" "(set! *warn-on-reflection* true)")
      (chk "a compiler flag set! in the source ends with the evaluation"
           (= before *warn-on-reflection*)))
    (chk "the :file option is bound to *file*"
         (= "custom.txt" (l/eval-in ctx "script.j" "*file*" {:file "custom.txt"})))
    (chk "and *file* defaults to nil"
         (nil? (l/eval-in ctx "script.k" "*file*")))
    (let [err (try (l/eval-in ctx "clojure.string" "1")
                   nil (catch :default e e))]
      (chk "an installed namespace the loader does not own is refused"
           (= :loader/bad-request (:type (ex-data err)))))
    (let [err (try (l/eval-in ctx "a/b" "1")
                   nil (catch :default e e))]
      (chk "a non-simple namespace name is refused"
           (= :loader/bad-request (:type (ex-data err)))))
    ;; a loader with no namespace backend of its own still links what was
    ;; evaluated through it, so its vars resolve
    (let [custom (l/->loader (fn [_req] nil))]
      (chk "a custom loader answers the evaluated value"
           (= 5 (l/eval-in custom "script.custom" "(def c 5) c")))
      (chk "and links the evaluated var"
           (= 5 (val-of (l/resolve custom {:kind :var :name "script.custom/c"})))))))

;; --- 39. eval-in keeps the namespace across calls ----------------------------
;; Eval into the same name is a REPL, not a reload: the installed namespace and
;; its cells stay, so a var another compilation already links keeps working.
(defcase 39 "eval-in re-evaluates in place and unload! unmaps the namespace"
  (let [ctx (l/classpath [(root-dir "evalin2")] {:parent (l/isolated)})]
    (l/eval-in ctx "script.re" "(def counter 0)")
    (let [cell (l/resolve ctx {:kind :var :name "script.re/counter"})]
      (chk "the var resolves after the first call" (some? cell))
      (l/eval-in ctx "script.re" "(def counter (inc counter))")
      (chk "the second call keeps the namespace (same cell)"
           (identical? cell (l/resolve ctx {:kind :var :name "script.re/counter"})))
      (chk "and evaluates in place" (= 1 (val-of cell))))
    (chk "the namespace link is installed"
         (seq (l/find ctx {:kind :ns :name "script.re"})))
    (l/unload! ctx)
    (chk "unload! unmaps the context's namespace"
         (nil? (find-ns 'script.re))))
  ;; a policy wrapper releases nothing of its own, so the name belongs to the
  ;; loader whose unload! can release it — the base — while the wrapper's own
  ;; policy still gates the evaluation's requires
  (let [base (l/classpath [(write! (root-dir "evalin3") "libw.clj"
                                   "(ns libw) (defn v [] :w)")]
                          {:parent (l/isolated)})
        wrapped (l/allow base #{"script"})]
    (l/eval-in wrapped "script.w" "(def v :wrapped)")
    (chk "a wrapper evaluates through its base"
         (= :wrapped (val-of (l/resolve base {:kind :var :name "script.w/v"}))))
    (chk "and find answers it through the wrapper too"
         (some? (first (l/find wrapped {:kind :var :name "script.w/v"}))))
    (let [denied (l/deny base #{"libw"})
          err (try (l/eval-in denied "script.g" "(require '[libw :as w]) (w/v)")
                   nil (catch :default e e))]
      (chk "the wrapper's policy gates the evaluation's requires"
           (:loader/denied (ex-data err))))
    (l/unload! base)
    (chk "the base's unload! unmaps a wrapper-evaluated namespace"
         (nil? (find-ns 'script.w)))
    (let [err (try (l/eval-in wrapped "script.dead" "1")
                   nil (catch :default e e))]
      (chk "an unloaded owner refuses further evaluation"
           (= :loader/unloaded (:type (ex-data err)))))))

;; --- 40. unload! releases the loader, not the id -----------------------------
;; A compiled call site carries the loader's id, not the object, so the
;; registry must keep an answer for the id after unload! — but only a closed
;; stand-in: the loader graph becomes collectable, and a stale call still fails
;; :loader/unloaded rather than :loader/bad-context.
(defn- unloaded-context
  "Build a context whose evaluated function requires at run time, unload it,
   and return the function plus a weak reference. The context itself stays
   inside this frame, so the registry is its only strong holder."
  []
  (let [d (write! (root-dir "evict") "libev.clj"
                  "(ns libev) (defn v [] :evicted)")
        ctx (l/classpath [d] {:parent (l/isolated)})]
    (l/eval-in ctx "script.stale"
               "(defn late [] (require '[libev :as e]) (e/v))")
    (let [late (val-of (l/resolve ctx {:kind :var :name "script.stale/late"}))
          wr (java.lang.ref.WeakReference. ctx)]
      (l/unload! ctx)
      {:late late :wr wr})))

(defcase 40 "unload! releases the loader's graph; a stale call site stays mapped"
  (let [{:keys [late wr]} (unloaded-context)]
    (let [err (try (late) nil (catch :default e e))]
      (chk "a stale runtime require still names its closed context"
           (= :loader/unloaded (:type (ex-data err)))))
    (host/gc-full!)
    (chk "the unloaded loader is collectable"
         (nil? (.get wr)))))

;; --- 41. unload! takes the loaded marks with the namespaces -----------------
;; A namespace unload! unmaps must not stay in *loaded-libs*: the mark would make
;; every later require a silent no-op for a namespace find-ns answers nil for,
;; and a :reload would rebuild the entry namespace over dependencies nothing
;; reloads (#1469). The host requires the namespaces first (that is where the
;; marks come from), a context evicts and reloads them privately, and after the
;; context's unload! a plain require brings the host's copies back.
(defcase 41 "unload! drops the loaded marks of what it unmapped; require reloads"
  (let [d (root-dir "unl-marks")
        _ (.mkdirs (java.io.File. (str d "/unlm")))
        _ (write! d "unlm/other.clj" "(ns unlm.other) (defn v [] :other-ok)")
        _ (write! d "unlm/fp.clj"
                  "(ns unlm.fp (:require [unlm.other :as other])) (defn f [] (other/v))")
        roots (vec (host/source-roots))
        marked? (fn [] (some @@#'clojure.core/*loaded-libs* '[unlm.fp unlm.other]))]
    (try
      (host/set-source-roots! (conj roots d))
      (require 'unlm.fp)
      (chk "the host's require marked the namespaces" (marked?))
      (let [ctx (l/classpath [d] {:parent (l/->loader (fn [_] nil))})]
        (doseq [n ["unlm.fp" "unlm.other"]]
          (l/load ctx {:kind :ns :name n}))
        (chk "the context's code runs"
             (= :other-ok ((val-of (l/resolve ctx {:kind :var :name "unlm.fp/f"})))))
        (let [report (l/unload! ctx)]
          (chk "unload! unmapped both namespaces"
               (= 2 (get-in report [:released :namespaces])))))
      (chk "the namespaces are gone" (and (nil? (find-ns 'unlm.fp))
                                          (nil? (find-ns 'unlm.other))))
      (chk "and so are their loaded marks" (not (marked?)))
      (require 'unlm.fp)
      (chk "a later require loads the namespace again" (some? (find-ns 'unlm.fp)))
      (chk "with its dependencies" (some? (find-ns 'unlm.other)))
      (chk "and its code runs" (= :other-ok ((resolve 'unlm.fp/f))))
      (finally (host/set-source-roots! roots)))))

;; --- runner -----------------------------------------------------------------
(defn run-case [[n title body]]
  (reset! failures [])
  (let [err (try (body) nil (catch :default e (or (ex-message e) (pr-str e))))
        fails @failures
        pass (and (nil? err) (empty? fails))]
    (println (format "CASE %02d %s  %s" n (if pass "PASS" "FAIL") title))
    (when err (println (str "    threw: " err)))
    (doseq [f fails] (println (str "    check failed: " f)))
    pass))

(let [ordered (sort-by first @cases)
      passed (count (filter true? (doall (map run-case ordered))))]
  (println (format "LOADERCONF %d/%d" passed (count ordered)))
  (fs/delete-tree tmp))

;; Done with the agent system's pools (futures, agents): end them, as a JVM
;; program does, or their idle workers hold the process up for their keep-alive.
(shutdown-agents)
