;; test/chez/concurrent-require.clj — two threads requiring one namespace.
;;
;; Nothing used to serialize load-namespace*, so both threads passed the
;; loaded-ns check and both ran the target's top-level forms, double-running
;; every def and side effect in it. Worse, the mark-before-load that terminates a
;; require CYCLE marks the namespace loaded BEFORE its forms run, so to a second
;; thread it read as fully loaded while it was still half-built — a require that
;; returns having defined nothing.
;;
;; The loader now follows JLS 12.4.2, the JVM's class-initialization procedure,
;; per namespace: in progress by another thread means block until notified, in
;; progress by this thread means complete normally (the cycle break, which is
;; what mark-before-load already did).
;;
;; Two properties, and the second is the one that mattered:
;;   A. the target's top level ran EXACTLY once, however many threads required it
;;   B. every thread, on return from its require, saw the LAST form in the file
;;
;; Then the other half of the question, which one namespace cannot ask: threads
;; requiring DIFFERENT namespaces at once. One shared target means only one load
;; ever runs, so it exercises none of what happens when two do. Loading distinct
;; namespaces in parallel broke two ways, and both are gated on this file now:
;;
;; The gate is per NAMESPACE, not one lock over every load: ldr-load-mu is held
;; only across the begin/end bookkeeping and the load itself runs unlocked, which
;; is the point of taking JLS 12.4.2 per namespace rather than Clojure's single
;; RT.REQUIRE_LOCK. So the threads below really are compiling at the same time,
;; and C and D are both live.
;;
;;   C. every require completes. The compiler's emit session used to live on one
;;      process-global unit, with emit-with-cells save/restoring its slots per
;;      def, so two threads emitting at once traded constant pools and a namespace
;;      that compiled fine alone died on "variable _kc$81 is not bound". The fix
;;      is not a lock: *cache-cells* and *const-pool* are thread-bound vars in
;;      backend_scheme now, so each thread's emit session is its own.
;;   D. every namespace is in *loaded-libs* afterwards. The loader used to conj
;;      the ref with a bare read-modify-write, so two threads finishing together
;;      dropped one of the two marks — and a namespace missing from *loaded-libs*
;;      reads as unloaded and runs its top level again on the next require, which
;;      is the bug A tests for, arriving through the other table. A separate probe
;;      lost 3 to 15 of 24 that way. Twelve loads finishing at once do overlap
;;      here, so this is the check for it and not a placeholder.
;;   E. two threads entering one require cycle from opposite ends report it instead
;;      of hanging, and one of them still completes. This is where the JVM gives up
;;      and deadlocks; the loader walks its wait-for graph before blocking.
;;   F. a (dosync (require ...)) that has to wait does not deadlock the loader, and
;;      nothing a load does needs stm-lock: the cycle would be stm-lock -> the load
;;      being waited on -> stm-lock.
;;   G. a require inside a transaction dedups, and an aborted transaction does not
;;      un-load a namespace that really loaded. The mark is two tables and buffering
;;      one of them in the txn log split them apart in both directions.
;;   H. a load that PARKS finishes, and the namespace is whole afterwards for the
;;      fiber that loaded it and for a thread that waited on it. A park is a
;;      continuation escaping the load body, and treating it as an exit wedged the
;;      namespace marked-but-empty for the life of the process.
;;   I. a sibling FIBER on the carrier the parked load is on gets the whole
;;      namespace too. It shares the owner's thread id, so ownership by thread read
;;      the claim as its own and returned half-loaded; and it has to wait by parking,
;;      because blocking that carrier is the one thing the parked load cannot survive.
;;   J. a requiring-resolve INSIDE a load, while another thread's requiring-resolve
;;      is waiting on that load. Clojure's requiring-resolve holds RT/REQUIRE_LOCK
;;      across its require because its loader has no other guard; here the
;;      per-namespace claim already blocks the second thread until the first one's
;;      load finishes, so the process-wide lock adds nothing — and it adds an edge
;;      the wait-for graph cannot see. Thread A holds the lock and waits on the
;;      namespace B is loading; B's top level reaches for the lock. Neither is a
;;      loader wait, so the cycle walk never fires and both hang for good. jolt's
;;      requiring-resolve is a plain require for that reason, and this pins it.
;;
;; Run: jolt run test/chez/concurrent-require.clj  (wired into smoke.sh)

(ns concurrent-require)

(def n-threads 8)
(def n-distinct 12)
(def tmp-root (str (System/getProperty "java.io.tmpdir") "/jolt-ldrtest-" (System/currentTimeMillis)))

;; The target. defonce keeps the SAME atom across a second load while the swap!
;; below it runs again, so the counter reads 2 if the file was loaded twice — it
;; measures double-loading directly rather than inferring it. `sentinel` is the
;; last form in the file on purpose: a thread that returned from require while
;; another was still loading would not see it. The loop widens the window.
(def target-src
  (str "(ns ldrtest.target)\n"
       "(defonce counter (atom 0))\n"
       "(swap! counter inc)\n"
       "(def slow (reduce + (range 200000)))\n"
       "(def sentinel :loaded)\n"))

;; One namespace per thread for phase two, aimed at the emit-session scratch rather
;; than being merely distinct: the keyword literals feed the hoisted constant pool,
;; the protocol call and the record feed the per-def cache cells, and the loop and
;; anon fns feed the gensym counter and the fn-source registry. A file of bare
;; (def x 1) exercises none of it. `sentinel` is a VALUE, so a corrupted compile
;; that somehow produced loadable code still has to produce the right answer.
(defn- distinct-src [i]
  (str "(ns ldrtest.m" i ")\n"
       "(defprotocol P" i " (go [this x]))\n"
       "(defrecord R" i " [a b] P" i " (go [this x] (+ a b x)))\n"
       "(def tags [:k" i "/a :k" i "/b :k" i "/c :k" i "/d :k" i "/e])\n"
       "(defn tally [n]\n"
       "  (loop [i 0 acc 0]\n"
       "    (if (< i n) (recur (inc i) (+ acc (go (->R" i " i 2) i))) acc)))\n"
       "(defn pick [m] (get m :k" i "/a (get m :k" i "/b :none)))\n"
       "(def sentinel {:tally (tally 20) :pick (pick {:k" i "/b " i "}) :tags tags})\n"))

;; (tally 20) sums (i + 2 + i) over 0..19 = 2*190 + 40.
(def tally-expected 420)

;; The two halves of a genuine require cycle, for property E. Each half announces
;; itself and waits for the other BEFORE requiring it, so both threads are certainly
;; inside their own load when they reach across — without that the faster thread
;; finishes both namespaces on its own and there is no cross-thread cycle to detect,
;; which made this pass for the wrong reason on roughly one run in three. The wait
;; is bounded so a loader that really is wedged fails by timeout rather than
;; spinning here forever.
(def cycle-gate (atom 0))
(defn- cycle-src [me other]
  (str "(ns ldrtest." me ")\n"
       "(swap! concurrent-require/cycle-gate inc)\n"
       "(loop [n 0]\n"
       "  (when (and (< @concurrent-require/cycle-gate 2) (< n 3000))\n"
       "    (Thread/sleep 1) (recur (inc n))))\n"
       "(require 'ldrtest." other ")\n"
       "(def sentinel :" me ")\n"))

(def dosync-src
  (str "(ns ldrtest.boom)\n"
       "(def x (reduce + (range 8000000)))\n"
       "(throw (ex-info \"boom\" {}))\n"))

;; Property G's targets. Same defonce-plus-swap! shape as the main target, so the
;; counter measures double-loading directly. Two of them because the transaction
;; that requires `dedup` commits and the one that requires `dedupab` aborts, and
;; those are different bugs in the same asymmetry.
(defn- dedup-src [nm]
  (str "(ns ldrtest." nm ")\n"
       "(defonce counter (atom 0))\n"
       "(swap! counter inc)\n"
       "(def sentinel :loaded)\n"))

;; Property H's target: a namespace that PARKS at top level. The channel is fed by
;; a real thread after a delay, so the (<!! ch) below cannot complete inline — on
;; the :fiber backend it is a genuine park with the load half-run, which is the
;; state the load protocol has to survive. `after` is last so a require that
;; returned early is visible.
(def parky-src
  (str "(ns ldrtest.parky (:require [clojure.core.async :as a]))\n"
       "(def ch (a/chan 1))\n"
       "(def before :ran)\n"
       "(a/thread (Thread/sleep 150) (a/>!! ch :fed))\n"
       "(def got (a/<!! ch))\n"
       "(def after :ran)\n"))

;; Property I's target: the same shape, in a namespace of its own so H has not
;; already loaded it. The delay is longer because two fibers have to reach it.
(def sibling-src
  (str "(ns ldrtest.sibling (:require [clojure.core.async :as a]))\n"
       "(def ch (a/chan 1))\n"
       "(def before :ran)\n"
       "(a/thread (Thread/sleep 250) (a/>!! ch :fed))\n"
       "(def got (a/<!! ch))\n"
       "(def after :ran)\n"))

;; Property J's three namespaces: rlx requires rly; rly's top level announces
;; itself, waits to be released, then requiring-resolves rlz — the reach for the
;; lock from inside a load. The release is external so thread A is certainly
;; waiting on rly before rly asks for anything. Bounded like the cycle gate.
(def rl-y-started (atom false))
(def rl-go (atom false))
(def rl-z-src "(ns ldrtest.rlz)\n(def bar 1)\n")
(def rl-x-src "(ns ldrtest.rlx (:require [ldrtest.rly]))\n(def foo 1)\n")
(def rl-y-src
  (str "(ns ldrtest.rly)\n"
       "(reset! concurrent-require/rl-y-started true)\n"
       "(loop [n 0]\n"
       "  (when (and (not @concurrent-require/rl-go) (< n 5000))\n"
       "    (Thread/sleep 1) (recur (inc n))))\n"
       "(def got (requiring-resolve 'ldrtest.rlz/bar))\n"
       "(def sentinel :rly)\n"))

(defn- write-sources! []
  (let [dir (str tmp-root "/ldrtest")]
    (.mkdirs (java.io.File. dir))
    (spit (str dir "/target.clj") target-src)
    (doseq [i (range n-distinct)]
      (spit (str dir "/m" i ".clj") (distinct-src i)))
    (spit (str dir "/x.clj") (cycle-src "x" "y"))
    (spit (str dir "/y.clj") (cycle-src "y" "x"))
    (spit (str dir "/boom.clj") dosync-src)
    (spit (str dir "/dedup.clj") (dedup-src "dedup"))
    (spit (str dir "/dedupab.clj") (dedup-src "dedupab"))
    (spit (str dir "/parky.clj") parky-src)
    (spit (str dir "/sibling.clj") sibling-src)
    (spit (str dir "/rlx.clj") rl-x-src)
    (spit (str dir "/rly.clj") rl-y-src)
    (spit (str dir "/rlz.clj") rl-z-src)))

;; A gate every thread spins on, so they all enter require together — without it
;; the first finishes before the rest start and there is no race to lose.
(defn- run-together [n f]
  (let [go? (atom false)
        fs (doall (for [i (range n)]
                    (future (while (not @go?) (Thread/sleep 1))
                            (f i))))]
    (reset! go? true)
    (mapv deref fs)))

;; Properties A and B: n threads, one namespace.
(defn- one-namespace-failures []
  (let [saw (atom [])
        errs (atom [])]
    (run-together n-threads
      (fn [_]
        (try
          (require 'ldrtest.target)
          ;; resolved AFTER require returns: property B
          (swap! saw conj (some? (resolve 'ldrtest.target/sentinel)))
          (catch Throwable e
            (swap! errs conj (str (.getMessage e)))))))
    (let [loads (deref @(resolve 'ldrtest.target/counter))
          sentinels @saw]
      (cond-> []
        (seq @errs)
        (conj (str "requires threw: " (pr-str @errs)))
        (not= 1 loads)
        (conj (str "the target's top level ran " loads " times, expected 1"))
        (not= n-threads (count sentinels))
        (conj (str "only " (count sentinels) " of " n-threads
                   " threads completed their require"))
        (not (every? true? sentinels))
        (conj (str (count (remove true? sentinels)) " of " n-threads
                   " threads returned from require before the"
                   " namespace's last form had run"))))))

;; Properties C and D: one namespace per thread, all at once.
(defn- distinct-namespaces-failures []
  (let [errs (atom [])
        done (atom [])]
    (run-together n-distinct
      (fn [i]
        (try
          (require (symbol (str "ldrtest.m" i)))
          (let [s (deref (resolve (symbol (str "ldrtest.m" i "/sentinel"))))]
            (swap! done conj (= tally-expected (:tally s)))
            (when-not (= tally-expected (:tally s))
              (swap! errs conj (str "m" i " compiled to the wrong value: tally="
                                    (:tally s) ", expected " tally-expected))))
          (catch Throwable e
            (swap! errs conj (str "m" i ": " (.getMessage e)))))))
    (let [libs (deref (deref (resolve 'clojure.core/*loaded-libs*)))
          missing (atom [])]
      (doseq [i (range n-distinct)]
        (when-not (contains? libs (symbol (str "ldrtest.m" i)))
          (swap! missing conj i)))
      (cond-> []
        (seq @errs)
        (conj (str "parallel requires of distinct namespaces threw: " (pr-str @errs)))
        (not (every? true? @done))
        (conj (str (count (remove true? @done)) " of " n-distinct
                   " parallel requires returned without defining the namespace"))
        (seq @missing)
        (conj (str (count @missing) " of " n-distinct
                   " namespaces are missing from *loaded-libs* after loading in"
                   " parallel: " (pr-str @missing)))))))

;; Property E: two threads entering one require cycle from opposite ends. On the JVM
;; this is a hang — spec-conformant, and OpenJDK closed it won't fix — because each
;; thread ends up waiting on a class the other is initializing. The loader records
;; the owner of every in-flight load, so it walks the wait-for graph before blocking
;; and raises instead. What we assert is both halves of that: SOMEBODY reports the
;; cycle rather than the pair hanging, and the other thread still finishes its load.
;; A hang here fails the run by timeout rather than by assertion, so the deref is
;; bounded — a wedged loader must not wedge the suite.
(defn- deref-timeout [f ms]
  (let [done (atom false) out (atom nil)]
    (future (reset! out (deref f)) (reset! done true))
    (loop [waited 0]
      (cond @done [:ok @out]
            (>= waited ms) [:timeout nil]
            :else (do (Thread/sleep 5) (recur (+ waited 5)))))))

(defn- cycle-failures []
  (let [go? (atom false)
        run (fn [ns-name]
              (future (while (not @go?) (Thread/sleep 1))
                      (try (require (symbol (str "ldrtest." ns-name))) [:ok nil]
                           (catch Throwable e [:err (str (.getMessage e))]))))
        fx (run "x")
        fy (run "y")]
    (reset! go? true)
    (let [[sx rx] (deref-timeout fx 15000)
          [sy ry] (deref-timeout fy 15000)]
      (cond
        (or (= :timeout sx) (= :timeout sy))
        ["two threads entering a require cycle from opposite ends hung; the"
         " wait-for graph walk did not fire"]
        :else
        (let [outcomes [rx ry]
              reported (filter (fn [r] (and (= :err (first r))
                                            (re-find #"Deadlocked require" (second r))))
                               outcomes)
              completed (filter (fn [r] (= :ok (first r))) outcomes)
              other-errs (filter (fn [r] (and (= :err (first r))
                                              (not (re-find #"Deadlocked require" (second r)))))
                                 outcomes)]
          (cond-> []
            (seq other-errs)
            (conj (str "the require cycle raised something other than the cycle"
                       " report: " (pr-str (mapv second other-errs))))
            (empty? reported)
            (conj (str "neither thread reported the require cycle; got "
                       (pr-str outcomes)))
            (empty? completed)
            (conj (str "both threads failed on the require cycle; one of them"
                       " should still complete its load"))))))))

;; Property F: a (dosync (require ...)) that has to wait must not deadlock the loader.
;;
;; A transaction waiting on another thread's load holds nothing, and the mark a load
;; writes goes through a leaf mutex rather than the STM, so a load never waits on a
;; transaction. The load here THROWS on purpose — the rollback is the path with the
;; wide window.
(defn- dosync-wait-failures []
  (let [plain (future (try (require 'ldrtest.boom) :loaded
                           (catch Throwable _ :threw)))
        ;; long enough to be inside the load, short enough to be before it throws
        _ (Thread/sleep 40)
        txn (future (try (dosync (require 'ldrtest.boom)) :loaded
                         (catch Throwable _ :threw)))
        [s1 _] (deref-timeout plain 20000)
        [s2 _] (deref-timeout txn 20000)]
    (cond-> []
      (or (= :timeout s1) (= :timeout s2))
      (conj (str "a (dosync (require ...)) waiting on another thread's load"
                 " deadlocked")))))

;; Property G: a require inside a transaction dedups like any other, and an aborted
;; transaction does not undo a load that really happened.
;;
;; The mark is two tables — loaded-ns and the *loaded-libs* ref — and ns-dedup-loaded?
;; wants both. Writing the ref through the ambient transaction's log split them.
;; A buffered write is invisible until commit, so the SECOND require in the same
;; transaction read the committed ref, saw no mark, and ran the whole namespace
;; again; and an abort discarded the mark while loaded-ns kept it, so the next
;; require after the transaction ran it again too. Neither is the transaction's call
;; to make: the file's defs are in the image either way. ldr-libs-update! writes the
;; ref directly now.
(defn- dosync-dedup-failures []
  (dosync (require 'ldrtest.dedup) (require 'ldrtest.dedup))
  (try (dosync (require 'ldrtest.dedupab) (throw (ex-info "abort" {})))
       (catch Throwable _ nil))
  (require 'ldrtest.dedupab)
  (let [in-txn (deref @(resolve 'ldrtest.dedup/counter))
        aborted (deref @(resolve 'ldrtest.dedupab/counter))]
    (cond-> []
      (not= 1 in-txn)
      (conj (str "two requires of one namespace inside a single dosync ran its top"
                 " level " in-txn " times, expected 1"))
      (not= 1 aborted)
      (conj (str "a require whose transaction then aborted left the namespace"
                 " half-marked; the next require ran its top level again, "
                 aborted " loads in total, expected 1")))))

;; Property H: a load that PARKS. (require 'x) from a go block on the :fiber backend,
;; where x's top level does a blocking channel op, is a continuation escaping the
;; middle of the load — and the fiber comes back to finish it, on the same thread,
;; because a fiber is pinned to its carrier for life.
;;
;; load-namespace* runs the load body inside a dynamic-wind, so the park fired the
;; after thunk: the claim was dropped and the waiters woken onto a half-built
;; namespace, and then the resume died in ldr-assert-claim! with the mark still
;; standing and the rollback stranded in the abandoned guard. The namespace was
;; wedged loaded-but-empty for the life of the process and every later require of it
;; no-op'd without an error. The after thunk skips on a park now.
;;
;; Asserted on the way a caller would notice: the require completes, the namespace is
;; WHOLE (the form after the park ran), and a thread that asked for the same
;; namespace while it was parked got the whole thing too rather than an empty shell.
;; ONE carrier for both H and I, set before anything spawns a fiber (the pool is
;; built at the first spawn and read once). H does not need it. I does: two fibers
;; only share a thread id when they share a carrier, and with the default pool they
;; would land on different ones and exercise the ordinary cross-thread path instead.
(defn- one-carrier! []
  (require 'clojure.core.async)
  (alter-var-root (resolve 'clojure.core.async/*fiber-carrier-count*) (constantly 1)))

(defn- parked-load-failures []
  (one-carrier!)
  (let [go-spawn (resolve 'clojure.core.async/go-spawn)
        backend (resolve 'clojure.core.async/*go-backend*)
        <!! (deref (resolve 'clojure.core.async/<!!))
        ;; go-spawn + the backend var directly rather than the `go` macro: this file
        ;; is loaded as data by `jolt run`, and the pass behind `go` is beside the
        ;; point here — what matters is that the body runs on a fiber.
        out (with-bindings {backend :fiber}
              (go-spawn (fn [] (require 'ldrtest.parky) :done)))
        ;; a second asker, on a plain thread, while the fiber's load is parked
        other (do (Thread/sleep 60)
                  (future (try (require 'ldrtest.parky)
                               (some? (resolve 'ldrtest.parky/after))
                               (catch Throwable e (str (.getMessage e))))))
        [s r] (deref-timeout (future (<!! out)) 15000)
        [s2 r2] (deref-timeout other 15000)]
    (cond-> []
      (= :timeout s)
      (conj "a require whose namespace parks at top level never completed")
      (and (not= :timeout s) (not= :done r))
      (conj (str "a require from a fiber over a parking namespace failed: "
                 (pr-str r) " (the load body's continuation could not be resumed)"))
      (nil? (resolve 'ldrtest.parky/after))
      (conj (str "ldrtest.parky is marked loaded but half-built: the form after its"
                 " top-level park never ran, and require no-ops from here on"))
      (= :timeout s2)
      (conj "a thread requiring a namespace whose load was parked never returned")
      (and (not= :timeout s2) (not (true? r2)))
      (conj (str "a thread that required ldrtest.parky while its load was parked got"
                 " a half-built namespace: " (pr-str r2))))))

;; Property I: a SIBLING FIBER on the carrier the parked load is on (jolt-n31).
;;
;; Two fibers on one carrier share a thread id, so keying load ownership by thread
;; read the parked load's claim as the sibling's own: it took step 3, the cycle
;; break, and returned with the namespace half-loaded and no error. Ownership is by
;; execution context now, so the sibling sees somebody else's claim and waits.
;;
;; And it has to wait by PARKING. The load it is waiting for is parked on the carrier
;; they share, so a condition-wait would block the one thread that can ever resume it
;; — the fix for the stale read would have been a hang. ldr-wait-for-load! picks the
;; mechanism from the waiter and ldr-end-load! resumes the parked ones.
;;
;; One carrier and two fibers requiring the same parking namespace, the second
;; spawned while the first is inside the load: the first claims and parks on its
;; channel, the carrier then runs the second, which finds the claim. Both must come
;; back with the namespace whole. A hang here fails by timeout, which is why the
;; derefs are bounded.
(defn- sibling-fiber-failures []
  (let [go-spawn (resolve 'clojure.core.async/go-spawn)
        backend (resolve 'clojure.core.async/*go-backend*)
        <!! (deref (resolve 'clojure.core.async/<!!))
        ask (fn [] (with-bindings {backend :fiber}
                     (go-spawn (fn []
                                 (try (require 'ldrtest.sibling)
                                      (if (resolve 'ldrtest.sibling/after) :whole :half)
                                      (catch Throwable e (str (.getMessage e))))))))
        a (ask)
        ;; inside the load and parked by now, not merely queued behind it
        b (do (Thread/sleep 60) (ask))
        [sa ra] (deref-timeout (future (<!! a)) 15000)
        [sb rb] (deref-timeout (future (<!! b)) 15000)]
    (cond-> []
      (or (= :timeout sa) (= :timeout sb))
      (conj (str "two fibers on one carrier requiring a namespace that parks did not"
                 " both return: the sibling blocked the carrier its owner needed in"
                 " order to finish"))
      (and (not= :timeout sa) (not= :whole ra))
      (conj (str "the fiber that owned the parked load came back with "
                 (pr-str ra) ", expected :whole"))
      (and (not= :timeout sb) (not= :whole rb))
      (conj (str "a sibling fiber on the same carrier came back with " (pr-str rb)
                 ", expected :whole — it read the parked load's claim as its own"
                 " and took the cycle break")))))

;; Property J: requiring-resolve does not hold a process-wide lock across its
;; require. B is inside rly's load (and will requiring-resolve from there) before A
;; starts; A's requiring-resolve of rlx requires rly and so waits on B. If A held
;; RT/REQUIRE_LOCK while waiting, B's reach for it would close a cycle the loader
;; cannot detect, and both would hang — which is what this reported before
;; requiring-resolve became a plain require.
(defn- require-lock-failures []
  (let [b (future (try (require 'ldrtest.rly) :ok
                       (catch Throwable e (str (.getMessage e)))))
        _ (loop [n 0]
            (when (and (not @rl-y-started) (< n 5000)) (Thread/sleep 1) (recur (inc n))))
        a (future (try (some? (requiring-resolve 'ldrtest.rlx/foo))
                       (catch Throwable e (str (.getMessage e)))))
        ;; long enough for A to be waiting on rly, which is where it holds a lock if
        ;; it holds one at all
        _ (Thread/sleep 200)
        _ (reset! rl-go true)
        [sa ra] (deref-timeout a 15000)
        [sb rb] (deref-timeout b 15000)]
    (cond-> []
      (or (= :timeout sa) (= :timeout sb))
      (conj (str "a requiring-resolve waiting on another thread's load, whose top level"
                 " itself requiring-resolves, hung: requiring-resolve holds a"
                 " process-wide lock across its require, and the loader's wait-for"
                 " graph cannot see it"))
      (and (not= :timeout sa) (not (true? ra)))
      (conj (str "requiring-resolve through a namespace another thread was loading"
                 " came back with " (pr-str ra)))
      (and (not= :timeout sb) (not= :ok rb))
      (conj (str "the load that requiring-resolved from its own top level failed: "
                 (pr-str rb))))))

(defn- clean-up! []
  ;; the temp root is per-run (currentTimeMillis), so clean it up rather than
  ;; leave one behind on every smoke run
  (doseq [i (range n-distinct)]
    (try (.delete (java.io.File. (str tmp-root "/ldrtest/m" i ".clj")))
         (catch Throwable _ nil)))
  (doseq [p [(str tmp-root "/ldrtest/target.clj") (str tmp-root "/ldrtest/x.clj")
             (str tmp-root "/ldrtest/y.clj") (str tmp-root "/ldrtest/boom.clj")
             (str tmp-root "/ldrtest/dedup.clj") (str tmp-root "/ldrtest/dedupab.clj")
             (str tmp-root "/ldrtest/parky.clj") (str tmp-root "/ldrtest/sibling.clj")
             (str tmp-root "/ldrtest/rlx.clj") (str tmp-root "/ldrtest/rly.clj")
             (str tmp-root "/ldrtest/rlz.clj")
             (str tmp-root "/ldrtest") tmp-root]]
    (try (.delete (java.io.File. p)) (catch Throwable _ nil))))

(defn -main []
  (write-sources!)
  (jolt.host/set-source-roots! (vec (cons tmp-root (jolt.host/source-roots))))
  (let [failures (-> []
                     (into (one-namespace-failures))
                     (into (distinct-namespaces-failures))
                     (into (cycle-failures))
                     (into (dosync-wait-failures))
                     (into (dosync-dedup-failures))
                     (into (parked-load-failures))
                     (into (sibling-fiber-failures))
                     (into (require-lock-failures)))]
    (clean-up!)
    (if (seq failures)
      (do (doseq [f failures] (println "FAIL:" f))
          (println "CONCURRENT-REQUIRE FAILED")
          (System/exit 1))
      (println "CONCURRENT-REQUIRE OK" n-threads "threads on 1 namespace,"
               n-distinct "namespaces in parallel, cycle and dosync-wait not hung,"
               "dosync dedups, a parked load finishes for its owner and its"
               "siblings, requiring-resolve takes no process-wide lock"))))

(-main)

;; Done with the agent system's pools (futures, agents): end them, as a JVM
;; program does, or their idle workers hold the process up for their keep-alive.
(shutdown-agents)
