;; The gate: which tasks it runs, and the runner that makes an incomplete run
;; impossible to read as a pass. Loaded by deps.edn's :init.
;;
;; Three things go wrong with a gate otherwise, and all three have happened
;; (the Makefile's run-gate comment has the history): a run that stops at the
;; first failure leaves the rest unrun while the log looks like a pass; a log
;; piped through tail ends on some passing gate's output; an ignore-failures
;; flag exits 0. So the runner runs every gate, always prints a verdict line
;; last, exits non-zero on any failure, and writes target/gate-receipt naming
;; the covered tree only on a complete pass. `jolt gate-status` reads it.
(ns tasks.gate
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [babashka.tasks :as t]
            [clojure.string :as str]
            [tasks.env :as env])
  (:import [java.util.concurrent Executors Callable]))

;; The CI gate: behavior only. selfhost is not in it, because the byte
;; fixpoint only holds on the Chez that minted the seed (jolt-8479).
(def ci-gates
  '[submodules values flonumprint recordinline corpus nsaudit unit documented grenadine clishim
    mvnhttp readscaling gcpolicy stacklimit footprint lazyretain compilescaling applyscaling
    lazyscaling vecscaling pipescaling chunkscaling printscaling pmoverhead complexity ioscaling
    hotscaling fastpathratio stdlibratio depssmoke taskssmoke scriptsmoke exitwait completionssmoke
    depscpcache depsunit
    smoke tracesmoke errorreport errorkinds buildsmoke embeddedresources reprobuildsmoke
    profilebuildsmoke buildlibsmoke staticnativesmoke zlibregistersmoke sci scifunctional cts
    loaderconf loadercache ffi ffidupsym ffiloadfail continuations stdlibfasl zlibunit depsnounzip
    zlibnativesmoke zipmemory noexecsmoke
    transient sortedcoll rrbprop rrbscaling stateimage infer inferscaling wp devirt fieldread numwp
    fieldnum fieldjoin contagion
    hasheq narrowhash
    protoret accfix pic narrow directlink linkedrace mintgate directcall defmetacells staticsite
    arraymap hamt recfast recfold arraybacking largebytesgc largefxgc unitcontext numeric oparity s64wrap
    toplevelinline mathfl flonumdisp flarr
    fnform poolshape coreproc traceemit traceeval tracecompilecost degradedbacktrace
    inline inline-body guardedinline dcerefs shakelocal manifestcheck readmecheck portcheck
    mirrordrift regexdfacheck regexdfa regexanchor regexanchorprims regexanchorcheck regexreplace
    regexsyntax regexbacktrack deadhost recordshadow emittedheads adaptercheck hostprops
    normalizecheck hostregistry hostarity enummodel imagecoverage lazyshims foreignhandles
    dispatchalloc hostsite regexmatcher winpath winplatform winparity statlayout lockcheck
    mmepochcheck parkcheck shelloutcheck errnocheck irvalidate seeddefs devbootsmoke
    gatebootsmoke aotcachesmoke aotworkersmoke aotcachepathsmoke aotcachesourcepathsmoke aotcachestarfilesmoke
    aotfingerprint vfaslceiling vfaslstrings buildscaling buildnatives buildsegments
    compilepathsmoke makefilesmoke versionsmoke attributioncheck noticecheck taskparity
    systemstreams closedstdout stderrsplit filelock deleteonclose userdirpaths mimetypes utf8decode
    certify gambitcheck gambitkernel gambitgencheck gambitseedcheck gambitboot gambiteval
    gambitunbound gambitvars gambitstatics gambittwins gambitprofile grenadinecheck fibers
    fiberspoll gosm asynctimer ticks interruptnest monitorescape windescape threadsafety
    maskedacquire ioerrno cas flow native nativebuild])

;; The full gate (dev machine): the CI gate plus the self-host byte fixpoint.
(def test-gates '[submodules selfhost ci])

;; Gates that judge a constant-factor timing ratio. They run on their own once
;; every other gate has finished: beside a parallel neighbour, a burst on one
;; arm moved a ratio by 1.5-3x (fastpathratio over its ceiling in 9 of 134 CI
;; jobs, 2026-11-03..04; stdlibratio the same way, jolt-h1d; pmoverhead read
;; 0.65-1.87 beside its shard for a quiet 1.05-1.15). A scaling gate
;; (n vs 4n of the same work) is not listed: interference moves both arms alike.
(def quiet-gates '#{fastpathratio stdlibratio pmoverhead})

(def receipt "target/gate-receipt")

(defn jobs
  "`-j N`, `-jN` or a bare `-j` (one per core) from the task's arguments; 1
  without one, as make runs."
  [args]
  (let [cores (.availableProcessors (Runtime/getRuntime))]
    (loop [[a & more] args]
      (cond
        (nil? a) 1
        (= "-j" a) (if-let [n (some-> (first more) parse-long)] n cores)
        (str/starts-with? a "-j") (or (parse-long (subs a 2)) cores)
        :else (recur more)))))

(defn fingerprint
  "A hash of every tracked and every untracked-but-not-ignored file (name and
  content, over a sorted list) plus the pinned submodule SHAs, so adding a
  file, editing one or bumping a submodule all invalidate the receipt."
  []
  (env/sh-out (str "sha=$(command -v sha256sum >/dev/null 2>&1 && echo sha256sum || echo 'shasum -a 256'); "
                   "{ git ls-files -z -c -o --exclude-standard | LC_ALL=C sort -z | xargs -0 $sha 2>/dev/null; "
                   "git submodule status --recursive 2>/dev/null; } | $sha | cut -d' ' -f1")))

;; The jolt running this gate, so each gate runs under it too: bin/jolt
;; exports JOLT_EXE, and a built jolt knows its own path. babashka.tasks/jolt
;; would take `jolt` from PATH, which is absent on CI and an older installed
;; jolt on a dev machine.
(def ^:private self-jolt
  (or (env/getenv "JOLT_EXE") (jolt.host/self-exe) (str env/root "/" env/jolt-bin)))

;; make runs each target once per invocation, and each gate here is its own
;; process that would walk its :depends again. Rebuilt beside other gates, that
;; raced: devbootsmoke touches rt.ss, a neighbour's testbin saw it and relinked
;; target/release/jolt under aotcachesmoke, and gatebootsmoke re-ran
;; devbootsmoke beside the gate itself. So the runner lists in JOLT_GATE_DONE
;; what it already ran, and those tasks check done? and skip.
(defn done?
  "True when the gate runner already ran `task` for the gate in this process."
  [task]
  (contains? (set (str/split (or (env/getenv "JOLT_GATE_DONE") "") #" ")) (str task)))

(defn- deps-of
  "Every task `task` depends on, transitively, as symbols."
  [tasks task]
  (let [d (:depends (get tasks task))]
    (into (set d) (mapcat #(deps-of tasks %) d))))

(defn- run-one
  "One gate as its own `jolt <gate> -j n` process; the -j reaches the one
  gate that is itself a gate list (test runs ci). `done` names the tasks it
  skips. Returns [gate exit-status]."
  [n gate done]
  [gate (:exit (p/shell {:continue true :dir env/root
                         :extra-env {"JOLT_GATE_DONE" (str/join " " (map str done))}}
                        self-jolt (str gate) "-j" (str n)))])

(defn run-gates
  "Run `gates`, at most `n` at once, then the quiet gates one at a time with
  nothing beside them. Every gate runs whatever fails. Returns the failed
  gates. testbin runs once first and no gate builds it again. A gate that
  depends on another gate of the run waits for it and skips it."
  [gates n]
  (t/run 'testbin)
  (let [tasks (:tasks (read-string (slurp (str env/root "/deps.edn"))))
        in-run (set (map symbol gates))
        before (fn [g] (filter in-run (deps-of tasks (symbol g))))
        done (fn [g] (into #{'testbin} (mapcat #(cons % (deps-of tasks %))) (before g)))
        ;; A gate's in-run deps have fewer in-run deps of their own, so this
        ;; order submits them first: by the time the pool takes a gate, what it
        ;; waits on has started.
        {quiet true loud false} (group-by (comp boolean quiet-gates symbol)
                                          (sort-by #(count (before %)) (map str gates)))
        pool (Executors/newFixedThreadPool (max 1 n))
        loud-results
        (try (let [futs (reduce (fn [acc g]
                                  (let [waits (keep #(get (into {} acc) (str %)) (before g))]
                                    (conj acc [g (.submit pool ^Callable
                                                          (fn [] (run! #(.get %) waits)
                                                            (run-one n g (done g))))])))
                                [] loud)]
               (mapv #(.get (second %)) futs))
             (finally (.shutdown pool)))
        results (into loud-results (map #(run-one 1 % (done %)) quiet))]
    (vec (keep (fn [[g ex]] (when-not (zero? ex) g)) results))))

(defn run-gate!
  "The `test`, `ci` and `bionic-ci` tasks."
  [gate-name gates args]
  (fs/delete-if-exists receipt)
  (let [failed (run-gates gates (jobs args))]
    (if (empty? failed)
      (do (fs/create-dirs "target")
          (spit receipt (str "gate: " gate-name "\n"
                             "tree: " (fingerprint) "\n"
                             "head: " (or (env/sh-out "git rev-parse HEAD 2>/dev/null") "none") "\n"
                             "targets: " (str/join " " gates) "\n"))
          (println (str "OK: " gate-name " gate passed (" (count gates) " targets)")))
      (do (println (str "FAILED: " gate-name " gate: " (count failed) " of " (count gates)
                        " targets failed: " (str/join " " failed)))
          (System/exit 1)))))

(defn gate-status!
  "Is THIS working tree covered by a complete gate run?"
  []
  (if-not (fs/exists? receipt)
    (do (println "NOT GATED: no receipt, run 'jolt test'") (System/exit 1))
    (let [r (slurp receipt)
          field #(second (re-find (re-pattern (str "(?m)^" % ": (.*)$")) r))]
      (if (= (field "tree") (fingerprint))
        (println (str "GATED: " (field "gate") " gate passed on this exact tree"))
        (do (println "NOT GATED: tree changed since the last full gate run")
            (println (str "  receipt: " (field "gate") " at " (field "head")))
            (System/exit 1))))))

;; --- shards ------------------------------------------------------------------
;; One shard of a gate, so CI can run it as parallel jobs. SHARD_SET is test
;; (selfhost plus the CI gates) or ci (the CI gates alone, what bionic runs);
;; shard SHARD of SHARDS takes every SHARDS-th gate of the set's sorted list,
;; so the shards between them run every gate exactly once and a gate added to
;; ci-gates lands in some shard without anyone listing it. A shard is not the
;; gate: it leaves no receipt.

(defn shard []
  (let [set-name (or (env/getenv "SHARD_SET") "test")
        pool (->> (if (= "ci" set-name) ci-gates (concat test-gates ci-gates))
                  (map str) (remove #{"ci"}) distinct sort vec)
        i (some-> (or (env/getenv "SHARD") "1") parse-long)
        n (some-> (or (env/getenv "SHARDS") "1") parse-long)]
    {:set set-name :pool pool :shard i :shards n
     :gates (when (and i n (pos? n))
              (vec (keep-indexed (fn [k g] (when (= (mod k n) (dec i)) g)) pool)))}))

(defn ci-shard! [args]
  (let [{:keys [set pool shard shards gates]} (shard)]
    (when-not (#{"test" "ci"} set)
      (println (str "ci-shard: SHARD_SET must be test or ci, not '" set "'")) (System/exit 2))
    (when-not (and shard shards (<= 1 shard shards))
      (println (str "ci-shard: need 1 <= SHARD <= SHARDS, got SHARD=" (env/getenv "SHARD")
                    " SHARDS=" (env/getenv "SHARDS")))
      (System/exit 2))
    (println (str "ci-shard: " set " shard " shard "/" shards ", " (count gates) " of " (count pool) " gates"))
    (let [failed (run-gates gates (jobs args))]
      (if (empty? failed)
        (println (str "OK: " set " shard " shard "/" shards " passed (" (count gates) " targets)"))
        (do (println (str "FAILED: " set " shard " shard "/" shards ": " (count failed) " of "
                          (count gates) " targets failed: " (str/join " " failed)))
            (System/exit 1))))))
