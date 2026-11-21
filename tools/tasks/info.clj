;; `jolt info`: every task, grouped by what it is about, with its one-line doc.
;; Easier to scan than `jolt tasks`, which is one alphabetical wall of 250.
;; deps.edn holds its tasks in the same groups and order. A task in no group
;; is printed under "Ungrouped", and taskparity fails on it, so a new task gets
;; a home instead of disappearing from the listing.
(ns tasks.info
  (:require [clojure.string :as str]))

(def groups
  [["Start here"
    '[info chez-path deps submodules hooks]]

   ["Build and install"
    '[build default testbin install-local jolt jolt-release jolt-debug remint devboot gateboot]]

   ["The gate"
    '[test ci bionic-ci ci-shard ci-shard-list gate-status]]

   ["Seed and self-host"
    '[selfhost seeddefs mintgate manifestcheck]]

   ["Conformance against Clojure on the JVM"
    '[corpus cts sci scifunctional certify documented documented-record unit libconformance libperf]]

   ["Values and collections"
    '[values hasheq narrowhash transient sortedcoll rrbprop rrbscaling arraymap arraybacking recfast
      recordinline recordshadow complexity cas s64wrap flonumprint flonumdisp utf8decode
      stateimage imagecoverage]]

   ["Compiler: inference and optimization"
    '[infer inferscaling wp numwp fieldjoin fieldnum fieldread contagion narrow protoret accfix pic
      devirt directlink directcall linkedrace inline inline-body guardedinline toplevelinline
      numeric mathfl flarr oparity recfold mandelbrot-num]]

   ["Compiler: emission and IR"
    '[dcerefs fnform poolshape coreproc defmetacells staticsite hostsite dispatchalloc unitcontext
      irvalidate emittedheads compilescaling shakelocal shakesmoke]]

   ["Errors and tracing"
    '[errorreport errorkinds tracesmoke traceemit traceeval tracecompilecost degradedbacktrace
      stacklimit]]

   ["Scaling and performance"
    '[readscaling applyscaling lazyscaling vecscaling pipescaling chunkscaling printscaling
      pmoverhead ioscaling hotscaling fastpathratio stdlibratio lazyretain footprint gcpolicy
      largebytesgc largefxgc zipmemory buildscaling doseqperf printperf sbperf aotcacheperf
      dynbench fibersbench]]

   ["Fibers, threads and locks"
    '[fibers fiberspoll fibersresidue gosm asynctimer flow ticks interruptnest monitorescape
      windescape threadsafety maskedacquire lockcheck parkcheck mmepochcheck exitwait continuations
      monitormodel enumregistrymodel cowpublishmodel failfastmodel]]

   ["CLI, deps.edn and tasks"
    '[smoke scriptsmoke depssmoke depsunit depscpcache depsnounzip taskssmoke completionssmoke
      clishim grenadine grenadinecheck mvnhttp httpsfetch userdirpaths versionsmoke makefilesmoke
      loaderconf loadercache nsaudit]]

   ["jolt build and the AOT cache"
    '[buildsmoke buildlibsmoke buildnatives buildsegments reprobuildsmoke profilebuildsmoke
      staticnativesmoke embeddedresources joltsmoke aotcachesmoke aotcachepathsmoke
      aotcachesourcepathsmoke aotcachestarfilesmoke aotfingerprint compilepathsmoke devbootsmoke
      gatebootsmoke vfaslceiling vfaslstrings stdlibfasl]]

   ["FFI, natives and zlib"
    '[ffi ffidupsym ffiloadfail foreignhandles statlayout errnocheck ioerrno zlibunit
      zlibnativesmoke zlibregistersmoke]]

   ["Host runtime"
    '[systemstreams closedstdout stderrsplit mimetypes noexecsmoke hostarity hostprops
      hostregistry enummodel lazyshims deadhost adaptercheck portcheck census normalizecheck]]

   ["Regex"
    '[regexdfa regexdfacheck regexdfacheck-regen regexanchor regexanchorprims regexanchorcheck
      regexanchorcheck-regen regexbacktrack regexmatcher regexreplace regexsyntax]]

   ["Windows behavior, checked on POSIX"
    '[winpath winplatform winparity shelloutcheck]]

   ["Gambit"
    '[gambitcheck gambitkernel gambiteval gambitboot gambitgen gambitgencheck gambitseed
      gambitseedcheck gambitunbound gambitunbound-regen gambitvars gambitvars-regen gambitstatics
      gambitstatics-regen gambittwins gambitprofile gambitweb mirrordrift mirrordrift-regen]]

   ["Repository hygiene"
    '[attributioncheck noticecheck readmecheck kondocheck taskparity]]])

(defn grouped-names [] (into #{} (comp (mapcat second) (map str)) groups))

(defn- first-line [doc]
  (let [l (first (str/split-lines (or doc "")))]
    (if (> (count l) 78) (str (subs l 0 75) "...") l)))

(defn print-info
  "`tasks` is the deps.edn :tasks map (symbol -> task)."
  [tasks]
  (let [esc (str (char 27) "[")
        tty? (some? (System/console))
        col (fn [c s] (if tty? (str esc c "m" s esc "0m") s))
        names (into #{} (comp (filter symbol?) (map str)) (keys tasks))
        doc #(first-line (:doc (get tasks (symbol %))))
        width (+ 6 (apply max (map count names)))
        row (fn [t] (println (str "  " (col "36" (format (str "%-" width "s") (str "jolt " t))) (doc t))))
        ungrouped (sort (remove (grouped-names) names))]
    (println (col "1;35" "\njolt: build and gate tasks (the Makefile's targets, run by jolt)"))
    (doseq [[title ts] groups]
      (println (str "\n" (col "35" (str "── " title " ──"))))
      (doseq [t ts :when (names (str t))] (row (str t))))
    (when (seq ungrouped)
      (println (str "\n" (col "33" "── Ungrouped (add these to tools/tasks/info.clj) ──")))
      (doseq [t ungrouped] (row t)))
    (println (str "\n" (col "2" "first run: jolt deps  ·  build: jolt build  ·  gate: jolt test -j 8")))))
