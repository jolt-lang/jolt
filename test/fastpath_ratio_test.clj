;; The fast paths that keep reading and string-scanning off the slow route, each
;; gated as a RATIO against a reference the same run measures on the same machine.
;;
;; These are constant-factor gates, and that is why they are not written like
;; read_scaling_test.clj or io_scaling_test.clj. Every regression below is
;; perfectly LINEAR — it just has a terrible constant — so a 1x-vs-4x scaling
;; ratio sees nothing at all. What separates fast from slow here is that the same
;; work is available two ways, and the slow way is 5-70x the fast one. Measuring
;; both arms in one process and judging their ratio needs no absolute budget, no
;; baseline file, and no babashka in CI.
;;
;; What each arm gates (all figures x86_64, jolt vs babashka 1.12, 2026-09-13):
;;
;;   reader-vs-string   (read <stream-backed PushbackReader>) used to drain the
;;                      reader one character at a time through
;;                      record-method-dispatch — a method-table lookup by hashing
;;                      the jhost tag and a handler lookup by hashing the method
;;                      NAME, per character. 332 files cost 3909 ms against bb's
;;                      57; the same parse from a string cost 52. The reference
;;                      arm IS that string parse, so the gate asks: does reading
;;                      from a stream still cost about what reading from a string
;;                      costs?
;;
;;   chunked-vs-slurp   .read(char[]) is the memory-bounded way to read a large
;;                      file, and it was SLOWER than reading the whole thing
;;                      (2.7x), because the fill loop set one array element at a
;;                      time through the checked generic setter. The gate asks
;;                      the question that made it absurd: is the bounded read at
;;                      least not slower than the unbounded one?
;;
;;   literal-split      #"\n" is a literal, and running an irregex search per
;;   literal-replace    line to find one character cost ~10x. The reference arm
;;   split-lines        is the SAME call with a literal string separator, which
;;                      always took the index scan. If the literal recognizer
;;                      stops recognising, the regex arm falls back to the engine
;;                      and the ratio jumps.
;;
;;   small-file-drain   the regression that ACTUALLY SHIPPED during this work.
;;                      A block-reading drain that sizes its scratch to the BLOCK
;;                      rather than to the input reads large files beautifully and
;;                      allocates 64 KB to hold 200 bytes — once per file. Every
;;                      other arm here uses a large payload and saw nothing; a
;;                      suite that reads many small files went from 900 ms to
;;                      1.8 s. The reference arm reads the SAME FILES, the same
;;                      number of times, by PATH — which takes the bytevector
;;                      route and never touches the drain. Holding everything but
;;                      the drain constant is what makes the ratio sharp.
;;
;;   chars-copy         a char array's backing IS a Chez string, so .toCharArray
;;   string-from-chars  is a string copy and (String. ca) is a substring of it.
;;                      Both used to build a cons per character and then walk the
;;                      list again — 21x babashka on the chunked-read path that
;;                      feeds them. The reference arm for both is (subs s 0 n),
;;                      which copies exactly as many characters by a route that
;;                      was always a block move. If the backing goes back to a
;;                      boxed vector, both arms walk again and the ratios jump
;;                      (measured: 0.68 -> 2.47 and 1.00 -> 1.63).
;;
;;   char-alloc         the same backing, seen with no traversal in it at all.
;;                      A char array of n elements is a flat 4-byte-per-character
;;                      string the collector never traces; a boxed vector of the
;;                      same n is n POINTERS at 8 bytes that it traces on every
;;                      major GC. So a char array must hold half the heap of a
;;                      long array of the same length: 0.50, where a boxed char
;;                      backing reads 1.00 (as object-array does). Measured in
;;                      BYTES, not time: timed, this arm's cost was set by the
;;                      adaptive nursery the earlier arms left behind — the same
;;                      binary read 0.29 to 0.53 across nursery sizes, and CI
;;                      runners 0.81 and 0.87 against the 0.8 ceiling.
;;
;;   substring-scan     str-index-of is the widest-reach scan in the string
;;                      layer: indexOf, contains, literal split and both literal
;;                      replaces all run it once per character. It called
;;                      char-by-char-match? at EVERY position, so the call cost
;;                      more than the comparison — testing the first character
;;                      inline took a 7.8 MB miss from 56 ms to 18 ms. The
;;                      reference arm is the SAME search with a CHAR needle,
;;                      which takes str-char-index and never had an inner call,
;;                      so it is unaffected by the regression and measures 23 ms
;;                      on both sides (0.80 -> 2.43).
;;
;;   trim-no-copy       clojure.string/trim was (trimr (triml s)), and triml
;;                      copied the whole string even with nothing to trim, so
;;                      trimming an already-trimmed string allocated it twice.
;;                      This one IS a scaling test, and deliberately: trimming a
;;                      string that needs no trimming must cost the same whether
;;                      the string is short or long. A copying trim is linear in
;;                      the length; a scanning one is not.
;;
;;   reify-instance     (instance? SomeProtocol r) on a reify walked every
;;   deftype-instance   host-shim instance-check arm (23 of them, each passing)
;;                      and then munged every declared protocol name to compare
;;                      it — ~3.5 us for a reify of six protocols against the
;;                      JVM's ~14 ns, ~500 ns for a deftype. core.logic asks it
;;                      of every var and constraint on every propagation step,
;;                      and its finite-domain solver spent 56 s seeing a trivial
;;                      contradiction. The reference arm calls a protocol method
;;                      on the same value, which never walked anything. Measured
;;                      before/after: reify 204 -> 1.38, deftype 7.75 -> 1.13.
;;
;;   instance-miss      ...and a NO walked all of them every time, then the
;;   number-miss        JVM taxonomy: 500-700 ns for (instance? IVar 5), which
;;                      core.logic asks of every term it walks. The builtin arms'
;;                      verdict per value kind is memoized now, the library arm
;;                      still asked live (31 -> 1.55, 22 -> 1.37).
;;
;;   instance-site      ...and past both memos, each (instance? T x) SITE caches
;;   instance-site-type its last type argument and receiver kind, so a repeat is
;;                      two eq?s and an epoch compare — the shape of the JVM's
;;                      inlined instanceof, where the memo path still interned the
;;                      type name and read two tables per call. Held near a
;;                      protocol call: the site measured 0.32 (quoted name) and
;;                      0.42 (a deftype as T) of one before the call got its
;;                      own inline cache, about 0.75-0.9 of the cached call
;;                      now; without the site cache, 1.15-1.95 of the uncached.
;;
;;   reify-construct    (reify …) built a {kw fn} map and then a hashtable from it
;;                      per instance (~310 ns against the JVM's ~25). The layout is
;;                      the site's, built once; an instance is a vector of fns.
;;                      The reference constructs a deftype (8.9 -> 1.05).
;;
;;   satisfies-reify    satisfies? on a reify re-munged every declared protocol
;;                      name ahead of the match (13.5 -> 0.82).
;;
;;   record-key-get     a map keyed on a deftype that declares equals/hashCode
;;                      (core.logic's LVar) looked both methods up by NAME, in two
;;                      string-keyed tables, on every key compared, and its
;;                      (.-id ^EqKey o) field read interned a keyword per call.
;;                      The reference is the same lookup over defrecord keys,
;;                      whose = is structural with no method to call, so the
;;                      ratio is what calling a declared equals adds per key
;;                      (4.66 -> 2.3; the JVM ~1.0). The field read then went
;;                      through record-method-dispatch's per-type table, ~40 ns
;;                      of every compare; a per-site cache of the receiver's type
;;                      and slot made it ~14 (2.3 -> 1.65). The REFERENCE then
;;                      got faster — a record key's hash and = answer from the
;;                      plain-record paths (#1470), 12.5 -> 9.8 ms here with the
;;                      EqKey arm unchanged — so the same EqKey cost reads
;;                      ~2.6, and the ceiling moved with it (3.2 -> 3.8).
;;
;;   record-eq          = on two records walked every registered equality arm
;;                      (the record arm registered first, so it was asked last)
;;                      and then looked the types' equiv and equals up by name
;;                      per compare: ~225 ns against ~36 for two one-element
;;                      vectors, and a small map keyed on records scans with =.
;;                      The reference is = on two equal vectors (6.8 -> 1.36).
;;
;;   record-count       a plain defrecord (one declaring none of the collection
;;   record-contains    methods) against the array map holding the same three
;;   record-reduce-kv   entries. Every one of these walked the host's whole arm
;;   record-into        list — ~17 predicates, one of them 58 ns on its own —
;;   record-seq         before reaching the record arm, which then looked the
;;                      type's declared count/containsKey/… up by NAME (several
;;                      string-keyed lookups per op); reduce-kv and into went
;;                      through a seq of entries on top. A plain record now
;;                      answers from its slots ahead of the walk (records-coll.ss
;;                      jrec-plain?), as a defrecord's generated methods do on
;;                      the JVM (#1470). Measured on x86_64, 2026-11:
;;                      count 16.3 -> 2.8, contains 6.6 -> 2.0, reduce-kv
;;                      20.9 -> 1.8, into 5.95 -> 1.4, seq 4.6 -> 1.2.
;;
;;   record-map->       map->R of a map holding one key that is not a field,
;;                      against building the same record positionally and
;;                      assoc'ing that key. A pre-check for "only field keys"
;;                      (a count plus a contains? per field) ahead of the old
;;                      dissoc-copy-and-fold made this 4.1 where it had been
;;                      2.45; one fold over the map that skips the field keys
;;                      reads ~1.85 (jolt run, arm64).
;;
;;   top-level-fn       a fn built in a bare top-level form — every deftype and
;;                      defrecord method, every extend-type impl and defmethod —
;;                      got no constant pool, so each keyword literal in it
;;                      re-interned on every call. The reference is the same fn
;;                      built under a def, which always had one (2.38 -> 1.02).
;;
;;   pipe-read          a child's stdout pipe copied each chunk out of its foreign
;;   pipe-write         read buffer into the port's bytevector one byte at a time
;;                      through sa-foreign-ref (and stdin the same way in), ~30 ns
;;                      a byte: draining `cat` of a 64 MB file took 2 s against
;;                      5 ms for the same file read directly, ~5x through a
;;                      line-seq. The reference arm spawns the same child and
;;                      reads (or writes) the same bytes through a file, so the
;;                      ratio is the pipe transfer alone (read ~45 -> ~1).
;;
;; Only the ratios are judged, never the absolute times — a slow shared runner
;; moves both arms together. Sampling is best-of-N (minimum, not mean —
;; interference only ever adds time) against a ceiling, and a row over it is
;; sampled up to twice more, judging each arm's minimum over all of it, so a
;; scheduler blip cannot fail the build. A real regression measures above the ceiling on every attempt, so
;; re-measuring costs no power. The Makefile runs this gate after the rest of
;; its shard has finished (QUIET-GATES), not beside it under make -j.

;;   hashmap-get        get on a java.util.HashMap walks the get arms to the
;;                      HashMap family's; a shim registering its arm AHEAD of it
;;                      adds a predicate to every such get (measured against
;;                      get on a persistent map, 2026-10: 4.6 on x86_64 Linux,
;;                      5.0 on macOS arm64, 6.1 on bionic arm64).
;;
;;   string-nth         nth / get on a string against the same on a vector: a
;;   string-get         string answers ahead of jolt-nth's set! wraps and the
;;                      get arms, so a new host family cannot slow it.
;;
;;   arraylist-count    count / contains? on an ArrayList, HashSet or HashMap
;;   hashset-count      against the collection's own .size / .containsKey: the
;;   hashmap-count      ratio is the arm walk ahead of the handler. The 0.8.24
;;   tag-pred-memo      cycle added the fail-fast, concurrent and legacy
;;                      families ahead of these, each a string compare per
;;                      call, and count got 1.2-1.3x slower while every answer
;;                      stayed right; the tag predicates are now remembered by
;;                      tag (define-jhost-tag-pred). Measured on macOS arm64,
;;                      2026-10: before 5.1 / 4.4 / 6.9 / 1.80, after 2.9 / 2.5 /
;;                      3.0 / 1.36 (0.8.23 read 4.3 / 3.3 / 6.1 / 1.31; the last
;;                      is contains?, whose row is now tag-pred-memo). get
;;                      against .get moved 1.24 -> 1.49 -> 1.22, too close to
;;                      its noise under make -j for a row of its own.
;;
;;   path-method        an unhinted Path member against an ArrayList's .size.
;;                      Path's members were a method arm that walked the arms
;;                      ahead of it and matched the name per call (~10x .size);
;;                      as a host method table they take the call-site cache.
;;
;;   host-object-pr     pr-str of a java.io.File is the #object fallback: the
;;                      print arms, the class name, the str registry, and the
;;                      identity hash minted for each fresh object. Against a
;;                      small vector's print (measured 1.24-1.40, 2026-10; then
;;                      1.53-1.60 once every #object carried its identity hash and
;;                      the vector print got ~12% faster, the File side itself
;;                      within 1.1x of before). Re-based 2026-11 at 2.4: the
;;                      streaming printer took the vector side from ~10 ms to
;;                      ~5.3 ms per 20k while the File side also got faster
;;                      (~13 ms to ~8.5 ms, release binaries, A/B in one
;;                      session), which left the ratio at 1.5-1.7 on macOS and
;;                      1.94-2.05 on the Linux runner.
;;
;;   var-call           the per-operation costs a reader pays per character and
;;   multimethod-call   per node (#1713, rewrite-clj and tools.reader at ~10x
;;   str-sb             the JVM). A call through a var read the cell through two
;;   char-at            more calls (the binding-stack gate, then the root) and
;;                      checked the arity with procedure-arity-mask, an
;;                      out-of-line library call, ahead of the call Chez checks
;;                      anyway: 16 ns where the procedure alone is 3. Against an
;;                      inline vector nth: 3.02 -> 1.82. A multimethod reached its
;;                      fixed-arity dispatch only after the variadic jolt-invoke
;;                      built a rest list and walked the prefix arms, then hashed
;;                      the dispatch value into the methods table; a keyword now
;;                      hits a per-multifn inline cache (172 -> 40 ns). Against
;;                      the plain fn call it wraps: 6.04 -> 3.81, and ~10 with only
;;                      the multimethod half reverted, since main's plain call was
;;                      the slow var call above. str of a StringBuilder walked
;;                      every registered str arm before its own (against its
;;                      .toString: 21.0 -> 10.1), and .charAt coerced its
;;                      index through an out-of-line call before the in-range read
;;                      (against nth: 1.36 -> 0.98). Measured x86_64, 2026-11-17.
;;
;;   wide-call          a call of more than four arguments had no fixed-arity
;;                      entry: jolt-invoke took a rest list, measured it, checked
;;                      the arity mask and applied it. Through a var it is
;;                      open-coded at any arity now (42 -> 6 ns for six args;
;;                      against a four-argument call ~7 -> ~1.05).
;;
;;   try-deep           a try's catch was R6RS guard: a full continuation, which
;;                      costs in proportion to the stack beneath it, and a body
;;                      thunk closing over every free variable of the body. The
;;                      body runs in place now behind a one-shot escape (rt.ss
;;                      jolt-guard-body): 2.59 -> 1.31 for twenty nested tries
;;                      under 200 frames against the bare recursion.
;;
;;   dyn-read-deep      a read of a dynamic var that has ever been bound walked
;;                      every binding frame on the thread to its binding, and a
;;                      reader binds one per nesting level (tools.reader's
;;                      *read-delim*) while asking *read-eval*, bound at the
;;                      bottom, for every form. The innermost frame memoizes its
;;                      last four lookups (dyn-binding.ss): 9.7 -> 1.0 beneath
;;                      thirty frames.
;;
;;   get-nil            (get nil k) -- and (:k nil), a keyword on a nil map --
;;                      walked every registered get arm before answering the
;;                      default: ~90 ns against the JVM's ~6 (rewrite-clj's
;;                      (:delimiter context) on a top-level form's nil context).
;;
;;   protocol-call      a protocol call went through the protocol fn's var, the
;;                      protocol fn and its per-site cache before the method, and
;;                      every deftype or defrecord method bound all its fields
;;                      first. The call site now reads the receiver type's
;;                      dispatch vector (rt.ss jrdesc-vtable-ref) and calls the
;;                      method, which binds only the fields it names: 17.5 ->
;;                      10.5 ns, against a plain call ~2.6 -> ~1.6.
;;
;;   host-protocol      a protocol call on a string, number, char, nil, boolean
;;                      or keyword resolved its impl on every call; the protocol
;;                      fn's cache keys those by type (rt.ss jolt-pic-host-key/i).
;;
;;   merge-nil          (merge nil m) was the variadic reduce (~450 ns); merge has
;;                      fixed arities and (conj {} m) answers m.
;;
;;   str-builder        (str sb) went through clojure.core/str's arms (~50 ns);
;;                      a StringBuilder answers inline (rt.ss jolt-str1/i).
;;
;;   deftype-method     (.m ^T x) on a deftype walked every dispatch arm by name
;;                      with an argument vector (~240 ns); the call site keeps the
;;                      type's declared method (host-static.ss jrec-site-miss):
;;                      ~17 ns, against the protocol call's ~10.
;;
;;   deftype-field      (.-f ^T x) was a call into the runtime and the checked
;;                      per-arity accessor (~15 ns); the site's hit is open-coded
;;                      (rt.ss jrec-field-site-ref/i): ~6 ns, a plain call's cost.
;;
;;   record-with-meta   a record's metadata lived in the weak identity side
;;                      table, so with-meta was a copy plus a weak-table insert
;;                      under a lock, ~440 ns against ~32 for a vector's; it is a
;;                      slot of the record now (values.ss jrec), ~66 ns.
;;
;;   record-meta        (meta r) of a record walked the collection kinds and two
;;                      calls into the type's derived vector before its slot
;;                      (~14 ns); records are tested first and the vector read
;;                      is open-coded (records-coll.ss jrec-iobj/i), ~5 ns.
;;
;;   mm-cycle           a multimethod's inline cache kept the last four dispatch
;;                      values, so one cycling through five or more (rewrite-clj's
;;                      parse-next*) missed every call and hashed into the methods
;;                      table; it keeps eight (multimethods.ss mm-resolve).
;;
(ns fastpath-ratio-test
  (:require [clojure.string :as str]))

(def ^:private samples 3)

;; FASTPATH_ONLY, a comma-separated list of labels, judges those rows and skips
;; the rest. The Makefile sets it to re-judge the rows a run failed in a fresh
;; process (see the fastpathratio target).
(def ^:private only
  (some-> (System/getenv "FASTPATH_ONLY") not-empty (str/split #",") set))
(defn- judged? [label] (or (nil? only) (contains? only label)))

;; A nanosecond clock, not currentTimeMillis: these ratios are judged to two
;; decimals and the fast arms run in single-digit milliseconds, which a millisecond
;; clock quantizes badly — and the fast arm is the denominator.
;;
;; This thread's CPU time, not the wall clock: under make -j beside other gates, a
;; preempted sample reads as a regression, which is what the alternating and the
;; re-measuring below were working around. The pipe rows bind the wall clock back,
;; since what they compare includes waiting on a child process.
(def ^:private thread-mx (java.lang.management.ManagementFactory/getThreadMXBean))
(defn- cpu-ns [] (.getCurrentThreadCpuTime thread-mx))
(defn- wall-ns [] (System/nanoTime))
(def ^:dynamic *clock* cpu-ns)
(defn- timed [f]
  (let [t (*clock*)
        v (f)]
    [(/ (- (*clock*) t) 1e6) v]))

;; Each sample starts from a fresh collection. An arm that allocates a few MB per
;; call (chars-copy builds a 1.5M-character string on both sides) trips a
;; collection every call or two, and which arm pays for it depends on where the
;; allocation counter happened to stand: the same binary read chars-copy at 0.33,
;; 0.34 and 1.40 in three runs, and 2.43 on a bionic runner against a 1.5
;; ceiling. After a collect the phase is the same for both arms, and twelve
;; ratios read 0.40-0.42.
;;
;; The two arms' samples alternate rather than run as two blocks. The gate runs
;; under make -j beside other gates, and a neighbour's burst lasting a few
;; hundred milliseconds landed on one arm's whole block: record-key-get read
;; 2.2-2.9 on main's CI runs and then 3.29 and 4.60 on one, 3.61 and 3.58 on
;; another, with nothing changed. Alternating puts a burst on both arms, so
;; the per-arm minimum still compares like with like.
(defn- best-of-pair [k fast slow]
  (let [ps (vec (repeatedly k #(vector (do (System/gc) (first (timed fast)))
                                       (do (System/gc) (first (timed slow))))))]
    [(reduce min (map first ps)) (reduce min (map second ps))]))

(def ^:private failures (atom []))

(defn- judge!
  "Measure SLOW-ARM against FAST-ARM and record a failure when the ratio exceeds
   CEILING. Over the ceiling it samples both arms again, up to twice, and judges
   the ratio of each arm's minimum over every sample taken."
  [label fast-arm slow-arm ceiling]
  (when (judged? label)
    (let [;; a floor on the denominator: an arm that measures as ~0 would make
          ;; any numerator look infinite
          ratio (fn [[f s]] (/ s (max f 0.05)))
          ;; Interference only ever adds time to a sample, so each arm's minimum
          ;; over more samples is a closer reading of that arm's cost, and the
          ;; ratio of the two minima is what a re-measure refines. The lowest
          ;; RATIO of separate readings would not be: a burst on the fast arm
          ;; lowers one, and under load that hid a reverted string-nth fast path
          ;; (1.33 against a 1.0 ceiling) in one run out of three. No reading is
          ;; too high to re-measure: one burst put host-object-pr at 6.0 against
          ;; its 1.9 ceiling, on a binary that reads 1.7.
          r (loop [[f s] (best-of-pair samples fast-arm slow-arm) left 2]
              (let [r (ratio [f s])]
                (if (and (pos? left) (> r ceiling))
                  (let [[f' s'] (best-of-pair samples fast-arm slow-arm)]
                    (println (format "  %s: %.2f — over the ceiling, sampling again" label r))
                    (recur [(min f f') (min s s')] (dec left)))
                  r)))]
      (println (format "  %-18s ratio %6.2f  (ceiling %.1f)%s"
                       label r ceiling (if (> r ceiling) "  <-- REGRESSED" "")))
      (when (> r ceiling)
        (swap! failures conj (format "%s: ratio %.2f exceeds ceiling %.1f" label r ceiling)))
      r)))

;; The heap bytes 16 results of MK hold, from a fresh collection. Exact on this
;; host (used memory is Chez's bytes-allocated), and 16 arrays of the size
;; char-alloc makes stay under the smallest nursery jolt runs (16MB), so no
;; collection lands between the two readings. Best of SAMPLES: anything else
;; allocating meanwhile only adds.
(defn- bytes-held [mk]
  (let [rt (Runtime/getRuntime)
        used #(- (.totalMemory rt) (.freeMemory rt))]
    (reduce min
            (repeatedly samples
                        #(do (System/gc)
                             (let [u0 (used)
                                   held (vec (repeatedly 16 mk))
                                   u1 (used)]
                               (when (empty? held) (throw (ex-info "unreachable" {})))
                               (- u1 u0)))))))

(defn- judge-bytes!
  "judge!, for a ratio of heap bytes held rather than of time: deterministic, so
   no re-measure."
  [label ref-mk mk ceiling]
  (when (judged? label)
    (let [r (/ (double (bytes-held mk)) (max 1 (bytes-held ref-mk)))]
      (println (format "  %-18s ratio %6.2f  (ceiling %.1f)%s"
                       label r ceiling (if (> r ceiling) "  <-- REGRESSED" "")))
      (when (> r ceiling)
        (swap! failures conj (format "%s: ratio %.2f exceeds ceiling %.1f" label r ceiling)))
      r)))

;; Some fast paths are not worth a timing row: the regression they guard costs
;; less than the spread one row shows across CI runners (protocol-call reads
;; 1.4-2.4 on main and 1.95 with its fast path gone), or none that a ratio can
;; see. Those are checked for the PATH instead, by asking the runtime
;; (jolt.host/scheme-eval-string) what the call does: which arms it walks, which
;; out-of-line helper it reaches, what it allocates. These answers do not move
;; with the machine, so they need no ceiling slack and no re-measure.
(def ^:private scheme (fn [s] (jolt.host/scheme-eval-string s)))

(defn- check!
  "Record a failure under LABEL unless OK?. DETAIL says what was seen."
  [label ok? detail]
  (when (judged? label)
    (println (format "  %-18s %s%s" label detail (if ok? "" "  <-- REGRESSED")))
    (when-not ok?
      (swap! failures conj (format "%s: %s" label detail)))))

;; Counts the calls that reach a runtime procedure while F runs, by set!-ing a
;; counting wrapper over the top-level binding and restoring it after.
(defn- calls-to [proc-name f]
  (let [hook (scheme (str "(lambda (f) (let ((n 0) (orig " proc-name "))"
                          " (set! " proc-name " (lambda args (set! n (+ n 1)) (apply orig args)))"
                          " (dynamic-wind void f (lambda () (set! " proc-name " orig))) n))"))]
    (hook f)))

;; Counts the registered get arms asked while F runs: a counting arm that claims
;; nothing goes in front of the walk for F's duration.
(defn- get-arms-asked [f]
  ((scheme (str "(lambda (f) (let ((n 0) (orig jolt-get-arms))"
                " (set! jolt-get-arms (cons (cons (lambda (c) (set! n (+ n 1)) #f) (lambda (c k d) d)) orig))"
                " (dynamic-wind void f (lambda () (set! jolt-get-arms orig))) n))"))
   f))

;; Bytes allocated by F, as Chez counts them (allocated plus reclaimed, so a
;; collection in the middle does not subtract).
(def ^:private alloc-bytes (scheme "(lambda () (+ (bytes-allocated) (bytes-deallocated)))"))
(defn- bytes-of [f]
  (let [b0 (alloc-bytes)] (f) (- (alloc-bytes) b0)))

;; The scheme text of the code a form compiles to in this namespace.
(defn- emitted [form]
  (jolt.backend-scheme/emit-top-form
   (jolt.analyzer/analyze (jolt.host/ctx-for-ns "fastpath-ratio-test") form
                          (@#'jolt.analyzer/empty-env))))

;; --- fixtures ---------------------------------------------------------------

(def ^:private one-form
  "(defn some-function-name\n  \"A docstring long enough that walking it one character at a time shows up.\"\n  [a b c]\n  (let [x (+ a b)] {:sum x :label \"result\"}))\n")

(def ^:private source-text
  (str "(ns bench.sample\n  \"Namespace docstring.\"\n  (:require [clojure.string :as str]))\n"
       (apply str (repeat 60 one-form))))

(def ^:private line
  (str "{\"role\":\"assistant\",\"content\":\"" (apply str (repeat 40 "abcdefghij")) "\"}"))

;; ~1.7 MB / 4000 lines: big enough that a per-character path cannot hide in
;; noise, small enough that a regressed run still finishes and fails fast.
(def ^:private payload (str/join "\n" (repeat 4000 line)))
(def ^:private crlf-payload (str/join "\r\n" (repeat 4000 line)))

(def ^:private tmpdir (System/getProperty "java.io.tmpdir"))
(def ^:private src-path (str tmpdir "/jolt-fastpath-src.clj"))
(def ^:private data-path (str tmpdir "/jolt-fastpath-data.txt"))

(defn- read-form-from-stream []
  (with-open [r (java.io.PushbackReader.
                 (java.io.InputStreamReader.
                  (java.io.FileInputStream. src-path)))]
    (first (read r))))

(defn- read-form-from-string []
  (first (read-string (slurp src-path))))

(defn- drain-chunked []
  (with-open [r (java.io.InputStreamReader. (java.io.FileInputStream. data-path) "UTF-8")]
    (let [cbuf (char-array 65536)]
      (loop [total 0]
        (let [n (.read r cbuf)]
          (if (neg? n) total (recur (+ total n))))))))

(defn- slurp-whole [] (count (slurp data-path)))

;; N small files whose bytes sum to one big file, for the per-file overhead arm
(def ^:private small-count 48)
(def ^:private small-paths
  (mapv (fn [i] (str tmpdir "/jolt-fastpath-small-" i ".txt")) (range small-count)))
(def ^:private small-text (apply str (repeat 6 one-form)))

;; Through an InputStreamReader, NOT (slurp path): slurping a PATH takes the
;; bytevector route and never reaches the char-reader drain this arm exists to
;; guard. Reading a reader object is what io/reader, line-seq and any code
;; holding a Reader actually do.
(defn- drain-reader-at [p]
  (with-open [r (java.io.InputStreamReader. (java.io.FileInputStream. p) "UTF-8")]
    (count (slurp r))))
(defn- drain-many-small []
  (reduce (fn [a p] (+ a (drain-reader-at p))) 0 small-paths))
;; the reference: the SAME files, the SAME number of opens, the same bytes —
;; read by path, which takes the bytevector route and never touches the drain.
;; Holding everything but the drain constant is what makes the ratio sharp:
;; a per-file allocation in the drain shows up here and nowhere else.
(defn- slurp-many-small-by-path []
  (reduce (fn [a p] (+ a (count (slurp p)))) 0 small-paths))

;; Through a child process: the pipe arms drain `cat FILE` from its stdout, and
;; feed the same bytes to `cat > /dev/null` on its stdin. Each reference spawns
;; the same program with the pipe out of the transfer (stdout discarded, stdin
;; from the file) and moves the bytes through a FileInputStream/FileOutputStream,
;; so spawning and waiting cancel and what is left is the pipe copy.
(def ^:private pipe-path (str tmpdir "/jolt-fastpath-pipe.bin"))
(def ^:private pipe-out-path (str tmpdir "/jolt-fastpath-pipe-out.bin"))
(def ^:private pipe-bytes (* 8 1024 1024))
(def ^:private pipe-payload (byte-array pipe-bytes (byte 65)))

(defn- drain-stream [^java.io.InputStream in]
  (let [buf (byte-array 65536)]
    (loop [total 0]
      (let [n (.read in buf)]
        (if (neg? n) total (recur (+ total n)))))))

(defn- spawn ^Process [cmd redirect-out?]
  (let [pb (ProcessBuilder. ^java.util.List cmd)]
    (when redirect-out? (.redirectOutput pb (java.io.File. "/dev/null")))
    (.start pb)))

(defn- pipe-read-arm []
  (let [p (spawn ["cat" pipe-path] false)
        n (with-open [in (.getInputStream p)] (drain-stream in))]
    (.waitFor p)
    n))
(defn- pipe-read-ref []
  (let [p (spawn ["cat" pipe-path] true)
        n (with-open [in (java.io.FileInputStream. pipe-path)] (drain-stream in))]
    (.waitFor p)
    n))
(defn- pipe-write-arm []
  (let [p (spawn ["cat"] true)]
    (with-open [out (.getOutputStream p)] (.write out ^bytes pipe-payload))
    (.waitFor p)))
(defn- pipe-write-ref []
  (let [p (spawn ["cat" pipe-path] true)]
    (with-open [out (java.io.FileOutputStream. pipe-out-path)] (.write out ^bytes pipe-payload))
    (.waitFor p)))

;; a clean string at two lengths, for the trim scaling arm
(def ^:private clean-short (apply str (repeat 64 "x")))
(def ^:private clean-long (apply str (repeat (* 64 64) "x")))

;; ~1.5 MB of text for the char-array and scan arms. Large enough that a
;; per-character path cannot hide in the noise of one allocation.
(def ^:private chars-text (apply str (repeat 40000 "abcdefghijklmnopqrstuvwxyz0123456789abc")))
(def ^:private chars-len (count chars-text))
(def ^:private chars-array (.toCharArray chars-text))
;; a needle that is NOT present, so both scan arms run to the end of the string
(def ^:private absent-str "QZXW")
(def ^:private absent-char \Q)

;; dispatch fixtures: a reify and a deftype declaring several protocols, and
;; deftype map keys with and without their own equals/hashCode
(defprotocol DP1 (dp1 [x])) (defprotocol DP2 (dp2 [x])) (defprotocol DP3 (dp3 [x]))
(defprotocol DP4 (dp4 [x])) (defprotocol DP5 (dp5 [x])) (defprotocol DP6 (dp6 [x]))
(def ^:private dispatch-reify
  (reify DP1 (dp1 [_] 1) DP2 (dp2 [_] 2) DP3 (dp3 [_] 3)
         DP4 (dp4 [_] 4) DP5 (dp5 [_] 5) DP6 (dp6 [_] 6)))
(deftype DispatchT [a] DP1 (dp1 [_] a) DP2 (dp2 [_] a))
(def ^:private dispatch-t (DispatchT. 1))
(def ^:private dispatch-n 100000)
(defprotocol DP7 (dp7 [x] [x y]))
(defn- mk-reify [a] (reify DP1 (dp1 [_] a) DP2 (dp2 [_] a) DP7 (dp7 [_] a) (dp7 [_ y] y)))
(defn- mk-type [a] (DispatchT. a))
(def ^:private made-reify (mk-reify 1))
(deftype EqKey [id]
  Object
  (equals [_ o] (and (instance? EqKey o) (= id (.-id ^EqKey o))))
  (hashCode [_] (hash id)))
(defrecord RecKey [id])
(def ^:private eq-key-map (zipmap (map #(EqKey. %) (range 8)) (range)))
(def ^:private eq-probe (EqKey. 7))
(def ^:private rec-key-map (zipmap (map ->RecKey (range 8)) (range)))
(def ^:private rec-probe (->RecKey 7))
;; two equal records, and two equal vectors to measure them against
(defrecord EqRec [a])
(def ^:private eq-rec-a (->EqRec 7))
(def ^:private eq-rec-b (->EqRec 7))
(def ^:private eq-vec-a [7])
(def ^:private eq-vec-b [7])
;; a plain record and the map holding the same entries, for the record-* arms
(defrecord PlainRec [x y z])
(def ^:private plain-rec (->PlainRec 1 2 3))
(def ^:private plain-map {:x 1 :y 2 :z 3})
(def ^:private kv-sum (fn [a k v] (+ a v)))
(def ^:private plain-map+ {:x 1 :y 2 :z 3 :w 4})
;; one body, built under a def and in a bare top-level form
(def ^:private kw-map {:a 1 :b 2 :c 3})
(def ^:private fn-via-def (fn [m] (+ (get m :a) (get m :b) (get m :c))))
(def ^:private fn-holder (atom nil))
(reset! fn-holder (fn [m] (+ (get m :a) (get m :b) (get m :c))))
(def ^:private fn-via-top @fn-holder)
;; the reader-path arms (#1713): a var-routed call, a keyword multimethod, a
;; token buffer turned into its string, and a scanner's .charAt
(def ^:private gate-vec [1 2 3 4 5 6 7 8])
(defn- gate-f2 [a b] (if a b a))
(defn- gate-f4 [a b c d] a)
(defn- gate-f6 [a b c d e f] a)
(defn- gate-rec [n] (if (zero? n) 0 (inc (gate-rec (dec n)))))
(defprotocol GateP (gate-pm [x]))
(deftype GateT [a] GateP (gate-pm [_] nil))
(def ^:private gate-t (GateT. 1))
(defn- gate-fn1 [x] nil)
;; a protocol extended to a host type, and the reader-path collection fixtures
(defprotocol GateH (gate-hm [x]))
(extend-protocol GateH String (gate-hm [_] nil) GateT (gate-hm [_] nil))
(def ^:private gate-str "abc")
(def ^:private gate-m4 {:line 1 :column 2 :end-line 1 :end-column 5})
(def ^:private gate-sb (doto (StringBuilder.) (.append "token")))
(defn- gate-rec-try [n] (try (if (zero? n) 0 (inc (gate-rec-try (dec n)))) (catch Exception e -1)))
(defn- gate-under [n f] (if (zero? n) (f) (+ 0 (gate-under (dec n) f))))
(def ^:dynamic *gate-deep* 0)
(def ^:dynamic *gate-top* 0)
(defn- gate-nest [n f] (if (zero? n) (f) (binding [*gate-top* n] (gate-nest (dec n) f))))
(defn- gate-plain [x] x)
(defmulti ^:private gate-mm identity)
(defmethod gate-mm :a [x] x)
(def ^:private gate-sb (doto (StringBuilder.) (.append "token")))
(def ^:private gate-str "abcdefgh")
(def ^:private gate-n 1000000)
(defrecord GateMetaRec [a b])
(def ^:private gate-meta-rec (->GateMetaRec 1 2))
(def ^:private gate-meta-vec [1 2])
(def ^:private gate-meta {:row 1 :col 2})
(defmulti ^:private gate-mm identity)
(defmethod gate-mm :a [_] 1) (defmethod gate-mm :b [_] 2) (defmethod gate-mm :c [_] 3)
(defmethod gate-mm :d [_] 4) (defmethod gate-mm :e [_] 5) (defmethod gate-mm :f [_] 6)
(def ^:private gate-mm-one [:a :a :a :a :a :a])
(def ^:private gate-mm-six [:a :b :c :d :e :f])

(defn -main [& _]
  (spit src-path source-text)
  (spit data-path payload)
  (doseq [p small-paths] (spit p small-text))
  (with-open [o (java.io.FileOutputStream. pipe-path)] (.write o ^bytes pipe-payload))
  (println "fastpath ratio gate")

  ;; Reading a form off a stream must cost about what reading it off a string
  ;; costs. Was ~70x when the drain went per-character through method dispatch.
  (judge! "reader-vs-string" read-form-from-string read-form-from-stream 8.0)

  ;; The memory-bounded read must not be slower than reading the whole file.
  ;; Was 2.7x when the fill loop went through the checked generic array setter.
  (judge! "chunked-vs-slurp" slurp-whole drain-chunked 2.0)

  ;; A literal pattern must not reach the regex engine. The reference arm is the
  ;; same split with a literal STRING separator, which never did.
  (judge! "literal-split"
          #(count (str/split payload "\n"))
          #(count (str/split payload #"\n"))
          3.0)

  ;; split-lines is #"\r?\n" — not a literal, so it needs its own recognition.
  (judge! "split-lines"
          #(count (str/split crlf-payload "\r\n"))
          #(count (str/split-lines crlf-payload))
          3.0)

  ;; A literal replace pattern with a literal replacement, against the same
  ;; function's literal-string arm.
  (judge! "literal-replace"
          #(count (str/replace payload "abc" "xyz"))
          #(count (str/replace payload #"abc" "xyz"))
          3.0)

  ;; Reading N small files must not cost wildly more than reading their bytes
  ;; from one file. Guards per-file overhead — a scratch sized to the block
  ;; rather than the input, an eager buffer, a per-open allocation.
  (judge! "small-file-drain" slurp-many-small-by-path drain-many-small 4.0)

  ;; Moving bytes through a child's pipe must cost about what moving them through
  ;; a file costs, with the same child spawned on both sides.
  (when-not (or (not (judged? "pipe-read")) (= pipe-bytes (pipe-read-arm)))
    (swap! failures conj "pipe-read: drained the wrong number of bytes"))
  (binding [*clock* wall-ns]
    (judge! "pipe-read" pipe-read-ref pipe-read-arm 3.0)
    (judge! "pipe-write" pipe-write-ref pipe-write-arm 3.0))

  ;; A char array is backed by a string, so building one from a string and
  ;; reading one back out are block moves — each must cost about what copying
  ;; the same characters with subs costs.
  (judge! "chars-copy"
          #(count (subs chars-text 0 chars-len))
          #(alength (.toCharArray chars-text))
          1.5)
  (judge! "string-from-chars"
          #(count (subs chars-text 0 chars-len))
          #(count (String. chars-array))
          1.35)

  ;; ...and a char array must take half the heap of a long array of the same
  ;; length, because its backing is half as wide and is never traced.
  (judge-bytes! "char-alloc" #(long-array 65536) #(char-array 65536) 0.8)

  ;; Searching for a multi-character needle must cost about what searching for a
  ;; single CHARACTER costs — the reference arm takes str-char-index, which never
  ;; had a call per position, so it does not move when str-index-of regresses.
  ;; x86 CI read up to 1.46 (2026-11), so 1.8; the per-position call read ~3x
  ;; the healthy ratio (0.80 -> 2.43).
  (judge! "substring-scan"
          #(str/index-of chars-text absent-char)
          #(str/index-of chars-text absent-str)
          1.8)

  ;; Trimming a string that needs no trimming must not copy it: 64x the length
  ;; must not cost 64x the time. A copying trim is linear here.
  (judge! "trim-no-copy"
          #(dotimes [_ 200] (str/trim clean-short))
          #(dotimes [_ 200] (str/trim clean-long))
          4.0)

  ;; instance? on a reify or deftype that declares the protocol must cost about
  ;; what calling one of its methods costs.
  (judge! "reify-instance"
          #(dotimes [_ dispatch-n] (dp6 dispatch-reify))
          #(dotimes [_ dispatch-n] (instance? fastpath_ratio_test.DP6 dispatch-reify))
          4.0)
  (judge! "deftype-instance"
          #(dotimes [_ dispatch-n] (dp1 dispatch-t))
          #(dotimes [_ dispatch-n] (instance? fastpath_ratio_test.DP2 dispatch-t))
          3.0)

  ;; ...and a miss, on a record and on a number, must too.
  (judge! "instance-miss"
          #(dotimes [_ dispatch-n] (dp1 dispatch-t))
          #(dotimes [_ dispatch-n] (instance? fastpath_ratio_test.DP7 dispatch-t))
          4.0)
  (judge! "number-miss"
          #(dotimes [_ dispatch-n] (dp1 dispatch-t))
          #(dotimes [_ dispatch-n] (instance? fastpath_ratio_test.DP7 42))
          4.0)

  ;; The site cache: about what the protocol call it is measured against
  ;; costs. A protocol fn carries its own inline cache since #1652/#1646, and
  ;; the call got about twice as fast again in the merges from #1690 to #1702
  ;; (32 ms per million on x86_64 Linux, where it was 68; instance? stayed at
  ;; 23). So both arms read near what a site costs: instance-site 0.75 locally
  ;; and 0.85 on CI, and instance-site-type 0.90 on CI and 1.01 on a slow box.
  ;; #1713 then made the protocol call itself cheaper again (it is a call
  ;; through a var: 16 -> 9.5 ns), and then open-coded that var call at the
  ;; site and the protocol fn's descriptor read with it (jolt-var-fn,
  ;; jrec-pic-desc/i), with the site unchanged: on the slow box the two arms
  ;; read 0.73 -> 1.11 and 1.00 -> 1.35 over both steps. The protocol call then
  ;; read the var cell and the receiver's descriptor without their type tests,
  ;; the instance? site unchanged: 1.35 and 1.56.
  ;;
  ;; What these two read depends on the CPU and on whether the machine is idle,
  ;; not on the sample. Over 22 CI runs of the x86 shard (2026-11-21..22)
  ;; instance-site-type read 1.53-1.75 in most and 1.88-2.10 in a few, and in
  ;; those the column moved with it: multimethod-call 6.7-7.0 (4.1-5.1
  ;; elsewhere), try-deep 1.64 (1.38-1.46), deftype-method 1.96 (1.47-1.69),
  ;; var-call 0.72 (0.90-1.17). Such a run read the same on all three of its
  ;; samplings and again in a fresh process (2.09, then 2.07). Eight runners
  ;; measured on purpose (2026-11-22): the AMD EPYCs (7763, 9V45) read 1.43-1.87
  ;; idle or loaded; the Intel Xeons (6973P, 6975P, Platinum 8573C) read
  ;; 1.54-1.89 beside four busy processes and 2.01-2.14 on an idle machine,
  ;; which is how this gate runs (QUIET-GATES). No sampling scheme takes that
  ;; out, so the ceilings cover it: instance-site read at most 1.72 there and
  ;; 1.56 on macOS arm64. The site with its cache bypassed (every call taking
  ;; the memo path, the state before the cache) reads 8.5 and 7.0 on macOS arm64
  ;; against 1.45 and 1.68 with it, so 2.2 and 3.0 fail it by 2.3-3.9x.
  ;; (Ceilings were 1.6 and 1.9.)
  (judge! "instance-site"
          #(dotimes [_ dispatch-n] (dp1 dispatch-t))
          #(dotimes [_ dispatch-n] (instance? fastpath_ratio_test.DP7 dispatch-t))
          2.2)
  (judge! "instance-site-type"
          #(dotimes [_ dispatch-n] (dp1 dispatch-t))
          #(dotimes [_ dispatch-n] (instance? DispatchT dispatch-t))
          3.0)

  ;; Building a reify must cost about what building a deftype costs. The
  ;; deftype's (T. a) then kept its resolved ctor at the call site (host-static.ss
  ;; host-new-site/i), ~2.3x cheaper, with the reify unchanged: 1.9 -> 4.4,
  ;; ceiling 3.0 -> 5.5. Over 46 x86 CI runs and 4 noise runners (2026-11-23)
  ;; it read up to 5.99, so 7.5; the per-instance map this gates read 8.9 against
  ;; the old, slower deftype, ~20 against today's.
  (judge! "reify-construct"
          #(dotimes [_ dispatch-n] (mk-type 1))
          #(dotimes [_ dispatch-n] (mk-reify 1))
          7.5)

  ;; satisfies? on a reify must cost about a protocol call. The protocol call
  ;; got cheaper under #1713 (a var call open-coded, the PIC's descriptor read
  ;; inline) with satisfies? unchanged: 2.23 on the slow box before, 2.9-3.06
  ;; after (and 1.24 on some runs), so 3.0 -> 4.5, against the 13.5 the
  ;; re-munging walk this gates read. Bionic runners read up to 4.04 and x86
  ;; 3.89 (2026-11), so 5.0.
  (judge! "satisfies-reify"
          #(dotimes [_ dispatch-n] (dp1 dispatch-t))
          #(dotimes [_ dispatch-n] (satisfies? DP2 made-reify))
          5.0)

  ;; A key with its own equals/hashCode must not cost wildly more to look up
  ;; than a record key.
  (judge! "record-key-get"
          #(dotimes [_ 20000] (get rec-key-map rec-probe))
          #(dotimes [_ 20000] (get eq-key-map eq-probe))
          3.8)

  ;; = on two records must cost about what = on two small vectors costs.
  (judge! "record-eq"
          #(dotimes [_ dispatch-n] (= eq-vec-a eq-vec-b))
          #(dotimes [_ dispatch-n] (= eq-rec-a eq-rec-b))
          2.5)

  ;; The core collection ops on a plain record cost about what they cost on the
  ;; map holding the same entries. record-count read up to 5.34 on x86 CI
  ;; (2026-11, the Xeon 6975P-C most), over its old 5.0 on ten first passes, so
  ;; 6.5; with the plain-record path gone it reads 11-20 there.
  (judge! "record-count"
          #(dotimes [_ dispatch-n] (count plain-map))
          #(dotimes [_ dispatch-n] (count plain-rec))
          6.5)
  (judge! "record-contains"
          #(dotimes [_ dispatch-n] (contains? plain-map :z))
          #(dotimes [_ dispatch-n] (contains? plain-rec :z))
          3.5)
  (judge! "record-reduce-kv"
          #(dotimes [_ dispatch-n] (reduce-kv kv-sum 0 plain-map))
          #(dotimes [_ dispatch-n] (reduce-kv kv-sum 0 plain-rec))
          3.5)
  (judge! "record-into"
          #(dotimes [_ 20000] (into {} plain-map))
          #(dotimes [_ 20000] (into {} plain-rec))
          2.5)
  (judge! "record-seq"
          #(dotimes [_ 20000] (seq plain-map))
          #(dotimes [_ 20000] (seq plain-rec))
          2.3)
  (judge! "record-map->"
          #(dotimes [_ 20000] (assoc (->PlainRec 1 2 3) :w 4))
          #(dotimes [_ 20000] (map->PlainRec plain-map+))
          ;; x86 CI read up to 2.63 (2026-11); the pre-check this gates read
          ;; ~2.2x the healthy ratio (4.1 against 1.85)
          3.2)

  ;; The same fn body costs the same whether a def or a bare form built it.
  (judge! "top-level-fn"
          #(dotimes [_ dispatch-n] (fn-via-def kw-map))
          #(dotimes [_ dispatch-n] (fn-via-top kw-map))
          1.5)

  ;; get on a java.util.HashMap goes through the get arms; every shim that
  ;; registers one ahead of the HashMap family's adds its predicate to this walk.
  (let [pm {1 2 3 4} hm (java.util.HashMap. {1 2 3 4})]
    (judge! "hashmap-get"
            #(dotimes [_ 200000] (get pm 3))
            #(dotimes [_ 200000] (get hm 3))
            7.0))

  ;; nth and get on a string against the same on a vector. A string is a
  ;; core receiver, but nth reached it through every set! wrap around jolt-nth
  ;; and get through every registered get arm's predicate, so each host family
  ;; added since made both slower: 1.34 / 2.7 by 0.8.24 against 1.1 / 1.8 at
  ;; 0.8.23. Both now answer a string before the wraps and the arms (0.56 /
  ;; 0.66, macOS arm64, 2026-11).
  ;;
  ;;
  ;; string-get read 0.72-0.95 over 46 x86 CI runs against a 1.1 ceiling, so
  ;; 1.3; with the string answered after the arms it reads ~9.4 (macOS arm64).
  ;; What it guards is that walk: get answers a string in natives-array.ss's
  ;; jolt-get wrapper and again in jolt-get-dispatch, both ahead of the arms,
  ;; and register-get-arm! refuses an arm that claims a string. string-get-arms
  ;; checks that directly: a get on a string asks no registered arm, where a
  ;; StringBuilder (which does walk them) shows the counting arm is in the walk.
  (let [s (apply str (repeat 40 "hello world ")) v (vec s)]
    (judge! "string-nth"
            #(dotimes [_ 200000] (nth v 300))
            #(dotimes [_ 200000] (nth s 300))
            1.0)
    (judge! "string-get"
            #(dotimes [_ 200000] (get v 300))
            #(dotimes [_ 200000] (get s 300))
            1.3)
    (let [on-str (get-arms-asked #(dotimes [_ 10] (get s 300)))
          on-sb (get-arms-asked #(dotimes [_ 10] (get gate-sb 0)))]
      (check! "string-get-arms" (and (zero? on-str) (pos? on-sb))
              (format "get arms asked: %d for 10 string gets (want 0), %d for 10 StringBuilder gets (want >0)"
                      on-str on-sb))))

  ;; count / contains? on the common java.util collections, each against the
  ;; collection's own method doing the same work (.size, .containsKey),
  ;; so the ratio is the arm walk in front of the handler. Every family whose arm
  ;; is walked ahead of these adds its predicate to the ratio.
  (let [al (java.util.ArrayList. (range 64))
        hs (java.util.HashSet. (range 64))
        hm (java.util.HashMap. (zipmap (range 64) (range 64)))]
    ;; Ceilings at 1.2x the highest x86 CI reading (2026-11: 3.52, 2.87, 3.83).
    ;; With the tag predicates unmemoized (define-jhost-tag-pred, the 0.8.24
    ;; slowdown) a Xeon 6975P-C read arraylist-count 4.7-5.5 and hashmap-count
    ;; 5.4-5.7, over these; hashset-count read 2.7-3.0 and contains? on a
    ;; HashMap 1.7, both inside main's own spread, so hashmap-contains is no
    ;; longer a row and the memo is checked directly (tag-pred-memo below).
    (judge! "arraylist-count"
            #(dotimes [_ 200000] (.size al))
            #(dotimes [_ 200000] (count al))
            4.3)
    (judge! "hashset-count"
            #(dotimes [_ 200000] (.size hs))
            #(dotimes [_ 200000] (count hs))
            3.5)
    (judge! "hashmap-count"
            #(dotimes [_ 200000] (.size hm))
            #(dotimes [_ 200000] (count hm))
            4.6)
    ;; The memo answers by the tag OBJECT, so a host object whose tag string is
    ;; changed after a first question keeps its first answer; an unmemoized
    ;; predicate re-reads the string and changes its answer.
    (let [n ((scheme (str "(lambda () (length (filter values (map (lambda (p tag)"
                          " (let* ((t (string-copy tag)) (h (make-jhost t #f)) (a (and (p h) #t)))"
                          " (string-set! t 0 #\\Q) (and a (p h) #t)))"
                          " (list hs-hashset? hm-hashmap? al-family?) (list \"hashset\" \"hashmap\" \"arraylist\")))))")))]
      (check! "tag-pred-memo" (= 3 n)
              (format "%d of the HashSet, HashMap, ArrayList tag predicates remembered by tag (want 3)" n)))
    ;; A Path member is a host method like an ArrayList's, served by the call
    ;; site's cache: getFileSystem reads a field, as size does.
    (let [p (java.nio.file.Paths/get "/tmp/work" (into-array String []))]
      (judge! "path-method"
              #(dotimes [_ 200000] (.size al))
              #(dotimes [_ 200000] (.getFileSystem p))
              2.5)))

  ;; The reader path's per-operation costs (#1713). A call through a var must
  ;; cost a small multiple of an inline vector read, a keyword multimethod a
  ;; small multiple of the plain fn call it wraps, str of a StringBuilder about
  ;; what its .toString costs, and .charAt about an inline vector read.
  (judge! "var-call"
          #(let [v gate-vec] (dotimes [i gate-n] (nth v 3)))
          #(let [v gate-vec] (dotimes [i gate-n] (gate-f2 i v)))
          2.4)
  ;; (multimethod-call's reference is the var call, open-coded since: 3.81 ->
  ;; 5.26 with the multimethod unchanged, and ~14 with its fix reverted. The
  ;; reference got cheaper again since: with the inline cache bypassed, every
  ;; call hashing into the methods table, it reads 37 against 5.1, macOS arm64
  ;; 2026-11-22. On CI it reads 3.4-5.5 in most runs and 6.4-7.6 on the Intel
  ;; Xeons when idle (see instance-site-type), which put it over the old 7.0
  ;; with nothing changed; 10 still fails the bypass by 3.5x.)
  (judge! "multimethod-call"
          #(dotimes [_ gate-n] (gate-plain :a))
          #(dotimes [_ gate-n] (gate-mm :a))
          10.0)
  (judge! "str-sb"
          #(let [b gate-sb] (dotimes [_ gate-n] (.toString ^StringBuilder b)))
          #(let [b gate-sb] (dotimes [_ gate-n] (str b)))
          14.0)
  (judge! "char-at"
          #(let [v gate-vec] (dotimes [i gate-n] (nth v 3)))
          #(let [^String x gate-str] (dotimes [i gate-n] (.charAt x 3)))
          ;; a coarse ceiling now: x86 CI read up to 1.16 (2026-11) and the
          ;; out-of-line index coercion this was tuned for reads 1.33 there, too
          ;; close to tell apart. char-at-inline below checks that path.
          1.4)
  ;; ...and by the path: an in-range fixnum index never reaches the coercing
  ;; helper (converters.ss jolt-char-at-coerced), where a double index does.
  (let [in-range (calls-to "jolt-char-at-coerced" #(let [^String x gate-str] (dotimes [i 10] (.charAt x 3))))
        coerced (calls-to "jolt-char-at-coerced" #(let [^String x gate-str] (.charAt x 3.0)))]
    (check! "char-at-inline" (and (zero? in-range) (pos? coerced))
            (format "jolt-char-at-coerced calls: %d for 10 in-range reads (want 0), %d for a double index (want >0)"
                    in-range coerced)))
  ;; A record's metadata is a slot of its own, like a vector's: with-meta on a
  ;; record costs about what it costs on a vector. It was a weak identity-table
  ;; insert under a lock, ~14x the vector's (rewrite-clj attaches position meta
  ;; to every node it reads).
  ;; A six-argument call costs about what a four-argument one does. Past four
  ;; there was no fixed-arity entry, and jolt-invoke built a rest list,
  ;; measured it and applied it: 42 ns against 6 (tools.reader's read* takes
  ;; six arguments per form it reads).
  (judge! "wide-call"
          #(dotimes [i gate-n] (gate-f4 i 2 3 4))
          #(dotimes [i gate-n] (gate-f6 i 2 3 4 5 6))
          2.0)
  ;; A try costs a small constant, wherever it runs. guard took a full
  ;; continuation and with-exception-handler a body thunk per entry, so a try
  ;; under a deep stack paid for the stack, and one in a big fn for its free
  ;; variables. Twenty nested tries under 200 frames, against the same
  ;; recursion without them.
  (judge! "try-deep"
          #(dotimes [_ 20000] (gate-under 200 (fn [] (gate-rec 20))))
          #(dotimes [_ 20000] (gate-under 200 (fn [] (gate-rec-try 20))))
          ;; x86 CI read up to 2.18 (2026-11), so 2.6; guard reads ~4.7 there
          2.6)
  ;; ...and by what a try allocates under that stack: ~220 bytes each, where the
  ;; R6RS guard's continuation took ~4500 (macOS arm64, 2026-11). Read from a
  ;; fresh collection, smallest of three: without the collect the same binary
  ;; read ~970 for a stretch after the earlier rows (stack segments they left).
  (let [per-try (reduce min (repeatedly 3 #(do (System/gc)
                                               (/ (bytes-of (fn [] (dotimes [_ 1000] (gate-under 200 (fn [] (gate-rec-try 20))))))
                                                  20000.0))))]
    (check! "try-deep-alloc" (< per-try 2000)
            (format "%.0f bytes allocated per try under 200 frames (ceiling 2000)" per-try)))
  ;; A dynamic var bound beneath thirty frames of another reads about as fast as
  ;; one bound in the top frame. Every read walked the frames to its binding.
  (judge! "dyn-read-deep"
          #(binding [*gate-deep* 1] (binding [*gate-top* 0] (dotimes [_ 200000] *gate-top*)))
          #(binding [*gate-deep* 1] (gate-nest 30 (fn [] (dotimes [_ 200000] *gate-deep*))))
          2.5)
  ;; get on nil costs about what get on an empty map does. It walked every
  ;; registered get arm before answering the default. x86 CI read up to 1.79
  ;; (2026-11), so 2.2; the walk reads ~9. get-nil-arms checks the walk itself.
  (judge! "get-nil"
          #(let [m {}] (dotimes [_ gate-n] (get m :k)))
          #(let [m nil] (dotimes [_ gate-n] (get m :k)))
          2.2)
  (let [on-nil (get-arms-asked #(let [m nil] (dotimes [_ 10] (get m :k))))
        on-sb (get-arms-asked #(dotimes [_ 10] (get gate-sb 0)))]
    (check! "get-nil-arms" (and (zero? on-nil) (pos? on-sb))
            (format "get arms asked: %d for 10 gets on nil (want 0), %d for 10 StringBuilder gets (want >0)"
                    on-nil on-sb)))
  ;; A protocol call on a deftype's own impl costs a little more than a plain fn
  ;; call. It went var -> protocol fn -> per-site cache -> method, and the
  ;; method bound every field before its body ran. 1.65 then; the plain call
  ;; then lost its var cell's type test (6.2 -> 4.9 ns) and the protocol call
  ;; less of its cost, so 1.9, ceiling 2.4.
  ;;
  ;; Checked by the path now, not timed: over 46 x86 CI runs it read 1.42-2.39
  ;; against that 2.4, and with the descriptor read gone it reads 1.95 (macOS
  ;; arm64), inside main's own spread. So: the call site the compiler emits for
  ;; a protocol call reads the receiver type's dispatch vector, and that read
  ;; answers this deftype's method.
  (let [site (emitted '(fn [t] (gate-pm t)))
        at-site? (boolean (re-find #"jrdesc-vtable-ref" site))
        hit? ((scheme "(lambda (r) (procedure? (jrdesc-vtable-ref (jrec-pic-desc r) (intern-pm-key \"fastpath-ratio-test/GateP\" \"gate-pm\"))))")
              gate-t)]
    (check! "protocol-call" (and at-site? hit?)
            (format "site reads the dispatch vector: %s, the read answers GateT's gate-pm: %s" at-site? hit?)))
  ;; A deftype's method called through interop, (.gate-pm ^GateT t), costs about
  ;; what the protocol call does: the site keeps the type's declared method
  ;; (host-static.ss jrec-site-miss). It walked every dispatch arm by name, with
  ;; an argument vector, on every call: ~240 ns against the protocol call's 10.
  ;; Reads ~1.7.
  (judge! "deftype-method"
          #(let [t gate-t] (dotimes [_ gate-n] (gate-pm t)))
          #(let [^GateT t gate-t] (dotimes [_ gate-n] (.gate-pm t)))
          3.0)
  ;; (.-a t) costs about what a plain fn call does: the site's hit is open-coded
  ;; (rt.ss jrec-field-site-ref/i), a descriptor test and a load. It was a call
  ;; into the runtime and the checked per-arity accessor, ~15 ns. Reads ~1.2.
  (judge! "deftype-field"
          #(let [t gate-t] (dotimes [_ gate-n] (gate-fn1 t)))
          #(let [^GateT t gate-t] (dotimes [_ gate-n] (.-a t)))
          2.0)
  ;; A protocol call on a string costs about what one on a record does: the
  ;; protocol fn keys its cache by the host type (rt.ss jolt-pic-host-key/i),
  ;; where a string resolved on every call. Reads ~1.3.
  (judge! "host-protocol"
          #(let [t gate-t] (dotimes [_ gate-n] (gate-hm t)))
          #(let [s gate-str] (dotimes [_ gate-n] (gate-hm s)))
          3.0)
  ;; (merge nil m) costs about what (conj m nil) does: merge's fixed arities and
  ;; (conj {} m) answering m. It was the variadic reduce, ~450 ns. Reads ~1.5.
  (judge! "merge-nil"
          #(let [m gate-m4] (dotimes [_ gate-n] (conj m nil)))
          #(let [m gate-m4] (dotimes [_ gate-n] (merge nil m)))
          3.0)
  ;; (str sb) costs about what (.toString sb) does (rt.ss jolt-str1/i); through
  ;; clojure.core/str it was ~50 ns against ~15. Reads ~0.7.
  (judge! "str-builder"
          #(let [sb gate-sb] (dotimes [_ gate-n] (.toString sb)))
          #(let [sb gate-sb] (dotimes [_ gate-n] (str sb)))
          2.0)
  (judge! "record-with-meta"
          #(let [v gate-meta-vec m gate-meta] (dotimes [_ 200000] (with-meta v m)))
          #(let [r gate-meta-rec m gate-meta] (dotimes [_ 200000] (with-meta r m)))
          4.0)
  ;; (meta r) of a record against (meta v) of a vector, both a slot read. Reads
  ;; ~1.0; the walk to the record arm costs ~14 ns against ~5, ~2.8.
  (judge! "record-meta"
          #(let [v gate-meta-vec] (dotimes [_ gate-n] (meta v)))
          #(let [r gate-meta-rec] (dotimes [_ gate-n] (meta r)))
          2.0)
  ;; six dispatch values in rotation against one: both answer from the inline
  ;; cache, so the rotation costs only the scan. Reads ~1.2; a rotation past the
  ;; cache's size reads ~14.
  (judge! "mm-cycle"
          #(let [ks gate-mm-one] (dotimes [i gate-n] (gate-mm (nth ks (rem i 6)))))
          #(let [ks gate-mm-six] (dotimes [i gate-n] (gate-mm (nth ks (rem i 6)))))
          3.0)

  ;; pr-str of a host record (java.io.File) is the #object fallback: the print
  ;; arms, the class name and the str registry, against a vector that prints
  ;; straight off the fast path.
  ;;
  ;; The narrowed walk is what this row was for, and it is checked by the path
  ;; below (host-object-pr-arms): with the narrowing gone the ratio read 1.47 ->
  ;; 2.28 (macOS arm64), inside the 1.4-2.51 x86 CI runs read on main. The row
  ;; stays as a coarse ceiling on the rest of the #object print (the class name,
  ;; the identity hash), at 1.2x the highest CI reading.
  (judge! "host-object-pr"
          #(dotimes [_ 20000] (pr-str [1 "a"]))
          #(dotimes [_ 20000] (pr-str (java.io.File. "/tmp")))
          3.0)
  ;; What that row exists for, by the path: the print walks for a java.io.File
  ;; are narrowed to the arms that can claim one (print-arms-for, host-static.ss).
  ;; Narrowed they visit 0 of 31 pr-str arms, 0 of 36 readable ones and 1 of 67
  ;; str ones (2026-11); unnarrowed, all of them.
  (let [[a b c d e f] (read-string
                       ((scheme "(lambda (x) (format \"[~a ~a ~a ~a ~a ~a]\" (length (print-arms-for 0 jolt-pr-str-arms x)) (length jolt-pr-str-arms) (length (print-arms-for 1 jolt-pr-readable-arms x)) (length jolt-pr-readable-arms) (length (print-arms-for 2 str-render-registry x)) (length str-render-registry)))")
                        (java.io.File. "/tmp")))]
    (check! "host-object-pr-arms" (and (<= (* 4 a) b) (<= (* 4 c) d) (<= (* 4 e) f))
            (format "a File's print walks visit %d/%d pr-str, %d/%d readable, %d/%d str arms (want at most a quarter)"
                    a b c d e f)))

  (.delete (java.io.File. src-path))
  (.delete (java.io.File. data-path))
  (doseq [p small-paths] (.delete (java.io.File. p)))
  (.delete (java.io.File. pipe-path))
  (.delete (java.io.File. pipe-out-path))

  (if (seq @failures)
    (do (println "\nFAIL — fast paths regressed:")
        (doseq [f @failures] (println "  -" f))
        (println "\nEach ratio compares two ways of doing the same work in one process.")
        (println "A ratio this high means the fast path stopped being taken; see the")
        (println "header of test/fastpath_ratio_test.clj for what each arm gates.")
        (System/exit 1))
    (println "\nok — every fast path still taken")))

(apply -main *command-line-args*)
