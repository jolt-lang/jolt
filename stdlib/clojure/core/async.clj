;; clojure.core.async — higher-level dataflow API over the channel primitives.
;;
;; The primitives (chan, <!, >!, <!!, >!!, close!, put!, take!, offer!, timeout,
;; promise-chan, buffer/dropping-buffer/sliding-buffer, go/go-loop/thread, go-spawn)
;; are provided natively (host/chez/java/async.ss) on real OS threads. This overlay
;; adds the portable dataflow operators — alts!, pipe, pipeline, split, reduce,
;; transduce, mult, mix, pub/sub, map, merge, and the deprecated map</map>/… —
;; ported from clojure.core.async over those primitives. Because go blocks are real
;; threads, parking ops are ordinary blocking ops and work anywhere; this is a
;; superset of the JVM model (no fixed thread pool, no pending-op limit).

(ns clojure.core.async
  (:refer-clojure :exclude [reduce transduce into merge map take partition partition-by]))

;; --- alts -------------------------------------------------------------------
;; do-alts uses a per-call handler registered on each channel (no poll loop).
;; The __do-alts host primitive handles the fast pass, registration, wait, and
;; unregistration atomically under per-channel locks.

(defn- alt-attempt [port]
  (if (vector? port)
    (let [ch (nth port 0) v (nth port 1)]
      (assert (some? v) "Can't put nil on channel")
      (let [r (clojure.core.async/__offer! ch v)]
        (when (some? r) [r ch])))
    (let [r (clojure.core.async/__poll! port)]
      (when (not= r ::none) [r port]))))

(defn do-alts
  "Returns [val port] for the first ready op among ports. ports is a vector of
  take ports and/or [channel val] put specs. opts may include :priority true
  (try in order) and :default val (return [val :default] if none ready)."
  [ports opts]
  (assert (pos? (count ports)) "alts must have at least one channel operation")
  (let [ports (vec ports)
        has-default (contains? opts :default)]
    ;; one fast non-blocking scan for :default support
    (let [start (if (:priority opts) 0 (rand-int (count ports)))
          n (count ports)
          hit (loop [k 0]
                (when (< k n)
                  (let [j (+ start k) i (if (< j n) j (- j n))]
                    (or (alt-attempt (nth ports i))
                        (recur (inc k))))))]
      (if hit
        hit
        (if has-default
          [(:default opts) :default]
          (clojure.core.async/__do-alts ports (boolean (:priority opts))))))))

(defn alts!!
  "Completes at most one of several channel operations. ports is a vector of take
  ports and/or [channel val] put specs. Returns [val port]. Blocks until ready."
  [ports & {:as opts}]
  (do-alts ports opts))

(defn alts!
  "Like alts!!. In jolt a go block is a real thread, so parking and blocking alts
  are the same operation."
  [ports & {:as opts}]
  (do-alts ports opts))

(defn poll!
  "Takes a val from port if possible immediately. Never blocks. Returns the value
  or nil."
  [port]
  (let [r (clojure.core.async/__poll! port)]
    (when (not= r ::none) r)))

;; --- thread variants --------------------------------------------------------

(defn thread-call
  "Executes f in another thread, returning a channel that receives f's result then
  closes."
  ([f] (clojure.core.async/go-spawn f))
  ([f _workload] (clojure.core.async/go-spawn f)))

(defmacro io-thread
  "Executes body in another thread, returning a channel that receives the result
  then closes."
  [& body]
  `(thread-call (fn [] ~@body) :io))

;; --- pipe / pipeline --------------------------------------------------------

(defn pipe
  "Takes elements from the from channel and supplies them to the to channel.
  Closes to when from closes unless close? is false."
  ([from to] (pipe from to true))
  ([from to close?]
   (go-loop []
     (let [v (<! from)]
       (if (nil? v)
         (when close? (close! to))
         (when (>! to v)
           (recur)))))
   to))

(defn- pipeline*
  [n to xf from close? ex-handler type]
  (assert (pos? n))
  (let [jobs (chan n)
        results (chan n)
        process (fn [job]
                  (if (nil? job)
                    (do (close! results) nil)
                    (let [v (nth job 0) p (nth job 1)
                          res (chan 1 xf ex-handler)]
                      (>!! res v)
                      (close! res)
                      (put! p res)
                      true)))
        afn (fn [job]
              (if (nil? job)
                (do (close! results) nil)
                (let [v (nth job 0) p (nth job 1)
                      res (chan 1)]
                  (xf v res)
                  (put! p res)
                  true)))]
    (dotimes [_ n]
      (case type
        (:blocking :compute) (thread
                               (loop []
                                 (let [job (<!! jobs)]
                                   (when (process job)
                                     (recur)))))
        :async (go-loop []
                 (let [job (<! jobs)]
                   (when (afn job)
                     (recur))))))
    (go-loop []
      (let [v (<! from)]
        (if (nil? v)
          (close! jobs)
          (let [p (chan 1)]
            (>! jobs [v p])
            (>! results p)
            (recur)))))
    (go-loop []
      (let [p (<! results)]
        (if (nil? p)
          (when close? (close! to))
          (let [res (<! p)]
            (loop []
              (let [v (<! res)]
                (when (and (not (nil? v)) (>! to v))
                  (recur))))
            (recur)))))))

(defn pipeline
  "Takes elements from from, applies transducer xf with parallelism n, supplies to
  to. Outputs are ordered relative to inputs."
  ([n to xf from] (pipeline n to xf from true))
  ([n to xf from close?] (pipeline n to xf from close? nil))
  ([n to xf from close? ex-handler] (pipeline* n to xf from close? ex-handler :compute)))

(defn pipeline-blocking
  "Like pipeline, for blocking operations."
  ([n to xf from] (pipeline-blocking n to xf from true))
  ([n to xf from close?] (pipeline-blocking n to xf from close? nil))
  ([n to xf from close? ex-handler] (pipeline* n to xf from close? ex-handler :blocking)))

(defn pipeline-async
  "Like pipeline, for async fns af of two args [input result-channel]."
  ([n to af from] (pipeline-async n to af from true))
  ([n to af from close?] (pipeline* n to af from close? nil :async)))

(defn split
  "Splits ch by predicate p into [true-chan false-chan]."
  ([p ch] (split p ch nil nil))
  ([p ch t-buf-or-n f-buf-or-n]
   (let [tc (chan t-buf-or-n)
         fc (chan f-buf-or-n)]
     (go-loop []
       (let [v (<! ch)]
         (if (nil? v)
           (do (close! tc) (close! fc))
           (when (>! (if (p v) tc fc) v)
             (recur)))))
     [tc fc])))

;; --- reduce / transduce / collection sinks ----------------------------------

(defn reduce
  "Returns a channel with the single result of reducing ch with f from init."
  [f init ch]
  (go-loop [ret init]
    (let [v (<! ch)]
      (if (nil? v)
        ret
        (let [ret' (f ret v)]
          (if (reduced? ret')
            @ret'
            (recur ret')))))))

(defn transduce
  "async/reduces ch with the transformation (xform f), returning a channel with the
  result."
  [xform f init ch]
  (let [f (xform f)]
    (go
      (let [ret (<! (reduce f init ch))]
        (f ret)))))

(defn- bounded-count [n coll]
  (if (counted? coll)
    (min n (count coll))
    (loop [i 0 s (seq coll)]
      (if (and s (< i n))
        (recur (inc i) (next s))
        i))))

(defn onto-chan!
  "Puts the contents of coll into ch, closing ch after unless close? is false.
  Returns a channel that closes when done."
  ([ch coll] (onto-chan! ch coll true))
  ([ch coll close?]
   (go-loop [vs (seq coll)]
     (if (and vs (>! ch (first vs)))
       (recur (next vs))
       (when close?
         (close! ch))))))

(defn to-chan!
  "Returns a channel containing the contents of coll, closing when exhausted."
  [coll]
  (let [c (bounded-count 100 coll)]
    (if (pos? c)
      (let [ch (chan c)]
        (onto-chan! ch coll)
        ch)
      (let [ch (chan)]
        (close! ch)
        ch))))

(defn onto-chan!!
  "Like onto-chan! for use when accessing coll might block."
  ([ch coll] (onto-chan!! ch coll true))
  ([ch coll close?]
   (thread
     (loop [vs (seq coll)]
       (if (and vs (>!! ch (first vs)))
         (recur (next vs))
         (when close?
           (close! ch)))))))

(defn to-chan!!
  "Like to-chan! for use when accessing coll might block."
  [coll]
  (let [c (bounded-count 100 coll)]
    (if (pos? c)
      (let [ch (chan c)]
        (onto-chan!! ch coll)
        ch)
      (let [ch (chan)]
        (close! ch)
        ch))))

(defn onto-chan
  "Deprecated - use onto-chan! or onto-chan!!"
  ([ch coll] (onto-chan! ch coll true))
  ([ch coll close?] (onto-chan! ch coll close?)))

(defn to-chan
  "Deprecated - use to-chan! or to-chan!!"
  [coll]
  (to-chan! coll))

(defn into
  "Returns a channel with the single collection result of conjoining items from ch
  onto coll. ch must close first."
  [coll ch]
  (reduce conj coll ch))

(defn take
  "Returns a channel that returns at most n items from ch, then closes."
  ([n ch] (take n ch nil))
  ([n ch buf-or-n]
   (let [out (chan buf-or-n)]
     (go (loop [x 0]
           (when (< x n)
             (let [v (<! ch)]
               (when (not (nil? v))
                 (>! out v)
                 (recur (inc x))))))
         (close! out))
     out)))

;; --- mult / tap -------------------------------------------------------------

(defprotocol Mux
  (muxch* [_]))

(defprotocol Mult
  (tap* [m ch close?])
  (untap* [m ch])
  (untap-all* [m]))

(defn mult
  "Creates a mult of ch. Copies can be created with tap and removed with untap.
  Each item is distributed to all taps synchronously."
  [ch]
  (let [cs (atom {})
        m (reify
            Mux
            (muxch* [_] ch)
            Mult
            (tap* [_ ch close?] (swap! cs assoc ch close?) nil)
            (untap* [_ ch] (swap! cs dissoc ch) nil)
            (untap-all* [_] (reset! cs {}) nil))
        dchan (chan 1)
        dctr (atom nil)
        done (fn [_] (when (zero? (swap! dctr dec))
                       (put! dchan true)))]
    (go-loop []
      (let [val (<! ch)]
        (if (nil? val)
          (doseq [[c close?] @cs]
            (when close? (close! c)))
          (let [chs (keys @cs)]
            (reset! dctr (count chs))
            (doseq [c chs]
              (when-not (put! c val done)
                (untap* m c)))
            (when (seq chs)
              (<! dchan))
            (recur)))))
    m))

(defn tap
  "Copies the mult source onto ch. Closes ch when the source closes unless close?
  is false."
  ([mult ch] (tap mult ch true))
  ([mult ch close?] (tap* mult ch close?) ch))

(defn untap
  "Disconnects ch from a mult."
  [mult ch]
  (untap* mult ch))

(defn untap-all
  "Disconnects all channels from a mult."
  [mult]
  (untap-all* mult))

;; --- mix --------------------------------------------------------------------

(defprotocol Mix
  (admix* [m ch])
  (unmix* [m ch])
  (unmix-all* [m])
  (toggle* [m state-map])
  (solo-mode* [m mode]))

(defn mix
  "Creates a mix of input channels put onto out. Inputs are added with admix,
  removed with unmix, and toggled (:mute/:pause/:solo) with toggle."
  [out]
  (let [cs (atom {})
        solo-modes #{:mute :pause}
        solo-mode (atom :mute)
        change (chan (sliding-buffer 1))
        changed #(put! change true)
        pick (fn [attr chs]
               (reduce-kv
                (fn [ret c v]
                  (if (attr v) (conj ret c) ret))
                #{} chs))
        calc-state (fn []
                     (let [chs @cs
                           mode @solo-mode
                           solos (pick :solo chs)
                           pauses (pick :pause chs)]
                       {:solos solos
                        :mutes (pick :mute chs)
                        :reads (conj
                                (if (and (= mode :pause) (seq solos))
                                  (vec solos)
                                  (vec (remove pauses (keys chs))))
                                change)}))
        m (reify
            Mux
            (muxch* [_] out)
            Mix
            (admix* [_ ch] (swap! cs assoc ch {}) (changed))
            (unmix* [_ ch] (swap! cs dissoc ch) (changed))
            (unmix-all* [_] (reset! cs {}) (changed))
            (toggle* [_ state-map] (swap! cs (partial merge-with clojure.core/merge) state-map) (changed))
            (solo-mode* [_ mode]
              (assert (solo-modes mode) (str "mode must be one of: " solo-modes))
              (reset! solo-mode mode)
              (changed)))]
    (go-loop [state (calc-state)]
      (let [{:keys [solos mutes reads]} state
            [v c] (alts! reads)]
        (if (or (nil? v) (= c change))
          (do (when (nil? v)
                (swap! cs dissoc c))
              (recur (calc-state)))
          (if (or (solos c)
                  (and (empty? solos) (not (mutes c))))
            (when (>! out v)
              (recur state))
            (recur state)))))
    m))

(defn admix
  "Adds ch as an input to the mix."
  [mix ch]
  (admix* mix ch))

(defn unmix
  "Removes ch as an input to the mix."
  [mix ch]
  (unmix* mix ch))

(defn unmix-all
  "Removes all inputs from the mix."
  [mix]
  (unmix-all* mix))

(defn toggle
  "Atomically sets the state of one or more channels in a mix."
  [mix state-map]
  (toggle* mix state-map))

(defn solo-mode
  "Sets the solo mode of the mix (:mute or :pause)."
  [mix mode]
  (solo-mode* mix mode))

;; --- pub / sub --------------------------------------------------------------

(defprotocol Pub
  (sub* [p v ch close?])
  (unsub* [p v ch])
  (unsub-all* [p] [p v]))

(defn pub
  "Creates a pub of ch partitioned by topic-fn. Subscribe with sub."
  ([ch topic-fn] (pub ch topic-fn (constantly nil)))
  ([ch topic-fn buf-fn]
   (let [mults (atom {})
         ensure-mult (fn [topic]
                       (or (get @mults topic)
                           (get (swap! mults
                                       #(if (% topic) % (assoc % topic (mult (chan (buf-fn topic))))))
                                topic)))
         p (reify
             Mux
             (muxch* [_] ch)
             Pub
             (sub* [_p topic ch close?]
               (let [m (ensure-mult topic)]
                 (tap m ch close?)))
             (unsub* [_p topic ch]
               (when-let [m (get @mults topic)]
                 (untap m ch)))
             (unsub-all* [_] (reset! mults {}))
             (unsub-all* [_ topic] (swap! mults dissoc topic)))]
     (go-loop []
       (let [val (<! ch)]
         (if (nil? val)
           (doseq [m (vals @mults)]
             (close! (muxch* m)))
           (let [topic (topic-fn val)
                 m (get @mults topic)]
             (when m
               (when-not (>! (muxch* m) val)
                 (swap! mults dissoc topic)))
             (recur)))))
     p)))

(defn sub
  "Subscribes ch to a topic of pub p."
  ([p topic ch] (sub p topic ch true))
  ([p topic ch close?] (sub* p topic ch close?)))

(defn unsub
  "Unsubscribes ch from a topic of pub p."
  [p topic ch]
  (unsub* p topic ch))

(defn unsub-all
  "Unsubscribes all channels from a pub, or from a topic."
  ([p] (unsub-all* p))
  ([p topic] (unsub-all* p topic)))

;; --- map / merge ------------------------------------------------------------

(defn map
  "Applies f to the set of first items from each source channel, then second, etc.
  Closes the output channel when any source closes."
  ([f chs] (map f chs nil))
  ([f chs buf-or-n]
   (let [chs (vec chs)
         out (chan buf-or-n)
         cnt (count chs)
         rets (atom (vec (repeat cnt nil)))
         dchan (chan 1)
         dctr (atom nil)
         done (mapv (fn [i]
                      (fn [ret]
                        (swap! rets assoc i ret)
                        (when (zero? (swap! dctr dec))
                          (put! dchan @rets))))
                    (range cnt))]
     (if (zero? cnt)
       (close! out)
       (go-loop []
         (reset! dctr cnt)
         (dotimes [i cnt]
           (take! (nth chs i) (nth done i)))
         (let [rets (<! dchan)]
           (if (some nil? rets)
             (close! out)
             (do (>! out (apply f rets))
                 (recur))))))
     out)))

(defn merge
  "Returns a channel with all values taken from the source channels chs. Closes
  after all sources close."
  ([chs] (merge chs nil))
  ([chs buf-or-n]
   (let [out (chan buf-or-n)]
     (go-loop [cs (vec chs)]
       (if (pos? (count cs))
         (let [[v c] (alts! cs)]
           (if (nil? v)
             (recur (filterv #(not= c %) cs))
             (do (>! out v)
                 (recur cs))))
         (close! out)))
     out)))

;; --- deprecated channel ops (rewritten as go-loops) -------------------------

(defn map<
  "Deprecated - use a transducer. Returns a read-side channel mapping f over ch."
  [f ch]
  (let [out (chan)]
    (go-loop []
      (let [v (<! ch)]
        (if (nil? v) (close! out) (do (>! out (f v)) (recur)))))
    out))

(defn map>
  "Deprecated - use a transducer. Returns a write-side channel mapping f into out."
  [f out]
  (let [in (chan)]
    (go-loop []
      (let [v (<! in)]
        (if (nil? v) (close! out) (do (>! out (f v)) (recur)))))
    in))

(defn filter<
  "Deprecated - use a transducer."
  ([p ch] (filter< p ch nil))
  ([p ch buf-or-n]
   (let [out (chan buf-or-n)]
     (go-loop []
       (let [val (<! ch)]
         (if (nil? val)
           (close! out)
           (do (when (p val) (>! out val))
               (recur)))))
     out)))

(defn remove<
  "Deprecated - use a transducer."
  ([p ch] (remove< p ch nil))
  ([p ch buf-or-n] (filter< (complement p) ch buf-or-n)))

(defn filter>
  "Deprecated - use a transducer."
  [p out]
  (let [in (chan)]
    (go-loop []
      (let [v (<! in)]
        (if (nil? v)
          (close! out)
          (do (when (p v) (>! out v))
              (recur)))))
    in))

(defn remove>
  "Deprecated - use a transducer."
  [p out]
  (filter> (complement p) out))

(defn- mapcat* [f in out]
  (go-loop []
    (let [val (<! in)]
      (if (nil? val)
        (close! out)
        (do (doseq [v (f val)]
              (>! out v))
            (recur))))))

(defn mapcat<
  "Deprecated - use a transducer."
  ([f in] (mapcat< f in nil))
  ([f in buf-or-n]
   (let [out (chan buf-or-n)]
     (mapcat* f in out)
     out)))

(defn mapcat>
  "Deprecated - use a transducer."
  ([f out] (mapcat> f out nil))
  ([f out buf-or-n]
   (let [in (chan buf-or-n)]
     (mapcat* f in out)
     in)))

(defn unique
  "Deprecated - use a transducer. Drops consecutive duplicates."
  ([ch] (unique ch nil))
  ([ch buf-or-n]
   (let [out (chan buf-or-n)]
     (go (loop [last nil]
           (let [v (<! ch)]
             (when (not (nil? v))
               (if (= v last)
                 (recur last)
                 (do (>! out v)
                     (recur v))))))
         (close! out))
     out)))

(defn partition
  "Deprecated - use a transducer. Partitions ch into vectors of n."
  ([n ch] (partition n ch nil))
  ([n ch buf-or-n]
   (let [out (chan buf-or-n)]
     (go-loop [arr [] idx 0]
       (let [v (<! ch)]
         (if (not (nil? v))
           (let [arr (conj arr v) new-idx (inc idx)]
             (if (< new-idx n)
               (recur arr new-idx)
               (do (>! out arr) (recur [] 0))))
           (do (when (> idx 0) (>! out arr))
               (close! out)))))
     out)))

(defn partition-by
  "Deprecated - use a transducer. Partitions ch by runs of (f v)."
  ([f ch] (partition-by f ch nil))
  ([f ch buf-or-n]
   (let [out (chan buf-or-n)]
     (go-loop [lst [] last ::nothing]
       (let [v (<! ch)]
         (if (not (nil? v))
           (let [new-itm (f v)]
             (if (or (= new-itm last) (identical? last ::nothing))
               (recur (conj lst v) new-itm)
               (do (>! out lst) (recur [v] new-itm))))
           (do (when (> (count lst) 0) (>! out lst))
               (close! out)))))
     out)))
