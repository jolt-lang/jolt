(ns jolt-ffi-arena-test)

;; Arenas (issue #799) and the babashka.ffi-compatible surface built on them:
;; the four arena kinds and who may close each, arena-owned allocations /
;; C strings / callbacks / views, the pointer vocabulary (size, address, slice,
;; segment, reinterpret, clone, copy), layout-shaped read and write, places,
;; typed array moves, and the cfn/defcfn forms.
;;
;; Every row here is a claim one FFI makes and the other has to keep, so a
;; failure names the claim rather than the mechanism.

(require '[jolt.ffi :as ffi])
(require '[jolt.fibers])

(def failures (atom []))
(defmacro check [label expr]
  `(let [result# (try ~expr (catch Throwable e# [:threw (ex-message e#)]))]
     (when-not (= true result#)
       (swap! failures conj [~label result#]))))
(defn rejects? [f]
  (try (f) false (catch Throwable _ true)))
(defn message-of [f]
  (try (f) nil (catch Throwable e (ex-message e))))

;; -- the four kinds -----------------------------------------------------------

(check "confined-arena is an arena" (ffi/arena? (ffi/confined-arena)))
(check "a plain map is not an arena" (not (ffi/arena? {:close (fn [] nil)})))
(check "a fresh arena is open" (ffi/arena-open? (ffi/confined-arena)))
(check "close-arena closes it"
       (let [a (ffi/confined-arena)] (ffi/close-arena a) (not (ffi/arena-open? a))))
;; A second close must not double-free, and must not raise either: an early
;; release inside a with-open body is legitimate, and an exception from the
;; `finally` would replace whatever the body was reporting.
(check "a second close releases nothing and does not raise"
       (let [real-free ffi/__free
             frees (atom 0)
             a (ffi/confined-arena)]
         (ffi/alloc a 8)
         (with-redefs [ffi/__free (fn [p] (swap! frees inc) (real-free p))]
           (ffi/close-arena a)
           (ffi/close-arena a))
         (= 1 @frees)))
(check "a single-block release failure still forgets its size"
       (let [a (ffi/confined-arena)
             p (ffi/alloc a 8)
             real-free ffi/__free]
         (with-redefs [ffi/__free (fn [addr]
                                    (real-free addr)
                                    (throw (ex-info "free reported failure" {})))]
           (rejects? #(ffi/close-arena a)))
         (zero? (ffi/size p))))
;; The check and the attach are one step. If they were not, an alloc that passed
;; the check just before another thread's close would conj its block onto the
;; state close had already reset — attached to a group nothing will ever release.
;; Racing it directly is timing-dependent, so the row closes the arena from
;; inside the swap instead, which is the interleaving the fix exists for.
(check "a block cannot attach to an arena that closed mid-allocation"
       (let [a (ffi/shared-arena)
             real-free ffi/__free
             real-calloc ffi/__calloc
             frees (atom 0)]
         (with-redefs [ffi/__free (fn [p] (swap! frees inc) (real-free p))
                       ffi/__calloc (fn [n]
                                      (let [p (real-calloc n)]
                                        (ffi/close-arena a)   ; the racing close
                                        p))]
           (and (rejects? #(ffi/alloc a 8))
                ;; the block the alloc had already taken was handed back
                (= 1 @frees)))))
(check "a confined block cannot attach after reentrant owner close"
       (let [a (ffi/confined-arena)
             real-free ffi/__free
             real-calloc ffi/__calloc
             frees (atom 0)]
         (with-redefs [ffi/__free (fn [p] (swap! frees inc) (real-free p))
                       ffi/__calloc (fn [n]
                                      (let [p (real-calloc n)]
                                        ;; Simulate an allocator boundary that
                                        ;; re-enters owner-thread code and closes
                                        ;; between the open check and attachment.
                                        (ffi/close-arena a)
                                        p))]
           (and (rejects? #(ffi/alloc a 8))
                (not (ffi/arena-open? a))
                (= 1 @frees)))))
(check "a lost allocation is not left recorded in the size table"
       (let [a (ffi/shared-arena)
             real-calloc ffi/__calloc
             seen (atom nil)]
         (with-redefs [ffi/__calloc (fn [n]
                                      (let [p (real-calloc n)]
                                        (reset! seen p)
                                        (ffi/close-arena a)
                                        p))]
           (rejects? #(ffi/alloc a 8)))
         (zero? (ffi/size @seen))))

(check "allocating in a closed arena raises"
       (let [a (ffi/confined-arena)]
         (ffi/close-arena a)
         (rejects? #(ffi/alloc a 8))))
(check "global-arena is one arena"
       (= (ffi/global-arena) (ffi/global-arena)))
(check "a global arena cannot be closed"
       (rejects? #(ffi/close-arena (ffi/global-arena))))
(check "an automatic arena cannot be closed"
       (rejects? #(ffi/close-arena (ffi/auto-arena))))
(check "shared-arena closes like a confined one"
       (let [a (ffi/shared-arena)] (ffi/close-arena a) (not (ffi/arena-open? a))))
(check "a non-arena is rejected where an arena goes"
       (rejects? #(ffi/alloc {:not :an-arena} 8)))

;; A confined arena belongs to the thread that made it. The failure this
;; prevents — two threads releasing one list of blocks — has no error of its
;; own, only a fault inside the allocator, so the check has to be here.
(check "a confined arena refuses another thread"
       (let [a (ffi/confined-arena)
             outcome (atom nil)
             t (Thread. (fn [] (reset! outcome (rejects? #(ffi/alloc a 8)))))]
         (.start t)
         (.join t)
         (ffi/close-arena a)
         (= true @outcome)))
(check "a confined arena refuses a close from another thread"
       (let [a (ffi/confined-arena)
             outcome (atom nil)
             t (Thread. (fn []
                          (reset! outcome [(rejects? #(ffi/close-arena a))
                                           (ffi/arena-open? a)])))]
         (.start t)
         (.join t)
         (ffi/close-arena a)
         (= [true true] @outcome)))
(check "a shared arena accepts another thread"
       (with-open [a (ffi/shared-arena)]
         (let [outcome (atom nil)
               t (Thread. (fn [] (reset! outcome (ffi/pointer? (ffi/alloc a 8)))))]
           (.start t)
           (.join t)
           (= true @outcome))))

;; One owner THREAD is not one mutator. jolt.host/thread-id is the CARRIER's id,
;; so every fiber on one carrier passes a confined arena's owner check, and
;; preemption is polled at procedure entries — a switch lands between the read
;; and the publish of the arena's state as readily as anywhere else. That is why
;; the confined fast paths publish with compare-and-set! rather than reset!:
;; a blind read-modify-write drops a sibling's block on the floor (allocated,
;; attached to a group that no longer exists, never freed) or re-opens an arena
;; a sibling has already closed and released.
;;
;; Both rows are exact counts, not timing thresholds: a correct publish frees
;; every block exactly once whatever the interleaving, so neither can flake.
(defn- on-one-carrier [f]
  (let [carriers (jolt.fibers/carrier-count)
        ticks (jolt.fibers/preempt-ticks)]
    (jolt.fibers/set-carrier-count! 1)
    (jolt.fibers/set-preempt-ticks! 100)   ; the floor: switch as often as allowed
    (try (f)
         (finally (jolt.fibers/set-preempt-ticks! ticks)
                  (jolt.fibers/set-carrier-count! carriers)))))
(defn- fibers-doing [n f]
  (let [fs (doall (map (fn [i] (jolt.fibers/spawn (fn [] (f i) :ok))) (range n)))]
    (doseq [fb fs] (jolt.fibers/join fb))))
;; The arena has to be MADE on the carrier too — it belongs to whichever thread
;; called confined-arena, and that is the carrier, not the spawning thread.
(defn- carrier-confined-arena []
  (let [held (atom nil)]
    (jolt.fibers/join (jolt.fibers/spawn (fn [] (reset! held (ffi/confined-arena)) :ok)))
    @held))

(check "sibling fibers on one carrier lose no allocation"
       (let [real-free ffi/__free
             frees (atom 0)
             blocks 200]
         (with-redefs [ffi/__free (fn [p] (swap! frees inc) (real-free p))]
           (on-one-carrier
            (fn []
              (let [a (carrier-confined-arena)]
                (fibers-doing blocks (fn [_] (ffi/alloc a 8)))
                (jolt.fibers/join (jolt.fibers/spawn (fn [] (ffi/close-arena a) :ok)))))))
         (= blocks @frees)))
;; The close side is the same hazard the other way round, and its window — the
;; read, then the publish — is a few instructions wide, so racing it with real
;; fibers is not something a gate can rely on landing. A validator runs on the
;; atom AFTER the close has read the state and BEFORE it publishes, which is
;; exactly where the switch goes, so the row drives the interleaving instead of
;; waiting for it. A sibling's block attached in that window must still be
;; released: two allocations, two frees.
(check "a sibling allocation between a close's read and its publish is not dropped"
       (let [real-free ffi/__free
             frees (atom 0)
             a (ffi/confined-arena)
             state (:jolt.ffi/state a)
             ;; armed AFTER the install: set-validator! validates the value it
             ;; finds, so arming first would fire the sibling there instead
             armed (atom false)]
         (ffi/alloc a 8)
         (with-redefs [ffi/__free (fn [p] (swap! frees inc) (real-free p))]
           (set-validator! state (fn [_]
                                   (when @armed          ; the sibling allocates once
                                     (reset! armed false)
                                     (ffi/alloc a 8))
                                   true))
           (reset! armed true)
           (try (ffi/close-arena a)
                (finally (set-validator! state nil))))
         (and (not (ffi/arena-open? a)) (= 2 @frees))))

;; -- with-open and with-arena -------------------------------------------------

(check "with-open closes the arena"
       (let [a (ffi/confined-arena)]
         (with-open [held a] (ffi/alloc held 8))
         (not (ffi/arena-open? a))))
(check "with-open closes it when the body throws"
       (let [a (ffi/confined-arena)]
         (rejects? #(with-open [held a] (ffi/alloc held 8) (throw (ex-info "boom" {}))))
         (not (ffi/arena-open? a))))
(check "with-arena answers the body value"
       (= :answer (ffi/with-arena [a] (ffi/alloc a 8) :answer)))

;; -- arena-owned allocations --------------------------------------------------

(check "alloc answers a pointer"
       (with-open [a (ffi/confined-arena)] (ffi/pointer? (ffi/alloc a 8))))
(check "alloc zeroes the block"
       (with-open [a (ffi/confined-arena)]
         (= [0 0] [(ffi/read (ffi/alloc a 16) :int64)
                   (ffi/read (ffi/alloc a 16) :int64 8)])))
(check "the arena-less alloc zeroes it too"
       (let [p (ffi/alloc 16)]
         (try (= 0 (ffi/read p :int64)) (finally (ffi/free p)))))
(check "alloc sizes a type keyword"
       (with-open [a (ffi/confined-arena)]
         (= (ffi/sizeof :int64) (ffi/size (ffi/alloc a :int64)))))
(check "alloc sizes a layout"
       (let [l (ffi/layout [:struct [[:a :int32] [:b :int32]]])]
         (with-open [a (ffi/confined-arena)]
           (= 8 (ffi/size (ffi/alloc a l))))))
(check "alloc rejects a size it cannot read"
       (with-open [a (ffi/confined-arena)] (rejects? #(ffi/alloc a "8"))))
(check "alloc rejects a non-positive byte count in jolt's own words"
       (with-open [a (ffi/confined-arena)]
         (= "jolt.ffi: a byte count must be positive"
            (message-of #(ffi/alloc a 0)))))
(check "an alignment above the allocator's own is honoured"
       (with-open [a (ffi/confined-arena)]
         (every? (fn [_] (zero? (rem (ffi/alloc a 24 64) 64))) (range 8))))
(check "an over-aligned block still reports the size asked for"
       (with-open [a (ffi/confined-arena)] (= 24 (ffi/size (ffi/alloc a 24 64)))))
;; An alignment that is not a positive power of two is neither used nor reported
;; by the paths below it — at or under the allocator's own it is ignored, above
;; it the rounding divides by whatever came in — so it has to be refused here.
(check "an alignment must be a positive power of two"
       (with-open [a (ffi/confined-arena)]
         (every? (fn [bad] (rejects? #(ffi/alloc a 8 bad))) [0 -8 24 1.5 :eight])))
(check "closing forgets the sizes it knew"
       (let [a (ffi/confined-arena)
             p (ffi/alloc a 8)]
         (ffi/close-arena a)
         (zero? (ffi/size p))))

;; -- C strings ----------------------------------------------------------------

(check "an arena string round-trips"
       (with-open [a (ffi/confined-arena)]
         (= "héllo" (ffi/ptr->string (ffi/string->ptr a "héllo")))))
(check "an arena string knows its size, terminator included"
       (with-open [a (ffi/confined-arena)]
         (= 7 (ffi/size (ffi/string->ptr a "héllo")))))
(check "a nil arena string is NULL and allocates nothing"
       (with-open [a (ffi/confined-arena)]
         (ffi/null? (ffi/string->ptr a nil))))
(check "the arena-less string->ptr is unchanged"
       (let [p (ffi/string->ptr "abc")]
         (try (= "abc" (ffi/ptr->string p)) (finally (ffi/free p)))))
(check "ptr->string stops at a limit rather than running off the end"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 4)]
           (ffi/write-array p :uint8 (byte-array [65 66 67 68]))   ; no NUL
           (rejects? #(ffi/ptr->string p 4)))))
(check "a limit only bounds the scan"
       (with-open [a (ffi/confined-arena)]
         (= "abc" (ffi/ptr->string (ffi/string->ptr a "abc") 64))))

;; -- the pointer vocabulary ---------------------------------------------------

(check "pointer? accepts an address and refuses a non-address"
       (= [true false false] [(ffi/pointer? 4096) (ffi/pointer? -1) (ffi/pointer? :x)]))
(check "address is the pointer's own value"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 8)] (= p (ffi/address p)))))
(check "size is 0 for a pointer jolt was never told about"
       (zero? (ffi/size 123456)))
(check "segment records a declared size"
       (= 16 (ffi/size (ffi/segment 987654 16))))
(check "slice offsets the pointer"
       (= 1008 (ffi/slice 1000 8)))
(check "slice with a length records that length"
       (= 4 (ffi/size (ffi/slice 2000 8 :int32))))
(check "slice takes a layout as its length"
       (let [l (ffi/layout [:struct [[:a :int32] [:b :int32]]])]
         (= 8 (ffi/size (ffi/slice 3000 16 l)))))
(check "reinterpret gives a C pointer a size"
       (let [p (ffi/alloc 8)]
         (try (= 8 (ffi/size (ffi/reinterpret p 8)))
              (finally (ffi/free p)))))
(check "an arena forgets a reinterpreted size when it closes"
       (let [a (ffi/confined-arena)
             p (ffi/alloc 8)]
         (ffi/reinterpret a p 8)
         (ffi/close-arena a)
         (try (zero? (ffi/size p)) (finally (ffi/free p)))))
(check "an arena runs a view's cleanup with the pointer, and frees nothing itself"
       (let [seen (atom nil)
             a (ffi/confined-arena)
             p (ffi/alloc 8)]
         (ffi/reinterpret a p 8 (fn [x] (reset! seen x)))
         (ffi/close-arena a)
         (try (= p @seen) (finally (ffi/free p)))))
;; babashka.ffi puts the arena after the size, (reinterpret seg size arena
;; cleanup?). An arena and a pointer differ in type, so jolt takes that order
;; as well as its own arena-first one, and neither can be read as the other.
(check "reinterpret takes babashka.ffi's order, the arena after the size"
       (let [seen (atom nil)
             p (ffi/alloc 8)]
         (try
           (and (= 16 (ffi/with-open [a (ffi/confined-arena)]
                        (ffi/reinterpret p 16 a)
                        (ffi/size p)))
                (zero? (ffi/size p))
                (do (ffi/with-open [a (ffi/confined-arena)]
                      (ffi/reinterpret p 8 a (fn [x] (reset! seen x))))
                    (= p @seen)))
           (finally (ffi/free p)))))
(check "reinterpret with neither the first nor the third argument an arena throws"
       (rejects? #(ffi/reinterpret 1000 8 :not-an-arena)))
(check "ffi/with-open closes what it binds, as babashka.ffi's does"
       (let [a (ffi/confined-arena)]
         (and (= :body (ffi/with-open [b a] (when (ffi/arena-open? b) :body)))
              (not (ffi/arena-open? a)))))

;; The size table is keyed by ADDRESS, so an entry that outlives its allocation
;; answers a size for a block that never had one — and copy/clone take that as a
;; byte count. free forgets; an arena forgets its blocks and its views.
(check "free forgets the size it recorded, so a recycled address inherits nothing"
       (let [p (ffi/alloc 1024)]
         (ffi/reinterpret p 1024)
         (ffi/free p)
         (let [q (ffi/alloc 1024)]                ; the allocator reuses the address
           (try (zero? (ffi/size q)) (finally (ffi/free q))))))
(check "an arena forgets a segment size when it closes"
       (let [a (ffi/confined-arena)
             p (ffi/alloc 64)]
         (ffi/segment a p 64)
         (ffi/close-arena a)
         (try (zero? (ffi/size p)) (finally (ffi/free p)))))
(check "an arena forgets a slice size when it closes"
       (let [a (ffi/confined-arena)
             p (ffi/alloc 64)]
         (ffi/slice a p 16 16)
         (ffi/close-arena a)
         (try (zero? (ffi/size (+ p 16))) (finally (ffi/free p)))))
(check "an arena-scoped slice still records the size while the arena is open"
       (let [p (ffi/alloc 64)]
         (try (with-open [a (ffi/confined-arena)]
                (= 16 (ffi/size (ffi/slice a p 16 16))))
              (finally (ffi/free p)))))
(check "the arena-first argument order is enforced, not just documented"
       (and (rejects? #(ffi/reinterpret (ffi/global-arena) 8))
            (rejects? #(ffi/segment (ffi/global-arena) 8))
            (rejects? #(ffi/slice (ffi/global-arena) 8 16))))

;; -- copy and clone -----------------------------------------------------------

(check "copy moves a counted block"
       (with-open [a (ffi/confined-arena)]
         (let [src (ffi/alloc a 8) dst (ffi/alloc a 8)]
           (ffi/write src :int64 -99)
           (ffi/copy src dst 8)
           (= -99 (ffi/read dst :int64)))))
(check "copy without a count uses the source's known size"
       (with-open [a (ffi/confined-arena)]
         (let [src (ffi/alloc a :int64) dst (ffi/alloc a :int64)]
           (ffi/write src :int64 7)
           (ffi/copy src dst)
           (= 7 (ffi/read dst :int64)))))
(check "copy without a count refuses a source of unknown size"
       (with-open [a (ffi/confined-arena)]
         (rejects? #(ffi/copy 123456 (ffi/alloc a 8)))))
(check "copy is memmove: overlapping regions still move correctly"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 8)]
           (ffi/write-array p :uint8 (byte-array [1 2 3 4 5 6 7 8]))
           (ffi/copy p (ffi/slice p 2) 4)
           (= [1 2 1 2 3 4 7 8] (vec (ffi/read-array p :uint8 8))))))
(check "clone copies into the arena"
       (with-open [a (ffi/confined-arena)]
         (let [src (ffi/alloc a :int64)]
           (ffi/write src :int64 12345)
           (= 12345 (ffi/read (ffi/clone a src) :int64)))))
(check "clone answers a distinct pointer"
       (with-open [a (ffi/confined-arena)]
         (let [src (ffi/alloc a :int64)] (not= src (ffi/clone a src)))))
(check "clone refuses a source of unknown size"
       (with-open [a (ffi/confined-arena)] (rejects? #(ffi/clone a 123456))))

;; -- sizeof / alignof over both a type and a layout ---------------------------

(check "sizeof takes a type keyword" (= 4 (ffi/sizeof :int32)))
(check "sizeof takes a layout"
       (= 8 (ffi/sizeof (ffi/layout [:struct [[:a :int32] [:b :int32]]]))))
(check "alignof takes a type keyword" (= 8 (ffi/alignof :double)))
(check "alignof takes a layout"
       (= 8 (ffi/alignof (ffi/layout [:struct [[:a :uint8] [:b :double]]]))))
(check "layout-size and sizeof agree"
       (let [l (ffi/layout [:struct [[:a :uint8] [:b :double]]])]
         (= (ffi/layout-size l) (ffi/sizeof l))))

;; -- layout-shaped read and write ---------------------------------------------

(def record
  (ffi/layout [:struct [[:tag :uint8]
                        [:name [:array :char 4]]
                        [:params [:array :uint32 3]]
                        [:matrix [:array [:array :uint16 2] 2]]
                        [:flag :bool]]]))
(def value
  {:tag 7 :name [\a \b \c \d] :params [1 2 3]
   :matrix [[10 20] [30 40]] :flag true})

(check "a struct round-trips as a map"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a record)]
           (ffi/write p record value)
           (= value (ffi/read p record)))))
(check "a layout read and a read-field of the same member agree"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a record)]
           (ffi/write p record value)
           (= (get-in (ffi/read p record) [:params 2])
              (ffi/read-field p record [:params 2])))))
(check "a nested array element reads through both routes"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a record)]
           (ffi/write p record value)
           (= 30 (ffi/read-field p record [:matrix 1 0])))))
(check "a layout write at an offset writes there"
       (with-open [a (ffi/confined-arena)]
         (let [l (ffi/layout [:struct [[:a :int32]]])
               p (ffi/alloc a 16)]
           (ffi/write p l {:a 5} 8)
           (= [0 5] [(ffi/read p :int32) (ffi/read p :int32 8)]))))
(check "a struct value missing a field is refused"
       (with-open [a (ffi/confined-arena)]
         (rejects? #(ffi/write (ffi/alloc a record) record (dissoc value :tag)))))
(check "a struct value with an extra field is refused"
       (with-open [a (ffi/confined-arena)]
         (rejects? #(ffi/write (ffi/alloc a record) record (assoc value :extra 1)))))
(check "a struct value that is not a map is refused"
       (with-open [a (ffi/confined-arena)]
         (rejects? #(ffi/write (ffi/alloc a record) record [7]))))
(check "an array value of the wrong length is refused"
       (let [l (ffi/layout [:struct [[:xs [:array :uint32 3]]]])]
         (with-open [a (ffi/confined-arena)]
           (rejects? #(ffi/write (ffi/alloc a l) l {:xs [1 2]})))))
(check "an array value may be a jolt array"
       (let [l (ffi/layout [:struct [[:xs [:array :uint32 3]]]])]
         (with-open [a (ffi/confined-arena)]
           (let [p (ffi/alloc a l)]
             (ffi/write p l {:xs (int-array [4 5 6])})
             (= {:xs [4 5 6]} (ffi/read p l))))))
(check "an array value may be any sequence of the right length"
       (let [l (ffi/layout [:struct [[:xs [:array :uint32 3]]]])]
         (with-open [a (ffi/confined-arena)]
           (let [p (ffi/alloc a l)]
             (ffi/write p l {:xs (range 1 4)})
             (= {:xs [1 2 3]} (ffi/read p l))))))
(check "the transposed array descriptor is a compile-time error"
       (rejects? #(eval '(jolt.ffi/__layout [:struct [[:x [:array 4 :int]]]]))))

;; -- places -------------------------------------------------------------------

(check "a place reads one scalar member"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a record)
               tag (ffi/place record :tag)]
           (ffi/write p record value)
           (= 7 (ffi/read p tag)))))
(check "a place writes one scalar member"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a record)
               tag (ffi/place record :tag)]
           (ffi/write p tag 3)
           (= 3 (ffi/read p tag)))))
(check "a place at an array member decodes as a vector"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a record)]
           (ffi/write p record value)
           (= [1 2 3] (ffi/read p (ffi/place record :params))))))
(check "a place reaches into a nested array"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a record)]
           (ffi/write p record value)
           (= [30 40] (ffi/read p (ffi/place record [:matrix 1]))))))
(check "a place at one array element is a scalar"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a record)]
           (ffi/write p record value)
           (= 2 (ffi/read p (ffi/place record [:params 1]))))))
(check "a place with no path is the whole layout"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a record)]
           (ffi/write p (ffi/place record) value)
           (= value (ffi/read p (ffi/place record))))))
(check "a place that names nothing raises, rather than answering nil"
       (rejects? #(ffi/place record :nope)))
(check "a place out of an array's bounds raises"
       (rejects? #(ffi/place record [:params 9])))
(check "read refuses a type it cannot read"
       (rejects? #(ffi/read 4096 "not-a-type")))

;; -- typed array moves --------------------------------------------------------

(check "int32 elements round-trip, bits and all"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 64)]
           (ffi/write-array p :int32 (int-array [1 -2 2147483647]))
           (= [1 -2 2147483647] (vec (ffi/read-array p :int32 3))))))
(check "a uint32 above the signed maximum reads back as its bits"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 64)]
           (ffi/write-array p :uint32 (int-array [-1]))
           (= [-1] (vec (ffi/read-array p :uint32 1))))))
(check "int64 elements round-trip"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 64)]
           (ffi/write-array p :int64 (long-array [10 -20]))
           (= [10 -20] (vec (ffi/read-array p :int64 2))))))
(check "int16 elements round-trip"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 64)]
           (ffi/write-array p :int16 (short-array [-32768 32767]))
           (= [-32768 32767] (vec (ffi/read-array p :int16 2))))))
(check "double elements round-trip"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 64)]
           (ffi/write-array p :double (double-array [1.5 -2.5]))
           (= [1.5 -2.5] (vec (ffi/read-array p :double 2))))))
(check "one-byte elements move as a byte-array"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 64)]
           (ffi/write-array p :uint8 (byte-array [1 2 3]))
           (= [1 2 3] (vec (ffi/read-array p :uint8 3))))))
(check "a typed move honours a byte offset"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 64)]
           (ffi/write-array p :int32 (int-array [9]) 8)
           (= [0 0 9] (vec (ffi/read-array p :int32 3))))))
(check "a slice of a byte-array writes with a one-byte type"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a 64)]
           (ffi/write-array p :byte (byte-array [4 5 6]) 0 0 3)
           (= [4 5 6] (vec (ffi/read-array p 3))))))
(check ":string cannot be copied as an array element"
       (rejects? #(ffi/read-array 4096 :string 1)))

;; -- functions ----------------------------------------------------------------

(ffi/load-library)
(ffi/defcfn c-strlen "strlen" [:string] :size_t)
(ffi/defcfn documented "the length of a C string" {:private true}
  "strlen" [:string] :size_t)
(ffi/defcfn strlen-plus "strlen" [:string] :size_t raw [s] (+ 100 (raw s)))
(def bound (ffi/cfn "strlen" [:string] :size_t))

(check "defcfn binds a C function" (= 5 (c-strlen "hello")))
(check "defcfn keeps a docstring" (= "the length of a C string" (:doc (meta #'documented))))
(check "defcfn keeps the attribute map's metadata" (= true (:private (meta #'documented))))
(check "the attribute map does not shadow the C symbol" (= 3 (documented "abc")))
(check "the wrapper form wraps the raw binding" (= 103 (strlen-plus "abc")))
(check "cfn is foreign-fn under babashka.ffi's name" (= 4 (bound "abcd")))
(check "a library-scoped cfn raises rather than searching everything"
       (rejects? #(macroexpand '(jolt.ffi/cfn {:path "x"} "strlen" [:string] :size_t))))
(check "a :library attribute raises for the same reason"
       (rejects? #(macroexpand '(jolt.ffi/defcfn f {:library :x} "strlen" [:string] :size_t))))
;; :& is babashka.ffi's variadic marker and means what :varargs means here, in
;; both of its forms: the declared tail, and the BARE marker whose tail each call
;; infers from the values it is given.
(check ":& declares a variadic binding"
       (integer? ((ffi/cfn "fcntl" [:int :int :& :int] :int) 0 1 0)))
(check "a leading :& rejects, as a leading :varargs does"
       (rejects? #(eval '(jolt.ffi/__cfn "fcntl" [:& :int] :int))))

;; One bare-:& binding serving several tail shapes, through the public macro —
;; babashka.ffi's [:string :int :&], which used to reject here.
(ffi/defcfn c-bare-snprintf "snprintf" [:pointer :size_t :string :&] :int)
(defn- bare-fmt [f & args]
  (let [b (ffi/alloc 128)]
    (try (apply c-bare-snprintf b 128 f args) (ffi/ptr->string b)
         (finally (ffi/free b)))))
(check "bare :& — empty tail" (= "none" (bare-fmt "none")))
(check "bare :& — integer tail" (= "42" (bare-fmt "%d" 42)))
(check "bare :& — double tail" (= "2.50" (bare-fmt "%.2f" 2.5)))
(check "bare :& — string tail" (= "hi" (bare-fmt "%s" "hi")))
(check "bare :& — boolean and nil ride as 64-bit integers"
       (= "1 0 0" (bare-fmt "%d %d %d" true false nil)))
(check "bare :& — a ratio promotes to double" (= "0.250" (bare-fmt "%.3f" 1/4)))
(check "bare :& — a second shape off the same binding, then the first again"
       (and (= "x=6" (bare-fmt "%s=%d" "x" 6)) (= "7" (bare-fmt "%d" 7))))
(check "bare :& — a tail value in no carrier names the carriers it has"
       (let [msg (message-of #(bare-fmt "%d" :kw))]
         (and (string? msg) (not (nil? (re-find #"bare :& tail" msg))))))
;; The tail is inferred per call, so :capture-native-error still pairs the C
;; result with the error slot — the two conventions compose here as they do for
;; a declared tail.
(ffi/defcfn c-bare-open "open" [:string :int :&] :int {:capture-native-error true})
(check "bare :& composes with :capture-native-error"
       (let [r (c-bare-open "/jolt/no/such/path" 0)]
         (and (neg? (nth r 0)) (pos? (nth r 1)))))
(check "bare :& + :capture-native-error, one-int tail"
       (let [r (c-bare-open "/jolt/no/such/path" 0 0644)]
         (and (neg? (nth r 0)) (pos? (nth r 1)))))

(check "find-symbol answers an address for a symbol that exists"
       (ffi/pointer? (ffi/find-symbol "strlen")))
(check "find-symbol answers nil for one that does not"
       (nil? (ffi/find-symbol "jolt_no_such_symbol_anywhere")))
;; babashka.ffi's library-scoped form searches the one library load-library
;; answered (and its dependencies). Windows loads every library into the process
;; namespace, so there is no handle to scope to and the form refuses.
(let [libm (ffi/load-system-library "m")
      windows? (.startsWith (str (System/getProperty "os.name")) "Windows")]
  (if windows?
    (check "find-symbol scoped to a library refuses on Windows"
           (rejects? #(ffi/find-symbol libm "cos")))
    (do
      (check "find-symbol scoped to a library finds its symbol"
             (= (ffi/find-symbol libm "cos") (ffi/find-symbol "cos")))
      (check "find-symbol scoped to a library answers nil for a missing one"
             (nil? (ffi/find-symbol libm "jolt_no_such_symbol_anywhere")))))
  (check "find-symbol with a nil library is the unscoped search"
         (= (ffi/find-symbol nil "strlen") (ffi/find-symbol "strlen")))
  (check "find-symbol scoped to a library that was never loaded throws"
         (rejects? #(ffi/find-symbol {:path "/jolt/no/such/libnothing.so"} "strlen"))))
(check "load-system-library answers a library map naming what loaded"
       (string? (:path (ffi/load-system-library "m"))))
;; The real candidate comes from load-system-library rather than being written
;; in: libm's soname is per-platform (libm.so.6 on glibc, libm.dylib on macOS),
;; and the row is about the ORDER, not the spelling. Whatever this host answers
;; for "m" is by construction loadable, so putting a name that cannot exist in
;; front of it tests exactly the one thing: the second candidate is the answer.
(check "load-library tries candidates in order and answers the one that took"
       (let [real (:path (ffi/load-system-library "m"))]
         (= real (:path (ffi/load-library ["libnot-a-library.so.99" real])))))
(check "load-library raises when no candidate loads"
       (rejects? #(ffi/load-library ["libnot-a-library.so.98" "libnot-a-library.so.99"])))
(check "(load-library nil) is still the documented no-op"
       (nil? (ffi/load-library nil)))

;; An arena-owned callback: C calls back into jolt through it, and the arena is
;; the only thing that releases it.
(ffi/defcfn c-qsort "qsort" [:pointer :size_t :size_t :pointer] :void)
(defn- compare-int32 [a b]
  (let [x (ffi/read a :int32) y (ffi/read b :int32)]
    (cond (< x y) -1 (> x y) 1 :else 0)))

(check "C sorts through an arena-owned callback"
       (with-open [a (ffi/confined-arena)]
         (let [p (ffi/alloc a (* 5 4))
               cmp (ffi/callback a compare-int32 [:pointer :pointer] :int)]
           (ffi/write-array p :int32 (int-array [5 3 9 1 7]))
           (c-qsort p 5 4 cmp)
           (= [1 3 5 7 9] (vec (ffi/read-array p :int32 5))))))
(check "a callback needs an open arena"
       (let [a (ffi/confined-arena)]
         (ffi/close-arena a)
         (rejects? #(ffi/callback a compare-int32 [:pointer :pointer] :int))))

;; -- automatic arenas ---------------------------------------------------------
;; An automatic arena is released once the COLLECTOR reclaims it, which is only
;; observable through a drain. The rows below drop a batch of them, collect, and
;; check that a drain reports the release — the release itself is a free, which
;; has no visible answer of its own.

(check "an automatic arena allocates"
       (ffi/pointer? (ffi/alloc (ffi/auto-arena) 128)))
(check "dropped automatic arenas are released on the next drain"
       (do (dotimes [_ 32] (ffi/alloc (ffi/auto-arena) 64))
           (System/gc)
           (pos? (ffi/drain-auto-arenas!))))
(check "a drain with nothing reclaimed releases nothing"
       (do (ffi/drain-auto-arenas!)
           (zero? (ffi/drain-auto-arenas!))))
(check "an automatic arena still reachable is not released"
       (let [held (ffi/auto-arena)
             p (ffi/alloc held :int64)]
         (System/gc)
         (ffi/drain-auto-arenas!)
         (ffi/write p :int64 42)
         (= [42 true] [(ffi/read p :int64) (ffi/arena-open? held)])))

;; -- a closing arena releases everything it holds -----------------------------

(check "close releases blocks, strings and callbacks in one pass"
       (let [real-free ffi/__free
             real-free-callable ffi/free-callable
             frees (atom 0)
             callables (atom 0)]
         (with-redefs [ffi/__free (fn [p] (swap! frees inc) (real-free p))
                       ffi/free-callable (fn [p] (swap! callables inc) (real-free-callable p))]
           (let [a (ffi/confined-arena)]
             (ffi/alloc a 8)
             (ffi/alloc a 8)
             (ffi/string->ptr a "x")
             (ffi/callback a compare-int32 [:pointer :pointer] :int)
             (ffi/close-arena a)))
         (= [3 1] [@frees @callables])))
(check "close releases multiple blocks in reverse allocation order"
       (let [a (ffi/confined-arena)
             blocks [(ffi/alloc a 8) (ffi/alloc a 8) (ffi/alloc a 8)]
             real-free ffi/__free
             released (atom [])]
         (with-redefs [ffi/__free (fn [p]
                                    (swap! released conj p)
                                    (real-free p))]
           (ffi/close-arena a))
         (= (vec (reverse blocks)) @released)))
(check "a cleanup that raises does not strand the rest of the group"
       (let [real-free ffi/__free
             frees (atom 0)]
         (with-redefs [ffi/__free (fn [p] (swap! frees inc) (real-free p))]
           (let [a (ffi/confined-arena)
                 view (ffi/alloc 8)]
             (ffi/alloc a 8)
             (ffi/reinterpret a view 8 (fn [_] (throw (ex-info "cleanup failed" {}))))
             (rejects? #(ffi/close-arena a))
             (real-free view)))
         (= 1 @frees)))

;; -- errno --------------------------------------------------------------------

(check "errno still reads an integer" (integer? (ffi/errno)))
(check "errno-message still answers a string" (string? (ffi/errno-message 2)))

(if (empty? @failures)
  (do (println "JOLT-FFI-ARENA-TEST OK") (flush) (System/exit 0))
  (do (doseq [[label result] @failures] (println "FAIL:" label "=>" (pr-str result)))
      (flush)
      (System/exit 1)))
