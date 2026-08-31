(ns jolt.ffi
  "Foreign-function interface for jolt libraries. A library loads a shared object
  and declares typed foreign functions, then exposes a Clojure API over them — no
  jolt built-in required.

      (require '[jolt.ffi :as ffi])
      (ffi/load-library {:darwin \"libsqlite3.0.dylib\" :linux \"libsqlite3.so.0\"})
      (ffi/defcfn sqlite3-open \"sqlite3_open\" [:string :pointer] :int)
      (with-open [arena (ffi/confined-arena)]
        (let [pp (ffi/alloc arena :pointer)]
          (sqlite3-open \"x.db\" pp)
          (ffi/read pp :pointer)))

  Every allocation belongs to an ARENA, which owns its lifetime, or to the
  caller, who frees it by hand. See \"arenas\" below.

  Types (keywords): :int :uint :long :ulong :int64 :uint64 :size_t :ssize_t
  :iptr :uptr :double :float :pointer :string :bool :void :uint8/:u8/:byte
  :char, plus exact scalar widths :int8/:i8, :int16/:short, :uint16/:ushort,
  :int32, :uint32.
  Exact widths use native byte order for both memory and signatures. Signed and
  unsigned names at one width expose the same stored bits; wire byte order stays
  an explicit codec or htons/ntohs concern.

  :bool is a ONE-BYTE C boolean (C99 _Bool), true or false in jolt. jolt
  truthiness decides the byte on the way out, so nil and false send 0 and every
  other value sends 1 — a C predicate answers true/false rather than the truthy
  number 0.

  :string carries NULL as nil in both directions: nil as an argument reaches C
  as a null char*, which is how APIs like setlocale spell \"query instead of
  set\", and a function returning NULL reads as nil. ffi/null stays the
  :pointer spelling of NULL and is unchanged. A foreign-callable gets the same
  translation with the directions swapped, since C is the caller there: a null
  char* C passes in arrives as nil, and returning nil hands C a null char*.

  nil is the only spelling of NULL a :string position accepts. false is
  rejected rather than sent, though Chez's own `string` type would take it as
  NULL: a boolean arriving in a :string position is a mistake — a `when` that
  did not fire, a predicate result — and silently sending it would land on the
  one value C reads as \"absent\". (A boolean belongs in a :bool position, where
  it means what it says.) Every other non-string is rejected too, which the
  other foreign types already did on their own.

  A struct passed or returned by value uses the same literal descriptor as
  layout, wrapped in [:by-value descriptor]. An argument value is a non-null
  caller-owned pointer to the struct bytes. An aggregate-returning callable takes
  a non-null caller-owned destination pointer as its first Jolt argument, writes
  the C return there, and returns that pointer. Aggregate callbacks and exports
  are not supported. A fixed aggregate may precede the variadic marker, but
  aggregate variadic arguments and aggregate-return-plus-varargs are rejected.

  LAYOUTS. (layout [:struct [[field type] ...]]) compiles a literal descriptor
  to ABI layout data. Fields may use [:array element-type positive-count];
  arrays may hold fixed-size scalars, nested structs, or other fixed arrays.
  read and write take a compiled layout where they take a type: a struct reads
  as a MAP of its fields and writes from one, an array reads as a VECTOR and
  writes from any sequence of the declared length. field-offset, read-field and
  write-field reach one member by path, and integer components address array
  elements: [:params 3], [:events 1 :frame], [:matrix 1 2]. `place` resolves one
  such path ONCE into a value read and write accept where they take a type, for
  a member read in a loop.

  TWO LIBRARIES, ONE COPY. A declared :jolt/native is dlopen'd RTLD_LOCAL, so
  its symbols reach its own defcfns and nobody else's. That is fine for a
  dependent library linked against the SHARED base — it resolves the base's
  symbols through its own handle and there is one copy of the base's globals.
  It is not fine for one linked against the base's STATIC archive: that library
  carries its own copy, so writes through one never appear in the other, and
  nothing raises. raygui built against libraylib.a is the case that named this
  — every control reads a mouse that never moves. jolt reports a duplicate
  native symbol on stderr the first time such a symbol is bound, and
  defining-libraries answers which libraries supply distinct definitions. The
  fix is always to rebuild the dependent against the shared base library.

  ARENAS. An arena is a group of allocations with one lifetime: allocate into
  it, and closing it releases every block, callback and registered cleanup at
  once. Four kinds, differing only in who may use them and who closes them:

      (with-open [a (ffi/confined-arena)]     ; one thread, closed here
        ...)
      (with-open [a (ffi/shared-arena)]       ; any thread, closed here
        ...)
      (ffi/global-arena)                      ; the process; cannot be closed
      (ffi/auto-arena)                        ; released once unreachable

  A confined arena is the one to reach for inside a function: it is cheapest to
  reason about, and using it from another thread raises rather than corrupting
  memory. A shared arena is for memory that outlives the call and is released
  elsewhere. An automatic arena is released when the collector reclaims the
  arena itself, which is what a callback C may invoke from a thread jolt did not
  start needs; jolt drains reclaimed automatic arenas when arenas are next
  created or allocated in, and drain-auto-arenas! forces it.

  alloc takes a byte count, a type keyword, or a compiled layout, and the memory
  is ZEROED — a struct a caller only partly fills is the ordinary case, and
  malloc's leftovers in the rest of it are a C-visible bug that reproduces only
  under load. An integer byte count aligns to 16; a type or layout aligns
  naturally; a third argument overrides. string->ptr, clone, reinterpret and
  callback all take an arena in the same first position.

      (with-open [a (ffi/shared-arena)]
        (let [buf (ffi/alloc a 4096)
              name (ffi/string->ptr a \"config.toml\")
              on-event (ffi/callback a handle-event [:pointer :int] :void)]
          ...))                              ; all three released here

  (ffi/alloc n) with no arena is still the caller-owned form: it answers a
  pointer nobody tracks, freed with ffi/free. The scoped helpers below
  (with-alloc, with-out, with-layout, with-c-string, with-c-string-array) are
  that form with the free attached to a lexical scope, and remain the lightest
  way to hold one allocation across one call.

  CAUTION: do not close an arena while C still holds one of its pointers. C can
  read released memory, and nothing raises.

  The memory/library primitives (read/write/sizeof/load-library/ptr->string/
  string->ptr/null/null?) are host-provided, as are the buffer moves:
  read-bytes/write-bytes decode and encode UTF-8, read-array/write-array move
  raw octets to and from a byte-array, and read-into! fills a slice of an
  EXISTING byte-array — so a caller reading a stream whose length it already
  knows fills one buffer instead of regrowing an accumulator per chunk. All of
  them move the block in one copy, not a byte at a time.

  string->ptr and ptr->string round-trip nil: nil answers NULL and allocates
  nothing, NULL reads back as nil, and \"\" still allocates its NUL byte and
  still reads back as \"\" — so an absent string stays distinguishable from a
  present empty one. Every other value keeps going through the `str` coercion,
  since with-c-string copies a VALUE. NULL is safe to free, so the scoped
  helpers need no special case for it.

  Where NULL is not available to mean it, a nil is rejected instead of rendered
  as \"\": a nil foreign type (read/write/sizeof), a nil name to loaded?, and a
  nil value to write-bytes, whose destination is the caller's own buffer so
  there is no pointer that could carry absence. (load-library nil) keeps its
  documented meaning — no name at all, the process's own symbols — which is an
  answer rather than a missing one.

      (let [buf (ffi/alloc 65536)
            frame (byte-array total)]
        (loop [off 0]
          (when (< off total)
            (let [n (recv fd buf 65536 0)]
              (ffi/read-into! buf frame off n)     ; no per-chunk array
              (recur (+ off n))))))

  foreign-fn lowers a compile-time-typed signature to a real Chez
  foreign-procedure (cfn is the same macro under babashka.ffi's name). Its
  optional trailing map accepts :blocking and :capture-native-error literal
  Booleans; capture returns [native-result error-code] atomically and requires
  a non-void scalar result. foreign-callable is the inverse — it wraps a jolt fn
  as a C-callable function pointer so C can call back into jolt (e.g. GTK signal
  handlers); free-callable releases it, and `callback` is the arena-owned form
  that needs no explicit release.

  BABASHKA.FFI COMPATIBILITY. This namespace is a name-for-name, semantics-for-
  semantics match of babashka.ffi wherever one substrate can match the other, so
  a shim in either direction is a namespace alias plus a short list of gaps.
  Matching: the arena constructors and every arena-taking function, alloc/free,
  read/write (INCLUDING the argument order — the offset is last), sizeof,
  alignof, place, copy, clone, size, address, segment, slice, reinterpret,
  pointer?, null, null?, ptr->string, string->ptr, read-array, write-array,
  find-symbol, load-library, load-system-library, cfn, defcfn, callback, and the
  type keywords with :bool included.

  Where the two differ, they differ because the substrate does:

    - A jolt pointer is a raw ADDRESS (an integer), not a sized MemorySegment.
      So read and write do not bounds-check, and a zero-size pointer is not a
      thing jolt can refuse. size answers the size jolt was TOLD — by an arena
      allocation, segment or reinterpret — and 0 for everything else, so copy
      and clone still work without an explicit count on memory jolt handed out.
      pointer? answers true for any non-negative integer, because that is all a
      pointer is here.
    - cfn/foreign-fn is a MACRO: Chez's foreign-procedure needs its types at
      compile time. Computed argtypes and a function-pointer target are not
      available, and the library-scoped 4-argument cfn raises rather than
      silently searching everything — jolt already resolves a declared native's
      symbols through its own handle (see TWO LIBRARIES above).
    - `callback` is likewise a macro, and takes jolt's :collect-safe option.
    - :& declares a variadic C function, as it does in babashka.ffi, and :varargs
      is jolt's older spelling of the same marker. The types after it are the
      tail this binding passes. babashka.ffi's BARE :& — no types after it, each
      call inferring its own tail from the values — is not available: a
      foreign-procedure's types are fixed when it is compiled, and a call has
      nothing to compile a new one from. Bind one signature per tail shape; a
      bare marker raises and says so.
    - A layout is COMPILED by the `layout` macro and read and write take that
      compiled value, where babashka.ffi takes the literal [:struct ...]
      descriptor at each call. Chez builds the ABI layout with an ftype, at
      compile time, which is also why the descriptor must be a literal —
      (layout d) on a runtime value has nothing to compile.
    - Layouts have no :union. read-array and write-array copy the one-byte
      widths in a single block move and the wider ones element by element.
    - No byte-buffer: there is no java.nio here. read-bytes, read-array and
      read-into! are the block moves in and out of jolt values.
    - jolt adds: (alloc n) with no arena, the with-* scoped helpers, arena?,
      arena-open?, close-arena, drain-auto-arenas!, layout-size,
      layout-alignment, field-offset, read-field, write-field, :varargs (a
      second spelling of :&),
      :blocking, :capture-native-error, errno, export!, foreign-callable,
      free-callable, loaded?, defining-libraries, read-bytes, write-bytes,
      read-into!, and the exact-width type aliases.")

;; The primitives this namespace is built over are the host's, reached by their
;; reserved __ names: alloc, read, write, sizeof, string->ptr, copy, read-array
;; and write-array are all DEFINED here, over the host functions of the same
;; name, so each needs a name its own definition has not taken.
;;
;; They are called rather than captured into locals. A jolt host with no FFI
;; layer has none of them, and a top-level capture would then fail to LOAD this
;; namespace instead of failing at the first foreign call — the difference
;; between a host that cannot call C and a host that cannot read a jolt program.
;; Inside a function body the reference resolves when it runs.

;; -- layouts ------------------------------------------------------------------

(defmacro layout
  "Compile a literal [:struct [[field type] ...]] descriptor into immutable ABI
  layout data. Field names are unique unqualified keywords; fields are fixed-size
  scalars, nested structs, or recursively nestable [:array type positive-count]
  descriptors. Chez supplies size, alignment, and offsets; array elements use
  integer path components, for example [:matrix 1 2]. Array metadata scales
  with the declared shape rather than the product of array dimensions."
  [descriptor]
  (list 'jolt.ffi/__layout descriptor))

(defn layout?
  "True for a value compiled by the layout macro."
  [x]
  (and (map? x) (= true (:jolt.ffi/layout x))))

(defn- checked-layout [layout]
  (when-not (layout? layout)
    (throw (ex-info "jolt.ffi: expected a compiled layout" {:layout layout})))
  layout)

(defn- checked-field-path [path]
  (let [p (if (keyword? path) [path] path)]
    (when-not (and (vector? p) (pos? (count p))
                   (keyword? (first p)) (nil? (namespace (first p)))
                   (every? #(or (and (keyword? %) (nil? (namespace %)))
                                (and (integer? %) (not (neg? %))))
                           p))
      (throw (ex-info "jolt.ffi: field path must start with an unqualified keyword and contain only unqualified keywords or non-negative array indices"
                      {:path path})))
    p))

(defn layout-size [layout] (:size (checked-layout layout)))
(defn layout-alignment [layout] (:alignment (checked-layout layout)))

(def ^:private layout-index-marker :jolt.ffi/index)

(defn- resolve-field-path [layout path]
  (let [counts (:jolt.ffi/array-counts layout)
        strides (:jolt.ffi/array-strides layout)]
    (loop [parts path compact [] delta 0]
      (if (empty? parts)
        [compact delta]
        (let [part (first parts)]
          (if (integer? part)
            (let [count (get counts compact)
                  stride (get strides compact)]
              (when-not (and count stride (< part count))
                (throw (ex-info "jolt.ffi: unknown layout field path" {:path path})))
              (recur (rest parts)
                     (conj compact layout-index-marker)
                     (+ delta (* part stride))))
            (recur (rest parts) (conj compact part) delta)))))))

(defn field-offset [layout path]
  (let [layout (checked-layout layout)
        path (checked-field-path path)
        [compact delta] (resolve-field-path layout path)
        offsets (:jolt.ffi/offsets layout)]
    (when-not (contains? offsets compact)
      (throw (ex-info "jolt.ffi: unknown layout field path" {:path path})))
    (+ (get offsets compact) delta)))

(defn- field-type [layout path]
  (let [[compact _] (resolve-field-path layout path)
        types (:jolt.ffi/types layout)]
    (when-not (contains? types compact)
      (throw (ex-info "jolt.ffi: field path names a struct or array, not a scalar field"
                      {:path path})))
    (get types compact)))

;; -- pointers -----------------------------------------------------------------
;; A jolt pointer is the raw machine ADDRESS, the same representation a void*
;; argument and result use. It carries no size of its own, so `size` answers the
;; size jolt was TOLD: an arena allocation, `segment` with a size, or
;; `reinterpret` records one here, and everything else answers 0 exactly as a
;; pointer straight out of C does. That is enough for copy and clone to work
;; without a count on memory jolt handed out, which is the reason the table
;; exists; a per-access bounds check is not on offer and would cost a lookup on
;; every read.
;;
;; An arena forgets its blocks' sizes when it closes. A `reinterpret` or
;; `segment` size given WITHOUT an arena is remembered for the life of the
;; process — the same unbounded lifetime babashka.ffi documents for that form —
;; so pass an arena when calling either in a loop.
(def ^:private known-sizes (atom {}))

(defn- remember-size! [pointer byte-count]
  (swap! known-sizes assoc pointer byte-count)
  pointer)

(defn- forget-sizes! [pointers]
  (swap! known-sizes (fn [m] (reduce dissoc m pointers))))

(defn pointer?
  "True when x could be a pointer: a non-negative integer address. A jolt
  pointer is an address and nothing more, so this cannot tell one from any other
  address-shaped integer."
  [x]
  (and (integer? x) (not (neg? x))))

(defn address
  "The native address of pointer p, as a long. A jolt pointer already IS its
  address, so this is the identity on one — it exists so code written against
  either FFI reads the same."
  [p]
  (when-not (pointer? p)
    (throw (ex-info "jolt.ffi/address: not a pointer" {:pointer p})))
  p)

(defn size
  "The size of pointer p in bytes, as far as jolt knows it: what an arena
  allocated, or what segment/reinterpret declared. 0 for a pointer jolt was
  never told the size of, including every pointer returned by C."
  [p]
  (get @known-sizes p 0))

(defn- byte-count-of
  "The byte count a size argument denotes: an integer count, a type keyword's
  width, or a compiled layout's size.

  A non-positive count is rejected here rather than at the allocator, which
  answers \"0 is not a positive fixnum\" from inside Chez and names neither the
  argument nor the caller."
  [n]
  (cond
    (integer? n)
    (do (when-not (pos? n)
          (throw (ex-info "jolt.ffi: a byte count must be positive" {:size n})))
        n)
    (layout? n) (:size n)
    (keyword? n) (jolt.ffi/__sizeof n)
    :else (throw (ex-info "jolt.ffi: expected a byte count, a type keyword, or a compiled layout"
                          {:size n}))))

(defn segment
  "A pointer to addr. With a byte count, records that size so `size`, copy and
  clone can use it.

  CAUTION: keep addr before size. Transposed, the size becomes the address and
  the first read can stop the process."
  ([addr] addr)
  ([addr byte-count] (remember-size! addr (byte-count-of byte-count))))

(defn slice
  "A pointer `offset` bytes into p. With a length — a byte count, a type
  keyword, or a compiled layout — the slice's size is recorded, so walking an
  array of structs takes the layout itself:

      (slice arr (* i (sizeof point)) point)

  CAUTION: keep offset before len. Transposed, the length becomes the offset."
  ([p offset] (+ p offset))
  ([p offset len] (remember-size! (+ p offset) (byte-count-of len))))

;; -- sizes --------------------------------------------------------------------

(defn sizeof
  "The size in bytes of a type keyword or of a compiled layout. A layout's size
  includes its padding."
  [t]
  (if (layout? t) (:size t) (jolt.ffi/__sizeof t)))

(defn alignof
  "The alignment in bytes of a type keyword or of a compiled layout. Every jolt
  scalar aligns naturally, so a type keyword's alignment is its width."
  [t]
  (if (layout? t) (:alignment t) (jolt.ffi/__sizeof t)))

;; -- arenas -------------------------------------------------------------------
;; An arena is a group of allocations with ONE lifetime. It is a map so that
;; with-open closes it through its :close entry, carrying an atom that holds the
;; group: the blocks to free, the callbacks to release, and the views whose
;; cleanup functions to run.
;;
;;   :blocks    [[usable raw] ...]  freed in reverse allocation order (LIFO, the
;;                                 order a caller built them in). `usable` is
;;                                 what the caller holds and `raw` what the
;;                                 allocator returned — they differ only for an
;;                                 over-aligned block, see arena-alloc below.
;;   :callables [addr ...]          released with free-callable
;;   :views     [[pointer f] ...]   memory jolt does NOT own: run f, free nothing
;;
;; The four kinds differ only in who may touch the arena and who closes it.
;; :confined records its creating thread and raises on use from another —
;; babashka.ffi's confined arena does the same, and the failure it prevents (two
;; threads freeing one list) has no error of its own, only a fault inside the
;; allocator. :shared is usable from any thread; the atom's swap! is what makes
;; that safe. :global and :auto cannot be closed by hand.

(def ^:private arena-marker :jolt.ffi/arena)

(defn arena?
  "True for a value from one of the arena constructors."
  [x]
  (and (map? x) (= true (get x arena-marker))))

(defn- checked-arena [a]
  (when-not (arena? a)
    (throw (ex-info "jolt.ffi: expected an arena from confined-arena, shared-arena, global-arena or auto-arena"
                    {:arena a})))
  a)

(defn arena-open?
  "True while arena a still owns its allocations. A global or automatic arena is
  always open; neither is ever closed by hand."
  [a]
  (:open? @(:jolt.ffi/state (checked-arena a))))

(defn- arena-usable! [a what]
  (let [a (checked-arena a)
        thread (:jolt.ffi/thread a)]
    (when-not (:open? @(:jolt.ffi/state a))
      (throw (ex-info (str "jolt.ffi/" what ": the arena is closed")
                      {:arena-kind (:jolt.ffi/kind a)})))
    (when (and thread (not= thread (jolt.host/thread-id)))
      (throw (ex-info (str "jolt.ffi/" what ": this arena is confined to the thread that created it")
                      {:arena-kind (:jolt.ffi/kind a)
                       :owner thread
                       :caller (jolt.host/thread-id)})))
    a))

;; Release one group: views first (their cleanup functions may still want to read
;; the memory), then callbacks, then the blocks in reverse allocation order.
;; Every step runs even if an earlier one threw — a cleanup function that raises
;; must not strand the rest of the group — and the first failure is re-thrown
;; once the group is empty.
(defn- release-group! [state]
  (let [group (first (reset-vals! state {:open? false :blocks [] :callables [] :views []}))
        failure (atom nil)
        attempt (fn [f]
                  (try (f)
                       (catch Throwable e
                         (when-not @failure (reset! failure e)))))]
    (when (:open? group)
      (doseq [view (:views group)]
        (when (second view) (attempt (fn [] ((second view) (first view))))))
      (doseq [addr (:callables group)]
        (attempt (fn [] (jolt.ffi/free-callable addr))))
      (doseq [block (reverse (:blocks group))]
        (attempt (fn [] (jolt.ffi/free (second block)))))
      (forget-sizes! (concat (map first (:blocks group))
                             (map first (:views group))))
      (when @failure (throw @failure)))
    nil))

;; Draining is what makes an automatic arena's release observable: the collector
;; hands back the state atoms it has reclaimed, and each one still names the
;; blocks to free. Called where a program shows it is still using arenas — on
;; every arena creation and every allocation into an automatic one — so an
;; automatic arena is released promptly in code that keeps allocating, and not
;; until the process ends in code that has stopped. Nothing here runs on the
;; collector's own thread, so a cleanup function is called from ordinary jolt
;; code with the whole runtime available.
(defn drain-auto-arenas!
  "Release every automatic arena the collector has reclaimed since the last
  drain. Returns the number released. jolt calls this when arenas are created or
  allocated into; call it directly to force a release at a chosen point."
  []
  (let [reclaimed (jolt.ffi/__auto-reclaimed)]
    (doseq [state reclaimed] (release-group! state))
    (count reclaimed)))

(defn- new-arena [kind thread]
  (drain-auto-arenas!)
  (let [state (atom {:open? true :blocks [] :callables [] :views []})
        arena {arena-marker true
               :jolt.ffi/kind kind
               :jolt.ffi/thread thread
               :jolt.ffi/state state
               :close (fn []
                        (when (or (= kind :global) (= kind :auto))
                          (throw (ex-info (str "jolt.ffi: a " (name kind) " arena cannot be closed")
                                          {:arena-kind kind})))
                        (when (and thread (not= thread (jolt.host/thread-id)))
                          (throw (ex-info "jolt.ffi: a confined arena closes on the thread that created it"
                                          {:owner thread :caller (jolt.host/thread-id)})))
                        (release-group! state))}]
    (when (= kind :auto) (jolt.ffi/__auto-guard! state))
    arena))

(defn confined-arena
  "An arena for ONE thread — the one calling this. Allocating in it or closing it
  from another thread raises. Create it in with-open:

      (with-open [a (ffi/confined-arena)]
        (let [pp (ffi/alloc a :pointer)] ...))

  This is the arena to reach for inside a function."
  []
  (new-arena :confined (jolt.host/thread-id)))

(defn shared-arena
  "An arena usable from any thread. Create it in with-open, or hand it to
  whatever closes it. Use it for memory that outlives the call that made it."
  []
  (new-arena :shared nil))

(def ^:private the-global-arena (atom nil))

(defn global-arena
  "The arena whose memory lives as long as the process. It cannot be closed."
  []
  (or @the-global-arena
      (do (compare-and-set! the-global-arena nil (new-arena :global nil))
          @the-global-arena)))

(defn auto-arena
  "An arena released once the collector reclaims the arena itself; it cannot be
  closed by hand. Keep it reachable for as long as C may use its pointers — the
  collector cannot see the copy of a pointer that C holds.

  This is the arena for a callback C may invoke from a thread jolt did not
  start, where no lexical scope can be the callback's lifetime. See
  drain-auto-arenas! for when the release actually happens."
  []
  (new-arena :auto nil))

(defn close-arena
  "Release every allocation, callback and registered cleanup in arena a; answers
  nil. This is what with-open calls.

  A second close releases nothing and is not an error. The group is swapped out
  of the arena in one step, so nothing can be freed twice however many times
  this is called — and making the second call raise would turn the legitimate
  early release inside a with-open body into an exception thrown from the
  `finally`, which replaces whatever the body was actually reporting. Closing a
  confined arena from another thread still raises, and so does closing a global
  or automatic arena at all: those are wrong, not redundant."
  [a]
  ((:close (checked-arena a)))
  nil)

(defn- arena-add! [a key value]
  (swap! (:jolt.ffi/state a) update key conj value)
  value)

;; -- allocation ---------------------------------------------------------------

(defn- align-of-spec
  "The alignment an allocation size argument asks for. A type or a layout aligns
  naturally; a bare byte count aligns to 16, which is what the allocator gives
  anyway and what babashka.ffi documents for the same form."
  [n]
  (cond
    (integer? n) 16
    (layout? n) (:alignment n)
    (keyword? n) (jolt.ffi/__sizeof n)
    :else (throw (ex-info "jolt.ffi: expected a byte count, a type keyword, or a compiled layout"
                          {:size n}))))

;; An alignment is a positive power of two, as it is for babashka.ffi's alloc and
;; for the C allocators underneath both. Anything else is checked here because
;; neither of the two paths below would report it: an alignment at or under the
;; allocator's own is not used at all, so a nonsense value would be silently
;; ignored, and above it the rounding would divide by whatever was passed — 0
;; faulting inside `rem`, 24 answering a multiple of 24 that is not an alignment.
(defn- checked-alignment [alignment]
  (when-not (and (integer? alignment) (pos? alignment)
                 (zero? (bit-and alignment (dec alignment))))
    (throw (ex-info "jolt.ffi: an alignment must be a positive power of two"
                    {:alignment alignment})))
  alignment)

;; An over-aligned block (alignment above the allocator's own 16) is allocated
;; align-1 bytes longer and rounded up inside itself, so the pointer the caller
;; holds is not the pointer that gets freed. That is the whole reason a block is
;; recorded as a [usable raw] pair.
(defn- arena-alloc [a byte-count alignment]
  (let [over? (> alignment 16)
        raw (jolt.ffi/__calloc (if over? (+ byte-count alignment -1) byte-count))
        remainder (if over? (rem raw alignment) 0)
        usable (if (zero? remainder) raw (+ raw (- alignment remainder)))]
    (arena-add! a :blocks [usable raw])
    (remember-size! usable byte-count)))

(defn alloc
  "Allocate ZEROED native memory and answer its pointer. `n` is a byte count, a
  type keyword, or a compiled layout.

      (alloc arena n)            ; owned by the arena, released when it closes
      (alloc arena n alignment)  ; the same, with the alignment spelled out
      (alloc n)                  ; owned by the CALLER, released with ffi/free

  A type or a layout aligns naturally, a byte count aligns to 16, and a third
  argument overrides. An alignment above the allocator's own is honoured by
  rounding up inside a larger block.

  Use a confined arena inside one function; use a shared arena for memory that
  outlives the call and is released elsewhere. The arena-less form is the one to
  reach for when the allocation lives exactly as long as one lexical scope —
  with-alloc, with-out and with-layout are that form with the free attached.

  If C allocates the memory, bind its allocator with defcfn and release the
  result with the matching C deallocator; reinterpret gives that pointer a size
  and, with an arena, a scope for its deallocator.

  CAUTION: do not close the arena while C still uses its memory. C can read
  released memory, and nothing raises."
  ([n] (jolt.ffi/__calloc (byte-count-of n)))
  ([arena n] (alloc arena n (align-of-spec n)))
  ([arena n alignment]
   (let [arena (arena-usable! arena "alloc")]
     (when (= :auto (:jolt.ffi/kind arena)) (drain-auto-arenas!))
     (arena-alloc arena (byte-count-of n) (checked-alignment alignment)))))

(defn string->ptr
  "Copy s to a NUL-terminated UTF-8 C string and answer its pointer. With an
  arena, the arena owns it; without one, the caller frees it.

  nil answers NULL and allocates nothing, which is how an optional C string
  argument is passed and what round-trips against ptr->string. Every other value
  goes through the `str` coercion, because this copies a VALUE."
  ([s] (jolt.ffi/__string->ptr s))
  ([arena s]
   (let [arena (arena-usable! arena "string->ptr")
         p (jolt.ffi/__string->ptr s)]
     (if (jolt.ffi/null? p)
       p                                        ; nil: nothing was allocated
       (do (arena-add! arena :blocks [p p])
           (remember-size! p (+ 1 (jolt.ffi/__utf8-length s))))))))

(defn copy
  "Copy bytes from pointer src to pointer dst; answers nil. Without n, copies
  src's known size (see `size`), and dst must be at least that large.

  The regions may overlap: the copy behaves as memmove. To copy into the middle
  of dst, slice it first — (copy src (slice dst 16) n)."
  ([src dst]
   (let [n (size src)]
     (when (zero? n)
       (throw (ex-info "jolt.ffi/copy: the source has no known size — pass a byte count, or declare one with reinterpret"
                       {:source src})))
     (jolt.ffi/__copy src dst n)))
  ([src dst n] (jolt.ffi/__copy src dst n)))

(defn clone
  "Allocate a copy of pointer src in arena and answer the new pointer. Without n
  the size comes from src's known size (see `size`), so a pointer straight out of
  C needs either an explicit count or a reinterpret first."
  ([arena src]
   (let [n (size src)]
     (when (zero? n)
       (throw (ex-info "jolt.ffi/clone: the source has no known size — pass a byte count, or declare one with reinterpret"
                       {:source src})))
     (clone arena src n)))
  ([arena src n]
   (let [dst (alloc arena n)]
     (jolt.ffi/__copy src dst n)
     dst)))

(defn reinterpret
  "Declare that pointer p addresses `byte-count` bytes, and answer p. This is how
  a pointer from C gets a size, which is what `size`, copy and clone read.

  With an arena, the declaration is forgotten when the arena closes, and the
  arena calls the optional cleanup function with p — the place to hand a C
  library's own deallocator. Without an arena the declaration lasts for the life
  of the process, so pass one when calling this in a loop.

  CAUTION: give the ACTUAL size. Nothing can check it, and jolt does not
  bounds-check a read; a size larger than the allocation is a read off the end.

  CAUTION: the arena frees nothing here — the memory is C's. What it releases is
  the declaration and, through the cleanup function, whatever C wants released."
  ([p byte-count] (remember-size! p (byte-count-of byte-count)))
  ([p byte-count arena] (reinterpret p byte-count arena nil))
  ([p byte-count arena cleanup]
   (let [arena (arena-usable! arena "reinterpret")]
     (arena-add! arena :views [p cleanup])
     (remember-size! p (byte-count-of byte-count)))))

;; -- reading and writing ------------------------------------------------------

(defn place?
  "True for a value from `place`."
  [x]
  (and (map? x) (= true (:jolt.ffi/place x))))

;; The sub-descriptor a compact path names, so a place at a struct or array
;; member decodes as that shape rather than only as a scalar.
(defn- descriptor-at [layout compact]
  (loop [desc (:descriptor layout) parts (seq compact)]
    (if (nil? parts)
      desc
      (let [part (first parts)]
        (when-not (vector? desc)
          (throw (ex-info "jolt.ffi: layout field path reaches past a scalar"
                          {:path compact})))
        (if (= part layout-index-marker)
          (do (when-not (= :array (nth desc 0))
                (throw (ex-info "jolt.ffi: layout field path indexes something that is not an array"
                                {:path compact})))
              (recur (nth desc 1) (next parts)))
          (do (when-not (= :struct (nth desc 0))
                (throw (ex-info "jolt.ffi: layout field path names a member of something that is not a struct"
                                {:path compact})))
              (let [field (first (filter (fn [f] (= part (nth f 0))) (nth desc 1)))]
                (when-not field
                  (throw (ex-info "jolt.ffi: unknown layout field path" {:path compact})))
                (recur (nth field 1) (next parts)))))))))

(defn place
  "One member of layout t, resolved ONCE, as a value read and write take where
  they take a type. `path` is a member name or a vector of member names and array
  indices reaching into nested layouts; with no path the place is the whole
  layout.

      (def parent (place bone :parent))
      (read p parent)                        ;=> 7
      (write p parent 3)
      (read p (place outer [:msgs 1 :data :result]))

  The member decodes and encodes as its own type, the way read and write do it: a
  struct as a map, an array as a vector. Resolving the path is the work a place
  removes, so make one once and keep it, as with a defcfn binding.

  A path that names nothing raises rather than answering nil: a layout is closed,
  so a member that is not in it is a mistake in the program."
  ([t] (place t nil))
  ([t path]
   (let [layout (checked-layout t)]
     (if (or (nil? path) (and (vector? path) (zero? (count path))))
       {:jolt.ffi/place true :jolt.ffi/layout layout
        :jolt.ffi/desc (:descriptor layout)
        :jolt.ffi/compact [] :jolt.ffi/delta 0}
       (let [parts (checked-field-path path)
             resolved (resolve-field-path layout parts)
             compact (nth resolved 0)]
         (when-not (contains? (:jolt.ffi/offsets layout) compact)
           (throw (ex-info "jolt.ffi/place: unknown layout field path" {:path path})))
         {:jolt.ffi/place true :jolt.ffi/layout layout
          :jolt.ffi/desc (descriptor-at layout compact)
          :jolt.ffi/compact compact :jolt.ffi/delta (nth resolved 1)})))))

;; A scalar's address is base + the member's own offset + the array indices
;; accumulated on the way in. The offsets and strides tables are the ones
;; field-offset reads, so a layout read and a read-field of the same member
;; resolve to the same byte.
(defn- decode-desc [p layout desc compact delta base]
  (cond
    (keyword? desc)
    (jolt.ffi/__read p desc (+ base delta (get (:jolt.ffi/offsets layout) compact)))

    (= :struct (nth desc 0))
    (reduce (fn [m field]
              (assoc m (nth field 0)
                     (decode-desc p layout (nth field 1)
                                  (conj compact (nth field 0)) delta base)))
            {}
            (nth desc 1))

    (= :array (nth desc 0))
    (let [element (nth desc 1)
          n (nth desc 2)
          stride (get (:jolt.ffi/array-strides layout) compact)
          child (conj compact layout-index-marker)]
      (mapv (fn [i] (decode-desc p layout element child (+ delta (* i stride)) base))
            (range n)))

    :else
    (throw (ex-info "jolt.ffi: unsupported layout descriptor" {:descriptor desc}))))

(defn- encode-desc [p layout desc compact delta base v]
  (cond
    (keyword? desc)
    (jolt.ffi/__write p desc v (+ base delta (get (:jolt.ffi/offsets layout) compact)))

    (= :struct (nth desc 0))
    (let [fields (nth desc 1)
          names (mapv (fn [f] (nth f 0)) fields)]
      (when-not (map? v)
        (throw (ex-info (str "jolt.ffi/write: a struct value is a map of " (pr-str names))
                        {:value v :fields names})))
      (doseq [nm names]
        (when-not (contains? v nm)
          (throw (ex-info (str "jolt.ffi/write: struct value misses field " (pr-str nm))
                          {:value v :fields names}))))
      (doseq [k (keys v)]
        (when-not (some (fn [nm] (= k nm)) names)
          (throw (ex-info (str "jolt.ffi/write: struct value has unknown field " (pr-str k))
                          {:value v :fields names}))))
      (doseq [field fields]
        (encode-desc p layout (nth field 1) (conj compact (nth field 0))
                     delta base (get v (nth field 0))))
      nil)

    (= :array (nth desc 0))
    (let [element (nth desc 1)
          n (nth desc 2)
          stride (get (:jolt.ffi/array-strides layout) compact)
          child (conj compact layout-index-marker)]
      ;; A jolt array as well as a sequence: an FFI caller often already has an
      ;; int-array or a byte-array in hand, and babashka.ffi takes a Java array
      ;; here for the same reason.
      (when-not (or (sequential? v) (array? v))
        (throw (ex-info (str "jolt.ffi/write: an array value is a sequence of " n " elements")
                        {:value v :count n})))
      (let [values (vec v)]
        (when-not (= n (count values))
          (throw (ex-info (str "jolt.ffi/write: an array value needs " n " elements, got " (count values))
                          {:value v :count n})))
        (dotimes [i n]
          (encode-desc p layout element child (+ delta (* i stride)) base (nth values i))))
      nil)

    :else
    (throw (ex-info "jolt.ffi: unsupported layout descriptor" {:descriptor desc}))))

(defn- composite-parts [t what]
  (cond
    (place? t) [(:jolt.ffi/layout t) (:jolt.ffi/desc t)
                (:jolt.ffi/compact t) (:jolt.ffi/delta t)]
    (layout? t) [t (:descriptor t) [] 0]
    :else (throw (ex-info (str "jolt.ffi/" what
                               ": the type must be a type keyword, a compiled layout, or a place")
                          {:type t}))))

(defn read
  "Read a value of type t from pointer p, at byte offset `off` (default 0).

  t is a type keyword, a compiled layout, or a place. A layout reads a struct as
  a map of its fields and an array as a vector; a place is one member of a layout
  with its path already resolved.

  A jolt pointer carries no size, so nothing here is bounds-checked: reading past
  an allocation reads whatever is there."
  ([p t] (if (keyword? t)
           (jolt.ffi/__read p t 0)
           (let [parts (composite-parts t "read")]
             (decode-desc p (nth parts 0) (nth parts 1) (nth parts 2) (nth parts 3) 0))))
  ([p t off] (if (keyword? t)
               (jolt.ffi/__read p t off)
               (let [parts (composite-parts t "read")]
                 (decode-desc p (nth parts 0) (nth parts 1) (nth parts 2) (nth parts 3) off)))))

(defn write
  "Write v as type t to pointer p, at byte offset `off` (default 0); answers nil.

  t is a type keyword, a compiled layout, or a place. A struct value is a map
  holding each of the layout's fields and no others; an array value is any
  sequence of the declared length.

  Note the argument order: the VALUE comes before the offset, as it does in
  babashka.ffi."
  ([p t v] (if (keyword? t)
             (jolt.ffi/__write p t v 0)
             (let [parts (composite-parts t "write")]
               (encode-desc p (nth parts 0) (nth parts 1) (nth parts 2) (nth parts 3) 0 v))))
  ([p t v off] (if (keyword? t)
                 (jolt.ffi/__write p t v off)
                 (let [parts (composite-parts t "write")]
                   (encode-desc p (nth parts 0) (nth parts 1) (nth parts 2) (nth parts 3) off v)))))

(defn read-field [pointer layout path]
  (let [layout (checked-layout layout)
        path (checked-field-path path)]
    (jolt.ffi/__read pointer (field-type layout path) (field-offset layout path))))

(defn write-field [pointer layout path value]
  (let [layout (checked-layout layout)
        path (checked-field-path path)]
    (jolt.ffi/__write pointer (field-type layout path) value (field-offset layout path))))

;; -- scalar array moves -------------------------------------------------------
;; A copy of one scalar type between foreign memory and a jolt array of that
;; width. The type gives the WIDTH and nothing else, so the bits land as they
;; are: :uint32 above Integer/MAX_VALUE reads back negative in an int-array,
;; exactly as a memcpy into a Java int[] does, which is what makes a round trip
;; through these byte-exact.
;;
;; The one-byte widths move as one block (read-array / write-array below the
;; dispatch, which is a single foreign block copy); the wider ones move element
;; by element, since a jolt array of 4- or 8-byte elements has no bytevector to
;; copy into. :string cannot be copied at all — its elements are pointers to
;; bytes elsewhere, which a copy cannot follow.
(def ^:private array-byte-types
  #{:int8 :i8 :uint8 :u8 :byte :char :bool})

(defn- array-carrier [t]
  (cond
    (contains? array-byte-types t) {:width 1}
    (= t :float) {:width 4 :ctor float-array :read-type :float :write-type :float :float? true}
    (= t :double) {:width 8 :ctor double-array :read-type :double :write-type :double :float? true}
    (or (= t :string) (= t :void) (not (keyword? t)))
    (throw (ex-info (str "jolt.ffi: read-array and write-array copy scalars; "
                         (if (= t :string)
                           ":string elements are pointers to bytes elsewhere, which a copy cannot follow"
                           (str "cannot copy " (pr-str t))))
                    {:type t}))
    :else
    (let [w (jolt.ffi/__sizeof t)]
      (case w
        2 {:width 2 :ctor short-array :read-type :int16 :write-type :uint16 :mask 0xffff}
        4 {:width 4 :ctor int-array :read-type :int32 :write-type :uint32 :mask 0xffffffff}
        8 {:width 8 :ctor long-array :read-type :int64 :write-type :uint64
           :signed-type :int64}
        (throw (ex-info (str "jolt.ffi: cannot copy " (pr-str t) " as an array element")
                        {:type t :width w}))))))

(defn- read-array-typed [p t n off]
  (let [carrier (array-carrier t)]
    (if (nil? (:ctor carrier))
      (jolt.ffi/__read-array (+ p off) n)
      (let [width (:width carrier)
            element (:read-type carrier)
            arr ((:ctor carrier) n)]
        (dotimes [i n] (aset arr i (jolt.ffi/__read p element (+ off (* i width)))))
        arr))))

;; One element, written so the BITS land as they are: a value wider than its
;; element folds into it rather than raising, which is the write half of "the type
;; gives the width and nothing else".
(defn- write-array-element [p carrier v off]
  (let [element (:write-type carrier)]
    (cond
      (:float? carrier) (jolt.ffi/__write p element v off)
      (:mask carrier) (jolt.ffi/__write p element (bit-and v (:mask carrier)) off)
      ;; Eight bytes has no fixnum mask to fold with — 0xffffffffffffffff is a
      ;; bignum and bit-and refuses it — so the SIGN picks the type instead. The
      ;; two cover the whole 64-bit range between them and store the same bits.
      (neg? v) (jolt.ffi/__write p (:signed-type carrier) v off)
      :else (jolt.ffi/__write p element v off))))

(defn- write-array-typed [p t arr off]
  (let [carrier (array-carrier t)]
    (if (nil? (:ctor carrier))
      (jolt.ffi/__write-array (+ p off) arr)
      (let [width (:width carrier)
            n (count arr)]
        (dotimes [i n]
          (write-array-element p carrier (nth arr i) (+ off (* i width))))
        n))))

(defn read-array
  "Copy elements out of foreign memory into a fresh jolt array.

      (read-array p n)             ; n raw octets -> a byte-array (jolt's form)
      (read-array p t n)           ; n elements of type t -> an array of its width
      (read-array p t n offset)    ; the same, starting `offset` bytes in

  The typed form answers a byte-array for the one-byte types, a short-array,
  int-array or long-array for the 2-, 4- and 8-byte integer and pointer types,
  and a float-array or double-array for :float and :double. For an array of
  structs, or for elements decoded the way read decodes them, use read with an
  [:array t n] layout instead."
  ([p n] (jolt.ffi/__read-array p n))
  ([p t n] (read-array-typed p t n 0))
  ([p t n off] (read-array-typed p t n off)))

(defn write-array
  "Copy a jolt array's elements into foreign memory; answers the element count.

      (write-array p arr)          ; every octet of a byte-array (jolt's form)
      (write-array p arr off n)    ; n octets of it, from index off
      (write-array p t arr)        ; arr's elements as type t
      (write-array p t arr offset) ; the same, starting `offset` bytes in

  The array must be the one for the type's width, as read-array answers it. A
  value wider than its element truncates to the element, since the copy carries
  bits and not a range."
  ([p arr] (jolt.ffi/__write-array p arr))
  ([p a b]
   (if (keyword? a)
     (write-array-typed p a b 0)
     (throw (ex-info "jolt.ffi/write-array: expected (write-array p t arr) or (write-array p arr off n)"
                     {:second a}))))
  ([p a b c]
   (if (keyword? a)
     (write-array-typed p a b c)
     (jolt.ffi/__write-array p a b c))))

;; -- scoped helpers -----------------------------------------------------------

(defn- helper-binding [macro-name binding]
  (when-not (and (vector? binding) (= 2 (count binding))
                 (symbol? (first binding)))
    (throw (str "jolt.ffi/" macro-name " requires [pointer value] binding")))
  binding)

(defmacro with-arena
  "Bind a confined arena for the body and close it on the way out — with-open
  with the constructor spelled in. Every allocation made in the arena is released
  when the body ends, however it ends."
  [binding & body]
  (when-not (and (vector? binding) (= 1 (count binding)) (symbol? (first binding)))
    (throw "jolt.ffi/with-arena requires a single-symbol binding, [arena]"))
  (let [arena (first binding)]
    `(let [~arena (jolt.ffi/confined-arena)]
       (try ~@body (finally (jolt.ffi/close-arena ~arena))))))

(defmacro with-alloc
  "Allocate byte-count bytes for pointer, evaluate body, and free exactly once.
  The pointer is valid only within body and must not escape the lexical scope."
  [binding & body]
  (let [[pointer byte-count] (helper-binding "with-alloc" binding)]
    `(let [~pointer (jolt.ffi/alloc ~byte-count)]
       (try ~@body (finally (jolt.ffi/free ~pointer))))))

(defmacro with-out
  "Lexically allocate one scalar value of scalar-type."
  [binding & body]
  (let [[pointer scalar-type] (helper-binding "with-out" binding)]
    `(jolt.ffi/with-alloc [~pointer (jolt.ffi/sizeof ~scalar-type)] ~@body)))

(defmacro with-layout
  "Lexically allocate one instance of a compiled layout."
  [binding & body]
  (let [[pointer layout] (helper-binding "with-layout" binding)]
    `(jolt.ffi/with-alloc [~pointer (jolt.ffi/layout-size ~layout)] ~@body)))

(defmacro with-c-string
  "Copy value to a lexical NUL-terminated UTF-8 C string. A nil value binds NULL
  rather than an empty string, which is how an optional C string argument is
  passed; freeing it is still a no-op."
  [binding & body]
  (let [[pointer value] (helper-binding "with-c-string" binding)]
    `(let [~pointer (jolt.ffi/string->ptr ~value)]
       (try ~@body (finally (jolt.ffi/free ~pointer))))))

(defmacro with-c-string-array
  "Build a lexical pointer array of count NUL-terminated UTF-8 strings. The
  values expression is evaluated once. Partially built arrays are cleaned up if
  conversion fails. Neither the array nor its member pointers may escape body.
  A nil member is a NULL entry, which is what an argv/envp-shaped array wants;
  it is tracked and freed like any other member, freeing NULL being a no-op."
  [binding values & body]
  (let [[pointer count-expr] (helper-binding "with-c-string-array" binding)]
    `(let [values# (vec ~values)
           count# ~count-expr]
       (when-not (= count# (count values#))
         (throw (ex-info "jolt.ffi: C string array count does not match values"
                         {:count count# :value-count (count values#)})))
       (let [~pointer (jolt.ffi/alloc (* count# (jolt.ffi/sizeof :pointer)))
             owned# (atom [])]
         (try
           (doseq [[index# value#] (map-indexed vector values#)]
             (let [string-pointer# (jolt.ffi/string->ptr value#)]
               (swap! owned# conj string-pointer#)
               (jolt.ffi/write ~pointer :pointer string-pointer#
                               (* index# (jolt.ffi/sizeof :pointer)))))
           ~@body
           (finally
             (doseq [string-pointer# @owned#]
               (jolt.ffi/free string-pointer#))
             (jolt.ffi/free ~pointer)))))))

;; -- foreign functions --------------------------------------------------------
;; foreign-fn binds C symbol `csym` to a typed callable. Expands to the __cfn
;; special form (always fully-qualified, so an :as alias on jolt.ffi resolves):
;; the analyzer/back end turn it into a Chez foreign-procedure.
;; An optional trailing :blocking marks a call that may block (accept/recv/...),
;; so it's emitted collect-safe and won't pin the garbage collector.
;; A :& (or :varargs — the same marker, jolt's older spelling) inside the argtype
;; vector declares a VARIADIC libc function and marks the boundary: the types
;; before it are the fixed (named) parameters, the types after it are the
;; concrete variadic arguments the binding always passes. The call is emitted
;; with the variadic calling convention (__varargs_after n, n = fixed-arg count)
;; — required on Apple arm64, which passes variadic arguments on the stack; a
;; fixed-arity binding silently corrupts them. fcntl is (int fd, int cmd, ...):
;;   (ffi/defcfn c-fcntl "fcntl" [:int :int :& :int] :int)
;; C's default argument promotions still apply after the marker: pass values
;; narrower than int as :int (and float as :double), not as an exact narrow type
;; — which includes :bool, since _Bool promotes to int in a variadic call.
;;
;; The tail belongs to the BINDING, not to the call. babashka.ffi's bare :& —
;; [:string :int :&], each call inferring its own tail — has no equivalent here:
;; a foreign-procedure's types are fixed when it is compiled. Bind one signature
;; per tail shape, which is also what keeps each of them a real typed call:
;;   (ffi/defcfn c-open "open" [:string :int :&] :int)        ; rejected, says why
;;   (ffi/defcfn c-open-mode "open" [:string :int :& :int] :int)
;; An options map may instead combine :blocking with
;; :capture-native-error. Capture returns [native-result error-code] (result
;; first), with the error slot read in the foreign-call return path. The analyzer
;; validates the literal map and rejects capture on :void.
(defn- cfn-form [csym argtypes rettype args who]
  (let [n (count args)]
    (cond
      (zero? n)
      (list 'jolt.ffi/__cfn csym argtypes rettype)

      (and (= n 1) (= (first args) :blocking))
      (list 'jolt.ffi/__cfn csym argtypes rettype :blocking)

      (and (= n 1) (map? (first args)))
      (list 'jolt.ffi/__cfn csym argtypes rettype (first args))

      :else
      (throw (ex-info (str "jolt.ffi/" who ": trailing option must be "
                           ":blocking or an options map; got " (vec args))
                      {:jolt/ffi-option args})))))

(defmacro foreign-fn [csym argtypes rettype & args]
  (cfn-form csym argtypes rettype args "foreign-fn"))

;; The babashka.ffi name for the same thing. It is a MACRO here, not a function:
;; Chez's foreign-procedure needs its argument and result types at COMPILE time,
;; so a literal signature is the whole reason the emitted binding is a real typed
;; call and not an interpreter. The consequences are worth naming, because they
;; are what a shim in either direction runs into:
;;   - argtypes must be a literal vector; a computed signature has nowhere to go.
;;   - the target must be a literal C symbol name, not a function pointer.
;;   - the 4-argument library-scoped form raises. jolt already resolves a
;;     declared native's symbols through its OWN dlopen handle, ahead of the
;;     global namespace, which is the guarantee the library argument buys in
;;     babashka.ffi — see TWO LIBRARIES, ONE COPY in the namespace docstring.
;;     Silently searching everything instead would answer the wrong library's
;;     symbol on exactly the setup the argument exists to protect.
(defmacro cfn [& args]
  (let [args (vec args)
        n (count args)]
    (when (< n 3)
      (throw (ex-info "jolt.ffi/cfn: expected (cfn csym argtypes rettype) and an optional trailing option"
                      {:args args})))
    (when-not (string? (nth args 0))
      (throw (ex-info "jolt.ffi/cfn: the first argument must be a literal C symbol name. A library-scoped (cfn lib sym argtypes rettype) has no jolt equivalent: a declared :jolt/native already resolves its own symbols through its own handle."
                      {:args args})))
    (cfn-form (nth args 0) (nth args 1) (nth args 2) (vec (drop 3 args)) "cfn")))

;; (defcfn name docstring? attr-map? "c_symbol" [argtypes] rettype
;;         [:blocking | {opts}]? [raw-name & fn-tail]?)
;;
;; The plain form defs `name` as the binding. The docstring and attribute map are
;; babashka.ffi's, and both land on the var, so a namespace of bindings documents
;; itself and ^:private still works. :library in the attribute map raises, for the
;; reason cfn's fourth argument does.
;;
;; The WRAPPER form binds the raw C function to a local name and defines `name`
;; as a jolt function over it — the shape for an out-parameter or an error code
;; that callers should never see:
;;
;;   (defcfn open-db
;;     "sqlite3_open_v2" [:string :pointer :int :string] :int
;;     open-native
;;     [filename flags]
;;     (with-open [arena (ffi/confined-arena)]
;;       (let [pdb (ffi/alloc arena :pointer)
;;             code (open-native filename pdb flags nil)]
;;         (if (zero? code)
;;           (ffi/read pdb :pointer)
;;           (throw (ex-info "open failed" {:code code}))))))
;;
;; The raw name is local to the wrapper body and does not enter the namespace.
;; The wrapper may have several arities and its own argument lists.
(defn- def-cfn-form [name docstring attrs args]
  (do
    (when (< (count args) 3)
      (throw (ex-info "jolt.ffi/defcfn: expected a C symbol, an argtype vector and a return type"
                      {:name name})))
    (when (and attrs (contains? attrs :library))
      (throw (ex-info "jolt.ffi/defcfn: :library has no jolt equivalent — a declared :jolt/native resolves its own symbols through its own handle, ahead of the global namespace"
                      {:name name})))
    (let [tail (vec (drop 3 args))
          option? (and (pos? (count tail))
                       (or (= :blocking (nth tail 0)) (map? (nth tail 0))))
          option (if option? [(nth tail 0)] [])
          tail (if option? (vec (rest tail)) tail)
          binding (cfn-form (nth args 0) (nth args 1) (nth args 2) option "defcfn")
          named (if attrs (with-meta name (merge (or (meta name) {}) attrs)) name)
          value (if (zero? (count tail))
                  binding
                  (let [raw (nth tail 0)]
                    (when-not (symbol? raw)
                      (throw (ex-info "jolt.ffi/defcfn: the wrapper form names the raw binding with a symbol after the return type"
                                      {:name name :got raw})))
                    (list 'let (vector raw binding)
                          (cons 'fn (cons name (vec (rest tail)))))))]
      (if docstring
        (list 'def named docstring value)
        (list 'def named value)))))

;;
;; A docstring and a C symbol are both strings, so the C symbol is found by the
;; shape around it rather than by counting from either end: it is the first
;; string FOLLOWED BY THE ARGTYPE VECTOR. Everything before it is the optional
;; docstring and attribute map, in that order.
(defmacro defcfn [name & args]
  (let [args (vec args)
        csym-index (first (keep-indexed
                           (fn [i x]
                             (when (and (string? x)
                                        (< (inc i) (count args))
                                        (vector? (nth args (inc i))))
                               i))
                           args))]
    (when (nil? csym-index)
      (throw (ex-info "jolt.ffi/defcfn: expected a C symbol name followed by an argtype vector and a return type"
                      {:name name})))
    (let [prefix (vec (take csym-index args))
          docstring (when (and (pos? (count prefix)) (string? (nth prefix 0))) (nth prefix 0))
          rest-prefix (if docstring (vec (rest prefix)) prefix)
          attrs (when (and (pos? (count rest-prefix)) (map? (nth rest-prefix 0)))
                  (nth rest-prefix 0))]
      (when-not (= (count rest-prefix) (if attrs 1 0))
        (throw (ex-info "jolt.ffi/defcfn: only a docstring and an attribute map may precede the C symbol"
                        {:name name :before prefix})))
      (def-cfn-form name docstring attrs (vec (drop csym-index args))))))

;; foreign-callable wraps a jolt fn `f` as a C-callable function pointer — the
;; inverse of foreign-fn, so C can call back INTO jolt (GTK signal handlers, a
;; qsort comparator, any C API that takes a callback). Returns the pointer; pass
;; it where C expects a function pointer. argtypes/rettype use the same keywords
;; as foreign-fn; the args C passes arrive as jolt values and the jolt return is
;; marshaled back. The callback stays live until free-callable is called on the
;; pointer — or, with `callback` below, until its arena closes.
;;
;; Pass a trailing :collect-safe when the callback can arrive on a thread that is
;; not an ACTIVE jolt thread at that moment. Two ways that happens: C invokes it
;; on a thread the runtime never started (a dispatch queue, a pthread the library
;; spawned), or on a jolt thread currently parked in a :blocking foreign call
;; (e.g. a GTK main loop). Either way the entry has to reactivate the thread
;; first; without :collect-safe it runs jolt code on a thread the collector does
;; not know is running, and the process dies with a nonrecoverable memory fault
;; that no handler can catch. It costs an activation per call, so leave it off
;; for a callback C only ever invokes on the thread that called into C (a qsort
;; comparator).
;;   (g-signal-connect button "clicked"
;;                     (ffi/foreign-callable on-click [:pointer :pointer] :void :collect-safe)
;;                     (ffi/null))
(defmacro foreign-callable [f argtypes rettype & [opt]]
  (if (= opt :collect-safe)
    (list 'jolt.ffi/__ccallable f argtypes rettype :collect-safe)
    (list 'jolt.ffi/__ccallable f argtypes rettype)))

(defn __arena-callback
  "Internal. Record an already-built callable entry point as arena-owned, so the
  arena releases it. `callback` expands to this around __ccallable."
  [arena addr]
  (let [arena (arena-usable! arena "callback")]
    (arena-add! arena :callables addr)))

;; The arena-owned foreign-callable: same pointer, released when the arena
;; closes, with no free-callable to remember. A macro for the same reason cfn is.
;;
;; Choose the arena for the thread that calls back. A shared arena lets C invoke
;; the callback from any thread, including one jolt never started — an event-loop
;; notification. A confined arena is the one for a callback C only invokes during
;; a call you make, such as a comparison function. An automatic arena releases
;; the pointer once the ARENA is unreachable, and the collector cannot see the
;; copy C holds, so use it only when your own reference outlives every call C can
;; make.
;;
;; CAUTION: C can call the pointer until its arena releases it and not one
;; instruction longer. Unregister the callback with the C library first.
(defmacro callback [arena f argtypes rettype & [opt]]
  (list 'jolt.ffi/__arena-callback arena
        (if (= opt :collect-safe)
          (list 'jolt.ffi/__ccallable f argtypes rettype :collect-safe)
          (list 'jolt.ffi/__ccallable f argtypes rettype))))

;; (export! name f [argtypes] rettype [:collect-safe]) — publish `f` as a
;; C-callable entry point under `name`, for `jolt build --library`. An embedder
;; resolves it via jolt_lookup("name") after jolt_library_init. Expands to a
;; register-export of a foreign-callable (the __ccallable special form), so the
;; callable is built with compile-time-typed argtypes and registered by name:
;;   (export! "add" (fn [x y] (+ x y)) [:int :int] :int)
;; The argtypes/rettype keywords are the same as foreign-fn/foreign-callable.
(defmacro export! [name f argtypes rettype & [opt]]
  (let [addr (if (= opt :collect-safe)
               (list 'jolt.ffi/__ccallable f argtypes rettype :collect-safe)
               (list 'jolt.ffi/__ccallable f argtypes rettype))]
    (list 'jolt.ffi/register-export name addr)))

;; -- errno --------------------------------------------------------------------
;; errno is a PER-THREAD slot behind a libc function on every platform jolt
;; runs on: __error on macOS, __errno_location on Linux, _errno on Windows
;; (ucrt). Each returns the calling thread's &errno, so reading through it is
;; correct under threads — and under fibers, whose syscall and errno read both
;; run on the fiber's carrier thread. A global cell would be wrong the moment
;; two threads made syscalls. The foreign-procedure form is created lazily on
;; first call, so declaring all three is safe; errno calls the live one.
;;
;; Read it IMMEDIATELY after the failing call: anything that can enter the
;; runtime between the call and the read — an allocation, a park, another FFI
;; call — may make a syscall of its own and overwrite the slot.
(defcfn c-errno-location "__errno_location" [] :pointer)
(defcfn c-error-location "__error" [] :pointer)
(defcfn c-errno-msvc "_errno" [] :pointer)
(defcfn c-strerror "strerror" [:int] :string)

(def ^:private errno-loc
  (case (System/getProperty "os.name")
    "Mac OS X" c-error-location
    "Windows"  c-errno-msvc
    c-errno-location))

(defn errno
  "The calling thread's errno, read through the platform's thread-local
  accessor. Read it immediately after the failing foreign call — any
  intervening call into the runtime can overwrite the slot."
  []
  (jolt.ffi/__read (errno-loc) :int 0))

(defn errno-message
  "strerror's description of errno code e; with no argument, of the current
  errno."
  ([] (errno-message (errno)))
  ([e] (c-strerror e)))
