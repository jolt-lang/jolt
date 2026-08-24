(ns jolt.ffi
  "Foreign-function interface for jolt libraries. A library loads a shared object
  and declares typed foreign functions, then exposes a Clojure API over them — no
  jolt built-in required.

      (require '[jolt.ffi :as ffi])
      (ffi/load-library {:darwin \"libsqlite3.0.dylib\" :linux \"libsqlite3.so.0\"})
      (ffi/defcfn sqlite3-open \"sqlite3_open\" [:string :pointer] :int)
      (let [pp (ffi/alloc (ffi/sizeof :pointer))]
        (sqlite3-open \"x.db\" pp)
        (let [db (ffi/read pp :pointer)] ...)
        (ffi/free pp))

  Types (keywords): :int :uint :long :ulong :int64 :uint64 :size_t :ssize_t
  :iptr :uptr :double :float :pointer :string :void :uint8/:u8/:byte :char,
  plus exact scalar widths :int8/:i8, :int16/:short, :uint16/:ushort, :int32,
  :uint32.
  Exact widths use native byte order for both memory and signatures. Signed and
  unsigned names at one width expose the same stored bits; wire byte order stays
  an explicit codec or htons/ntohs concern.

  :string carries NULL as nil in both directions: nil as an argument reaches C
  as a null char*, which is how APIs like setlocale spell \"query instead of
  set\", and a function returning NULL reads as nil. ffi/null stays the
  :pointer spelling of NULL and is unchanged. A foreign-callable gets the same
  translation with the directions swapped, since C is the caller there: a null
  char* C passes in arrives as nil, and returning nil hands C a null char*.

  nil is the only spelling of NULL a :string position accepts. false is
  rejected rather than sent, though Chez's own `string` type would take it as
  NULL: there is no :bool foreign type for a boolean to have come from, so one
  arriving here is a mistake — a `when` that did not fire, a predicate result —
  and silently sending it would land on the one value C reads as \"absent\".
  Every other non-string is rejected too, which the other foreign types already
  did on their own.

  A struct passed or returned by value uses the same literal descriptor as
  layout, wrapped in [:by-value descriptor]. An argument value is a non-null
  caller-owned pointer to the struct bytes. An aggregate-returning callable takes
  a non-null caller-owned destination pointer as its first Jolt argument, writes
  the C return there, and returns that pointer. Aggregate callbacks and exports
  are not supported. A fixed aggregate may precede :varargs, but aggregate
  variadic arguments and aggregate-return-plus-varargs are rejected.

  The memory/library primitives (alloc/free/read/write/sizeof/load-library/
  ptr->string/string->ptr/null/null?) are provided by the host, as are the
  buffer moves: read-bytes/write-bytes decode and encode UTF-8, read-array/
  write-array move raw octets to and from a byte-array, and read-into! fills a
  slice of an EXISTING byte-array — so a caller reading a stream whose length
  it already knows fills one buffer instead of regrowing an accumulator per
  chunk. All of them move the block in one copy, not a byte at a time.

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

  foreign-fn lowers
  a compile-time-typed signature to a real Chez foreign-procedure. foreign-callable
  is the inverse — it wraps a jolt fn as a C-callable function pointer so C can
  call back into jolt (e.g. GTK signal handlers); free-callable releases it.")

(defmacro layout
  "Compile a literal [:struct [[field type] ...]] descriptor into immutable ABI
  layout data. Field names are unique unqualified keywords; fields are fixed-size
  scalars or nested structs. Chez supplies size, alignment, and offsets."
  [descriptor]
  (list 'jolt.ffi/__layout descriptor))

(defn- checked-layout [layout]
  (when-not (and (map? layout) (= true (:jolt.ffi/layout layout)))
    (throw (ex-info "jolt.ffi: expected a compiled layout" {:layout layout})))
  layout)

(defn- checked-field-path [path]
  (let [p (if (keyword? path) [path] path)]
    (when-not (and (vector? p) (pos? (count p))
                   (every? #(and (keyword? %) (nil? (namespace %))) p))
      (throw (ex-info "jolt.ffi: field path must be an unqualified keyword or non-empty vector of them"
                      {:path path})))
    p))

(defn layout-size [layout] (:size (checked-layout layout)))
(defn layout-alignment [layout] (:alignment (checked-layout layout)))

(defn field-offset [layout path]
  (let [layout (checked-layout layout)
        path (checked-field-path path)
        offsets (:jolt.ffi/offsets layout)]
    (when-not (contains? offsets path)
      (throw (ex-info "jolt.ffi: unknown layout field path" {:path path})))
    (get offsets path)))

(defn- field-type [layout path]
  (let [types (:jolt.ffi/types layout)]
    (when-not (contains? types path)
      (throw (ex-info "jolt.ffi: field path names a struct, not a scalar field"
                      {:path path})))
    (get types path)))

(defn read-field [pointer layout path]
  (let [layout (checked-layout layout)
        path (checked-field-path path)]
    (jolt.ffi/read pointer (field-type layout path) (field-offset layout path))))

(defn write-field [pointer layout path value]
  (let [layout (checked-layout layout)
        path (checked-field-path path)]
    (jolt.ffi/write pointer (field-type layout path) (field-offset layout path) value)))

(defn- helper-binding [macro-name binding]
  (when-not (and (vector? binding) (= 2 (count binding))
                 (symbol? (first binding)))
    (throw (str "jolt.ffi/" macro-name " requires [pointer value] binding")))
  binding)

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
               (jolt.ffi/write ~pointer :pointer
                               (* index# (jolt.ffi/sizeof :pointer))
                               string-pointer#)))
           ~@body
           (finally
             (doseq [string-pointer# @owned#]
               (jolt.ffi/free string-pointer#))
             (jolt.ffi/free ~pointer)))))))

;; foreign-fn binds C symbol `csym` to a typed callable. Expands to the __cfn
;; special form (always fully-qualified, so an :as alias on jolt.ffi resolves):
;; the analyzer/back end turn it into a Chez foreign-procedure.
;; An optional trailing :blocking marks a call that may block (accept/recv/...),
;; so it's emitted collect-safe and won't pin the garbage collector.
;; A :varargs marker inside the argtype vector declares a VARIADIC libc
;; function and marks the boundary: the types before :varargs are the fixed
;; (named) parameters, the types after it are the concrete variadic arguments
;; the binding always passes. The call is emitted with the variadic calling
;; convention (__varargs_after n, n = fixed-arg count) — required on Apple
;; arm64, which passes variadic arguments on the stack; a fixed-arity binding
;; silently corrupts them. fcntl is (int fd, int cmd, ...), so:
;;   (ffi/defcfn c-fcntl "fcntl" [:int :int :varargs :int] :int)
;; C's default argument promotions still apply after the marker: pass values
;; narrower than int as :int (and float as :double), not as an exact narrow type.
(defmacro foreign-fn [csym argtypes rettype & [opt]]
  (if (= opt :blocking)
    (list 'jolt.ffi/__cfn csym argtypes rettype :blocking)
    (list 'jolt.ffi/__cfn csym argtypes rettype)))

;; (defcfn name "c_symbol" [argtypes] rettype [:blocking]) — def a foreign function.
(defmacro defcfn [name csym argtypes rettype & [opt]]
  (list 'def name (if (= opt :blocking)
                    (list 'jolt.ffi/__cfn csym argtypes rettype :blocking)
                    (list 'jolt.ffi/__cfn csym argtypes rettype))))

;; foreign-callable wraps a jolt fn `f` as a C-callable function pointer — the
;; inverse of foreign-fn, so C can call back INTO jolt (GTK signal handlers, a
;; qsort comparator, any C API that takes a callback). Returns the pointer; pass
;; it where C expects a function pointer. argtypes/rettype use the same keywords
;; as foreign-fn; the args C passes arrive as jolt values and the jolt return is
;; marshaled back. The callback stays live until free-callable is called on the
;; pointer. Pass a trailing :collect-safe when C invokes the callback from a
;; thread parked in a :blocking foreign call (e.g. a GTK main loop):
;;   (g-signal-connect button "clicked"
;;                     (ffi/foreign-callable on-click [:pointer :pointer] :void :collect-safe)
;;                     (ffi/null))
(defmacro foreign-callable [f argtypes rettype & [opt]]
  (if (= opt :collect-safe)
    (list 'jolt.ffi/__ccallable f argtypes rettype :collect-safe)
    (list 'jolt.ffi/__ccallable f argtypes rettype)))

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
  (jolt.ffi/read (errno-loc) :int 0))

(defn errno-message
  "strerror's description of errno code e; with no argument, of the current
  errno."
  ([] (errno-message (errno)))
  ([e] (c-strerror e)))
