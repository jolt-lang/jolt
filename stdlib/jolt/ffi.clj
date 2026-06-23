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
  :iptr :uptr :double :float :pointer :string :void :uint8 :char.

  The memory/library primitives (alloc/free/read/write/sizeof/load-library/
  ptr->string/string->ptr/null/null?) are provided by the host. foreign-fn lowers
  a compile-time-typed signature to a real Chez foreign-procedure. foreign-callable
  is the inverse — it wraps a jolt fn as a C-callable function pointer so C can
  call back into jolt (e.g. GTK signal handlers); free-callable releases it.")

;; foreign-fn binds C symbol `csym` to a typed callable. Expands to the __cfn
;; special form (always fully-qualified, so an :as alias on jolt.ffi resolves):
;; the analyzer/back end turn it into a Chez foreign-procedure.
;; An optional trailing :blocking marks a call that may block (accept/recv/...),
;; so it's emitted collect-safe and won't pin the garbage collector.
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
