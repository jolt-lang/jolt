# Jolt Core Library
# Clojure-compatible core functions for the Jolt interpreter.
#
# This file is the AGGREGATOR (jolt-nma8, phase 2b): the implementation now lives
# in cohesive cluster modules, loaded here in dependency order and re-exported
# (:export true) so every consumer keeps a single `(use ./core)`. The bottom of
# this file owns the registration table (core-bindings) and init-core!, which
# reference fns from every cluster.

(use ./types)
(use ./phm)
(use ./phs)
(use ./lazyseq)
(use ./regex)
(use ./config)
(use ./pv)
(use ./plist)

# Cluster modules, in load order (each uses the ones before it). :export true
# re-exports each module's OWN defs through core, so `(use ./core)` sees them all.
(import ./core_types :prefix "" :export true)
(import ./core_coll :prefix "" :export true)
(import ./core_print :prefix "" :export true)
(import ./core_io :prefix "" :export true)
(import ./core_refs :prefix "" :export true)
(import ./core_extra :prefix "" :export true)

(def- core-bindings
  "Map of symbol name → function for all core functions."
  @{"nil?" core-nil?
    "string?" core-string?
    "number?" core-number?
    "fn?" core-fn?
    "keyword?" core-keyword?
    "symbol?" core-symbol?
    "vector?" core-vector?
    "map?" core-map?
    "seq?" core-seq?
    "coll?" core-coll?
    "identical?" core-identical?
    "integer?" core-integer?
    "list?" core-list?
    "+" core-+
    "-" core-sub
    "*" core-*
    "/" core-/
    "inc" core-inc
    "dec" core-dec
    "even?" core-even?
    "odd?" core-odd?
    "mod" core-mod
    "rem" core-rem
    "quot" core-quot
    "rand" core-rand
    "=" core-=
    "<" core-<
    ">" core->
    "<=" core-<=
    ">=" core->=
    "conj" core-conj
    "assoc" core-assoc
    "dissoc" core-dissoc
    "get" core-get
    "contains?" core-contains?
    "count" core-count
    "format" core-format
    "first" core-first
    "rest" core-rest
    "next" core-next
    "cons" core-cons
    "seq" core-seq
    "vec" core-vec
    "__sq1" core-sq1
    "__sqcat" core-sqcat
    "__sqvec" core-sqvec
    "__sqmap" core-sqmap
    "__sqset" core-sqset
    "into" core-into
    "with-meta" core-with-meta
    "map" core-map
    "filter" core-filter
    "remove" core-remove
    "reduce" core-reduce
    "apply" core-apply
    "map-entry?" core-map-entry?
    "future-call" core-future-call
    "future?" core-future?
    "future-cancel" core-future-cancel
    "tagged-literal" core-tagged-literal
    "re-groups" core-re-groups
    "transient" core-transient
    "transient?" core-transient?
    "persistent!" core-persistent!
    "conj!" core-conj!
    "assoc!" core-assoc!
    "dissoc!" core-dissoc!
    "pop!" core-pop!
    "hash-combine" core-hash-combine
    "hash-ordered-coll" core-hash-ordered-coll
    "hash-unordered-coll" core-hash-unordered-coll
    "gensym" gensym
    "__write" core-write
    "__eprint" core-eprint
    "__eprintf" core-eprintf
    "__jdbc-wrap-conn" core-jdbc-wrap-conn
    "__jdbc-conn-raw" core-jdbc-conn-raw
    "__jdbc-make-stmt" core-jdbc-make-stmt
    "__make-file" core-make-file
    "__file?" core-file?
    "__pr-str1" core-pr-str1
    "__make-uuid" make-uuid
    "compare" core-compare
    "type" core-type
    "slurp" core-slurp
    "spit" core-spit
    "flush" core-flush
    "get-thread-bindings" core-get-thread-bindings
    "__thread-bound?" core-thread-bound?*
    "__dir?" core-dir?
    "__list-dir" core-list-dir
    "parse-long" core-parse-long
    "parse-double" core-parse-double
    "current-time-ms" core-current-time-ms
    "mapcat" core-mapcat
    "sequence" core-sequence
    "keyword" core-keyword
    "symbol" core-symbol
    "namespace" core-namespace
    "reduced" core-reduced
    "reduced?" core-reduced?
    "rseq" core-rseq
    "ex-info" core-ex-info
    "__with-out-str" core-with-out-str
    "delay?" core-delay?
    "make-delay" core-make-delay
    "take" core-take
    "drop" core-drop
    "take-while" core-take-while
    "drop-while" core-drop-while
    "concat" core-concat
    "nth" core-nth
    "sort" core-sort
    "partition" core-partition
    "range" core-range
    "vector" core-vector
    "hash-map" core-hash-map
    "array-map" core-array-map
    "hash-set" core-hash-set
    "set" core-set
    "list" core-list
    "set?" core-set?
    "disj" core-disj
    "coll->cells" coll->cells
    "make-lazy-seq" make-lazy-seq
    "lazy-cons" lazy-cons
    "lazy-from" lazy-from
    "str" core-str
    "name" core-name
    "subs" core-subs
    "str-trim" string/trim
    "str-upper" string/ascii-upper
    "str-lower" string/ascii-lower
    "str-find" string/find
    "str-replace" core-str-replace-first
    "str-replace-all" core-str-replace-all
    "str-reverse-b" string/reverse
    "str-join" core-str-join
    "str-split" core-str-split
    "re-pattern" re-pattern
    "re-find" re-find
    "re-matches" re-matches
    "re-seq" re-seq
    "regex?" regex?
    "str-triml" string/triml
    "str-trimr" string/trimr
    # Java-style arrays (buffers for bytes, arrays otherwise)
    "aclone" core-aclone
    "object-array" core-object-array
    "int-array" core-int-array
    "long-array" core-long-array
    "short-array" core-short-array
    "double-array" core-double-array
    "float-array" core-float-array
    "char-array" core-char-array
    "boolean-array" core-boolean-array
    "byte-array" core-byte-array
    "aset-byte" core-aset-byte
    "aset-int" core-aset-int
    "aset-long" core-aset-long
    "aset-short" core-aset-short
    "aset-double" core-aset-double
    "aset-float" core-aset-float
    "aset-char" core-aset-char
    "aset-boolean" core-aset-boolean
    "make-array" core-make-array
    "into-array" core-into-array
    "to-array" core-to-array
    "bytes" core-bytes
    "booleans" core-booleans
    "ints" core-ints
    "longs" core-longs
    "shorts" core-shorts
    "doubles" core-doubles
    "floats" core-floats
    "chars" core-chars
    "byte" core-byte
    "short" core-short
    "bigint" core-bigint
    "biginteger" core-biginteger
    "chunk-buffer" core-chunk-buffer
    "chunk-append" core-chunk-append
    "chunk" core-chunk
    "chunk-first" core-chunk-first
    "chunk-rest" core-chunk-rest
    "chunk-next" core-chunk-next
    "chunk-cons" core-chunk-cons
    "boolean" core-boolean
    "cat" core-cat
    "disj!" core-disj!
    "reader-conditional" core-reader-conditional
    "class" core-class
    "re-matcher" core-re-matcher
    # Bit operations
    "__bit-and" core-bit-and
    "__bit-or" core-bit-or
    "__bit-xor" core-bit-xor
    "bit-not" core-bit-not
    "bit-shift-left" core-bit-shift-left
    "bit-shift-right" core-bit-shift-right
    "bit-clear" core-bit-clear
    "bit-set" core-bit-set
    "bit-flip" core-bit-flip
    "bit-test" core-bit-test
    "__bit-and-not" core-bit-and-not
    "unsigned-bit-shift-right" core-unsigned-bit-shift-right
    # Integer coercion / unchecked math
    "int" core-int
    "long" core-long
    "double" core-double
    "float" core-float
    "char" core-char
    # Hash
    "hash" core-hash
    "atom" core-atom
    "deref" core-deref
    "reset!" core-reset!
    "swap!" core-swap!
    "not" core-not
    "Object" core-Object
    "make-protocol" core-make-protocol
    # satisfies?/resolve are interned by install-stateful-fns! (ctx-capturing);
    # type->str was an inert SCI stub with no callers.
    "implements?" core-implements?
    "volatile!" core-volatile!
    "Thread" core-Thread
    "ThreadLocal" core-ThreadLocal
    "IllegalStateException" core-IllegalStateException
    "copy-core-var" core-copy-core-var
    "copy-var" core-copy-var
    "macrofy" core-macrofy
    "new-var" core-new-var
    "__local-var" core-local-var
    "__close" core-close-resource
    "avoid-method-too-large" core-avoid-method-too-large
    "bytes?" core-bytes?
    "meta" core-meta
    "var-get" core-var-get
    "var-set" core-var-set
    "var?" core-var?
    "var-dynamic?" core-var-dynamic?
    "alter-var-root" core-alter-var-root
    "alter-meta!" core-alter-meta!
    "reset-meta!" core-reset-meta!
    "push-thread-bindings" core-push-thread-bindings
    "pop-thread-bindings" core-pop-thread-bindings
    # Dynamic vars — stubs for SCI bootstrap
    "*unchecked-math*" false
    "*clojure-version*" @{:major 1 :minor 11 :incremental 0 :qualifier nil}
    "*1" :jolt/nil-sentinel
    "*2" :jolt/nil-sentinel
    "*3" :jolt/nil-sentinel
    "*e" :jolt/nil-sentinel
    "*assert" true})

(defn core-macro-names
  "Set of core binding names that are macros. Empty now that every core macro
  lives in the Clojure overlay (clojure.core.*-syntax / *-macros tiers)."
  []
  @{})

# Wire the print-method callback once the overlay (and its print-method
# multimethod) exists: the renderer's record fallthrough consults the methods
# table on the var; only a USER-registered method fires — the multimethod's
# :default would bounce straight back into the renderer.
(defn install-print-method-cb! [ctx]
  (def core-ns (ctx-find-ns ctx "clojure.core"))
  (def pm-var (ns-find core-ns "print-method"))
  (when pm-var
    (set-print-method-cb!
      (fn [v emit]
        (def methods (get pm-var :jolt/methods))
        (when methods
          (def mt (core-meta v))
          (def t (and mt (core-get mt :type)))
          (def dval (if (keyword? t) t (core-type v)))
          (def m (get methods dval))
          (when m
            (m v @{:jolt/type :jolt/writer :sink emit})
            true)))))
  # A record/deftype's own Object/toString (jolt-rt6n): str routes records here
  # so a deftype with (toString [_] ...) renders via it instead of the data repr.
  (set-record-tostring-cb!
    (fn [v]
      (def tag (record-tag v))
      (when tag
        (def m (find-method-any-protocol ctx tag "toString"))
        (when m (m v))))))

(def init-core!
  (fn [& args]
    (case (length args)
      1 (let [ctx (args 0)
               ns (ctx-find-ns ctx "clojure.core")]
           (loop [[name fn] :pairs core-bindings]
             (def v (ns-intern ns name (if (= fn :jolt/nil-sentinel) nil fn)))
             (when (get (core-macro-names) name)
               (put v :macro true)))
           ns)
       2 (let [ctx (args 0) ns-name (args 1)
               ns (ctx-find-ns ctx ns-name)]
           (loop [[name fn] :pairs core-bindings]
             (def v (ns-intern ns name (if (= fn :jolt/nil-sentinel) nil fn)))
            (when (get (core-macro-names) name)
              (put v :macro true)))
          ns)
       (error "Wrong number of args passed to: init-core!"))))
