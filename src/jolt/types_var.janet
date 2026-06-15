# Jolt value layer — Var
# Extracted from types.janet (jolt-bvek phase 5a split).

(use ./types_symbols)
# ============================================================
# Var
# ============================================================

# Dynamic-var binding stack. Stored fiber-locally (via Janet's dyn), so that
# concurrent go blocks — each a Janet fiber — don't interleave each other's
# dynamic bindings, and a go block conveys the bindings in effect when it was
# spawned (see snapshot-bindings/install-bindings). Each fiber lazily gets its
# own array on first use.
(defn cur-binding-stack []
  (or (dyn :jolt/binding-stack)
      (let [s @[]] (setdyn :jolt/binding-stack s) s)))

(defn push-thread-bindings
  "Push a frame of dynamic var bindings. Takes a struct of var→value."
  [bindings]
  (array/push (cur-binding-stack) bindings))

(defn pop-thread-bindings
  "Pop the most recent frame of dynamic var bindings."
  []
  (array/pop (cur-binding-stack)))

(defn snapshot-bindings
  "Shallow copy of the current binding stack (frames are immutable value maps).
  Captured by a go block at spawn time for binding conveyance."
  []
  (array/slice (cur-binding-stack)))

(defn install-bindings
  "Install a snapshot as this fiber's binding stack (a fresh copy, so the
  fiber's own push/pop/var-set don't mutate the snapshot's frames array)."
  [snap]
  (setdyn :jolt/binding-stack (array/slice snap)))

(defn make-var
  "Create a new Jolt Var.
  (make-var name)           — unbound var
  (make-var name init-val)  — var with root binding
  (make-var name init-val meta) — var with root and metadata
  
  name is a symbol struct {:jolt/type :symbol ...}"
  [name &opt init-val meta]
  (default init-val nil)
  (default meta nil)
  (let [m (if meta meta {:name name})
        result @{:jolt/type :jolt/var
                 :name name
                 :root init-val
                 :meta m
                 # Generation: bumped on every root change (redefinition). Call
                 # sites / dispatch caches keyed on this can detect a redef and
                 # invalidate; direct-linked (sealed) sites can detect staleness.
                 :gen 0
                 :dynamic (if meta (get meta :dynamic) false)
                 :macro (if meta (get meta :macro) false)
                 :ns (if meta (get meta :ns) nil)}]
    result))

(defn var?
  "Check if x is a Jolt Var."
  [x]
  (and (table? x) (= :jolt/var (x :jolt/type))))

(defn var-dynamic?
  "Check if var is marked :dynamic."
  [v]
  (v :dynamic))

(defn var-macro?
  "Check if var is marked :macro."
  [v]
  (v :macro))

(defn var-name
  "Return the symbol name of the var."
  [v]
  (v :name))

(defn var-meta
  "Return the metadata of the var."
  [v]
  (v :meta))

(defn var-ns
  "Return the namespace of the var."
  [v]
  (v :ns))

(defn var-get
  "Deref the var. If the var is dynamic and has a thread-local binding, return that.
  Otherwise return the root binding."
  [v]
  # Fast path: no dynamic bindings are active (the common case — the stack is
  # only non-empty inside a `binding` block), so the value is just the root. This
  # is the hot path for every global deref; skip building the walk otherwise.
  (def bs (cur-binding-stack))
  (if (= 0 (length bs))
    (v :root)
    # walk binding stack top-down for this var
    (do
      (var result nil)
      (var i (dec (length bs)))
      (while (>= i 0)
        (let [frame (in bs i)
              val (get frame v)]
          (if (not (nil? val))
            (do
              (set result (if (var? val) (var-get val) val))
              (set i -1))
            (-- i))))
      (if (not (nil? result)) result (v :root)))))

(defn var-set
  "Set a var's value. If the var has a thread-local binding on the stack, update
  the innermost frame that binds it (matching Clojure, where var-set targets the
  current binding); otherwise set the root."
  [v val]
  (def bs (cur-binding-stack))
  (var i (dec (length bs)))
  (var done false)
  (while (and (not done) (>= i 0))
    (let [frame (in bs i)]
      (if (not (nil? (get frame v)))
        (do (put bs i (merge frame {v val})) (set done true))
        (-- i))))
  (unless done (do (put v :root val) (put v :gen (+ 1 (or (v :gen) 0)))))
  val)

(defn alter-var-root
  "Atomically alter the root binding of v by applying f to current value plus args."
  [v f & args]
  (let [new-val (f (v :root) ;args)]
    (put v :root new-val)
    (put v :gen (+ 1 (or (v :gen) 0)))
    new-val))

(defn alter-meta!
  "Atomically update a var's metadata via (apply f args)."
  [v f & args]
  (let [new-meta (apply f (var-meta v) args)]
    (put v :meta new-meta)
    new-meta))

(defn reset-meta!
  "Reset a var's metadata to the given value."
  [v meta]
  (put v :meta meta)
  meta)


(defn with-meta
  "Return a new var with updated metadata. The original var is unchanged."
  [v meta]
  # build new meta as a table first (to allow adding keys), then convert
  (let [new-meta-table (merge @{} (v :meta) meta)
        # convert to struct by extracting all keys
        new-meta (table/to-struct new-meta-table)]
    @{:jolt/type :jolt/var
      :name (v :name)
      :root (v :root)
      :meta new-meta
      :gen (or (v :gen) 0)
      :dynamic (v :dynamic)
      :macro (v :macro)
      :ns (v :ns)}))

(defn bind-root
  "Set the root binding and bump the var's generation (the redefinition
  chokepoint: def, ns-intern-with-val, and the root-set paths all route here)."
  [v val]
  (put v :root val)
  (put v :gen (+ 1 (or (v :gen) 0)))
  val)

