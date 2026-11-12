(ns jolt.passes.types.records
  "Record struct types for jolt.passes.types: a record's struct TYPE built from its
  shape-registry entry and declared field hints, the inferred-field-type box an
  untagged field reads, and the ctor-site field-join bookkeeping the
  whole-program fixpoint folds into per-record field types. Pure over the
  lattice — no inference walk — so jolt.passes.types and the narrowing rules
  (jolt.passes.types.narrow) both build on it."
  (:require [jolt.passes.types.lattice :refer
             [struct-type? mk-struct join nilable? decl]]))

;; inferred record field types: ctor-key "ns/->Name" -> {field-kw -> type}. Each
;; field's type is the CLOSED join of its ctor-argument types across every reachable
;; (->Ctor ...) site, derived by wp-infer! A field every ctor site fills with a
;; flonum -> :double (reads unbox); a record-or-nil field -> nilable record (guarded
;; reads narrow to the direct accessor); a conflicting/escaping/mutable field is
;; absent here (reads :any). Consulted by record-type-from-entry for UNTAGGED fields.
;; The field-types atom the walk reads for an UNTAGGED record field. Bound per
;; read context to the unit's lean :field-types (the default) or its rich
;; :rich-field-types (while contagion-specialize-arity runs). field-type-from-tag
;; reads it; unbound (nil) it reads no inferred types, so a missed binding site
;; only widens a field to :any (a perf, not a correctness, effect).
(def ^:dynamic *field-type-box* nil)

;; simple record name from a ctor-key "ns/->Name".
(defn- ctor-simple-name [ctor-key]
  (let [i (.indexOf ^String ctor-key "/->")]
    (if (>= i 0) (.substring ^String ctor-key (+ i 3)) ctor-key)))
;; defrecord emits a map->Name fn whose body is the (->Name (get m :f) ...) site —
;; the untyped-map ctor path. Its positional ->Name call is excluded from the direct
;; join (a map->Name call demotes the record instead, via wp-field-demote-box), so a
;; never-called map->Name can't poison every field to :any.
(defn map->-self-for-record? [self-name ctor-key]
  (and self-name (.startsWith ^String self-name "map->")
       (= (.substring ^String self-name 5) (ctor-simple-name ctor-key))))
;; join one ctor site's positional arg types into the per-pass accumulator.
(defn join-ctor-args [joins ctor-key fields argtypes]
  (let [cur (or (get joins ctor-key) (vec (repeat (count fields) nil)))]
    (assoc joins ctor-key
           (vec (for [i (range (count cur))]
                  (let [c (nth cur i) a (nth argtypes i nil)]
                    (cond (nil? c) a (nil? a) c :else (join c a))))))))

;; build a record's struct TYPE from its registry entry, resolving each field's
;; declared type hint against `shapes` ("ns/->Name" -> entry). A field tagged with
;; a record type (its ctor-key) recurses, so a Vec3 stored in a Ray field reads
;; back as Vec3 — not :any — which is what lets nested-record code prove its reads.
;; Depth-bounded so a self/cyclic-referencing record type can't loop.
(declare record-type-from-entry)
;; the type a field reads back as. A declared coercible ^double hint always wins
;; (the ctor coerces the arg to a flonum). A declared ^Record hint recurses. An
;; UNTAGGED field takes the inferred ctor-arg join from the unit lean field types (or :any).
;;
;; The scalar tags arrive already normalized by chez-resolve-field-tag
;; (host/chez/records.ss): "double", "num" (which is also where ^long lands),
;; "str" and "kw". A tag that is none of those is a record type name to recurse
;; into. The point of "str"/"kw" is that a field read now carries its declared
;; type to the interop path — (.length (:nm p)) on a ^String field emits the
;; string native with no runtime string? test. Declared, not proven: the
;; constructor doesn't check the value, so nothing else trusts it (lattice decl).
(defn- field-type-from-tag [tag depth shapes ctor-key field-kw]
  (cond
    (<= depth 0) :any
    (= tag "double") :double
    (= tag "num") :num
    (= tag "str") (decl :str)
    (= tag "kw") (decl :kw)
    (some? tag) (let [e (get shapes tag)]
                  (if e (record-type-from-entry e tag depth shapes) :any))
    :else (let [box *field-type-box*
                inf (when box (get-in @box [ctor-key field-kw]))]
            (if inf inf :any))))
(defn record-type-from-entry [rs ctor-key depth shapes]
  (let [fields (get rs :fields)
        tags (get rs :tags)
        fmap (reduce (fn [m i]
                       (let [fw (nth fields i)]
                         (assoc m fw (field-type-from-tag (when tags (nth tags i))
                                                           (dec depth) shapes ctor-key fw))))
                     {} (range (count fields)))]
    (assoc (mk-struct fmap) :shape (vec fields) :type (get rs :type))))

;; a struct type carrying a defrecord/deftype :type tag
(defn record-t? [t] (and (struct-type? t) (some? (get t :type))))

;; fold a pass's positional ctor-arg joins into a {ctor-key {field-kw type}} map,
;; dropping demoted/escaped records (their fields read :any) and :any/conflicting
;; fields (the sound default — a conflicting join unboxes nothing). Field types are
;; projected to a SHALLOW form (type + shape + nilable, no nested :struct) — a deep
;; nilable-Node would embed Node (which re-embeds nilable-Node), deepening each
;; fixpoint round and never value-equaling, so the field-type loop wouldn't converge.
;; The shallow form is stable and all the read/mark machinery needs (it keys off
;; :type/:shape/:nilable, not nested fields — a nilable struct's subfields read :any).
(defn shallow-field-type [t]
  (cond
    ;; Option B: only a genuine all-flonum ctor-arg join unboxes. A :num (integer/
    ;; mixed) join reads :any, so flonum arithmetic over it stays generic — dbl
    ;; contagion is reserved for :double fields (the all-flonum->:double spec).
    (= t :double) t
    (struct-type? t) (let [base {:type (get t :type) :struct {} :shape (get t :shape)}]
                       (if (nilable? t) (assoc base :nilable true) base))
    :else :any))
;; the rich variant keeps :num, so a specialized clone's :num field reads type :num
;; and dbl-arith? contagions them beside a proven :double operand (the invariant).
;; Feeds the rich field types only; never the shared path.
(defn shallow-field-type-rich [t]
  (cond
    (or (= t :double) (= t :num)) t
    (struct-type? t) (let [base {:type (get t :type) :struct {} :shape (get t :shape)}]
                       (if (nilable? t) (assoc base :nilable true) base))
    :else :any))
(defn derive-field-types [unit joins demoted escaped shallow]
  (reduce-kv
    (fn [m ctor-key argts]
      (if (or (contains? demoted ctor-key) (contains? escaped ctor-key))
        m
        (let [fields (get-in @(:config unit) [:record-shapes ctor-key :fields])]
          (reduce (fn [m2 i]
                    (let [fw (nth fields i nil) t (nth argts i nil)]
                      (if (and fw t (not= t :any))
                        (assoc-in m2 [ctor-key fw] (shallow t)) m2)))
                  m (range (count argts))))))
    {} joins))
