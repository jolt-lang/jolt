(ns jolt.backend-scheme.literals
  "Literal data for the Scheme back end: a constant scalar as the Scheme literal
  that reads back to it, and quoted data — raw reader forms and plain jolt
  values alike — as the constructor calls that rebuild it, hash-consed when a
  quote pool is bound."
  (:require [clojure.string :as str]
            [jolt.host
             :refer [form-sym? form-sym-name form-sym-ns form-sym-meta form-list?
                     form-vec? form-map? form-set? form-char? form-literal?
                     form-elements form-vec-items form-map-pairs form-set-items
                     form-char-code form-regex? form-regex-source form-inst?
                     form-inst-source form-uuid? form-uuid-source form-bigdec?
                     form-bigdec-source form-class-value? form-class-value-name]]
            [jolt.backend-scheme.names :refer [chez-str-lit]]
            [jolt.backend-scheme.hoist :refer [hoist-const]]))

(defn emit-const [v]
  (cond
    (nil? v) "jolt-nil"
    (boolean? v) (if v "#t" "#f")
    ;; Numeric tower: emit a literal Chez re-reads as the SAME number, via the
    ;; host's own writer (jolt.host/chez-number-literal) — NOT jolt's str, whose
    ;; rendering follows the reference printer (bigint N suffix, E exponents)
    ;; and is not Chez-readable. ##Inf/##-Inf/##NaN -> Chez's flonum literals.
    (number? v) (cond
                  (= v ##Inf) "+inf.0"
                  (= v ##-Inf) "-inf.0"
                  (not= v v) "+nan.0"
                  :else (jolt.host/chez-number-literal v))
    (string? v) (chez-str-lit v)
    ;; keyword literal -> (keyword ns name), hoisted to a per-def constant so the
    ;; intern lookup runs once per def rather than at every use site — a keyword
    ;; in a hot path ((:left node), a cond's :else, a map key) was re-interned on
    ;; every evaluation, ~25% of the cost of a keyword-keyed lookup.
    (keyword? v) (hoist-const
                   (if-let [kns (namespace v)]
                     (str "(keyword " (chez-str-lit kns) " " (chez-str-lit (name v)) ")")
                     (str "(keyword #f " (chez-str-lit (name v)) ")")))
    ;; char literal -> (integer->char <codepoint>). Get the codepoint via the host
    ;; contract (form-char-code), NOT (get v :ch): on Chez (the self-hosted spine)
    ;; a char is a native char, so a struct-field read returns nil and would emit
    ;; (integer->char) with no arg.
    (form-char? v) (str "(integer->char " (form-char-code v) ")")
    :else (throw (ex-info (str "emit-const: unsupported literal " (pr-str v)) {}))))

;; Quoted literals. A :quote node's :form is the RAW reader form;
;; reconstruct each as the matching Chez RT constructor — the runtime value of a
;; quote is just that literal data. The form is walked via the jolt.host form-*
;; contract (the portable seam the analyzer uses), NOT host-native predicates, so
;; this stays host-neutral — the contract walks the host's reader forms.
(declare emit-quoted)

;; --- sharing the quoted constructions ----------------------------------------
;; emit-quoted renders a form as the constructor calls that rebuild it, and it
;; renders a SUBTREE every time it meets one. That is fine for a lone quoted datum
;; and quadratic for the fnsrc registrations, which are the one caller that emits
;; forms NESTED INSIDE EACH OTHER: every anon fn registers its whole source form,
;; and an anon fn written inside another one is part of both, so a chain of depth
;; N emits O(N^2) text. The CPS pass in clojure.core.async builds exactly that
;; chain — one closure per park site — and a 240-park go body emitted 5.4 MB of
;; registrations, which analyze/emit/Chez then each paid for in proportion.
;;
;; So the pool hash-conses: a construction is emitted once, bound to a name, and
;; every later occurrence of the same construction is that name. Because a child's
;; construction is interned before its parent's is built, the parent is assembled
;; out of NAMES and costs O(its own arity) rather than O(its subtree) — which is
;; what turns the total linear. It also collapses the repetition inside one form
;; (every (jolt-symbol #f "x") in a body is now one object).
;;
;; Sharing the VALUES is sound because these are immutable constructions: symbols,
;; persistent collections and literals. It is the same argument hoist-const makes,
;; and the values reaching image-register-fn-form! are equal to the ones it got
;; before, structurally and by every observation the image writer makes of them.
;;
;; nil pool (every other caller, and every system namespace) is the identity: the
;; construction is returned inline, byte for byte as before, so the seed mint and
;; the core overlay are untouched.
(def ^:dynamic *quote-pool* nil)

;; Rows already emitted, as [src-form binding-name]. Interning alone made the TEXT
;; linear but left the WALK quadratic: assembling a parent's construction still
;; descended through the nested literal it contains, all the way to the leaves,
;; only to rediscover constructions the pool already had. This stops the descent —
;; a sub-form that IS an already-emitted literal is that literal's binding name.
;;
;; Matched by IDENTITY, which is exact here and costs a pointer compare: analyze-fn
;; hands a literal's :src-form back as the very object nested inside its parent's
;; :src-form. Equality would be the obvious alternative and is the wrong tool — a
;; jolt seq does not cache its hash, so asking whether two forms are equal walks
;; the subtree this exists to avoid walking. A miss (a form the analyzer had to
;; rebuild) just descends as before.
(def ^:dynamic *quote-shared* nil)

(defn- q-shared-name [form]
  (when-let [rows *quote-shared*]
    (loop [i 0]
      (when (< i (count rows))
        (let [r (nth rows i)]
          (if (identical? (nth r 0) form) (nth r 1) (recur (inc i))))))))

(defn- q-intern
  "The text to use at a USE SITE for a construction: the construction itself with
  no pool, else a binding name, recording the construction once."
  [expr]
  (let [pool *quote-pool*]
    (if (nil? pool)
      expr
      (or (get (:by-expr @pool) expr)
          (let [nm (str "_q$" (count (:order @pool)))]
            (swap! pool (fn [p] (-> p
                                    (assoc-in [:by-expr expr] nm)
                                    (update :order conj [nm expr]))))
            nm)))))

;; Both sorted renderings below order by EMITTED TEXT, because a set and a map
;; value have no source order and the host hash order is not stable across Chez
;; versions (jolt-8479). A pooled subterm renders as a binding name handed out in
;; traversal order — i.e. in host-hash order — so sorting those would reintroduce
;; exactly the instability the sort exists to remove. Emit the items unpooled, sort
;; the real text, and intern only the finished construction.
(defn- emit-quoted-map [pairs]
  ;; pairs: a jolt vector of [k-form v-form] pairs (form-map-pairs)
  (q-intern
   (str "(jolt-hash-map "
        (str/join " " (mapcat (fn [p] [(emit-quoted (nth p 0)) (emit-quoted (nth p 1))]) pairs))
        ")")))
(defn- emit-quoted-map-value [m]
  ;; A jolt map VALUE (def/symbol metadata is a value, not a reader form). (keys m)
  ;; iterates in host-hash order, which is not stable across Chez versions, so emit
  ;; the pairs sorted by their emitted Scheme text — keeps the seed byte-fixed
  ;; regardless of the host hash (jolt-8479).
  (let [pairs (binding [*quote-pool* nil]
                (vec (sort (map (fn [k] (str (emit-quoted k) " " (emit-quoted (get m k)))) (keys m)))))]
    (q-intern (str "(jolt-hash-map " (str/join " " pairs) ")"))))
;; emit-quoted reconstructs both raw reader forms (from :quote) AND plain jolt
;; values (def/symbol :meta). Reader forms are walked via the jolt.host form-*
;; contract; the native-predicate branches below catch genuine jolt collection
;; VALUES. The form-* branches come first so a reader form (a host-native struct/
;; array that a native predicate might also match) is always handled as a form.
(defn emit-quoted [form]
  (cond
    (form-char? form) (emit-const form)
    (form-literal? form) (emit-const form)
    (form-sym? form)
    (let [m (form-sym-meta form) sns (form-sym-ns form) nm (form-sym-name form)]
      (q-intern
       (if (and m (pos? (count m)))
         ;; carry reader metadata (^:foo bar) onto the quoted symbol so (meta 'x) sees it
         (str "(jolt-symbol/meta " (if sns (chez-str-lit sns) "#f") " " (chez-str-lit nm) " "
              (emit-quoted m) ")")
         (str "(jolt-symbol " (if sns (chez-str-lit sns) "#f") " " (chez-str-lit nm) ")"))))
    ;; sort items by emitted text: a set has no source order, and host-hash order
    ;; is not stable across Chez versions (jolt-8479) — so the items are emitted
    ;; unpooled, or the sort would be over binding names handed out in that same
    ;; unstable order. See the pool comment above.
    (form-set? form)
    (q-intern (str "(jolt-hash-set "
                   (str/join " " (binding [*quote-pool* nil]
                                   (vec (sort (map emit-quoted (form-set-items form))))))
                   ")"))
    ;; a fn literal is a list, so this is the only branch that has to ask
    (form-list? form)
    (or (q-shared-name form)
        (q-intern (str "(jolt-list " (str/join " " (map emit-quoted (form-elements form))) ")")))
    (form-vec? form) (q-intern (str "(jolt-vector " (str/join " " (map emit-quoted (form-vec-items form))) ")"))
    (form-map? form) (emit-quoted-map (form-map-pairs form))
    ;; a quoted #"…" regex value -> reconstruct it (same as the :regex IR leaf).
    (form-regex? form) (str "(jolt-regex " (chez-str-lit (form-regex-source form)) ")")
    ;; quoted #inst / #uuid literals construct their value, like the JVM reader
    ;; (which builds the Date/UUID at read time, so a quoted/macro form carries the
    ;; value, not the raw tagged form). Same emit as the :inst / :uuid IR leaves.
    (form-inst? form) (str "(jolt-inst-from-string " (chez-str-lit (form-inst-source form)) ")")
    ;; a Class value inside quoted structure (a macro spliced (resolve 'C) under
    ;; a quote) reconstructs through the interner, like #inst/#uuid.
    (form-class-value? form) (str "(jolt-class-for " (chez-str-lit (form-class-value-name form)) ")")
    (form-uuid? form) (str "(jolt-uuid-from-string " (chez-str-lit (form-uuid-source form)) ")")
    ;; ...and a quoted 1.5M builds its BigDecimal the same way (the :bigdec IR
    ;; leaf's emit). Without this arm the raw reader form went out as the datum:
    ;; (first '[1.5M]) was an opaque object printing as #bigdec "1.5", = to
    ;; nothing, and (eval '(+ 1.5M 1)) could not compile it.
    (form-bigdec? form) (str "(jolt-bigdec-from-string " (chez-str-lit (form-bigdec-source form)) ")")
    ;; a quoted custom #tag with no registered reader -> a tagged-literal value
    ;; (Clojure's reader builds a TaggedLiteral), not the raw reader map. The tag is
    ;; stored as a :#name keyword; strip the leading # to the bare symbol.
    ;; stored as a :#name / :#ns/name keyword; form-tag-name is the tag as written,
    ;; and a qualified one is a QUALIFIED symbol (a quoted #foo/bar used to come
    ;; back as the bare symbol named "foo/bar").
    (and (map? form) (= :jolt/tagged (get form :jolt/type)))
    (let [tag (jolt.host/form-tag-name form)
          i (str/index-of tag "/")
          tns (when i (subs tag 0 i))
          tn (if i (subs tag (inc i)) tag)]
      (q-intern (str "(jolt-tagged-literal (jolt-symbol " (if tns (chez-str-lit tns) "#f") " "
                     (chez-str-lit tn) ") " (emit-quoted (get form :form)) ")")))
    ;; plain jolt VALUES (metadata maps and anything nested in them)
    (map? form) (emit-quoted-map-value form)
    (vector? form) (q-intern (str "(jolt-vector " (str/join " " (map emit-quoted form)) ")"))
    (set? form) (q-intern (str "(jolt-hash-set "
                               (str/join " " (binding [*quote-pool* nil]
                                               (vec (sort (map emit-quoted form)))))
                               ")"))
    (seq? form) (q-intern (str "(jolt-list " (str/join " " (map emit-quoted form)) ")"))
    :else (throw (ex-info (str "emit-quoted: unsupported quoted form " (pr-str form)) {}))))

