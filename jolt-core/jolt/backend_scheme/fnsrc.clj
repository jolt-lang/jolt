(ns jolt.backend-scheme.fnsrc
  "Source-form registration for fn literals: the unique name each literal is
  bound under, the per-site maker a closure's captures are read back through,
  and the image-register-fn-form! rows — source text, or the quoted
  construction — a top-level form flushes ahead of itself, so a state image can
  rebuild a live closure as code."
  (:require [clojure.string :as str]
            [jolt.host
             :refer [form-sym? form-sym-name form-sym-ns form-sym-meta form-list?
                     form-vec? form-map? form-set? form-char? form-literal?
                     form-elements form-vec-items form-map-pairs form-set-items
                     form-regex? form-regex-source form-inst? form-inst-source
                     form-uuid? form-uuid-source form-bigdec? form-bigdec-source
                     form-class-value?]]
            [jolt.backend-scheme.names :refer [chez-str-lit munge-chars munge-name]]
            [jolt.backend-scheme.session :refer [fresh-label]]
            [jolt.backend-scheme.hoist :refer [*cache-cells*]]
            [jolt.backend-scheme.literals
             :refer [*quote-pool* *quote-shared* emit-quoted]]))

;; R1 (bead jolt-hqpn): unique letrec names + source-form registration for
;; anonymous fn literals in NON-SYSTEM namespaces, so a live closure reports its
;; source form through Chez's inspector and a state image can rebuild it as code.
;; Bound where emit-top-form / the :def cases establish per-top-level-form
;; context: *fnsrc-ns* (the namespace being emitted), *fnsrc-def* (the enclosing
;; top-level def name, nil for a top-level expression), *fnsrc-counter* (an
;; atom: anon literals seen so far in this top-level form), *fnsrc-regs* (an
;; atom: the collected [name form ns free-names] rows, flushed as
;; image-register-fn-form! siblings after the form), and *fnsrc-def-init?* (true
;; only while emitting a def's DIRECT fn init, which the define itself names and
;; must not wrap). All nil/false outside any top-level form: emit-fn then keeps
;; the old bare lambda, so off-context emission is byte-unchanged.
(def ^:dynamic *fnsrc-ns* nil)
(def ^:dynamic *fnsrc-def* nil)
(def ^:dynamic *fnsrc-counter* nil)
(def ^:dynamic *fnsrc-regs* nil)
(def ^:dynamic *fnsrc-def-init?* false)
;; A fn literal registers its source unless there is no per-form context to name
;; it by — that is the whole rule now.
;;
;; It used to exclude every namespace the language owns (clojure.core and any
;; clojure.* / jolt.* prefix), to keep the seed mint and the core overlay
;; byte-identical. The consequence was that a closure clojure.core made could not
;; be written to a state image at all: `partial`, `comp`, `memoize` and every
;; lazy seq from an overlay fn refused, and RFC 0009 documented that as a limit
;; of the format. It is not a limit of the format, it was a limit of the build —
;; and an image feature that cannot carry core's own closures is not carrying
;; program state, it is carrying the part of it the compiler found convenient.
;;
;; So core's literals register too. The seed prelude grows by their source forms;
;; that is the price, and it is paid once at mint rather than by every program.
(defn fnsrc-system-ns? [ns] (nil? ns))
;; The globally unique letrec name for the next anon literal:
;; jfn$<ns>/<def>$<counter> (counter per top-level def); literals outside any
;; def use jfn$<ns>/$<counter>, with the counter per NAMESPACE for the life of
;; the process. Per top-level form, every deftype method body and every
;; defmethod in a namespace started at $0 and their registrations (keyed by
;; name) overwrote each other, so an image restore of one such closure came
;; back with the LAST form's source. Deterministic still: one mint or build
;; emits a namespace's forms in source order, so the same source emits the
;; same names.
;;
;; The parts are munge-chars, not munge-name: the jfn$ prefix already keeps
;; the whole name off every reserved word. The separator is `/` because no
;; escape can forge it: munge-chars never emits one and the reader never lets
;; one into a namespace or def name. With `$` as the separator, ns "a$" + def
;; "c" and ns "a" + def "$c" both spelled jfn$a$$$c$0, and the second
;; registration silently replaced the first.
(def ^:private fnsrc-ns-counters (atom {}))

(defn seed-fnsrc-ns-counter!
  "Start namespace ns's anon-literal counter at n. The build does this as it
  emits each namespace: the counter otherwise carries whatever the in-process
  load of that namespace used — a source load consumes names, a cached one does
  not — and a namespace's emitted text must not depend on which it was, since
  the build caches one compiled unit per namespace on its text (#1059). The
  build seeds far above zero so a built binary's own runtime evaluation, whose
  counters start at 0, cannot reuse a baked name."
  [ns n]
  (swap! fnsrc-ns-counters assoc (str ns) n))
(defn fnsrc-name []
  (str "jfn$" (munge-chars *fnsrc-ns*) "/"
       (if *fnsrc-def* (munge-chars *fnsrc-def*) "")
       "$"
       (if *fnsrc-def*
         (let [n @*fnsrc-counter*] (swap! *fnsrc-counter* inc) n)
         (let [k (str *fnsrc-ns*)
               n (get @fnsrc-ns-counters k 0)]
           (swap! fnsrc-ns-counters assoc k (inc n))
           n))))

;; A top-level form's collected anon-fn registrations as Scheme siblings:
;;   (image-register-fn-form! "jfn$ns/def$0" (image-fn-form-src "<source text>") "ns" <quoted free names>)
;; "" when the namespace is system or nothing was collected, so any fn-free def
;; emits byte-identically.
;;
;; The form travels as SOURCE TEXT, not as a quoted construction. A construction
;; is a let* of jolt-symbol/jolt-list/jolt-vector calls that runs at every
;; process start and sits in the compiled runtime as code; the text is a
;; bytevector constant the registry parses on the first lookup, which only the
;; image writer ever makes (fn-form-registry.ss image-fn-form-src). Rendered by
;; fnsrc-src and CHECKED, per row, to read back to the construction it replaces;
;; a form that does not (a live class value a macro spliced in has no reader
;; syntax) keeps the construction, emitted under the *quote-pool* as before --
;; interned so a chain of N nested literals costs O(N) text, bound by one let*
;; header around the row calls (see the pool comment at emit-quoted).
;;
;; A row that throws leaves whatever it interned before throwing in the pool, so
;; the let* can carry a binding nothing references. Dead, valid, and confined to a
;; path that is already best-effort.
;; The per-site makers, as top-level defines. Emitted AHEAD of the registrations
;; that name them and of the form itself, so a form whose own evaluation creates
;; one of these closures finds the maker already defined.
;; A site's MAKER: (lambda (free…) <the literal>), which the site then calls.
;;
;; Writing a closure to an image needs its captured values, and Chez hands those
;; back by POSITION. The NAMES that say which is which are inspector information,
;; which a release build does not generate — it costs +117% on the compiled
;; prelude, a debugging model of every procedure, to name the captures of a few
;; hundred. So `jolt run` refused every closure clojure.core makes (cycle,
;; repeat, partial, comp) while a default app build, which does generate it,
;; wrote them fine. The position order is not the source order and is not ours to
;; predict.
;;
;; A maker settles it: every instance of a site comes from one code object, so
;; the capture layout is a property of the CODE and identical across instances.
;; The image calls the maker once with distinct sentinels, sees which slot each
;; landed in, and reads every later instance through that permutation.
;;
;; The maker is built HERE, in the site's own scope, and not hoisted to the top of
;; the form. Hoisting looked cheaper — one maker per form rather than one per
;; closure — but the emitted body can reference bindings this scope has and that
;; one does not: a cache cell, and, as core.async's CPS transform showed, the
;; gensym a `letfn*` binds around a loop. Hoisted, those are unbound at runtime.
;; Only the FIRST maker is registered (the cell guards it), because all it is used
;; for is the layout, and every instance shares the code object that decides it.
(defn fnsrc-maker-site [nm frees inner]
  (let [c (fresh-label "_mkc$")
        v (fresh-label "_mk$")
        ps (map munge-name frees)
        args (apply str (map #(str " " %) ps))]
    (swap! *cache-cells* conj c)
    (str "(let ((" v " (lambda (" (str/join " " ps) ") " inner "))) "
         "(if (not " c ") (begin (set! " c " #t) "
         "(image-fn-form-maker! " (chez-str-lit nm) " " v "))) "
         "(" v args "))")))

;; The SOURCE TEXT of a registration's form: the syntax the raw reader
;; (jolt.host/fn-form-parse, positions off) reads back to the form emit-quoted
;; would have constructed. Mirrors emit-quoted branch for branch, so everything
;; it can render has a rendering here except a live class value, which has no
;; reader syntax. Sets and metadata maps sort by rendered text for the reason
;; emit-quoted sorts them (the seed must not depend on host hash order); a
;; reader-built map keeps its source order, which the reader records again.
(declare fnsrc-src)
(defn- fnsrc-src-items [items] (str/join " " (map fnsrc-src items)))
(defn- fnsrc-src-map-value [m]
  (str "{" (str/join " " (sort (map (fn [k] (str (fnsrc-src k) " " (fnsrc-src (get m k)))) (keys m)))) "}"))
(defn fnsrc-src [form]
  (cond
    (form-char? form) (pr-str form)
    (form-literal? form) (pr-str form)
    (form-sym? form)
    (let [m (form-sym-meta form) sns (form-sym-ns form) nm (form-sym-name form)
          s (if sns (str sns "/" nm) nm)]
      ;; the meta map renders like any map: a reader-built one in source order
      ;; (which is how emit-quoted carries it), a plain value sorted
      (if (and m (pos? (count m)))
        (str "^" (fnsrc-src m) " " s)
        s))
    (form-set? form) (str "#{" (str/join " " (sort (map fnsrc-src (form-set-items form)))) "}")
    (form-list? form) (str "(" (fnsrc-src-items (form-elements form)) ")")
    (form-vec? form) (str "[" (fnsrc-src-items (form-vec-items form)) "]")
    (form-map? form)
    (str "{" (str/join " " (map (fn [p] (str (fnsrc-src (nth p 0)) " " (fnsrc-src (nth p 1))))
                                (form-map-pairs form))) "}")
    (form-regex? form) (str "#\"" (form-regex-source form) "\"")
    (form-inst? form) (str "#inst " (pr-str (form-inst-source form)))
    (form-class-value? form) (throw (ex-info "fnsrc-src: a class value has no reader syntax" {}))
    (form-uuid? form) (str "#uuid " (pr-str (form-uuid-source form)))
    (form-bigdec? form) (str (form-bigdec-source form) "M")
    (and (map? form) (= :jolt/tagged (get form :jolt/type)))
    (str "#" (jolt.host/form-tag-name form) " " (fnsrc-src (get form :form)))
    (map? form) (fnsrc-src-map-value form)
    (vector? form) (str "[" (fnsrc-src-items form) "]")
    (set? form) (str "#{" (str/join " " (sort (map fnsrc-src form))) "}")
    (seq? form) (str "(" (fnsrc-src-items form) ")")
    :else (throw (ex-info (str "fnsrc-src: no source rendering for " (pr-str form)) {}))))

;; A registration's form argument as text, (image-fn-form-src "..."), or nil when
;; the form has no rendering that reads back to the same construction -- checked
;; here against the very parse the registry runs, so a mismatch falls back to
;; the construction instead of registering a different form.
(defn- fnsrc-row-src [form]
  (try
    (let [s (fnsrc-src form)
          same? (binding [*quote-pool* nil *quote-shared* nil]
                  (= (emit-quoted (jolt.host/fn-form-parse s)) (emit-quoted form)))]
      (when same? (str "(image-fn-form-src " (chez-str-lit s) ")")))
    (catch Exception _ nil)))

(defn fnsrc-flush []
  (if (or (fnsrc-system-ns? *fnsrc-ns*) (empty? @*fnsrc-regs*))
    ""
    ;; best-effort: a macro can splice a LIVE value (a namespace, a var's
    ;; value) into a fn body, and emit-quoted has no rendering for those.
    ;; Such a literal just goes unregistered -- its closure refuses at dump
    ;; like any other unregistered fn -- rather than failing the whole
    ;; compilation of code that never dumps anything.
    (let [pool (atom {:by-expr {} :order []})
          call (fn [nm f ns frees lives]
                 (str "(image-register-fn-form! " (chez-str-lit nm) " " f " " (chez-str-lit ns) " "
                      (emit-quoted frees)
                      ;; the optional 5th argument, emitted only when the copy's
                      ;; captures differ from the source names -- so every
                      ;; un-spliced registration stays byte-identical.
                      (if lives (str " " (emit-quoted lives)) "")
                      ")"))
          ;; the rows in order. A text row touches no pool; a constructed one is
          ;; emitted with every EARLIER constructed row available to stop the walk
          ;; at (see *quote-shared*). Innermost first, so a nested literal is
          ;; already there by the time its parent is emitted. reduce and not map:
          ;; each row's emission depends on the ones before it.
          out (binding [*quote-pool* pool]
                (reduce
                 (fn [acc row]
                   (let [nm (nth row 0) form (nth row 1) ns (nth row 2) frees (nth row 3)
                         lives (nth row 4)
                         lives (when (and lives (not= lives frees)) lives)
                         src (fnsrc-row-src form)
                         q (if src
                             [nil (binding [*quote-pool* nil] (call nm src ns frees lives))]
                             (try
                               (binding [*quote-shared* (:shared acc)]
                                 (let [f (emit-quoted form)] [f (call nm f ns frees lives)]))
                               (catch Exception _ nil)))]
                     (cond
                       (nil? q) acc
                       (nil? (nth q 0)) (update acc :calls conj (nth q 1))
                       :else (-> acc
                                 (update :calls conj (nth q 1))
                                 (update :shared conj [form (nth q 0)])))))
                 {:calls [] :shared []}
                 @*fnsrc-regs*))
          calls (:calls out)
          binds (:order @pool)]
      (cond
        ;; every row threw: nothing to register, and a let* with no body is not a
        ;; form at all
        (empty? calls) ""
        (empty? binds) (str " " (str/join " " calls))
        :else
        (str " (let* ("
             (str/join " " (map (fn [b] (str "(" (nth b 0) " " (nth b 1) ")")) binds))
             ") " (str/join " " calls) ")")))))

