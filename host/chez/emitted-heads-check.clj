;; Every head the Scheme back end writes into its output text must be one a
;; user local cannot shadow: munge-name has to prefix a local of that name.
;; The back end keeps the list of such names by hand (rt-emitted-names), and an
;; emission site added without a matching entry let (let [pair? true] ...) or
;; (let [fx+ 1] (try 1 (finally nil))) call the local instead of the runtime.
;;
;; This reads backend_scheme.clj with the reader, takes every string literal,
;; and collects each "(head" in them, plus the heads spliced after
;; (unsafe-prefix), which are bare on a target without one. A head munge-name
;; leaves unchanged fails unless it is in `allowed` below, and an `allowed`
;; entry that no longer occurs fails too, so the exceptions cannot go stale.
(require '[clojure.walk :as walk])

(def path "jolt-core/jolt/backend_scheme.clj")
(def src (slurp path))

;; Heads that are not runtime procedures or syntax a local could shadow.
(def allowed
  {;; prose in the namespace docstring and comments-as-strings
   "jolt.ir" :prose "read" :prose "eval" :prose "no" :prose
   "argument" :prose
   ;; ffi type syntax: (& name) is foreign-procedure's by-reference marker;
   ;; & cannot name a Clojure local
   "&" :ffi-syntax
   ;; an ftype field path in (ftype-&ref T (value) p), not a reference
   "value" :ftype-field
   ;; binders whose scope holds only emitted text, never a user form:
   ;; ffi wrappers (a p result address), the catch result r (a let, so the
   ;; catch body is evaluated outside it), the record ctor's v and the PIC's
   ;; _nv/_d/_f cache temporaries
   "a" :binder "p" :binder "r" :binder "v" :binder "result" :binder
   "address" :binder "_nv" :binder "_d" :binder "_f" :binder})

(def strs
  (let [acc (atom #{})]
    (walk/postwalk (fn [x] (when (string? x) (swap! acc conj x)) x)
                   (read-string (str "[" src "]")))
    @acc))

(def heads
  (into (sorted-set)
        (concat
         (mapcat #(map second (re-seq #"\(([^\s()\"'\[\]{}#;0-9][^\s()\"'\[\]{};]*)" %)) strs)
         (map second (re-seq #"\(unsafe-prefix\) \"([^\s\"]+)" src)))))

(def munge-name @(resolve 'jolt.backend-scheme/munge-name))

(def bare (remove #(not= % (munge-name %)) heads))
(def unlisted (remove allowed bare))
(def stale (remove (set bare) (keys allowed)))

(doseq [h unlisted]
  (println (str "FAIL: emitted head " (pr-str h) " is not renamed away from user locals;"
                " add it to rt-emitted-names in " path)))
(doseq [h stale]
  (println (str "FAIL: allowed head " (pr-str h) " no longer occurs bare; drop it from"
                " host/chez/emitted-heads-check.clj")))
(if (or (seq unlisted) (seq stale))
  (System/exit 1)
  (println (str "OK: " (count heads) " emitted heads, none shadowable ("
                (count allowed) " allowed)")))
