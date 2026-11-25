;; IR inlining (jolt.passes.inline), enabled under optimization. A small
;; single-arity defn is stashed and spliced at its call sites, removing the call.
;; A ^double/^long fn's param-entry and return coercions travel with the splice
;; (via :coerce nodes) so an inlined call matches the called one — incl. coercing a
;; non-double arg — and the body's fl*/fx* fast path still fires. Run:
;;   chez --script test/chez/inline-test.ss

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (has? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))
(define (emitf ns str)            ; analyze + run-passes (optimize on) + emit
  (let-values (((f j) (rdr-read-form str 0 (string-length str))))
    (let ((ctx (make-analyze-ctx ns)))
      (jolt-ce-emit (jolt-ce-run-passes (jolt-ce-analyze ctx f) ctx)))))
;; The CODE portion of an emission. An anon literal's registration replays its
;; original SOURCE form, so a callee name that inlining removed from the code is
;; still there verbatim — drop the registrations before asserting.
;;
;; They are a PREAMBLE, not a tail: emit-top-form puts them first, as
;; (begin <registrations> <code>). Cutting at the first (image-register-fn-form!
;; and keeping the prefix therefore kept "(begin " and threw the code away, so
;; every assertion below that used this passed no matter what the code said. It
;; only surfaced when the registrations grew a (let* …) header binding the shared
;; constructions, which put the callee's name into the part being kept.
;;
;; So match parens instead of scanning for a marker: skip the one balanced form
;; that follows "(begin " and keep the rest. String literals are opaque to the
;; scan — the registrations are full of them, and a symbol named ")" would
;; otherwise unbalance it.
(define (skip-form s i)
  (let ((n (string-length s)))
    (let loop ((i i) (depth 0) (in-str #f))
      (if (>= i n)
          i
          (let ((c (string-ref s i)))
            (cond
              (in-str (cond ((char=? c #\\) (loop (+ i 2) depth #t))
                            ((char=? c #\") (loop (+ i 1) depth #f))
                            (else (loop (+ i 1) depth #t))))
              ((char=? c #\") (loop (+ i 1) depth #t))
              ((char=? c #\() (loop (+ i 1) (+ depth 1) #f))
              ((char=? c #\)) (if (= depth 1) (+ i 1) (loop (+ i 1) (- depth 1) #f)))
              (else (loop (+ i 1) depth #f))))))))
;; Matched against the two shapes the preamble can take and nothing else: a
;; top-level `do` also emits (begin …, and dropping its first statement would be a
;; silent hole in whatever asserted on it.
(define (starts-with? s pre)
  (and (>= (string-length s) (string-length pre))
       (string=? (substring s 0 (string-length pre)) pre)))
;; The code is the LAST form of that begin: the preamble is registrations and,
;; for a literal with no source rendering, a (let* …) header — but the code can
;; open with a (let* …) of its own too (a top-level fn's constant pool), so
;; guessing which leading forms are preamble by their head threw the code away
;; and left ")" to assert on. Taking the last form cannot mistake one for the other.
(define (code-part s)
  (if (or (starts-with? s "(begin (let* (")
          (starts-with? s "(begin (image-register-fn-form!"))
      (let loop ((i 7) (last-start #f))   ; 7 = past "(begin "
        (let ((i (let ws ((i i))
                   (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
                       (ws (+ i 1)) i))))
          (if (or (>= i (string-length s)) (char=? (string-ref s i) #\)))
              (if last-start
                  (let ((end (skip-form s last-start))) (substring s last-start end))
                  s)
              (loop (skip-form s i) i))))
      s))
(define (ev s) (jolt-compile-eval s "u"))
(define clj= (var-deref "clojure.core" "="))

;; inlining is a closed-world optimization — requires optimize + direct-link.
(set-optimize! #t)
(set-direct-link-flag! #t)

(define assoc-in-pair
  "(defn sibling-edits [m k a b] (-> m (assoc-in [:root k :a] a) (assoc-in [:root k :b] b)))")
(let ((e (code-part (emitf "u" assoc-in-pair))))
  (ok "assoc-in chain emits guards and ordered sibling writes"
      (and (has? e "jolt-assoc-in-original") (has? e "(jolt-assoc-in-map-edit ")
           (has? e "(jolt-assoc-in-leaf-edit ") (has? e "jv$clojure.core/assoc-in"))))
(for-each
  (lambda (form)
    (ok (string-append "ordinary assoc-in emission: " form)
        (not (has? (code-part (emitf "u" form)) "(jolt-assoc-in-map-edit "))))
  '("(defn no-edit [assoc-in m k a b] (-> m (assoc-in [:root k :a] a) (assoc-in [:root k :b] b)))"
    "(defn no-edit [m k a b] (-> m (assoc-in [:root k] a) (assoc-in [:root k :b] b)))"
    "(defn no-edit [m k a b] (-> m (assoc-in [:root (k) :a] a) (assoc-in [:root (k) :b] b)))"
    "(defn no-edit [m k a b] (-> m (assoc-in [:root k :a] (a)) (assoc-in [:root k :b] b)))"
    "(defn no-edit [m k a b] (-> (m) (assoc-in [:root k :a] a) (assoc-in [:root k :b] b)))"
    "(defn no-edit [m k a b] (assoc-in (let [k :other] (assoc-in m [:root k :a] a)) [:root k :b] b))"))
(ok "a literal path, as inlining leaves a keyword argument, still fuses"
    (has? (code-part (emitf "u" "(defn lit-edit [m a b] (-> m (assoc-in [:root :shape :a] a) (assoc-in [:root :shape :b] b)))"))
          "(jolt-assoc-in-map-edit "))
(set-direct-link-flag! #f)

(ok "dynamic calls keep ordinary assoc-in dispatch"
    (not (has? (code-part (emitf "u" assoc-in-pair)) "(jolt-assoc-in-map-edit ")))
(set-direct-link-flag! #t)
(ev assoc-in-pair)
(ev "(defrecord EditRecord [root])")
(ok "assoc-in keeps nil and missing, native and record paths, and retained versions"
    (ev "(let [ordinary (var-get #'clojure.core/assoc-in)
               k (symbol \"namespace\") leaf (with-meta (hash-map :b 9) {:leaf true})
               root (with-meta {:root {k leaf}} {:root true})]
           (and (every? (fn [m]
                          (= (sibling-edits m k nil false)
                             (ordinary (ordinary m [:root k :a] nil) [:root k :b] false)))
                        [nil {} {:root nil} {:root {k nil}} root (->EditRecord {k leaf})])
                (= (sibling-edits nil nil 1 2) {:root {nil {:a 1 :b 2}}})
                (= (sibling-edits nil [:composite] 1 2) {:root {[:composite] {:a 1 :b 2}}})
                (= (sibling-edits {:root [{}]} 0 1 2) {:root [{:a 1 :b 2}]})
                (= 9 (get-in root [:root k :b]))
                (= {:root true} (meta (sibling-edits root k 1 2)))
                (= {:leaf true} (meta (get-in (sibling-edits root k 1 2) [:root k])))
                (= (class leaf) (class (get-in (sibling-edits root k 1 2) [:root k])))))"))
(ok "assoc-in compares no-op identity without canonicalizing canceled edits"
    (ev "(let [old (with-meta {:p {:x :old}} {:retained true})
               same (-> old (assoc-in [:p :x] :old) (assoc-in [:p :x] :old))
               changed (-> old (assoc-in [:p :x] :new) (assoc-in [:p :x] :old))]
           (and (identical? old same) (= old changed)
                (not (identical? old changed))
                (not (identical? (:p old) (:p changed)))))"))
(ev "(defn repeated-edit [m k a b] (-> m (assoc-in [k :x] a) (assoc-in [k :x] b)))")
(ok "fused assoc-in preserves no-op and canceled edit identities"
    (ev "(let [edit (var-get #'repeated-edit) m {:p {:x :old}}
               same (edit m :p :old :old)
               changed (edit m :p :new :old)]
           (and (identical? m same) (= m changed) (not (identical? m changed))
                (not (identical? (:p m) (:p changed)))))"))
(ok "assoc-in preserves HAMT collision entries and the original stored UUID"
    (ev "(let [stored #uuid \"00000000-0000-0000-0000-000000000001\"
               sibling #uuid \"00000000-0000-0000-0000-000100000000\"
               missing #uuid \"00000001-0000-0000-0000-000000000000\"
               probe (parse-uuid (str stored))
               m {:root (hash-map stored {} sibling :retained)}
               replaced (sibling-edits m probe 1 2)
               inserted (sibling-edits m missing false nil)]
           (and (= (hash stored) (hash sibling) (hash missing))
                (= {:a 1 :b 2} (get-in replaced [:root stored]))
                (= :retained (get-in replaced [:root sibling]))
                (identical? stored (key (find (:root replaced) probe)))
                (= {:a false :b nil} (get-in inserted [:root missing]))
                (= :retained (get-in inserted [:root sibling]))
                (nil? (get-in m [:root missing]))))"))
;; 54 and 95 share the low five hash bits with the \"AaAa\"/\"BBBB\" bucket but
;; not its hash: the bucket nests beside them, in the JVM's order
(ok "assoc-in of a key landing on a bucket's slot nests the bucket"
    (ev "(let [ordinary (var-get #'clojure.core/assoc-in)
               inner (hash-map \"AaAa\" {} \"BBBB\" {})
               m {:root inner}]
           (every? (fn [k]
                     (let [got (sibling-edits m k 1 2)
                           want (ordinary (ordinary m [:root k :a] 1) [:root k :b] 2)]
                       (and (= got want) (= (keys (:root got)) (keys (:root want)))
                            (= (count (:root got)) (count (:root want))))))
                   [54 95 \"AaAa\" \"AaBB\"]))"))
(ok "overlapping paths and externally retained intermediate maps stay ordered"
    (ev "(let [old {:root {:shape {:discarded 0}}} child {:keep true}
               first (assoc-in old [:root :shape] child)
               second (assoc-in first [:root :shape :added] false)]
           (and (= second (-> old (assoc-in [:root :shape] child)
                                   (assoc-in [:root :shape :added] false)))
                (= {:discarded 0} (get-in old [:root :shape]))
                (identical? child (get-in first [:root :shape]))
                (= {:keep true :added false} (get-in second [:root :shape]))))"))
(ok "assoc-in fallback preserves an earlier bad receiver and operand order"
    (ev "(let [seen (atom []) note (fn [k v] (swap! seen conj k) v)
               run (fn [f] (try [(f)] (catch Throwable e [(class e) (.getMessage e)])))
               invalid {:root {:bad 1}}
               generic (var-get #'clojure.core/assoc-in)]
           (and (= (run #(sibling-edits invalid :bad 1 2))
                   (run #(generic (generic invalid [:root :bad :a] 1) [:root :bad :b] 2)))
                (not= [:returned]
                      (run #(do (-> (note :m invalid)
                                    (assoc-in [:root (note :key :bad) :a] (note :value 1))
                                    (assoc-in [:root (note :later-key :bad) :b] (note :later-value 2)))
                                :returned)))
                (= [:m :key :value] @seen)))"))
(ok "assoc-in follows successive live core roots after a redefinition"
    ;; Call the compiled body so inlining cannot turn its paths into constants.
    (ev "(let [edit (var-get #'sibling-edits) seen (atom [])
               next (fn [m path v] (swap! seen conj :next) (assoc m (peek path) v))
               init (fn [m path v] (swap! seen conj :init)
                      (alter-var-root #'clojure.core/assoc-in (constantly next))
                      (assoc m (peek path) v))]
           (with-redefs [clojure.core/assoc-in init]
             (and (= {:a 1 :b 2} (edit {} :shape 1 2))
                  (= [:init :next] @seen))))"))

;; Both ordinary and fused array-map edits use this slot write. Install a new
;; root after the first write has run, without changing an eligible operand.
(let ((cell (var-cell-lookup "clojure.core" "assoc-in"))
      (write-slot pmap-array-assoc-at)
      (set-trace! (var-deref "jolt.backend-scheme" "set-trace-frames!"))
      (root-key (keyword #f "root")) (shape (keyword #f "shape"))
      (replaced (keyword #f "replaced")) (later (keyword #f "later")))
  (define original (var-cell-root cell))
  (define (run label trace? paths)
    (let* ((name (format "assoc-in-rebind-~a" label))
           (edits (apply string-append
                         (map (lambda (path) (apply format " (assoc-in [:root ~a :~a] v)" path)) paths)))
           (form (format "(defn ~a [m k v] (-> m~a))" name edits))
           (ks (map (lambda (path) (keyword #f (cadr path))) paths))
           (root (ev "(with-meta {:root {:shape {:old true}} :kept true} {:marked true})"))
           (v (jolt-vector (keyword #f "token")))
           (writes '()) (calls '()))
      (define (value-at m k)
        (jolt-get (jolt-get (jolt-get m root-key) shape) k))
      (define (replacement marker)
        (lambda (m path arg)
          (set! calls (cons (list marker m path arg) calls))
          (when (eq? marker replaced) (var-root-set! cell (replacement later)))
          (original m path marker)))
      (set-trace! trace?)
      (ok (string-append name " uses fused edits")
          (has? (code-part (emitf "u" form)) "(jolt-assoc-in-map-edit "))
      (ev form)
      (let ((result (dynamic-wind
                      (lambda ()
                        (set! pmap-array-assoc-at
                          (lambda (m arr i k value)
                            (let ((r (write-slot m arr i k value)))
                              (when (memq k ks) (set! writes (cons k writes)))
                              (when (eq? k (car ks)) (var-root-set! cell (replacement replaced)))
                              r))))
                      (lambda () ((var-deref "u" name) root shape v))
                      (lambda ()
                        (set! pmap-array-assoc-at write-slot)
                        (var-root-set! cell original)))))
        (ok name
            (and (equal? (reverse writes) ks)
                 (eq? jolt-nil (value-at root (car ks)))
                 (eq? (jolt-meta root) (jolt-meta result))
                 (= (length calls) (- (length ks) 1))
                 (let loop ((i 0) (keys ks) (calls (reverse calls)) (before root))
                   (or (null? keys)
                       (let* ((k (car keys))
                              (marker (cond ((= i 0) v) ((= i 1) replaced) (else later)))
                              (path (jolt-vector root-key shape k)))
                         (and (eq? marker (value-at result k))
                              (or (= i 0)
                                  (let ((entry (car calls)))
                                    (and (eq? marker (car entry)) (clj= before (cadr entry))
                                         (eq? (jolt-meta root) (jolt-meta (cadr entry)))
                                         (clj= path (caddr entry)) (eq? v (cadddr entry)))))
                              (loop (+ i 1) (cdr keys) (if (= i 0) calls (cdr calls))
                                    (original before path marker)))))))))))
  (run 'dynamic #f '(("k" "a") ("k" "b") ("k" "c") ("k" "d")))
  (run 'traced-tail #t '((":shape" "a") (":shape" "b")))
  (run 'literal #f '((":shape" "a") (":shape" "b") (":shape" "c")))
  (set-trace! #f))

(ok "the UUID edit guard accepts the boot registries" (assoc-in-native-uuid?))

(let ((original jolt-eq-arms) (observed 0))
  (dynamic-wind
    (lambda ()
      (register-eq-arm!
        (lambda (a b)
          (and (juuid? a) (juuid? b)
               (begin (set! observed (+ observed 1)) #t)))
        (lambda (a b) #f)))
    (lambda ()
      (ok "a UUID equality extension keeps its original calls"
          (ev "(defn uuid-edit-probe [mode]
                 (let [stored #uuid \"00000000-0000-0000-0000-000000000001\"
                       k (parse-uuid (str stored)) m {:root {stored {}}}
                       edit (var-get #'sibling-edits)
                       generic (var-get #'clojure.core/assoc-in)
                       result (if mode (edit m k 1 2)
                                  (generic (generic m [:root k :a] 1) [:root k :b] 2))]
                   (and (= {:a 1 :b 2} (get-in result [:root k]))
                        (= {} (get-in result [:root stored]))
                        (= 2 (count (:root result))))))"))
      (set! observed 0)
      (let* ((generic (ev "(uuid-edit-probe false)")) (expected observed))
        (set! observed 0)
        (ok "the guard does no speculative equality calls"
            (and (ev "(uuid-edit-probe true)") generic (positive? expected) (= expected observed)))))
    (lambda () (set! jolt-eq-arms original))))

;; Use a HAMT built under the installed arm so looking up the UUID with its
;; native hash would miss the existing entry.
(let ((original-hash jolt-hash-arms)
      (original-hasheq jolt-hasheq-arms)
      (observed 0))
  (define (custom-hash k) (set! observed (+ observed 1)) 17)
  (define (check label install restore)
    (dynamic-wind install
      (lambda ()
        (let* ((probe (ev "(fn [f]
                             (let [stored #uuid \"00000000-0000-0000-0000-000000000001\"
                                   k (parse-uuid (str stored))
                                   m {:root (hash-map stored {:kept true} :other :retained)}
                                   result (f m k 1 2)]
                               [(get-in result [:root k]) (count (:root result))
                                (get-in result [:root :other]) (get-in m [:root k])]))"))
               (ordinary (ev "(let [a (var-get #'clojure.core/assoc-in)]
                                (fn [m k x y] (a (a m [:root k :a] x) [:root k :b] y)))")))
          (set! observed 0)
          (let* ((expected (jolt-invoke probe ordinary))
                 (expected-calls observed))
            (set! observed 0)
            (let ((actual (jolt-invoke probe (var-deref "u" "sibling-edits"))))
              (ok label
                  (and (positive? expected-calls) (= expected-calls observed)
                       (clj= expected actual)
                       (clj= expected (ev "[{:kept true :a 1 :b 2} 2 :retained {:kept true}]"))))))))
      restore))
  (check "a UUID hash arm gets the ordinary HAMT edits and calls"
         (lambda () (register-hash-arm! juuid? custom-hash))
         (lambda () (set! jolt-hash-arms original-hash)))
  (check "a UUID hasheq arm gets the ordinary HAMT edits and calls"
         (lambda () (set! jolt-hasheq-arms (cons (cons juuid? custom-hash) original-hasheq)))
         (lambda () (set! jolt-hasheq-arms original-hasheq))))

(ev "(defn alias-edit-probe [a base-lang lang ns-sym alias-sym target]
       (swap! a (fn [n] (-> n
                          (assoc-in [base-lang lang ns-sym :qualify-ns alias-sym] target)
                          (assoc-in [base-lang lang ns-sym :aliases alias-sym] target)))))")
(ok "assoc-in alias callback re-reads the current maps after a failed CAS"
    (ev "(let [path [:cljc :clj 'demo] a (atom (assoc-in {} path {:version :old}))
               changed (atom false) seen (atom [])]
           (set-validator! a
             (fn [proposed]
               (let [slot (get-in proposed path)]
                 (when (:aliases slot)
                   (swap! seen conj (:version slot))
                   (when (compare-and-set! changed false true)
                     (reset! a (assoc-in {} path {:version :new :kept true})))))
               true))
           (alias-edit-probe a :cljc :clj 'demo 'str 'clojure.string)
           (and (= [:old :new] @seen)
                (= {:version :new :kept true :qualify-ns {'str 'clojure.string}
                    :aliases {'str 'clojure.string}} (get-in @a path))))"))

;; One public site accepts both bounded native vector leaves and generic
;; sources. Check the executable form, since the preamble quotes the input.
(define widths-form
  "(def row-widths (fn* ([xs] (into [] (comp (filter sequential?) (map count)) xs))))")
(let ((e (code-part (emitf "u" widths-form))))
  (ok "row widths calls the fused leaf walk" (has? e "(jolt-vector-filter-map "))
  (ok "row widths checks the source representation" (has? e "(pvec-ent "))
  (ok "row widths retains the ordinary transducers on a miss"
      (and (has? e "(td-filter ") (has? e "(td-map "))))
(ok "a shadowed stage is not fused"
    (not (has? (code-part (emitf "u" "(def w (fn* ([filter xs] (into [] (comp (filter sequential?) (map count)) xs))))"))
               "(jolt-vector-filter-map ")))
(ok "a custom final stage is not fused"
    (not (has? (code-part (emitf "u" "(def w (fn* ([stop xs] (into [] (comp (filter sequential?) (map count) stop) xs))))"))
               "(jolt-vector-filter-map ")))
(let ((e (code-part (emitf "u" "(def w (fn* ([pvec-ent td-map xs] (into [] (comp (filter pvec-ent) (map td-map)) xs))))"))))
  (ok "filter/map locals cannot shadow emitted runtime names"
      (and (has? e "$Rpvec-ent") (has? e "$Rtd-map") (has? e "(pvec-ent ") (has? e "(td-map "))))
(set-direct-link-flag! #f)
(ok "dynamically linked row widths keep the ordinary cascade"
    (not (has? (code-part (emitf "u" widths-form)) "(jolt-vector-filter-map ")))
(set-direct-link-flag! #t)

;; a small plain fn is spliced; the call to it disappears.
(ev "(def add1 (fn* ([x] (+ x 1))))")
(let ((e (emitf "u" "(fn* ([y] (add1 y)))")))
  (ok "plain fn is inlined (call to add1 gone)" (not (has? (code-part e) "add1")))
  (ok "inlined body present (jolt-n+ ... 1)" (has? e "(jolt-n+")))
(ok "inlined plain fn runtime: (add1 41) = 42" (= 42 (jnum->exact (ev "((fn* ([y] (add1 y))) 41)"))))

;; a ^double fn: body fl-ops fire after inlining, and the call is gone.
(ev "(def ^double dwork (fn* ([^double a ^double b] (+ (* a a) (* b b)))))")
(let ((e (emitf "u" "(fn* ([] (dwork 3.0 4.0)))")))
  (ok "inlined ^double fn body uses fl*" (has? e "(#3%fl*"))
  (ok "inlined ^double fn call to dwork is gone" (not (has? (code-part e) "dwork"))))
(ok "inlined ^double call: 3^2+4^2 = 25" (= 25 (jnum->exact (ev "((fn* ([] (dwork 3.0 4.0))))"))))
;; coercion travels with the splice: int args become doubles, so the result is a
;; flonum 25.0 — matching the called fn, not an exact 25.
(ok "inlined ^double with int args still returns a flonum" (flonum? (ev "((fn* ([] (dwork 3 4))))")))

;; the cast fast path is decided in the CALLER's context after inlining: a typed
;; body inlined where the argument is untyped still carries its entry coercion
;; (lowered), and an untyped body inlined under a ^double argument is lowered too.
(ev "(def dlong (fn* ([^double d] (long d))))")
(ev "(def ulong (fn* ([d] (long d))))")
(ok "inlined ^double cast under an untyped caller is lowered through its entry coercion"
    (has? (emitf "u" "(fn* ([x] (dlong x)))") "(flonum->fixnum"))
(ok "inlined untyped cast under a ^double caller is lowered"
    (has? (emitf "u" "(fn* ([^double x] (ulong x)))") "(flonum->fixnum"))
(ok "inlined untyped cast under an untyped caller is not lowered"
    (not (has? (emitf "u" "(fn* ([x] (ulong x)))") "(flonum->fixnum")))
;; which entry a lowered cast falls back to is the CALLEE's: an untyped param is
;; boxed at the JVM's call, whatever the caller proves about the argument.
(ev "(def dbyte (fn* ([^double d] (byte d))))")
(ev "(def ubyte (fn* ([d] (byte d))))")
(ev "(def ubyte-copy (fn* ([d] (let* [e d] (byte e)))))")
(ev "(def ubyte-outer (fn* ([^double d] (ubyte d))))")
(ok "inlined ^double byte falls back to the primitive entry"
    (has? (emitf "u" "(fn* ([x] (dbyte x)))") "(jolt-byte-cast-prim "))
(ok "inlined untyped byte under a ^double caller keeps the fast path"
    (has? (emitf "u" "(fn* ([^double x] (ubyte x)))") "(flonum->fixnum"))
(ok "inlined untyped byte under a ^double caller falls back boxed"
    (not (has? (emitf "u" "(fn* ([^double x] (ubyte x)))") "-prim ")))
(ok "a let copy of an untyped param stays boxed"
    (not (has? (emitf "u" "(fn* ([^double x] (ubyte-copy x)))") "-prim ")))
(ok "boxed survives a second splice"
    (not (has? (emitf "u" "(fn* ([x] (ubyte-outer x)))") "-prim ")))
(ok "lowered cast answers: 2.9 -> 2, -2.9 -> -2"
    (equal? '(2 -2) (list (jnum->exact (ev "(dlong 2.9)")) (jnum->exact (ev "(dlong -2.9)")))))

;; a ^long fn inlines with long coercion + the checked long ops.
(ev "(def ^long lsum (fn* ([^long a ^long b] (+ a b))))")
(let ((e (emitf "u" "(fn* ([] (lsum 3 4)))")))
  (ok "inlined ^long fn body uses jolt-l+" (has? e "(jolt-l+")))
(ok "inlined ^long call: 3+4 = 7 (fixnum)" (let ((r (ev "((fn* ([] (lsum 3 4))))"))) (and (fixnum? r) (= r 7))))

;; an accumulator over an inlined ^double call: the whole loop body fuses to fl-ops.
(ev "(def ^double sq (fn* ([^double x] (* x x))))")
(let ((e (emitf "u" "(fn* ([] (loop [acc 0.0 i 0] (if (< i 3) (recur (+ acc (sq 2.0)) (inc i)) acc))))")))
  (ok "accumulator over inlined ^double call lowers to fl+" (has? e "(#3%fl+"))
  (ok "the sq call is inlined away" (not (has? (code-part e) "sq"))))
(ok "accumulator over inlined ^double call: 3*4.0 = 12" (= 12 (jnum->exact (ev "((fn* ([] (loop [acc 0.0 i 0] (if (< i 3) (recur (+ acc (sq 2.0)) (inc i)) acc)))))"))))

;; --- recursion cycles are never inlined (jolt-682) ---------------------------
;; A fn whose stashed body can reach itself through other stashes (mutual
;; recursion) must not be spliced: the spliced body re-exposes a call into the
;; cycle, so each inline-fixpoint round pastes one more layer and the body grows
;; by the cycle's branching factor per round until the cap. Ruuter's 4-way
;; matcher cycle unrolled to 4^5 copies — a 3.4MB match-trie and +73MiB idle RSS.
;; The cycle member keeps its real call, exactly as it compiles without --opt.
(define (count-occ s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0) (c 0))
      (cond ((> (+ i nsub) ns) c)
            ((string=? (substring s i (+ i nsub)) sub) (loop (+ i nsub) (+ c 1)))
            (else (loop (+ i 1) c))))))

;; two-fn cycle, branching 1
(ev "(declare pongf)")
(ev "(def pingf (fn* ([n] (if (< n 1) 0 (pongf (- n 1))))))")
(ev "(def pongf (fn* ([n] (if (< n 1) 1 (pingf (- n 1))))))")
(let ((e (emitf "u" "(fn* ([n] (pingf n)))")))
  (ok "a mutual-recursion member is not inlined (one pingf call remains)"
      (= 1 (count-occ (code-part e) "pingf")))
  (ok "its cycle partner is not pasted in" (= 0 (count-occ (code-part e) "pongf"))))
(ok "mutual pair still runs: (pingf 4) = 0" (= 0 (jnum->exact (ev "((fn* ([] (pingf 4))))"))))
(ok "mutual pair still runs: (pingf 3) = 1" (= 1 (jnum->exact (ev "((fn* ([] (pingf 3))))"))))

;; branching cycle (the jolt-682 shape): bx-a calls bx-b TWICE, bx-b calls back.
;; Unrolling this doubles per round — 2^rounds copies — so the count assertion
;; is the size gate.
(ev "(declare bx-b)")
(ev "(def bx-a (fn* ([n] (if (< n 1) 0 (+ (bx-b (- n 1)) (bx-b (- n 2)))))))")
(ev "(def bx-b (fn* ([n] (bx-a (- n 1)))))")
(let ((e (emitf "u" "(fn* ([n] (bx-a n)))")))
  (ok "a branching cycle member is not inlined" (= 1 (count-occ (code-part e) "bx-a")))
  (ok "no cycle layer is pasted in" (= 0 (count-occ (code-part e) "bx-b"))))
(ok "branching cycle still runs: (bx-a 3) = 0" (= 0 (jnum->exact (ev "((fn* ([] (bx-a 3))))"))))

;; an ACYCLIC helper called from cycle code still inlines — only membership in
;; the cycle disqualifies, not proximity to one.
(ev "(def tinyh (fn* ([x] (+ x 1))))")
(let ((e (emitf "u" "(def bx-caller (fn* ([n] (bx-a (tinyh n)))))")))
  (ok "an acyclic helper beside a cycle call is still inlined"
      (= 0 (count-occ (code-part e) "tinyh")))
  (ok "while the cycle call stays real" (= 1 (count-occ (code-part e) "bx-a"))))

;; --- code growth is bounded per top-level form (#1059) ------------------------
;; Each splice passes the per-site size check on its own, so a form with many
;; call sites of a mid-size callee used to grow by the callee's size at EVERY
;; site: clojure.test's `is` inlined do-report twice per assertion, a 41-line
;; deftest emitted 69KB of Scheme, and inlining was half of a test app's build.
;; The growth a form may take is bounded, charged at the NET size of a splice
;; (the body minus the call it replaces), so tiny helpers stay free.
(define (vec-of-calls f n)
  (let loop ((i 0) (acc ""))
    (if (= i n) (string-append "[" acc "]")
        (loop (+ i 1) (string-append acc " (" f " (+ y " (number->string i) "))")))))
(ev "(def midh (fn* ([x] (if (< x 0) (- 0 x) (if (> x 100) (* x 2) (+ (* x 3) (* x 4) (* x 5) (* x 6) (* x 7) (* x 8) (* x 9) (* x 10) (* x 11) (* x 12) (* x 13) (* x 14)))))))")
(let* ((src (string-append "(fn* ([y] " (vec-of-calls "midh" 300) "))"))
       (e (emitf "u" src))
       (left (count-occ (code-part e) "midh")))
  (ok "a mid-size callee at 300 sites is inlined at some of them" (< left 300))
  (ok "…but not at all of them: the form's growth is bounded" (> left 0)))
(ok "a partly inlined form still computes the same values"
    (equal? (map jnum->exact (seq->list (ev (string-append "((fn* ([y] " (vec-of-calls "midh" 300) ")) 1)"))))
            (let loop ((i 299) (acc '()))
              (if (< i 0) acc
                  (loop (- i 1) (cons (let ((x (+ 1 i))) (if (> x 100) (* x 2) (* x 102))) acc))))))
(let ((e (emitf "u" (string-append "(fn* ([y] " (vec-of-calls "add1" 300) "))"))))
  (ok "a tiny callee is inlined at every one of 300 sites" (= 0 (count-occ (code-part e) "add1"))))

;; --- scalar-replace moves field values only where they mean the same ----------
;; Dropping (let [m {:x a ..}] ..) moves `a` to each (:x m). Under a binder that
;; rebinds `a` it read the inner one (12, not 3); into a fn literal it left the
;; literal's captures naming m, which no longer exists, so a build's image maker
;; read an unbound variable. Both keep the map; the plain case still folds.
(ok "scalar-replace does not move a value under a binder that shadows its local"
    (= 3 (jnum->exact (ev "((fn* ([a] (let [m {:x a :y 2}] (let [a 10] (+ (:x m) (:y m)))))) 1)"))))
(ok "scalar-replace does not move a value under a loop binding that shadows it"
    (= 6 (jnum->exact (ev "((fn* ([a] (let [m {:x a :y 2}] (loop [a 0 acc 0] (if (< a 2) (recur (inc a) (+ acc (:x m) (:y m))) acc))))) 1)"))))
(ok "scalar-replace does not move a value under a fn param that shadows it"
    (= 3 (jnum->exact (ev "(((fn* ([a] (let [m {:x a :y 2}] (fn* ([a] (+ (:x m) (:y m))))))) 1) 100)"))))
(let ((e (code-part (emitf "u" "(fn* ([a] (let [m {:x a :y 2}] (fn* ([] (+ (:x m) (:y m)))))))"))))
  (ok "a map a fn literal captures is not scalar-replaced into it" (has? e "amap-slots->pmap")))
(let ((e (code-part (emitf "u" "(fn* ([a] (let [m {:x a :y 2}] (+ (:x m) (:y m)))))"))))
  (ok "a map read only in its own scope is still scalar-replaced" (not (has? e "amap-slots->pmap"))))
;; Moving a value that allocates to two reads made two objects where the map
;; held one: (identical? (:x m) (:x m)) was false, and a read in a loop body made
;; one per iteration. The value is bound once instead (records: build-smoke.sh).
(ok "a field read twice whose value allocates is read as one object"
    (eq? #t (ev "((fn* ([a] (let [m {:x [a] :y 2}] (identical? (:x m) (:x m))))) 1)")))
(ok "a field read once per loop iteration whose value allocates is one object"
    (eq? #t (ev "((fn* ([a] (let [m {:x [a] :y 2}] (loop [i 0 prev (:x m)] (if (< i 2) (recur (inc i) (:x m)) (identical? prev (:x m))))))) 1)")))
(let ((e (code-part (emitf "u" "(fn* ([a] (let [m {:x [a] :y 2}] (+ (count (:x m)) (:y m)))))"))))
  (ok "a map with an allocating field is still scalar-replaced" (not (has? e "amap-slots->pmap"))))
(let ((e (code-part (emitf "u" "(fn* ([a] (let [m {:x [a] :y (nil? a)}] (+ (count (:x m)) (if (:y m) 1 2) (if (:y m) 3 4)))))"))))
  (ok "a non-allocating field read twice is still scalar-replaced" (not (has? e "amap-slots->pmap"))))
(ok "a nested map read twice computes the same value" (= 4 (jnum->exact (ev "((fn* ([a] (let [m {:p {:x a :y 2} :q 1}] (+ (:x (:p m)) (:y (:p m)) (:q m))))) 1)"))))

;; Check executable bodies and run the same forms with and without the passes.
(define (compare-map-scalarization name src args expected removes-map?)
  (let ((run (string-append "(" src " " args ")"))
        (map-constructor "(amap-slots->pmap "))
    (set-optimize! #f)
    (set-direct-link-flag! #f)
    (let ((baseline (ev run))
          (ordinary (code-part (emitf "u" src))))
      (set-optimize! #t)
      (set-direct-link-flag! #t)
      (ok (string-append name ": executable constructor")
          (and (has? ordinary map-constructor)
               (eq? (not (has? (code-part (emitf "u" src)) map-constructor)) removes-map?)))
      (ok (string-append name ": equal runtime values and effects")
          (and (clj= baseline (ev expected)) (clj= baseline (ev run)))))))

(compare-map-scalarization "direct lookup"
  "(fn [x] (let [m {:k x}] (:k m)))" "7" "7" #t)
(compare-map-scalarization "adjacent aliases and keyword projections"
  "(fn [x] (let [m {:k x :other false} a m b a hit (get b :k)]
             [hit (:k m) (:other b) (:missing a)]))" "7" "[7 7 false nil]" #t)
(compare-map-scalarization "pooled literal behind an alias"
  "(fn [] (let [m {:k 7} a m] (:k a)))" "" "7" #t)
(compare-map-scalarization "nested alias preserves one allocating field"
  "(fn [x] (let [m {:k [x]}] (let [a m] [(identical? (:k a) (:k m)) (:k a)])))"
  "7" "[true [7]]" #t)
(compare-map-scalarization "effects outside the aliased map's scope"
  "(fn [x] (let [seen (atom [])]
             (swap! seen conj :before)
             [(let [m {:k x} a m] (:k a)) (swap! seen conj :after)]))"
  "7" "[7 [:before :after]]" #t)
(compare-map-scalarization "published aggregate"
  "(fn [x] (let [m {:k x} a m] [a (:k a)]))" "7" "[{:k 7} 7]" #f)
(compare-map-scalarization "observed map identity"
  "(fn [x] (let [m {:k x} a m] [(identical? m a) (:k a)]))" "7" "[true 7]" #f)
(compare-map-scalarization "shadowed value and alias"
  "(fn [x] (let [m {:k x} a m] (let [x :inner a a] [(:k a) x])))"
  "7" "[7 :inner]" #f)
(compare-map-scalarization "field read under a shadowing binder"
  "(fn [x] (let [m {:k x} a m] (let [x :inner] [(:k a) x])))"
  "7" "[7 :inner]" #f)
(compare-map-scalarization "live previous loop value"
  "(fn [x] (let [m {:k x} a m]
             (loop [cur a old nil]
               (if old [(identical? old a) (:k old) (:k cur)]
                 (recur (assoc cur :k (inc x)) cur)))))" "7" "[true 7 8]" #f)
(compare-map-scalarization "locally consumed container needs further lowering"
  "(fn [x] (let [m {:k x} a m box [a]] (:k (nth box 0))))" "7" "7" #f)
(compare-map-scalarization "retained closure"
  "(fn [x] (let [closures (let [m {:k x} a m] [(fn [] (:k a))]) f (nth closures 0)]
             [(f) (f)]))"
  "7" "[7 7]" #f)
(compare-map-scalarization "throwing unread sibling"
  "(fn [x] (let [seen (atom [])]
             (try (let [m {:k x :bad (do (swap! seen conj :field) (/ 1 0))} a m]
                    (swap! seen conj :body) (:k a))
                  (catch ArithmeticException _ [:threw @seen]))))"
  "7" "[:threw [:field]]" #f)

;; Public eval's compiler-options path shares the emitter's unit, so a
;; substitution can mark the fixpoint dirty and expose a let on the next pass.
(set-optimize! #f)
(set-direct-link-flag! #f)
(for-each
  (lambda (direct?)
    (define (eval-form src)
      (ev (format "(binding [*compiler-options* {:direct-linking ~a}] (eval (read-string ~s)))"
                  (if direct? "true" "false") src)))
    (for-each
      (lambda (alias?)
        (let ((bind (if alias? " a m" "")) (m (if alias? "a" "m")))
          (ok "a field's inner let does not capture a later read of the enclosing x"
              (clj= (ev "[9 7]")
                    (eval-form (format "((fn [x] (let [m {:k (let [x 9] x)}~a v (:k ~a)] [v x])) 7)"
                                       bind m))))
          (ok "two unpooled reads of a quoted field share one object"
              (eq? #t (eval-form (format "(let [m {:k '(1)}~a] (identical? (:k ~a) (:k ~a)))"
                                         bind m m))))
          (ok "an unused map still checks for keys that become duplicates after folding"
              (equal? "Duplicate key: :k"
                      (eval-form (format "(try (let [m {(if true :k :other) 1 :k 2}~a] :ok) (catch IllegalArgumentException e (.getMessage e)))"
                                         bind))))))
      '(#f #t))
    (ok "flattening a let initializer preserves the scope of subsequent initializers"
        (clj= (ev "[9 7]")
              (eval-form "((fn [x] (let [v (let [x 9] x) outer x] [v outer])) 7)")))
    (ok "a keyword projection still checks a discarded map literal for duplicate keys"
        (equal? "Duplicate key: :k"
                (eval-form "(try (:k {(if true :k :other) 1 :k 2}) (catch IllegalArgumentException e (.getMessage e)))")))
    (ok "a discarded sibling map still checks for duplicate keys"
        (equal? "Duplicate key: :k"
                (eval-form "(try (:value {:value 1 :unused {(if true :k :other) 1 :k 2}}) (catch IllegalArgumentException e (.getMessage e)))"))))
  '(#f #t))
(set-optimize! #t)
(set-direct-link-flag! #t)

(let ((set-debug-locals! (var-deref "jolt.backend-scheme" "set-debug-locals!")))
  (set-debug-locals! #t)
  (ok "instrumented locals keep even a projection-only aliased map"
      (has? (code-part (emitf "u" "(fn [x] (let [m {:k x} a m] (:k a)))")) "(amap-slots->pmap "))
  (set-debug-locals! #f))

;; Lexical facts describe occurrences in a particular IR revision. Scheme
;; paths below use symbols for the IR's keyword keys and integers for indices.
(define facts-unit ((var-deref "jolt.passes.types" "new-unit")))
(define body-facts (var-deref "jolt.passes.facts" "body-facts"))
(define binding-at (var-deref "jolt.passes.facts" "binding-at"))
(define origins-at (var-deref "jolt.passes.facts" "origins-at"))
(define (fact-kw name) (keyword #f (symbol->string name)))
(define (ir-path steps)
  (apply jolt-vector (map (lambda (s) (if (symbol? s) (fact-kw s) s)) steps)))
(define (ir-at ir steps)
  (fold-left (lambda (x s)
               (if (symbol? s) (jolt-get x (fact-kw s) jolt-nil) (jolt-nth x s)))
             ir steps))
(define (fact-ir src) (jolt-ce-analyze (make-analyze-ctx "u") (jolt-ce-read src)))
(define (facts-for src) (body-facts facts-unit (fact-ir src)))
(define (binds? facts use binder)
  (clj= (binding-at facts (ir-path use)) (ir-path binder)))
(define (origin? facts use kind . path)
  (let* ((base (jolt-hash-map (fact-kw 'kind) (fact-kw kind)))
         (entry (if (eq? kind 'unknown)
                    base
                    (jolt-assoc base (fact-kw (if (eq? kind 'argument) 'binding 'site))
                                (ir-path path)))))
    (clj= (origins-at facts (ir-path use)) (jolt-hash-set entry))))

(let ((f (facts-for "(fn* ([^double x ^long y z] (let* [a x b y c z] [a b c])))")))
  (ok "numeric entry coercions give aliases an unknown origin"
      (and (origin? f '(arities 0 body body items 0) 'unknown)
           (origin? f '(arities 0 body body items 1) 'unknown)))
  (ok "the adjacent uncoerced alias still has its incoming origin"
      (origin? f '(arities 0 body body items 2) 'argument 'arities 0 'params 2)))

(let ((f (facts-for "(fn* ([x] (let* [outer x x [x] alias x alias2 alias] [outer x alias2])))")))
  (ok "a shadowing let initializer sees the outer parameter"
      (binds? f '(arities 0 body bindings 1 1 items 0) '(arities 0 params 0)))
  (ok "after shadowing, the same name resolves to the new binding"
      (binds? f '(arities 0 body body items 1) '(arities 0 body bindings 1 0)))
  (ok "a direct alias initializer resolves its original binding"
      (binds? f '(arities 0 body bindings 2 1) '(arities 0 body bindings 1 0)))
  (ok "the use still resolves to the last alias's own lexical binder"
      (binds? f '(arities 0 body body items 2) '(arities 0 body bindings 3 0)))
  (ok "an alias of a parameter has the caller-supplied origin"
      (origin? f '(arities 0 body body items 0) 'argument 'arities 0 'params 0))
  (ok "an alias chain retains the original execution-time constructor"
      (origin? f '(arities 0 body body items 2) 'allocation 'arities 0 'body 'bindings 1 1)))

(let* ((outer (fact-ir "(let* [x 1] (let* [x x] x))"))
       (inner (ir-at outer '(body)))
       (use (ir-at inner '(bindings 0 1)))
       (shared (jolt-assoc outer (fact-kw 'body) (jolt-assoc inner (fact-kw 'body) use)))
       (f (body-facts facts-unit shared)))
  (ok "one shared local node resolves independently in two lexical scopes"
      (and (eq? use (ir-at shared '(body body)))
           (binds? f '(body bindings 0 1) '(bindings 0 0))
           (binds? f '(body body) '(body bindings 0 0)))))

(let ((f (facts-for "(letfn [(a [] [a b]) (b [] a)] [a b])")))
  (ok "letfn binds a later sibling in an earlier closure"
      (binds? f '(bindings 0 1 arities 0 body items 1) '(bindings 1 0)))
  (ok "letfn binds an earlier sibling in a later closure"
      (binds? f '(bindings 1 1 arities 0 body) '(bindings 0 0)))
  (ok "the function self name shadows its letfn binder inside the function"
      (binds? f '(bindings 0 1 arities 0 body items 0) '(bindings 0 1 name)))
  (ok "the earlier closure sees the later sibling's constructor origin"
      (origin? f '(bindings 0 1 arities 0 body items 1) 'allocation 'bindings 1 1)))

(let ((f (facts-for "(fn* self ([self] self) ([x y & more] [self x y more]))")))
  (ok "a parameter can shadow the fn self name in just its own arity"
      (binds? f '(arities 0 body) '(arities 0 params 0)))
  (ok "the fn self binder is visible in the second arity"
      (binds? f '(arities 1 body items 0) '(name)))
  (ok "rest has its own formal and an incoming origin"
      (and (binds? f '(arities 1 body items 3) '(arities 1 rest))
           (origin? f '(arities 1 body items 3) 'argument 'arities 1 'rest))))

(let ((f (facts-for "(let* [e [:outer]] (try e (catch Throwable e e) (finally e)))")))
  (ok "the source catch binding receives the exception from the IR catch binder"
      (binds? f '(body catch-body bindings 0 1) '(body catch-sym)))
  (ok "the catch name shadows the outer local only in the handler"
      (and (binds? f '(body catch-body body) '(body catch-body bindings 0 0))
           (binds? f '(body body) '(bindings 0 0))
           (binds? f '(body finally) '(bindings 0 0))
           (origin? f '(body catch-body body) 'unknown))))

(let* ((ir (fact-ir "(fn* ([x] (let* [a [x] b [x] c [[1] :k] q '(x) r (identity x)] [a b c q r])))"))
       (b '(arities 0 body))
       (let-ir (ir-at ir b))
       (alloc (ir-at let-ir '(bindings 0 1)))
       ;; The two constructor occurrences now use the same map object.
       (bindings (ir-at let-ir '(bindings)))
       (shared (jolt-assoc let-ir (fact-kw 'bindings)
                 (jolt-assoc bindings 1 (jolt-vector "b" alloc))))
       (original (body-facts facts-unit ir))
       (f (body-facts facts-unit shared)))
  (ok "equal constructor syntax yields two distinct origin paths"
      (and (origin? original '(arities 0 body body items 0) 'allocation 'arities 0 'body 'bindings 0 1)
           (origin? original '(arities 0 body body items 1) 'allocation 'arities 0 'body 'bindings 1 1)))
  (ok "even the same constructor node object has distinct occurrences"
      (and (eq? (ir-at shared '(bindings 0 1)) (ir-at shared '(bindings 1 1)))
           (origin? f '(body items 0) 'allocation 'bindings 0 1)
           (origin? f '(body items 1) 'allocation 'bindings 1 1)))
  (ok "constant collection and quoted data have potentially shared literal origins"
      (and (origin? f '(body items 2) 'literal 'bindings 2 1)
           (origin? f '(body items 3) 'literal 'bindings 3 1)))
  (ok "an opaque invoke and a binding outside the requested root have unknown origins"
      (and (origin? f '(body items 4) 'unknown)
           (origin? f '(bindings 0 1 items 0) 'unknown)
           (jolt-nil? (binding-at f (ir-path '(bindings 0 1 items 0)))))))

(let* ((src "(fn* ([x] (loop* [i 0 prev nil] (let* [obj [x] alias obj] (if (= i 0) (recur 1 alias) [(= alias prev) (identical? alias prev)])))))")
       (f (facts-for src)))
  (ok "a loop body's alias carries its constructor site onto the backedge"
      (origin? f '(arities 0 body body body then args 1) 'allocation 'arities 0 'body 'body 'bindings 0 1))
  (ok "the outer fn parameter remains incoming across an inner loop's recur"
      (origin? f '(arities 0 body body bindings 0 1 items 0) 'argument 'arities 0 'params 0))
  (ok "a loop-carried local might have been rebound by recur"
      (and (binds? f '(arities 0 body body body else items 0 args 1) '(arities 0 body bindings 1 0))
           (origin? f '(arities 0 body body body else items 0 args 1) 'unknown)))
  (ok "two evaluations of one loop site produce equal objects with distinct identities"
      (equal? (seq->list (ev (string-append "(" src " 7)"))) '(#t #f))))
(let ((f (facts-for "(fn* ([x] (let* [alias x] (if x alias (recur [x])))))")))
  (ok "aliases of a fn parameter can see a value rebound by fn recur"
      (origin? f '(arities 0 body body then) 'unknown)))
(let ((f (facts-for "(fn* ([x] (loop* [x [x] y x] y)))")))
  (ok "loop initializers have sequential scope and see first-entry values"
      (and (binds? f '(arities 0 body bindings 0 1 items 0) '(arities 0 params 0))
           (origin? f '(arities 0 body bindings 1 1) 'allocation 'arities 0 'body 'bindings 0 1))))
(let ((f (facts-for "(fn* ([x] {x x}))")))
  (ok "a map key and value are separate child occurrences"
      (and (binds? f '(arities 0 body pairs 0 0) '(arities 0 params 0))
           (binds? f '(arities 0 body pairs 0 1) '(arities 0 params 0)))))

(ev "(def facts-copy (fn* ([p] [p p])))")
(let* ((ctx (make-analyze-ctx "u"))
       (before (jolt-ce-analyze ctx (jolt-ce-read "(fn* ([x] (let* [a [x]] (facts-copy a))))")))
       (original (body-facts facts-unit before))
       (after (jolt-ce-run-passes before ctx))
       (rewritten (body-facts facts-unit after))
       (other (body-facts ((var-deref "jolt.passes.types" "new-unit")) after)))
  (ok "the same body revision hits the unit's fact cache"
      (eq? rewritten (body-facts facts-unit after)))
  (ok "different compilation units compute separate snapshots" (not (eq? rewritten other)))
  (ok "the pass pipeline splices the call"
      (not (has? (code-part (jolt-ce-emit after)) "facts-copy")))
  (ok "recomputing after a splice finds both substituted uses in caller scope"
      (and (origin? original '(arities 0 body body) 'unknown)
           (binds? original '(arities 0 body body args 0) '(arities 0 body bindings 0 0))
           (binds? rewritten '(arities 0 body body items 0) '(arities 0 body bindings 0 0))
           (binds? rewritten '(arities 0 body body items 1) '(arities 0 body bindings 0 0))
           (origin? rewritten '(arities 0 body body items 1) 'allocation 'arities 0 'body 'bindings 0 1)))
  (let* ((opaque (jolt-hash-map (fact-kw 'op) (fact-kw 'future-op)))
         (mixed (jolt-hash-map (fact-kw 'op) (fact-kw 'vector)
                               (fact-kw 'items) (jolt-vector after opaque)))
         (f (body-facts facts-unit mixed)))
    (ok "an unrecognized IR op discards partial facts"
        (and (origin? f '(items 1) 'unknown)
             (origin? f '(items 0 arities 0 body body items 1) 'unknown)))))

(let* ((leaf (fact-ir "1"))
       (deep (let loop ((n 200) (ir leaf))
               (if (zero? n) ir
                   (loop (- n 1) (jolt-hash-map (fact-kw 'op) (fact-kw 'vector)
                                                (fact-kw 'items) (jolt-vector ir))))))
       (node (jolt-hash-map (fact-kw 'op) (fact-kw 'vector)
                            (fact-kw 'items) (jolt-vector leaf deep))))
  (ok "an exhausted analysis budget discards even the prefix's facts"
      (origin? (body-facts facts-unit node) '(items 0) 'unknown)))

(define projection-at (var-deref "jolt.passes.facts" "projection-at"))
(define contained-at (var-deref "jolt.passes.facts" "contained-at"))
(define captures-at (var-deref "jolt.passes.facts" "captures-at"))
(define confined? (var-deref "jolt.passes.facts" "confined?"))
(define may-publish? (var-deref "jolt.passes.facts" "may-publish?"))
(define observations-at (var-deref "jolt.passes.facts" "observations-at"))
(define (in-body . path) (ir-path (append '(arities 0 body) path)))
(define map-site (in-body 'bindings 0 1))
(define box-site (in-body 'bindings 1 1))
(define ret-site (in-body 'body))
(define (fact-get facts key) (jolt-get facts (fact-kw key) jolt-nil))
(define (projects? f path present? from)
  (let ((p (projection-at f path)))
    (and (not (jolt-nil? p))
         (eq? (fact-get p 'present?) present?)
         (clj= (fact-get p 'origins) (origins-at f from)))))

(let ((f (facts-for "(fn [x] (loop* [m {:k x} saved m] (identical? m saved)))")))
  (ok "identity observed through a loop slot reaches its confined initializer"
      (and (confined? f map-site)
           (clj= (observations-at f map-site) (jolt-hash-set (fact-kw 'identity))))))
(let ((f (facts-for "(fn [x] (loop* [m {:k x} saved m] (class saved)))")))
  (ok "class through a loop slot retains its representation and opaque observations"
      (clj= (observations-at f map-site)
            (jolt-hash-set (fact-kw 'representation) (fact-kw 'unknown)))))

(let ((f (facts-for "(fn [x] (let [m {:k x} box [m]] (:k (nth box 0))))")))
  (ok "nested projections forward only the selected field"
      (and (projects? f (in-body 'body 'args 0) #t map-site)
           (projects? f ret-site #t (in-body 'bindings 0 1 'pairs 0 1))))
  (ok "projecting through both containers confines both allocations"
      (and (confined? f map-site) (confined? f box-site)
           (may-publish? f (in-body 'bindings 0 1 'pairs 0 1))
           (clj= (contained-at f box-site) (origins-at f map-site)))))

(let* ((prefix "(fn [x] (let [m {:k x} alias m box [alias]] ")
       (local (facts-for (string-append prefix "(:k (nth box 0))))")))
       (published (facts-for (string-append prefix "box))")))
       (box (in-body 'bindings 2 1)))
  (ok "returning the same container publishes its aliased map"
      (and (confined? local map-site) (confined? local box)
           (may-publish? published map-site) (may-publish? published box))))

(for-each
 (lambda (tail)
   (let ((f (facts-for (string-append "(fn [x sink] (let [m {:k x} box [[m] [m]] other [x]] " tail "))"))))
     (ok (string-append "shared child publishes transitively via " tail)
         (and (not (confined? f map-site)) (not (confined? f box-site))
              (confined? f (in-body 'bindings 2 1))))))
 '("box" "(reset! sink box)" "(set! *print-length* box)" "(throw box)"
   "(try (throw box) (catch Throwable e nil))"))

(let ((f (facts-for "(fn [x callback] (let [field [x] m {:k field} box [m]] (callback (:k (nth box 0)))))")))
  (ok "passing only a projected child to an opaque callback confines its holders"
      (and (not (confined? f (in-body 'bindings 0 1)))
           (confined? f (in-body 'bindings 1 1))
           (confined? f (in-body 'bindings 2 1)))))

(let ((f (facts-for "(fn [x i] (let [m {:k x} box [m] other [x]] (nth box i)))")))
  (ok "unknown indexing conservatively exposes possible children and leaves unrelated sites local"
      (and (jolt-nil? (projection-at f ret-site))
           (not (confined? f map-site)) (not (confined? f box-site))
           (confined? f (in-body 'bindings 2 1)))))

(let ((f (facts-for "(fn [x] (let [m {:k x} box [m]] (identical? m (nth box 0))))")))
  (ok "local identity is observable even while both objects are confined"
      (and (confined? f map-site) (confined? f box-site)
           (clj= (observations-at f map-site) (jolt-hash-set (fact-kw 'identity)))
           (clj= (observations-at f box-site) (jolt-hash-set)))))

(let ((f (facts-for "(fn [x] (let [m {:k x}] (class m)))")))
  (ok "representation and opaque uses are reported separately"
      (and (not (confined? f map-site))
           (clj= (observations-at f map-site)
                 (jolt-hash-set (fact-kw 'representation) (fact-kw 'unknown))))))

(let ((f (facts-for "(fn [x] (let [m {:a nil :b false :c (x)} v [nil false (x)]] [(get m :a x) (:b m x) (:c m) (nth v 0 x) (nth v 1) (nth v 2) (:z m x)]))")))
  (ok "nil, false, and unknown values preserve literal key and slot presence"
      (and (projects? f (in-body 'body 'items 0) #t (in-body 'bindings 0 1 'pairs 0 1))
           (projects? f (in-body 'body 'items 1) #t (in-body 'bindings 0 1 'pairs 1 1))
           (projects? f (in-body 'body 'items 2) #t (in-body 'bindings 0 1 'pairs 2 1))
           (projects? f (in-body 'body 'items 3) #t (in-body 'bindings 1 1 'items 0))
           (projects? f (in-body 'body 'items 4) #t (in-body 'bindings 1 1 'items 1))
           (projects? f (in-body 'body 'items 5) #t (in-body 'bindings 1 1 'items 2))
           (projects? f (in-body 'body 'items 6) #f (in-body 'body 'items 6 'args 1)))))

(let ((f (facts-for "(fn [x] (let [key [x] val [x] m {key val} members #{key val}] [m members]))")))
  (ok "maps retain both keys and values and sets retain members"
      (let ((children (jolt-conj2 (origins-at f map-site)
                                  (jolt-first (origins-at f box-site)))))
        (and (clj= (contained-at f (in-body 'bindings 2 1)) children)
             (clj= (contained-at f (in-body 'bindings 3 1)) children)))))
(let ((f (facts-for "(fn [] (get #{false} false :missing))")))
  (ok "set lookup forwards the stored member"
      (projects? f (in-body) #t (in-body 'args 0 'items 0))))

(let* ((f (facts-for "(fn [x] (let [m {:k x} f (fn [m] (fn [] m)) g (fn [] (fn [] m))] g))"))
       (g (captures-at f (in-body 'bindings 2 1)))
       (capture (jolt-get g "m" jolt-nil)))
  (ok "nested capture follows lexical shadowing and returning a closure publishes its capture"
      (and (clj= (captures-at f box-site) (jolt-hash-map))
           (clj= (fact-get capture 'binding) (in-body 'bindings 0 0))
           (clj= (fact-get capture 'origins) (origins-at f map-site))
           (not (confined? f map-site)))))

(ev "(def facts-make-fn (fn* ([p q] (fn* ([] [p q])))))")
(let* ((ctx (make-analyze-ctx "u"))
       (ir (jolt-ce-analyze ctx (jolt-ce-read "(fn [x] (let [m {:k x}] (facts-make-fn m false)))")))
       (spliced (jolt-ce-run-passes ir ctx))
       (closure (ir-at spliced '(arities 0 body body)))
       (captures (captures-at (body-facts facts-unit spliced) ret-site)))
  (ok "spliced image metadata retains a renamed live binding and carries false as data"
      (and (clj= (fact-get closure 'free-names) (jolt-vector "p" "q"))
           (clj= (fact-get closure 'live-names) (jolt-vector "m" (jolt-vector #f)))
           (clj= (fact-get (jolt-get captures "m" jolt-nil) 'binding) (in-body 'bindings 0 0))
           (= 1 (jolt-count captures)))))

(let ((f (facts-for "(fn [x] (let [m {:k x} f (fn [] m) a f] (:k (a))))")))
  (ok "a proven local zero-arg invocation forwards its capture without publication"
      (and (confined? f map-site) (confined? f box-site)
           (projects? f ret-site #t (in-body 'bindings 0 1 'pairs 0 1)))))
(let ((f (facts-for "(fn [x] (let [m {:k x}] ((fn [] (throw m)))))")))
  (ok "an immediately invoked closure can still publish a capture through its effects"
      (not (confined? f map-site))))

(let* ((f (facts-for "(fn [x] (let [field [x] m {:k field} f (^:once fn* [] (let [box [m]] (:k (nth box 0) :missing)))] (f)))"))
       (once-site (in-body 'bindings 2 1))
       (capture (jolt-get (captures-at f once-site) "m" jolt-nil))
       (read-site (in-body 'bindings 2 1 'arities 0 'body 'bindings 0 1 'items 0))
       (lookup-site (in-body 'bindings 2 1 'arities 0 'body 'body))
       (receiver-site (in-body 'bindings 2 1 'arities 0 'body 'body 'args 0)))
  (ok "a once closure keeps the lexical binder and possible first-call value"
      (and (clj= (fact-get capture 'binding) (in-body 'bindings 1 0))
           (clj= (binding-at f read-site) (in-body 'bindings 1 0))
           (clj= (fact-get capture 'origins) (origins-at f box-site))
           (jolt-contains? (origins-at f read-site) (jolt-hash-map (fact-kw 'kind) (fact-kw 'unknown)))
           (jolt-contains? (origins-at f read-site) (jolt-first (origins-at f box-site)))))
  (ok "a cleared capture invalidates derived field proofs and may still publish its child"
      (and (projects? f receiver-site #t read-site)
           (jolt-nil? (projection-at f lookup-site))
           (origin? f '(arities 0 body body) 'unknown)
           (may-publish? f map-site))))

(ok "a once closure retried after throwing reads the cleared capture"
    (clj= (ev "(let [m {:k [1]} f (^:once fn* [fail?] (let [v (:k m :missing)] (if fail? (throw (ex-info \"retry\" {})) v)))] [(try (f true) (catch Throwable e :threw)) (f false)])")
          (jolt-vector (fact-kw 'threw) (fact-kw 'missing))))

(for-each
  (lambda (hint)
    (let* ((f (facts-for (string-append "(fn [x] ((fn* ^" hint " [] x)))")))
           (read (in-body 'fn 'arities 0 'body))
           (returns (origins-at f (in-body))))
      (ok (string-append "^" hint " closure return may preserve or replace the captured argument")
          (and (origin? f '(arities 0 body fn arities 0 body) 'argument 'arities 0 'params 0)
               (jolt-contains? returns (jolt-first (origins-at f read)))
               (jolt-contains? returns (jolt-hash-map (fact-kw 'kind) (fact-kw 'unknown)))
               (may-publish? f read)))))
  '("double" "long"))
(let* ((source "(fn [v] (let [m {:k v}] (try ((fn* ^double [] m)) (catch Throwable e (ex-message e)))))")
       (f (facts-for source)))
  (ok "a caught return coercion can report the map's runtime representation"
      (and (jolt-contains? (observations-at f map-site) (fact-kw 'representation))
           (has? (ev (string-append "(" source " 1)")) "PersistentArrayMap"))))
(let* ((body (fact-ir "(fn [x] (let [m {:k x}] (try ((fn* ^double [] m)) nil (catch Throwable e e))))"))
       (unit ((var-deref "jolt.passes.types" "new-unit")))
       (plain (body-facts unit body)))
  ((var-deref "clojure.core" "reset!") (fact-get unit 'debug-locals?) #t)
  (let ((debug (body-facts unit body)))
    (ok "a returned debug exception may retain the map even if the normal cast result is discarded"
        (and (confined? plain map-site) (may-publish? debug map-site)
             (not (confined? debug map-site))))))
(let ((d (ev "((fn [x] ((fn* ^double [] x))) 1)"))
      (n (ev "((fn [x] ((fn* ^long [] x))) 18446744073709551616N)")))
  (ok "fixed-arity return coercion widens to double and wraps long"
      (and (flonum? d) (= d 1.0) (fixnum? n) (= n 0))))

;; A positional record constructor stores its arguments, so its fields are
;; contained values for escape purposes.
(ev "(defrecord FactRecord [a b])")
((var-deref "jolt.passes.types" "set-record-shapes!") facts-unit (chez-record-shapes-map))
(let* ((f (facts-for "(fn [a b] (get (FactRecord. a b) :a :missing))"))
       (first-arg (in-body 'args 0 'args 0)))
  (ok "a positional record retains both field arguments"
      (clj= (contained-at f (in-body 'args 0))
            (jolt-hash-set (jolt-first (origins-at f first-arg))
                           (jolt-first (origins-at f (in-body 'args 0 'args 1)))))))
(let ((f (facts-for "(fn [x sink] (let [m {:k x} r (FactRecord. m false)] (sink r) (:a r)))")))
  (ok "record fields exposed to mutation have no unconditional proof"
      (and (jolt-nil? (projection-at f (in-body 'body 'ret)))
           (not (confined? f map-site)) (not (confined? f box-site)))))

(define call-summary-at (var-deref "jolt.passes.facts" "call-summary-at"))
(define (argument-effects summary i)
  (jolt-nth (fact-get summary 'arguments) i))
(define (effects? summary i . effects)
  (clj= (argument-effects summary i) (apply jolt-hash-set (map fact-kw effects))))

(let* ((source "(fn [x] (let [m {:k x} read-k (fn [a] (:k a))] (read-k m)))")
       (f (facts-for source)))
  (ok "a fixed-arity helper reads the caller's map and returns just its field"
      (and (confined? f map-site)
           (effects? (call-summary-at f ret-site) 0 'read)
           (origin? f '(arities 0 body body) 'argument 'arities 0 'params 0)
           (clj= (ev (string-append "(" source " 42)")) 42))))

;; The same request lookup shape that Ring uses for path-info with a URI fallback.
(let* ((source "(fn [path uri] (let [request {:path-info path :uri uri}] (letfn [(path-for [r] (or (:path-info r) (:uri r)))] (path-for request))))")
       (f (facts-for source))
       (call (in-body 'body 'body)))
  (ok "a closed request helper reads both fields without exposing the request"
      (and (confined? f map-site) (effects? (call-summary-at f call) 0 'read)
           (equal? (ev (string-append "(" source " nil \"/fallback\")")) "/fallback")
           (equal? (ev (string-append "(" source " \"/path\" \"/fallback\")")) "/path"))))

(let* ((f (facts-for "(fn [x] (let [m {:k x} pass (fn [a] a) r (pass m)] (:k r)))"))
       (call (in-body 'bindings 2 1)))
  (ok "returning an argument forwards its original alias"
      (and (confined? f map-site) (effects? (call-summary-at f call) 0 'return-alias)
           (clj= (origins-at f call) (origins-at f map-site)))))

(for-each
  (lambda (pack read-out)
    (let* ((prefix (string-append "(fn [x] (let [m {:k x} pack " pack " r (pack m)] "))
           (inside (facts-for (string-append prefix read-out "))")))
           (outside (facts-for (string-append prefix "r))")))
           (call (in-body 'bindings 2 1)))
      (ok (string-append "the result retains its argument until it escapes: " pack)
          (and (effects? (call-summary-at inside call) 0 'return-contains)
               (confined? inside map-site) (may-publish? outside map-site)))))
  '("(fn [a] [a])" "(fn [a] (fn [] a))")
  '("(:k (nth r 0))" "(:k (r))"))

(ev "(def call-summary-sink (atom nil))")
(let* ((prefix "(fn [x] (let [m {:k x} pack (fn [a] (fn [] (reset! call-summary-sink a))) run (pack m)] ")
       (discarded (facts-for (string-append prefix "nil))")))
       (returned (facts-for (string-append prefix "run))")))
       (called (facts-for (string-append prefix "(do (run) nil)))")))
       (call (in-body 'bindings 2 1)))
  (ok "a returned closure's publication stays latent until it is called or escapes"
      (and (effects? (call-summary-at discarded call) 0 'return-contains)
           (confined? discarded map-site)
           (may-publish? returned map-site) (may-publish? called map-site))))

(let* ((f (facts-for "(fn [x] (let [m {:k x} helper (fn ([a] (:k a)) ([a b] [a b]))] (do (helper m) (helper m x) nil)))"))
       (one (call-summary-at f (in-body 'body 'statements 0)))
       (two (call-summary-at f (in-body 'body 'statements 1))))
  (ok "one closure has independent fixed-arity summaries"
      (and (confined? f map-site)
           (= (fact-get one 'arity) 1) (effects? one 0 'read)
           (= (fact-get two 'arity) 2) (effects? two 0 'return-contains)
           (effects? two 1 'return-contains))))

(let* ((source "(fn [x] (let [m {:k x} same? (fn [a b] (identical? a b))] (same? m m)))")
       (f (facts-for source))
       (s (call-summary-at f ret-site)))
  (ok "identity observations cross the call without publishing either argument"
      (and (confined? f map-site) (effects? s 0 'identity) (effects? s 1 'identity)
           (clj= (observations-at f map-site) (jolt-hash-set (fact-kw 'identity)))
           (eq? #t (ev (string-append "(" source " 1)"))))))

(ev "(defprotocol CallSummaryProtocol (call-summary-step [receiver a]))")
(for-each
  (lambda (action)
    (let* ((f (facts-for (string-append "(fn [x cb] (let [m {:k x} helper (fn [a] "
                                       action ")] (do (helper m) nil)))")))
           (summary (call-summary-at f (in-body 'body 'statements 0))))
      (ok (string-append "an argument can escape during " action)
          (and (may-publish? f map-site)
               (jolt-contains? (argument-effects summary 0) (fact-kw 'publish))
               (jolt-contains? (argument-effects summary 0) (fact-kw 'unknown))))))
  '("(reset! call-summary-sink a)" "(.toString a)" "(cb a)" "(call-summary-step cb a)"
    "(throw a)" "(class a)" "((jolt.ffi/__cfn \"puts\" [:string] :int) a)"))

(let* ((f (facts-for "(fn [x cb] (let [field [x] m {:k field} helper (fn [a] (cb (:k a)))] (helper m)))"))
       (field (in-body 'bindings 0 1))
       (map (in-body 'bindings 1 1)))
  (ok "an opaque callback can publish a field while the helper's map stays confined"
      (and (confined? f map) (may-publish? f field)
           (effects? (call-summary-at f ret-site) 0 'read))))

(ev "(deftype CallSummaryLookup [value] clojure.lang.ILookup (valAt [this k] (reset! call-summary-sink this) value) (valAt [this k d] (reset! call-summary-sink this) value))")
(let* ((source "(fn [receiver] (let [lookup (fn [a] (:k a))] (lookup receiver)))")
       (f (facts-for source)))
  (ok "a literal-key helper cannot assume how a custom receiver looks up a key"
      (and (effects? (call-summary-at f ret-site) 0 'publish 'unknown)
           (eq? #t (ev (string-append "(let [custom (CallSummaryLookup. :found)] "
                                      "(and (= :found (" source " custom)) "
                                      "(identical? custom @call-summary-sink)))"))))))

(let* ((source "(fn [x] (let [m {:k x} fail (fn [a] (throw (ex-info \"stop\" {:value a})))] (try (fail m) (catch Throwable e (:value (ex-data e))))))")
       (f (facts-for source)))
  (ok "an argument can reach the caller through a thrown exception"
      (and (may-publish? f map-site)
           (clj= (ev (string-append "(" source " 7)")) (ev "{:k 7}")))))

(let ((f (facts-for "(fn [x] (let [m {:k x} inspect (fn [a] (class a))] (inspect m)))")))
  (ok "a representation observer is carried through its helper"
      (jolt-contains? (argument-effects (call-summary-at f ret-site) 0)
                      (fact-kw 'representation))))

(let* ((f (facts-for "(fn [x cb] (let [m {:k x}] (letfn [(apply1 [f a] (f a)) (read-k [a] (:k a))] [(apply1 read-k m) (apply1 cb m)])))"))
       (inside (in-body 'body 'body 'items 0))
       (opaque (in-body 'body 'body 'items 1)))
  (ok "higher-order dispatch uses the supplied closure, with an opaque fallback"
      (and (effects? (call-summary-at f inside) 1 'read)
           (effects? (call-summary-at f opaque) 1 'publish 'unknown)
           (may-publish? f map-site))))

(for-each
  (lambda (helper)
    (let* ((f (facts-for (string-append "(fn [x] (let [m {:k x} helper " helper "] (helper m true)))")))
           (summary (call-summary-at f ret-site)))
      (ok "a recursive transfer is conservative"
          (and (may-publish? f map-site)
               (jolt-contains? (argument-effects summary 0) (fact-kw 'unknown))))))
  '("(fn self [a again?] (if again? (self a false) (:k a)))"
    "(fn [a again?] (if again? (recur a false) (:k a)))"))

(let* ((f (facts-for "(fn [x] (let [m {:k x}] (letfn [(a [v] (b v)) (b [v] (a v))] (a m))))")))
  (ok "a mutually recursive cycle does not yield confinement"
      (and (may-publish? f map-site)
           (jolt-contains? (argument-effects (call-summary-at f (in-body 'body 'body)) 0)
                           (fact-kw 'unknown)))))

(let* ((f (facts-for "(fn [x] (let [m {:k x} helper (fn [a] a)] (helper m x)))"))
       (summary (call-summary-at f ret-site)))
  (ok "unsupported arities use the conservative path"
      (and (eq? #f (fact-get summary 'resolved?)) (may-publish? f map-site))))

(set-optimize! #f)
(set-direct-link-flag! #f)

(define reducer-confinement-at (var-deref "jolt.passes.facts" "reducer-confinement-at"))
(define opaque-accumulator-frames (fact-kw 'opaque-accumulator-frames))
(define (reducer-call step seed source)
  (string-append "(reduce-kv " step " " seed " " source ")"))
(define plain-reducer "(fn [a k v] (assoc a k v))")
(define (reducer-facts source path)
  (let* ((ctx (make-analyze-ctx "u"))
         (ir (jolt-ce-run-passes (jolt-ce-analyze ctx (jolt-ce-read source)) ctx)))
    (reducer-confinement-at facts-unit ir path opaque-accumulator-frames)))
(define (guarded-reducer? proof)
  (let ((guard (fact-get proof 'guard)))
    (and (clj= (fact-get proof 'driver) (fact-kw 'native-map-reduce-kv))
         (equal? (fact-get guard 'predicate) "pmap?")
         (clj= (fact-get guard 'argument) 2))))

(define named-params
  "(fn [opts sym->kw] (reduce-kv (fn [m k v] (assoc m (sym->kw k) v)) {} (:params opts)))")
(define request-headers
  "(fn [request replace-newline] (reduce-kv (fn [m k v] (assoc m k (replace-newline v))) {} (:headers request)))")

;; Code generation happens after the analysis and can use another profile.
;; Compile the same emitted Scheme with and without named inspector locals.
(define (compile-with-inspector emitted inspector?)
  (let-values (((out bytes) (open-bytevector-output-port)))
    (parameterize ((optimize-level 2)
                   (generate-inspector-information inspector?)
                   (generate-procedure-source-information inspector?))
      (call-with-port (open-input-string emitted) (lambda (in) (compile-port in out))))
    (call-with-port (open-bytevector-input-port (bytes)) load-compiled-from-port)))

(jolt-load-string (call-with-input-file "stdlib/jolt/debug.clj" get-string-all))
(define reducer-frame-acc
  (ev "(fn [t] (some #(get (:locals %) 'acc) (jolt.debug/frames t)))"))
(define (raw-frame-value? t matches?)
  (exists (lambda (frame)
            (let loop ((i 0))
              (and (< i (frame 'length))
                   (or (matches? (((frame 'ref i) 'ref) 'value))
                       (loop (+ i 1))))))
          (sa-continuation-frames (car (jolt-thrown-cont t)))))

(for-each
  (lambda (enabled)
    (set-optimize! enabled)
    (set-direct-link-flag! enabled)
    (let* ((src "(defn inspecting-alias [x f]
                   (let [m {:observed x} alias m retained (f)]
                     [retained (:observed alias)]))")
           (emitted (emitf "u" src))
           (payload (jolt-vector 7))
           (callback (ev "(fn [] (try (throw (ex-info \"snapshot\" {})) (catch Throwable t t)))")))
      (compile-with-inspector emitted #f)
      (let* ((answer (jolt-invoke (var-deref "u" "inspecting-alias") payload callback))
             (thrown (jolt-nth answer 0)))
        (ok "a throwable retained by an opaque callback sees the aliased map in raw frames"
            (and (has? (code-part emitted) "(amap-slots->pmap ")
                 (eq? payload (jolt-nth answer 1))
                 (raw-frame-value? thrown (lambda (v)
                                            (and (pmap? v)
                                                 (eq? payload (jolt-get v (fact-kw 'observed)))))))))))
  '(#f #t))

(for-each
  (lambda (enabled)
    (set-optimize! enabled)
    (set-direct-link-flag! enabled)
    (let ((proof (reducer-facts named-params (in-body))))
      (ok "HoneySQL's named parameter reducer needs the native map guard"
          (and (guarded-reducer? proof)
               (clj= (fact-get proof 'step) (in-body 'args 0 'arities 0 'body))
               (clj= (ev (string-append "(" named-params " {:params {'p 2}} keyword)"))
                     (ev "{:p 2}")))))
    (ok "Pedestal's header reducer keeps its capture and guards the lookup result"
        (and (guarded-reducer? (reducer-facts request-headers (in-body)))
             (clj= (ev (string-append "(" request-headers " {:headers {\"accept\" \"x\\ny\"}}"
                                      " #(clojure.string/replace % \"\\n\" \",\"))"))
                   (ev "{\"accept\" \"x,y\"}"))))
    (let* ((proof (reducer-facts "(fn [x] (let [source {:z x}] (reduce-kv (fn [a k v] (assoc a (let [a k] a) v)) {} source)))"
                                 (in-body 'body)))
           (code (code-part (emitf "u" named-params))))
      (ok "a literal-map alias needs no guard and a shadowed name is independent"
          (and (clj= (fact-get proof 'driver) (fact-kw 'native-map-reduce-kv))
               (jolt-nil? (fact-get proof 'guard))))
      (ok "the query leaves the public driver and assoc in executable code"
          (and (has? code "jolt-reduce-kv") (has? code "jolt-assoc3"))))
    ;; This closed fixture binds the transform to the core keyword operation
    ;; and uses only builtin keys. Neither it nor assoc's comparisons have a
    ;; stack observer. Compile it without named locals, as in the --opt app.
    (compile-with-inspector (emitf "u" (string-append "(def named-param-fixture " named-params ")")) #f)
    (ok "the closed named-parameter fixture runs after deferred code generation"
        (clj= (jolt-invoke (var-deref "u" "named-param-fixture")
                          (ev "{:params (array-map 'p 2 'q 3)}")
                          (var-deref "clojure.core" "keyword"))
              (ev "{:p 2 :q 3}")))
    (for-each
      (lambda (step)
        (let* ((ctx (make-analyze-ctx "u"))
               (source (string-append "(defn inspecting-reducer [f m] "
                          (reducer-call step "{}" "m") ")"))
               (ir (jolt-ce-run-passes (jolt-ce-analyze ctx (jolt-ce-read source)) ctx))
               (path (ir-path '(init arities 0 body)))
               (emitted (jolt-ce-emit ir))
               (inputs (ev "(array-map :a 1 :b 2)"))
               (capture (ev "(fn [k] (try (throw (ex-info (name k) {})) (catch Throwable t t)))"))
               (a (fact-kw 'a))
               (b (fact-kw 'b)))
          (ok "generic analysis withholds the proof for a frame-observing step"
              (jolt-nil? (reducer-confinement-at facts-unit ir path)))
          (for-each
            (lambda (inspector?)
              (compile-with-inspector emitted inspector?)
              (let* ((m (jolt-invoke (var-deref "u" "inspecting-reducer") capture inputs))
                     (first-error (jolt-get m a))
                     (second-error (jolt-get m b))
                     (previous? (lambda (x)
                                  (and (pmap? x) (= 1 (pmap-cnt x))
                                       (eq? first-error (jolt-get x a))))))
                (ok "the final profile can expose both previous maps through jolt.debug/frames"
                    (if inspector?
                        (and (clj= empty-pmap (reducer-frame-acc first-error))
                             (previous? (reducer-frame-acc second-error)))
                        (and (jolt-nil? (reducer-frame-acc first-error))
                             (jolt-nil? (reducer-frame-acc second-error)))))
                (ok "the native inspector sees the previous map even without inspector metadata"
                    (raw-frame-value? second-error previous?))))
            '(#t #f))))
      '("(fn [acc k v] (assoc acc k (ex-info \"snapshot\" {})))"
        "(fn [acc k v] (assoc acc k (f k)))")))
  '(#f #t))
(set-optimize! #f)
(set-direct-link-flag! #f)

;; Even the plain assoc step can call an observer for a user-defined key.
(let* ((a (vector 'reducer-eq-probe-a))
       (b (vector 'reducer-eq-probe))
       (source (jolt-array-map a 1 b 2))
       (run "(fn [m] (reduce-kv (fn [acc k v] (assoc acc k v)) {} m))")
       (ordinary (ev run))
       (old-arms jolt-eq-arms)
       (snap #f))
  (dynamic-wind
    (lambda ()
      (register-eq-arm! (lambda (x y) (and (eq? x a) (eq? y b)))
                        (lambda (x y) (set! snap (jolt-ex-info "comparison" empty-pmap)) #f)))
    (lambda () (jolt-invoke ordinary source))
    (lambda () (set! jolt-eq-arms old-arms)))
  (ok "a key comparison can indirectly retain the previous map of a plain assoc step"
      (and snap
           (raw-frame-value? snap (lambda (x) (and (pmap? x) (= 1 (pmap-cnt x))
                                                   (eqv? 1 (jolt-get x a)))))
           (jolt-nil? (reducer-confinement-at facts-unit (fact-ir run) (in-body))))))

(ev "(def reducer-sink (atom nil))")
(ev "(defn reducer-forward [x] x)")
(for-each
  (lambda (step)
    (ok (string-append "unsupported reducer step: " step)
        (jolt-nil? (reducer-facts (reducer-call step "{}" "{:x 1}") (ir-path '())))))
  '("(fn [a k v] (assoc a k (do (reset! reducer-sink a) v)))"
    "(fn [a k v] (assoc a k [a]))"
    "(fn [a k v] (assoc a k (let [alias a] (reducer-forward [alias]))))"
    "(fn [a k v] (assoc a k (fn [] a)))"
    "(fn [a k v] (assoc a k (class a)))"
    "(fn [a k v] (assoc a k (identical? a a)))"
    "(fn [a k v] (assoc a k (count a)))"
    "(fn [a k v] (assoc a k (throw (ex-info \"stop\" {:acc a}))))"
    "(fn [a k v] (reduced (assoc a k v)))"
    "(fn [a k v] (assoc k a v))"
    "(fn [a k v & more] (assoc a k v))"
    "(fn [a k ^long v] (assoc a k v))"
    "assoc"))

(for-each
  (lambda (source path)
    (ok (string-append "unsupported reduction call: " source)
        (jolt-nil? (reducer-facts source path))))
  (list "(fn [reduce-kv] (reduce-kv (fn [a k v] (assoc a k v)) {} {}))"
        "(fn [assoc] (reduce-kv (fn [a k v] (assoc a k v)) {} {}))"
        "(reduce (fn [a [k v]] (assoc a k v)) {} {:x 1})"
        (reducer-call plain-reducer "{:seed true}" "{:x 1}")
        (reducer-call plain-reducer "(reduced {})" "{:x 1}"))
  (list (in-body) (in-body) (ir-path '()) (ir-path '()) (ir-path '())))

(let* ((ir (fact-ir (reducer-call plain-reducer "{}" "{:x 1}")))
       (annotated (jolt-assoc ir (fact-kw 'proto) "test/OtherProtocol"))
       (debug-unit ((var-deref "jolt.passes.types" "new-unit"))))
  ((var-deref "clojure.core" "reset!") (fact-get debug-unit 'debug-locals?) #t)
  (ok "protocol dispatch and debug-local frames withhold the proof"
      (and (jolt-nil? (reducer-confinement-at facts-unit annotated (ir-path '()) opaque-accumulator-frames))
           (jolt-nil? (reducer-confinement-at debug-unit ir (ir-path '()) opaque-accumulator-frames))))
  (ok "even a literal native source needs the final frame guarantee"
      (and (jolt-nil? (reducer-confinement-at facts-unit ir (ir-path '())))
           (jolt-nil? (reducer-confinement-at facts-unit ir (ir-path '()) (fact-kw 'stripped)))
           (clj= (fact-get (reducer-confinement-at facts-unit ir (ir-path '()) opaque-accumulator-frames)
                          'driver)
                 (fact-kw 'native-map-reduce-kv)))))

(ok "direct core calls and calls through values retain their different redefinition behavior"
    (eq? #t (ev "(with-redefs [reduce-kv (fn [& _] :replacement) assoc (fn [& _] :step)]
                    (let [driver @#'reduce-kv step @#'assoc]
                      (and (= {:x 1} (reduce-kv (fn [a k v] (assoc a k v)) {} {:x 1}))
                           (= :replacement (driver step {} {:x 1}))
                           (= :step (reduce-kv step {} {:x 1})))))")))

(let* ((custom (ev "(reify clojure.lang.IKVReduce
                     (kvreduce [_ f seed]
                       (let [first-step (f seed :b 2) final (f first-step :a 1)]
                         (reduced [seed first-step final]))))"))
       (run (ev "(fn [source] (reduce-kv (fn [a k v] (assoc a k v)) {} source))"))
       (answer (jolt-invoke run custom)))
  (ok "the pmap guard excludes a custom driver that keeps the seed and an old step"
      (and (pmap? (ev "{:x 1}")) (pmap? (ev "(hash-map :x 1)"))
           (not (pmap? custom)) (not (pmap? (ev "(sorted-map :x 1)")))
           (not (pmap? (ev "[:x]"))) (not (pmap? jolt-nil))
           (jolt-reduced? answer)
           (clj= (jolt-reduced-val answer) (ev "[{} {:b 2} {:b 2 :a 1}]")))))

(ok "ordinary reduction keeps published and nested snapshots distinct"
    (clj= (ev "(let [m (reduce-kv (fn [a k v] (assoc a k (do (reset! reducer-sink a) [a v])))
                                {} (array-map :a 1 :b 2))]
                 [@reducer-sink m])")
          (ev "[{:a [{} 1]} {:a [{} 1] :b [{:a [{} 1]} 2]}]")))
(ok "a callback can observe the count and class of successive persistent versions"
    (clj= (ev "(reduce-kv (fn [a k v] (assoc a k [(count a) (= clojure.lang.PersistentArrayMap (class a))]))
                        {} (array-map :a 1 :b 2))")
          (ev "{:a [0 true] :b [1 true]}")))
(ok "ordinary native traversal preserves step order, stops on reduced and never retries a throw"
    (eq? #t (ev "(let [source (hash-map :c 3 :b 2 :a 1) order (vec (keys source)) seen (atom [])
                       run (fn [f m]
                             (reduce-kv (fn [a k v] (assoc a (do (swap! seen conj [:key k]) k) (f k v))) {} m))
                       all (run (fn [k v] (swap! seen conj [:val k]) v) source)
                       trace @seen
                       _ (reset! seen [])
                       problem (ex-info \"stop\" {})
                       thrown (try (run (fn [k v] (swap! seen conj [:val k]) (throw problem)) source)
                                   (catch Throwable e e))
                       before-throw @seen
                       _ (reset! seen [])
                       early (reduce-kv (fn [a k v] (swap! seen conj k) (reduced (assoc a k v))) {} source)]
                   (and (= source all) (= trace (vec (mapcat (fn [k] [[:key k] [:val k]]) order)))
                        (= before-throw [[:key (first order)] [:val (first order)]])
                        (identical? thrown problem) (= early (select-keys source [(first order)]))
                        (= @seen [(first order)])))")))
(ok "empty native traversal returns its original seed and skips the callback"
    (eq? #t (ev "(let [seed {}] (identical? seed (reduce-kv (fn [_ _ _] (throw (ex-info \"called\" {}))) seed {})))")))
(ok "equal nine-key results expose different persistent and transient representations"
    (eq? #t (ev "(let [source (apply array-map (interleave [:a :b :c :d :e :f :g :h :i] (range 9)))
                       ordinary (reduce-kv (fn [a k v] (assoc a k v)) {} source)
                       built (persistent! (reduce-kv assoc! (transient {}) source))]
                   (and (= ordinary built) (= (vec (keys ordinary)) (vec (keys source)))
                        (= clojure.lang.PersistentArrayMap (class ordinary))
                        (= clojure.lang.PersistentHashMap (class built))
                        (not= (vec (keys ordinary)) (vec (keys built)))))")))

;; --- a letfn spliced into a let keeps its letrec* (#1808) ----------------------
;; A letfn passed as an argument to an inlined call becomes a let binding init.
;; flatten-lets hoisted its bindings into the enclosing let*, dropping the
;; :letrec flag, so each fn's closure read its sibling before it was bound and
;; resolved it as a top-level variable: "variable ... is not bound".
;; (ja 2 835) -> (ju 1 2) -> (ja 0 1) = 1, so lf-t answers 2, as the JVM does.
(ev "(def lf-t (fn* ([x] (+ x 1))))")
(ok "a mutually recursive letfn passed to an inlined call keeps its letrec"
    (= 2 (jnum->exact (ev "((fn* ([x y] (lf-t (letfn [(ja [d g] (if (zero? d) g (ju (dec d) y))) (ju [d g] (if (zero? d) g (ja (dec d) x)))] (ja (mod y 5) 835))))) 1 2)"))))

;; --- every pass reads a letfn's binders under letrec scope ------------------
;; Each init of a letfn sees every sibling. Passes that threaded scope through
;; the bindings in sequence read a later sibling as whatever an outer binding of
;; its name held: the inline splice substituted the inlined fn's ARGUMENT for a
;; sibling sharing a param's name, and the type pass gave it an outer local's
;; type and folded number?/nil? on it. jolt.ir/thread-bindings is the one rule.
(ev "(def lf-h2 (fn* ([g] (letfn [(f [] (g)) (g [] 42)] (f)))))")
(ok "a letfn sibling named like the inlined fn's param is the sibling"
    (= 42 (jnum->exact (ev "((fn* ([] (lf-h2 7))))"))))
(ev "(def lf-h15 (fn* ([b] (letfn [(a [] (fn? b)) (b [] 99)] (a)))))")
(ok "a forward letfn sibling is not copy-propagated from the arg"
    (eq? #t (ev "((fn* ([] (lf-h15 1))))")))
(ev "(def lf-h3 (fn* ([n] (letfn [(e? [n] (if (zero? n) true (o? (dec n)))) (o? [n] (if (zero? n) false (e? (dec n))))] (e? n)))))")
(ok "an inlined body holding a mutual letfn" (eq? #t (ev "((fn* ([] (lf-h3 10))))")))
(ok "a mutual letfn as a let init, no inline call"
    (equal? "\"done\"" (ev "(pr-str (name (let [r (letfn [(a [k] (if (zero? k) :done (b (dec k)))) (b [k] (a k))] (a 3))] r)))")))
(ok "a self-recursive letfn as a let init"
    (= 10 (jnum->exact (ev "(let [r (letfn [(f [k] (if (zero? k) 0 (+ k (f (dec k)))))] (f 4))] r)"))))
(ok "the type pass does not give a letfn sibling an outer local's type"
    (equal? "[:other :other]" (ev "(pr-str [(let [g 1.5] (letfn [(f [] (if (number? g) :num :other)) (g [] 0)] (f))) (let [g nil] (letfn [(f [] (if (nil? g) :nil :other)) (g [] 0)] (f)))])")))
(ok "a loop init reads the earlier loop var, not an outer local of its name"
    (equal? "[3.0 3.5]" (ev "(pr-str [(let [x 1.5] (loop [x 1 y x i 0] (if (< i 2) (recur x (+ y 1.0) (inc i)) y))) (let [x 1] (loop [x 1.5 y x i 0] (if (< i 2) (recur x (+ y 1) (inc i)) y)))])")))
;; --- fixed-field loop accumulators (jolt.passes.loop-maps) -------------------
(define direct-loop
  "(fn [x] (loop* [i 0 m {:k x}] (if (< i 3) (recur (inc i) (assoc m :k (inc (:k m)))) m)))")
(ok "ordinary evaluation computes the same fixed-field result"
    (clj= (ev (string-append "(" direct-loop " 5)")) (ev "{:k 8}")))

;; Exercise the loop rewrite through analyze, passes, emission and evaluation.
(define (loop-shape src)
  (code-part (emitf "u" src)))

(define boxed-loop-divide
  "(fn [a] (loop* [again true m {:x 0.0 :n 0}]
             (if again (recur false (assoc m :n 1)) (/ a (:x m)))))")
(define destructured-loop
  "(fn [n]
     (let [{:keys [items schemas]}
           (loop [i n m {:items [] :schemas []}]
             (if (zero? i) m (recur (dec i) (update m :items conj i))))]
       [items schemas]))")

(for-each
  (lambda (enabled)
    (set-optimize! enabled)
    (set-direct-link-flag! enabled)
    (ok "a let can rebind the name of a loop result"
        (clj= (ev "(let* [x (loop* [n 2] (if (pos? n) (recur (dec n)) 12))
                          x (inc x)]
                      x)")
              13))
    (ok "destructuring a non-map exit uses its original sequence coercion"
        (clj= (ev "(let [f (fn [stay? other]
                           (let [{:keys [a b]}
                                 (loop* [i 0 m {:a 1 :b 2}]
                                   (if (zero? i) (recur 1 (assoc m :a 3))
                                     (if stay? m other)))]
                             [a b]))]
                     [(f true nil) (f false (list :a 7 :b 8)) (f false nil) (f false 6)])")
              (ev "[[3 2] [7 8] [nil nil] [nil nil]]")))
    (ok "an update replacement can return a sequence to the destructurer"
        (clj= (ev (string-append "(let [f " destructured-loop "]
                      (with-redefs [clojure.core/update (fn [& _] (list :items [7] :schemas [8]))]
                        (f 2)))"))
              (ev "[[7] [8]]")))
    (ok "fallback lookup order and exceptions protect the consumer"
        (clj= (ev "(let [seen (atom [])
                         error (ex-info \"lookup\" {})
                         other (reify clojure.lang.ILookup
                                 (valAt [_ k] (swap! seen conj k)
                                   (if (= k :a) 1 (throw error))))]
                     (try
                       (let [{:keys [a b]}
                             (loop* [i 0 m {:a 1 :b 2}]
                               (if (zero? i) (recur 1 (assoc m :a 3))
                                 (if (= i 2) m (do (swap! seen conj :exit) other))))]
                         (swap! seen conj :consumer)
                         [a b])
                       (catch Throwable t [(identical? t error) @seen])))")
              (ev "[true [:exit :a :b]]")))
    (ok "destructuring with :as receives the unchanged seed object"
        (eq? #t (ev "(let [seed {:a 1 :b 2}
                            {:keys [a b] :as out}
                            (loop* [i 0 m seed]
                              (if (zero? i) m (recur (dec i) (assoc m :a 3))))]
                        (and (identical? seed out) (= [1 2] [a b])))")))
    (ok "a consumer recur still targets the enclosing loop"
        (let ((src "(loop* [i 0 total {:x 1}]
                       (let [{:keys [a b]}
                             (loop* [j 0 m {:a 1 :b 2}]
                               (if (zero? j) (recur 1 (assoc m :a 3)) m))]
                         (if (= i 2) (:x total)
                           (recur (inc i) (assoc total :x (+ (:x total) a b))))))"))
          (and (eq? (not enabled) (has? (loop-shape src) "(jolt-assoc3 "))
               (clj= (ev src) 11))))
    (ok "a default expression still runs when the field is present"
        (eq? #t (ev "(let [error (ex-info \"default\" {})]
                        (try
                          (let [{:keys [a b] :or {a (throw error)}}
                                (loop* [i 0 m {:a 1 :b 2}]
                                  (if (zero? i) (recur 1 (assoc m :a 3)) m))]
                            [a b])
                          (catch Throwable t (identical? t error))))")))
    (ok "a destructurer compiled with a nonstandard seed predicate still calls it"
        (clj= (ev (string-append "(let [standard seq?
                                        seen (atom [])
                                        f (with-redefs [clojure.core/seq?
                                                        (fn [x]
                                                          (when (and (map? x) (contains? x :items)
                                                                     (contains? x :schemas))
                                                            (swap! seen conj x))
                                                          (standard x))]
                                            (eval '" destructured-loop "))]
                                    [(f 1) @seen])"))
              (ev (if enabled "[[[1] []] [{:items [1] :schemas []}]]" "[[[1] []] []]")))))
  '(#f #t))

(for-each
  (lambda (enabled)
    (set-optimize! enabled)
    (set-direct-link-flag! enabled)
    (let ((s (loop-shape direct-loop)))
      (ok "scalar loop carries the field and keeps the final array map"
          (and (eq? (not enabled) (has? s "(jolt-assoc3 "))
               (has? s "(amap-slots->pmap "))))
    (ok "zero and no-op iterations preserve the seed; changing it and back creates a fresh map"
        (eq? #t (ev "(let [f (fn [n forced]
                              (let [seed {:a :initial :b 2}
                                    out (loop [m seed i n]
                                          (if (zero? i) m
                                            (recur (assoc m :a (if (and forced (= i 2))
                                                                 :different :initial))
                                                   (dec i))))]
                                [(identical? seed out) (= (vec seed) (vec out))
                                 (= (class seed) (class out))]))]
                        (= [[true true true] [true true true] [false true true]]
                           [(f 0 true) (f 2 false) (f 2 true)]))")))
    (ok "branch-dependent writes preserve old values and simultaneous recur order"
        (clj= (ev "(let [events (atom [])
                         ans (loop [a 0 m {:x 1 :y 0} b 0]
                               (if (< a 2)
                                 (recur (do (swap! events conj [:first (:x m)]) (inc a))
                                        (assoc (assoc m :x (if (zero? a) 7 9))
                                               :y (do (swap! events conj [:field (:x m)]) (:x m)))
                                        (do (swap! events conj [:last (:x m)]) (:x m)))
                                 [(:x m) (:y m) b]))]
                     [ans @events])")
              (ev "[[9 7 7] [[:first 1] [:field 1] [:last 1]
                                       [:first 7] [:field 7] [:last 7]]]")))
    (ok "throwing field expression short-circuits the following recur operand"
        (clj= (ev "(let [events (atom []) problem (ex-info \"stop\" {})]
                     (try
                       (loop [a 0 m {:k a} b 0]
                         (if (zero? a)
                           (recur (do (swap! events conj :first) 1)
                                  (assoc m :k (do (swap! events conj :field) (throw problem)))
                                  (do (swap! events conj :last) 1))
                           (:k m)))
                       (catch Throwable e [(identical? e problem) @events])))")
              (ev "[true [:first :field]]")))
    (let ((src "(fn [x] (:k (loop [m {:k x} n 2]
                              (if (zero? n) m (recur (assoc m :k (inc (:k m))) (dec n))))))"))
      (ok "a direct consumer gets the field at loop exit"
          (and (eq? (not enabled) (has? (loop-shape src) "(jolt-assoc3 "))
               (clj= (ev (string-append "(" src " 5)")) 7))))
    (ok "a projected flonum still participates in boxed division"
        (clj= (ev (string-append "(try (" boxed-loop-divide
                                 " 1.0) (catch ArithmeticException _ :arithmetic))"))
              (ev ":arithmetic")))
    (ok "a projected get remains boxed through an alias and another arithmetic call"
        (clj= (ev "(let [f (fn [a]
                           (loop* [again true m {:x 0.0}]
                             (if again (recur false (assoc m :x 0.0))
                               (let [x (get m :x)] (/ a (+ x 0.0))))))]
                     (try (f 1.0) (catch ArithmeticException _ :arithmetic)))")
              (ev ":arithmetic")))
    (ok "an explicit double cast of a projected field keeps primitive division"
        (clj= (ev "(let [f (fn [a]
                           (loop* [again true m {:x 0.0}]
                             (if again (recur false (assoc m :x 0.0))
                               (/ a (double (:x m))))))]
                     (f 1.0))")
              (ev "##Inf")))
    (ok "a boxed map read truncates before the byte range check"
        (clj= (ev "(loop* [again true m {:x 127.5}]
                     (if again (recur false (assoc m :x 127.5)) (byte (:x m))))")
              127))
    (ok "destructured loop exits bind multiple fields directly when optimized"
        (and (eq? enabled (has? (loop-shape destructured-loop) "(let-values "))
             (clj= (ev (string-append "(" destructured-loop " 2)"))
                   (ev "[[2 1] []]")))))
  '(#f #t))

(for-each
  (lambda (case)
    (let ((src (cadr case)))
      (ok (car case)
          (and (has? (loop-shape src) "(jolt-assoc3 ")
               (clj= (ev src) (ev (caddr case)))))))
  '(("a prior version carried alongside the loop state stays persistent"
     "(loop [m {:k 0} i 0 prev nil]
        (if (< i 2) (recur (assoc m :k (inc i)) (inc i) m) [m prev]))"
     "[{:k 2} {:k 1}]")
    ("a closure capturing the loop state sees its original snapshot"
     "(loop [m {:k 0} i 0 prior nil]
        (if (< i 2) (recur (assoc m :k (inc i)) (inc i) #(:k m)) [(:k m) (prior)]))"
     "[2 1]")
    ("adding a key keeps ordinary recurrence"
     "(loop [m {:k 0} i 0]
        (if (zero? i) (recur (assoc m :extra 2) (inc i)) m))"
     "{:k 0 :extra 2}")
    ("a dynamic receiver use inside the loop keeps ordinary recurrence"
     "(let [f (fn [m k] (m k))]
        (loop [m {:k 0} i 0]
          (if (< i 2) (do (f m :k) (recur (assoc m :k (inc i)) (inc i))) m)))"
     "{:k 2}")))

(let ((debug! (var-deref "jolt.backend-scheme" "set-debug-locals!")))
  (debug! #t)
  (ok "debug frames keep the source accumulator"
      (has? (loop-shape direct-loop) "(jolt-assoc3 "))
  (debug! #f))

(define guarded-counter
  "(defn guarded-counter [change]
     (loop [i 0 m {:count 0 :items []}]
       (if (= i 2) m
         (recur (inc i)
                (let [m (update m :count inc)]
                  (update m :items conj (change i)))))))")
(ok "linked update checks the operation-time callee after its operands"
    (has? (loop-shape guarded-counter) "jolt-loop-map-update"))
(ev guarded-counter)
(ok "an update rebound midway through recur sees the current map and can return nil"
    (clj= (ev "(let [original update seen (atom [])
                     replacement (fn [m k f & args]
                                   (swap! seen conj [m k (vec args)])
                                   nil)]
                 (try
                   (let [answer (guarded-counter
                                  (fn [i]
                                    (when (zero? i)
                                      (alter-var-root #'clojure.core/update (constantly replacement)))
                                    i))]
                     [answer @seen])
                   (finally (alter-var-root #'clojure.core/update (constantly original)))))")
          (ev "[nil [[{:count 1 :items []} :items [0]] [nil :count []] [nil :items [1]]]]")))
(ev "(defmulti loop-map-edit (fn [& args] (count args)))")
(ev "(defmethod loop-map-edit 3 [m k f] (assoc m k 20))")
(ev "(defmethod loop-map-edit 4 [m k f v] (assoc m k [v]))")
(ok "an invokable replacement for update keeps ordinary linked dispatch"
    (clj= (ev "(with-redefs [clojure.core/update loop-map-edit] (guarded-counter identity))")
          (ev "{:count 20 :items [1]}")))

;; Resume once before a write and once after it. A write that runs a second
;; time creates a different map, while a repeated exit from one write returns
;; the very same map. Keep the continuation in Scheme so it can be re-entered.
(define continuation-loop
  "(fn [done?]
     (loop [i 0 m {:k 0}]
       (if (done? i) m (recur (inc i) (assoc m :k (inc (:k m)))))))")
(define (loop-reentry f capture-at)
  (let ((saved #f) (first-map #f))
    (let* ((done? (lambda (i)
                    (if (= i capture-at)
                        (call/cc (lambda (k) (set! saved k) (= i 1)))
                        (= i 1))))
           (result (jolt-invoke1 f done?)))
      (if first-map
          (and (clj= result first-map) (eq? (eq? result first-map) (= capture-at 1)))
          (begin (set! first-map result) (saved (= capture-at 1)))))))
(for-each
  (lambda (enabled)
    (set-optimize! enabled)
    (set-direct-link-flag! enabled)
    (let ((f (ev continuation-loop)))
      (ok "continuation reentry before a write makes a fresh version" (loop-reentry f 0))
      (ok "continuation reentry after a write returns the same version" (loop-reentry f 1))))
  '(#f #t))

;; Put a real Chez timer delivery inside the first native replacement, after
;; its caller has chosen the replacement procedure for that entry.
(set-optimize! #t)
(set-direct-link-flag! #t)
(define builder-headers-body
  "[m] (reduce-kv (fn [a k v] (assoc a k (clojure.string/replace v \"\\n\" \",\"))) {} m)")
(ok "a confined header reducer emits a guarded builder"
    (has? (code-part (emitf "u" (string-append "(fn " builder-headers-body ")")))
          "(jolt-reduce-kv-build "))
(ev (string-append "(defn builder-headers " builder-headers-body ")"))
(define builder-header-source (ev "(array-map :a \"a\\n1\" :b \"b\\n2\" :c \"c\\n3\")"))
(define (with-header-tick handler run)
  (let ((replace str-replace-literal) (fired? #f))
    (parameterize ((timer-interrupt-handler (lambda () (set! fired? #t) (handler))))
      (dynamic-wind
        (lambda ()
          (set! str-replace-literal
            (lambda (s match replacement)
              (when (and (not fired?) (string=? s "a\n1"))
                (set-timer 1)
                (let spin ((i 2000) (x s))
                  (unless (zero? i) (spin (- i 1) (cons i x)))))
              (replace s match replacement))))
        run
        (lambda () (set-timer 0) (set! str-replace-literal replace))))))
(let* ((root (jolt-var "clojure.string" "replace")) (original (var-cell-deref root))
       (calls 0))
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (with-header-tick
        (lambda ()
          (var-root-set! root (lambda (s _match _replacement) (set! calls (+ calls 1)) s)))
        (lambda ()
          (let ((result (jolt-invoke1 (var-deref "u" "builder-headers") builder-header-source)))
            (ok "header steps dispatch at the time of the individual replacement"
                (and (= calls 2) (clj= result (ev "{:a \"a,1\" :b \"b\\n2\" :c \"c\\n3\"}"))))))))
    (lambda () (var-root-set! root original))))
(let ((resume #f) (results '()) (expected (ev "{:a \"a,1\" :b \"b,2\" :c \"c,3\"}")))
  (with-header-tick
    (lambda () (call/cc (lambda (k) (set! resume k))))
    (lambda ()
      (let ((result (guard (e (#t #f))
                      (jolt-invoke1 (var-deref "u" "builder-headers") builder-header-source))))
        (set! results (cons result results))
        (if (and resume (< (length results) 3))
            (resume (void))
            (ok "a header fold can resume twice from the same timer continuation"
                (and (= (length results) 3)
                     (for-all (lambda (m) (and (pmap? m) (clj= m expected))) results)
                     (not (eq? (car results) (cadr results)))
                     (not (eq? (cadr results) (caddr results))))))))))

(ev "(defn builder-keyword [s]
       (if (symbol? s) (if-let [n (namespace s)] (keyword n (name s)) (keyword (name s))) s))")
(ev "(defn builder-probe [k] k)")
(for-each
  (lambda (name body)
    (ok (string-append name " emits the builder")
        (has? (code-part (emitf "u" (string-append "(fn " body ")"))) "(jolt-reduce-kv-build "))
    (ev (string-append "(defn " name " " body ")")))
  '("builder-copy" "builder-symbols" "builder-calls")
  '("[source] (reduce-kv (fn [a k v] (assoc a k v)) {} (source))"
    "[source] (reduce-kv (fn [a k v] (assoc a (builder-keyword k) v)) {} source)"
    "[f source] (reduce-kv (fn [a k v] (assoc a (f k) [k v])) {} source)"))
(for-each
  (lambda (step)
    (ok "an observing or early-exit reducer keeps its original driver"
        (not (has? (code-part (emitf "u" (string-append "(fn [f m] "
                              (reducer-call step "{}" "m") ")")))
                   "(jolt-reduce-kv-build "))))
  '("(fn [a k v] (assoc a k (do (reset! reducer-sink a) v)))"
    "(fn [a k v] (assoc a k [a]))"
    "(fn [a k v] (assoc a (f a) v))"
    "(fn [a k v] (reduced (assoc a k v)))"))
(ok "an IKVReduce source gets the original callback and seed, following one source evaluation"
    (eq? #t (ev "(let [seen (atom [])
                       custom (reify clojure.lang.IKVReduce
                                (kvreduce [_ f seed]
                                  (let [first-map (f seed :first 1)]
                                    (reduced [f seed first-map (f first-map :second 2)]))))
                       r (builder-copy #(do (swap! seen conj :source) custom))
                       [f seed old result] @r]
                   (and (= [:source] @seen) (= old {:first 1}) (= result {:first 1 :second 2})
                        (= (f old :third 3) {:first 1 :third 3})
                        (identical? seed (builder-copy (constantly {})))))")))
(ok "duplicate transformed keys retain output order and value identity"
    (eq? #t (ev "(let [value (Object.)
                       source (array-map 'k :discard :k value 'app/k value (symbol \"app/k\") value)
                       result (builder-symbols source)]
                   (and (= [:k :app/k] (vec (keys result)))
                        (identical? value (:k result)) (identical? value (:app/k result))))")))
(ok "a cold transformed key is interned by the step itself"
    (eq? #t (ev "(let [k (gensym \"cold-builder-key\")
                       source (array-map 'k 1 k 2 'app/k 3)
                       actual (builder-symbols source)]
                   (= [[:k 1] [(keyword (name k)) 2] [:app/k 3]] (vec actual)))")))
(ok "key and value expressions run once each, in source order"
    (eq? #t (ev "(let [seen (atom [])
                       source (array-map :a 1 :b 2 :c 3 :d 4)
                       r (builder-calls (fn [k] (swap! seen conj k) (name k)) source)]
                   (and (= [:a :b :c :d] @seen)
                        (= {\"a\" [:a 1] \"b\" [:b 2] \"c\" [:c 3] \"d\" [:d 4]} r)
                        (= [\"a\" \"b\" \"c\" \"d\"] (vec (keys r)))))")))
(ok "a key the builder cannot hash freezes the prefix and assocs the rest in order"
    (eq? #t (ev "(let [seen (atom [])
                       odd (reify Object
                             (hashCode [_] 7)
                             (equals [this o] (identical? this o)))
                       source (array-map :a 1 :b 2 :c 3 :d 4 :e 5)
                       r (builder-calls (fn [k] (swap! seen conj k) (if (= k :c) odd k)) source)]
                   (and (= [:a :b :c :d :e] @seen)
                        (= [:a :b odd :d :e] (vec (keys r)))
                        (= [:c 3] (get r odd)) (= [:e 5] (get r :e))))")))
(ev "(defn builder-literal [f source] (reduce-kv (fn [a k v] (assoc a (f k) [:literal])) {} source))")
(ok "a literal in the step is one object across the frozen prefix and the rest"
    (eq? #t (ev "(let [odd (reify Object (hashCode [_] 7) (equals [this o] (identical? this o)))
                       r (builder-literal (fn [k] (if (= k :b) odd k)) (array-map :a 1 :b 2 :c 3 :d 4))
                       [x & more] (vals r)]
                   (and (= 4 (count r)) (every? #(identical? x %) more)))")))
(ok "a thrown value expression propagates as the ordinary call's throwable"
    (equal? (ev "(try (builder-headers (array-map :a \"a\\n1\" :b nil :c \"unused\"))
                  (catch Throwable t (str (class t) \" \" (ex-message t))))")
            (ev "(try (clojure.string/replace nil \"\\n\" \",\")
                  (catch Throwable t (str (class t) \" \" (ex-message t))))")))

;; A root change mid-fold, then a continuation captured inside a step and
;; re-entered: each resumption carries the entries collected before it, and
;; user code never runs with interrupts masked.
(let* ((root (jolt-var "clojure.string" "replace")) (original (var-cell-deref root))
       (native-replace str-replace-literal)
       (resume #f) (first #f) (unmasked? #f)
       (source (ev "(array-map :a \"a\\n1\" :b \"b\\n2\" :c \"c\\n3\")")))
  (dynamic-wind
    (lambda ()
      (set! str-replace-literal
        (lambda (s match replacement)
          (when (string=? s "a\n1")
            (var-root-set! root
              (lambda (s _match _replacement)
                (if (string=? s "b\n2")
                    (begin
                      (set! unmasked? (zero? (sa-disable-count)))
                      (call/cc (lambda (k) (set! resume k) "first")))
                    (string-append s "!")))))
          (native-replace s match replacement))))
    (lambda ()
      (let ((result (jolt-invoke1 (var-deref "u" "builder-headers") source)))
        (cond
          (first
           (ok "a changed root applies from the next step and a step can resume twice"
               (and unmasked?
                    (clj= first (ev "{:a \"a,1\" :b \"first\" :c \"c\\n3!\"}"))
                    (clj= result (ev "{:a \"a,1\" :b \"second\" :c \"c\\n3!\"}"))
                    (not (eq? first result)))))
          (resume (set! first result) (resume "second"))
          (else (ok "a changed root was invoked" #f)))))
    (lambda ()
      (set! str-replace-literal native-replace)
      (var-root-set! root original))))
(ev "(defmulti builder-replace-multi (fn [s _ _] s))")
(ev "(defmethod builder-replace-multi :default [s _ _] (str s \"!\"))")
(ok "the replace root can be an IFn"
    (eq? #t (ev "(with-redefs [clojure.string/replace builder-replace-multi]
                   (= {:a \"a!\" :b \"b!\" :c \"c!\"}
                      (builder-headers (array-map :a \"a\" :b \"b\" :c \"c\"))))")))
(ok "a root replacement receives the same literal objects on every step"
    (eq? #t (ev "(let [args (atom [])]
                   (with-redefs [clojure.string/replace (fn [s m r] (swap! args conj [m r]) s)]
                     (builder-headers (array-map :a \"a\" :b \"b\" :c \"c\")))
                   (let [[[a b] [c d] [e f]] @args]
                     (and (identical? a c) (identical? c e) (identical? b d) (identical? d f))))")))
;; --- emit-time assoc-chain fusion (jolt.backend-scheme.fused) ----------------
(define (count-sub s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0) (n 0))
      (cond ((> (+ i nsub) ns) n)
            ((string=? (substring s i (+ i nsub)) sub) (loop (+ i nsub) (+ n 1)))
            (else (loop (+ i 1) n))))))

;; Eligible chains (2..4 total pairs, locals/consts/pure-op calls on later steps)
;; collapse to one multi-pair (jolt-assoc ...) with zero nested jolt-assoc3 calls.
(for-each
  (lambda (pair)
    (let* ((label (car pair))
           (form (cadr pair))
           (e (code-part (emitf "u" form))))
      (ok (string-append "assoc-chain fuses: " label)
          (and (= 1 (count-sub e "(jolt-assoc "))
               (= 0 (count-sub e "(jolt-assoc3 "))))))
  '(("2-step threaded"
     "(fn [m x y] (-> m (assoc :a x) (assoc :b y)))")
    ("2-step nested"
     "(fn [m x y] (assoc (assoc m :a x) :b y))")
    ("3-step threaded"
     "(fn [m x y] (-> m (assoc :a 1) (assoc :b x) (assoc :c y)))")
    ("4-step threaded"
     "(fn [m] (-> m (assoc :a 1) (assoc :b 2) (assoc :c 3) (assoc :d 4)))")
    ("multi-pair 2+1 = 3 pairs"
     "(fn [m x] (-> m (assoc :a 1 :b 2) (assoc :c x)))")
    ("multi-pair 1+2 = 3 pairs"
     "(fn [m x y] (-> m (assoc :a 1) (assoc :b x :c y)))")
    ("multi-pair 2+2 = 4 pairs"
     "(fn [m x y] (-> m (assoc :a 1 :b 2) (assoc :c x :d y)))")
    ("pure arithmetic later operand (- end start)"
     "(fn [m x end start] (-> m (assoc :a x) (assoc :b (- end start))))")
    ("pure keyword-lookup later operand"
     "(fn [m o] (-> m (assoc :a (:x o)) (assoc :b (:y o))))")
    ("nested pure later operands (+ get some?)"
     "(fn [m o z] (-> m (assoc :a 1) (assoc :b (+ (get o :k 0) 1)) (assoc :c (some? z))))")
    ("impure first step + pure later step"
     "(fn [f g end start] (-> (f) (assoc :a (g)) (assoc :b (- end start))))")))

;; Native op in every mode: no direct-link or optimize gate required.
(set-optimize! #f)
(set-direct-link-flag! #f)
(let ((e (code-part (emitf "u" "(fn [m x y] (-> m (assoc :a x) (assoc :b y)))"))))
  (ok "assoc-chain fuses without direct-link or optimize"
      (and (= 1 (count-sub e "(jolt-assoc "))
           (= 0 (count-sub e "(jolt-assoc3 ")))))
(set-optimize! #t)
(set-direct-link-flag! #t)

;; Ineligible chains must stay completely unfused (no tiling, no partial fusion).
(ev "(def assoc-chain-glob 1)")
(for-each
  (lambda (pair)
    (let* ((label (car pair))
           (form (cadr pair))
           (e (code-part (emitf "u" form))))
      (ok (string-append "assoc-chain does not fuse: " label)
          (= 0 (count-sub e "(jolt-assoc ")))))
  '(("shadowed assoc local"
     "(fn [assoc m x y] (-> m (assoc :a x) (assoc :b y)))")
    ("effectful later operand"
     "(fn [m f] (-> m (assoc :a 1) (assoc :b (f))))")
    ("pure head with effectful nested arg"
     "(fn [m f] (-> m (assoc :a 1) (assoc :b (+ (f) 1))))")
    ("cond-> conditional assoc chain"
     "(fn [m p x y] (cond-> m true (assoc :a x) p (assoc :b y)))")
    ("non-pure count later operand (kondo:1118 shape)"
     "(fn [ctx arg-vec children pre-post-map]
        (-> ctx
            (assoc :fn-args (:children arg-vec))
            (assoc :body-children-count (count children))
            (assoc :pre-post-map (some? pre-post-map))))")
    ("non-pure division later operand"
     "(fn [m a b] (-> m (assoc :a 1) (assoc :b (/ a b))))")
    ("host-call later operand"
     "(fn [m x] (-> m (assoc :a 1) (assoc :b (.toString x))))")
    ("global var later operand"
     "(fn [m] (-> m (assoc :a 1) (assoc :b assoc-chain-glob)))")
    ("later local with :clear-caps (^:once)"
     "(fn [m x y] (^:once fn* [] (-> m (assoc :a x) (assoc :b y))))")
    ("later pure-call arg with :clear-caps (^:once)"
     "(fn [m x y] (^:once fn* [] (-> m (assoc :a x) (assoc :b (+ y 1)))))")
    ("5-step chain (> 4 pairs, no tiling)"
     "(fn [m] (-> m (assoc :a 1) (assoc :b 2) (assoc :c 3) (assoc :d 4) (assoc :e 5)))")))

(let ((e (code-part (emitf "u" "(fn [m] (-> m (assoc :a 1 :b 2 :c 3) (assoc :d 4 :e 5)))"))))
  (ok "3+2 multi-pair chain (> 4 pairs) stays two unfused jolt-assoc calls"
      (= 2 (count-sub e "(jolt-assoc "))))

(let ((e (code-part (emitf "u" "(fn [m x] (-> m (assoc :a 1) (assoc :b (-> x (assoc :p 1) (assoc :q 2)))))"))))
  (ok "independent eligible chain inside an unfused outer value still fuses"
      (and (= 1 (count-sub e "(jolt-assoc "))
           (= 2 (count-sub e "(jolt-assoc3 ")))))

;; Evaluation-order tests:
;; 1) Effectful later operand does NOT run when an earlier assoc throws.
(ok "assoc-chain preserves throw-before-effectful-later-operand order"
    (eq? #t (ev "(let [ran (atom false)
                       f (fn [] (reset! ran true) 42)
                       threw (try
                               (assoc (assoc [1 2] 5 99) 0 (f))
                               false
                               (catch IndexOutOfBoundsException _ true))]
                   (and threw (not @ran)))")))
;; 2) First-step side effects still run left-to-right before pure later operands.
(ok "assoc-chain preserves left-to-right order of first-step side effects"
    (clj= (ev "(let [log (atom [])
                     out (-> (do (swap! log conj :base) {:init 0})
                             (assoc (do (swap! log conj :k1) :a)
                                    (do (swap! log conj :v1) 1))
                             (assoc :b (- 10 3)))]
                 [out @log])")
          (ev "[{:init 0 :a 1 :b 7} [:base :k1 :v1]]")))
;; 3) ^:once capture in a later step survives retry if an earlier step throws.
(ok "assoc-chain does not clear ^:once capture in later step before earlier throw"
    (eq? #t (ev "(let [make-f (fn [m y]
                                (^:once fn* [fail?]
                                  (-> (if fail? [1] m)
                                      (assoc 5 99)
                                      (assoc :b (+ y 1)))))
                       f (make-f {} 41)
                       first-threw (try (f true) false (catch IndexOutOfBoundsException _ true))
                       second-res (f false)]
                   (and first-threw (= {5 99 :b 42} second-res)))")))

;; Regression 1: loop-maps still scalar-replaces a chained-assoc loop.
(let* ((loop-src "(fn [n] (loop [i 0 m {:a 0 :b 0}]
                            (if (< i n)
                              (recur (inc i) (-> m (assoc :a (inc (:a m))) (assoc :b (+ (:b m) 2))))
                              m)))")
       (e (code-part (emitf "u" loop-src))))
  (ok "loop-maps still scalar-replaces chained-assoc loop"
      (and (has? e "(amap-slots->pmap ")
           (= 0 (count-sub e "(jolt-assoc "))
           (= 0 (count-sub e "(jolt-assoc3 "))
           (clj= (ev (string-append "(" loop-src " 5)")) (ev "{:a 5 :b 10}")))))

;; Regression 2: record field demotion in types.clj still fires on chained assoc.
(ev "(defrecord ChainDemoteRec [a b])")
(ev "(defrecord ChainKeptRec [c d])")
(let* ((u ((var-deref "jolt.passes.types" "new-unit")))
       (ctx (make-analyze-ctx "u"))
       (parse1 (lambda (s)
                 (let-values (((f _) (rdr-read-form s 0 (string-length s))))
                   (jolt-ce-analyze ctx f))))
       (tweak-src "(defn cdr-tweak [m] (-> m (assoc :a \"str\") (assoc :b 3.0)))")
       (nodes (jolt-vector
                (parse1 "(defn cdr-mk [] (->ChainDemoteRec 1.0 2.0))")
                (parse1 tweak-src)
                (parse1 "(defn ckr-mk [] (->ChainKeptRec 1.0 2.0))")))
       (ft-box (jolt-get u (keyword #f "field-types"))))
  ((var-deref "jolt.passes.types" "set-record-shapes!") u (chez-record-shapes-map))
  ((var-deref "jolt.passes.types" "set-protocol-methods!") u (chez-protocol-methods-map))
  ((var-deref "jolt.passes.types" "wp-infer!") u nodes)
  (let* ((ftypes ((var-deref "clojure.core" "deref") ft-box))
         (demoted (jolt-get ftypes "u/->ChainDemoteRec"))
         (kept (jolt-get ftypes "u/->ChainKeptRec"))
         (tweak-emitted (code-part (emitf "u" tweak-src))))
    (ok "types.clj record field demotion still fires on chained assoc while tweak itself fuses"
        (and (jolt-nil? demoted)
             (eq? (keyword #f "double") (jolt-get kept (keyword #f "c")))
             (= 1 (count-sub tweak-emitted "(jolt-assoc "))
             (= 0 (count-sub tweak-emitted "(jolt-assoc3 "))))))

;; Result equivalence across all receiver types (fused compiled fns vs left fold).
(ev "(defrecord ChainRec [a b])")
(ev "(deftype ChainCustom [m log]
       clojure.lang.Associative
       (containsKey [_ k] (contains? m k))
       (entryAt [_ k] (find m k))
       (assoc [_ k v] (swap! log conj [k v]) (ChainCustom. (assoc m k v) log))
       clojure.lang.ILookup
       (valAt [_ k] (get m k))
       (valAt [_ k d] (get m k d)))")
(ev "(defn chain-f2 [m k1 v1 k2 v2] (-> m (assoc k1 v1) (assoc k2 v2)))")
(ev "(defn chain-f3 [m k1 v1 k2 v2 k3 v3] (-> m (assoc k1 v1) (assoc k2 v2) (assoc k3 v3)))")
(ev "(defn chain-f4 [m k1 v1 k2 v2 k3 v3 k4 v4] (-> m (assoc k1 v1) (assoc k2 v2) (assoc k3 v3) (assoc k4 v4)))")
(ev "(defn chain-f2-pure [m a end start] (-> m (assoc :a a) (assoc :dt (- end start))))")
(ok "assoc-chain results match unfused across nil, array-map, hash-map, vector, record, sorted-map, custom"
    (eq? #t (ev "(let [fold (fn [m & kvs] (reduce (fn [acc [k v]] (assoc acc k v)) m (partition 2 kvs)))
                       am (with-meta {:a 1 :b 2} {:m :am})
                       hm (with-meta (hash-map :a 1 :b 2 :c 3) {:m :hm})
                       v  (with-meta [10 20 30] {:m :v})
                       r  (with-meta (->ChainRec 1 2) {:m :r})
                       sm (with-meta (sorted-map :b 2 :a 1) {:m :sm})
                       l1 (atom []) l2 (atom [])
                       c1 (ChainCustom. {:a 0} l1)
                       c2 (ChainCustom. {:a 0} l2)
                       cr1 (chain-f3 c1 :a 1 :b 2 :c 3)
                       cr2 (fold c2 :a 1 :b 2 :c 3)]
                   (and
                     (= (chain-f2 nil :a 1 :b 2) (fold nil :a 1 :b 2))
                     (= (chain-f3 nil :a 1 :b 2 :c 3) (fold nil :a 1 :b 2 :c 3))
                     (= (chain-f4 nil :a 1 :b 2 :c 3 :d 4) (fold nil :a 1 :b 2 :c 3 :d 4))
                     ;; array-map: value, key order, meta, no-op identical?, duplicate key
                     (= (chain-f2 am :c 3 :d 4) (fold am :c 3 :d 4))
                     (= (vec (keys (chain-f2 am :c 3 :d 4))) (vec (keys (fold am :c 3 :d 4))))
                     (= (meta (chain-f2 am :c 3 :d 4)) {:m :am})
                     (identical? am (chain-f2 am :a 1 :b 2))
                     (= (chain-f3 am :c 3 :c 30 :d 4) (fold am :c 3 :c 30 :d 4))
                     (= (chain-f2-pure am 9 100 42) {:a 9 :b 2 :dt 58})
                     ;; hash-map
                     (= (chain-f3 hm :a 10 :d 4 :e 5) (fold hm :a 10 :d 4 :e 5))
                     (= (class (chain-f3 hm :a 10 :d 4 :e 5)) (class hm))
                     (= (meta (chain-f3 hm :a 10 :d 4 :e 5)) {:m :hm})
                     ;; vector
                     (= (chain-f2 v 0 1 3 40) (fold v 0 1 3 40))
                     (= (meta (chain-f2 v 0 1 3 40)) {:m :v})
                     ;; defrecord (declared fields + ext keys)
                     (= (chain-f3 r :a 10 :b 20 :c 30) (fold r :a 10 :b 20 :c 30))
                     (instance? ChainRec (chain-f3 r :a 10 :b 20 :c 30))
                     (= (meta (chain-f3 r :a 10 :b 20 :c 30)) {:m :r})
                     ;; sorted-map
                     (= (chain-f3 sm :d 4 :c 3 :a 10) (fold sm :d 4 :c 3 :a 10))
                     (= (keys (chain-f3 sm :d 4 :c 3 :a 10)) '(:a :b :c :d))
                     (= (meta (chain-f3 sm :d 4 :c 3 :a 10)) {:m :sm})
                     ;; custom Associative
                     (= [(:a cr1) (:b cr1) (:c cr1)] [(:a cr2) (:b cr2) (:c cr2)])
                     (= @l1 @l2)))")))

;; Randomized differential test over compiled fused 2/3/4-step chains:
;; covers counts around 8 and 64, mixed key types, duplicates, metadata, and no-op identity.
(let ((f2 (var-deref "u" "chain-f2"))
      (f3 (var-deref "u" "chain-f3"))
      (f4 (var-deref "u" "chain-f4"))
      (kpool (list->vector
               (append (map (lambda (i) (keyword #f (string-append "k" (number->string i))))
                            '(0 1 2 3 4 5 6 7 8 9 62 63 64 65))
                       (list 0 1 7 #\a "str" (jolt-vector 1 2)))))
      (state 410041))
  (define (rng n)
    (set! state (bitwise-and (+ (* state 1103515245) 12345) #x7fffffff))
    (modulo (quotient state 65536) n))
  (define (build-amap n kw-only?)
    (let loop ((i 0) (m empty-pmap))
      (if (= i n)
          (jolt-with-meta m (ev "{:tag :rand}"))
          (let ((k (if (or kw-only? (> i 0))
                       (keyword #f (string-append "k" (number->string i)))
                       "s0")))
            (loop (+ i 1) (jolt-assoc3 m k i))))))
  (define (ref-fold m kvs)
    (let loop ((acc m) (xs kvs))
      (if (null? xs) acc (loop (jolt-assoc3 acc (car xs) (cadr xs)) (cddr xs)))))
  (let ((sizes '(0 3 6 7 8 9 62 63 64 65))
        (all-ok #t))
    (let trial ((t 0))
      (when (and (< t 400) all-ok)
        (let* ((sz (list-ref sizes (rng (length sizes))))
               (kw-only? (zero? (rng 2)))
               (m (if (and (= sz 0) (zero? (rng 3))) jolt-nil (build-amap sz kw-only?)))
               (arity (+ 2 (rng 3)))
               (noop? (and (pmap? m) (> sz 0) (zero? (rng 8))))
               (kvs (let mk ((j 0) (acc '()))
                      (if (= j arity)
                          acc
                          (let* ((idx (if noop? (rng sz) 0))
                                 (k (if noop?
                                        (if (and (not kw-only?) (= idx 0))
                                            "s0"
                                            (keyword #f (string-append "k" (number->string idx))))
                                        (vector-ref kpool (rng (vector-length kpool)))))
                                 (v (if noop? (jolt-get m k) (rng 50))))
                            (mk (+ j 1) (cons k (cons v acc)))))))
               (got (case arity
                      ((2) (apply f2 (cons m kvs)))
                      ((3) (apply f3 (cons m kvs)))
                      ((4) (apply f4 (cons m kvs)))))
               (want (ref-fold m kvs)))
          (unless (and (clj= got want)
                       (eq? (pmap-array? got) (pmap-array? want))
                       (clj= (jolt-vec (jolt-keys got)) (jolt-vec (jolt-keys want)))
                       (clj= (jolt-meta got) (jolt-meta want))
                       (or (not noop?) (eq? got m)))
            (set! all-ok #f))
          (trial (+ t 1)))))
    (ok "randomized differential assoc-chain (400 trials around 8/64, mixed keys, no-op)" all-ok)))

;; --- emit-time adjacent dissoc/assoc pair fusion (jolt-46) -------------------
;; Eligible single-key dissoc -> single-pair assoc pairs emit jolt-dissoc1-assoc1.
(for-each
  (lambda (pair)
    (let* ((label (car pair))
           (form (cadr pair))
           (e (code-part (emitf "u" form))))
      (ok (string-append "dissoc1-assoc1 fuses: " label)
          (and (= 1 (count-sub e "(jolt-dissoc1-assoc1 "))
               (= 0 (count-sub e "(jolt-dissoc2 "))
               (= 0 (count-sub e "(jolt-assoc3 "))))))
  '(("threaded basic"
     "(fn [m x] (-> m (dissoc :a) (assoc :b x)))")
    ("nested basic"
     "(fn [m x] (assoc (dissoc m :a) :b x))")
    ("aleph http.clj:483 shape (- end start)"
     "(fn [rsp end start] (-> rsp (dissoc :aleph/destroy-conn?) (assoc :connection-time (- end start))))")
    ("pure keyword lookup outer operand"
     "(fn [m o] (-> m (dissoc :a) (assoc :b (:x o))))")
    ("nested pure outer operands (+ get some?)"
     "(fn [m o z] (-> m (dissoc :a) (assoc :b (+ (get o :k 0) 1))))")
    ("impure inner step + pure outer step"
     "(fn [f g end start] (-> (f) (dissoc (g)) (assoc :dt (- end start))))")
    ("same key dk = ak"
     "(fn [m x] (-> m (dissoc :a) (assoc :a x)))")))

;; Eligible single-pair assoc -> single-key dissoc pairs emit jolt-assoc1-dissoc1.
(for-each
  (lambda (pair)
    (let* ((label (car pair))
           (form (cadr pair))
           (e (code-part (emitf "u" form))))
      (ok (string-append "assoc1-dissoc1 fuses: " label)
          (and (= 1 (count-sub e "(jolt-assoc1-dissoc1 "))
               (= 0 (count-sub e "(jolt-dissoc2 "))
               (= 0 (count-sub e "(jolt-assoc3 "))))))
  '(("threaded basic"
     "(fn [m x] (-> m (assoc :a x) (dissoc :b)))")
    ("nested basic"
     "(fn [m x] (dissoc (assoc m :a x) :b))")
    ("pure keyword lookup outer key"
     "(fn [m x o] (-> m (assoc :a x) (dissoc (:k o))))")
    ("impure inner step + pure outer key"
     "(fn [f g h] (-> (f) (assoc (g) (h)) (dissoc :b)))")
    ("same key ak = dk"
     "(fn [m x] (-> m (assoc :a x) (dissoc :a)))")))

;; Native ops in every mode: no direct-link or optimize gate required.
(set-optimize! #f)
(set-direct-link-flag! #f)
(let ((e1 (code-part (emitf "u" "(fn [m x] (-> m (dissoc :a) (assoc :b x)))")))
      (e2 (code-part (emitf "u" "(fn [m x] (-> m (assoc :a x) (dissoc :b)))"))))
  (ok "dissoc/assoc pairs fuse without direct-link or optimize"
      (and (= 1 (count-sub e1 "(jolt-dissoc1-assoc1 "))
           (= 0 (count-sub e1 "(jolt-dissoc2 "))
           (= 0 (count-sub e1 "(jolt-assoc3 "))
           (= 1 (count-sub e2 "(jolt-assoc1-dissoc1 "))
           (= 0 (count-sub e2 "(jolt-dissoc2 "))
           (= 0 (count-sub e2 "(jolt-assoc3 ")))))
(set-optimize! #t)
(set-direct-link-flag! #t)

;; Precedence: assoc-chain keeps precedence when both rules could match.
(let ((e1 (code-part (emitf "u" "(fn [m] (-> m (dissoc :a) (assoc :b 1) (assoc :c 2)))")))
      (e2 (code-part (emitf "u" "(fn [m] (-> m (assoc :a 1) (assoc :b 2) (dissoc :c)))")))
      (e3 (code-part (emitf "u" "(fn [m f] (-> m (dissoc :a) (assoc :b 1) (assoc :c (f))))"))))
  (ok "assoc-chain keeps precedence over dissoc->assoc pair"
      (and (= 1 (count-sub e1 "(jolt-assoc "))
           (= 1 (count-sub e1 "(jolt-dissoc2 "))
           (= 0 (count-sub e1 "(jolt-dissoc1-assoc1 "))
           (= 0 (count-sub e1 "(jolt-assoc3 "))))
  (ok "assoc-chain keeps precedence over assoc->dissoc pair"
      (and (= 1 (count-sub e2 "(jolt-assoc "))
           (= 1 (count-sub e2 "(jolt-dissoc2 "))
           (= 0 (count-sub e2 "(jolt-assoc1-dissoc1 "))
           (= 0 (count-sub e2 "(jolt-assoc3 "))))
  (ok "rejected assoc-chain over dissoc does not partially fuse inner pair"
      (and (= 0 (count-sub e3 "(jolt-assoc "))
           (= 0 (count-sub e3 "(jolt-dissoc1-assoc1 "))
           (= 1 (count-sub e3 "(jolt-dissoc2 "))
           (= 2 (count-sub e3 "(jolt-assoc3 ")))))

;; Ineligible pairs must stay completely unfused.
(for-each
  (lambda (pair)
    (let* ((label (car pair))
           (form (cadr pair))
           (e (code-part (emitf "u" form))))
      (ok (string-append "dissoc/assoc pair does not fuse: " label)
          (and (= 0 (count-sub e "(jolt-dissoc1-assoc1 "))
               (= 0 (count-sub e "(jolt-assoc1-dissoc1 "))))))
  '(("shadowed dissoc (da)"
     "(fn [dissoc m] (-> m (dissoc :a) (assoc :b 1)))")
    ("shadowed assoc (da)"
     "(fn [assoc m] (-> m (dissoc :a) (assoc :b 1)))")
    ("shadowed dissoc (ad)"
     "(fn [dissoc m] (-> m (assoc :a 1) (dissoc :b)))")
    ("shadowed assoc (ad)"
     "(fn [assoc m] (-> m (assoc :a 1) (dissoc :b)))")
    ("effectful outer value (da)"
     "(fn [m f] (-> m (dissoc :a) (assoc :b (f))))")
    ("effectful outer key (da)"
     "(fn [m f] (-> m (dissoc :a) (assoc (f) 1)))")
    ("effectful outer key (ad)"
     "(fn [m f] (-> m (assoc :a 1) (dissoc (f))))")
    ("pure head with effectful nested arg (da)"
     "(fn [m f] (-> m (dissoc :a) (assoc :b (+ (f) 1))))")
    ("pure head with effectful nested arg (ad)"
     "(fn [m f] (-> m (assoc :a 1) (dissoc (+ (f) 1))))")
    ("non-pure count outer operand (da)"
     "(fn [m xs] (-> m (dissoc :a) (assoc :b (count xs))))")
    ("non-pure division outer operand (da)"
     "(fn [m a b] (-> m (dissoc :a) (assoc :b (/ a b))))")
    ("host-call outer operand (da)"
     "(fn [m x] (-> m (dissoc :a) (assoc :b (.toString x))))")
    ("global var outer operand (da)"
     "(fn [m] (-> m (dissoc :a) (assoc :b assoc-chain-glob)))")
    ("global var outer operand (ad)"
     "(fn [m] (-> m (assoc :a 1) (dissoc assoc-chain-glob)))")
    ("outer local with :clear-caps (^:once, da)"
     "(fn [m y] (^:once fn* [] (-> m (dissoc :a) (assoc :b y))))")
    ("outer pure-call arg with :clear-caps (^:once, da)"
     "(fn [m y] (^:once fn* [] (-> m (dissoc :a) (assoc :b (+ y 1)))))")
    ("outer local with :clear-caps (^:once, ad)"
     "(fn [m y] (^:once fn* [] (-> m (assoc :a 1) (dissoc y))))")
    ("multi-key dissoc inside pair"
     "(fn [m] (-> m (dissoc :a :b) (assoc :c 1)))")
    ("multi-pair assoc outside pair"
     "(fn [m] (-> m (dissoc :a) (assoc :b 1 :c 2)))")
    ("multi-pair assoc inside pair"
     "(fn [m] (-> m (assoc :a 1 :b 2) (dissoc :c)))")
    ("multi-key dissoc outside pair"
     "(fn [m] (-> m (assoc :a 1) (dissoc :b :c)))")
    ("cond-> conditional dissoc->assoc"
     "(fn [m p] (cond-> m true (dissoc :a) p (assoc :b 1)))")))

;; Evaluation-order tests for dissoc/assoc pairs:
(ok "dissoc->assoc preserves throw-before-effectful-outer-operand order"
    (eq? #t (ev "(let [ran (atom false)
                       f (fn [] (reset! ran true) 42)
                       threw (try
                               (assoc (dissoc [1 2] 0) :a (f))
                               false
                               (catch ClassCastException _ true))]
                   (and threw (not @ran)))")))
(ok "assoc->dissoc preserves throw-before-effectful-outer-operand order"
    (eq? #t (ev "(let [ran (atom false)
                       f (fn [] (reset! ran true) :a)
                       threw (try
                               (dissoc (assoc [1 2] 5 99) (f))
                               false
                               (catch IndexOutOfBoundsException _ true))]
                   (and threw (not @ran)))")))
(ok "dissoc/assoc pairs preserve left-to-right order of first-step side effects"
    (clj= (ev "(let [log (atom [])
                     r1 (-> (do (swap! log conj :m1) {:a 1 :x 0})
                            (dissoc (do (swap! log conj :dk) :a))
                            (assoc :dt (- 10 3)))
                     r2 (-> (do (swap! log conj :m2) {:b 2 :x 0})
                            (assoc (do (swap! log conj :ak) :c)
                                   (do (swap! log conj :av) 9))
                            (dissoc :b))]
                 [r1 r2 @log])")
          (ev "[{:x 0 :dt 7} {:x 0 :c 9} [:m1 :dk :m2 :ak :av]]")))
(ok "dissoc/assoc pairs do not clear ^:once capture in outer step before inner throw"
    (eq? #t (ev "(let [make-da (fn [m y]
                                 (^:once fn* [fail?]
                                   (-> (if fail? [1] m)
                                       (dissoc :a)
                                       (assoc :b (+ y 1)))))
                       make-ad (fn [m y]
                                 (^:once fn* [fail?]
                                   (-> (if fail? [1] m)
                                       (assoc 5 99)
                                       (dissoc (+ y 1)))))
                       fda (make-da {:a 1} 41)
                       fad (make-ad {:a 1 42 2} 41)
                       t1 (try (fda true) false (catch ClassCastException _ true))
                       r1 (fda false)
                       t2 (try (fad true) false (catch IndexOutOfBoundsException _ true))
                       r2 (fad false)]
                   (and t1 (= {:b 42} r1) t2 (= {:a 1 5 99} r2)))")))
(ok "user locals named jolt-dissoc1-assoc1 / jolt-assoc1-dissoc1 do not shadow emitted helpers"
    (clj= (ev "(let [jolt-dissoc1-assoc1 1 jolt-assoc1-dissoc1 2]
                 [(-> {:a 1 :b 2} (dissoc :a) (assoc :c jolt-dissoc1-assoc1))
                  (-> {:a 1 :b 2} (assoc :c jolt-assoc1-dissoc1) (dissoc :a))])")
          (ev "[{:b 2 :c 1} {:b 2 :c 2}]")))

;; Regression: types.clj record field demotion still fires when assoc is inside a pair.
(ev "(defrecord PairDemoteRec [a b])")
(let* ((u ((var-deref "jolt.passes.types" "new-unit")))
       (ctx (make-analyze-ctx "u"))
       (parse1 (lambda (s)
                 (let-values (((f _) (rdr-read-form s 0 (string-length s))))
                   (jolt-ce-analyze ctx f))))
       (tweak-src "(defn pdr-tweak [m] (-> m (dissoc :x) (assoc :a \"str\")))")
       (nodes (jolt-vector
                (parse1 "(defn pdr-mk [] (->PairDemoteRec 1.0 2.0))")
                (parse1 tweak-src)))
       (ft-box (jolt-get u (keyword #f "field-types"))))
  ((var-deref "jolt.passes.types" "set-record-shapes!") u (chez-record-shapes-map))
  ((var-deref "jolt.passes.types" "set-protocol-methods!") u (chez-protocol-methods-map))
  ((var-deref "jolt.passes.types" "wp-infer!") u nodes)
  (let* ((ftypes ((var-deref "clojure.core" "deref") ft-box))
         (demoted (jolt-get ftypes "u/->PairDemoteRec"))
         (tweak-emitted (code-part (emitf "u" tweak-src))))
    (ok "types.clj record field demotion still fires on dissoc->assoc pair while tweak fuses"
        (and (jolt-nil? demoted)
             (= 1 (count-sub tweak-emitted "(jolt-dissoc1-assoc1 "))
             (= 0 (count-sub tweak-emitted "(jolt-dissoc2 "))
             (= 0 (count-sub tweak-emitted "(jolt-assoc3 "))))))

;; Compiled fused pair equivalence across all receiver types.
(ev "(deftype PairInlineCustom [m log]
       clojure.lang.IPersistentMap
       (without [_ k] (swap! log conj [:without k]) (PairInlineCustom. (dissoc m k) log))
       (assoc [_ k v] (swap! log conj [:assoc k v]) (PairInlineCustom. (assoc m k v) log))
       (assocEx [_ k v] (PairInlineCustom. (assoc m k v) log))
       clojure.lang.ILookup
       (valAt [_ k] (get m k))
       (valAt [_ k d] (get m k d))
       clojure.lang.Counted
       (count [_] (count m))
       clojure.lang.Seqable
       (seq [_] (seq m)))")
(ev "(defn pair-da [m dk ak av] (-> m (dissoc dk) (assoc ak av)))")
(ev "(defn pair-ad [m ak av dk] (-> m (assoc ak av) (dissoc dk)))")
(ev "(defn pair-aleph [rsp end start] (-> rsp (dissoc :aleph/destroy-conn?) (assoc :connection-time (- end start))))")
(ok "compiled fused dissoc/assoc pairs match unfused across nil, array-map, hash-map, record, sorted-map, custom"
    (eq? #t (ev "(let [am (with-meta {:a 1 :b 2 :c 3} {:m :am})
                       hm (with-meta (hash-map :a 1 :b 2 :c 3) {:m :hm})
                       r  (with-meta (assoc (->ChainRec 1 2) :x 10) {:m :r})
                       sm (with-meta (sorted-map :c 3 :a 1 :b 2) {:m :sm})
                       lda (atom []) lad (atom [])
                       cda (pair-da (PairInlineCustom. {:a 1 :b 2} lda) :a :c 3)
                       cad (pair-ad (PairInlineCustom. {:a 1 :b 2} lad) :c 3 :a)
                       rsp {:status 200 :headers {} :aleph/destroy-conn? false :aleph/keep-alive? true}]
                   (and
                     (= (pair-da nil :a :b 2) {:b 2})
                     (= (pair-ad nil :b 2 :a) {:b 2})
                     (= (pair-ad nil :a 2 :a) {})
                     (= (pair-da am :a :d 4) {:b 2 :c 3 :d 4})
                     (= (vec (keys (pair-da am :a :a 1))) [:b :c :a])
                     (= (meta (pair-da am :a :d 4)) {:m :am})
                     (identical? am (pair-da am :z :b 2))
                     (= (pair-ad am :d 4 :a) {:b 2 :c 3 :d 4})
                     (= (meta (pair-ad am :d 4 :a)) {:m :am})
                     (identical? am (pair-ad am :b 2 :z))
                     (= (pair-aleph rsp 100 42) {:status 200 :headers {} :aleph/keep-alive? true :connection-time 58})
                     (= (pair-da hm :a :d 4) {:b 2 :c 3 :d 4})
                     (= (class (pair-da hm :a :d 4)) (class hm))
                     (= (meta (pair-da hm :a :d 4)) {:m :hm})
                     (= (pair-ad hm :d 4 :a) {:b 2 :c 3 :d 4})
                     (= (class (pair-ad hm :d 4 :a)) (class hm))
                     (= (meta (pair-ad hm :d 4 :a)) {:m :hm})
                     (= (pair-da r :x :y 20) (assoc (->ChainRec 1 2) :y 20))
                     (instance? ChainRec (pair-da r :x :y 20))
                     (= (meta (pair-da r :x :y 20)) {:m :r})
                     (= (pair-ad r :y 20 :x) (assoc (->ChainRec 1 2) :y 20))
                     (instance? ChainRec (pair-ad r :y 20 :x))
                     (= (meta (pair-ad r :y 20 :x)) {:m :r})
                     (= (keys (pair-da sm :b :d 4)) '(:a :c :d))
                     (= (meta (pair-da sm :b :d 4)) {:m :sm})
                     (= (keys (pair-ad sm :d 4 :b)) '(:a :c :d))
                     (= (meta (pair-ad sm :d 4 :b)) {:m :sm})
                     (= (:c cda) 3) (nil? (:a cda)) (= @lda [[:without :a] [:assoc :c 3]])
                     (= (:c cad) 3) (nil? (:a cad)) (= @lad [[:assoc :c 3] [:without :a]])))")))

;; --- emit-time mixed assoc/update chain fusion (jolt-43) ---------------------
;; Eligible 2..4-pair mixed assoc/update chains on distinct literal keyword keys
;; emit guarded (pmap? / jolt-nil + jolt-loop-map-update?) multi-pair jolt-assoc
;; with the original unfused chain in the fallback lane (Chez direct-link only).
(for-each
  (lambda (pair)
    (let* ((label (car pair))
           (form (cadr pair))
           (e (code-part (emitf "u" form))))
      (ok (string-append "assoc-update chain fuses: " label)
          (and (= 1 (count-sub e "(jolt-loop-map-update? "))
               (= 1 (count-sub e "(pmap? "))
               (>= (count-sub e "(jolt-assoc ") 1)))))
  '(("assoc+update 2-step (athens shape)"
     "(fn [state op] (-> state (assoc :last-op op) (update :op-count inc)))")
    ("update+assoc 2-step"
     "(fn [m x] (-> m (update :a inc) (assoc :b x)))")
    ("update+update 2-step"
     "(fn [m] (-> m (update :a inc) (update :b dec)))")
    ("update+update+update 3-step (re-frame shape)"
     "(fn [state id parent-id epoch]
        (-> state
            (update :seen-ids conj id)
            (update :pending-children dec parent-id)
            (update :cascade-epochs conj epoch)))")
    ("assoc+update+assoc+update 4-step"
     "(fn [m x y] (-> m (assoc :a x) (update :b inc) (assoc :c y) (update :d conj x)))")
    ("4x update chain"
     "(fn [m] (-> m (update :a inc) (update :b inc) (update :c inc) (update :d inc)))")
    ("multi-pair assoc + update (3 total pairs)"
     "(fn [m x y] (-> m (assoc :a x :b y) (update :c inc)))")
    ("update + multi-pair assoc (4 total pairs)"
     "(fn [m x y z] (-> m (update :a inc) (assoc :b x :c y :d z)))")
    ("pure later assoc value and extra update arg"
     "(fn [m o end start] (-> m (update :a inc) (assoc :dt (- end start)) (update :b + (:n o))))")
    ("impure first step + pure later steps"
     "(fn [f g h x] (-> (f) (update :a (g) (h)) (assoc :b (+ x 1))))")
    ("arbitrary updater fn expression in later step"
     "(fn [m mk] (-> m (assoc :a 1) (update :b (mk))))")
    ("anonymous fn updater"
     "(fn [m] (-> m (assoc :a 1) (update :b (fn [v] (+ (or v 0) 2)))))")
    ("keyword updater"
     "(fn [m] (-> m (assoc :a 1) (update :b :sub)))")
    ("multi-extra-arg update (6 total args)"
     "(fn [m a b c] (-> m (assoc :x 1) (update :y + a b c)))")
    ("variadic-arity update (7 total args)"
     "(fn [m a b c d] (-> m (assoc :x 1) (update :y + a b c d)))")))

;; Chez direct-link builds only: plain (non-direct-link) mode emits ordinary calls.
(set-direct-link-flag! #f)
(let ((e1 (code-part (emitf "u" "(fn [state op] (-> state (assoc :last-op op) (update :op-count inc)))")))
      (e2 (code-part (emitf "u" "(fn [m x] (-> m (update :a inc) (update :b conj x)))"))))
  (ok "assoc-update chain does not fuse when direct-link is off"
      (and (= 0 (count-sub e1 "(jolt-loop-map-update? "))
           (= 0 (count-sub e1 "(pmap? "))
           (= 0 (count-sub e1 "(jolt-assoc "))
           (= 0 (count-sub e2 "(jolt-loop-map-update? "))
           (= 0 (count-sub e2 "(pmap? "))
           (= 0 (count-sub e2 "(jolt-assoc ")))))
(set-direct-link-flag! #t)

;; Precedence among the three collection-edit peepholes in fused.clj.
(let ((e-u-aa (code-part (emitf "u" "(fn [m] (-> m (update :a inc) (assoc :b 1) (assoc :c 2)))")))
      (e-u-aa-dup (code-part (emitf "u" "(fn [m] (-> m (update :a inc) (assoc :a 1) (assoc :b 2)))")))
      (e-aa-u-dup (code-part (emitf "u" "(fn [m] (-> m (assoc :a 1) (assoc :b 2) (update :a inc)))")))
      (e-d-a-u (code-part (emitf "u" "(fn [m] (-> m (dissoc :a) (assoc :b 1) (update :c inc)))")))
      (e-d-a-u-dup (code-part (emitf "u" "(fn [m] (-> m (dissoc :a) (assoc :b 1) (update :b inc)))")))
      (e-u-a-d (code-part (emitf "u" "(fn [m] (-> m (update :a inc) (assoc :b 1) (dissoc :c)))")))
      (e-u-a-d-dup (code-part (emitf "u" "(fn [m] (-> m (update :a inc) (assoc :a 1) (dissoc :c)))")))
      (e-nested (code-part (emitf "u" "(fn [m x] (-> m (update :a inc) (assoc :a (-> x (assoc :p 1) (update :q inc)))))"))))
  (ok "assoc-update chain takes precedence over trailing assoc-chain"
      (and (= 1 (count-sub e-u-aa "(jolt-loop-map-update? "))
           (= 1 (count-sub e-u-aa "(jolt-assoc "))))
  (ok "rejected mixed chain with trailing pure assocs still lets assoc-chain fuse outer assocs"
      (and (= 0 (count-sub e-u-aa-dup "(jolt-loop-map-update? "))
           (= 1 (count-sub e-u-aa-dup "(jolt-assoc "))
           (= 0 (count-sub e-u-aa-dup "(jolt-assoc3 "))))
  (ok "rejected mixed chain with leading pure assocs still lets assoc-chain fuse inner assocs"
      (and (= 0 (count-sub e-aa-u-dup "(jolt-loop-map-update? "))
           (= 1 (count-sub e-aa-u-dup "(jolt-assoc "))
           (= 0 (count-sub e-aa-u-dup "(jolt-assoc3 "))))
  (ok "assoc-update chain takes precedence over inner dissoc->assoc pair"
      (and (= 1 (count-sub e-d-a-u "(jolt-loop-map-update? "))
           (= 1 (count-sub e-d-a-u "(jolt-assoc "))
           (= 0 (count-sub e-d-a-u "(jolt-dissoc1-assoc1 "))))
  (ok "rejected mixed chain over dissoc->assoc still lets dissoc->assoc fuse"
      (and (= 0 (count-sub e-d-a-u-dup "(jolt-loop-map-update? "))
           (= 1 (count-sub e-d-a-u-dup "(jolt-dissoc1-assoc1 "))))
  (ok "assoc-update chain takes precedence over outer assoc->dissoc pair"
      (and (= 1 (count-sub e-u-a-d "(jolt-loop-map-update? "))
           (= 1 (count-sub e-u-a-d "(jolt-assoc "))
           (= 0 (count-sub e-u-a-d "(jolt-assoc1-dissoc1 "))))
  (ok "rejected mixed chain under dissoc still lets assoc->dissoc fuse"
      (and (= 0 (count-sub e-u-a-d-dup "(jolt-loop-map-update? "))
           (= 1 (count-sub e-u-a-d-dup "(jolt-assoc1-dissoc1 "))))
  (ok "independent eligible mixed chain inside an unfused outer chain still fuses"
      (and (= 1 (count-sub e-nested "(jolt-loop-map-update? "))
           (= 1 (count-sub e-nested "(jolt-assoc ")))))

;; A chain nested in an updater or operand of a fused chain is emitted in both
;; lanes of the outer chain. It fuses only in the fast lane, so emitted size
;; stays linear in nesting depth instead of doubling per level.
(let ((e (code-part (emitf "u" "(fn [m f] (-> m (assoc :a 1) (update :b (fn [v] (-> v (assoc :p 1) (update :q f))))))"))))
  (ok "chain nested in a fused chain's updater fuses once, not once per enclosing lane"
      (= 2 (count-sub e "(jolt-loop-map-update? "))))

;; Ineligible mixed chains must NOT emit the mixed guard/fusion (and must not tile).
(for-each
  (lambda (pair)
    (let* ((label (car pair))
           (form (cadr pair))
           (e (code-part (emitf "u" form))))
      (ok (string-append "assoc-update chain does not fuse: " label)
          (and (= 0 (count-sub e "(jolt-loop-map-update? "))
               (= 0 (count-sub e "(pmap? "))))))
  '(("repeated key assoc+update"
     "(fn [m] (-> m (assoc :a 1) (update :a inc)))")
    ("repeated key update+assoc"
     "(fn [m] (-> m (update :a inc) (assoc :a 1)))")
    ("repeated key update+update"
     "(fn [m] (-> m (update :a inc) (update :a dec)))")
    ("repeated key across 3 steps"
     "(fn [m] (-> m (update :a inc) (assoc :b 1) (update :a dec)))")
    ("repeated key inside multi-pair assoc"
     "(fn [m] (-> m (assoc :a 1 :a 2) (update :b inc)))")
    ("non-keyword local key in update"
     "(fn [m k] (-> m (assoc :a 1) (update k inc)))")
    ("non-keyword local key in assoc"
     "(fn [m k] (-> m (assoc k 1) (update :b inc)))")
    ("non-keyword string key"
     "(fn [m] (-> m (assoc \"a\" 1) (update :b inc)))")
    ("non-keyword integer key (vector index)"
     "(fn [v] (-> v (assoc 0 1) (update 1 inc)))")
    ("shadowed update local"
     "(fn [update m] (-> m (assoc :a 1) (update :b inc)))")
    ("shadowed assoc local"
     "(fn [assoc m] (-> m (assoc :a 1) (update :b inc)))")
    ("5-step update chain (> 4 pairs, no tiling)"
     "(fn [m] (-> m (update :a inc) (update :b inc) (update :c inc) (update :d inc) (update :e inc)))")
    ("assoc 3-pair + 2x update (5 pairs, no tiling)"
     "(fn [m] (-> m (assoc :a 1 :b 2 :c 3) (update :d inc) (update :e inc)))")
    ("effectful later assoc value"
     "(fn [m f] (-> m (update :a inc) (assoc :b (f))))")
    ("effectful later update extra arg"
     "(fn [m f] (-> m (assoc :a 1) (update :b + (f))))")
    ("pure head with effectful nested arg in later update"
     "(fn [m f] (-> m (assoc :a 1) (update :b + (+ (f) 1))))")
    ("non-pure count later update extra arg"
     "(fn [m xs] (-> m (assoc :a 1) (update :b + (count xs))))")
    ("non-pure division later assoc value"
     "(fn [m a b] (-> m (update :a inc) (assoc :b (/ a b))))")
    ("global var later update extra arg"
     "(fn [m] (-> m (assoc :a 1) (update :b + assoc-chain-glob)))")
    ("later assoc value with :clear-caps (^:once)"
     "(fn [m y] (^:once fn* [] (-> m (update :a inc) (assoc :b y))))")
    ("later update extra arg with :clear-caps (^:once)"
     "(fn [m y] (^:once fn* [] (-> m (assoc :a 1) (update :b + y))))")
    ("later update pure-call arg with :clear-caps (^:once)"
     "(fn [m y] (^:once fn* [] (-> m (assoc :a 1) (update :b + (+ y 1)))))")
    ("cond-> conditional chain"
     "(fn [m p] (cond-> m true (assoc :a 1) p (update :b inc)))")
    ("single update step"
     "(fn [m] (update m :a inc))")
    ("pure assoc chain (uses assoc-chain without guard)"
     "(fn [m x y] (-> m (assoc :a x) (assoc :b y)))")))

;; Runtime tests: compiled mixed chains across shapes, fallbacks, with-redefs, throws, order.
(ev "(defn mu-au2 [m x] (-> m (assoc :a x) (update :b inc)))")
(ev "(defn mu-ua2 [m x] (-> m (update :a inc) (assoc :b x)))")
(ev "(defn mu-uu2 [m] (-> m (update :a inc) (update :b dec)))")
(ev "(defn mu-au3 [m x y] (-> m (assoc :a x) (update :b + y) (assoc :c (+ x y))))")
(ev "(defn mu-uuu3 [m a b c] (-> m (update :a conj a) (update :b + b) (update :c conj c)))")
(ev "(defn mu-mix4 [m x y] (-> m (assoc :a x) (update :b inc) (assoc :c y) (update :d conj x)))")
(ev "(defn mu-nil2 [m x] (-> m (assoc :a x) (update :b (fnil inc 0))))")
(ev "(defn mu-multi-extra [m a b c d] (-> m (assoc :x a) (update :y (fnil + 0) a b c d)))")
(ev "(defn mu-athens [state op] (-> state (assoc :last-op op) (update :op-count inc)))")
(ev "(defn mu-dec-pending [pending parent-id]
       (if parent-id
         (let [n (get pending parent-id 0)]
           (cond (> n 1) (assoc pending parent-id (dec n))
                 (= n 1) (dissoc pending parent-id)
                 :else pending))
         pending))")
(ev "(defn mu-reframe [state id parent-id epoch]
       (-> state
           (update :seen-ids conj id)
           (update :pending-children mu-dec-pending parent-id)
           (update :cascade-epochs conj epoch)))")

(ok "assoc-update 2/3/4-step shapes, multi-extra-arg, athens, and re-frame produce exact results"
    (eq? #t (ev "(let [m {:a 10 :b 20 :c 30 :d []}
                       athens-s {:id :q :stages {} :last-op :init :op-count 5}
                       rf-s {:cascade-ids #{:r} :seen-ids #{:r} :cascade-epochs []
                             :pending-children {:r 2 :p 1} :unmatched-epochs []}]
                   (and
                     (= (mu-au2 m 99) {:a 99 :b 21 :c 30 :d []})
                     (= (mu-ua2 m 99) {:a 11 :b 99 :c 30 :d []})
                     (= (mu-uu2 m) {:a 11 :b 19 :c 30 :d []})
                     (= (mu-au3 m 3 4) {:a 3 :b 24 :c 7 :d []})
                     (= (mu-uuu3 {:a #{} :b 10 :c []} :k 5 :v) {:a #{:k} :b 15 :c [:v]})
                     (= (mu-mix4 m 7 8) {:a 7 :b 21 :c 8 :d [7]})
                     (= (mu-nil2 nil 42) {:a 42 :b 1})
                     (= (mu-nil2 {:a 1 :b 10} 2) {:a 2 :b 11})
                     (= (mu-multi-extra {:y 10} 1 2 3 4) {:y 20 :x 1})
                     (= (mu-multi-extra nil 1 2 3 4) {:x 1 :y 10})
                     (= (mu-athens athens-s [:add :s1]) {:id :q :stages {} :last-op [:add :s1] :op-count 6})
                     (= (mu-reframe rf-s :e2 :r {:id :e2})
                        {:cascade-ids #{:r} :seen-ids #{:r :e2} :cascade-epochs [{:id :e2}]
                         :pending-children {:r 1 :p 1} :unmatched-epochs []})
                     (= (mu-reframe rf-s :e2 :p {:id :e2})
                        {:cascade-ids #{:r} :seen-ids #{:r :e2} :cascade-epochs [{:id :e2}]
                         :pending-children {:r 2} :unmatched-epochs []})))")))

;; Fallback lane: with-redefs and alter-var-root on clojure.core/update.
(ok "with-redefs on clojure.core/update takes fallback and returns redefined result"
    (eq? #t (ev "(let [calls (atom 0)
                       orig update
                       redef-res (with-redefs [update (fn [m k f & args]
                                                        (swap! calls inc)
                                                        (assoc m k :redefined))]
                                   [(mu-au2 {:a 1 :b 2} 9)
                                    (mu-uu2 {:a 1 :b 2})
                                    (mu-nil2 nil 5)])
                       after-res (mu-au2 {:a 1 :b 2} 9)]
                   (and (= @calls 4)
                        (= redef-res [{:a 9 :b :redefined}
                                      {:a :redefined :b :redefined}
                                      {:a 5 :b :redefined}])
                        (= after-res {:a 9 :b 3})))")))

(ok "alter-var-root on clojure.core/update takes fallback and restores cleanly"
    (eq? #t (ev "(let [orig (var-get #'clojure.core/update)
                       calls (atom 0)]
                   (try
                     (alter-var-root #'clojure.core/update
                                     (constantly (fn [m k f & args]
                                                   (swap! calls inc)
                                                   (assoc m k :altered))))
                     (let [r (mu-ua2 {:a 1 :b 2} 7)]
                       (and (= @calls 1) (= r {:a :altered :b 7})))
                     (finally
                       (alter-var-root #'clojure.core/update (constantly orig)))))")))

;; Throwing updaters & short-circuiting of later steps.
(ok "throwing updater in step 0 or step 1 propagates and skips later steps"
    (eq? #t (ev "(let [log (atom [])
                       f0 (fn [m]
                            (-> m
                                (update :a (fn [_] (swap! log conj :s0) (throw (ex-info \"boom0\" {}))))
                                (update :b (fn [x] (swap! log conj :s1) (inc x)))))
                       f1 (fn [m]
                            (-> m
                                (assoc :a (do (swap! log conj :a0) 1))
                                (update :b (fn [_] (swap! log conj :b1) (throw (ex-info \"boom1\" {}))))
                                (update :c (fn [x] (swap! log conj :c2) (inc x)))))
                       e0 (try (f0 {:a 1 :b 2}) nil (catch Exception e (ex-message e)))
                       l0 @log
                       _  (reset! log [])
                       e1 (try (f1 {:a 1 :b 2 :c 3}) nil (catch Exception e (ex-message e)))
                       l1 @log]
                   (and (= e0 \"boom0\") (= l0 [:s0])
                        (= e1 \"boom1\") (= l1 [:a0 :b1])))")))

;; Evaluation order: base evaluated once (hit and miss), source order across steps, ^:once retry.
(ok "assoc-update chain evaluates base once on hit/miss and preserves source order"
    (eq? #t (ev "(let [log (atom [])
                       run (fn [mk]
                             (-> (do (swap! log conj :base) (mk))
                                 (assoc :a (do (swap! log conj :a) 1))
                                 (update :b (fn [v] (swap! log conj [:u v]) (inc (or v 0))))))
                       r-hit (run (fn [] {:b 10}))
                       l-hit @log
                       _     (reset! log [])
                       r-rec (run (fn [] (->ChainRec 0 10)))
                       l-rec @log
                       _     (reset! log [])
                       v-threw (try
                                 (run (fn [] [1 2]))
                                 false
                                 (catch IllegalArgumentException _ true))
                       l-vec @log]
                   (and (= r-hit {:b 11 :a 1}) (= l-hit [:base :a [:u 10]])
                        (= r-rec (->ChainRec 1 11)) (= l-rec [:base :a [:u 10]])
                        v-threw (= l-vec [:base :a])))")))

(ok "assoc-update chain does not clear ^:once capture in later step before earlier throw"
    (eq? #t (ev "(let [make-f (fn [m y]
                                (^:once fn* [fail?]
                                  (-> (if fail? [1] m)
                                      (assoc :a 1)
                                      (update :b + y))))
                       f (make-f {:b 10} 32)
                       threw (try (f true) false (catch IllegalArgumentException _ true))
                       res (f false)]
                   (and threw (= {:b 42 :a 1} res)))")))

;; Local shadowing of emitted guard heads.
(ok "user locals named pmap? / jolt-loop-map-update? do not shadow emitted guard heads"
    (clj= (ev "(let [pmap? 10 jolt-loop-map-update? 5]
                 (-> {:a 1 :b 2}
                     (assoc :a pmap?)
                     (update :b + jolt-loop-map-update?)))")
          (ev "{:a 10 :b 7}")))

;; Non-pmap fallbacks (pvec, defrecord, sorted-map, custom Associative) + nil + hash-map + array-map + meta + identical?.
(ok "assoc-update chain matches unfused across nil, array-map, hash-map, pvec, defrecord, sorted-map, custom Associative"
    (eq? #t (ev "(let [am (with-meta {:a 1 :b 2 :c 3} {:m :am})
                       hm (with-meta (hash-map :a 1 :b 2 :c 3) {:m :hm})
                       r  (with-meta (->ChainRec 1 2) {:m :r})
                       sm (with-meta (sorted-map :c 3 :b 2 :a 1) {:m :sm})
                       lc (atom [])
                       ca (ChainCustom. {:a 1 :b 2} lc)
                       cr (mu-au2 ca 9)
                       noop-fn (fn [m] (-> m (assoc :a 1) (update :b identity)))
                       pvec-threw (try (mu-au2 [1 2] 9) false (catch IllegalArgumentException _ true))]
                   (and
                     ;; nil
                     (= (mu-nil2 nil 5) {:a 5 :b 1})
                     ;; array-map: value, key order, meta, no-op identical?
                     (= (mu-au2 am 9) {:a 9 :b 3 :c 3})
                     (= (vec (keys (mu-au2 am 9))) [:a :b :c])
                     (= (meta (mu-au2 am 9)) {:m :am})
                     (identical? am (noop-fn am))
                     ;; hash-map: value, class, meta
                     (= (mu-au2 hm 9) {:a 9 :b 3 :c 3})
                     (= (class (mu-au2 hm 9)) (class hm))
                     (= (meta (mu-au2 hm 9)) {:m :hm})
                     ;; pvec fallback throws on keyword key
                     pvec-threw
                     ;; defrecord fallback: declared fields, ext keys, class, meta
                     (= (mu-au2 r 9) (->ChainRec 9 3))
                     (instance? ChainRec (mu-au2 r 9))
                     (= (meta (mu-au2 r 9)) {:m :r})
                     (= (mu-mix4 (assoc r :d []) 9 8) (assoc (->ChainRec 9 3) :c 8 :d [9]))
                     ;; sorted-map fallback: order + meta
                     (= (mu-au2 sm 9) {:a 9 :b 3 :c 3})
                     (= (keys (mu-au2 sm 9)) '(:a :b :c))
                     (= (meta (mu-au2 sm 9)) {:m :sm})
                     ;; custom Associative fallback
                     (= [(:a cr) (:b cr)] [9 3])
                     (= @lc [[:a 9] [:b 3]])))")))

;; Regressions: loop-maps and types.clj record field demotion still work with mixed chains.
(let* ((loop-src "(fn [n] (loop [i 0 m {:a 0 :b 0}]
                            (if (< i n)
                              (recur (inc i) (-> m (assoc :a i) (update :b inc)))
                              m)))")
       (e (code-part (emitf "u" loop-src))))
  (ok "loop-maps still scalar-replaces mixed assoc/update loop without firing fused rule"
      (and (has? e "(amap-slots->pmap ")
           (= 0 (count-sub e "(pmap? "))
           (= 0 (count-sub e "(jolt-assoc "))
           (clj= (ev (string-append "(" loop-src " 5)")) (ev "{:a 4 :b 5}")))))

(ev "(defrecord MixedDemoteRec [a b])")
(let* ((u ((var-deref "jolt.passes.types" "new-unit")))
       (ctx (make-analyze-ctx "u"))
       (parse1 (lambda (s)
                 (let-values (((f _) (rdr-read-form s 0 (string-length s))))
                   (jolt-ce-analyze ctx f))))
       (tweak-src "(defn mdr-tweak [m] (-> m (assoc :a \"str\") (update :b inc)))")
       (nodes (jolt-vector
                (parse1 "(defn mdr-mk [] (->MixedDemoteRec 1.0 2.0))")
                (parse1 tweak-src)))
       (ft-box (jolt-get u (keyword #f "field-types"))))
  ((var-deref "jolt.passes.types" "set-record-shapes!") u (chez-record-shapes-map))
  ((var-deref "jolt.passes.types" "set-protocol-methods!") u (chez-protocol-methods-map))
  ((var-deref "jolt.passes.types" "wp-infer!") u nodes)
  (let* ((ftypes ((var-deref "clojure.core" "deref") ft-box))
         (demoted (jolt-get ftypes "u/->MixedDemoteRec"))
         (tweak-emitted (code-part (emitf "u" tweak-src))))
    (ok "types.clj record field demotion still fires on mixed assoc/update while tweak fuses"
        (and (jolt-nil? demoted)
             (= 1 (count-sub tweak-emitted "(jolt-loop-map-update? "))
             (= 1 (count-sub tweak-emitted "(jolt-assoc "))))))

;; Randomized differential test over compiled fused mixed chains around counts 8 and 64,
;; covering nil, array-map, hash-map, metadata, missing keys, promotion, and no-op identical?.
(ev "(defn mu-rand2 [m x f] (-> m (assoc :k0 x) (update :k3 f)))")
(ev "(defn mu-rand3 [m x f g] (-> m (update :k1 f) (assoc :k64 x) (update :k65 g)))")
(ev "(defn mu-rand4 [m x y f g] (-> m (assoc :k0 x) (update :k2 f) (assoc :k63 y) (update :k64 g)))")
(let ((f2 (var-deref "u" "mu-rand2"))
      (f3 (var-deref "u" "mu-rand3"))
      (f4 (var-deref "u" "mu-rand4"))
      (upd (var-deref "clojure.core" "update"))
      (fn-id (ev "identity"))
      (fn-inc (ev "(fnil inc 0)"))
      (fn-add (ev "(fn [v] (+ (or v 0) 7))"))
      (state 430043))
  (define (rng n)
    (set! state (bitwise-and (+ (* state 1103515245) 12345) #x7fffffff))
    (modulo (quotient state 65536) n))
  (define (build-map n kw-only?)
    (let loop ((i 0) (m empty-pmap))
      (if (= i n)
          (jolt-with-meta m (ev "{:tag :mu-rand}"))
          (let ((k (if (or kw-only? (> i 0))
                       (keyword #f (string-append "k" (number->string i)))
                       "s0")))
            (loop (+ i 1) (jolt-assoc3 m k i))))))
  (let ((sizes '(0 3 6 7 8 9 62 63 64 65))
        (all-ok #t))
    (let trial ((t 0))
      (when (and (< t 400) all-ok)
        (let* ((sz (list-ref sizes (rng (length sizes))))
               (kw-only? (zero? (rng 2)))
               (m (if (and (= sz 0) (zero? (rng 3))) jolt-nil (build-map sz kw-only?)))
               (arity (+ 2 (rng 3)))
               (noop? (and (pmap? m) kw-only? (>= sz 4) (= arity 2) (zero? (rng 6))))
               (k0 (keyword #f "k0")) (k1 (keyword #f "k1")) (k2 (keyword #f "k2"))
               (k3 (keyword #f "k3")) (k63 (keyword #f "k63"))
               (k64 (keyword #f "k64")) (k65 (keyword #f "k65"))
               (x (if noop? (jolt-get m k0) (rng 50)))
               (y (rng 50))
               (u1 (if noop? fn-id (if (zero? (rng 2)) fn-inc fn-add)))
               (u2 (if (zero? (rng 2)) fn-inc fn-add))
               (got (case arity
                      ((2) (f2 m x u1))
                      ((3) (f3 m x u1 u2))
                      ((4) (f4 m x y u1 u2))))
               (want (case arity
                       ((2) (upd (jolt-assoc3 m k0 x) k3 u1))
                       ((3) (upd (jolt-assoc3 (upd m k1 u1) k64 x) k65 u2))
                       ((4) (upd (jolt-assoc3 (upd (jolt-assoc3 m k0 x) k2 u1) k63 y) k64 u2)))))
          (unless (and (clj= got want)
                       (eq? (pmap-array? got) (pmap-array? want))
                       (clj= (jolt-vec (jolt-keys got)) (jolt-vec (jolt-keys want)))
                       (clj= (jolt-meta got) (jolt-meta want))
                       (or (not noop?) (eq? got m)))
            (set! all-ok #f))
          (trial (+ t 1)))))
    (ok "randomized differential assoc-update chains (400 trials around 8/64, nil, meta, no-op)" all-ok)))

(set-optimize! #f)
(set-direct-link-flag! #f)

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
