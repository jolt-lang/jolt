;; Phase 1 (jolt-cf1q.2) — the minimal Chez RT the emitted Scheme rests on.
;;
;; Sits above the value model (values.ss) and below an emitted program. Adds the
;; two things the back end's output references that aren't in the value layer:
;;   1. the var-cell late-binding registry (Clojure vars — a global root that a
;;      reference reads at call time, so redefinition / mutual recursion work);
;;   2. the rt primitive shims the emitter names (jolt-inc/dec/not) and jolt's
;;      number printing (all jolt numbers model Clojure doubles; integer-valued
;;      print without a trailing ".0", matching the Janet host).
;;
;; Emitted programs do `(load "host/chez/rt.ss")`; this loads values.ss in turn.

(load "host/chez/values.ss")
(load "host/chez/collections.ss")
(load "host/chez/seq.ss")

;; --- rt arithmetic / logic shims (named in emit.janet's native-ops) ----------
(define (jolt-inc x) (+ x 1))
(define (jolt-dec x) (- x 1))
;; jolt `not`: only nil and false are falsey.
(define (jolt-not x) (if (jolt-truthy? x) #f #t))

;; --- exceptions (jolt-vcsl) --------------------------------------------------
;; throw raises the jolt value RAW (no envelope), like the Janet compiled back
;; end; catch (emitted as `guard`) binds it directly. Chez `raise` accepts any
;; object, so a thrown number/map/ex-info all work; uncaught -> non-zero exit.
(define (jolt-throw v) (raise v))
;; ex-info builds the tagged map {:jolt/type :jolt/ex-info :message :data :cause}
;; — a real jolt-hash-map, so the ex-data/ex-message/ex-cause tier fns read it
;; via jolt-get for free. Arity 2 (msg data) or 3 (msg data cause).
(define jolt-kw-ex-type (keyword "jolt" "type"))
(define jolt-kw-ex-info (keyword "jolt" "ex-info"))
(define jolt-kw-message (keyword #f "message"))
(define jolt-kw-data (keyword #f "data"))
(define jolt-kw-cause (keyword #f "cause"))
(define (jolt-ex-info msg data . more)
  (jolt-hash-map jolt-kw-ex-type jolt-kw-ex-info
                 jolt-kw-message msg
                 jolt-kw-data data
                 jolt-kw-cause (if (null? more) jolt-nil (car more))))

;; --- host interop (jolt-0kf5) ------------------------------------------------
;; (.method target arg*) lowers to (jolt-host-call "method" target arg*). JVM
;; interop has no general Chez analog, but the few methods jolt-core's io tier
;; calls map onto Chez equivalents: a writer's .write is a port display; a File's
;; .isDirectory / .listFiles work over a path string (Chez has no File type, so
;; file-seq's File branch is unreachable here — these keep the forms honest). An
;; unsupported method raises rather than silently returning nil.
(define (jolt-host-call method target . args)
  (cond
    ((string=? method "write") (display (car args) target) jolt-nil)
    ((string=? method "isDirectory") (if (file-directory? target) #t #f))
    ((string=? method "listFiles") (list->cseq (directory-list target)))
    (else (error 'jolt-host-call (string-append "unsupported host method: ." method)))))

;; --- var cells: late-bound global roots (Clojure vars) -----------------------
;; A var is a mutable cell keyed by "ns/name". A `:def` sets the root; a `:var`
;; reference reads it at use time (late binding), so a forward/mutually-recursive
;; reference resolves to whatever the cell holds when the call actually runs.
;; declare / (def name) with no init reserves a cell holding this placeholder
;; until the real def overwrites it (a forward reference resolves to the cell, and
;; correct code never reads it before the binding def runs).
(define jolt-unbound (string->symbol "#<jolt-unbound>"))
;; `defined?` distinguishes a genuinely interned var (def / declare / a native-op
;; cell) from a cell lazily materialised by a forward `var-deref` / `(var x)` on a
;; not-yet-defined name — `resolve` returns the cell iff defined? (jolt-yxqm).
;; ns-unmap clears it. Avoids the (def x nil) edge of probing the root.
(define-record-type var-cell (fields ns name (mutable root) (mutable defined?)) (nongenerative var-cell-v2))
(define var-table (make-hashtable string-hash string=?))
(define (jolt-var ns name)
  (let ((k (string-append ns "/" name)))
    (or (hashtable-ref var-table k #f)
        (let ((c (make-var-cell ns name jolt-nil #f)))
          (hashtable-set! var-table k c)
          c))))
;; non-creating lookup (resolve / find-var / ns-unmap): #f when absent, so a
;; probe never interns an empty cell.
(define (var-cell-lookup ns name) (hashtable-ref var-table (string-append ns "/" name) #f))
(define (var-deref ns name) (var-cell-root (jolt-var ns name)))
;; def-var! / declare-var! return the VAR CELL, not the value — Clojure's `def`
;; evaluates to #'ns/name (a first-class var), so (var? (def x 1)) is true and
;; (pr-str (def x 1)) is "#'ns/x". The prelude's def-var! forms discard the
;; return, so this is transparent there.
(define (def-var! ns name v) (let ((c (jolt-var ns name))) (var-cell-root-set! c v) (var-cell-defined?-set! c #t) c))
;; var def-time metadata (jolt-zikh): the :def emit passes the def's reader meta
;; (^:private / ^Type tag / docstring -> {:doc}) here, stored in an eq side-table
;; keyed by the cell. jolt-meta (natives-meta.ss) merges it onto {:ns :name},
;; which it derives from the cell — so EVERY var (plain def, native-op, declare)
;; reports {:ns :name} like Clojure, with the user meta layered on when present.
(define var-meta-table (make-eq-hashtable))
(define jolt-kw-var-ns (keyword #f "ns"))
(define jolt-kw-var-name (keyword #f "name"))
(define (def-var-with-meta! ns name v m)
  (let ((c (def-var! ns name v))) (hashtable-set! var-meta-table c m) c))
;; declare / (def name) with no init: reserve the cell ONLY if absent. An
;; existing root is left intact — Clojure's (def x) with no init does not clobber
;; a prior binding (do (def x 7) (def x) x) => 7. Returns the cell either way.
(define (declare-var! ns name)
  (let ((k (string-append ns "/" name)))
    (or (hashtable-ref var-table k #f)
        (let ((c (make-var-cell ns name jolt-unbound #t)))  ; declared => interned/resolvable
          (hashtable-set! var-table k c)
          c))))

;; regex (jolt-i0s3): defines regex-t + the re-* fns (def-var!'d into
;; clojure.core), so it loads after def-var! and before the printer below (which
;; renders a regex-t as #"source").
(load "host/chez/regex.ss")

;; atoms (jolt-9ziu): host-coupled mutable cells; def-var!'d into clojure.core
;; (atom/deref/swap!/reset! + the compare/vals kernel). Loads after def-var! and
;; jolt-invoke (seq.ss) / jolt= (values.ss) / jolt-vector (collections.ss).
(load "host/chez/atoms.ss")

;; type predicates + simple accessors (jolt-9ziu): seed natives the overlay
;; assumes (map?/vector?/nil?/number?/.../name/namespace), def-var!'d into
;; clojure.core. Loads after the value-model record predicates they wrap.
(load "host/chez/predicates.ss")

;; --- jolt number printing ----------------------------------------------------
;; jolt models every number as a Clojure double: integer-valued values print
;; without a ".0" (the Janet host prints (* 1.0 5) as "5", (/ 1 2) as "0.5").
(define (jolt-num->string x)
  (cond
    ;; the -e / element printer renders the infinities and NaN as inf/-inf/nan
    ;; (Chez's number->string gives +inf.0 etc.); the str/print family uses the
    ;; long "Infinity"/"NaN" forms (see jolt-str-render-one in converters.ss).
    ((and (flonum? x) (fl= x +inf.0)) "inf")
    ((and (flonum? x) (fl= x -inf.0)) "-inf")
    ((and (flonum? x) (not (fl= x x))) "nan")
    ((and (rational? x) (integer? x)) (number->string (exact x)))
    (else (number->string x))))

;; Program-final-value printer. jolt's `-e` prints in str-style: strings raw (no
;; quotes), chars as `\c`/`\newline`, collections recursively. NOTE: maps/sets
;; render in HAMT-iteration order, which does NOT match the Janet host's order —
;; so unordered values are compared via `=` (true/false), not printed form.
;; The full canonical printer is Phase 2.
(define (jolt-str-join strs)
  (cond ((null? strs) "") ((null? (cdr strs)) (car strs))
        (else (string-append (car strs) " " (jolt-str-join (cdr strs))))))
(define (jolt-char->string c)
  (string-append "\\" (case c ((#\newline) "newline") ((#\space) "space") ((#\tab) "tab")
                        ((#\return) "return") (else (string c)))))
;; Program-final printer: jolt's `-e` is str-style at the top level, where a
;; bare nil renders as the empty string (a nil ELEMENT inside a collection still
;; prints "nil", which jolt-pr-str handles).
(define (jolt-final-str x) (if (jolt-nil? x) "" (jolt-pr-str x)))
(define (jolt-pr-str x)
  (cond
    ((jolt-nil? x) "nil")
    ((eq? x #t) "true")
    ((eq? x #f) "false")
    ((number? x) (jolt-num->string x))
    ((string? x) x)
    ((char? x) (jolt-char->string x))
    ((keyword? x) (let ((ns (keyword-t-ns x)))
                    (if ns (string-append ":" ns "/" (keyword-t-name x)) (string-append ":" (keyword-t-name x)))))
    ((jolt-symbol? x) (let ((ns (symbol-t-ns x)))
                        (if (or (jolt-nil? ns) (not ns) (eq? ns '())) (symbol-t-name x)
                            (string-append ns "/" (symbol-t-name x)))))
    ((regex-t? x) (string-append "#\"" (regex-t-source x) "\""))
    ((pvec? x) (let ((acc '())) (let loop ((i (fx- (pvec-count x) 1)))
                 (when (fx>=? i 0) (set! acc (cons (jolt-pr-str (pvec-nth-d x i jolt-nil)) acc)) (loop (fx- i 1))))
                 (string-append "[" (jolt-str-join acc) "]")))
    ((pset? x) (string-append "#{" (jolt-str-join (pset-fold x (lambda (e a) (cons (jolt-pr-str e) a)) '())) "}"))
    ((pmap? x) (string-append "{" (jolt-str-join
                 (pmap-fold x (lambda (k v a) (cons (string-append (jolt-pr-str k) " " (jolt-pr-str v)) a)) '())) "}"))
    ;; lists / cons / lazy seqs all print as (...) — forces a finite seq.
    ((empty-list-t? x) "()")
    ((cseq? x) (string-append "(" (jolt-str-join
                 (let loop ((s x) (acc '()))
                   (if (jolt-nil? s) (reverse acc)
                       (loop (jolt-seq (seq-more s)) (cons (jolt-pr-str (seq-first s)) acc))))) ")"))
    (else (format "~a" x))))

;; converters + string ops (jolt-t6cr): str/subs/vec/keyword/symbol/compare/int/
;; double/gensym — host-coupled seed natives def-var!'d into clojure.core. Loaded
;; LAST because `str` reuses jolt-pr-str (defined just above).
(load "host/chez/converters.ss")

;; transients (jolt-kl2l): copy-on-write transient collections + persistent disj;
;; extends get/count/contains? to see through a transient. After collections.ss
;; (the persistent ops it delegates to).
(load "host/chez/transients.ss")

;; seq-native shims (jolt-y6mv): mapcat/take-while/drop-while/partition/sort +
;; reduced/reduced?/identical? — seed-native fns the overlay assumes are core
;; natives. Over the seq layer + jolt-compare, so loaded after converters.ss.
(load "host/chez/natives-seq.ss")

;; readable printer + output seams (jolt-9zhh, Phase 2 inc B): __pr-str1/__write/
;; __with-out-str/__eprint/__eprintf — the host seams the overlay print family
;; (pr-str/pr/prn/print/println/*-str) is built on. After converters.ss (uses
;; jolt-pr-str/jolt-str-join) + seq.ss (jolt-invoke).
(load "host/chez/printing.ss")

;; collection constructors + rand (jolt-agw6, Phase 2 inc A): bind the public
;; clojure.core names hash-map/hash-set/array-map/set/rand to the existing
;; pmap/pset ctors. After collections.ss (the ctors) + seq.ss (seq->list).
(load "host/chez/natives-coll.ss")

;; bit ops + parse-long/parse-double (jolt-cf1q.3 inc C): host-coupled scalar
;; seed natives over the all-flonum number model.
(load "host/chez/natives-num.ss")

;; multimethods (jolt-9ls5): defmulti/defmethod dispatch runtime. Needs jolt-invoke
;; (seq.ss), jolt=/key-hash/jolt-hash-map (collections.ss), jolt-atom? (atoms.ss),
;; jolt-pr-str (above), and the var-cell machinery — so loaded last.
(load "host/chez/multimethods.ss")

;; records + protocols (jolt-jgoc, Phase 2 inc D): defrecord/deftype/defprotocol/
;; extend-type/reify. A jrec record type set!-extended into the collection
;; dispatchers + a protocol registry. After multimethods.ss (chez-current-ns) and
;; the dispatchers/printers it wraps (collections/seq/values/converters/printing/
;; transients).
(load "host/chez/records.ss")

;; metadata (jolt-rkbc, Phase 2 inc E): meta / with-meta over an identity-keyed
;; side-table. After records.ss (jrec) + the collection ctors it copies.
(load "host/chez/natives-meta.ss")

;; host class tokens (jolt-13zk): bare class names (String/Keyword/File...) ->
;; canonical JVM class-name strings + (class x). After natives-meta.ss (jolt-type)
;; and the printer (jolt-str-render-one).
(load "host/chez/host-class.ss")

;; dynamic vars (jolt-9ls5): *clojure-version* / *unchecked-math* constants the seed
;; binds natively. After collections.ss (jolt-hash-map) + def-var!.
(load "host/chez/dynamic-vars.ss")

;; host tables + sorted collections (jolt-0zoy, Phase 2): jolt.host/tagged-table/
;; ref-put!/ref-get + the 25-sorted tier's runtime (sorted-map/sorted-set routed
;; through their :ops table). Loaded LAST — wraps the jrec-extended dispatchers
;; (records.ss), jolt-disj (transients.ss), and value-host-tags (records.ss).
(load "host/chez/host-table.ss")

;; lazy-seq bridge (jolt-dmw9, Phase 2): make-lazy-seq / coll->cells over the
;; cseq model — unblocks every overlay fn built on the lazy-seq macro (repeat/
;; iterate/cycle/dedupe/take-nth/keep/interpose/reductions/tree-seq/lazy-cat).
;; Loaded LAST so %ls-seq captures the fully-extended (sorted-aware) jolt-seq.
(load "host/chez/lazy-bridge.ss")

;; volatiles + sequence / transduce (jolt-xjx6, Phase 2): native volatile boxes +
;; the transduce/sequence entry points over into-xform/reduce-seq. After
;; natives-seq.ss (into-xform), seq.ss (reduce-seq) + atoms.ss (deref).
(load "host/chez/natives-xform.ss")

;; vars as first-class objects (jolt-n7rz, Phase 2): var?/var-get/deref/invoke/=/
;; pr-str over the rt.ss var-cell. After natives-xform.ss (chains deref) + the
;; printers. emit lowers :the-var to (jolt-var ns name).
(load "host/chez/vars.ss")

;; misc scalar natives (jolt-cf1q.3): UUID (random-uuid/parse-uuid/uuid?), format/
;; printf, tagged-literal, bigint. After the printers + converters (str/pr-str of
;; a uuid). Overlay names (uuid?/random-uuid/parse-uuid/tagged-literal?) re-asserted
;; in post-prelude.ss.
(load "host/chez/natives-misc.ss")

;; namespaces (jolt-yxqm, Phase 2): the namespace value model — find-ns/ns-name/
;; all-ns/the-ns/create-ns/in-ns/ns-publics/ns-map/ns-interns/ns-aliases/resolve/
;; find-var/ns-unmap/*ns*, over the var-table + chez-current-ns. Loaded LAST: needs
;; var-cell + var-cell-defined?, jolt-symbol/jolt-hash-map/jolt-assoc, chez-current-ns
;; (multimethods.ss), list->cseq (seq.ss), and the fully-patched printers (vars.ss).
(load "host/chez/ns.ss")

;; dynamic var binding (jolt-2o7x, Phase 2): the per-thread binding stack +
;; push/pop/get-thread-bindings/__thread-bound?/var-set/alter-var-root/__local-var.
;; Chains var-deref (rt.ss) and jolt-var-get (vars.ss) onto the stack, so a `binding`
;; frame is seen by every var read. Loaded LAST: needs the fully-extended var-read
;; paths + jolt-hash-map/pmap-fold/pmap-assoc (collections.ss).
(load "host/chez/dyn-binding.ss")

;; java.lang.String method interop (jolt-nfca, Phase 2): jolt-string-method, the
;; portable String/CharSequence surface record-method-dispatch falls through to on
;; a string target. After regex.ss (jolt-re-pattern/regex-t-irx) + records.ss
;; (which references jolt-string-method).
(load "host/chez/natives-str.ss")

;; host class statics + constructors (jolt-avt6, Phase 2): host-static-ref/
;; host-static-call/host-new + the jhost method registry. Loads LAST — it extends
;; record-method-dispatch (records.ss) and reuses natives-str helpers (str-trim,
;; ascii-string-down, re-split, str-split-drop-trailing) + the regex-t accessors.
(load "host/chez/host-static.ss")

;; generic dot-form dispatch (jolt-kuic): field access + map/vector member access
;; for the `.` / `.-field` desugar. Loads after host-static.ss so it wraps every
;; record-method-dispatch arm (jhost/number/regex/jrec/string) and falls through.
(load "host/chez/dot-forms.ss")

;; java.io.File + host file I/O (jolt-yyud): path-backed jfile record, slurp/spit/
;; flush, file-seq dir primitives, clojure.java.io/file. Loads LAST so its jfile
;; arm wraps the fully-built record-method-dispatch and the str/type/instance-check
;; extensions sit over every prior shim.
(load "host/chez/io.ss")

;; #inst values + java.time formatting (jolt-at0a inc X): jinst (RFC3339 ms) +
;; DateTimeFormatter/Instant/ZoneId/LocalDateTime/FormatStyle/Locale/Date. Loads
;; LAST — it extends record-method-dispatch / jolt-get / jolt= / jolt-hash /
;; jolt-pr-str / jolt-type / instance-check and uses host-static.ss's registries.
(load "host/chez/inst-time.ss")

;; Chez-side data reader (jolt-r8ku inc Y): read-string / __parse-next /
;; __read-tagged. Loads after inst-time.ss — __read-tagged reuses its #uuid/#inst
;; constructors, and the reader needs the full value/collection layer above.
(load "host/chez/reader.ss")
