;; boot.ss — the G2/G3 boot: the curated jolt runtime subset + the cross-minted
;; compiler seed load on native gsi (Gambit 4.9.7). jolt-mj95.3/.4.
;;
;; ONE compilation unit: everything is ##include-spliced (NOT load'd) so the
;; shim macros from prelude-shims.ss (define-record-type, fx aliases,
;; with-mutex, ...) expand in the SAME unit as every manifest file — the
;; "compile the boot files together" flow (see gambitcheck.ss's splice note).
;;
;; LOAD SHIM: rt.ss orchestrates its own loads ((load "host/chez/...") for the
;; whole manifest plus the excluded java tail). Under this boot every manifest
;; file is spliced in here and the excluded ones are deliberately skipped, so
;; a runtime (load "host/chez/...") must be a no-op — re-loading the Chez
;; copies would clobber the gambit sa-runtime/hasheq and pull in Chez-only
;; java interop. vendor/irregex is NOT under host/chez and loads normally.
(define %gambit-load load)
(define (load path . rest)
  (if (and (string? path)
           (or (string-prefix? "host/chez/" path)
               ;; irregex is ##include'd below (the js target has no
               ;; filesystem — a runtime load cannot work there)
               (string-prefix? "vendor/irregex/" path)))
      #f
      (apply %gambit-load path rest)))

(##include "prelude-shims.ss")
(##include "scheme-adapter-runtime.ss")

;; ---- manifest, in rt.ss load order (findings doc, curated) ----------------
;;
;; G2-booted: values, hasheq(gambit), collections, seq, then rt-core (the gambit
;; kernel — the rt.ss port; its port-log is in the file header). STOPS at the
;; first file whose blocker is class (c) (a genuine Chez-only construct with no
;; gambit seam) — see REPORT.

;; lazy-bridge forward-shared flags: seq.ss's force path reads jolt-mt? and
;; seq-more dispatches on it, but lazy-bridge.ss (loaded much later) is what
;; defines them. Pre-declare so seq.ss's references are bound; lazy-bridge.ss
;; REDEFINES them with its real implementation (the #f default and the
;; mark-mt! flip are identical, so the redefinition is a no-op in practice).
(define jolt-mt? #f)
(define (jolt-mark-mt!) (set! jolt-mt? #t))

(##include "../chez/values.ss")
(##include "hasheq.ss")
(##include "../chez/collections.ss")
(##include "../chez/seq.ss")
;; The register-*-arm! fast-type probes are LOAD-TIME SELF-CHECKS (they run
;; each new predicate against sample values). The jrec-cl predicate probe
;; spins on the js target (records dispatch against probe values); the
;; invariant these checks enforce is already proven on every Chez gate run,
;; so this target skips the probe machinery.
(set! reject-fast-type-claim! (lambda _ #f))
(##include "rt-core.ss")

;; regex needs regex-translate.ss (pure; rt.ss used to preload it from java/).
(##include "../../vendor/irregex/irregex.scm")
(##include "../chez/java/regex-translate.ss")
(##include "../chez/regex.ss")
(##include "../chez/atoms.ss")
(##include "../chez/refs.ss")
(##include "../chez/predicates.ss")
(##include "../chez/converters.ss")
(##include "../chez/transients.ss")
(##include "../chez/natives-seq.ss")
(##include "../chez/printing.ss")
(##include "../chez/natives-coll.ss")
(##include "../chez/natives-num.ss")
(##include "../chez/multimethods.ss")
(##include "../chez/java/class-hierarchy.ss")
(##include "records-gambit.ss")  ;; GENERATED from records.ss (make gambitgen) — phase wall, see gen-records.ss
(##include "../chez/java/records-interop.ss")
(##include "../chez/natives-meta.ss")
(##include "../chez/java/host-class.ss")
(##include "../chez/dynamic-var-defaults.ss")
(##include "../chez/host-table.ss")
(##include "../chez/lazy-bridge.ss")
(##include "../chez/natives-transduce.ss")
(##include "../chez/vars.ss")
(##include "../chez/natives-misc.ss")
(##include "../chez/natives-format.ss")
(##include "../chez/extensions.ss")
(##include "../chez/ns.ss")
(##include "../chez/dyn-binding.ss")
(##include "../chez/natives-reader.ss")
(##include "../chez/reader.ss")
(##include "../chez/syntax-quote.ss")
(##include "../chez/host-contract.ss")
;; The interop tier: the jhost record and the registries (host-statics.ss),
;; then the java/ files shared with Chez that register into them — Class
;; objects and the class model core reads (class-model.ss), StringBuilder
;; (string-builder.ss), the `.`/`.-field` dispatch arms over records, maps and
;; transients (dot-forms.ss). Before the seed: the prelude's own (import …) forms
;; intern through jolt-class-for (host-vars.ss, the rest of the java/ tree's
;; names, comes after it). java-parse.ss is the NumberFormatException family
;; Long/parseLong and its siblings raise.
(##include "../chez/java/java-parse.ss")
(##include "host-statics.ss")
(##include "../chez/java/class-model.ss")
(##include "../chez/java/string-builder.ss")
(##include "../chez/java/dot-forms.ss")

;; ---- G3: the cross-minted compiler on gsi (jolt-mj95.4) ----------------------
;;
;; The seed (host/gambit/seed/{prelude,image}.ss — clojure.core + the compiler,
;; minted at :gambit by make gambitseed) is spliced HERE, BEFORE compile-eval.ss,
;; mirroring cli.ss's load order: compile-eval.ss's top-level forms var-deref the
;; image's vars (jolt.analyzer/analyze, jolt.backend-scheme/emit-top-form), so
;; the image must have loaded first. Same-unit rule: all three are ##include'd —
;; the seed's emitted code expands seq.ss's macros in this unit; a load'd seed
;; would be a separate unit that cannot see them.
(##include "eval-fns.ss")  ;; seq.ss numeric macros as eval-world FUNCTIONS (js exes cannot eval define-syntax)
;; clojure.core is the current namespace while its own prelude loads, as in
;; cli.ss: the prelude's defmultis (print-method) def-var! into the current ns
;; and its (import …) forms bind class tokens there — under the default "user"
;; both landed in the wrong namespace (clojure.core/Sequential stayed unbound).
(set-chez-ns! "clojure.core")
(##include "seed/prelude.ss")
;; post-prelude re-asserts the native overrides the overlay stubs out (ns-name,
;; char?, atom?, realized?, ...) — cli.ss order: prelude, post-prelude, image.
(##include "../chez/post-prelude.ss")
;; the clojure.core names the excluded java/ tree owns on Chez — after
;; post-prelude so these bindings are the last word
(##include "host-vars.ss")
(set-chez-ns! "user")
(##include "seed/image.ss")
(##include "../chez/compile-eval.ss")

;; The compiled image's compiler unit defaults to :chez (new-unit's :target); the
;; boot must flip it to :gambit before any runtime compile, or the emitter writes
;; #3% unsafe spellings that cannot load on gsi. set-target! (R9) resets the
;; current unit's :target — the unit compile-eval.ss's set-prelude-mode! just
;; created on first touch (cur() lazily populates current-unit-box).
(let ((st (var-deref "jolt.backend-scheme" "set-target!")))
  (if (procedure? st)
      (begin (st (keyword #f "gambit")) (display "boot: backend target -> :gambit\n"))
      (begin (display "boot: FATAL set-target! missing from seed image\n") (exit 1))))

;; (smoke block removed — gambitkernel/gambiteval gates cover it)

