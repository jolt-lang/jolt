Jolt Compiler Architecture (Phase 0-6, dfa9874→c1dde76): Two-phase — analyze-form (Clojure form → annotated AST) → emit-ast (→ Janet source string) or emit-expr (→ Janet data structures for eval). analyze-form takes [form bindings &opt ctx]; ctx needed for macro expansion. Symbol classification: bindings first (:local), then core-renames (:core-symbol), then plain (:symbol). compile-and-eval takes [form ctx]; pass nil for no macro ctx.

Key naming: Clojure - → core-sub (NOT core--). Missing from core-renames early: fn?, list, name, subs, nth. core-renames MUST match actual defn names in core.janet — add to BOTH core-renames (string table) and core-fn-values (fn value table).

Janet eval gotchas: bare tuples treated as fn calls → emit ['tuple ...]. Janet try = (try body ([err] handler)) NOT (try body (catch sym handler)). make-symbol: / at pos 0 = unqualified symbol. raw-form->janet: pass quoted forms through verbatim, don't re-analyze.

eval-string dispatch: When :compile? true, EVERYTHING goes through compiler EXCEPT stateful forms (defmacro, ns, deftype, defmulti, defmethod, require, in-ns, syntax-quote, set!, var, ., new). Bare symbols now also go through compile path (Phase 0 fix).

Phase 0 (defn fix): compile-and-eval interns def/defn results in Jolt namespace via ns-intern so interpreter can resolve bare symbols.

Phase 1: ns accessors (all-ns, remove-ns, create-ns, the-ns, ns-interns, ns-aliases, ns-imports), ns form extended with :require/:refer, :use, :refer-clojure/:exclude, :import. binding macro via push-thread-bindings/pop-thread-bindings.

Macro expansion: resolve-macro at analyze time → expand → re-analyze. Loop: (do (var _loop_N nil) (set _loop_N (fn [params] body)) (_loop_N vals...)). Recur: emits (loop-name args...) via :loop-name in AST.
§
Test files: test/phase6-final.janet (47 tests, 58 assertions — collections, math, predicates, comparison, seq ops, special forms, macros, complex nesting). Phase 1 tests appended to test/compiler-test.janet (ns accessors, ns form extensions). All 317 tests pass.
