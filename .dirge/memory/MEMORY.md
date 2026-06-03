Jolt Compiler Architecture (Phases 1-6, dfa9874→1de109f): Two-phase — analyze-form (Clojure form → annotated AST) → emit-ast (→ Janet source string) or emit-expr (→ Janet data structures for eval). analyze-form takes [form bindings &opt ctx]; ctx needed for macro expansion. Symbol classification: bindings first (:local), then core-renames (:core-symbol), then plain (:symbol). Two emitter paths: string (compile-form) and data structures (compile-ast). Core fn values resolved via core-fn-values table. compile-and-eval takes [form ctx]; pass nil for no macro ctx.

Key naming/facts:
- Clojure - → core-sub (NOT core--)
- core-nth did not exist — had to add both the function and core-bindings entry
- Missing from core-renames early: fn?, list, name, subs
- Bare tuples in Janet eval → treated as function calls. Always emit (tuple ...) or ['tuple ...]
- make-symbol: / at position 0 means unqualified symbol (was parsing empty ns)
- raw-form->janet converter for quote: don't re-analyze quoted forms, pass through verbatim
- emit-try-expr: Janet format is (try body ([err] handler)) not (try body (catch sym handler))
- Loop compilation: (do (var _loop_N nil) (set _loop_N (fn [params] body)) (_loop_N init-vals...))
- Recur compilation: rewrites to (loop-name arg1 arg2...) via :loop-name in AST

eval-string dispatch: When :compile? true, stateful forms (defmacro, ns, deftype, defmulti, defmethod, require, in-ns) use interpreter. All others (def, macros like defn) go through compile-and-eval. Macros expanded at analyze time via resolve-macro.

Remaining: syntax-quote, set! compiler support. deftype/defmulti/defmethod routed to interpreter.
§
Janet gotchas: (1) `parse` returns `[form, consumed-count]`, NOT `[form, error?]` — use parser/new→consume→eof→produce pipeline. (2) `:#inst` is invalid keyword literal — use `(keyword "#inst")` dynamically. (3) Janet `case` works for multi-arity simulation when `defn ([] body) ([x] body)` fails. (4) Bare tuples in eval are function calls — always use `['tuple ...]`. (5) Core `-` maps to `core-sub` NOT `core--`.
