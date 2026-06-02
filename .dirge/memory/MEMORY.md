SCI load order: macros→protocols→types→unrestrict→vars→lang→utils→namespaces→core. Utils after lang (needs lang/->Namespace, vars/unqualify-symbol). Namespaces after utils (needs clojure-core-ns, dynamic-var). Core after namespaces (needs *1,*2,*3,*e, resolve). Current: 314/317 ok, 3 remaining fails in namespaces (major destructure, clojure-version-var update, avoid-method-too-large iterable).
§
Janet `last` works only on indexed types (tuple, array). On strings it returns nil. Use `(s (- (length s) 1))` to get the last character of a string, or `(string/slice s (- (length s) 1))` for the last char as string. `(last "hello")` → nil, not `\o`.
§
resolve-sym returns `:jolt/not-found` sentinel to distinguish nil local bindings from absent ones, preventing accidental fallthrough to global resolution. Falls back to `clojure.core` namespace for unqualified symbols. `bind-put` helper wraps nil as `:jolt/nil`; resolve-sym unwraps. Multi-arity `fn*` dispatches on fixed-params count vs variadic args.
§
Janet `(string :keyword)` returns `"keyword"` (without colon). Use this for keyword→string conversion in destructuring code. Avoid `(name :keyword)` in Jolt evaluation context — `name` is a Janet built-in but may not be available in all eval contexts.
§
`:jolt/nil-sentinel` used in `core-bindings` map for vars that should have nil root values. Janet table literals drop nil entries: `@{"*1" nil}` → empty table. `init-core!` unwraps sentinels back to nil with `(if (= fn :jolt/nil-sentinel) nil fn)`. Applies to `*1`, `*2`, `*3`, `*e` and potentially other nil-rooted vars.
§
`fn*` and `defmacro` now capture `defining-ns` at definition time and restore it via `(ctx-set-current-ns ctx defining-ns)` / `(ctx-set-current-ns ctx saved-ns)` around body evaluation. This ensures symbols in function/macro bodies resolve in the defining namespace, not the calling context. Applies to both multi-arity and single-arity `fn*` forms.
