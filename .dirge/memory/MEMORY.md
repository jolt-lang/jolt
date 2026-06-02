`fn*` and `defmacro` now capture `defining-ns` at definition time and restore it via `(ctx-set-current-ns ctx defining-ns)` / `(ctx-set-current-ns ctx saved-ns)` around body evaluation. This ensures symbols in function/macro bodies resolve in the defining namespace, not the calling context. Applies to both multi-arity and single-arity `fn*` forms.
§
Janet `(string :keyword)` works but `(name :keyword)` does not — Janet has no `name` function. Use `(string kw)` to convert keywords to strings.
§
Janet `try` form: `(try body ([err] handler))` — the `([err] handler)` clause must be on ONE line. Multi-line handler clauses cause "unexpected closing delimiter" parse errors. Correct: `(try (do-stuff) ([err] nil))`. Wrong: `(try (do-stuff) ([err] nil))` with `nil` on next line.
§
SCI depends on edamame (external Clojure parser) for `sci.core/eval-string`. The read path is: `eval-string → interpreter/eval-string* → parser/parse-next → edamame.core/parse-string`. Jolt's reader can't directly replace this without a shim. SCI also requires `clojure.tools.reader.reader-types` (indexing-push-back-reader, string-push-back-reader).
§
`:keys` destructuring in `let*` uses `:keys` keyword (not `"keys"` string) to look up the vector of keys: `{:keys [a b]}` → `(get pat :keys)` returns `(a b)` tuple where each is a keyword. Bind each using `(get val (keyword kname))`.
§
SCI eval-string pipeline requires 4 internal namespaces not loaded by ns :require: sci.impl.interpreter, sci.impl.parser, sci.impl.analyzer, sci.impl.opts. Their source files must be loaded separately. After loading all 9 SCI source files, these namespaces have 0 bindings. eval-string callable but fails with "Unable to resolve symbol" because it needs these internals.
§
Edamame shim lives in core.janet, embedded alongside core bindings. Uses `make-string-reader` to create `@{:s str :pos 0 :line 1 :col 1}` reader tables. `shim-edamame-eof` returns `:edamame/eof` keyword. `init-edamame-shim!` takes `ctx`, `parse-str` (e.g. Jolt's `parse-string`), and `read-f` (e.g. Jolt's `read-form`) as arguments to avoid requiring `./reader` from `core.janet`. Line/col tracking increments on newline (chr 10).
