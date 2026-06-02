When you need to mutate a local with `set`, use `(var x nil)` not `(def x nil)`. `def` creates constants — `(set ns-name ...)` on a `def` fails with "cannot set constant". This hit us in loader.janet ns-name extraction loop.
§
core-renames MUST match actual function names in core.janet. `"-"`→`core-sub` NOT `core--`. `"fn?"` was missing entirely (caused silent nil). Missing entries → symbol treated as unknown global, returns nil. When adding: grep core.janet for actual `(defn core-XXX)` name, add to BOTH core-renames (string table) and core-fn-values (fn value table).
§
Bare tuples in Janet's `eval` are function calls: `(eval [1 2 3])` tries to call `1` as function. Always emit `['tuple 1 2 3]` or `(tuple 1 2 3)` in data-structure emitter. Similarly, `(eval (try body (sym handler)))` fails because `catch` is not a Janet special form — must be `(try body ([sym] handler))`. Discovered during Phase 5/6 compiler work.
§
Duplicate function definitions in the same file cause hard-to-diagnose "unknown symbol" errors. In compiler.janet, emit-quote-str was defined twice (once before emit-ast dispatch, once before emit-expr section). The second definition compiled but the first was used by emit-ast dispatch — causing "unknown symbol raw-form->janet". Always grep for the fn name before adding a new definition.
