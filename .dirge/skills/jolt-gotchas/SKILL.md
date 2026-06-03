---
name: jolt-gotchas
description: Common pitfalls and workarounds discovered during Jolt implementation
---

# jolt-gotchas

Recurring pitfalls and their fixes discovered across all implementation phases.

## PHM/Set Metadata Key Leakage

PHM and set internal keys (`:jolt/deftype`, `:cnt`, `:buckets`, `:_meta`, `:jolt/type`, `:phm`) leak into `pairs`/`keys` iteration. Must filter in merge, merge-with, keys, vals, and print-collection.

```janet
(when (and (not= k :jolt/deftype) (not= k :cnt) 
           (not= k :buckets) (not= k :_meta)
           (not= k :jolt/type) (not= k :phm)) ...)
```

## Keywords with `#` Are Invalid Janet Literals

`:#inst`, `:#uuid` cause parse errors. Use dynamic table construction:
```janet
(let [dr @{}] (put dr (keyword "#inst") fn) dr)
```

## Janet `break` Only Works in Loops

Does NOT work inside `let`. Use `(var found nil)` + `(set found val) (break)` pattern.

## Bare Tuples in `eval` Are Function Calls

`(eval [1 2 3])` calls `1` as function. Use `['tuple 1 2 3]` in data-structure emitter.

## Janet `case` for Multi-Arity

Janet lacks Clojure-style multi-arity defn. Use `(defn f [& args] (case (length args) 1 ... 2 ...))`.

## core-renames + core-fn-values Must Stay in Sync

Both tables must be updated together when adding core fns. Missing entries = silent nil returns. `"-"` is `core-sub` NOT `core--`.

## `set!` Field Mutation Reader Quirk

`(set! (.-x obj) val)` parses as array with `.-x` symbol head — not as standalone `.-x` symbol. Check for this case before the `(. obj -field)` shorthand.

## Janet `cond` Requires `true` Guard for Catch-All

A bare expression in the last position of `cond` is treated as a **test** clause (not body). Use `true` as the test:

```janet
(cond (nil? x) (buf "nil") (number? x) (buf (string x)) true (buf (string x)))
```

Without `true`, the last expression executes as a side-effect test between branches. Hit us in buffer-based write-value — raw tuple addresses leaked into REPL output.

## Janet `cond` Requires `true` Guard for Catch-All

A bare expression in the last position of `cond` is treated as a **test** clause (not body). It executes between other branches as a side-effect test. Use `true` as the test to make it a proper catch-all:

```janet
(cond
  (nil? x) (buf "nil")
  (number? x) (buf (string x))
  true (buf (string x)))  ; ← `true` required
```

Without `true`, `(push-str buf (string v))` in the last position leaked raw tuple addresses into REPL output.