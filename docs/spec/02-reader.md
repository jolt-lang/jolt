# §2 The Reader (Lexical Syntax)

**Status**: token grammar drafted; reader-macro catalog complete with
normative entries; #inst and literal-collapse divergences resolved.
Conformance: jolt `reader-forms-spec` + `reader-syntax-spec` (granularity
model: jank's per-construct corpus, 62 files under
`test/jank/{reader-macro,syntax-quote}` — adapted rows cited per entry).

The reader maps a stream of characters to *forms* (data). Reading is
independent of evaluation: every form the reader produces is a value of the
language (§4), and `read-string` exposes the reader as a function. Evaluation
of forms is §1's concern; only `quote`-family reader macros reference it here.

## 2.1 Tokens

Whitespace is space, tab, newline, return, **and comma** (`,` is whitespace —
S1). A `;` begins a comment to end of line (S2). Tokens:

```
form        := literal | symbol | keyword | list | vector | map | set
             | reader-macro-form
list        := '(' form* ')'
vector      := '[' form* ']'
map         := '{' (form form)* '}'
literal     := nil | boolean | number | string | character
nil         := 'nil'        boolean := 'true' | 'false'
```

- S3. A map literal MUST contain an even number of forms; duplicate keys
  MUST be an error at read time.
- S4. A set literal (`#{…}`, §2.3) with duplicate elements MUST be an error
  at read time.

### Numbers

```
integer  := ['+'|'-'] (digits | '0' [xX] hexdigits | '0' octdigits | radixR digits)
float    := ['+'|'-'] digits '.' digits? exponent? | ['+'|'-'] digits exponent
ratio    := ['+'|'-'] digits '/' digits            ; host-numeric-tower (§4 note)
exponent := [eE] ['+'|'-'] digits
```

- S5. Trailing `N` (BigInt) and `M` (BigDecimal) suffixes are part of the
  grammar; their value semantics are the §4 numeric-tower question.
  Implementations without those towers SHOULD read them as the nearest
  numeric type and MUST document the choice. The Chez host carries the full
  tower: `N` reads as an exact integer (arbitrary precision) and `M` as a real
  BigDecimal — `1.5M`, `0.0M`, `3M` — with value equality ignoring scale
  (`1.0M = 1.00M`), `(class 1.5M)` ⇒ `java.math.BigDecimal`, and `decimal?` true.

### Symbols and keywords

```
symbol   := name | ns '/' name        ; '/' alone names the division fn
keyword  := ':' name | ':' ns '/' name | '::' name | '::' alias '/' name
```

- S6. Symbol constituent characters: alphanumerics and `* + ! - _ ' ? < > =
  . $ & %` (with `%` and `&` further constrained inside `#()`); a symbol
  MUST NOT begin with a digit; `.` and `/` have positional restrictions.
- S7. `::kw` MUST resolve to the current namespace at *read* time
  (`::k` in ns `user` reads as `:user/k`); `::alias/k` resolves `alias` through
  the current namespace's aliases. (Clojure raises a read error for an unknown
  alias; jolt reads it as `:alias/k`.)

### Strings and characters

- S8. Strings are `"…"` with escapes `\" \\ \n \t \r \b \f \uNNNN \oNNN`.
- S9. Character literals: `\c`, the named set `\newline \space \tab
  \return \backspace \formfeed`, unicode `\uNNNN`, octal `\oNNN`.

**Conformance** (2.1): jolt `reader-syntax-spec` "dispatch & sugar";
clojure-test-suite reader files; jank `form/*` literal dirs. S3/S4 duplicate
checks → UNVERIFIED (rows to add).

## 2.2 Quote-family reader macros

| Sugar | Reads as | |
|---|---|---|
| `'form` | `(quote form)` | S10 |
| `@form` | `(clojure.core/deref form)` | S11 |
| `^meta form` | form with metadata attached (see below) | S12 |
| `#'sym` | `(var sym)` | S13 |
| `` `form `` | syntax-quote (§2.4) | |
| `~form`, `~@form` | unquote / unquote-splicing — only within syntax-quote (S14: MUST error outside) | |

- S11. `@form` reads as `(clojure.core/deref form)` — the operator is the
  fully-qualified `clojure.core/deref`, not a bare `deref`, so `@x` still
  dereferences in a namespace that excludes and rebinds `deref`
  (`(ns … (:refer-clojure :exclude [deref]))`), matching Clojure.
- S12a. `^:kw form` ≡ `^{:kw true} form`; `^Sym form` ≡ `^{:tag Sym} form`;
  `^"str"` ≡ `^{:tag "str"} form`. Multiple `^` stack, rightmost innermost,
  merged left-over-right.
- S12b. Type hints are semantically transparent: a hint MUST NOT change a
  program's result. Hints parse in every position they do in Clojure (params,
  `let` bindings, `def` names, return position, arbitrary forms) and are
  otherwise inert. As a non-normative optimization, jolt recognizes two hints
  on a local as an assertion that a constant-keyword lookup may skip its
  runtime representation guard: `^:struct` (a plain struct/record map) and
  `^Name` where `Name` is a `defrecord`/`deftype`. The assertion is the
  programmer's (an inaccurate hint yields a wrong lookup, like a wrong Clojure
  `^String`); `JOLT_CHECK_HINTS=1` turns a violated hint into an error at no
  cost to unchecked builds. See RFC 0004.
- S13a. `#'ns/sym` MUST denote the same var as `(var ns/sym)`:
  `(= (var clojure.core/str) #'clojure.core/str)` is true.

**Conformance**: jolt `reader-forms-spec` "var-quote #'", "metadata ^",
"syntax-quote"; jank `var-quote/pass-qualified.jank`, `metadata/*`.

## 2.3 Dispatch (`#`) reader macros

| Form | Meaning | Entry |
|---|---|---|
| `#{…}` | set literal | S4 above |
| `#"…"` | regex literal — reads to a regex value; escaping is regex-level, not string-level (single `\d`) | S15 |
| `#(…)` | anonymous fn | S16 below |
| `#_form` | discard | S17 below |
| `#?(…)` / `#?@(…)` | reader conditional (+splicing) | S18 below |
| `##Inf ##-Inf ##NaN` | symbolic floats | S19 |
| `#tag form` | tagged literal | S20 below |
| `#! …` | shebang comment line (implementations SHOULD accept) | |

### S16 — anonymous function `#(…)`

- `#(body)` reads as `(fn [args…] (body))` with parameters derived from the
  `%`-symbols appearing in body: `%`≡`%1`, `%n` positional, `%&` the rest
  parameter. Arity = highest `%n` mentioned (plus rest if `%&`).
- The `%`-symbols are collected from the WHOLE body, recursing through every
  nested form including vector, map and set literals — `#(assoc {} :k %)`,
  `#(hash-set % %2)` and `#(get {:t %} :t)` all see their `%`s. (A reader that
  scanned only call forms would miscompile `#(identity {:text %})` as a 0-arg fn.)
- The synthesized parameters are auto-gensyms (their names carry the `#` suffix,
  like Clojure's `p1__N#`), so an `#()` written inside a syntax-quote survives:
  the params are mapped consistently and left unqualified rather than being
  qualified to the current namespace (a qualified symbol is not a valid
  parameter). E.g. ``  `(map #(inc %) xs) `` expands correctly inside a macro.
- `#()` literals MUST NOT nest.

```clojure
(#(+ %1 %2) 1 2)            ;=> 3
(apply #(apply + %&) [1 2 3]) ;=> 6
(map #(* % %) [1 2])        ;=> (1 4)
```

### S17 — discard `#_`

- `#_form` reads and discards the next form entirely (it is never evaluated).
- Discards compose: `#_ #_ a b` discards two following forms.
- `#_` inside collection literals removes the element: `[1 #_2 3]` ⇒ `[1 3]`.

### S18 — reader conditionals

- `#?(:feat₁ f₁ :feat₂ f₂ …)` reads as the form of the first feature key the
  platform satisfies, else nothing. `:default` matches any platform.
  `#?@(…)` splices a sequential form into the surrounding context.
- Feature keys are implementation-defined; each implementation MUST document
  its feature set, and SHOULD follow the portable convention *own dialect key
  + `:default`*. Matching MUST be by **clause order** — the first clause whose
  key the platform satisfies wins (`#?(:default 5 :clj 6)` is `5` everywhere)
  — not by key priority. Implementations SHOULD provide a per-loading-context
  compatibility override for foreign-dialect libraries. (jolt:
  `#{:jolt :clj :default}` — jolt emulates `clojure.lang.*`/`java.*`, so it
  reads the `:clj` branch of a `.cljc` library by default; a library can put a
  `:jolt` branch first to override, or a loading context can call
  `reader-features-set!`. History in RFC 0002.)
- Reader conditionals MUST be an error outside `.cljc`-style reading unless
  the implementation documents otherwise.

### S19 — symbolic values

`##Inf`, `##-Inf`, `##NaN` read as the IEEE-754 values. `(= ##NaN ##NaN)` is
false; `(NaN? ##NaN)` is true.

### S20 — tagged literals

- `#tag form`: the reader resolves `tag` in the data-reader table and MUST
  apply the reader function to the *read* form, yielding its result as the
  read value. An unknown tag MUST be a read error (jank
  `fail-unsupported-tag`).
- Built-in tags every implementation MUST provide: `#uuid "…"` → a UUID
  value (§9 `parse-uuid` semantics — round-trips through printing), and
  `#inst "…"` → an instant value: RFC3339 with partial-timestamp defaults
  (`#inst "2020"` ≡ `#inst "2020-01-01T00:00:00.000-00:00"`), equality by
  instant (offset-normalized), `inst?`/`inst-ms` (epoch milliseconds), printed
  canonically as `#inst "yyyy-MM-ddThh:mm:ss.fff-00:00"` and round-tripping. A
  malformed timestamp MUST be an error.

**Conformance** (2.3): jolt `reader-forms-spec` "#() (% %N %&)" + new rows
(symbolic values, stacked discard, conditionals); `uuid-spec` reader-literal
group; jank `reader-macro/{function,regex,uuid,symbolic-value}/*`,
`fail-unsupported-tag.jank`.

## 2.4 Syntax-quote

Syntax-quote (`` ` ``) is read-level template construction with namespace
resolution:

- S21. Inside syntax-quote, an unqualified symbol that resolves in
  `clojure.core` MUST be qualified to `clojure.core/sym`; a symbol resolving
  through a namespace alias MUST be qualified to the aliased namespace; an
  unresolved symbol MUST be qualified to the current namespace. Special-form
  names stay bare.
- S22. `sym#` generates a fresh symbol, stable *within one syntax-quote
  template* (all `sym#` in the same template denote the same generated
  symbol; distinct templates generate distinct symbols).
- S23. `~form` inserts the value of `form`; `~@form` splices a sequential
  value; `~'sym` is the idiom for an intentionally-unqualified symbol.
- S24. Syntax-quote distributes through collection literals (vectors, maps,
  sets) — qualification and unquoting apply inside them.
- S25. A syntax-quoted self-evaluating literal is the literal, collapsed at
  read time — so nested/adjacent backticks over literals are inert:
  `(= "meow" ```"meow")` is true. General nested syntax-quote over symbols
  and collections expands recursively (quasiquote semantics) — that general
  case remains UNVERIFIED pending dedicated conformance rows.

**Conformance**: jolt `reader-forms-spec` "syntax-quote" (gensym, unquote,
splice) + conformance "syntax-quote fully-qualifies"; jank
`syntax-quote/{pass-gensym,pass-namespace-resolution,pass-resolve-alias,
unquote,unquote-splice}/*`. S25 → UNVERIFIED.

## 2.5 What the reader is not

The reader performs **no macroexpansion and no evaluation** (tagged-literal
reader functions are the deliberate exception, S20). Forms read identically
whether or not they will be evaluated; `read-string` of any printable value
`v` followed by evaluation yields a value equal to `v` for the
self-evaluating types (§4 print/read round-trip contract).
