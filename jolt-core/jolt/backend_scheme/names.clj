(ns jolt.backend-scheme.names
  "How the Scheme back end spells what it writes: Chez string literals, the
  injective munging of a Clojure name into a Scheme identifier (and the names it
  must stay clear of), and the direct-link binding name of a var. Pure text — no
  emit-session state — so every other back-end namespace can lean on it."
  (:require [clojure.string :as str]
            [jolt.op-registry :as op-registry]))

;; A Chez string literal. Every char outside printable ASCII becomes a
;; codepoint hex escape \x<cp>; ; the named escapes (\n \t \r \" \\) match what
;; Chez's reader accepts. For pure printable ASCII this is byte-identical to %j.
(defn- char-escape [cp]
  (cond
    (= cp 34) "\\\""
    (= cp 92) "\\\\"
    (= cp 10) "\\n"
    (= cp 9) "\\t"
    (= cp 13) "\\r"
    (and (>= cp 32) (< cp 127)) (str (char cp))
    :else (str "\\x" (format "%x" cp) ";")))

(defn chez-str-lit [s]
  (str "\"" (apply str (map (fn [c] (char-escape (int c))) s)) "\""))
;; --- identifier munging ------------------------------------------------------
;; Every Clojure name the emitter writes as a Scheme IDENTIFIER — a local, a
;; param, a fn's letrec self-name, a direct-link binding — goes through
;; munge-chars. A Clojure symbol can hold any character: the reader lets ' # |
;; and $ through, and (symbol s) lets everything through, including the chars
;; the Scheme reader takes as delimiters (a space, ; ( ) [ ] { } " , ` \) — an
;; identifier emitted bare with one of those either fails to read, reads as two
;; datums, or (a backslash, which Chez swallows) silently ALIASES a different
;; name. The escape is INJECTIVE, so two distinct Clojure names never munge to
;; one Scheme identifier: $ is the escape lead and is itself escaped FIRST
;; ($$ = a literal $), then ' -> $P, # -> $H, | -> $V, and any other unsafe
;; char -> $U<hex>$ (self-delimiting, so the codepoint width is free). Every $
;; in the output therefore begins one of those tokens, and decoding is a plain
;; left-to-right inverse. The same mapping runs at a binding and at every
;; reference, so resolution stays consistent.
;;
;; The unsafe set was derived by probing both readers (Chez 10.4, Gambit 4.9),
;; not from the standards: whitespace and controls (code < 33), the fourteen
;; ASCII chars below, and the non-ASCII Unicode White_Space characters, which
;; Chez's char-whitespace? delimits on while the reference reader lets them
;; through (Java's isWhitespace excludes NBSP). test/chez/unit.edn
;; munge-injective pins one row per class.
(def ^:private munge-unsafe-ascii
  ;; " # $ ' ( ) , ; [ \ ] ` { | }
  #{34 35 36 39 40 41 44 59 91 92 93 96 123 124 125})
(defn- munge-unicode-space? [cp]
  (or (= cp 0x85) (= cp 0xA0) (= cp 0x1680)
      (and (>= cp 0x2000) (<= cp 0x200A))
      (= cp 0x2028) (= cp 0x2029) (= cp 0x202F) (= cp 0x205F) (= cp 0x3000)))
(defn- munge-unsafe? [cp]
  (or (< cp 33)
      (contains? munge-unsafe-ascii cp)
      (and (> cp 127) (munge-unicode-space? cp))))
(defn- munge-char [cp]
  (cond (= cp 36) "$$"
        (= cp 39) "$P"
        (= cp 35) "$H"
        (= cp 124) "$V"
        :else (str "$U" (format "%x" cp) "$")))
(defn munge-chars [s]
  ;; a name with nothing to escape (nearly all of them) is returned as-is
  (if (some (fn [c] (munge-unsafe? (int c))) s)
    (apply str (map (fn [c] (let [cp (int c)] (if (munge-unsafe? cp) (munge-char cp) c))) s))
    s))
;; Scheme syntactic keywords. A jolt local with one of these names would, when
;; emitted verbatim, shadow the Scheme form in operator position (a local named
;; `if` would turn the special form (if …) the back end emits into a call), so
;; such locals are prefixed. Matches the spec: special-form heads are not
;; shadowable, but a value local may legally be named `if`.
(def ^:private scheme-reserved
  #{"if" "begin" "lambda" "let" "let*" "letrec" "letrec*" "quote" "quasiquote"
    "unquote" "set!" "define" "define-syntax" "cond" "case" "when" "unless"
    "and" "or" "do" "else" "guard" "parameterize" "delay" "values"})

;; clojure.core ops emitted as a BARE Scheme name (where native-ops maps the op
;; to itself: + - * / < > min max …). A local binding with one of these names
;; would otherwise shadow the emitted prim — e.g. (fn [max] (clojure.core/max …))
;; emits (max …) calling the param — so such locals are prefixed, like reserved
;; words. Derived from native-ops so the two never drift.
(def ^:private bare-native-names
  (set (keep (fn [[k v]] (when (= k v) k)) op-registry/native-ops)))

;; All Scheme identifiers the back end emits bare — every :call, :dbl, :lng,
;; :fixed, :value, and :bd name from op-registry, plus a closed set of runtime
;; helpers the registry doesn't enumerate (devirt dispatchers, jolt-invokeN,
;; var-deref, list->cseq, literal/interop heads, etc.). Derived from op-registry
;; so it stays in sync with per-op facts. A local whose munged name matches ANY
;; of these would shadow the emitted runtime procedure (e.g. (let [jolt-nil 5]
;; nil) returns 5 if the local is emitted verbatim).
;; INVARIANT: any NEW bare Scheme head an emit* clause outputs — a procedure or
;; special-form name emitted as a call head that a user local could shadow —
;; MUST be added to the `helpers` set below. The op-registry derivation covers
;; registry-driven heads; everything else is enumerated there by emission site.
(def ^:private rt-emitted-names
  (let [from-registry op-registry/registry-emitted-names
        helpers #{"jolt-nil" "jolt-invoke" "jolt-invoke0" "jolt-invoke1"
                  "jolt-invoke2" "jolt-invoke3" "jolt-invoke4"
                  "jolt-invoke-t0" "jolt-invoke-t1" "jolt-invoke-t2" "jolt-invoke-t3" "jolt-invoke-t4"
                  "var-deref" "list->cseq" "cseq->list"
                  "jolt-rest-seq" "jolt-register-variadic!"
                  "make-jrec" "jrec-field-at" "jrec-field-set!"
                  "devirt-resolve" "devirt-resolve-fl"
                  "register-clone*" "protocol-resolve"
                  "protocol-dispatch1" "protocol-dispatch2" "protocol-dispatch3"
                  "jolt->fl" "jolt->fx" "jolt->fx-ret"
                  ;; the bit-op inline (bit-fx-ops): its fixnum guard and the
                  ;; four Chez fixnum ops it open-codes to; flonum?/fixnum? also
                  ;; guard the hint-coercion inline (emit-nhint-coerce) and the
                  ;; :fl-coerce contagion widening, whose fixnum->flonum has been
                  ;; emitted bare since before this set existed.
                  "fixnum?" "flonum?" "fixnum->flonum"
                  ;; the cast fast path on a proven double (the :coerce arm's
                  ;; :fl-arg branch): its lane and the runtime's window
                  ;; constants, which the emitted test reads by name
                  "flonum->fixnum"
                  "jolt-cast-lo-long" "jolt-cast-hi-long" "jolt-cast-lo-int" "jolt-cast-hi-int"
                  "jolt-cast-lo-short" "jolt-cast-hi-short" "jolt-cast-lo-byte" "jolt-cast-hi-byte"
                  "jolt-cast-lo-char" "jolt-cast-hi-char"
                  ;; the trace site-restore around a non-tail call (site-restore,
                  ;; lookup-restore): it reads and writes the site slots and
                  ;; tests the slot's pair. honeysql's (let [pair? ...]) turned
                  ;; that test into a call to its boolean. jolt-site-call is the
                  ;; local a clause binds the restore to (site-call-helper).
                  "pair?" "virtual-register" "set-virtual-register!" "jolt-site-call"
                  "fxand" "fxior" "fxxor" "fxnot"
                  "jolt-register-source!" "jolt-proto-epoch"
                  ;; --- bare heads emitted at sites the registry doesn't cover ---
                  ;; INVARIANT: any new bare Scheme head an emit* clause outputs
                  ;; (a head a user local could shadow) MUST be added here. Literal
                  ;; + form emission (emit-const): keyword/char construction.
                  "keyword" "integer->char"
                  ;; fn forms (emit-fn): a multi-arity fn lowers to case-lambda.
                  "case-lambda"
                  ;; binding + try/finally (emit-try): both thread through
                  ;; dynamic-wind; a binding form and a finally both emit it.
                  ;; jolt-finally-in is that dynamic-wind's before-thunk, emitted
                  ;; by name because the fiber park recognises it with eq? — a
                  ;; shadowed or inlined copy would make the finally run mid-park.
                  "dynamic-wind" "jolt-finally-in"
                  ;; host interop (emit-invoke static call, :host-new,
                  ;; :host-static field ref).
                  "host-new" "host-static-call" "host-static-ref"
                  ;; per-site caches: the static member site and the instance?
                  ;; site, each with the constructor its hoisted cell calls
                  "jolt-once-tag" "jolt-once-clear!" "host-static-ref-site" "host-static-proc-site" "host-static-site-make"
                  "jolt-instance-site" "jolt-instance-site-make"
                  "jolt-class-for-site" "jolt-class-for-site-make"
                  ;; record/reify protocol-method dispatch (:host-call fallback
                  ;; for any .method not in supported-host-methods).
                  "record-method-dispatch"
                  ;; proven-target interop natives (string-direct-emit,
                  ;; keyword-direct-emit, sb-direct-emit). These are bare heads a
                  ;; user local could shadow and the "jolt-" prefix net does not
                  ;; catch them, so they are enumerated per the invariant above:
                  ;; (let [str-trim (fn [_] :shadowed)] (.trim ^String s)) would
                  ;; otherwise emit a call to the local.
                  "str-trim" "str-needle" "str-starts-with?" "str-ends-with?"
                  "str-index-of" "str-index-of-any" "str-replace-literal"
                  "java-string-hash" "java-symbol-hash"
                  "keyword-t-ns" "keyword-t-name"
                  "sb-append!" "sb-str" "sb-length" "sb-piece" "sb-char-at" "->num"
                  "sb-append*!" "sb-str*" "sb-length*" "sb-char-at*"
                  ;; cell-cached var deref (the whole-program var-cache? path).
                  "var-cell-deref"
                  ;; devirt cached-desc lookup (emit-invoke ctor inlining).
                  "hashtable-ref"
                  ;; top-level def / forward-declare / ns-value splice
                  ;; (emit-def-cached, :forward-decl, :the-ns).
                  "define" "def-var!" "def-var-plain!" "def-var-with-meta!"
                  "declare-var!" "intern-ns!"
                  ;; ffi lowering (emit-ffi-fn/emit-ffi-callable: the sa-* adapter
                  ;; syntaxes a Chez foreign-procedure/callable expands to).
                  "sa-foreign-procedure" "sa-foreign-procedure-blocking"
                  "sa-foreign-callable" "sa-foreign-callable-collect-safe"
                  "jolt-ffi-native-error-procedure" "call-with-values"
                  ;; bare :& lowering (emit-ffi-bare-varargs-fn): the dispatcher
                  ;; spreads the inferred tail with Scheme's own apply, keeping
                  ;; the fixed arguments as arguments rather than appending them.
                  "apply"
                  ;; layout lowering (emit-ffi-layout): the ftype heads the
                  ;; struct/array metadata is computed with. A local named
                  ;; ftype-pointer-address answered ITS value as the layout's
                  ;; :alignment; the other four broke the compile.
                  "define-ftype" "make-ftype-pointer" "ftype-sizeof"
                  "ftype-&ref" "ftype-pointer-address"
                  ;; ftype and foreign-procedure syntax keywords, matched by
                  ;; binding, so a local of the same name breaks the form.
                  "array" "struct" "foreign-procedure" "__varargs_after"
                  ;; Scheme procedures emitted bare: the try/finally fiber mask
                  ;; (fx+ fx- fx>), the PIC and record-ctor caches, string and
                  ;; keyword interop, symbol hashing, and the unsafe-prefixed ops
                  ;; (fx= fl+ fl- vector-ref) on a target with no unsafe prefix.
                  ;; (let [fx+ 1] (try 1 (finally nil))) called the local.
                  "+" "=" "not" "eq?" "null?" "procedure?" "string?"
                  "car" "cdr" "cons" "vector" "vector-ref" "vector-set!"
                  ;; debug locals (*dbg-scope*): every tail leaf and recur
                  "keep-live"
                  "string-append" "string-length" "string-downcase" "string-upcase"
                  "fx+" "fx-" "fx>" "fx=" "fx=?" "fx>=?" "fl+" "fl-"
                  "flsingle" "flvector-ref" "flvector-set!"
                  ;; runtime helpers emitted bare at boxed-float, record-field,
                  ;; PIC, ffi, def, and image sites.
                  "make-jfloat" "jfloat?" "jfloat-fl" "jfloat-unbox" "jnum->exact"
                  "jrec-field-site-make" "jrec-field-site-ref" "jrec-own-field" "jolt-set-own-field!" "jrec-pic-desc" "jrec?" "jrec-desc"
                  "jrec1?" "jrec2?" "jrec3?" "jrec4?" "jrec5?" "jrec6?" "jrec7?" "jrec8?"
                  "keyword-t?" "mark-macro!" "set-var-meta!" "throw-jvm"
                  "def-var-linked!" "amap-slots->pmap"
                  "image-fn-form-maker!" "image-fn-form-src" "image-register-fn-form!"}]
    (into from-registry helpers)))

;; A munged name both Scheme readers would take for a NUMBER rather than an
;; identifier. The reference reader yields SYMBOLS for +i, -i, +inf.0, -nan.0,
;; .5, -.5 and the like (its number match needs a digit right after the sign),
;; and emitted bare they abort compilation ("invalid bound variable 0+1i").
;; Derived by probing both readers over the sign/dot-led tokens a symbol can
;; start with: a number begins with a digit; with . and a digit; or with a sign
;; and then a digit, a dot, a lone i, or inf.0/nan.0 in any case. A lone . is
;; the pair dot, unreadable in a binding position. Over-approximates on purpose
;; (+.. and +inf.0x are identifiers): a spare prefix costs nothing where a
;; missed number is a reader error. The common -name/+name shapes (-invoke,
;; +inc, -i0) are untouched. A digit-led name can only come from (symbol s).
(defn- digit-char? [c] (let [cp (int c)] (and (>= cp 48) (<= cp 57))))
(defn- number-lookalike? [s]
  (let [n (count s) c0 (nth s 0) c1 (when (> n 1) (nth s 1))]
    (or (digit-char? c0)
        (and (= c0 \.) (or (nil? c1) (digit-char? c1)))
        (and (or (= c0 \+) (= c0 \-))
             (some? c1)
             (or (digit-char? c1)
                 (= c1 \.)
                 (and (= n 2) (or (= c1 \i) (= c1 \I)))
                 (and (or (= c1 \i) (= c1 \I) (= c1 \n) (= c1 \N))
                      (let [r (str/lower-case (subs s 1))]
                        (or (str/starts-with? r "inf.0") (str/starts-with? r "nan.0")))))))))

;; The emitted identifier for a Clojure name: munge-chars (above), then a $R
;; prefix on any name that would otherwise collide with something the emitter
;; writes bare or the reader would not take as an identifier — a Scheme keyword
;; (a local named `if` would turn the (if …) the back end emits into a call), a
;; bare-emitted native op, ANY runtime-emitted identifier (rt-emitted-names, or
;; the "jolt-"/"jv$" prefixes as a safety net for additions not yet in that
;; registry), a number lookalike, or the empty name. The prefix is an escape
;; token, not a plain `_`: a bare `_` is text a user can write, so `if` and
;; `_if` both emitted `_if` and (let [if 1 _if 2] if) answered 2. No unprefixed
;; munge can start with $R (an unprefixed $ is always $$/$P/$H/$V/$U), so the
;; prefix keeps munge-name injective. A $ -> $$ still leaves the jv$/jolt-
;; prefix tests true, and those runtime names never reach munge-name.
(defn munge-name [s]
  (let [s (munge-chars s)]
    (if (or (zero? (count s))
            (contains? scheme-reserved s)
            (contains? bare-native-names s)
            (contains? rt-emitted-names s)
            (.startsWith ^String s "jolt-")
            (.startsWith ^String s "jv$")
            (number-lookalike? s))
      (str "$R" s) s)))

;; A direct-link Scheme binding name for a var: jv$ + the munged fqn (ns/name),
;; the same identity the direct-link registry keys on (dl-fqn), so two distinct
;; vars can never share a binding. It used to be jv$<ns>$<name> over per-part
;; _D_/_H_/_Q_/_V_ substitutions — text a user can write, so (def _V_ …) and
;; (def | …) bound the same jv$app$_V_ — and even with injective parts a $
;; separator can be forged by an escape in the ns name (a$P + $ + Pc reads as
;; a + $ + P$Pc). The / is a plain identifier char on both hosts. Nothing
;; parses the shape back: the registry maps a binding to its (ns, name), and
;; dce.ss / the jv$ shadow guard test only the prefix.
(defn dl-fqn [ns nm] (str ns "/" nm))
(defn dl-name [ns nm] (str "jv$" (munge-chars (dl-fqn ns nm))))
