# Chez Phase 3 inc 5a (jolt-50xx) — value-parity gate for the PORTABLE Clojure
# reader (jolt.reader) vs the Janet seed reader (src/jolt/reader.janet, oracle).
#
# jolt.reader holds the lexing/parsing LOGIC in portable Clojure and delegates
# form construction + number parsing to the jolt.host contract. Here it runs
# interpreted ON THE JANET HOST (loaded via bootstrap-load-source); each input is
# read by BOTH readers and the resulting FORMS compared with jolt's own = (so the
# representation, host-built either way, matches structurally). Positions differ
# (char vs byte indices) and are not compared.
#
#   janet test/chez/reader-parity.janet      (from repo root)
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as r)
(import ../../src/jolt/types_ctx :as tctx)
(import ../../src/jolt/types_ns :as tns)
(import ../../src/jolt/types_var :as tvar)
(import ../../src/jolt/core :as core)

(var total 0) (var fails 0)
(defn ok [name pred &opt extra]
  (++ total)
  (if pred (printf "ok: %s" name)
    (do (++ fails) (printf "FAIL: %s   %s" name (or extra "")))))

(def ctx (api/init {:compile? true}))
(def src (get (get (ctx :env) :embedded-sources) "jolt.reader"))
(assert src "jolt.reader not embedded (check stdlib_embed collect)")
(backend/bootstrap-load-source ctx "jolt.reader" src)
(def read-one (tvar/var-get (tns/ns-find (tctx/ctx-find-ns ctx "jolt.reader") "read-one")))
(assert read-one "jolt.reader/read-one not found")

# jolt's own value equality (the host =), the same comparator the corpus gate uses.
(defn jeq [a b] (core/jolt-equal? a b))

(defn check [input]
  (def w (protect (in (r/parse-next input) 0)))   # Janet seed reader (oracle)
  (def g (protect (read-one input)))               # portable Clojure reader
  (cond
    # both readers throw on the same input = faithful parity (safety net for
    # genuinely-invalid input). We FIX latent bugs rather than reproduce them
    # (fix-bugs-dont-reproduce), so this should be rare.
    (and (not (w 0)) (not (g 0))) (ok input true)
    (not (w 0)) (ok input false (string "janet threw, clj didn't: clj=" (string/format "%p" (g 1))))
    (not (g 0)) (ok input false (string "clj threw, janet ok: " (string (g 1))))
    (ok input (jeq (w 1) (g 1))
        (string "clj=" (string/format "%p" (g 1)) " janet=" (string/format "%p" (w 1))))))

# For inputs where the Janet seed reader is a BUGGY oracle (a latent bug we FIX in
# the port rather than reproduce, fix-bugs-dont-reproduce), assert the portable
# reader against the hand-verified correct value. The Janet seed isn't fixed —
# it's deleted in Phase 5 — so we don't compare against it here.
(defn check-correct [input expected]
  (def g (protect (read-one input)))
  (if (not (g 0))
    (ok input false (string "clj threw: " (string (g 1))))
    (ok input (jeq expected (g 1))
        (string "clj=" (string/format "%p" (g 1)) " want=" (string/format "%p" expected)))))

# --- inc 5a: atoms -------------------------------------------------------------
# nil / bool
(each i ["nil" "true" "false"] (check i))
# symbols (plain, ns'd, punctuation, special chars)
(each i ["foo" "foo-bar" "my.ns/bar" "+" "-" "*" "->" "<=" "some?" "a1" "x'" "ns/+"] (check i))
# keywords
(each i [":a" ":foo-bar" ":my.ns/key" "::auto" ":a1" ":+"] (check i))
# strings (escapes)
(each i [`"hello"` `"with space"` `"tab\there"` `"nl\nhere"` `"q\"q"` `"back\\slash"` `""`] (check i))
# integers / signs / hex / radix
(each i ["0" "42" "-7" "123456" "0xFF" "0x10" "-0xff" "2r1010" "16rFF" "36rZ" "8r17"] (check i))
# floats / exponent / ratio / N|M suffix
(each i ["3.14" "-2.5" "0.0" "1e10" "1.5e-3" "2E5" "10N" "3.14M" "1/2" "-3/4" "22/7"] (check i))
# leading + reads as the positive number (jolt-if19 fixed in the port; the Janet
# seed reader still errors on these, so assert against the correct value).
(check-correct "+5" 5)
(check-correct "+42" 42)
(check-correct "+0xff" 255)
(check-correct "+3.5" 3.5)
# characters
(each i [`\a` `\Z` `\0` `\newline` `\tab` `\space` `\return` `\\` `\(` `\{` `\%` `A` `\o101`] (check i))

(printf "\n%d/%d ok" (- total fails) total)
(when (> fails 0) (os/exit 1))
