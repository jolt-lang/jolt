# Selmer acceptance (jolt-ea7): load the real Selmer template engine from
# ~/src/selmer and render through its full pipeline — the java.time shims
# (DateTimeFormatter/Instant/ZoneId/LocalDateTime), the java.io shims
# (StringReader/StringBuilder + char-array readers), vector :import sharing
# deftype ctors, and :refer :all. SKIPS cleanly if the checkout is absent
# (CI has no ~/src/selmer); the shim surface itself is covered by
# test/spec/host-interop-spec.janet either way.

(import ../../src/jolt/api :as api)
(use ../../src/jolt/reader)

(def selmer-src (string (os/getenv "HOME") "/src/selmer/src"))
(def selmer-res (string (os/getenv "HOME") "/src/selmer/resources"))

(if (nil? (os/stat (string selmer-src "/selmer/parser.clj")))
  (print "selmer-test: ~/src/selmer not present, skipping")
  (do
    (reader-features-set! ["jolt" "clj" "default"])
    (def ctx (api/init {:paths [selmer-src selmer-res]}))

    (print "loading selmer.parser...")
    (api/eval-string ctx "(require (quote [selmer.parser :as sp]))")
    (print "  ok")

    (defn render [tpl ctx-map-src]
      (api/eval-string ctx (string "(sp/render " (describe tpl) " " ctx-map-src ")")))

    (print "variable + filter...")
    (assert (= "Hello WORLD!" (render "Hello {{name|upper}}!" "{:name \"world\"}")))
    (print "  ok")

    (print "date filter (java.time path)...")
    (def d (render "{{d|date:yyyy-MM-dd}}" "{:d #inst \"2020-03-05T10:00:00Z\"}"))
    (assert (peg/match '(* :d :d :d :d "-" :d :d "-" :d :d -1) d)
            (string "date filter renders a yyyy-MM-dd date, got: " d))
    (print "  ok")

    (print "if / for tags...")
    (assert (= "YES" (render "{% if ok %}YES{% else %}NO{% endif %}" "{:ok true}")))
    (assert (= "NO" (render "{% if ok %}YES{% else %}NO{% endif %}" "{:ok false}")))
    (assert (= "1,2,3," (render "{% for x in xs %}{{x}},{% endfor %}" "{:xs [1 2 3]}")))
    (print "  ok")

    (print "nested lookup + escaping...")
    (assert (= "7" (render "{{m.a.b}}" "{:m {:a {:b 7}}}")))
    (assert (= "&lt;b&gt;&amp;" (render "{{x}}" "{:x \"<b>&\"}")))
    (print "  ok")

    (print "file templates (render-file + cache)...")
    (def tpl-dir (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-selmer-test"))
    (os/mkdir tpl-dir)
    (spit (string tpl-dir "/t.html") "File says {{x|upper}}")
    (api/eval-string ctx (string "(selmer.util/set-custom-resource-path! " (describe (string tpl-dir "/")) ")"))
    (assert (= "File says HI"
               (api/eval-string ctx "(sp/render-file \"t.html\" {:x \"hi\"})")))
    # second render goes through the template cache (last-modified check)
    (assert (= "File says AGAIN"
               (api/eval-string ctx "(sp/render-file \"t.html\" {:x \"again\"})")))
    (print "  ok")

    (print "selmer-test: all passed")))
