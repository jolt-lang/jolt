;; jolt.reload gate — runs clj-reload's own test suites (parse, core, keep),
;; ported to jolt.reload, against the fixture namespaces beside this file.
;; The suites rewrite fixture files and restore them, so smoke.sh runs this
;; from a scratch copy of test/chez/reload, never in place:
;;
;;   tmp=$(mktemp -d) && cp -R test/chez/reload/. "$tmp" && (cd "$tmp" && jolt run run.clj)
;;
;; Prints RELOAD OK when every test passes.
(require '[clojure.test :as t]
         'jolt.reload.parse-test
         'jolt.reload.core-test
         'jolt.reload.keep-test)

(let [{:keys [test pass fail error]}
      (t/run-tests 'jolt.reload.parse-test 'jolt.reload.core-test 'jolt.reload.keep-test)]
  (if (and (pos? test) (zero? fail) (zero? error))
    (println "RELOAD OK" test "tests," pass "assertions")
    (do (println "RELOAD FAIL" fail "failures," error "errors")
        (System/exit 1))))
