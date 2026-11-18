;; A #'var inside a def's metadata under JOLT_LAZY_META (rt.ss
;; def-var-lazy-meta!, backend literals.clj meta-desc). The var is the one the
;; def named when it ran, however the namespaces change before the metadata is
;; first read: such metadata is never deferred, so nothing resolves a name late,
;; and reading it can neither pick up another var nor recreate a namespace
;; that was removed. Run with `jolt run`, knob set or not; prints
;; LAZY-META-VAR OK <mode> when every check holds.
(def failures (atom []))
(defn chk [label expected actual]
  (when-not (= expected actual)
    (swap! failures conj [label :expected expected :got actual])))
(def lazy? (let [e (System/getenv "JOLT_LAZY_META")] (boolean (and e (not= e "") (not= e "0")))))
(def meta-lazy? (resolve 'jolt.host/var-meta-lazy?))

;; the namespace a var in the metadata lives in is removed and made again
(create-ns 'lmv.other)
(intern 'lmv.other 'target :first)
(def captured #'lmv.other/target)
(def ^{:ref #'lmv.other/target :n 1} holder 1)
(chk "metadata naming a var is built when the def runs" false (boolean (meta-lazy? (find-var 'user/holder))))
(remove-ns 'lmv.other)
(create-ns 'lmv.other)
(intern 'lmv.other 'target :second)
(chk "the var in the metadata is the one the def named" true
     (identical? captured (:ref (meta #'holder))))
(chk "...whose value is the first one's" :first @(:ref (meta #'holder)))

;; the namespace is removed and NOT made again: reading the metadata must not
;; intern a var into it, or bring it back
(create-ns 'lmv.gone)
(intern 'lmv.gone 'v :gone)
(def gone-var #'lmv.gone/v)
(def ^{:ref #'lmv.gone/v} holder2 2)
(remove-ns 'lmv.gone)
(let [r (:ref (meta #'holder2))]
  (chk "a removed namespace's var stays the removed var" true (identical? gone-var r))
  (chk "...and reading it does not bring the namespace back" nil (find-ns 'lmv.gone)))

;; the name is unmapped and defined again in the def's own namespace
(def target3 :t3)
(def t3-var #'target3)
(def ^{:ref #'target3} holder3 3)
(ns-unmap *ns* 'target3)
(def target3 :t3-again)
(chk "an unmapped name's var" true (identical? t3-var (:ref (meta #'holder3))))

;; nested inside a collection of the metadata, too
(create-ns 'lmv.nested)
(intern 'lmv.nested 'n :nested)
(def nested-var #'lmv.nested/n)
(def ^{:refs [#'lmv.nested/n] :k :v} holder4 4)
(remove-ns 'lmv.nested)
(create-ns 'lmv.nested)
(intern 'lmv.nested 'n :other)
(chk "a var nested in the metadata" true (identical? nested-var (first (:refs (meta #'holder4)))))

;; constant metadata with no var in it still goes lazy under the knob (read
;; through find-var: analyzing #'plain reads the metadata)
(def ^{:doc "plain" :k [1 2]} plain 5)
(chk "constant metadata with no var is deferred under the knob" lazy? (boolean (meta-lazy? (find-var 'user/plain))))
(chk "...and reads back" "plain" (:doc (meta #'plain)))

(if (empty? @failures)
  (println "LAZY-META-VAR OK" (if lazy? "lazy" "default"))
  (do (doseq [f @failures] (println "FAIL" f))
      (System/exit 1)))
