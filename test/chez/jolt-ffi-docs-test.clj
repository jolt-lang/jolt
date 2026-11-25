;; Every public jolt.ffi var carries a docstring, so `doc` and an editor's hover
;; answer for it. The host defines its primitives without metadata, and
;; stdlib/jolt/ffi.clj attaches theirs, so a new host var shows up here first.
;; Run: bin/jolt run test/chez/jolt-ffi-docs-test.clj, or `make ffi`.
(ns jolt-ffi-docs-test)

(require '[clojure.string :as str]
         '[jolt.ffi])

(def undocumented
  (->> (ns-publics 'jolt.ffi)
       (remove (fn [[sym _]] (str/starts-with? (name sym) "__")))
       (remove (fn [[_ v]] (seq (:doc (meta v)))))
       (map first)
       sort))

(if (empty? undocumented)
  (do (println "JOLT-FFI-DOCS-TEST OK" (count (ns-publics 'jolt.ffi)) "vars") (flush) (System/exit 0))
  (do (doseq [s undocumented] (println (str "FAIL: no :doc on jolt.ffi/" s)))
      (println "JOLT-FFI-DOCS-TEST FAILED:" (count undocumented))
      (flush)
      (System/exit 1)))
