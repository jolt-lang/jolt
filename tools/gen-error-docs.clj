#!/usr/bin/env jolt
;; gen-error-docs.clj — render the error reference page from the kind registry.
;;
;;   jolt tools/gen-error-docs.clj > ../jolt-lang.github.io/resources/md/errors.md
;;
;; test/conformance/error-kinds.edn is the single source of truth for what errors
;; jolt can raise: `make errorkinds` already fails when a kind is raised without
;; being registered, or registered without being raised. Generating the page from
;; it extends that guarantee to the documentation, instead of creating a second
;; list that drifts from the first.
(ns gen-error-docs
  (:require [clojure.string :as str]))

(def registry-path "test/conformance/error-kinds.edn")

(defn- phase-of [k] (namespace k))

(def ^:private phase-titles
  {"read"    "Reader"
   "analyze" "Analysis"
   "runtime" "Runtime"})

(def ^:private phase-blurbs
  {"read"    "Raised while reading source text into forms, before anything is compiled."
   "analyze" "Raised while analyzing a form — the compile-time errors."
   "runtime" "Raised while a program runs."})

(defn- anchor [k]
  (str/replace (str (namespace k) "-" (name k)) #"[^a-z0-9-]" ""))

(defn -main [& _]
  (let [kinds (read-string (slurp registry-path))
        by-phase (group-by (comp phase-of key) kinds)]
    (println "Every error Jolt raises carries a **kind** — a namespaced keyword")
    (println "identifying what went wrong, independent of the wording. The kind is")
    (println "what to search for, and what tooling should key on: the message beside")
    (println "it is free to improve, the kind is not.")
    (println)
    (println "```")
    (println "error[analyze/invalid-def]: First argument to def must be a Symbol")
    (println "  --> ./src/app.clj:3:1")
    (println "   |")
    (println " 3 | (def :foo 2)")
    (println "   |      ^^^^ the name must be a symbol")
    (println "```")
    (println)
    (println "Set `JOLT_DIAG=edn` to get the same diagnostic as a single line of EDN")
    (println "— kind, position, the offending token and the surrounding source —")
    (println "for editors and other tooling.")
    (println)
    (println "This page is generated from")
    (println "[`test/conformance/error-kinds.edn`](https://github.com/jolt-lang/jolt/blob/main/test/conformance/error-kinds.edn),")
    (println "which the build gates against the compiler's actual raise sites.")
    (println)
    (doseq [phase ["read" "analyze" "runtime"]
            :let [entries (sort-by key (get by-phase phase))]
            :when (seq entries)]
      (println (str "## " (get phase-titles phase phase)))
      (println)
      (when-let [b (get phase-blurbs phase)] (println b) (println))
      (doseq [[k doc] entries]
        (println (str "### `" (namespace k) "/" (name k) "`"))
        (println)
        (println (str/replace (str/trim doc) #"\n\s+" "\n"))
        (println)))))
