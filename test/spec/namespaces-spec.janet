# Specification: namespaces, vars and require.
(use ../support/harness)

(defspec "namespaces / def & vars"
  ["def + deref"        "5"        "(do (def x 5) x)"]
  ["def returns var"    "true"     "(var? (def y 1))"]
  ["declare then def"   "2"        "(do (declare z) (def z 2) z)"]
  ["var special form"   "true"     "(var? (var +))"]
  ["var sugar #'"       "true"     "(var? #'+)"]
  ["var-get"            "5"        "(do (def w 5) (var-get #'w))"]
  ["defn defines fn"    "3"        "(do (defn f [x] (inc x)) (f 2))"]
  ["def with docstring" "7"        "(do (def d \"a doc\" 7) d)"]
  ["dynamic var binding" "2"       "(do (def ^:dynamic *x* 1) (binding [*x* 2] *x*))"]
  ["binding restores"    "1"       "(do (def ^:dynamic *y* 1) (binding [*y* 9] nil) *y*)"]
  ["var-set in binding"  "5"       "(do (def ^:dynamic *z* 1) (binding [*z* 0] (var-set (var *z*) 5) *z*))"])

(defspec "namespaces / ns operations"
  ["in-ns switches"     "true"     "(do (in-ns 'my.ns) (symbol? 'x))"]
  # ns is a macro over in-ns/require/use/import (Stage 2 jolt-eaa): the form sets
  # the current ns and processes its clauses.
  ["ns form + alias"    "\"HI\""   "(do (ns my.app (:require [clojure.string :as s])) (s/upper-case \"hi\"))"]
  ["ns :use refers all" "9"        "(do (ns src.lib) (def helper 9) (ns dst.app (:use [src.lib])) helper)"]
  ["standalone use"     "7"        "(do (ns src.l2) (def k 7) (in-ns 'dst.a2) (use '[src.l2]) k)"]
  ["ns-name"            "true"     "(do (require (quote [clojure.string])) (= 'clojure.string (ns-name (find-ns 'clojure.string))))"]
  ["find-ns existing"   "true"     "(some? (find-ns 'clojure.core))"]
  ["find-ns missing"    "nil"      "(find-ns 'does.not.exist)"]
  ["resolve var"        "true"     "(var? (resolve '+))"]
  ["resolve missing"    "nil"      "(resolve 'totally-undefined-xyz)"])

(defspec "namespaces / require & refer"
  ["require :as"        "\"AB\""   "(do (require '[clojure.string :as s]) (s/upper-case \"ab\"))"]
  ["require :refer"     "true"     "(do (require '[clojure.string :refer [blank?]]) (blank? \"\"))"]
  ["require :as + :refer" "true"   "(do (require '[clojure.string :as s :refer [blank?]]) (and (blank? \"\") (= \"X\" (s/upper-case \"x\"))))"]
  ["require clojure.set" "#{1 2 3}" "(do (require '[clojure.set :as set]) (set/union #{1 2} #{3}))"]
  ["require clojure.walk" "{:a 2}" "(do (require '[clojure.walk :as w]) (w/postwalk (fn [x] (if (number? x) (inc x) x)) {:a 1}))"]
  ["walk keywordize-keys" "{:a 1}" "(do (require '[clojure.walk :as w]) (w/keywordize-keys {\"a\" 1}))"]
  ["walk stringify-keys" "true"    "(do (require '[clojure.walk :as w]) (= {\"a\" 1} (w/stringify-keys {:a 1})))"])

(defspec "namespaces / alias, ns-unalias, ns-publics"
  ["alias + use"        "\"1,2\"" "(do (require (quote clojure.string)) (alias (quote st) (quote clojure.string)) (st/join \",\" [1 2]))"]
  ["ns-unalias removes" "true"
   "(do (require (quote clojure.string)) (alias (quote st2) (quote clojure.string)) (ns-unalias (quote user) (quote st2)) (nil? (get (ns-aliases (quote user)) (quote st2))))"]
  ["ns-publics has var" "true"  "(do (def npv 1) (some? (get (ns-publics (quote user)) (quote npv))))"]
  ["newline returns nil" "nil"  "(newline)"])
