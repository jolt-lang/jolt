# A malformed catch clause must be rejected with a clean, Clojure-like error in
# BOTH the interpreter and the compiler — not an internal Janet crash ("expected
# integer key for array …") and not silently swallowed. jolt's catch is
# (catch Class binding body*); the binding (3rd element) must be a symbol.
# Regression for jolt-kg6p (surfaced building the Chez try/throw emit).
(import ../../src/jolt/api :as api)

(var total 0) (var fails 0)
(defn ok [name pred &opt extra]
  (++ total)
  (if pred (printf "ok: %s" name)
    (do (++ fails) (printf "FAIL: %s   %s" name (or extra "")))))

(defn err-msg [ctx s]
  (let [r (protect (api/eval-string ctx s))]
    (if (r 0) :no-error (string (r 1)))))

(defn val-of [ctx s] (api/eval-string ctx s))

# malformed: the binding position holds a non-symbol (a call form / a literal),
# or the clause is too short. Each must raise a clean error mentioning catch,
# and must NOT leak the internal Janet indexing crash.
(def malformed
  ["(try 1 (catch e (* e 10)))"
   "(try 1 (catch e 5))"
   "(try 1 (catch Exception))"])

# well-formed catch still works (class is a symbol or :default; binding a symbol).
(def wellformed
  [["(try (throw 7) (catch Exception e (* e 10)))" 70]
   ["(try (throw 42) (catch :default e e))" 42]
   ["(try (+ 2 3) (catch :default e 0))" 5]])

(each [mode opts] [["interpret" {}] ["compile" {:compile? true}]]
  (def ctx (api/init opts))
  (each s malformed
    (def m (err-msg ctx s))
    (ok (string mode " rejects: " s)
        (and (not= m :no-error)
             (string/find "catch" m)
             (not (string/find "expected integer key" m)))
        (string "msg=" m)))
  (each [s want] wellformed
    (ok (string mode " ok: " s) (= want (val-of ctx s))
        (string "got " (val-of ctx s)))))

(printf "\ntry-catch-validation: %d/%d passed" (- total fails) total)
(when (> fails 0) (error (string fails " failing")))
