# Specification: a non-array ISeq (plist/lazy-seq, e.g. from cons/concat/list or
# ~@) used as a FORM is evaluated as a call, not returned as self-evaluating data
# (jolt-2rx). The interpreter only treated a reader LIST (Janet array) as a call;
# a runtime-built list (a plist/lazy-seq table) fell through to self-eval, so
# (eval (cons '+ '(1 2))) returned the list instead of 3. The analyzer already
# punts such forms to the interpreter, so the fix lives in eval-form.
(use ../support/harness)

(defspec "ISeq call forms (jolt-2rx)"
  ["eval a cons'd call"      "3"   "(eval (cons (quote +) (quote (1 2))))"]
  ["eval a list-built call"  "6"   "(eval (list (quote +) 1 2 3))"]
  ["eval a concat'd call"    "10"  "(eval (concat (list (quote +)) (list 1 2 3 4)))"]
  ["nested cons'd subform"   "7"   "(eval (list (quote +) 3 (cons (quote +) (quote (1 3)))))"]
  ["empty list self-evals"   "()"  "(eval (list))"]
  ["macro output via cons"   "3"   "(do (defmacro mc [] (cons (quote +) (quote (1 2)))) (mc))"]
  ["macro output via concat" "6"   "(do (defmacro mk [] (concat (list (quote +)) (list 1 2 3))) (mk))"]
  # regressions: vectors/quoted-data are NOT calls
  ["vector value self-evals" "[1 2 3]" "(eval (vec [1 2 3]))"]
  ["quoted list of data"     "(quote (1 2 3))" "(quote (1 2 3))"])
