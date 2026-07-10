(ns app.readers)
;; returns a FORM (a list) so the loader splices it as code at build time
(defn expand-form [form]
  (list 'println (str "expanded:" (first form))))
