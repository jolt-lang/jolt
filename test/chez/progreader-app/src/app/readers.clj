(ns app.readers)
(defn reverse-str [s] (apply str (reverse s)))
;; PROGRAMMATIC registration: NOT in any data_readers.clj file, so
;; dce-data-reader-roots (file scan) never sees it. It reaches the baked
;; *data-readers* map only through this alter-var-root at load time.
(alter-var-root (var clojure.core/*data-readers*) assoc 'prog/rev 'app.readers/reverse-str)
