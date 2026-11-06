;; The docstring's two unescaped quotes split it, and the file still reads.

(ns split-doc
  "Declare one like this:

      (defcfn c-open "open" [:string] :int)

  and call it.")
