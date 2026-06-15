# Jolt value layer — characters + symbol helpers
# Extracted from types.janet (jolt-bvek phase 5a split).

# Jolt Types
# Core types for the Clojure-on-Janet interpreter.
#
# Types:
#   JoltVar        — mutable container with metadata (like Clojure Var)
#   JoltNamespace  — namespace with symbol→var mappings and imports
#   JoltContext     — evaluation context (env atom, namespaces)
#
# Symbols are represented as {:jolt/type :symbol :ns <string-or-nil> :name <string>}
# as produced by the reader.

# Characters are {:jolt/type :jolt/char :ch <codepoint>}, distinct from strings.
(defn make-char [code] {:jolt/type :jolt/char :ch code})

(def- char-named @{"newline" 10 "space" 32 "tab" 9 "return" 13
                   "formfeed" 12 "backspace" 8 "newpage" 12 "nul" 0})

(defn char-from-name
  "Resolve a reader char-literal name (\\a, \\newline, \\uNNNN, \\oNNN) to a char value."
  [name]
  (cond
    (= 1 (length name)) (make-char (in name 0))
    (get char-named name) (make-char (get char-named name))
    (and (> (length name) 1) (= (in name 0) (get "u" 0)))
      (make-char (scan-number (string "16r" (string/slice name 1))))
    (and (> (length name) 1) (= (in name 0) (get "o" 0)))
      (make-char (scan-number (string "8r" (string/slice name 1))))
    (error (string "Unsupported character: \\" name))))

# ============================================================
# Symbol helpers
# ============================================================

(defn sym?
  "Check if x is a Jolt symbol struct."
  [x]
  (and (struct? x) (= :symbol (x :jolt/type))))

