# Drift check for the seed ↔ overlay boundary (refactor phase 5d).
#
# Recomputes the dispatch-twin set from source and asserts it matches what
# docs/seed-overlay-registry.md documents. A twin is a name with BOTH a seed
# `core-X` defn (src/jolt/*.janet) and an overlay `(defn X …)`
# (jolt-core/clojure/core/*.clj). If you add, remove, or re-home a twin, update
# the doc table and the EXPECTED-TWINS set below together.
#
# This is source analysis, not Clojure eval — a plain Janet test (no harness).

(def repo-root
  # test/unit/<this> -> repo root is two levels up
  (let [d (os/cwd)] d))

(defn- slurp-safe [path]
  (try (slurp path) ([_] nil)))

(defn- files-with-ext [dir ext]
  (def acc @[])
  (each name (os/dir dir)
    (def full (string dir "/" name))
    (case (os/stat full :mode)
      :directory (array/concat acc (files-with-ext full ext))
      :file (when (string/has-suffix? ext name) (array/push acc full))))
  acc)

# Collect bare names from `(defn NAME` / `(defn- NAME`, optionally stripping a
# `prefix` (e.g. "core-") and keeping only those that had the prefix.
(defn- defn-names [src prefix]
  (def names @{})
  (def pat (peg/compile
             ~{:sym (capture (some (+ (range "az" "AZ" "09") (set "*?!<>=+/.&_-"))))
               :defn (* "(defn" (? "-") (some (set " \t")) :sym)
               :main (some (+ :defn 1))}))
  (each m (or (peg/match pat src) @[])
    (when (string? m)
      (if prefix
        (when (string/has-prefix? prefix m)
          (put names (string/slice m (length prefix)) true))
        (put names m true))))
  names)

(defn- merge-into [dst src] (eachk k src (put dst k true)) dst)

# --- seed core-X names across src/jolt/*.janet ---
(def seed-names @{})
(each f (files-with-ext (string repo-root "/src/jolt") ".janet")
  (merge-into seed-names (defn-names (slurp f) "core-")))

# --- overlay public defns across jolt-core/clojure/core/*.clj ---
(def overlay-names @{})
(each f (files-with-ext (string repo-root "/jolt-core/clojure/core") ".clj")
  (merge-into overlay-names (defn-names (slurp f) nil)))

# --- twins = intersection ---
(def twins @{})
(eachk k seed-names (when (get overlay-names k) (put twins k true)))

(def EXPECTED-TWINS
  {"char?" true "sorted?" true "sorted-map?" true "sorted-set?" true
   "transduce" true})

(defn- keyset [t] (sort (keys t)))

(unless (deep= (keyset twins) (keyset EXPECTED-TWINS))
  (error (string "seed↔overlay twin drift!\n"
                 "  computed: " (string/join (keyset twins) ", ") "\n"
                 "  expected: " (string/join (keyset EXPECTED-TWINS) ", ") "\n"
                 "  Update docs/seed-overlay-registry.md and EXPECTED-TWINS together.")))

# --- core-bindings registered keys (the public/dispatch registration table) ---
(def core-src (slurp (string repo-root "/src/jolt/core.janet")))
(def bind-start (string/find "@{" core-src (string/find "core-bindings" core-src)))
# brace-match from the `@{` to its close
(defn- match-brace [src open]
  (var depth 0) (var i open) (var end nil)
  (while (and (< i (length src)) (nil? end))
    (case (src i)
      (chr "{") (++ depth)
      (chr "}") (do (-- depth) (when (= depth 0) (set end i))))
    (++ i))
  end)
(def bind-end (match-brace core-src (+ bind-start 1)))
(def bind-region (string/slice core-src bind-start (+ bind-end 1)))
(def registered @{})
(each m (or (peg/match ~(some (+ (* "\"" (capture (some (if-not "\"" 1))) "\"") 1)) bind-region) @[])
  (put registered m true))

# Twins must NOT be registered — the overlay copy shadows; the seed copy is
# internal-only. A twin sneaking into core-bindings means the seed copy is
# masquerading as the public binding.
(eachk t EXPECTED-TWINS
  (when (get registered t)
    (error (string "twin '" t "' is registered in core-bindings — it should be "
                   "overlay-public only (see docs/seed-overlay-registry.md)"))))

# The seed-public anchors must stay registered (guards the into/transduce
# asymmetry the registry documents).
(each anchor ["into" "reduce"]
  (unless (get registered anchor)
    (error (string "seed-public anchor '" anchor "' missing from core-bindings"))))

(print "seed↔overlay registry: " (length (keys twins)) " twins, "
       (length (keys registered)) " registered bindings — OK")
