;; kernel-test.ss — the G2 kernel-test gate (jolt-mj95.4).
;;
;; Run via `make gambitkernel` (detection-gated: skips when gambit-scheme is
;; absent, and always uses $(brew --prefix gambit-scheme)/bin/gsi — NEVER bare
;; gsc/gsi, which is Ghostscript on this machine). Run from the repo root (the
;; irregex load inside the boot is cwd-relative).
;;
;; ONE compilation unit with the boot: (##include "boot.ss") textually splices
;; the whole 39-file manifest here (prelude-shims + scheme-adapter-runtime +
;; hasheq + the curated chez kernel files), exactly the G2 build flow — the
;; shim macros expand in the same top level, and NO file-level import appears
;; in this file (a module boundary would hide them; see gambitcheck.ss's splice
;; note).
;;
;; The rows drive the REAL native fns (jolt-vector, jolt-conj, jolt-assoc,
;; jolt-get, jolt-dissoc, jolt-count, jolt-nth, jolt-first, jolt-next, jolt-seq,
;; jolt-take, jolt-map, jolt-partition, jolt-pr-readable, jolt-read-string,
;; jolt-hasheq, jolt-with-meta, jolt-meta, jolt=, def-var!, var-deref,
;; def-dynvar!) — all defined in the booted chez sources (names verified
;; against host/chez before writing).
;;
;; Every expected STRING / hasheq value below was captured from the Chez build
;; via bin/jolt — the paste-command is in a comment beside each row. Maps and
;; sets with 2+ entries render in HAMT-iteration order (not stable insertion
;; order), so those are compared via jolt= / accessors, never printed form
;; (single-entry ones print deterministically and ARE rendered).

(##include "boot.ss")

;; ---- test harness (mirrors gambitcheck.ss) -----------------------------------

(define failures 0)

(define (check label actual expected)
  (if (equal? actual expected)
      (begin (printf "  ok     ~a\n" label) #t)
      (begin (printf "  FAIL   ~a: got ~s expected ~s\n" label actual expected)
             (set! failures (+ failures 1))
             #f)))

(define (check-true label v)
  (if v
      (begin (printf "  ok     ~a\n" label) #t)
      (begin (printf "  FAIL   ~a: got #f\n" label)
             (set! failures (+ failures 1))
             #f)))

;; ---- collection construction + count -----------------------------------------

(define (test-construction)
  (printf "== construction: jolt-vector / jolt-list / jolt-hash-map / jolt-hash-set / jolt-count ==\n")
  ;; Chez: bin/jolt -e "(pr-str [1 2 3])" -> "[1 2 3]"
  (check "vector 1 2 3" (jolt-pr-readable (jolt-vector 1 2 3)) "[1 2 3]")
  ;; Chez: bin/jolt -e "(pr-str (list 1 2 3))" -> "(1 2 3)"
  (check "list 1 2 3" (jolt-pr-readable (jolt-list 1 2 3)) "(1 2 3)")
  ;; Chez: bin/jolt -e "(pr-str {:a 1})" -> "{:a 1}"
  (check "map {:a 1}" (jolt-pr-readable (jolt-hash-map (keyword #f "a") 1)) "{:a 1}")
  ;; Chez: bin/jolt -e "(pr-str #{7})" -> "#{7}"
  (check "set #{7}" (jolt-pr-readable (jolt-hash-set 7)) "#{7}")
  (check "count vector" (jolt-count (jolt-vector 1 2 3)) 3)
  (check "count list" (jolt-count (jolt-list 1 2 3 4)) 4)
  (check "count map" (jolt-count (jolt-hash-map (keyword #f "a") 1 (keyword #f "b") 2)) 2)
  (check "count set" (jolt-count (jolt-hash-set 1 2 3)) 3)
  (check "count string" (jolt-count "hello") 5)
  (check "count empty vector" (jolt-count (jolt-vector)) 0)
  (check "count empty map" (jolt-count (jolt-hash-map)) 0)
  (check "count empty set" (jolt-count (jolt-hash-set)) 0)
  (check "count empty list" (jolt-count (jolt-list)) 0)
  ;; Chez: bin/jolt -e "(pr-str [])" -> "[]"
  (check "empty vector render" (jolt-pr-readable (jolt-vector)) "[]")
  ;; Chez: bin/jolt -e "(pr-str {})" -> "{}"
  (check "empty map render" (jolt-pr-readable (jolt-hash-map)) "{}")
  ;; Chez: bin/jolt -e "(pr-str #{})" -> "#{}"
  (check "empty set render" (jolt-pr-readable (jolt-hash-set)) "#{}")
  ;; Chez: bin/jolt -e "(pr-str (list))" -> "()"
  (check "empty list render" (jolt-pr-readable (jolt-list)) "()"))

;; ---- conj / assoc / dissoc ---------------------------------------------------

(define (test-conj-assoc-dissoc)
  (printf "== jolt-conj / jolt-assoc / jolt-dissoc ==\n")
  ;; Chez: bin/jolt -e "(pr-str (conj [1 2] 3))" -> "[1 2 3]"
  (check "conj on vector" (jolt-pr-readable (jolt-conj (jolt-vector 1 2) 3)) "[1 2 3]")
  ;; Chez: bin/jolt -e "(pr-str (conj (list 1 2) 3))" -> "(3 1 2)"
  (check "conj on list prepends" (jolt-pr-readable (jolt-conj (jolt-list 1 2) 3)) "(3 1 2)")
  ;; Chez: bin/jolt -e "(pr-str (conj nil 1 2))" -> "(2 1)"
  (check "conj on nil builds a list" (jolt-pr-readable (jolt-conj jolt-nil 1 2)) "(2 1)")
  (check "conj [k v] pair into map -> get" (jolt-get (jolt-conj (jolt-hash-map (keyword #f "a") 1) (jolt-vector (keyword #f "b") 2)) (keyword #f "b")) 2)
  (check "conj [k v] pair into map -> count" (jolt-count (jolt-conj (jolt-hash-map (keyword #f "a") 1) (jolt-vector (keyword #f "b") 2))) 2)
  (check "assoc new key -> get" (jolt-get (jolt-assoc (jolt-hash-map (keyword #f "a") 1) (keyword #f "b") 2) (keyword #f "b")) 2)
  (check "assoc new key -> count" (jolt-count (jolt-assoc (jolt-hash-map (keyword #f "a") 1) (keyword #f "b") 2)) 2)
  ;; Chez: bin/jolt -e "(pr-str (assoc [0 0 0] 1 9))" -> "[0 9 0]"
  (check "assoc on vector by index" (jolt-pr-readable (jolt-assoc (jolt-vector 0 0 0) 1 9)) "[0 9 0]")
  (check "assoc on nil -> map" (jolt-get (jolt-assoc jolt-nil (keyword #f "a") 1) (keyword #f "a")) 1)
  (check "dissoc removes key" (jolt-nil? (jolt-get (jolt-dissoc (jolt-hash-map (keyword #f "a") 1 (keyword #f "b") 2) (keyword #f "a")) (keyword #f "a"))) #t)
  (check "dissoc shrinks count" (jolt-count (jolt-dissoc (jolt-hash-map (keyword #f "a") 1 (keyword #f "b") 2) (keyword #f "a"))) 1)
  (check "dissoc absent key is a no-op" (jolt-count (jolt-dissoc (jolt-hash-map (keyword #f "a") 1) (keyword #f "zzz"))) 1))

;; ---- get / nth ---------------------------------------------------------------

(define (test-get-nth)
  (printf "== jolt-get / jolt-nth ==\n")
  (check "get vector by index" (jolt-get (jolt-vector 10 20) 1) 20)
  (check "get vector out of range -> nil" (jolt-nil? (jolt-get (jolt-vector 10 20) 9)) #t)
  (check "get map present key" (jolt-get (jolt-hash-map (keyword #f "a") 1) (keyword #f "a")) 1)
  (check "get map missing key -> default" (jolt-get (jolt-hash-map (keyword #f "a") 1) (keyword #f "zzz") 99) 99)
  (check "get set membership returns key" (jolt-get (jolt-hash-set 7) 7) 7)
  (check "get set absent -> nil" (jolt-nil? (jolt-get (jolt-hash-set 7) 8)) #t)
  (check "nth vector" (jolt-nth (jolt-vector 10 20 30) 1) 20)
  (check "nth list" (jolt-nth (jolt-list 5 6 7) 2) 7)
  (check "nth string" (jolt-nth "abc" 1) #\b)
  ;; subs's in-range arm is a block copy through sa-string-copy-range!
  ;; (converters.ss); a Chez-ordered string-copy! here returned a blank span and
  ;; wrote it back over the source.
  (check "subs cuts the span and leaves the source alone"
    (let ((src (string-copy "hello world")))
      (list (jolt-subs src 6 11) (jolt-subs src 6) (jolt-subs src 0 0) src))
    (list "world" "world" "" "hello world"))
  (check "nth out of range -> not-found" (jolt-nth (jolt-vector 10 20) 5 99) 99)
  (check-true "nth out of range raises jolt-throw"
    (guard (e (#t (jolt-throw-condition? e)))
      (jolt-nth (jolt-vector 10 20) 5)
      #f))
  (check "nth oob exception class"
    (jolt-ex-info-record-class-name
      (jolt-throw-condition-value
        (guard (e (#t e)) (jolt-nth (jolt-vector 10 20) 5) #f)))
    "java.lang.IndexOutOfBoundsException"))

;; ---- seq walking -------------------------------------------------------------

(define (test-seq-walking)
  (printf "== jolt-first / jolt-next / jolt-seq ==\n")
  (check "first vector" (jolt-first (jolt-vector 10 20 30)) 10)
  ;; Chez: bin/jolt -e "(pr-str (next [10 20 30]))" -> "(20 30)"
  (check "next vector" (jolt-pr-readable (jolt-next (jolt-vector 10 20 30))) "(20 30)")
  (check "first of next" (jolt-first (jolt-next (jolt-vector 10 20 30))) 20)
  (check-true "next of single-element list is nil" (jolt-nil? (jolt-next (jolt-list 1))))
  ;; Chez: bin/jolt -e "(pr-str (seq [10 20]))" -> "(10 20)"
  (check "seq vector" (jolt-pr-readable (jolt-seq (jolt-vector 10 20))) "(10 20)")
  ;; Chez: bin/jolt -e "(pr-str (seq (list 1 2 3)))" -> "(1 2 3)"
  (check "seq list" (jolt-pr-readable (jolt-seq (jolt-list 1 2 3))) "(1 2 3)")
  ;; Chez: bin/jolt -e "(pr-str (seq \"abc\"))" -> "(\\a \\b \\c)"
  (check "seq string" (jolt-pr-readable (jolt-seq "abc")) "(\\a \\b \\c)")
  (check "count seq string" (jolt-count (jolt-seq "abcde")) 5)
  (check "first of seq list" (jolt-first (jolt-seq (jolt-list 1 2 3))) 1)
  (check "count seq of 2-entry map" (jolt-count (jolt-seq (jolt-hash-map (keyword #f "a") 1 (keyword #f "b") 2))) 2)
  (check-true "seq of map yields entry vectors" (pvec? (jolt-first (jolt-seq (jolt-hash-map (keyword #f "a") 1)))))
  ;; Chez: bin/jolt -e "(pr-str (seq {:a 1}))" -> "([:a 1])"
  (check "seq single-entry map" (jolt-pr-readable (jolt-seq (jolt-hash-map (keyword #f "a") 1))) "([:a 1])")
  (check-true "seq of empty vector is nil" (jolt-nil? (jolt-seq (jolt-vector)))))

;; ---- lazy seqs (real natives-seq producers, forced through the printer) ------

(define (test-lazy)
  (printf "== lazy seqs: jolt-take / jolt-map / jolt-partition forced through jolt-pr-readable ==\n")
  ;; Chez: bin/jolt -e "(pr-str (take 3 [1 2 3 4 5]))" -> "(1 2 3)"
  (check "take 3 vector" (jolt-pr-readable (jolt-take 3 (jolt-vector 1 2 3 4 5))) "(1 2 3)")
  (check "count take 3" (jolt-count (jolt-take 3 (jolt-vector 1 2 3 4 5))) 3)
  (check "first of take" (jolt-first (jolt-take 3 (jolt-vector 9 8 7 6))) 9)
  ;; Chez: bin/jolt -e "(pr-str (take 1 (list 7 8 9)))" -> "(7)"
  (check "take 1 list" (jolt-pr-readable (jolt-take 1 (jolt-list 7 8 9))) "(7)")
  ;; Chez: bin/jolt -e "(pr-str (map inc [1 2 3]))" -> "(2 3 4)"
  (check "map inc vector" (jolt-pr-readable (jolt-map jolt-inc (jolt-vector 1 2 3))) "(2 3 4)")
  ;; Chez: bin/jolt -e "(pr-str (partition 2 [1 2 3 4 5 6]))" -> "((1 2) (3 4) (5 6))"
  (check "partition 2 vector" (jolt-pr-readable (jolt-partition 2 (jolt-vector 1 2 3 4 5 6))) "((1 2) (3 4) (5 6))")
  (check "count partition 2" (jolt-count (jolt-partition 2 (jolt-vector 1 2 3 4 5 6))) 3))

;; ---- rendering (jolt-pr-readable) --------------------------------------------

(define (test-rendering)
  (printf "== rendering: jolt-pr-readable (captures from bin/jolt on Chez) ==\n")
  ;; Chez: bin/jolt -e "(pr-str [[1 2] (list 3 4)])" -> "[[1 2] (3 4)]"
  (check "nested vector+list" (jolt-pr-readable (jolt-vector (jolt-vector 1 2) (jolt-list 3 4))) "[[1 2] (3 4)]")
  ;; Chez: bin/jolt -e "(pr-str [1 {:a [2 3]} (list 4 5)])" -> "[1 {:a [2 3]} (4 5)]"
  (check "deep nesting" (jolt-pr-readable (jolt-vector 1 (jolt-hash-map (keyword #f "a") (jolt-vector 2 3)) (jolt-list 4 5))) "[1 {:a [2 3]} (4 5)]")
  ;; Chez: bin/jolt -e "(pr-str [1 nil true false])" -> "[1 nil true false]"
  (check "nil and booleans" (jolt-pr-readable (jolt-vector 1 jolt-nil #t #f)) "[1 nil true false]")
  ;; Chez: bin/jolt -e "(pr-str [\"a\\\"b\" \"c\\\\d\" \"tab\\there\" \"nl\\nx\"])" -> "[\"a\\\"b\" \"c\\\\d\" \"tab\\there\" \"nl\\nx\"]"
  (check "strings with escapes" (jolt-pr-readable (jolt-vector "a\"b" "c\\d" "tab\there" "nl\nx")) "[\"a\\\"b\" \"c\\\\d\" \"tab\\there\" \"nl\\nx\"]")
  ;; Chez: bin/jolt -e "(pr-str [\\a \\b \\newline \\space \\tab])" -> "[\\a \\b \\newline \\space \\tab]"
  (check "chars incl named" (jolt-pr-readable (jolt-vector #\a #\b #\newline #\space #\tab)) "[\\a \\b \\newline \\space \\tab]")
  ;; Chez: bin/jolt -e "(pr-str [##Inf ##-Inf ##NaN])" -> "[##Inf ##-Inf ##NaN]"
  (check "infinities and NaN" (jolt-pr-readable (jolt-vector +inf.0 -inf.0 +nan.0)) "[##Inf ##-Inf ##NaN]")
  ;; Chez: bin/jolt -e "(pr-str [9223372036854775808 -9223372036854775809])" -> "[9223372036854775808N -9223372036854775809N]"
  (check "bigints with N suffix" (jolt-pr-readable (jolt-vector 9223372036854775808 -9223372036854775809)) "[9223372036854775808N -9223372036854775809N]")
  ;; Chez: bin/jolt -e "(pr-str [2/3 1/100])" -> "[2/3 1/100]"
  (check "ratios" (jolt-pr-readable (jolt-vector 2/3 1/100)) "[2/3 1/100]")
  ;; Chez: bin/jolt -e "(pr-str [:a/b :c/d])" -> "[:a/b :c/d]"
  (check "namespaced keywords" (jolt-pr-readable (jolt-vector (keyword "a" "b") (keyword "c" "d"))) "[:a/b :c/d]")
  ;; Chez: bin/jolt -e "(pr-str (list (quote foo/bar) (quote sym)))" -> "(foo/bar sym)"
  (check "symbols incl namespaced" (jolt-pr-readable (jolt-list (jolt-symbol "foo" "bar") (jolt-symbol #f "sym"))) "(foo/bar sym)"))

;; ---- reader round-trips -------------------------------------------------------

(define (test-reader-roundtrip)
  (printf "== jolt-read-string round-trips (jolt= to constructed + re-render) ==\n")
  ;; Chez: bin/jolt -e "(pr-str (read-string \"[1 :kw {\\\"s\\\" 2/3} #{sym}]\"))" -> "[1 :kw {\"s\" 2/3} #{sym}]"
  (check "corpus re-render" (jolt-pr-readable (jolt-read-string "[1 :kw {\"s\" 2/3} #{sym}]")) "[1 :kw {\"s\" 2/3} #{sym}]")
  (check-true "corpus jolt= to constructed"
    (jolt= (jolt-read-string "[1 :kw {\"s\" 2/3} #{sym}]")
           (jolt-vector 1 (keyword #f "kw") (jolt-hash-map "s" 2/3) (jolt-hash-set (jolt-symbol #f "sym")))))
  ;; Chez: bin/jolt -e "(pr-str (read-string \"[\\\"a\\\\\\\"b\\\" \\\\newline]\"))" -> "[\"a\\\"b\" \\newline]"
  (check "escapes round-trip re-render" (jolt-pr-readable (jolt-read-string "[\"a\\\"b\" \\newline]")) "[\"a\\\"b\" \\newline]")
  (check-true "escapes round-trip jolt="
    (jolt= (jolt-read-string "[\"a\\\"b\" \\newline]") (jolt-vector "a\"b" #\newline)))
  ;; Chez: bin/jolt -e "(pr-str (read-string \"[##Inf ##NaN]\"))" -> "[##Inf ##NaN]"
  (check "inf/NaN round-trip re-render" (jolt-pr-readable (jolt-read-string "[##Inf ##NaN]")) "[##Inf ##NaN]")
  (check-true "inf round-trip jolt=" (jolt= (jolt-read-string "[##Inf]") (jolt-vector +inf.0)))
  ;; Chez: bin/jolt -e "(pr-str (read-string \"\\\\a\"))" -> "\\a"
  (check "char round-trip" (jolt-pr-readable (jolt-read-string "\\a")) "\\a")
  ;; Chez: bin/jolt -e "(pr-str (read-string \"{:a 1}\"))" -> "{:a 1}"
  (check-true "map round-trip jolt=" (jolt= (jolt-read-string "{:a 1}") (jolt-hash-map (keyword #f "a") 1)))
  (check-true "set round-trip jolt=" (jolt= (jolt-read-string "#{1 2 3}") (jolt-hash-set 1 2 3)))
  (check-true "list round-trip jolt=" (jolt= (jolt-read-string "(1 2 3)") (jolt-list 1 2 3)))
  (check-true "integer round-trip jolt=" (jolt= (jolt-read-string "42") 42))
  (check-true "keyword round-trip jolt=" (jolt= (jolt-read-string ":kw") (keyword #f "kw")))
  (check-true "symbol round-trip jolt=" (jolt= (jolt-read-string "foo/bar") (jolt-symbol "foo" "bar"))))

;; ---- hasheq (real collections vs Chez-captured (hash ...)) -------------------

(define (test-hasheq)
  (printf "== hasheq: jolt-hasheq on real collections, pinned to Chez (hash ...) ==\n")
  ;; Chez: bin/jolt -e "(hash [1 2 3])" -> 736442005
  (check "hash [1 2 3]" (jolt-hasheq (jolt-vector 1 2 3)) 736442005)
  ;; Chez: bin/jolt -e "(hash (list 1 2 3))" -> 736442005
  (check "hash (list 1 2 3) == hash [1 2 3]" (jolt-hasheq (jolt-list 1 2 3)) 736442005)
  ;; Chez: bin/jolt -e "(hash [])" -> -2017569654
  (check "hash []" (jolt-hasheq (jolt-vector)) -2017569654)
  ;; Chez: bin/jolt -e "(hash {})" -> -15128758
  (check "hash {}" (jolt-hasheq (jolt-hash-map)) -15128758)
  ;; Chez: bin/jolt -e "(hash #{})" -> -15128758
  (check "hash #{} == hash {}" (jolt-hasheq (jolt-hash-set)) -15128758)
  ;; Chez: bin/jolt -e "(hash {:a 1})" -> 1772842048
  (check "hash {:a 1}" (jolt-hasheq (jolt-hash-map (keyword #f "a") 1)) 1772842048)
  ;; Chez: bin/jolt -e "(hash {:a 1 :b 2})" -> 161871944
  (check "hash {:a 1 :b 2}" (jolt-hasheq (jolt-hash-map (keyword #f "a") 1 (keyword #f "b") 2)) 161871944)
  ;; Chez: bin/jolt -e "(hash #{1 2})" -> 460223544
  (check "hash #{1 2}" (jolt-hasheq (jolt-hash-set 1 2)) 460223544)
  ;; Chez: bin/jolt -e "(hash [1 :a \"s\" 2/3])" -> 262523492
  (check "hash mixed vector" (jolt-hasheq (jolt-vector 1 (keyword #f "a") "s" 2/3)) 262523492)
  ;; Chez: bin/jolt -e "(hash 9223372036854775808)" -> -2147483648
  (check "hash bigint" (jolt-hasheq 9223372036854775808) -2147483648)
  ;; Chez: bin/jolt -e "(hash 2/3)" -> 1
  (check "hash ratio" (jolt-hasheq 2/3) 1)
  ;; Chez: bin/jolt -e "(hash (with-meta [1 2] {:k 1}))" -> 156247261 == (hash [1 2])
  (check "hash ignores meta" (jolt-hasheq (jolt-with-meta (jolt-vector 1 2) (jolt-hash-map (keyword #f "k") 1))) 156247261))

;; ---- meta round-trip ---------------------------------------------------------

(define (test-meta)
  (printf "== jolt-with-meta / jolt-meta ==\n")
  (check-true "meta round-trip"
    (jolt= (jolt-meta (jolt-with-meta (jolt-vector 1 2) (jolt-hash-map (keyword #f "k") 1)))
           (jolt-hash-map (keyword #f "k") 1)))
  ;; Chez: bin/jolt -e "(pr-str (with-meta [1 2] {:k 1}))" -> "[1 2]"
  (check "meta not printed" (jolt-pr-readable (jolt-with-meta (jolt-vector 1 2) (jolt-hash-map (keyword #f "k") 1))) "[1 2]")
  (check-true "meta of a number is nil" (jolt-nil? (jolt-meta 42)))
  (check-true "meta of a bare symbol is nil" (jolt-nil? (jolt-meta (jolt-symbol #f "s")))))

;; ---- def-var! / var-deref / def-dynvar! (rt-core) ----------------------------

(define (test-vars)
  (printf "== def-var! / var-deref / def-dynvar! through rt-core ==\n")
  (def-var! "kernel.test" "x" 42)
  (check "def-var! then var-deref" (var-deref "kernel.test" "x") 42)
  (def-var! "kernel.test" "x" 43)
  (check "re-def-var! mutates root" (var-deref "kernel.test" "x") 43)
  (check-true "never-defined var is unbound sentinel" (jolt-var-unbound? (var-deref "kernel.test" "never-defined")))
  (def-dynvar! "kernel.test" "*dx*" 7)
  (check "def-dynvar! then var-deref" (var-deref "kernel.test" "*dx*") 7)
  (check-true "dynvar carries :dynamic meta"
    (jolt= (jolt-get (jolt-meta (jolt-var "kernel.test" "*dx*")) (keyword #f "dynamic")) #t))
  (check-true "def-var! returns the var cell" (var-cell? (def-var! "kernel.test" "y" 1))))

;; ---- jolt= structural equality, mixed nesting --------------------------------

(define (test-equality)
  (printf "== jolt= structural equality ==\n")
  (check-true "vectors equal" (jolt= (jolt-vector 1 2) (jolt-vector 1 2)))
  (check-true "deep nesting equal" (jolt= (jolt-vector 1 (jolt-vector 2 (jolt-vector 3))) (jolt-vector 1 (jolt-vector 2 (jolt-vector 3)))))
  (check-true "map of vector equal" (jolt= (jolt-hash-map (keyword #f "a") (jolt-vector 1 2)) (jolt-hash-map (keyword #f "a") (jolt-vector 1 2))))
  (check-true "set order-insensitive" (jolt= (jolt-hash-set 1 2 3) (jolt-hash-set 3 2 1)))
  ;; Chez: bin/jolt -e "(= (list 1 2) [1 2])" -> true — jolt= is SEQUENTIAL
  ;; equality: a seq and a vector with the same elements are equal (a known
  ;; jolt/CHEZ-parity quirk vs Clojure's false). Pin the parity, not Clojure.
  (check-true "seq == vector (sequential equality, matches Chez)" (jolt= (jolt-list 1 2) (jolt-vector 1 2)))
  (check-true "map key mismatch" (not (jolt= (jolt-hash-map (keyword #f "a") 1) (jolt-hash-map (keyword #f "b") 1))))
  (check-true "vector element mismatch" (not (jolt= (jolt-vector 1 2) (jolt-vector 1 3)))))

;; ---- main --------------------------------------------------------------------

(test-construction)
(test-conj-assoc-dissoc)
(test-get-nth)
(test-seq-walking)
(test-lazy)
(test-rendering)
(test-reader-roundtrip)
(test-hasheq)
(test-meta)
(test-vars)
(test-equality)

;; Forcing a lazy cell once a thread exists takes seq.ss's claim path
;; (cell-force-claimed! for a cell, force-claimed! for a node), which the single-threaded rows above never reach: every
;; shim that path needs on this host has to be present, and two threads on the
;; same unforced cell must still run its thunk once.
(define (test-threaded-force)
  (printf "== forcing lazy cells with threads ==\n")
  (let* ((runs 0)
         (cell (cseq-lazy 0 (lambda () (set! runs (+ runs 1)) (cseq-realized 1 jolt-nil))))
         (node (jolt-make-lazy-seq (lambda () (set! runs (+ runs 1)) (cseq-realized 2 jolt-nil))))
         (t1 (fork-thread (lambda () (seq-more cell) (force-lazyseq node))))
         (t2 (fork-thread (lambda () (seq-more cell) (force-lazyseq node)))))
    (thread-join! t1) (thread-join! t2)
    (check "a forked thread flips the multi-threaded flag" jolt-mt? #t)
    (check "two threads forcing one cell and one node ran each thunk once" runs 2)
    (check "the cell's tail is published" (seq-first (seq-more cell)) 1)
    (check "the node's value is published" (seq-first (force-lazyseq node)) 2)
    (check "no claim is left behind" (list (tail-claim? (cseq-tail cell)) (jolt-lazyseq-lock node)) '(#f #f))))
(test-threaded-force)

(printf "\nkernel-test: ~a failure(s)\n" failures)
(if (= failures 0)
    (begin (printf "kernel-test: PASS — booted kernel + natives verified on native gsi\n")
           (exit 0))
    (begin (printf "kernel-test: FAILED\n")
           (exit 1)))
