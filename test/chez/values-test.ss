;; Tests for the Jolt value model on Chez (nil/truthiness, interned keywords,
;; symbols, exactness-aware =, hashing). Run from repo root:
;;   chez --script test/chez/values-test.ss
(import (chezscheme))
(load "host/chez/values.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

;; nil distinct from #f and '()
(ok "nil not #f"        (not (eq? jolt-nil #f)))
(ok "nil not '()"       (not (eq? jolt-nil '())))
(ok "nil? jolt-nil"     (jolt-nil? jolt-nil))
(ok "nil? not on #f"    (not (jolt-nil? #f)))

;; truthiness: only nil and false falsey
(ok "nil falsey"        (not (jolt-truthy? jolt-nil)))
(ok "false falsey"      (not (jolt-truthy? #f)))
(ok "true truthy"       (jolt-truthy? #t))
(ok "0 truthy"          (jolt-truthy? 0))
(ok "empty-str truthy"  (jolt-truthy? ""))
(ok "empty-list truthy" (jolt-truthy? '()))

;; keywords interned -> identity
(ok "kw eq"             (eq? (keyword #f "foo") (keyword #f "foo")))
(ok "kw ns eq"          (eq? (keyword "a" "foo") (keyword "a" "foo")))
(ok "kw diff ns"        (not (eq? (keyword "a" "foo") (keyword #f "foo"))))
(ok "kw?"               (keyword? (keyword #f "x")))
(ok "kw not sym"        (not (jolt-symbol? (keyword #f "x"))))

;; symbols NOT interned but jolt= by ns/name
(ok "sym not eq"        (not (eq? (jolt-symbol #f "x") (jolt-symbol #f "x"))))
(ok "sym jolt="         (jolt= (jolt-symbol #f "x") (jolt-symbol #f "x")))
(ok "sym diff name"     (not (jolt= (jolt-symbol #f "x") (jolt-symbol #f "y"))))
(ok "sym?"              (jolt-symbol? (jolt-symbol "ns" "n")))

;; numbers: exactness-aware = (Clojure semantics)
(ok "1 = 1"             (jolt= 1 1))
(ok "1 not= 1.0"        (not (jolt= 1 1.0)))
(ok "1.0 = 1.0"         (jolt= 1.0 1.0))
(ok "ratio ="           (jolt= 1/2 1/2))
(ok "bigint=int exact"  (jolt= 2 (expt 2 1)))
(ok "= variadic"        (jolt= 3 3 3))
(ok "= variadic false"  (not (jolt= 3 3 4)))

;; strings / chars
(ok "str ="             (jolt= "ab" "ab"))
(ok "str !="            (not (jolt= "ab" "ac")))
(ok "char ="            (jolt= #\a #\a))

;; hashing consistent with =
(ok "hash kw stable"    (= (jolt-hash (keyword #f "k")) (jolt-hash (keyword #f "k"))))
(ok "hash sym stable"   (= (jolt-hash (jolt-symbol #f "k")) (jolt-hash (jolt-symbol #f "k"))))
(ok "hash 1 != 1.0"     (not (= (jolt-hash 1) (jolt-hash 1.0))))
(ok "hash str stable"   (= (jolt-hash "abc") (jolt-hash "abc")))

;; regression: keyword intern key must not collide across ns/name boundary
(ok "kw no boundary collide" (not (eq? (keyword "a" "b/c") (keyword "a/b" "c"))))
;; regression: jolt-hash must not throw on non-finite floats
(ok "hash +inf ok" (number? (jolt-hash +inf.0)))
(ok "hash +nan ok"  (number? (jolt-hash +nan.0)))
(ok "hash inf != exact" (not (= (jolt-hash +inf.0) (jolt-hash 0))))

(printf "values-test: ~a/~a passed\n" (- total fails) total)
(exit (if (> fails 0) 1 0))
