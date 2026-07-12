;; regex-compare.ss — V1 vs V2 comparison harness for regex translator.
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define test-rows
  '(
    ;; ── Basic literals ──
    ("abc" "abc")
    ("abc" "xabcy")
    ("abc" "xyz")
    ("" "")
    ("" "anything")
    ("a" "a")
    ("a" "b")

    ;; ── Character classes ──
    ("[a-z]+" "hello123")
    ("[^0-9]+" "abc123")
    ("[a-zA-Z_][a-zA-Z0-9_]*" "_foo123 bar")
    ("[abc]" "a")
    ("[abc]" "d")
    ("[^abc]" "d")
    ("[^abc]" "a")
    ("[\\]]" "]")
    ("[\\]]" "[")
    ("[-a]" "-")
    ("[-a]" "a")
    ("[a-]" "-")
    ("[a-]" "a")
    ("[a-z&&[^b]]" "a")
    ("[a-z&&[^b]]" "b")
    ("[a-z&&[bc]]" "a")
    ("[a-z&&[bc]]" "b")

    ;; ── Shorthand escapes ──
    ("\\d+" "abc123def")
    ("\\d+" "abcdef")
    ("\\D+" "abc123def")
    ("\\D+" "123")
    ("\\w+" "foo_bar 123")
    ("\\w+" "!@#")
    ("\\W+" "foo bar!")
    ("\\W+" "abc")
    ("\\s+" "a b  c")
    ("\\s+" "abc")
    ("\\S+" "a b c")
    ("\\S+" "   ")

    ;; ── Word boundaries ──
    ("\\bword\\b" "a word here")
    ("\\bword\\b" "password")
    ("\\bword\\b" "sword")
    ("\\Bword\\B" "password")
    ("\\Bword\\B" "word")

    ;; ── Anchors ──
    ("^abc" "abc def")
    ("^abc" "xabc")
    ("abc$" "xabc")
    ("abc$" "abc")
    ("^abc$" "abc")
    ("^abc$" "xabcy")
    ("^$" "")
    ("^$" "a")

    ;; ── Dot ──
    ("a.b" "axb")
    ("a.b" "a\nb")
    ("a.b" "ab")

    ;; ── Quantifiers ──
    ("a*" "aaa")
    ("a*" "bbb")
    ("a+" "aaa")
    ("a+" "bbb")
    ("a?" "aaa")
    ("a?" "bbb")
    ("a{2,4}" "aaaaa")
    ("a{2,4}" "a")
    ("a{2,}" "a")
    ("a{2,}" "aa")
    ("a{3}" "aaa")
    ("a{3}" "aa")
    ("a*?" "aaa")
    ("a+?" "aaaaa")
    ("a??" "aaa")
    ("a{2,4}?" "aaaaa")

    ;; ── Groups ──
    ("(a)(b)" "ab")
    ("(?:a)(b)" "ab")
    ("(a|b)+" "abac")
    ("((a))" "a")
    ("(a*)*" "aaa")
    ("(a+)+" "aaa")

    ;; ── Alternation ──
    ("foo|bar" "foo")
    ("foo|bar" "bar")
    ("foo|bar" "baz")
    ("a|b|c" "b")
    ("|a" "")
    ("|a" "a")
    ("a|" "a")
    ("a|" "")

    ;; ── Inline/case flags ──
    ("(?i)abc" "ABC")
    ("(?i)abc" "AbC")
    ("(?i)abc" "abd")
    ("(?i:a)B" "aB")
    ("(?i:a)B" "AB")
    ("(?i:a)B" "ab")

    ;; ── Combined flags ──
    ("(?si)." "A")
    ("(?si)." "\n")
    ("(?sm)^b" "a\nb")
    ("(?sm)a$" "a\nb")

    ;; ── \Q...\E ──
    ("\\Qa.b\\E" "a.b")
    ("\\Qa.b\\E" "axb")
    ("\\Qa|b(c)\\E" "a|b(c)")

    ;; ── Unicode properties ──
    ("\\p{L}+" "abc")
    ("\\p{L}+" "ABC")
    ("\\p{L}+" "123")
    ("\\p{Nd}+" "123")
    ("\\p{Nd}+" "abc")
    ("\\p{Lu}" "A")
    ("\\p{Lu}" "a")
    ("\\p{Ll}" "a")
    ("\\p{Ll}" "A")
    ("\\P{L}" "1")
    ("\\P{L}" "a")

    ;; ── Hex/unicode escapes ──
    ("\\x41" "A")
    ("\\x41" "B")
    ("\\x{41}" "A")
    ("\\u0041" "A")

    ;; ── Literal escapes ──
    ("\\t" "\t")
    ("\\n" "\n")
    ("\\r" "\r")
    ("\\f" "\f")
    ("\\\\" "\\")
    ("\\." ".")
    ("\\*" "*")
    ("\\+" "+")
    ("\\?" "?")
    ("\\(" "(")
    ("\\)" ")")
    ("\\[" "[")
    ("\\{" "{")
    ("\\|" "|")
    ("\\^" "^")
    ("\\$" "$")

    ;; ── $ with trailing newline ──
    ("a$" "a")
    ("a$" "a\n")
    ("a$" "a\n\n")

    ;; ── MULTILINE mode ──
    ("(?m)^b" "a\nb")
    ("(?m)^b" "ab")
    ("(?m)a$" "a\nb")
    ("(?m)a$" "ab")

    ;; ── Character class ranges ──
    ("[0-9]+" "abc123def")
    ("[a-f]" "c")
    ("[a-f]" "z")
    ("[0-9a-fA-F]" "E")

    ;; ── Complex patterns from corpus ──
    ("\\d{4}-\\d{2}-\\d{2}" "2020-03-05")
    ("\\d{4}-\\d{2}-\\d{2}" "20-03-05")
    ("(?si)A.B" "axb")
    ("(?si)A.B" "a\nb")
    ("(?sx)foo bar" "foobar")
    ("(?sx)foo bar" "foo bar")
    ))

(define (try-v1 pat)
  (guard (e (#t (cons 'v1-error e)))
    (let ((re (jolt-regex-v1 pat)))
      (cons 'v1-ok re))))

(define (try-v2 pat)
  (guard (e (#t (cons 'v2-error e)))
    (let ((re (jolt-regex-v2 pat)))
      (cons 'v2-ok re))))

(define (test-row pat input)
  (let ((v1r (try-v1 pat))
        (v2r (try-v2 pat)))
    (cond
     ((and (eq? (car v1r) 'v1-error) (eq? (car v2r) 'v2-error))
      (printf ".") 'ok)
     ((eq? (car v1r) 'v1-error)
      (printf "\n  V1-ERR V2-OK: ~a\n" pat)
      (cons 'v1-only pat))
     ((eq? (car v2r) 'v2-error)
      (printf "\n  V1-OK V2-ERR: ~a  ~a\n" pat (cdr v2r))
      (cons 'v2-error pat))
     (else
      (let ((r1 (jolt-re-find (cdr v1r) input))
            (r2 (jolt-re-find (cdr v2r) input)))
        ;; Use Clojure = for structural equality (handles vectors, nil, strings)
        (if (jolt=2 r1 r2)
            (begin (printf ".") 'ok)
            (begin
              (printf "\n  MISMATCH ~a on ~a\n" pat input)
              (printf "    V1: ~a\n" r1)
              (printf "    V2: ~a\n" r2)
              (cons 'mismatch pat))))))))

(printf "regex-compare: ~a rows\n" (length test-rows))
(flush-output-port (current-output-port))
(let loop ((rows test-rows) (ok 0) (fail 0) (fails '()))
  (if (null? rows)
      (begin
        (printf "\n~a passed, ~a failed\n" ok fail)
        (for-each (lambda (f) (printf "  FAIL: ~a\n" f)) (reverse fails)))
      (let* ((row (car rows))
             (pat (car row))
             (input (cadr row))
             (result (test-row pat input)))
        (case result
          ((ok) (loop (cdr rows) (+ ok 1) fail fails))
          (else (loop (cdr rows) ok (+ fail 1) (cons (list result pat input) fails)))))))
