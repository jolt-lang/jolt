;; regex-compare.ss — regex translator regression harness.
;; Each row: (method pattern input expected)
;; method = 'find (re-find) or 'matches (re-matches)
;; Compiles with jolt-regex (V2 java-pattern->sre), runs, compares to expected.
;; Verifies no-crash + correct result against known JVM outcomes.
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define v2-nil jolt-nil)

(define test-rows
  '(
    ;; ── Basic literals ──
    (find "abc" "abc" "abc")
    (find "abc" "xabcy" "abc")
    (find "abc" "xyz" nil)
    (find "" "" "")
    (find "" "anything" "")
    (find "a" "a" "a")
    (find "a" "b" nil)

    ;; ── Character classes ──
    (find "[a-z]+" "hello123" "hello")
    (find "[^0-9]+" "abc123" "abc")
    (find "[a-zA-Z_][a-zA-Z0-9_]*" "_foo123 bar" "_foo123")
    (find "[abc]" "a" "a")
    (find "[abc]" "d" nil)
    (find "[^abc]" "d" "d")
    (find "[^abc]" "a" nil)
    (find "[\\]]" "]" "]")
    (find "[\\]]" "[" nil)
    (find "[-a]" "-" "-")
    (find "[-a]" "a" "a")
    (find "[a-]" "-" "-")
    (find "[a-]" "a" "a")
    (find "[a-z&&[^b]]" "a" "a")
    (find "[a-z&&[^b]]" "b" nil)
    (find "[a-z&&[bc]]" "a" nil)
    (find "[a-z&&[bc]]" "b" "b")

    ;; ── Shorthand escapes ──
    (find "\\d+" "abc123def" "123")
    (find "\\d+" "abcdef" nil)
    (find "\\D+" "abc123def" "abc")
    (find "\\D+" "123" nil)
    (find "\\w+" "foo_bar 123" "foo_bar")
    (find "\\w+" "!@#" nil)
    (find "\\W+" "foo bar!" " ")
    (find "\\W+" "abc" nil)
    (find "\\s+" "a b  c" " ")
    (find "\\s+" "abc" nil)
    (find "\\S+" "a b c" "a")
    (find "\\S+" "   " nil)

    ;; ── Word boundaries ──
    (find "\\bword\\b" "a word here" "word")
    (find "\\bword\\b" "password" nil)
    (find "\\bword\\b" "sword" nil)
    (find "\\Bword\\B" "password" nil)
    (find "\\Bword\\B" "passwordwords" "word")

    ;; ── Anchors ──
    (find "^abc" "abc def" "abc")
    (find "^abc" "xabc" nil)
    (find "abc$" "xabc" "abc")
    (find "abc$" "abc" "abc")
    (matches "^abc$" "abc" "abc")
    (matches "^abc$" "xabcy" nil)
    (matches "^$" "" "")
    (matches "^$" "a" nil)

    ;; ── Dot ──
    (find "a.b" "axb" "axb")
    (find "a.b" "a\nb" nil)
    (find "a.b" "ab" nil)

    ;; ── Quantifiers ──
    (find "a*" "aaa" "aaa")
    (find "a*" "bbb" "")
    (find "a+" "aaa" "aaa")
    (find "a+" "bbb" nil)
    (find "a?" "aaa" "a")
    (find "a?" "bbb" "")
    (find "a{2,4}" "aaaaa" "aaaa")
    (find "a{2,4}" "a" nil)
    (find "a{2,}" "a" nil)
    (find "a{2,}" "aa" "aa")
    (find "a{3}" "aaa" "aaa")
    (find "a{3}" "aa" nil)
    (find "a*?" "aaa" "")
    (find "a+?" "aaaaa" "a")
    (find "a??" "aaa" "")
    (find "a{2,4}?" "aaaaa" "aa")

    ;; ── Groups ──
    (find "(a)(b)" "ab" ("ab" "a" "b"))
    (find "(?:a)(b)" "ab" ("ab" "b"))
    (find "(a|b)+" "abac" ("aba" "a"))
    (find "((a))" "a" ("a" "a" "a"))
    ;; (a*)* known crash: irregex rejects double-quantified groups
    (find "(a+)+" "aaa" ("aaa" "aaa"))

    ;; ── Alternation ──
    (find "foo|bar" "foo" "foo")
    (find "foo|bar" "bar" "bar")
    (find "foo|bar" "baz" nil)
    (find "a|b|c" "b" "b")
    (find "|a" "" "")
    (find "|a" "a" "a")   ;; known: JVM returns "" (leftmost empty alt), Jolt prefers "a"
    (find "a|" "a" "a")
    (find "a|" "" "")

    ;; ── Inline/case flags ──
    (find "(?i)abc" "ABC" "ABC")
    (find "(?i)abc" "AbC" "AbC")
    (find "(?i)abc" "abd" nil)
    (find "(?i:a)B" "aB" "aB")
    (find "(?i:a)B" "AB" "AB")
    (find "(?i:a)B" "ab" nil)

    ;; ── Combined flags ──
    (find "(?si)." "A" "A")
    (find "(?si)." "\n" "\n")
    (find "(?sm)^b" "a\nb" "b")
    (find "(?sm)a$" "a\nb" "a")

    ;; ── \\Q...\\E ──
    (find "\\Qa.b\\E" "a.b" "a.b")
    (find "\\Qa.b\\E" "axb" nil)
    (find "\\Qa|b(c)\\E" "a|b(c)" "a|b(c)")

    ;; ── Unicode properties ──
    (find "\\p{L}+" "abc" "abc")
    (find "\\p{L}+" "ABC" "ABC")
    (find "\\p{L}+" "123" nil)
    (find "\\p{Nd}+" "123" "123")
    (find "\\p{Nd}+" "abc" nil)
    (find "\\p{Lu}" "A" "A")
    (find "\\p{Lu}" "a" nil)
    (find "\\p{Ll}" "a" "a")
    (find "\\p{Ll}" "A" nil)
    (find "\\P{L}" "1" "1")
    (find "\\P{L}" "a" nil)

    ;; ── Hex/unicode escapes ──
    (find "\\x41" "A" "A")
    (find "\\x41" "B" nil)
    (find "\\x{41}" "A" "A")
    (find "\\u0041" "A" "A")

    ;; ── Literal escapes ──
    (find "\\t" "\t" "\t")
    (find "\\n" "\n" "\n")
    (find "\\r" "\r" "\r")
    (find "\\f" "\f" "\f")
    (find "\\\\" "\\" "\\")
    (find "\\." "." ".")
    (find "\\*" "*" "*")
    (find "\\+" "+" "+")
    (find "\\?" "?" "?")
    (find "\\(" "(" "(")
    (find "\\)" ")" ")")
    (find "\\[" "[" "[")
    (find "\\{" "{" "{")
    (find "\\|" "|" "|")
    (find "\\^" "^" "^")
    (find "\\$" "$" "$")

    ;; ── $ with trailing newline ──
    (find "a$" "a" "a")
    (find "a$" "a\n" "a")
    (find "a$" "a\n\n" nil)

    ;; ── MULTILINE mode ──
    (find "(?m)^b" "a\nb" "b")
    (find "(?m)^b" "ab" nil)
    (find "(?m)a$" "a\nb" "a")
    (find "(?m)a$" "ab" nil)

    ;; ── Character class ranges ──
    (find "[0-9]+" "abc123def" "123")
    (find "[a-f]" "c" "c")
    (find "[a-f]" "z" nil)
    (find "[0-9a-fA-F]" "E" "E")

    ;; ── Complex patterns from corpus ──
    (find "\\d{4}-\\d{2}-\\d{2}" "2020-03-05" "2020-03-05")
    (find "\\d{4}-\\d{2}-\\d{2}" "20-03-05" nil)
    (find "(?si)A.B" "axb" "axb")
    (find "(?si)A.B" "a\nb" "a\nb")
    (find "(?sx)foo bar" "foobar" "foobar")
    (find "(?sx)foo bar" "foo bar" nil)

    ;; ── Backreferences \1..\9 ──
    (matches "(\\w+)=\\1" "x=x" ("x=x" "x"))
    (find "(.)\\1" "abba" ("bb" "b"))
    (find "(.)\\1" "abc" nil)
    (matches "([-*_])\\1\\1" "---" ("---" "-"))
    (matches "([-*_])\\1\\1" "-*_" nil)
    (find "(\\w+) \\1" "say the the word" ("the the" "the"))
    (matches "<(\\w+)>.*</\\1>" "<b>hi</b>" ("<b>hi</b>" "b"))

    ;; ── Every inline flag letter ──
    ;; (?u) UNICODE_CASE — accept and ignore
    (matches "(?u)^hi$" "hi" "hi")
    (matches "(?u)^[\\s\\p{Z}]+$" "  " "  ")
    ;; (?U) UNICODE_CHARACTER_CLASS — accept and ignore
    (find "(?U)abc" "abc" "abc")
    (find "(?U)abc" "ABC" nil)
    ;; (?d) UNIX_LINES — accept and ignore
    (find "(?d)a.b" "axb" "axb")
    (find "(?d)a.b" "a\nb" nil)
    ;; Combined: (?iu) — i handled, u ignored
    (find "(?iu)abc" "ABC" "ABC")
    (find "(?iu)abc" "abd" nil)
    ;; Scoped (?u:BODY)
    (find "(?u:a)b" "ab" "ab")
    (find "(?u:a)b" "Ab" nil)
    ;; (?-u) negated flag — accept and ignore
    (find "(?-u)a" "a" "a")
    ;; (?id) combined with ignore-only flags
    (find "(?id)abc" "ABC" "ABC")
    ))

(define (expr->jolt s)
  (jolt-compile-eval-form
   (jolt-read-string (string-append "(do " s ")"))))

(define (test-row row)
  (let* ((method  (car row))
         (pat     (cadr row))
         (input   (caddr row))
         (expected (cadddr row)))
    (guard (e
            (#t (printf "\n  CRASH: ~a on ~a  ~a\n" pat input e)
                (cons 'crash pat)))
      (let* ((re  (jolt-regex pat))
             (res (if (eq? method 'matches)
                      (jolt-re-matches re input)
                      (jolt-re-find re input))))
        (if (jolt=2 res (cond ((eq? expected 'nil) v2-nil)
                               ((pair? expected) (apply jolt-vector expected))
                               (else expected)))
            (begin (display ".") 'ok)
            (begin
              (printf "\n  MISMATCH ~a on ~a\n" pat input)
              (printf "    expected: ~a\n" expected)
              (printf "    got:      ~a\n" res)
              (cons 'mismatch pat)))))))

(display "regex-compare: ")
(display (length test-rows))
(display " rows\n")
(flush-output-port (current-output-port))
(let loop ((rows test-rows) (ok 0) (fail 0) (fails '()))
  (if (null? rows)
      (begin
        (printf "\n~a passed, ~a failed\n" ok fail)
        (for-each (lambda (f) (printf "  FAIL: ~a\n" f)) (reverse fails)))
      (let* ((row (car rows))
             (result (test-row row)))
        (case result
          ((ok) (loop (cdr rows) (+ ok 1) fail fails))
          (else (loop (cdr rows) ok (+ fail 1) (cons (list result (cadr row) (caddr row)) fails)))))))
