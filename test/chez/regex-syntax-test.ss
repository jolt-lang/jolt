;; regex-syntax-test.ss — java.util.regex pattern-syntax errors must read exactly
;; like the JVM's.
;;
;; A malformed pattern is a java.util.regex.PatternSyntaxException, and its
;; getMessage() is "<description> near index N" followed by the pattern and —
;; when N lands inside the pattern — a caret under column N.  The translator
;; (host/chez/java/regex-translate.ss) raises a &java-pattern-error carrying the
;; description and index; host/chez/regex.ss renders it.  The vectors below are
;; the JVM's own getMessage() captured verbatim (babashka 1.13.x, JDK 20); #f
;; means the pattern compiles and matches nothing here.
;;   chez --script test/chez/regex-syntax-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)

;; The rendered message for a bad pattern, or #f when it compiles.  A non-syntax
;; failure (should not happen) is surfaced as its own string so the diff is loud.
(define (pattern-message s)
  (guard (e (#t (if (java-pattern-error? e)
                    (pattern-syntax-message (java-pattern-error-desc e)
                                            (java-pattern-error-index e)
                                            s)
                    (string-append "NON-SYNTAX-ERROR: " (condition-message-of e)))))
    (java-pattern->sre s)
    #f))

(define vectors
  '(
    ("(a" "Unclosed group near index 2\n(a")
    ("(" "Unclosed group near index 1\n(")
    ("(?:x" "Unclosed group near index 4\n(?:x")
    ("(?=x" "Unclosed group near index 4\n(?=x")
    ("(?!x" "Unclosed group near index 4\n(?!x")
    ("(?<=x" "Unclosed group near index 5\n(?<=x")
    ("(?<!x" "Unclosed group near index 5\n(?<!x")
    ("(?<a>b" "Unclosed group near index 6\n(?<a>b")
    ("(?<a" "named capturing group is missing trailing '>' near index 4\n(?<a")
    ("(?<a>" "Unclosed group near index 5\n(?<a>")
    ("(?i:x" "Unclosed group near index 5\n(?i:x")
    ("(?#x" "Unknown inline modifier near index 2\n(?#x\n  ^")
    ("(?i" "Unknown inline modifier near index 3\n(?i")
    ("(?im" "Unknown inline modifier near index 4\n(?im")
    ("(?)" #f)
    ("(?ii" "Unknown inline modifier near index 4\n(?ii")
    ("*" "Dangling meta character '*' near index 0\n*\n^")
    ("+" "Dangling meta character '+' near index 0\n+\n^")
    ("?" "Dangling meta character '?' near index 0\n?\n^")
    ("a**" "Dangling meta character '*' near index 2\na**\n  ^")
    ("*a" "Dangling meta character '*' near index 0\n*a\n^")
    ("(?:*" "Dangling meta character '*' near index 3\n(?:*\n   ^")
    ("(|*" "Dangling meta character '*' near index 2\n(|*\n  ^")
    ("(?i)*" "Dangling meta character '*' near index 4\n(?i)*\n    ^")
    ("(?i)+" "Dangling meta character '+' near index 4\n(?i)+\n    ^")
    ("(?i)?" "Dangling meta character '?' near index 4\n(?i)?\n    ^")
    ("(?i){" "Illegal repetition near index 5\n(?i){")
    ("(?i){2}" #f)
    ("a{2,1}" "Illegal repetition range near index 5\na{2,1}\n     ^")
    ("{2,1}" "Illegal repetition range near index 4\n{2,1}\n    ^")
    ("a{,1}" "Illegal repetition near index 2\na{,1}\n  ^")
    ("a{}" "Illegal repetition near index 2\na{}\n  ^")
    ("a{x}" "Illegal repetition near index 2\na{x}\n  ^")
    ("a{1x}" "Unclosed counted closure near index 3\na{1x}\n   ^")
    ("a{1" "Unclosed counted closure near index 3\na{1")
    ("a{1,2,3}" "Unclosed counted closure near index 5\na{1,2,3}\n     ^")
    ("{" "Illegal repetition near index 1\n{")
    ("a{" "Illegal repetition near index 2\na{")
    ("a{2}*" "Dangling meta character '*' near index 4\na{2}*\n    ^")
    ("[b" "Unclosed character class near index 1\n[b\n ^")
    ("[abc" "Unclosed character class near index 3\n[abc\n   ^")
    ("[" "Unclosed character class near index 0\n[\n^")
    ("[^b" "Unclosed character class near index 2\n[^b\n  ^")
    ("[a-" "Illegal character range near index 3\n[a-")
    ("[b-a]" "Illegal character range near index 3\n[b-a]\n   ^")
    ("[a&&" "Unclosed character class near index 3\n[a&&\n   ^")
    ("[a&&b]" #f)
    ("[\\b]" "Illegal/unsupported escape sequence near index 2\n[\\b]\n  ^")
    ("[\\R]" "Illegal/unsupported escape sequence near index 2\n[\\R]\n  ^")
    ("[\\Q\\E]" "Unclosed character class near index 1\n[\\Q\\E]\n ^")
    ("[\\Q" "Unclosed character class near index 0\n[\\Q\n^")
    ("[a-\\d]" "Illegal character range near index 4\n[a-\\d]\n    ^")
    ("[]]" #f)
    ("\\c" "Illegal control escape sequence near index 1\n\\c\n ^")
    ("\\c1" #f)
    ("\\p" "Unknown character property name {\x00;} near index 2\n\\p")
    ("\\p{" "Unclosed character family near index 3\n\\p{")
    ("\\p{bogus}" "Unknown character property name {bogus} near index 8\n\\p{bogus}\n        ^")
    ("\\p{L" "Unclosed character family near index 4\n\\p{L")
    ("\\x" "Illegal hexadecimal escape sequence near index 2\n\\x")
    ("\\x{" "Illegal hexadecimal escape sequence near index 2\n\\x{\n  ^")
    ("\\x{}" "Illegal hexadecimal escape sequence near index 2\n\\x{}\n  ^")
    ("\\x{110000}" "Hexadecimal codepoint is too big near index 8\n\\x{110000}\n        ^")
    ("\\xG" "Illegal hexadecimal escape sequence near index 2\n\\xG\n  ^")
    ("\\N" "Illegal character name escape sequence near index 2\n\\N")
    ("\\N{" "Unclosed character name escape sequence near index 3\n\\N{")
    ("\\N{LATIN SMALL LETTER A}" "\\N{name} is unsupported on jolt (no Unicode character name table) near index 1\n\\N{LATIN SMALL LETTER A}\n ^")
    ("\\k" "\\k is not followed by '<' for named capturing group near index 2\n\\k")
    ("\\k<" "capturing group name does not start with a Latin letter near index 3\n\\k<")
    ("\\k<a>" "named capturing group <a> does not exist near index 4\n\\k<a>\n    ^")
    ("\\ka" "\\k is not followed by '<' for named capturing group near index 2\n\\ka\n  ^")
    ("(?<1>b)" "capturing group name does not start with a Latin letter near index 3\n(?<1>b)\n   ^")
    ("(?<a_b>b)" "named capturing group is missing trailing '>' near index 4\n(?<a_b>b)\n    ^")
    ("(?<a-1>b)" "named capturing group is missing trailing '>' near index 4\n(?<a-1>b)\n    ^")
    ("(?<>b)" "capturing group name does not start with a Latin letter near index 3\n(?<>b)\n   ^")
    ("(?<y>b)(?<y>c)" "Named capturing group <y> is already defined near index 11\n(?<y>b)(?<y>c)\n           ^")
    ("a\\" "Unescaped trailing backslash near index 2\na\\")
    ("abc\\" "Unescaped trailing backslash near index 4\nabc\\")
    ("[a]\\" "Unescaped trailing backslash near index 4\n[a]\\")
    ("(?<a>b" "Unclosed group near index 6\n(?<a>b")
    ("(?-i)" #f)
    ("(?i-i)" #f)
    ("\\Q" #f)
    ;; An index after a \Q…\E span counts the JVM's REWRITTEN buffer
    ;; (Pattern.RemoveQEQuoting): a quoted non-alphanumeric becomes \c, a
    ;; quoted digit at the quote's start becomes \x3N, a quoted backslash \\.
    ("\\Qa.b\\E(" "Unclosed group near index 5\n\\Qa.b\\E(\n     ^")
    ("\\Q.\\E*" #f)
    ("\\Qab\\E*" #f)
    ("\\Q\\E*" "Dangling meta character '*' near index 0\n\\Q\\E*\n^")
    ("\\Q(\\E[" "Unclosed character class near index 2\n\\Q(\\E[\n  ^")
    ("x\\Q.\\Ey{" "Illegal repetition near index 5\nx\\Q.\\Ey{\n     ^")
    ("\\Q1\\E(" "Unclosed group near index 5\n\\Q1\\E(\n     ^")
    ("\\Q12\\E(" "Unclosed group near index 6\n\\Q12\\E(\n      ^")
    ("\\Qé.\\E(" "Unclosed group near index 4\n\\Qé.\\E(\n    ^")
    ("\\Q\\\\\\E(" "Unclosed group near index 5\n\\Q\\\\\\E(\n     ^")
    ("\\Qa\\E\\Qb\\E(" "Unclosed group near index 3\n\\Qa\\E\\Qb\\E(\n   ^")
    ("a\\Qb" #f)
    ("a\\Qb(" #f)
    ("\\E" "Illegal/unsupported escape sequence near index 1\n\\E\n ^")
    ("(\\Qx\\E" "Unclosed group near index 2\n(\\Qx\\E\n  ^")
    ("\\Q.\\E{" "Illegal repetition near index 3\n\\Q.\\E{\n   ^")
    ("a\\E" "Illegal/unsupported escape sequence near index 2\na\\E\n  ^")
    ;; A top-level ) stops the JVM's parse where it stands, before anything after
    ;; it is read, and the index it names is the unit BEFORE the paren (cursor-1):
    ;; -1 for a leading one, so no " near index" clause and no caret.
    (")" "Unmatched closing ')'\n)")
    ("a)" "Unmatched closing ')' near index 0\na)\n^")
    ("(a))" "Unmatched closing ')' near index 2\n(a))\n  ^")
    ("a)(" "Unmatched closing ')' near index 0\na)(\n^")
    (")(" "Unmatched closing ')'\n)(")
    ("ab)cd" "Unmatched closing ')' near index 1\nab)cd\n ^")
    ("(?i))" "Unmatched closing ')' near index 3\n(?i))\n   ^")
    ("a|)" "Unmatched closing ')' near index 1\na|)\n ^")
    ("\\Q)\\E)" "Unmatched closing ')' near index 1\n\\Q)\\E)\n ^")
    ("[)]" #f)
    ("a{2})" "Unmatched closing ')' near index 3\na{2})\n   ^")
    ("*)" "Dangling meta character '*' near index 0\n*)\n^")
    ("(a)b)c(" "Unmatched closing ')' near index 3\n(a)b)c(\n   ^")
    ;; A trailing backslash reads the JVM's end sentinel as a literal, so what
    ;; fails is the construct around it, one past the end for a group.
    ("(?i:a\\" "Unclosed group near index 7\n(?i:a\\")
    ("(?=a\\" "Unclosed group near index 6\n(?=a\\")
    ("(?<n>a\\" "Unclosed group near index 8\n(?<n>a\\")
    ("(a\\" "Unclosed group near index 4\n(a\\")
    ("((a\\" "Unclosed group near index 5\n((a\\")
    ("(a[\\" "Unclosed character class near index 4\n(a[\\")
    ("(a[b\\" "Unclosed character class near index 5\n(a[b\\")
    ("\\" "Unescaped trailing backslash near index 1\n\\")
    ("[\\" "Unclosed character class near index 2\n[\\")
    ("[a\\" "Unclosed character class near index 3\n[a\\")
    ;; (? at the end, and the two group types the JVM names but does not have
    ("(?" "Unknown inline modifier near index 2\n(?")
    ("(?<" "capturing group name does not start with a Latin letter near index 3\n(?<")
    ("(?P<n>a)" "Unknown inline modifier near index 2\n(?P<n>a)\n  ^")
    ("(?'n'a)" "Unknown inline modifier near index 2\n(?'n'a)\n  ^")
    ("(?^i)a" "Unknown inline modifier near index 2\n(?^i)a\n  ^")
    ("(?--i)" "Unknown inline modifier near index 3\n(?--i)\n   ^")
    ("(?U)a" #f) ("(?d)a" #f) ("(?u)a" #f) ("(?c)a" #f) ("(?x)" #f) ("(?-)" #f) ("(?i-)a" #f)
    ("(?<=a*)b" #f) ("(?<!a+)b" #f) ("(?>a)" #f)
    ;; octal: \0 wants one to three digits, the third only after a 0-3
    ("\\0" "Illegal octal escape sequence near index 2\n\\0")
    ("\\08" "Illegal octal escape sequence near index 2\n\\08\n  ^")
    ("\\01" #f) ("\\012" #f) ("\\0123" #f) ("\\0377" #f) ("\\0400" #f) ("\\01238" #f)
    ("[\\0]" "Illegal octal escape sequence near index 3\n[\\0]\n   ^")
    ("[\\01]" #f)
    ("\\0123{" "Illegal repetition near index 6\n\\0123{")
    ("\\0377{" "Illegal repetition near index 6\n\\0377{")
    ("\\0400{" "Illegal repetition near index 6\n\\0400{")
    ("\\1" #f) ("\\10" #f) ("(a)\\1" #f)
    ("\\k<a>(?<a>b)" "named capturing group <a> does not exist near index 4\n\\k<a>(?<a>b)\n    ^")
    ;; \u and \x report the first bad unit, \x{…} the unit after its digits
    ("\\u7" "Illegal Unicode escape sequence near index 3\n\\u7")
    ("]$\\u7" "Illegal Unicode escape sequence near index 5\n]$\\u7")
    ("\\uZZZZ" "Illegal Unicode escape sequence near index 2\n\\uZZZZ\n  ^")
    ("\\u12" "Illegal Unicode escape sequence near index 4\n\\u12")
    ("a\\x" "Illegal hexadecimal escape sequence near index 3\na\\x")
    ("\\x4" "Illegal hexadecimal escape sequence near index 3\n\\x4")
    ("\\x{1" "Unclosed hexadecimal escape sequence near index 4\n\\x{1")
    ("\\x{1z}" "Unclosed hexadecimal escape sequence near index 4\n\\x{1z}\n    ^")
    ("\\x{zz}" "Illegal hexadecimal escape sequence near index 2\n\\x{zz}\n  ^")
    ("\\x{110000" "Hexadecimal codepoint is too big near index 8\n\\x{110000\n        ^")
    ("\\x{ffffff}" "Hexadecimal codepoint is too big near index 8\n\\x{ffffff}\n        ^")
    ("\\u0041\\u0042" #f) ("\\ud83d\\ude00" #f)
    ;; \N{…}: the braces are the JVM's business, the name is jolt's
    ("\\N{abc" "Unclosed character name escape sequence near index 5\n\\N{abc\n     ^")
    ("\\N{}" "Unknown character name [] near index 3\n\\N{}\n   ^")
    ("\\N{bogus name}" "\\N{name} is unsupported on jolt (no Unicode character name table) near index 1\n\\N{bogus name}\n ^")
    ("\\N{[^{2}" "Unknown character name [[^{2] near index 7\n\\N{[^{2}\n       ^")
    ("(?x)a )" "Unmatched closing ')' near index 5\n(?x)a )\n     ^")
    ("(?x)a #c\n)" "Unmatched closing ')' near index 8\n(?x)a #c\n)\n        ^")
    ("(?x)(a #c" "Unclosed group near index 9\n(?x)(a #c")
    ("(?x)(a \\" "Unclosed group near index 9\n(?x)(a \\")
    ("(?x)(?<=\\1 )" "Look-behind group does not have an obvious maximum length near index 10\n(?x)(?<=\\1 )\n          ^")
    ;; COMMENTS mode: what the translator strips, the validator never sees, and
    ;; the index it reports is mapped back to the unstripped pattern
    ("(?x)a # comment (" #f) ("(?x)a b*" #f) ("(?x)[a - z]" #f) ("(?x)a\\ b" #f) ("(?x)a {2}" #f)
    ("(?x)[a#]\n]" #f) ("(?x)a #c" #f) ("(?x)a #c\n*" #f) ("(?x)\\p{ L}" #f) ("(?x)a{2, 3}" #f)
    ("(?x)a{2}  ?" #f) ("(?x)[a -z]" #f) ("(?x)[a- z]" #f) ("(?x)[a-]" #f) ("(?ix)a b" #f)
    ("(?x)[z - a]" "Illegal character range near index 9\n(?x)[z - a]\n         ^")
    ("(?x)#c\n(" "Unclosed group near index 8\n(?x)#c\n(")
    ("(?x) (" "Unclosed group near index 6\n(?x) (")
    ("(?x)[ ]" "Unclosed character class near index 6\n(?x)[ ]\n      ^")
    ("(?x)[a# ]" "Unclosed character class near index 8\n(?x)[a# ]\n        ^")
    ("(?x) *" "Dangling meta character '*' near index 5\n(?x) *\n     ^")
    ("(?x)(?#c)" "Unknown inline modifier near index 9\n(?x)(?#c)")
    ;; ...and a flag group whose OFF set names x leaves COMMENTS off.  An x
    ;; anywhere in the group used to turn it on, so the validator stripped what
    ;; the translator kept and read a different pattern than the one built:
    ;; these three compiled here and are errors on the JVM.
    ("(?-x)#\\k" "\\k is not followed by '<' for named capturing group near index 8\n(?-x)#\\k")
    ("(?i-x)#\\k" "\\k is not followed by '<' for named capturing group near index 9\n(?i-x)#\\k")
    ("(?idmsux-idmsux)#\\k" "\\k is not followed by '<' for named capturing group near index 19\n(?idmsux-idmsux)#\\k")
    ("(?-x)#(" "Unclosed group near index 7\n(?-x)#(")
    ("(?x)#\\k" #f)
    ;; An x in the ON set turns COMMENTS on whatever else the group names, and an
    ;; x in the OFF set turns it off whatever came before the "-" — addFlag and
    ;; then subFlag, so (?x-x) is off and (?x-i) is on.  All eight letters of
    ;; "idmsuxUc" are a flag group's alphabet; c (CANON_EQ) used to end the scan.
    ("(?x-i)#\\k" #f) ("(?xi-m)#\\k" #f) ("(?x-)#\\k" #f) ("(?x-s)#\\k" #f) ("(?cx)#\\k" #f)
    ("(?xc)#\\k" #f) ("(?dx)#\\k" #f)
    ("(?x-i)#(" #f) ("(?xi-m)#(" #f) ("(?x-i)a b" #f)
    ("(?s-x)#\\k" "\\k is not followed by '<' for named capturing group near index 9\n(?s-x)#\\k")
    ("(?x-x)#\\k" "\\k is not followed by '<' for named capturing group near index 9\n(?x-x)#\\k")
    ;; one "-" and no more, and a letter outside the alphabet ends the group
    ("(?-x-x)a" "Unknown inline modifier near index 4\n(?-x-x)a\n    ^")
    ("(?x--x)a" "Unknown inline modifier near index 4\n(?x--x)a\n    ^")
    ("(?X)a" "Unknown inline modifier near index 2\n(?X)a\n  ^")
    ("(?x-y)a" "Unknown inline modifier near index 4\n(?x-y)a\n    ^")
    ("(?)a" #f) ("(?-)a" #f) ("(?x-)a" #f) ("(?c)a" #f)
    ;; COMMENTS is scoped like any other flag: an unscoped (?x) runs to the end of
    ;; the enclosing group, (?x:…) to the end of its own, and (?-x) turns it off.
    ("(?x:a b)c d" #f) ("(?x)(?-x)a b" #f) ("(a(?x)b c)d e" #f) ("(?i:(?x)A B)C D" #f)
    ("(?x)a(?#c)b" "Unknown inline modifier near index 11\n(?x)a(?#c)b")
    ("(?x)[a#b]" "Unclosed character class near index 8\n(?x)[a#b]\n        ^")
    ("(?x)[a b]" #f) ("(?x)\\Qa b\\E" #f) ("(?x)a\\ b" #f)
    ;; \p{…} is read RAW: the JVM skips comments once after the "{" and then takes
    ;; the family name to the first "}" as it stands, so \p{ Lu} is \p{Lu} while
    ;; \p{Lu } is a name of its own.  A (?x: inside a name is name text, not a
    ;; flag group, and turns stripping on for nothing that follows it.
    ("(?x)\\p{ Lu}" #f)
    ("(?x)\\p{ Lu }" "Unknown character property name {Lu } near index 11\n(?x)\\p{ Lu }\n           ^")
    ("(?x)\\p{L u}" "Unknown character property name {L u} near index 10\n(?x)\\p{L u}\n          ^")
    ("\\p{(?#c)(?x: {2}(\\p{gc=Lu}" "Unknown character property name {(?#c)(?x: {2} near index 15\n\\p{(?#c)(?x: {2}(\\p{gc=Lu}\n               ^")
    ;; a quantifier's "{" peeks the next unit raw, comments in force or not
    ("(?x)a{1 ,2}" #f)
    ("(?x)a{ 1,2}" "Illegal repetition near index 6\n(?x)a{ 1,2}\n      ^")
    ("(?x)a{#c\n1,2}" "Illegal repetition near index 6\n(?x)a{#c\n1,2}\n      ^")
    ;; ── \Q…\E is rewritten away before anything reads the pattern ──────────────
    ;; Pattern.RemoveQEQuoting runs first on the JVM, so \c sees the Z the quote
    ;; yields.  The translator used to read \Q inline instead and took the
    ;; backslash that opens the quote as \c's control letter.
    ("\\c\\QZ(?=" #f) ("\\c\\QA" #f) ("\\Qab\\E*" #f)
    ("\\c\\Q" "Illegal control escape sequence near index 1\n\\c\\Q\n ^")
    ("\\c\\Q(" "Unclosed group near index 4\n\\c\\Q(\n    ^")
    ("\\Q\\E*" "Dangling meta character '*' near index 0\n\\Q\\E*\n^")
    ("\\u00\\Q41" "Illegal Unicode escape sequence near index 4\n\\u00\\Q41\n    ^")
    ;; ── an index that names a cursor, not the unit that caused the error ──────
    ;; Pattern.error() reports cursor - 1, and some errors are raised after a
    ;; next() that has already skipped the run following the offending unit. The
    ;; index then names what comes after that run.
    ("(?x)* " "Dangling meta character '*' near index 5\n(?x)* \n     ^")
    ("(?x)*a" "Dangling meta character '*' near index 4\n(?x)*a\n    ^")
    ("(?x)*  x" "Dangling meta character '*' near index 6\n(?x)*  x\n      ^")
    ("(?x)* #c\n" "Dangling meta character '*' near index 8\n(?x)* #c\n\n        ^")
    ("(?x)?  x" "Dangling meta character '?' near index 6\n(?x)?  x\n      ^")
    ("(?x)+ #c\n" "Dangling meta character '+' near index 8\n(?x)+ #c\n\n        ^")
    ("(?x)a|* " "Dangling meta character '*' near index 7\n(?x)a|* \n       ^")
    ;; Pattern.x() peeks the first unit raw and only READS once it has seen a
    ;; "{", so a malformed \x{…} reports from past the run and \xZ does not.
    ("(?x)\\x{" "Illegal hexadecimal escape sequence near index 6\n(?x)\\x{\n      ^")
    ("(?x)\\x{4 1}" #f) ("(?x)a{1 ,2}" #f)
    ("(?x)\\x{ " "Illegal hexadecimal escape sequence near index 7\n(?x)\\x{ \n       ^")
    ("(?x)\\x{  x" "Illegal hexadecimal escape sequence near index 8\n(?x)\\x{  x\n        ^")
    ("(?x)\\x{ }" "Illegal hexadecimal escape sequence near index 7\n(?x)\\x{ }\n       ^")
    ("(?x)\\xZ " "Illegal hexadecimal escape sequence near index 6\n(?x)\\xZ \n      ^")
    ;; an unclosed \N{…}: the cursor stops on the last unit read, unless an empty
    ;; name or a trailing stripped run has carried it past the sentinel
    ("(?x)\\N{" "Unclosed character name escape sequence near index 7\n(?x)\\N{")
    ("(?x)\\N{A" "Unclosed character name escape sequence near index 7\n(?x)\\N{A\n       ^")
    ("(?x)\\N{AB" "Unclosed character name escape sequence near index 8\n(?x)\\N{AB\n        ^")
    ("(?x)\\N{ " "Unclosed character name escape sequence near index 8\n(?x)\\N{ ")
    ("(?x)\\N{  x" "Unclosed character name escape sequence near index 9\n(?x)\\N{  x\n         ^")
    ("(?x)\\N{A " "Unclosed character name escape sequence near index 9\n(?x)\\N{A ")
    ("(?x)\\N{ #c\n" "Unclosed character name escape sequence near index 11\n(?x)\\N{ #c\n")
    ;; ── a \c that runs off the end of a COMMENTS pattern ─────────────────────
    ;; Pattern.c() tests cursor < patternLength against the UNSTRIPPED buffer and
    ;; then read()s, which skips the run and hands back the sentinel. So the
    ;; letter is simply the next unit that survives — (?x)\c a is \ca and
    ;; (?x)\c ) is \c) — and with nothing left the JVM carries on past its own
    ;; end: an open group is unclosed, an open class is unclosed, and at the top
    ;; level the read past the sentinel is the JDK's own internal error.
    ("(?x)\\c a" #f) ("(?x)\\c )" #f)
    ("(?x)\\c" "Illegal control escape sequence near index 5\n(?x)\\c\n     ^")
    ("(?x)\\c " "Unexpected internal error near index 7\n(?x)\\c ")
    ("(?x)\\c\n" "Unexpected internal error near index 7\n(?x)\\c\n")
    ("(?x)\\c#z" "Unexpected internal error near index 8\n(?x)\\c#z")
    ("(?x)a|\\c " "Unexpected internal error near index 9\n(?x)a|\\c ")
    ("(?x)(\\c " "Unclosed group near index 9\n(?x)(\\c ")
    ("(?x)(?:\\c " "Unclosed group near index 11\n(?x)(?:\\c ")
    ("(?x)[a\\c " "Unclosed character class near index 9\n(?x)[a\\c ")
    ;; ── a look-behind body the JVM cannot put a maximum length on ────────────
    ;; Not "unbounded": (?<!ab*) and (?<!a{2,}+) are both measured. What defeats
    ;; study() is an unbounded LAZY or POSSESSIVE repetition with something ahead
    ;; of it in the same sequence. Zero-width things are not ahead of it, and a
    ;; group holding a top-level | is a Branch, which passes nothing either way.
    ("(?<!ab*)" #f) ("(?<!a*+b)" #f) ("(?<!a{2,}+)" #f) ("(?<!a*+)" #f) ("(?<!ab??)" #f)
    ("(?<!ab{2,3}+)" #f) ("(?<!a|b*+)" #f) ("(?<!^a*+)" #f) ("(?<!\\ba*+)" #f)
    ("(?<!(?i)ab*+)" "Look-behind group does not have an obvious maximum length near index 11\n(?<!(?i)ab*+)\n           ^")
    ("(?<!(?=c)ab*+)" "Look-behind group does not have an obvious maximum length near index 12\n(?<!(?=c)ab*+)\n            ^")
    ("(?<!a(b*+|c))" #f)
    ("(?<!a(b|c)d*+)" #f) ("(?<!a(?:b|c)d*+)" #f) ("(?<!(a|b)c*+)" #f)
    ("(?<!\\x41*+)" #f) ("(?<!((b|c)d*+))" #f)
    ("(?<!ab*+)" "Look-behind group does not have an obvious maximum length near index 7\n(?<!ab*+)\n       ^")
    ("(?<!ab{2,}+)" "Look-behind group does not have an obvious maximum length near index 10\n(?<!ab{2,}+)\n          ^")
    ("(?<!ab+?)" "Look-behind group does not have an obvious maximum length near index 7\n(?<!ab+?)\n       ^")
    ("(?<!a(b*+))" "Look-behind group does not have an obvious maximum length near index 9\n(?<!a(b*+))\n         ^")
    ("(?<!(a)b*+)" "Look-behind group does not have an obvious maximum length near index 9\n(?<!(a)b*+)\n         ^")
    ("(?<!a(?=c)b*+)" "Look-behind group does not have an obvious maximum length near index 12\n(?<!a(?=c)b*+)\n            ^")
    ("(?<!\\p{Sc}{2,}+)" #f)
    ("(?<!a\\p{Sc}{2,}+)" "Look-behind group does not have an obvious maximum length near index 15\n(?<!a\\p{Sc}{2,}+)\n               ^")
    ("(?<!a\\x41*+)" "Look-behind group does not have an obvious maximum length near index 10\n(?<!a\\x41*+)\n          ^")
    ("(?<=ab*+)" "Look-behind group does not have an obvious maximum length near index 7\n(?<=ab*+)\n       ^")
    ("(?<!ab*+" "Look-behind group does not have an obvious maximum length near index 7\n(?<!ab*+\n       ^")
    ("(?<!a{2,}+" "Unclosed group near index 10\n(?<!a{2,}+")
    ("a#(?<!\t\\p{Sc}{2,}+" "Look-behind group does not have an obvious maximum length near index 17\na#(?<!\t\\p{Sc}{2,}+\n      \t          ^")
    ;; ── \p{name=value} has a message of its own ───────────────────────────────
    ;; Pattern.family splits a braced spec at the FIRST "=" and names the two
    ;; halves separately, the key lowercased and the value as written.  Only gc,
    ;; general_category, sc, script, blk and block are keys; the key is matched
    ;; without regard to case and the gc value with.
    ("\\p{GC=Lu}" #f) ("\\p{Gc=Lu}" #f) ("\\p{general_category=Lu}" #f)
    ("\\p{GENERAL_CATEGORY=Lu}" #f)
    ("\\p{foo=bar}" "Unknown Unicode property {name=<foo>, value=<bar>} near index 10\n\\p{foo=bar}\n          ^")
    ("\\p{FOO=BAR}" "Unknown Unicode property {name=<foo>, value=<BAR>} near index 10\n\\p{FOO=BAR}\n          ^")
    ("\\P{FOO=BAR}" "Unknown Unicode property {name=<foo>, value=<BAR>} near index 10\n\\P{FOO=BAR}\n          ^")
    ("[\\p{foo=bar}]" "Unknown Unicode property {name=<foo>, value=<bar>} near index 11\n[\\p{foo=bar}]\n           ^")
    ("\\p{\\p{gc=Lu}" "Unknown Unicode property {name=<\\p{gc>, value=<Lu>} near index 11\n\\p{\\p{gc=Lu}\n           ^")
    ("\\p{=}" "Unknown Unicode property {name=<>, value=<>} near index 4\n\\p{=}\n    ^")
    ("\\p{a=}" "Unknown Unicode property {name=<a>, value=<>} near index 5\n\\p{a=}\n     ^")
    ("\\p{=b}" "Unknown Unicode property {name=<>, value=<b>} near index 5\n\\p{=b}\n     ^")
    ("\\p{gc=lu}" "Unknown Unicode property {name=<gc>, value=<lu>} near index 8\n\\p{gc=lu}\n        ^")
    ("\\p{gc=Bogus}" "Unknown Unicode property {name=<gc>, value=<Bogus>} near index 11\n\\p{gc=Bogus}\n           ^")
    ("\\p{gc=Lu=x}" "Unknown Unicode property {name=<gc>, value=<Lu=x>} near index 10\n\\p{gc=Lu=x}\n          ^")
    ("(?x)\\k <a>" "named capturing group <a> does not exist near index 9\n(?x)\\k <a>\n         ^")
    ("(?x)a{2} {" "Illegal repetition near index 10\n(?x)a{2} {")
    ("(?x)a\\ (" "Unclosed group near index 8\n(?x)a\\ (")
    ("(?xi)a b(" "Unclosed group near index 9\n(?xi)a b(")
    ("(?i)a b(" "Unclosed group near index 8\n(?i)a b(")
    ;; \p: the shape, then the JVM's name table
    ("\\P" "Unknown character property name {\x00;} near index 2\n\\P")
    ("\\P{" "Unclosed character family near index 3\n\\P{")
    ("\\p{}" "Empty character family near index 3\n\\p{}\n   ^")
    ("\\p{In}" "Unknown character property name {In} near index 5\n\\p{In}\n     ^")
    ("\\p{Foo}" "Unknown character property name {Foo} near index 6\n\\p{Foo}\n      ^")
    ("\\pX" "Unknown character property name {X} near index 2\n\\pX\n  ^")
    ("\\p{IsL}" #f) ("\\p{Lu}" #f) ("\\pL" #f) ("\\p{javaLowerCase}" #f) ("\\P{L}" #f)
    ("\\p{Alpha}" #f) ("\\p{Sc}" #f) ("\\p{IsAlphabetic}" #f) ("\\p{gc=Lu}" #f) ("\\p{all}" #f)
    ;; class escapes: what the JVM refuses inside [ ], and what it does not
    ("[\\1]" "Illegal/unsupported escape sequence near index 2\n[\\1]\n  ^")
    ("[\\9]" "Illegal/unsupported escape sequence near index 2\n[\\9]\n  ^")
    ("[\\A]" "Illegal/unsupported escape sequence near index 2\n[\\A]\n  ^")
    ("[\\G]" "Illegal/unsupported escape sequence near index 2\n[\\G]\n  ^")
    ("[\\z]" "Illegal/unsupported escape sequence near index 2\n[\\z]\n  ^")
    ("[\\Z]" "Illegal/unsupported escape sequence near index 2\n[\\Z]\n  ^")
    ("[\\B]" "Illegal/unsupported escape sequence near index 2\n[\\B]\n  ^")
    ("[\\k<a>]" "Illegal/unsupported escape sequence near index 2\n[\\k<a>]\n  ^")
    ("[\\Q]" "Unclosed character class near index 2\n[\\Q]\n  ^")
    ("[\\E]" "Illegal/unsupported escape sequence near index 2\n[\\E]\n  ^")
    ("[\\X{" "Illegal/unsupported escape sequence near index 2\n[\\X{\n  ^")
    ("[\\X]" "Illegal/unsupported escape sequence near index 2\n[\\X]\n  ^")
    ("[\\Y]" "Illegal/unsupported escape sequence near index 2\n[\\Y]\n  ^")
    ("[\\h]" #f) ("[\\v]" #f) ("[\\V]" #f) ("[\\H]" #f) ("[\\a]" #f) ("[\\e]" #f)
    ("\\Y" "Illegal/unsupported escape sequence near index 1\n\\Y\n ^")
    ("\\y" "Illegal/unsupported escape sequence near index 1\n\\y\n ^")
    ("\\i" "Illegal/unsupported escape sequence near index 1\n\\i\n ^")
    ("\\C" "Illegal/unsupported escape sequence near index 1\n\\C\n ^")
    ("\\U" "Illegal/unsupported escape sequence near index 1\n\\U\n ^")
    ("\\V" #f) ("\\v" #f) ("\\h" #f) ("\\H" #f) ("\\R" #f)
    ("\\X{" "Illegal repetition near index 3\n\\X{")
    ;; ranges: the end may be an escape, and is compared as its code point; a
    ;; class escape cannot end one; a - that opens the class, follows a range
    ;; or a class escape, or precedes ] or a nested [ is a literal
    ("[a-z-" "Unclosed character class near index 4\n[a-z-\n    ^")
    ("[-" "Unclosed character class near index 1\n[-\n ^")
    ("[^-" "Unclosed character class near index 2\n[^-\n  ^")
    ("[\\d-" "Unclosed character class near index 3\n[\\d-\n   ^")
    ("[a&&-" "Unclosed character class near index 4\n[a&&-\n    ^")
    ("[--!]" "Illegal character range near index 3\n[--!]\n   ^")
    ("[--a]" #f) ("[a-\\x7a]" #f) ("[\\x41-\\x7a]" #f) ("[!-\\\\]" #f) ("[\\t-a]" #f)
    ("[a-\\x41]" "Illegal character range near index 6\n[a-\\x41]\n      ^")
    ("[a-\\t]" "Illegal character range near index 4\n[a-\\t]\n    ^")
    ("[a-\\u0041]" "Illegal character range near index 8\n[a-\\u0041]\n        ^")
    ("[a-\\cA]" "Illegal character range near index 5\n[a-\\cA]\n     ^")
    ("[a-\\0101]" "Illegal character range near index 7\n[a-\\0101]\n       ^")
    ("[a-\\Q]" "Illegal character range near index 4\n[a-\\Q]\n    ^")
    ("[a-\\n]" "Illegal character range near index 4\n[a-\\n]\n    ^")
    ("[a-\\]]" "Illegal character range near index 4\n[a-\\]]\n    ^")
    ("[a-\\-]" "Illegal character range near index 4\n[a-\\-]\n    ^")
    ("[a-\\.]" "Illegal character range near index 4\n[a-\\.]\n    ^")
    ("[a-\\p{L}]" "Illegal/unsupported escape sequence near index 4\n[a-\\p{L}]\n    ^")
    ("[\\p{L}-a]" #f) ("[a-b-c]" #f) ("[a-b-]" #f) ("[\\d-a]" #f) ("[a\\d-]" #f) ("[-]" #f) ("[^-]" #f)
    ("(?i)[a-\\x{10}]" "Illegal character range near index 12\n(?i)[a-\\x{10}]\n            ^")
    ("[\\x{41}-\\x{30}]" "Illegal character range near index 13\n[\\x{41}-\\x{30}]\n             ^")
    ("[\\t-\\x08]" "Illegal character range near index 7\n[\\t-\\x08]\n       ^")
    ("[a-\\Qb\\E]" #f)
    ("[\\Qb\\E-a]" "Illegal character range near index 3\n[\\Qb\\E-a]\n   ^")
    ;; nested classes and intersections
    ("[a[b]" "Unclosed character class near index 4\n[a[b]\n    ^")
    ("[[]" "Unclosed character class near index 2\n[[]\n  ^")
    ("[[]]" "Unclosed character class near index 3\n[[]]\n   ^")
    ("[a-[b]]" #f) ("[a-]" #f) ("[]a]" #f) ("[^]a]" #f)
    ("[]" "Unclosed character class near index 1\n[]\n ^")
    ("[^]" "Unclosed character class near index 2\n[^]\n  ^")
    ("[a&&]" #f) ("[&&a]" #f) ("[a&&&]" #f) ("[&]" #f) ("[a&&b&&c]" #f) ("[a&&[b]&&c]" #f) ("[[a]&&[b]]" #f) ("[a-z&&[^m]]" #f)
    ("[&&]" "Bad class syntax near index 2\n[&&]\n  ^")
    ("[&&&&]" "Bad class syntax near index 2\n[&&&&]\n  ^")
    ;; quantifiers: a counted closure never dangles, everything else needs an atom
    ("a{2,}" #f) ("a{2,3}}" #f) ("}" #f) ("a}" #f) ("a{1,2}{1}" #f) ("x{2}{3}" #f) ("(a){2}" #f)
    ("a{,}" "Illegal repetition near index 2\na{,}\n  ^")
    ("a{2,3" "Unclosed counted closure near index 5\na{2,3")
    ("a{1,2}{" "Illegal repetition near index 7\na{1,2}{")
    ("^*" #f) ("$*" #f) ("\\b*" #f) ("(?=a)*" #f)
    ("(?i)a{2}?+" "Dangling meta character '+' near index 9\n(?i)a{2}?+\n         ^")
    ("a?*" "Dangling meta character '*' near index 2\na?*\n  ^")
    ("a+?+" "Dangling meta character '+' near index 3\na+?+\n   ^")
    ("a*+*" "Dangling meta character '*' near index 3\na*+*\n   ^")
    ("|" #f) ("||" #f) ("(|)" #f) ("(?:)" #f) ("()" #f) ("(?<n>)" #f)
    ("\\k<n>" "named capturing group <n> does not exist near index 4\n\\k<n>\n    ^")
    ("(?<a>x)\\k<a" "named capturing group is missing trailing '>' near index 11\n(?<a>x)\\k<a")
    ("(?<a>x)\\k<b>" "named capturing group <b> does not exist near index 11\n(?<a>x)\\k<b>\n           ^")
    ("(?<a>x)\\k<a1>" "named capturing group <a1> does not exist near index 12\n(?<a>x)\\k<a1>\n            ^")
    ("(?<a>x)(?<A>y)" #f) ("(?<a>x)\\k<a>" #f)
    ;; a look-behind may be unbounded on JDK 25 (a*, .*) but not hold a back-reference
    ("(?<=a*)b" #f) ("(?<=.*)x" #f) ("(?<=a{2,})" #f) ("(?<=a|bc)" #f) ("(?<=(?<=a*)b)" #f)
    ("(?<=\\1)" "Look-behind group does not have an obvious maximum length near index 5\n(?<=\\1)\n     ^")
    ("(?<=(a)\\1)" "Look-behind group does not have an obvious maximum length near index 8\n(?<=(a)\\1)\n        ^")
    ("(?<=a\\1)b" "Look-behind group does not have an obvious maximum length near index 6\n(?<=a\\1)b\n      ^")
    ("(?<=\\1" "Look-behind group does not have an obvious maximum length near index 5\n(?<=\\1\n     ^")
    ("(?<a>x)(?<=\\k<a>)" "Look-behind group does not have an obvious maximum length near index 15\n(?<a>x)(?<=\\k<a>)\n               ^")
    ("(?<=\\k<a>)(?<a>x)" "named capturing group <a> does not exist near index 8\n(?<=\\k<a>)(?<a>x)\n        ^")
    ;; the index counts code points (Pattern parses those); the caret line is
    ;; measured against UTF-16 units (getMessage pads the Java String), so an
    ;; astral character before the index is two columns and counts two toward
    ;; "inside the pattern"
    ("(?:😀" "Unclosed group near index 4\n(?:😀\n    ^")
    ("((?<=\\B😀" "Unclosed group near index 8\n((?<=\\B😀\n        ^")
    ("😀(" "Unclosed group near index 2\n😀(\n  ^")
    ("😀*" #f)
    ("😀😀)" "Unmatched closing ')' near index 1\n😀😀)\n ^")
    ("a\t😀(" "Unclosed group near index 4\na\t😀(\n \t  ^")
    ;; a lone surrogate compiles, as on the JVM, to match nothing: no jolt
    ;; string can hold one (#1611). It used to be refused here.
    ("\\ud83d" #f) ("[\\ud83d-\\ude00]" #f) ("[a\\uD800]" #f) ("[^\\uD800-\\uDFFF]" #f)
    ;; but a range out of order is still the JVM's error, surrogates or not
    ("[\\uDBFF-\\uD800]" "Illegal character range near index 13\n[\\uDBFF-\\uD800]\n             ^")
    ;; ── jolt's own refusals of patterns the JVM compiles (known-divergences.edn) ──
    ("\\N{NULL}" "\\N{name} is unsupported on jolt (no Unicode character name table) near index 1\n\\N{NULL}\n ^")
    ("\\X" "\\X (extended grapheme cluster) is unsupported on jolt near index 1\n\\X\n ^")
    ("\\p{InGreek}" "\\p{InGreek} is unsupported on jolt (no Unicode block table for it) near index 10\n\\p{InGreek}\n          ^")
    ("\\p{IsLatin}" "\\p{IsLatin} is unsupported on jolt (no Unicode script or property table for it) near index 10\n\\p{IsLatin}\n          ^")
    ))

(for-each
 (lambda (v)
   (set! total (+ total 1))
   (let* ((pat (car v)) (want (cadr v)) (got (pattern-message pat)))
     (unless (equal? got want)
       (set! fails (+ fails 1))
       (printf "FAIL: ~s\n  want ~s\n  got  ~s\n" pat want got))))
 vectors)

(printf "regex-syntax: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
