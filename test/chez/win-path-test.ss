;; The path rules that read a platform rather than a filesystem, per platform:
;; the lexical half of java.io.File/getCanonicalPath (jolt-lang/jolt#991) and
;; the glob translator's separator class (jolt-lang/jolt#1086). Run:
;;   chez --script test/chez/win-path-test.ss
;;
;; realpath(3) is not bound on a Windows build, so THIS is the whole of
;; getCanonicalPath there — and its POSIX-only spelling rejoined every segment
;; as "/" + segment, so `(babashka.fs/canonicalize "C:/Users/x/a.txt")` answered
;; "/C:/Users/x/a.txt": a path rooted on the current drive, which every later
;; read or write resolved as "C:/C:/Users/x/…" and failed. Backslashes were
;; worse — no separator was recognized, so the whole path was one segment.
;;
;; The Windows rows are the reason the platform is a parameter: they are
;; unreachable from the host that runs CI, and the walk that re-attaches a
;; missing tail to its longest existing ancestor takes realpath as a parameter
;; too, so it is driven here from a table rather than from a filesystem.

(import (chezscheme))
(load "host/chez/gate-boot.ss")
(load "host/chez/java/io.ss")
(load "host/chez/java/nio-file.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (row label windows? given want)
  (let ((got (jfile-fold-dots-for windows? given)))
    (set! total (+ total 1))
    (unless (string=? got want)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a (windows? ~s): ~s -> ~s, want ~s\n" label windows? given got want))))

;; --- POSIX is unchanged ------------------------------------------------------
;; "\" is an ordinary filename character here and stays one; a doubled separator
;; collapses; "." and ".." fold; the root is its own answer.
(row "posix absolute"      #f "/a/b"            "/a/b")
(row "posix dot"           #f "/a/./b"          "/a/b")
(row "posix dotdot"        #f "/a/c/../b"       "/a/b")
(row "posix dotdot at root" #f "/a/../.."       "/")
(row "posix doubled sep"   #f "/a//b"           "/a/b")
(row "posix leading pair"  #f "//a/b"           "/a/b")
(row "posix root"          #f "/"               "/")
(row "posix backslash is a name" #f "/a/b\\c"   "/a/b\\c")
;; a relative path stays relative rather than growing a root it never had
(row "posix relative"      #f "a/b"             "a/b")
(row "posix relative dot"  #f "a/./b"           "a/b")

;; --- Windows: the root is reproduced, never invented -------------------------
;; Rendered with "/", which is what getAbsolutePath, babashka.fs/absolutize and
;; babashka.fs/normalize already answer with on Windows — so canonicalize agrees
;; with its neighbours, and one file has ONE canonical string however the caller
;; spelled its separators. That identity is what getCanonicalPath exists for.
(row "drive slash"         #t "C:/Users/x/a.txt"  "C:/Users/x/a.txt")
(row "drive backslash"     #t "C:\\Users\\x\\a.txt" "C:/Users/x/a.txt")
(row "drive mixed"         #t "C:\\Users\\x/a.txt"  "C:/Users/x/a.txt")
(row "drive lowercase"     #t "d:/x"               "d:/x")
(row "drive root only"     #t "C:/"                "C:/")
(row "drive dot"           #t "C:/a/./b"           "C:/a/b")
(row "drive dotdot"        #t "C:\\a\\c\\..\\b"    "C:/a/b")
(row "drive dotdot past root" #t "C:/a/../.."      "C:/")
(row "drive doubled sep"   #t "C:/a//b"            "C:/a/b")
;; UNC: \\server\share is the root, so neither half may be folded away by a ".."
(row "unc"                 #t "\\\\srv\\sh\\a\\b"  "//srv/sh/a/b")
(row "unc forward"         #t "//srv/sh/a"         "//srv/sh/a")
(row "unc root only"       #t "//srv/sh"           "//srv/sh/")
(row "unc dotdot past root" #t "//srv/sh/a/../.."  "//srv/sh/")
;; device paths (\\?\C:\x) have the same two-segment root shape
(row "device path"         #t "\\\\?\\C:\\a"       "//?/C:/a")
;; rooted on the current drive: still not a drive, so it keeps the one "/" it
;; was given and gains nothing
(row "current-drive rooted" #t "/a/b"              "/a/b")
;; C:a names the per-drive current directory, which this process cannot see.
;; Keep the caller's meaning rather than invent a root for it — "/C:a" named a
;; different file, and "C:/a" would silently name the drive's root.
(row "drive-relative kept" #t "C:a\\b"             "C:a/b")
(row "relative"            #t "a\\b"               "a/b")

;; --- the missing-tail walk ---------------------------------------------------
;; The JVM canonicalizes a path whose tail does not exist by resolving the
;; longest ancestor that DOES and re-attaching the rest. Driven from a table:
;; "C:/Users/real" is the deepest thing that exists, and it answers under a
;; different spelling (a junction), which is exactly what must survive.
(define (fake-realpath p)
  (cond ((string=? p "C:/Users/real") "C:/Users/target")
        ((string=? p "/u/real") "/u/target")
        (else #f)))
(define (canon label windows? given want)
  (let ((got (jfile-canonical-for windows? fake-realpath given)))
    (set! total (+ total 1))
    (unless (string=? got want)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a: ~s -> ~s, want ~s\n" label given got want))))

(canon "existing path answers realpath" #t "C:/Users/real" "C:/Users/target")
(canon "missing leaf re-attaches"       #t "C:/Users/real/nope.txt" "C:/Users/target/nope.txt")
(canon "missing tree re-attaches"       #t "C:/Users/real/no/such/f" "C:/Users/target/no/such/f")
(canon "backslash input walks too"      #t "C:\\Users\\real\\nope.txt" "C:/Users/target/nope.txt")
(canon "dotdot folds in the missing tail" #t "C:/Users/real/no/../d/f" "C:/Users/target/d/f")
;; nothing on the path exists: the whole thing folds lexically, root intact
(canon "no ancestor resolves"           #t "C:/gone/./a/../b" "C:/gone/b")
(canon "posix walk unchanged"           #f "/u/real/nope.txt" "/u/target/nope.txt")
(canon "posix no ancestor resolves"     #f "/gone/./a/../b" "/gone/b")

;; --- the glob translator's separator class (jolt-lang/jolt#1086) -------------
;; Same reason the platform is a parameter here: jolt rendered every path with
;; "/" everywhere before #1110, but the vendored babashka.fs/match reads os.name and on
;; Windows hands the matcher `escaped-base + "\\" + "/" + pattern-with-"/"-
;; rewritten-to-"\\\\"`. So the Windows rows below spell a separator the way
;; babashka.fs really does — as an escaped backslash — and that spelling used to
;; translate to a literal backslash no "/"-rendered path could ever hold:
;; (fs/glob "src" "**/*.clj") answered () on Windows while "**.clj" answered
;; every file. These rows are unreachable from the host that runs CI.
(define (rx label windows? pattern want)
  (let ((got (npath-glob->regex-for windows? pattern)))
    (set! total (+ total 1))
    (unless (string=? got want)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a (windows? ~s): ~s -> ~s, want ~s\n" label windows? pattern got want))))

;; and the translation actually applied to a path, which is what fs/glob asks
(define (globs label windows? pattern path want)
  (let ((got (and (jolt-truthy? (jolt-re-matches (jolt-re-pattern (npath-glob->regex-for windows? pattern))
                                                 path))
                  #t)))
    (set! total (+ total 1))
    (unless (eq? got want)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a (windows? ~s): ~s vs ~s -> ~s, want ~s\n"
              label windows? pattern path got want))))

;; POSIX is unchanged: "/" alone separates and "\" is an ordinary filename
;; character, so "a\\b" still names the file really called a\b and not a/b.
(rx "posix segment class"   #f "*.clj"     "^[^/]*\\.clj$")
(rx "posix crossing"        #f "**/*.clj"  "^.*/[^/]*\\.clj$")
(rx "posix escaped backslash is a literal" #f "a\\\\b" "^a\\\\b$")
(globs "posix ** crosses"   #f "**/*.clj" "/r/d1/d2/two.clj" #t)
(globs "posix * does not cross" #f "*.clj" "d1/one.clj" #f)
(globs "posix backslash names a file" #f "a\\\\b" "a\\b" #t)
(globs "posix backslash is not a separator" #f "a\\\\b" "a/b" #f)

;; Windows: both spellings separate, as the JDK's Globs reads a Windows pattern.
(rx "win segment class"     #t "*.clj"       "^[^/\\\\]*\\.clj$")
(rx "win rewritten crossing" #t "**\\\\*.clj" "^.*[/\\\\][^/\\\\]*\\.clj$")
(rx "win plain / still separates" #t "**/*.clj" "^.*[/\\\\][^/\\\\]*\\.clj$")
;; and since a path renders with "\\" on Windows (jolt-lang/jolt#1110), a "/" in
;; a pattern has to meet one: the JDK's Windows glob reads "/" as the separator
(globs "win / in a pattern matches a native separator" #t "sub/*.clj" "sub\\deep.clj" #t)
(globs "win / in a pattern still matches /" #t "sub/*.clj" "sub/deep.clj" #t)
;; what babashka.fs/match builds once the base it str's is native: every "\\"
;; in the base escaped, then the escaped separator, then the rewritten pattern
(globs "win native base matches a native path"
       #t "C:\\\\src\\\\**\\\\*.clj" "C:\\src\\kmet\\libs\\terminal.clj" #t)
(globs "win native base rejects the wrong extension"
       #t "C:\\\\src\\\\**\\\\*.clj" "C:\\src\\kmet\\libs\\terminal.cljc" #f)
;; the whole string babashka.fs/match builds for (fs/glob "C:/src" "**/*.clj"):
;; the base, the escaped separator it appends, then the rewritten pattern
(globs "win rewritten pattern matches a nested file"
       #t "C:/src\\/**\\\\*.clj" "C:/src/kmet/libs/terminal.clj" #t)
(globs "win rewritten pattern rejects the wrong extension"
       #t "C:/src\\/**\\\\*.clj" "C:/src/kmet/libs/terminal.cljc" #f)
(globs "win ** with no separator still matches"
       #t "C:/src\\/**.clj" "C:/src/kmet/libs/terminal.clj" #t)
;; * stops at either separator rather than running through the tree
(globs "win * does not cross /"  #t "*.clj" "d1/one.clj" #f)
(globs "win * does not cross \\" #t "*.clj" "d1\\one.clj" #f)
;; inside a character class "\\" is still a literal: [/\] would name two
;; members there, not one separator
(rx "win class keeps the literal" #t "a[\\\\]b" "^a[\\\\]b$")
;; an escape of anything else is untouched on both platforms
(rx "win escaped star"      #t "a\\*b"     "^a\\*b$")
(rx "posix escaped star"    #f "a\\*b"     "^a\\*b$")

;; --- Files/isHidden: the name on POSIX, the DOS attribute on Windows ---------
;; java.nio.file.Files.isHidden is implementation-specific and the JDK answers it
;; per platform. This tested the leading dot everywhere, so BOTH Windows rows were
;; wrong: a dot-prefixed file read as hidden when Windows says it is not, and a
;; file carrying the attribute read as visible when Windows says it is
;; (jolt-lang/jolt#1110). babashka.fs/hidden? is Files/isHidden verbatim and
;; fs/glob skips hidden entries by asking it, so the two answers decided which
;; files a glob returned.
;;
;; `attrs` is the DOS attribute word GetFileAttributesW answers, or #f when the
;; path cannot be read — the parameter that lets the Windows rows run here.
(define HIDDEN #x2)
(define DIR    #x10)
(define ARCHIVE #x20)
(define (hid label windows? attrs name want)
  (let ((got (nio-hidden-for? windows? attrs name)))
    (set! total (+ total 1))
    (unless (eq? (and got #t) want)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a (windows? ~s): got ~s, want ~s\n" label windows? got want))))

;; POSIX: the NAME decides, and an attribute word is not consulted even if one
;; were somehow available.
(hid "posix dot file is hidden"        #f #f ".dot.clj"  #t)
(hid "posix plain file is not"         #f #f "plain.clj" #f)
(hid "posix bare dot-name is hidden"   #f #f "."         #t)
(hid "posix empty name is not hidden"  #f #f ""          #f)
(hid "posix ignores the attribute"     #f HIDDEN "plain.clj" #f)

;; Windows: the ATTRIBUTE decides, and the name is not consulted at all.
(hid "win dot file without the attribute is visible" #t ARCHIVE ".dot.clj"  #f)
(hid "win plain file with the attribute is hidden"   #t (bitwise-ior ARCHIVE HIDDEN) "attr.clj" #t)
(hid "win plain file without it is visible"          #t ARCHIVE "plain.clj" #f)
(hid "win dot file WITH the attribute is hidden"     #t HIDDEN  ".dot.clj"  #t)
;; no directory exception on either side — Files.isHidden of a hidden directory
;; is true on Windows, and WindowsFileAttributes.isHidden is the bare bit test
(hid "win hidden directory is hidden"  #t (bitwise-ior DIR HIDDEN) "sub" #t)
(hid "win plain directory is not"      #t DIR "sub" #f)
;; an unreadable path answers "not hidden" rather than raising, which is what a
;; failed GetFileAttributesW (INVALID_FILE_ATTRIBUTES) reaches this as
(hid "win unreadable path is not hidden" #t #f "gone.clj" #f)

;; --- the directory half of an output path (jolt-lang/jolt#1207) ---------------
;; build.ss bld-mkdir-p and loader.ss aot-mkdir-p walk up with path-parent until
;; something exists. It split on "/" only, so a native Windows path —
;; "C:\proj\out\app.exe.build" — had no parent at all, and the walk recursed with
;; #f into a string=?. Even a "/"-spelled drive path bottomed out at "C:", which
;; is the drive's CURRENT directory rather than its root. #f is the answer for
;; a root or a bare name: there is nothing above it to create.
(define (parent label windows? given want)
  (let ((got (path-parent-for windows? given)))
    (set! total (+ total 1))
    (unless (equal? got want)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a (windows? ~s): ~s -> ~s, want ~s\n" label windows? given got want))))

(parent "posix nested"            #f "/a/b"        "/a")
(parent "posix under the root"    #f "/a"          "/")
(parent "posix root"              #f "/"           #f)
(parent "posix relative"          #f "a/b"         "a")
(parent "posix bare name"         #f "a"           #f)
(parent "posix empty"             #f ""            #f)
(parent "posix trailing sep"      #f "/a/b/"       "/a/b")
(parent "posix backslash is a name" #f "/a/b\\c"   "/a")
(parent "posix drive spelling is a name" #f "C:/a" "C:")

(parent "win backslash"           #t "C:\\proj\\out\\app.exe.build" "C:\\proj\\out")
(parent "win backslash under root" #t "C:\\proj"   "C:\\")
(parent "win backslash root"      #t "C:\\"        #f)
(parent "win slash"               #t "C:/proj/out" "C:/proj")
(parent "win slash under root"    #t "C:/proj"     "C:/")
(parent "win slash root"          #t "C:/"         #f)
(parent "win mixed"               #t "C:\\proj/out\\x" "C:\\proj/out")
(parent "win bare drive"          #t "C:"          #f)
(parent "win drive-relative"      #t "C:a"         "C:")
(parent "win unc"                 #t "\\\\srv\\sh\\a\\b" "\\\\srv\\sh\\a")
(parent "win unc under the share" #t "\\\\srv\\sh\\a" "\\\\srv\\sh")
(parent "win unc share is a root" #t "\\\\srv\\sh" #f)
(parent "win unc forward"         #t "//srv/sh/a"  "//srv/sh")
(parent "win rooted on the current drive" #t "\\a" "\\")
(parent "win relative"            #t "a\\b"        "a")
(parent "win bare name"           #t "a"           #f)

;; The walk itself, as bld-mkdir-p takes it: every parent until one exists, and
;; it ends — a #f is where it stops, never an argument to the next step.
(define (walk windows? exists p)
  (let loop ((p p) (acc '()))
    (if (or (not p) (member p exists)) (reverse acc) (loop (path-parent-for windows? p) (cons p acc)))))
(set! total (+ total 1))
(let ((got (walk #t '() "C:\\proj\\out\\app.exe.build")))
  (unless (equal? got '("C:\\proj\\out\\app.exe.build" "C:\\proj\\out" "C:\\proj" "C:\\"))
    (set! fails (+ fails 1))
    (printf "FAIL: the drive walk ends at the root: ~s\n" got)))

;; --- the long spelling (jolt-lang/jolt#1281) ----------------------------------
;; getCanonicalPath on Windows replaces each existing component with the name
;; its directory lists, so an 8.3 alias and a miscased name both come back as the
;; real one. The listing is a table here; on Windows it is FindFirstFileW.
(define listing
  '(("C:/Users/MARKO~1.KOC" . "Marko.Kocic")
    ("C:/Users" . "Users")
    ("C:/users" . "Users")
    ("C:/Users/Marko.Kocic" . "Marko.Kocic")
    ("C:/Users/Marko.Kocic/AppData" . "AppData")
    ("C:/Users/Marko.Kocic/AppData/Local" . "Local")
    ("C:/Users/Marko.Kocic/AppData/Local/Temp" . "Temp")
    ("C:/Users/Marko.Kocic/appdata" . "AppData")
    ("//srv/sh/Dir" . "Dir")
    ("//srv/sh/dir" . "Dir")))
(define (listed p) (let ((e (assoc p listing))) (and e (cdr e))))
(define (long label given want)
  (let ((got (win32-long-path-with listed given)))
    (set! total (+ total 1))
    (unless (equal? got want)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a: ~s -> ~s, want ~s\n" label given got want))))
(long "short profile name" "C:\\Users\\MARKO~1.KOC\\AppData\\Local\\Temp"
      "C:/Users/Marko.Kocic/AppData/Local/Temp")
(long "trailing separator" "C:\\Users\\MARKO~1.KOC\\AppData\\Local\\Temp\\"
      "C:/Users/Marko.Kocic/AppData/Local/Temp")
(long "case and drive letter" "c:/users/Marko.Kocic/appdata" "C:/Users/Marko.Kocic/AppData")
(long "dots fold first" "C:/Users/x/../MARKO~1.KOC/./AppData" "C:/Users/Marko.Kocic/AppData")
(long "drive root" "c:\\" "C:/")
(long "unc" "\\\\srv\\sh\\dir" "//srv/sh/Dir")
(long "missing component" "C:/Users/nobody/x" #f)
(long "a wildcard names no file" "C:/Users/*" #f)
(long "relative" "Users" #f)

(set! total (+ total 1))
(unless (and (string=? (win32-strip-final-prefix "\\\\?\\C:\\Users\\Marko.Kocic") "C:\\Users\\Marko.Kocic")
             (string=? (win32-strip-final-prefix "\\\\?\\UNC\\srv\\sh\\d") "\\\\srv\\sh\\d")
             (string=? (win32-strip-final-prefix "C:\\x") "C:\\x"))
  (set! fails (+ fails 1))
  (printf "FAIL: GetFinalPathNameByHandleW prefixes\n"))

;; a relative path under the root directory gets one separator, not two: "//src"
;; as a URL path is file://src/, a host named "src"
(ok "path under the root" (string=? (path-under "/" "src") "/src"))
(ok "path under a directory" (string=? (path-under "/a" "src") "/a/src"))
(ok "path under a drive root" (string=? (path-under "C:\\" "src") "C:\\src"))

;; The File operations used by babashka.fs on a relative name must work
;; without consulting user.dir. Count lookups so the check is independent of
;; the host's cwd lookup cost.
(let ((saved jolt-user-dir) (lookups 0) (f (make-jfile "dir/file.edn")))
  (set! jolt-user-dir (lambda () (set! lookups (+ lookups 1)) (saved)))
  (ok "relative File.getName" (equal? (jfile-method f "getName" '()) '("file.edn")))
  (ok "relative File.toPath" (string=? (nio-path-str (car (jfile-method f "toPath" '()))) "dir/file.edn"))
  (ok "lexical File operations skip user.dir" (zero? lookups))
  (jfile-method f "getAbsolutePath" '())
  (ok "absolute File path consults user.dir" (> lookups 0))
  (set! jolt-user-dir saved))

(if (> fails 0)
    (begin (printf "WIN-PATH FAILURES: ~a of ~a\n" fails total) (exit 1))
    (printf "WIN-PATH OK (~a checks)\n" total))
