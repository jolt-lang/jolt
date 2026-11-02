;; The Windows-shaped answers a Linux runner can never observe
;; (jolt-lang/jolt#1074). Run:
;;   chez --script test/chez/win-platform-test.ss
;;
;; Every one of these shipped in v0.8.10 because nothing here was a parameter,
;; so each row was whichever one the CI host happened to be:
;;
;;   * path.separator answered ":" on Windows, where ":" is the drive suffix.
;;     (babashka.fs/split-paths "C:/a;C:/b") came back ["C" "/a;C" "/b"], so
;;     fs/exec-paths was garbage and fs/which never found anything.
;;   * ProcessBuilder split PATH on ":" too, treated only a leading "/" as
;;     absolute, and knew nothing of PATHEXT — so a bare "curl" and a
;;     drive-absolute "C:/Windows/System32/curl.exe" were both unresolvable and
;;     babashka.process threw before a spawn was attempted.
;;   * The java.nio.file Path shim had a POSIX-only root, so a drive path was a
;;     RELATIVE path whose first segment happened to be "C:": fs/absolute? said
;;     false, getRoot said nil, getParent walked off the drive letter and
;;     normalize could fold a path above its own root.
;;   * java.io.tmpdir read only TMPDIR, which Windows does not set, so every
;;     temp file went to "/tmp" on whichever drive the process was on.
;;   * File/listRoots answered "/" instead of enumerating the mounted drives.
;;   * spit staged into a temp file and renamed over the target, and Windows
;;     refuses a rename onto an existing destination — so the SECOND spit to any
;;     path failed. That one needs a real filesystem and is gated on the Windows
;;     runner (.github/workflows/tests.yml); what is pinned here is the POSIX
;;     half of the same helper, which must keep replacing without the delete.
;;
;; Like host-derived-props-test.ss and win-path-test.ss, the table is pinned over
;; the platform the run does NOT have: the *-for entry points exist for exactly
;; that, and the row that broke is only reachable from a host we do not gate on.

(import (chezscheme))
(load "host/chez/gate-boot.ss")
(load "host/chez/java/io.ss")
(load "host/chez/java/process.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (same name got want)
  (set! total (+ total 1))
  (unless (equal? got want)
    (set! fails (+ fails 1))
    (printf "FAIL: ~a: got ~s, want ~s\n" name got want)))

;; --- the PATH-list separator -------------------------------------------------
;; Not the file separator: ";" on Windows because ":" is the drive suffix.
(same "posix path-list separator"   (path-list-separator-for #f) ":")
(same "windows path-list separator" (path-list-separator-for #t) ";")
;; and the running host still answers for itself, so this file cannot pass by
;; pinning a table that nothing consults
(same "this host's separator" (path-list-separator)
      (path-list-separator-for (eq? (sa-os-family) 'windows)))

;; --- PATH splitting ----------------------------------------------------------
;; The entry that motivated the whole issue: a drive-lettered directory survives
;; intact only when the split knows ";".
(same "windows PATH keeps drive letters"
      (proc-path-entries-for #t "C:\\Windows\\System32;C:\\Users\\x\\bin")
      '("C:\\Windows\\System32" "C:\\Users\\x\\bin"))
(same "posix PATH is unchanged"
      (proc-path-entries-for #f "/usr/bin:/usr/local/bin")
      '("/usr/bin" "/usr/local/bin"))
;; a ":" inside a Windows entry is data, and a ";" inside a POSIX one is too
(same "windows does not split on colon"
      (proc-path-entries-for #t "C:/a") '("C:/a"))
(same "posix does not split on semicolon"
      (proc-path-entries-for #f "/a;b") '("/a;b"))
;; cmd's quotes around an entry holding a space are the shell's, not the name's
(same "windows entry is unquoted"
      (proc-path-entries-for #t "\"C:\\Program Files\\x\";C:\\y")
      '("C:\\Program Files\\x" "C:\\y"))
(same "an empty PATH yields one empty entry, which the walk skips"
      (proc-path-entries-for #t "") '(""))

;; --- joining a directory to a program name -----------------------------------
;; A native Windows directory keeps its backslashes rather than gaining a mixed
;; spelling; a trailing separator of either kind is not doubled.
(same "windows native join"      (proc-path-join-for #t "C:\\Windows\\System32" "curl.exe")
      "C:\\Windows\\System32\\curl.exe")
(same "windows slash join"       (proc-path-join-for #t "C:/Windows/System32" "curl.exe")
      "C:/Windows/System32/curl.exe")
(same "windows trailing backslash" (proc-path-join-for #t "C:\\bin\\" "x.exe") "C:\\bin\\x.exe")
(same "windows trailing slash"   (proc-path-join-for #t "C:/bin/" "x.exe") "C:/bin/x.exe")
(same "windows mixed prefers slash" (proc-path-join-for #t "C:/a\\b" "x") "C:/a\\b/x")
(same "posix join"               (proc-path-join-for #f "/usr/bin" "curl") "/usr/bin/curl")
(same "posix trailing slash"     (proc-path-join-for #f "/usr/bin/" "curl") "/usr/bin/curl")
;; on POSIX a trailing backslash is part of the filename, so it is NOT a
;; separator and the join still adds one
(same "posix trailing backslash is a name" (proc-path-join-for #f "/usr/b\\" "curl")
      "/usr/b\\/curl")
(same "empty base"               (proc-path-join-for #t "" "curl.exe") "curl.exe")

;; --- what counts as a separator in a program name ----------------------------
(ok "windows sees a backslash"      (proc-has-separator-for? #t "bin\\x"))
(ok "windows sees a slash"          (proc-has-separator-for? #t "bin/x"))
(ok "posix sees a slash"            (proc-has-separator-for? #f "bin/x"))
;; a backslash is an ordinary filename character on POSIX, so "a\b" is a bare
;; name to look up on PATH, not a relative path
(ok "posix backslash is not a separator" (not (proc-has-separator-for? #f "a\\b")))
(ok "a bare name has none"          (not (proc-has-separator-for? #t "curl")))

;; --- PATHEXT candidates ------------------------------------------------------
;; The bare name is tried FIRST, so a program that already carries its extension
;; resolves as itself rather than as "curl.exe.COM".
(same "windows default PATHEXT" (proc-name-candidates-for #t #f "curl")
      '("curl" "curl.COM" "curl.EXE" "curl.BAT" "curl.CMD"))
(same "windows honors PATHEXT" (proc-name-candidates-for #t ".EXE;.PS1" "curl")
      '("curl" "curl.EXE" "curl.PS1"))
(same "an empty PATHEXT falls back to cmd's list"
      (proc-name-candidates-for #t "" "curl")
      '("curl" "curl.COM" "curl.EXE" "curl.BAT" "curl.CMD"))
(same "empty PATHEXT entries are dropped"
      (proc-name-candidates-for #t ".EXE;;" "curl") '("curl" "curl.EXE"))
(same "posix has no extension search" (proc-name-candidates-for #f ".EXE" "curl") '("curl"))

;; --- program resolution ------------------------------------------------------
;; Driven from a table rather than a filesystem, the way win-path-test.ss drives
;; realpath: these are the files each platform's run is asked to believe in.
(define win-files
  '("C:\\Windows\\System32\\curl.exe"
    "C:\\Users\\x\\bin\\tool.bat"
    "C:\\proj\\sub\\local.exe"
    "\\\\srv\\sh\\net.exe"
    "\\WinRooted\\only.exe"))
(define posix-files
  '("/usr/bin/curl" "/proj/sub/local" "/usr/bin/weird\\name"))
;; The Windows table is matched case-insensitively and with either separator
;; spelling, because that is what NTFS is: "C:/Windows/System32/CURL.EXE" and
;; "C:\Windows\System32\curl.exe" open the same file. A table that insisted on
;; one spelling would fail rows the real platform passes — and would make the
;; PATHEXT rows (which append ".EXE" to a file named curl.exe) meaningless.
(define (win-key p)
  (string-downcase
   (list->string (map (lambda (c) (if (char=? c #\\) #\/ c)) (string->list p)))))
(define win-keys (map win-key win-files))
(define (win-exists? p) (and (member (win-key p) win-keys) #t))
;; POSIX is neither: case and backslashes are part of the name.
(define (posix-exists? p) (and (member p posix-files) #t))

(define win-path "C:\\Windows\\System32;C:\\Users\\x\\bin")
(define (winres prog) (proc-program-resolvable-for? #t #f win-path "C:\\proj" prog win-exists?))
(define (posixres prog) (proc-program-resolvable-for? #f #f "/usr/bin" "/proj" prog posix-exists?))

;; the three shapes the issue reported as broken
(ok "windows bare name resolves through PATHEXT" (winres "curl"))
(ok "windows drive-absolute with slashes"        (winres "C:/Windows/System32/curl.exe"))
(ok "windows drive-absolute with backslashes"    (winres "C:\\Windows\\System32\\curl.exe"))
;; a .bat on PATH is found the same way
(ok "windows PATHEXT reaches .bat"               (winres "tool"))
;; UNC and current-drive-rooted programs are named as spelled, never joined to
;; the child cwd
(ok "windows UNC program"                        (winres "\\\\srv\\sh\\net.exe"))
(ok "windows current-drive-rooted program"       (winres "\\WinRooted\\only.exe"))
;; a separator-bearing relative program resolves against the child cwd
(ok "windows relative program joins the cwd"     (winres "sub\\local.exe"))
(ok "windows relative program, slash spelling"   (winres "sub/local.exe"))
;; and what is genuinely absent still says so
(ok "windows missing bare name"                  (not (winres "nosuch")))
(ok "windows missing absolute"                   (not (winres "C:\\nope.exe")))
(ok "windows missing relative"                   (not (winres "sub\\nope.exe")))
(ok "an empty program is unresolvable"           (not (winres "")))

;; POSIX keeps every answer it had
(ok "posix bare name on PATH"        (posixres "curl"))
(ok "posix absolute"                 (posixres "/usr/bin/curl"))
(ok "posix relative joins the cwd"   (posixres "sub/local"))
(ok "posix missing bare name"        (not (posixres "nosuch")))
(ok "posix missing absolute"         (not (posixres "/nope")))
(ok "an empty posix program is unresolvable" (not (posixres "")))
;; no extension is ever appended on POSIX: "curl" must not resolve because
;; "curl.EXE" happens to exist
(ok "posix appends no extension"
    (not (proc-program-resolvable-for? #f ".EXE" "/usr/bin" "/proj" "weird"
                                       (lambda (p) (string=? p "/usr/bin/weird.EXE")))))
;; a backslash-bearing name on POSIX is a bare name, looked up on PATH
(ok "posix backslash name goes to PATH" (posixres "weird\\name"))

;; --- absolute-path classification, as the resolver asks it -------------------
(ok "windows drive with slash is absolute"     (jfile-path-absolute-for? #t "C:/x"))
(ok "windows drive with backslash is absolute" (jfile-path-absolute-for? #t "C:\\x"))
(ok "windows UNC is absolute"                  (jfile-path-absolute-for? #t "\\\\srv\\sh"))
(ok "windows single slash is NOT absolute"     (not (jfile-path-absolute-for? #t "/x")))
(ok "windows drive-relative is NOT absolute"   (not (jfile-path-absolute-for? #t "C:x")))
(ok "posix slash is absolute"                  (jfile-path-absolute-for? #f "/x"))
(ok "posix drive letter is NOT absolute"       (not (jfile-path-absolute-for? #f "C:/x")))
(ok "windows single slash is root-relative"    (windows-root-relative-for? #t "\\x"))
(ok "windows UNC is not root-relative"         (not (windows-root-relative-for? #t "\\\\srv\\sh")))
(ok "posix has no root-relative shape"         (not (windows-root-relative-for? #f "/x")))

;; --- the java.nio.file Path shim's root ---------------------------------------
;; A Path's ROOT is the prefix that is not a segment. The shim assumed the POSIX
;; shape everywhere, so on Windows a drive path was a RELATIVE path whose first
;; segment happened to be "C:" — which is what made (fs/absolute? "C:/Windows")
;; answer false, getRoot answer nil, and getParent walk off the drive letter.
(define (root label windows? given want)
  (same label (npath-root-for windows? given) want))
(root "posix absolute root"      #f "/a/b"            "/")
(root "posix relative has none"  #f "a/b"             "")
(root "posix backslash is a name" #f "\\a\\b"           "")
(root "windows drive root"       #t "C:/a"            "C:/")
(root "windows drive root, backslash" #t "C:\\a"       "C:/")
(root "windows drive-relative"   #t "C:a"             "C:")
(root "windows current-drive rooted" #t "\\a"          "/")
(root "windows UNC root"         #t "\\\\srv\\sh\\a"    "//srv/sh/")
(root "windows relative has none" #t "a\\b"           "")

;; isAbsolute is not "has a root": "\x" and "C:x" are rooted but name nothing on
;; their own, so the JVM calls neither absolute.
(ok "windows drive path is absolute"      (npath-absolute-for? #t "C:/a"))
(ok "windows UNC path is absolute"        (npath-absolute-for? #t "\\\\srv\\sh\\a"))
(ok "windows current-drive rooted is not" (not (npath-absolute-for? #t "\\a")))
(ok "windows drive-relative is not"       (not (npath-absolute-for? #t "C:a")))
(ok "posix absolute is absolute"          (npath-absolute-for? #f "/a"))

;; segments never include the root
(same "windows drive segments"   (npath-segs-for #t "C:\\a\\b") '("a" "b"))
(same "windows UNC segments"     (npath-segs-for #t "\\\\srv\\sh\\a") '("a"))
(same "posix segments"           (npath-segs-for #f "/a/b") '("a" "b"))
(same "posix keeps a backslash in a name" (npath-segs-for #f "/a\\b") '("a\\b"))

;; getParent: the parent of the last segment under a root IS the root, and a
;; root has no parent. This used to answer "/" for every rooted path, so the
;; parent of "C:/a" was a directory on another drive.
(define (parent label windows? given want)
  (same label (npath-parent-for windows? given) want))
(parent "windows drive leaf"     #t "C:/a"      "C:/")
(parent "windows drive deeper"   #t "C:\\a\\b"   "C:/a")
(parent "windows drive root"     #t "C:/"       jolt-nil)
(parent "windows UNC leaf"       #t "\\\\srv\\sh\\a" "//srv/sh/")
(parent "windows relative leaf"  #t "a"         jolt-nil)
(parent "posix leaf"             #f "/a"        "/")
(parent "posix deeper"           #f "/a/b"      "/a")
(parent "posix root"             #f "/"         jolt-nil)
(parent "posix relative leaf"    #f "a"         jolt-nil)

;; normalize: ".." cannot climb above a root, but survives above a relative path
(define (norm label windows? given want)
  (same label (npath-normalize-for windows? given) want))
(norm "windows drive dotdot"     #t "C:/a/../b"   "C:/b")
(norm "windows drive dotdot past root" #t "C:/a/../.." "C:/")
(norm "windows backslash dot"    #t "C:\\a\\.\\b"   "C:/a/b")
(norm "windows UNC past root"    #t "\\\\srv\\sh\\a\\.." "//srv/sh/")
(norm "windows relative keeps dotdot" #t "a\\..\\..\\b" "../b")
(norm "posix dotdot"             #f "/a/../b"     "/b")
(norm "posix past root"          #f "/a/../.."    "/")
(norm "posix relative keeps dotdot" #f "a/../../b" "../b")
(norm "posix empty stays empty"  #f ""            "")

;; resolve: an absolute other replaces this; a Windows other that is rooted but
;; not absolute takes THIS path's root
(define (res label windows? a b want)
  (same label (npath-resolve-for windows? a b) want))
;; a native parent keeps its backslashes rather than gaining a mixed spelling
(res "windows relative child"    #t "C:\\Windows\\System32" "curl.exe" "C:\\Windows\\System32\\curl.exe")
(res "windows slash parent keeps slashes" #t "C:/Windows/System32" "curl.exe" "C:/Windows/System32/curl.exe")
(res "windows absolute other wins" #t "C:/a" "D:/b" "D:/b")
(res "windows rooted other takes this root" #t "C:/a" "\\b" "C:/b")
(res "windows rooted other, rootless this" #t "a" "\\b" "\\b")
(res "windows empty other"       #t "C:/a" "" "C:/a")
(res "posix relative child"      #f "/usr/bin" "curl" "/usr/bin/curl")
(res "posix absolute other wins" #f "/usr/bin" "/bin/sh" "/bin/sh")
(res "posix trailing separator not doubled" #f "/usr/bin/" "curl" "/usr/bin/curl")
;; on POSIX a leading backslash is an ordinary name, so it is a plain child
(res "posix backslash other is a child" #f "/usr" "\\b" "/usr/\\b")

;; startsWith compares ROOTS, not merely absoluteness — "C:/a" does not start
;; with "D:/" though both are rooted — and is spelling-independent, since the
;; root renders with "/" either way
(ok "windows same drive"          (npath-starts-with-for #t "C:/a/b" "C:\\a"))
(ok "windows different drive"     (not (npath-starts-with-for #t "C:/a" "D:/a")))
(ok "windows rooted vs relative"  (not (npath-starts-with-for #t "C:/a" "a")))
(ok "posix prefix"                (npath-starts-with-for #f "/a/b" "/a"))
(ok "posix absolute vs relative"  (not (npath-starts-with-for #f "/a/b" "a")))

;; --- the two-arg File constructor's resolve ----------------------------------
;; FileSystem.resolve(parent, child). Every POSIX row here is the JVM's own
;; answer, taken from a real JDK run, because this is the behaviour the rewrite
;; had to preserve exactly; the Windows rows are the ones a Linux runner cannot
;; reach. The old code asked (string=? p "/") — "is the parent the root", written
;; for the one platform that has a single root.
(define (join label windows? p c want)
  (same label (jolt-file-join-for windows? p c) want))
(join "posix child"                 #f "/a/b" "c"   "/a/b/c")
(join "posix rooted child"          #f "/a/b" "/c"  "/a/b/c")
(join "posix empty child"           #f "/a/b" ""    "/a/b")
(join "posix separator child"       #f "/a/b" "/"   "/a/b")
(join "posix root parent"           #f "/"    "c"   "/c")
(join "posix root parent, rooted child" #f "/" "/c" "/c")
(join "posix root parent, empty"    #f "/"    ""    "/")
(join "posix empty parent defaults to the root" #f "" "c" "/c")
(join "posix relative parent"       #f "a"    "b"   "a/b")
(join "posix relative parent, rooted child" #f "a" "/b" "a/b")
(join "posix nested child"          #f "/a"   "b/c" "/a/b/c")

;; Windows: a backslash is a separator too, and every root ends in one — so the
;; drive root joins without doubling, exactly as "/" does on POSIX.
(join "windows drive root parent"   #t "C:/"  "c"        "C:/c")
(join "windows drive root, rooted child" #t "C:/" "/c"   "C:/c")
(join "windows drive root, backslash child" #t "C:/" "\\c" "C:/c")
(join "windows drive parent"        #t "C:/a" "b"        "C:/a/b")
(join "windows drive parent, rooted child" #t "C:/a" "/b" "C:/a/b")
(join "windows native parent keeps backslashes" #t "C:\\a" "b" "C:\\a\\b")
(join "windows native parent, backslash child" #t "C:\\a" "\\b" "C:\\a\\b")
(join "windows UNC root parent"     #t "//srv/sh/" "c"   "//srv/sh/c")
(join "windows UNC parent"          #t "//srv/sh/a" "b"  "//srv/sh/a/b")
(join "windows separator child alone" #t "C:/a" "\\"   "C:/a")
(join "windows empty child"         #t "C:/a" ""         "C:/a")
;; on POSIX a backslash is an ordinary character, so it is a plain child name
;; and the join still adds a "/"
(join "posix backslash child is a name" #f "/a" "\\b"  "/a/\\b")

;; --- as-relative-path asks .isAbsolute ---------------------------------------
;; io/file puts every child through it, so a wrong answer either rejects a legal
;; call or silently joins an absolute path onto a parent.
(define (rel label windows? p absolute?)
  (ok label (eq? (jfile-path-absolute-for? windows? p) absolute?)))
(rel "posix rooted child is rejected"      #f "/c"  #t)
(rel "posix relative child is kept"        #f "c"   #f)
(rel "posix nested relative child is kept" #f "a/b" #f)
;; the two Windows rows the old leading-"/" test got backwards, both ways round
(rel "windows drive child IS absolute"     #t "C:/c" #t)
(rel "windows current-drive child is NOT"  #t "/c"   #f)
(rel "windows UNC child IS absolute"       #t "//srv/sh/c" #t)
(rel "windows relative child is kept"      #t "c"    #f)

;; --- java.io.tmpdir ----------------------------------------------------------
;; TMPDIR is the POSIX spelling; Windows sets TEMP and TMP and not TMPDIR, so
;; the old chain answered "/tmp" on a drive nobody chose.
(define (env-from alist) (lambda (k) (cond ((assoc k alist) => cdr) (else #f))))
(same "posix default" (host-temp-dir-for #f (env-from '())) "/tmp")
(same "posix honors TMPDIR" (host-temp-dir-for #f (env-from '(("TMPDIR" . "/scratch")))) "/scratch")
(same "posix ignores TEMP" (host-temp-dir-for #f (env-from '(("TEMP" . "C:/t")))) "/tmp")
(same "windows honors TMPDIR first"
      (host-temp-dir-for #t (env-from '(("TMPDIR" . "T:/x") ("TEMP" . "C:/t")))) "T:/x")
(same "windows takes TEMP" (host-temp-dir-for #t (env-from '(("TEMP" . "C:/Users/x/Temp")))) "C:/Users/x/Temp")
(same "windows falls back to TMP" (host-temp-dir-for #t (env-from '(("TMP" . "C:/t2")))) "C:/t2")
(same "windows derives one from SystemRoot"
      (host-temp-dir-for #t (env-from '(("SystemRoot" . "D:/Windows")))) "D:/Windows/Temp")
(same "windows last resort" (host-temp-dir-for #t (env-from '())) "C:/Windows/Temp")
;; an empty value is not a value
(same "an empty TMPDIR is ignored" (host-temp-dir-for #f (env-from '(("TMPDIR" . "")))) "/tmp")

;; --- File's trailing-separator normalization ---------------------------------
;; Every java.io.File is built through jolt-path-normalize, which drops a
;; trailing separator — except the one that IS the root. "/" was already
;; protected by a length test; the Windows drive root "C:/" was not, so it
;; normalized to "C:", the drive's CURRENT DIRECTORY, a different file. That is
;; how File/listRoots came back drive-relative on the Windows runner even after
;; it started enumerating drives (#1074). Driven through the File constructor
;; because the invariant belongs to every construction site, not to one helper.
(define (norm-path label windows? given want)
  (same label (jolt-path-normalize-for windows? given) want))
;; POSIX rows first: none of this may move, and the only POSIX path whose
;; trailing separator is its root is "/" itself.
(norm-path "posix root survives"            #f "/"        "/")
(norm-path "posix trailing separator goes"  #f "/a/"      "/a")
(norm-path "posix deeper trailing goes"     #f "/a/b/"    "/a/b")
(norm-path "posix doubled separator folds"  #f "/a//b"    "/a/b")
(norm-path "posix doubled and trailing"     #f "/a//b//"  "/a/b")
(norm-path "posix relative untouched"       #f "a/b"      "a/b")
(norm-path "posix bare name untouched"      #f "a"        "a")
;; a drive letter means nothing on POSIX, so "C:/" is an ordinary relative name
;; whose trailing separator goes
(norm-path "posix has no drive root"        #f "C:/"      "C:")

;; Windows: the drive root and the UNC root keep their separator; everything
;; below them loses it.
(norm-path "windows drive root survives"    #t "C:/"       "C:/")
(norm-path "windows drive child trims"      #t "C:/a/"     "C:/a")
(norm-path "windows deeper child trims"     #t "C:/a/b/"   "C:/a/b")
(norm-path "windows drive root, doubled"    #t "C://"      "C:/")
(norm-path "windows current-drive root"     #t "/"         "/")
;; The drive root and the UNC root are NOT symmetric here, and that is the JVM's
;; asymmetry rather than an accident: java.io.File keeps "C:\\" whole because the
;; separator is what makes it absolute rather than drive-relative, while
;; "\\\\srv\\sh\\" normalizes to "\\\\srv\\sh" — the share IS the root, and the
;; trailing separator adds nothing. path-root-end encodes exactly that: it counts
;; the separator into a drive root and leaves it out of a UNC one. (The
;; java.nio.file Path shim answers "//srv/sh/" for getRoot, WITH the separator,
;; because that is what Path.getRoot does — a different API with a different
;; convention, pinned separately above.)
(norm-path "windows UNC root drops its trailing sep" #t "//srv/sh/" "//srv/sh")
(norm-path "windows UNC root already bare"  #t "//srv/sh"  "//srv/sh")
(norm-path "windows UNC child trims"        #t "//srv/sh/a/" "//srv/sh/a")
(norm-path "windows UNC keeps its leading pair" #t "//srv/sh/a" "//srv/sh/a")
(norm-path "windows drive-relative kept"    #t "C:a/"      "C:a")
(norm-path "windows doubled separator folds" #t "C:/a//b"  "C:/a/b")
;; and the File constructor carries the same invariant, since that is where
;; every construction site goes through
(same "the File constructor keeps a drive root"
      (jolt-path-normalize-for #t "C:/") "C:/")
(same "a File on this host still normalizes"
      (jfile-path (make-jfile "/a/b/")) "/a/b")

;; --- the native spelling (jolt-lang/jolt#1110) -------------------------------
;; A File or Path is HELD with "/" on Windows, however the caller spelled it, and
;; RENDERED with "\\" — str, toString, getPath and the rest go through
;; path-native — so File/separator, file.separator and every rendered path say
;; the same thing, as they do on the JDK. POSIX holds and renders the same string.
(norm-path "windows backslashes are held as /"   #t "C:\\a\\b\\"   "C:/a/b")
(norm-path "windows mixed and doubled separators" #t "C:\\a/\\b"    "C:/a/b")
(norm-path "windows UNC in backslashes"          #t "\\\\srv\\sh\\a" "//srv/sh/a")
(norm-path "windows relative in backslashes"     #t "sub\\deep.clj"  "sub/deep.clj")
(norm-path "posix backslash stays a name char"   #f "a\\b"           "a\\b")
(same "windows renders a drive path natively" (path-native-for #t "C:/a/b") "C:\\a\\b")
(same "windows renders a UNC path natively" (path-native-for #t "//srv/sh/a") "\\\\srv\\sh\\a")
(same "windows renders a relative path natively" (path-native-for #t "sub/deep.clj") "sub\\deep.clj")
(same "windows renders the empty path as itself" (path-native-for #t "") "")
(same "posix renders what it holds" (path-native-for #f "/a/b\\c") "/a/b\\c")
(same "the file separator is \\ on windows" (file-separator-for #t) "\\")
(same "the file separator is / on posix" (file-separator-for #f) "/")
;; the relativize #1110 measured: babashka answers "..\\x\\b.clj" there
(same "windows relativize renders natively"
      (path-native-for #t (npath-relativize-for #t "C:/tmp/a/b.clj" "C:/tmp/a/x/b.clj")) "..\\x\\b.clj")
;; A Path keeps a lone UNC root's trailing separator where File drops it
;; (WindowsPathParser vs WinNTFileSystem); anything below the root trims.
(same "a windows Path keeps a UNC root's separator" (npath-held-for #t "\\\\srv\\sh") "//srv/sh/")
(same "a windows Path keeps a UNC root's separator, given" (npath-held-for #t "//srv/sh/") "//srv/sh/")
(same "a windows Path trims below a UNC root" (npath-held-for #t "//srv/sh/a/") "//srv/sh/a")
(same "a windows Path trims below a drive" (npath-held-for #t "C:\\a\\") "C:/a")
(same "a posix Path collapses and trims like File" (npath-held-for #f "a//b/") "a/b")
(same "a posix Path keeps the root" (npath-held-for #f "/") "/")
(same "the empty Path stays empty" (npath-held-for #t "") "")

;; --- File/listRoots ----------------------------------------------------------
;; One root on POSIX; one per mounted drive on Windows, where "/" named a
;; directory on whichever drive the process was on and enumerated nothing.
(same "posix has one root" (file-list-roots-for #f (lambda (_) #t)) '("/"))
(same "windows enumerates the mounted drives"
      (file-list-roots-for #t (lambda (p) (member p '("C:/" "D:/" "Z:/"))))
      '("C:/" "D:/" "Z:/"))
(same "windows never answers empty"
      (file-list-roots-for #t (lambda (_) #f)) '("C:/"))
(same "windows probes every letter"
      (length (file-list-roots-for #t (lambda (_) #t))) 26)

;; --- the parsed path agrees with the two readers it replaces (jolt-2sp) -------
;; path-parse exists so a helper reads a path's root and segments from ONE scan
;; instead of asking path-root and path-segments separately, each of which
;; begins with its own path-root-end. That is only safe while the parse says
;; exactly what those two say, so this pins the equivalence directly rather than
;; inferring it from the callers passing: every shape the root scan distinguishes
;; — POSIX, drive, drive-relative, UNC, current-drive-rooted, relative — on both
;; platforms, including the ones whose roots the callers below never build.
(define (parse-agrees label windows? given)
  (let ((pp (path-parse windows? given)))
    (same (string-append label " / root") (ppath-root pp) (path-root windows? given))
    (same (string-append label " / segs") (ppath-segs pp) (path-segments windows? given))
    (same (string-append label " / rooted?")
          (ppath-rooted? pp) (not (string=? (path-root windows? given) "")))))
(parse-agrees "posix absolute"        #f "/a/b/c")
(parse-agrees "posix relative"        #f "a/b")
(parse-agrees "posix root itself"     #f "/")
(parse-agrees "posix empty"           #f "")
(parse-agrees "posix doubled sep"     #f "//a//b")
(parse-agrees "posix backslash name"  #f "\\a\\b")
(parse-agrees "posix dots"            #f "/a/./b/../c")
(parse-agrees "win drive"             #t "C:/a/b")
(parse-agrees "win drive backslash"   #t "C:\\a\\b")
(parse-agrees "win drive root only"   #t "C:/")
(parse-agrees "win drive-relative"    #t "C:a/b")
(parse-agrees "win unc"               #t "//srv/sh/a")
(parse-agrees "win unc root only"     #t "//srv/sh/")
(parse-agrees "win unc backslash"     #t "\\\\srv\\sh\\a")
(parse-agrees "win current-drive"     #t "\\a\\b")
(parse-agrees "win relative"          #t "a\\b")
(parse-agrees "win empty"             #t "")
(parse-agrees "win dots"              #t "C:/a/./b/../c")

;; ppath-render is path-rebuild over the parse — the third of the three calls a
;; helper used to make. Rendering a parse back unchanged is the identity on any
;; already-normal path, which is what makes "parse, transform segments, render"
;; a safe replacement for the string-in/string-out shape.
(define (render-roundtrip label windows? given want)
  (let ((pp (path-parse windows? given)))
    (same label (ppath-render pp (ppath-segs pp)) want)))
(render-roundtrip "posix roundtrip"   #f "/a/b"        "/a/b")
(render-roundtrip "posix rel"         #f "a/b"         "a/b")
(render-roundtrip "win drive"         #t "C:/a/b"      "C:/a/b")
(render-roundtrip "win drive bs"      #t "C:\\a\\b"    "C:/a/b")
(render-roundtrip "win unc"           #t "//srv/sh/a"  "//srv/sh/a")
(render-roundtrip "win current-drive" #t "\\a\\b"      "/a/b")

;; --- rename-replace! on this host --------------------------------------------
;; The Windows branch needs a real Windows filesystem and is gated on the
;; Windows runner. What is checked here is that adding it did not cost POSIX its
;; atomic replace: the destination is replaced in one step, with no window in
;; which it is missing, and a rename onto a directory still fails loudly.
(let* ((d (string-append "target/win-platform-test-" (number->string (sa-real-time-ms))))
       (src (string-append d "/src")) (dst (string-append d "/dst")))
  (mkdirs! d)
  (let ((out (open-output-file src 'replace))) (put-string out "new") (close-output-port out))
  (let ((out (open-output-file dst 'replace))) (put-string out "old") (close-output-port out))
  (rename-replace! src dst)
  (ok "the destination is replaced" (file-exists? dst))
  (ok "the source is gone" (not (file-exists? src)))
  (same "the destination holds the new content"
        (let* ((in (open-input-file dst)) (s (get-string-all in))) (close-input-port in) s)
        "new")
  ;; a rename onto a non-empty directory is an error on both platforms, and the
  ;; Windows pre-delete must not turn it into a silent success
  (let ((sub (string-append d "/sub")))
    (mkdirs! sub)
    (let ((out (open-output-file (string-append sub "/keep") 'replace)))
      (put-string out "k") (close-output-port out))
    (let ((out (open-output-file src 'replace))) (put-string out "n") (close-output-port out))
    (ok "renaming over a non-empty directory still fails"
        (guard (e (#t #t)) (rename-replace! src sub) #f))
    (ok "the directory's contents survive" (file-exists? (string-append sub "/keep")))
    (delete-file (string-append sub "/keep") #f)
    (delete-path! sub))
  (delete-file src #f)
  (delete-file dst #f)
  (delete-path! d))

;; --- the Windows AV retry (jolt-lang/jolt#1263) ------------------------------
;; A scanner holding the freshly closed temp file is Windows-only, so the retry
;; loop rename-replace! wraps that rename in is exercised directly here: a
;; transient failure is waited out, a persistent one still raises the ORIGINAL
;; error once the delays run out, and a failure retry? rejects is not retried.
(let ((calls 0))
  (same "a transient failure is retried until it succeeds"
        (call-with-fs-retry
         (lambda () (set! calls (+ calls 1))
                 (if (< calls 3) (error 'rename-file "permission denied") 'renamed))
         (lambda (e) #t))
        'renamed)
  (same "it took exactly the failing attempts plus one" calls 3))
(let ((calls 0))
  (ok "a persistent failure rethrows its own error"
      (guard (e ((error? e) (string=? (condition-message e) "still locked")))
        (call-with-fs-retry (lambda () (set! calls (+ calls 1)) (error 'rename-file "still locked"))
                            (lambda (e) #t))
        #f))
  (same "a persistent failure is tried once per delay, then once more"
        calls (+ 1 (length fs-retry-delays-ms))))
(let ((calls 0))
  (ok "a failure retry? rejects raises on the first attempt"
      (guard (e (#t (= calls 1)))
        (call-with-fs-retry (lambda () (set! calls (+ calls 1)) (error 'rename-file "is a directory"))
                            (lambda (e) #f))
        #f)))
(ok "the retry budget stays around a second"
    (<= (apply + fs-retry-delays-ms) 1000))

;; --- the Windows command line (jolt-lang/jolt#1108) --------------------------
;; Where posix_spawn is missing, every spawn used to go to Chez's
;; open-process-ports carrying proc-build-shell-command's /bin/sh string — `exec `,
;; `cd 'DIR' &&`, `env -i K=V …`, sh quoting — which on Windows reaches cmd.exe.
;; cmd stops at the first token, so EVERY spawn failed, and failed with exit 0 and
;; an empty stdout: only stderr said anything. The fix is a real CreateProcessW
;; spawn with no shell on either side, and this is the half of it a POSIX runner
;; can check — the one string CreateProcessW takes, built by the JDK's rules
;; (ProcessImpl.createCommandLine in VERIFICATION_LEGACY, the mode a plain
;; ProcessBuilder.start() takes there).

(define (cmdline label argv want)
  (let ((got (proc-win-command-line argv)))
    (set! total (+ total 1))
    (unless (string=? got want)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a: got ~s, want ~s\n" label got want))))

;; the two repros from the issue, which used to produce "exec 'git' '--version'"
;; and "exec 'cmd' '/c' 'echo' 'hi'"
(cmdline "bare program and flag" '("git" "--version") "git --version")
(cmdline "cmd /c passthrough" '("cmd" "/c" "echo" "hi") "cmd /c echo hi")

;; A program path takes the native spelling — `new File(cmd[0]).getPath()` is the
;; JDK's first move and rewrites "/" to "\" on Windows. A bare name has no
;; separator and is untouched.
(same "program spelling rewrites /" (proc-win-program-spelling "C:/Windows/System32/curl.exe")
      "C:\\Windows\\System32\\curl.exe")
(same "program spelling leaves a bare name" (proc-win-program-spelling "curl") "curl")
(same "program spelling leaves backslashes" (proc-win-program-spelling "C:\\bin\\x.exe") "C:\\bin\\x.exe")
(cmdline "drive-absolute program" '("C:/Windows/System32/curl.exe" "-s" "http://x/")
         "C:\\Windows\\System32\\curl.exe -s http://x/")
(cmdline "relative program" '("./tool" "-v") ".\\tool -v")

;; A program whose path holds a space is quoted, so CreateProcessW's own parse
;; does not stop at the space and look for "C:\Program.exe".
(cmdline "program with a space is quoted" '("C:/Program Files/Git/bin/git.exe" "status")
         "\"C:\\Program Files\\Git\\bin\\git.exe\" status")

;; --- which ARGUMENTS get quoted ---------------------------------------------
;; LEGACY's escape set is exactly {space, tab}. An empty argument must be quoted
;; or it disappears; an argument the caller already quoted is passed through.
(ok "empty argument needs escaping"     (proc-win-needs-escaping? ""))
(ok "space needs escaping"              (proc-win-needs-escaping? "a b"))
(ok "tab needs escaping"                (proc-win-needs-escaping? "a\tb"))
(ok "plain argument does not"           (not (proc-win-needs-escaping? "abc")))
(ok "already-quoted is passed through"  (not (proc-win-needs-escaping? "\"a b\"")))
;; cmd's metacharacters are NOT escaped: no command processor is involved, so
;; they are ordinary bytes on the way to the child's own argv.
(ok "& is not a metacharacter here"     (not (proc-win-needs-escaping? "a&b")))
(ok "| is not a metacharacter here"     (not (proc-win-needs-escaping? "a|b")))
(ok "> is not a metacharacter here"     (not (proc-win-needs-escaping? "a>b")))

(cmdline "argument with a space"  '("echo" "a b")   "echo \"a b\"")
(cmdline "empty argument"         '("prog" "")      "prog \"\"")
(cmdline "pre-quoted argument"    '("prog" "\"a b\"") "prog \"a b\"")
(cmdline "metacharacters ride through" '("echo" "a&b|c>d") "echo a&b|c>d")

;; A run of backslashes immediately before the closing quote would escape it —
;; msvcrt reads \" as a literal quote — so the run is doubled. This is why
;; "C:\my dir\" cannot be quoted naively.
(same "count trailing backslashes (none)" (proc-win-count-leading-backslash "abc" 3) 0)
(same "count trailing backslashes (one)"  (proc-win-count-leading-backslash "a\\" 2) 1)
(same "count trailing backslashes (two)"  (proc-win-count-leading-backslash "a\\\\" 3) 2)
(cmdline "trailing backslash in a quoted argument"
         '("prog" "C:\\my dir\\") "prog \"C:\\my dir\\\\\"")
;; ...and an argument that needs no quotes keeps its backslashes as they are
(cmdline "trailing backslash with no space" '("prog" "C:\\dir\\") "prog C:\\dir\\")

;; --- the environment block ---------------------------------------------------
;; CreateProcessW takes "K=V\0K=V\0…", sorted case-insensitively by name — which
;; Windows requires and ProcessEnvironment.toEnvironmentBlock produces. This is
;; what replaces `env -i K=V …`: there is no env program on Windows, and the block
;; is exact, which is the semantics the sh prefix was reaching for.
(define (envblock label pairs want . root)
  (let ((got (proc-win-env-entries pairs (if (null? root) #f (car root)))))
    (set! total (+ total 1))
    (unless (string=? got want)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a: got ~s, want ~s\n" label got want))))

(define NUL (string #\nul))
(envblock "one entry" '(("PATH" . "C:\\bin")) (string-append "PATH=C:\\bin" NUL))
(envblock "sorted case-insensitively by name"
          '(("zeta" . "1") ("ALPHA" . "2") ("Beta" . "3"))
          (string-append "ALPHA=2" NUL "Beta=3" NUL "zeta=1" NUL))
;; an empty environment is still a block, not a null pointer
(envblock "empty environment" '() NUL)
;; SystemRoot rides along from the parent, in its sorted place...
(envblock "SystemRoot is added in sorted order"
          '(("zeta" . "1") ("ALPHA" . "2"))
          (string-append "ALPHA=2" NUL "SystemRoot=C:\\Windows" NUL "zeta=1" NUL)
          "C:\\Windows")
;; ...unless the caller already set it, in any case
(envblock "an explicit SystemRoot wins"
          '(("systemroot" . "D:\\W"))
          (string-append "systemroot=D:\\W" NUL)
          "C:\\Windows")
(envblock "SystemRoot alone fills an empty environment" '()
          (string-append "SystemRoot=C:\\Windows" NUL) "C:\\Windows")
;; ...in NameComparator's order, which upper-cases: `_` (0x5F) sorts after Z
;; (0x5A), where a lower-cased comparison put it before a (0x61)
(envblock "an underscore sorts after Z, as Windows canonicalizes upward"
          '(("_JAVA_OPTIONS" . "1") ("ZETA" . "2") ("alpha" . "3"))
          (string-append "alpha=3" NUL "ZETA=2" NUL "_JAVA_OPTIONS=1" NUL))
(envblock "a shorter name sorts first when one prefixes the other"
          '(("PATHEXT" . "1") ("Path" . "2"))
          (string-append "Path=2" NUL "PATHEXT=1" NUL))
;; The MAP is case-sensitive, as the JDK's is: ProcessBuilder.environment() is a
;; clone of a HashMap there too, and only System.getenv(String) ignores case. So
;; a "PATH" put beside an inherited "Path" is a second entry on the JVM as well.
(same "the environment map keeps PATH and Path apart, as the JDK's HashMap does"
      (let ((em (make-proc-env-from-strings (jolt-vector "Path=C:\\old"))))
        (hashtable-set! (jhost-state em) "PATH" "C:\\new")   ; what .put does
        (length (proc-env-map-pairs em)))
      2)

;; --- STARTUPINFOW / PROCESS_INFORMATION layout -------------------------------
;; Derived from the pointer width rather than hardcoded, so the same formulas
;; serve x64 and x86. The runner is 64-bit, where the documented sizes are 104
;; and 24 — if the derivation were wrong, CreateProcessW would read the std
;; handles out of the padding and the child would get none of them.
(when (= 8 (sa-foreign-sizeof 'void*))
  (same "STARTUPINFOW size on x64"     proc-win-si-size 104)
  (same "STARTUPINFOW dwFlags offset"  proc-win-si-flags-off 60)
  (same "STARTUPINFOW hStdInput"       proc-win-si-stdin-off 80)
  (same "STARTUPINFOW hStdOutput"      proc-win-si-stdout-off 88)
  (same "STARTUPINFOW hStdError"       proc-win-si-stderr-off 96)
  (same "PROCESS_INFORMATION size"     proc-win-pi-size 24)
  ;; (HANDLE)-1 arrives through Chez's void* as an unsigned address, so the
  ;; INVALID_HANDLE_VALUE test has to be all-ones at the pointer width — against
  ;; -1 it would never match and every failed CreateFileW would look usable.
  (same "INVALID_HANDLE_VALUE is all ones" proc-win-INVALID-HANDLE #xFFFFFFFFFFFFFFFF)
  (ok "INVALID_HANDLE_VALUE is not a usable handle"
      (not (proc-win-handle-ok? proc-win-INVALID-HANDLE)))
  (ok "NULL is not a usable handle" (not (proc-win-handle-ok? 0)))
  (ok "a real address is a usable handle" (proc-win-handle-ok? 12345)))

;; Nothing above resolved a Win32 entry point: this is a POSIX runner, and the
;; whole surface is gated on the machine type. If any of it had tried, the
;; accessors would answer #f rather than raising — which is also what a Windows
;; host missing an entry gets, and what proc-win-spawn-ok? turns into a loud
;; IOException instead of the silent exit-0 that started this. On the Windows
;; runner the same rows flip: every entry must resolve there.
(if (eq? (sa-os-family) 'windows)
    (begin
      (ok "every Win32 entry resolves on a Windows host" (proc-win-spawn-ok?))
      (ok "proc-win? is true on a Windows host" proc-win?))
    (begin
      (ok "no Win32 entry resolves on a POSIX host" (not (proc-win-spawn-ok?)))
      (ok "proc-win? is false on a POSIX host" (not proc-win?))))

;; --- file: URLs, both directions (jolt-lang/jolt#1118) -------------------------
;; File.toURI on Windows rendered "file:C:%5CUsers%5C…": the separators were
;; percent-encoded and the drive had no "/" in front, so jolt could not open the
;; URL its own toURI produced, and the JDK's spellings "file:/C:/…" and
;; "file:///C:/…" were refused as "/C:/…" paths. Whatever toURI emits has to
;; come back through the opener as the same path.
(same "uri path: drive, backslashes"   (file-uri-path-for #t "C:\\Users\\x\\a.txt") "/C:/Users/x/a.txt")
(same "uri path: drive, slashes"       (file-uri-path-for #t "C:/Users/x/a.txt")    "/C:/Users/x/a.txt")
(same "uri path: UNC keeps its host"   (file-uri-path-for #t "\\\\srv\\sh\\a")      "////srv/sh/a")
(same "uri path: posix unchanged"      (file-uri-path-for #f "/a/b\\c")             "/a/b\\c")

;; Path.toUri is not File.toURI (jolt-lang/jolt#1467): the authority is written,
;; empty for a drive and the host for UNC (WindowsUriSupport), and on POSIX every
;; byte outside the RFC 2396 path set is escaped (UnixUriUtils), non-ASCII too.
(same "path uri: drive"                (nio-path-uri-spec-for #t "C:\\Users\\x\\a b.txt") "file:///C:/Users/x/a%20b.txt")
(same "path uri: UNC host is authority" (nio-path-uri-spec-for #t "\\\\srv\\sh\\a")       "file://srv/sh/a")
(same "path uri: posix escapes bytes"  (nio-path-uri-spec-for #f "/x/\x00FC; [a].clj")  "file:///x/%C3%BC%20%5Ba%5D.clj")

(same "url->path: JDK spelling"        (file-url->path-for #t "file:/C:/Users/x/a.txt")   "C:/Users/x/a.txt")
(same "url->path: empty authority"     (file-url->path-for #t "file:///C:/Users/x/a.txt") "C:/Users/x/a.txt")
(same "url->path: localhost authority" (file-url->path-for #t "file://localhost/C:/a")    "C:/a")
(same "url->path: bare drive"          (file-url->path-for #t "file:C:/a/b")              "C:/a/b")
(same "url->path: raw backslashes"     (file-url->path-for #t "file:C:\\a\\b")            "C:\\a\\b")
(same "url->path: escaped separators"  (file-url->path-for #t "file:C:%5CUsers%5Cx")      "C:\\Users\\x")
(same "url->path: escaped space"       (file-url->path-for #t "file:/C:/has%20space/x")   "C:/has space/x")
(same "url->path: UNC host"            (file-url->path-for #t "file://srv/sh/a")          "//srv/sh/a")
(same "url->path: drive root"          (file-url->path-for #t "file:/C:/")                "C:/")
(same "url->path: a stray % is literal" (file-url->path-for #t "file:/C:/100%/x")         "C:/100%/x")
;; what jolt.loader opens a resource hit through now (jolt-lang/jolt#1203): it
;; dropped the scheme with (subs url 5), leaving "/C:/proj/…", which Windows
;; resolved to "\C:\proj\…" on the current drive
(same "url->path: a loader resource hit" (file-url->path-for #t "file:/C:/proj/src/res/x.txt") "C:/proj/src/res/x.txt")
(same "url->path: a hit under an escaped root" (file-url->path-for #t "file:/C:/my%20proj/res/x.txt") "C:/my proj/res/x.txt")

;; new File(URI) / Path.of(URI) (jolt-lang/jolt#1198) share the drive rule with
;; the URL reader: the URI's "/C:/..." path is the Windows path "C:/...".
(same "uri->file: drive"               (uri->file-path-for #t (uri-parse "file:///C:/Users/x/a.txt")) "C:/Users/x/a.txt")
(same "uri->file: escaped space"       (uri->file-path-for #t (uri-parse "file:/C:/has%20space/x"))   "C:/has space/x")
(same "uri->file: drive root"          (uri->file-path-for #t (uri-parse "file:///C:/"))              "C:/")
(same "uri->file: posix keeps /C:"     (uri->file-path-for #f (uri-parse "file:///C:/x"))             "/C:/x")
(same "url->path: utf-8 escapes"       (file-url->path-for #f "file:/a/%C3%A4")           "/a/\x00e4;")
;; POSIX keeps the leading "/", and a path whose first segment merely LOOKS like
;; a drive is still a POSIX path
(same "url->path: posix"               (file-url->path-for #f "file:/a/b")                "/a/b")
(same "url->path: posix empty authority" (file-url->path-for #f "file:///a/b")            "/a/b")
(same "url->path: posix /C: is a name" (file-url->path-for #f "file:/C:/a")               "/C:/a")
(same "url->path: relative"            (file-url->path-for #f "file:a/b")                 "a/b")
;; the round trip toURI's spelling has to survive, per platform
(for-each
  (lambda (w? path want)
    (same (format "round trip ~s (windows? ~s)" path w?)
          (file-url->path-for w? (string-append "file:" (uri-quote-path (file-uri-path-for w? path))))
          want))
  '(#t #t #f)
  '("C:\\Users\\has space\\x.txt" "C:/100%/x" "/tmp/has space/x.txt")
  '("C:/Users/has space/x.txt" "C:/100%/x" "/tmp/has space/x.txt"))

;; --- a LIVE spawn, on whichever host is running -------------------------------
;; Everything above is a table. This is the part that would actually have caught
;; jolt-lang/jolt#1108: it starts a real child through the real ProcessBuilder, so
;; on the Windows runner it drives CreateProcessW end to end, and on a POSIX one
;; it drives posix_spawn. The failure it is written against is the one the issue
;; describes — a spawn that "succeeds" with exit 0 and an empty stdout — which no
;; assertion on the exit code alone can see.
;;
;; The windows-deps job runs `make winplatform` for exactly this reason: before
;; it, that job ran only `make depsunit mvnhttp`, and jolt.mvn-http initializes
;; its own Winsock and spawns nothing, so the broken paths were never touched.

(define (pdispatch o m . args)
  (record-method-dispatch o m (if (null? args) jolt-nil (apply jolt-list args))))

;; The child's whole stdout (or stderr) as a string, read to EOF.
(define (drain-stream is)
  (let loop ((acc '()))
    (let ((b (jnum->exact (pdispatch is "read"))))
      (if (< b 0)
          (utf8->string (u8-list->bytevector (reverse acc)))
          (loop (cons b acc))))))

;; Windows line endings are the shell's, not the test's.
(define (strip-cr s)
  (list->string (filter (lambda (c) (not (char=? c #\return))) (string->list s))))

;; -> (values stdout stderr exit-code)
(define (run-child argv)
  (let* ((pb (host-new "ProcessBuilder" (apply jolt-vector argv)))
         (p  (pdispatch pb "start"))
         (o  (drain-stream (pdispatch p "getInputStream")))
         (e  (drain-stream (pdispatch p "getErrorStream")))
         (rc (jnum->exact (pdispatch p "waitFor"))))
    (values (strip-cr o) (strip-cr e) rc)))

(define live-windows? (eq? (sa-os-family) 'windows))

;; One trivial program per platform. cmd.exe is the Windows one precisely because
;; it is what the old sh string was being handed to — `cmd /c echo ok` printed
;; nothing at all and exited 0 before the CreateProcessW path.
;; The POSIX one is "sh", found on PATH, not "/bin/sh": termux-docker has no /bin
;; at all (termux-exec rewrites a /bin/sh exec, but the resolver stats the
;; absolute name first and reports it missing), and Termux's sh is on PATH.
(define echo-argv    (if live-windows? '("cmd" "/c" "echo" "ok")        '("sh" "-c" "echo ok")))
;; The redirect goes first on Windows: cmd echoes everything up to the operator,
;; so `echo err 1>&2` writes "err " with the space.
(define stderr-argv  (if live-windows? '("cmd" "/c" "1>&2" "echo" "err") '("sh" "-c" "echo err 1>&2")))
(define exit3-argv   (if live-windows? '("cmd" "/c" "exit" "3")          '("sh" "-c" "exit 3")))
;; An argument holding a space must arrive as ONE argument. On Windows the
;; command-line builder quotes it, so cmd's echo prints the quotes back —
;; which is the observable difference from it having been split into two.
(define spaced-argv  (if live-windows? '("cmd" "/c" "echo" "a b")        '("sh" "-c" "echo $#" "sh" "a b")))
(define spaced-want  (if live-windows? "\"a b\"\n" "1\n"))

(call-with-values (lambda () (run-child echo-argv))
  (lambda (o e rc)
    ;; the whole bug in one row: stdout was empty and the status said success
    (same "live spawn: stdout"    o "ok\n")
    (same "live spawn: stderr"    e "")
    (same "live spawn: exit code" rc 0)))

(call-with-values (lambda () (run-child stderr-argv))
  (lambda (o e rc)
    (same "live spawn: stderr is its own stream" e "err\n")
    (same "live spawn: stdout stays empty"       o "")
    (same "live spawn: exit code with stderr"    rc 0)))

(call-with-values (lambda () (run-child exit3-argv))
  (lambda (o e rc)
    (same "live spawn: a nonzero exit is reported" rc 3)))

(call-with-values (lambda () (run-child spaced-argv))
  (lambda (o e rc)
    (same "live spawn: an argument with a space stays one argument" o spaced-want)))

;; Two in a row: the child's pipe ends have to be closed on this side after each
;; spawn, or the second child inherits the first's and neither read ever reaches
;; EOF — which would hang here rather than fail.
(call-with-values (lambda () (run-child echo-argv))
  (lambda (o e rc)
    (same "live spawn: a second child is independent" o "ok\n")
    (same "live spawn: ...and exits cleanly"          rc 0)))

;; A program that cannot be resolved is refused before any spawn, with the
;; JVM's message — this is #1074's guarantee, re-checked live because the
;; Windows resolver (PATHEXT, drive-rooted paths) only runs on that host.
(ok "live spawn: an unresolvable program raises"
    (guard (e (#t #t))
      (run-child '("jolt-no-such-program-anywhere"))
      #f))

;; FILETIME <-> epoch ns (java/io.ss). 116444736000000000 is 1970-01-01 in 100ns
;; ticks since 1601; the three times GetFileAttributesEx / GetFileTime read come
;; back through filetime->unix-ns, and every set goes out through
;; unix-ns->filetime, so a FileTime keeps the FILETIME's 100ns resolution.
(ok "the Unix epoch is FILETIME 116444736000000000"
    (= 116444736000000000 (unix-ns->filetime 0)))
(ok "filetime->unix-ns inverts unix-ns->filetime at 100ns"
    (andmap (lambda (ns) (= ns (filetime->unix-ns (unix-ns->filetime ns))))
            '(0 100 1100000000250000000 1600000000123456700 -100 -11644473600000000000)))
(ok "a FILETIME tick is 100ns, so the last two digits of a nanosecond count go"
    (= 1600000000123456700 (filetime->unix-ns (unix-ns->filetime 1600000000123456789))))

;; Following a symbolic link for a time. CreateFileW follows a link unless
;; FILE_FLAG_OPEN_REPARSE_POINT is passed, and GetFileAttributesExW never does —
;; so NOFOLLOW needs the flag on a set, and FOLLOW needs a handle on a read of a
;; reparse point. Both were the other way round.
(ok "a FOLLOW open does not ask for the reparse point"
    (= 0 (bitwise-and (win32-attr-open-flags #t) win32-FILE-FLAG-OPEN-REPARSE-POINT)))
(ok "a NOFOLLOW open asks for the reparse point, and can still open a directory"
    (= (win32-attr-open-flags #f)
       (bitwise-ior win32-FILE-FLAG-OPEN-REPARSE-POINT win32-FILE-FLAG-BACKUP-SEMANTICS)))
(ok "a FOLLOW read of a reparse point goes through a handle"
    (win32-times-need-handle? (bitwise-ior #x20 win32-FILE-ATTRIBUTE-REPARSE-POINT) #t))
(ok "a NOFOLLOW read of one, and any read of a plain file, does not"
    (and (not (win32-times-need-handle? win32-FILE-ATTRIBUTE-REPARSE-POINT #f))
         (not (win32-times-need-handle? #x20 #t))))

;; A FileSystemException's message renders its PATHS natively and leaves the
;; reason alone: strerror's "Input/output error" is not a path.
(same "windows fs message flips only the paths"
      (fs-exception-message-for #t "C:/a/b" "C:/c" "Input/output error")
      "C:\\a\\b -> C:\\c: Input/output error")
(same "posix fs message"
      (fs-exception-message-for #f "a/b" #f "Input/output error")
      "a/b: Input/output error")
(same "a bare path" (fs-exception-message-for #t "a/b" #f #f) "a\\b")

(if (> fails 0)
    (begin (printf "WIN-PLATFORM FAILURES: ~a of ~a\n" fails total) (exit 1))
    (printf "WIN-PLATFORM OK (~a checks)\n" total))
