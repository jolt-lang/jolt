;; The FILE argument's path classification (jolt-lang/jolt#992). Relative paths
;; belong to the project directory (JOLT_PWD, which the launcher carries because
;; bin/jolt cd's to its own checkout); everything rooted is used as given.
;;
;; `jolt C:/Users/x/hello.clj` was classified as project-relative, because only
;; a leading "/" counted as rooted, so the prebuilt Windows binary — where
;; JOLT_PWD is unset and the project dir is "." — tried to open
;; "./C:/Users/x/hello.clj" and reported the user's perfectly good path as an
;; invalid argument. The Windows rows below are the ones that broke and are
;; unreachable from the host CI runs on, so the platform is a parameter.
;;
;; The same line has to be drawn for a :jolt/native candidate path, so
;; native-candidate's rows are here too — it is the other place the runtime
;; decides whether a path the user wrote belongs to the project directory.
;;
;; Run: bin/jolt run test/chez/file-arg-test.clj (smoke.sh greps for
;; "FILE-ARG OK").
(ns file-arg-test
  (:require [jolt.deps :as deps]))

(require 'jolt.main)
(def file-arg-for @#'jolt.main/file-arg-for)

(def failures (atom []))
(defn check [label got want]
  (when-not (= got want)
    (swap! failures conj (str label ": want " (pr-str want) " got " (pr-str got)))))

(defn- posix [dir x] (file-arg-for (@#'deps/native-path-kind-for false x) false dir x))
(defn- win [dir x] (file-arg-for (@#'deps/native-path-kind-for true x) true dir x))

;; --- "-" is stdin on both, and is checked before anything classifies it -------
(check "posix dash" (posix "/proj" "-") "/dev/stdin")
(check "windows dash" (win "C:/proj" "-") "/dev/stdin")

;; --- POSIX is unchanged ------------------------------------------------------
(check "posix absolute as given" (posix "/proj" "/tmp/a.clj") "/tmp/a.clj")
(check "posix relative joins" (posix "/proj" "a.clj") "/proj/a.clj")
(check "posix ./ joins once" (posix "/proj" "./a.clj") "/proj/a.clj")
(check "posix nested relative" (posix "/proj" "src/a.clj") "/proj/src/a.clj")
;; a Windows drive path means nothing here: ":" and "\" are ordinary filename
;; characters on POSIX, so this stays project-relative, as it always was
(check "posix drive path is relative" (posix "/proj" "C:/a.clj") "/proj/C:/a.clj")
;; the prebuilt binary's case: JOLT_PWD unset, so the project dir is "."
(check "posix dot project dir" (posix "." "a.clj") "a.clj")
;; ...and no "./" is added or kept: load-file names the file by its absolute
;; path, which keeps a "./" as getAbsolutePath does
(check "posix dot project dir, ./ argument" (posix "." "./a.clj") "a.clj")

;; --- Windows: every rooted spelling is used as given -------------------------
;; This is the bug: all four of these used to come back joined to the project
;; directory, and none of them could then be opened.
(check "drive-absolute forward" (win "." "C:/Users/x/hello.clj") "C:/Users/x/hello.clj")
(check "drive-absolute backslash" (win "." "C:\\Users\\x\\hello.clj") "C:\\Users\\x\\hello.clj")
(check "drive-absolute lowercase" (win "." "d:/x.clj") "d:/x.clj")
(check "unc" (win "." "//server/share/x.clj") "//server/share/x.clj")
(check "unc backslash" (win "." "\\\\server\\share\\x.clj") "\\\\server\\share\\x.clj")
;; rooted on the current drive, and rooted on a drive's own current directory:
;; neither is absolute, but this process cannot resolve either one better than
;; the OS can, so both are handed over rather than joined to something wrong
(check "root-relative" (win "C:/proj" "/x.clj") "/x.clj")
(check "drive-relative" (win "C:/proj" "C:x.clj") "C:x.clj")

;; --- Windows: a relative path still belongs to the project -------------------
(check "windows relative joins" (win "C:/proj" "a.clj") "C:/proj/a.clj")
(check "windows ./ joins once" (win "C:/proj" "./a.clj") "C:/proj/a.clj")
;; .\ is the same argument, and cmd.exe tab-completion writes it that way
(check "windows .\\ joins once" (win "C:/proj" ".\\a.clj") "C:/proj/a.clj")
(check "windows nested relative" (win "C:/proj" "src\\a.clj") "C:/proj/src\\a.clj")

;; --- :jolt/native candidates: the same rooted/relative line -------------------
;; native-candidate joins a candidate that names a PATH to the deps.edn that
;; declared it, and leaves a rooted one to the OS — dlopen's own rule, which is
;; why a BARE name (no separator) is never joined. Recognizing only "/" and a
;; drive prefix left the backslash spellings joined to a project directory, and
;; the Windows fallback now COMPOSES candidates out of PATH entries, which on a
;; domain-joined host can be UNC.
(def native-candidate @#'jolt.main/native-candidate)

(check "a bare name is searched for, not joined"
       (native-candidate "C:/proj" "crypto.dll") "crypto.dll")
(check "a relative path joins"
       (native-candidate "C:/proj" "native/libfoo.so") "C:/proj/native/libfoo.so")
(check "a posix absolute is left alone"
       (native-candidate "/proj" "/usr/lib/libz.so.1") "/usr/lib/libz.so.1")
(check "a drive-absolute is left alone"
       (native-candidate "C:/proj" "C:/Git/mingw64/bin/libcrypto-3-x64.dll")
       "C:/Git/mingw64/bin/libcrypto-3-x64.dll")
;; a UNC directory on PATH: "C:/proj/\\srv\share\bin/…" is openable by nothing
(check "a UNC candidate is left alone"
       (native-candidate "C:/proj" "\\\\srv\\share\\bin/libcrypto-3-x64.dll")
       "\\\\srv\\share\\bin/libcrypto-3-x64.dll")
(check "a current-drive-rooted candidate is left alone"
       (native-candidate "C:/proj" "\\Windows\\System32/libcrypto-3-x64.dll")
       "\\Windows\\System32/libcrypto-3-x64.dll")
;; the build-time half draws the same line, and additionally joins a bare name:
;; the linker reads "libfoo.a" as a file here, not as a name to search for
(def native-build-path @#'jolt.main/native-build-path)
(check "a bare archive joins" (native-build-path "/dep" "libfoo.a") "/dep/libfoo.a")
(check "a UNC libdir is left alone"
       (native-build-path "C:/proj" "\\\\srv\\share\\lib") "\\\\srv\\share\\lib")

(if (seq @failures)
  (do (println "FILE-ARG FAILURES:")
      (doseq [f @failures] (println " " f))
      (System/exit 1))
  (println "FILE-ARG OK"))
