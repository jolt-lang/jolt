;; ffi.ss — the runtime side of jolt's foreign-function interface (jolt.ffi).
;;
;; A jolt LIBRARY binds native code itself: it loads a shared object and declares
;; typed foreign functions, then exposes a Clojure API. The TYPED CALL is lowered
;; at compile time to a Chez `foreign-procedure` by the backend (the
;; `jolt.ffi/foreign-fn` special form) — this file provides everything that does
;; NOT need compile-time types: loading libraries, allocating/reading/writing
;; foreign memory, and string/pointer marshaling. All exposed under `jolt.ffi`.
;;
;; A foreign pointer is a Chez machine address (an exact integer / uptr), the same
;; representation `void*` arguments and results use, so pointers flow between
;; foreign-fn calls and these helpers transparently.

;; --- a jolt value used AS a string -------------------------------------------
;; jolt-str-render-one is the `str` coercion, and it renders nil as "". That is
;; right for str and it is right where the boundary copies a VALUE — string->ptr
;; and with-c-string document exactly that. It is wrong wherever the string names
;; something, because an absent name silently became an empty one: dlopen(""), a
;; foreign type called "", a zero-byte write that reports 0 octets written and is
;; indistinguishable from writing "".
;;
;; nil has no representation in any of those positions. Where it DOES have one it
;; is already used — NULL, in a :string argument and in string->ptr — so the split
;; is between "absence means NULL here" and "absence means nothing here", and this
;; is the second. Every other value keeps the coercion, so 42 still names "42".
(define (ffi-str-arg what x)
  (if (jolt-nil? x)
      (throw-jvm 'IllegalArgumentException
                 (string-append "jolt.ffi: " what " must be a string, got nil"))
      (jolt-str-render-one x)))

;; --- loading shared objects --------------------------------------------------
;; (jolt.ffi/load-library name) loads a .so/.dylib by name (resolved by the OS
;; loader against the standard search paths). A library typically calls this once
;; at load with a platform-specific name. (load-library) with no name (or #f)
;; makes the running process's own symbols (libc, sockets) resolvable.
;;
;; NEITHER form may call (sa-load-shared-object #f) any more: Chez resolves
;; foreign-entry most-recently-loaded first, and re-loading the process-global
;; handle RE-PROMOTES it above every :jolt/native loaded so far — which is how
;; Apple's /usr/lib/libboringssl.dylib (a global-namespace resident that exports
;; the whole EVP_* set) came to serve jolt-lang/crypto's OpenSSL bindings and
;; abort in digest_update. The boot already loaded the global handle once
;; (jolt-foreign-proc-safe, rt.ss); process symbols resolve through that without
;; any reload. A NAMED library goes through the scoped loader below — RTLD_LOCAL
;; + a registered handle — so its symbols are reachable by its own defcfns and
;; invisible to everyone else's.
(define (ffi-load-library . args)
  (cond
    ((or (null? args) (jolt-nil? (car args)))
     jolt-nil)                                   ; boot's global handle suffices
    ;; The documented per-OS map spec — {:darwin "…" :linux "…" :windows "…"} —
    ;; select this platform's entry. (It was documented but never implemented:
    ;; the map rendered to a string and dlopen'd garbage; surfaced auditing the
    ;; scoped-resolution change.) A map with no entry for this platform raises,
    ;; naming the platform, rather than silently loading nothing. :mac is
    ;; babashka.ffi's spelling of :darwin and selects the same entry.
    ((jolt-map? (car args))
     (let* ((spec (car args))
            (family (sa-os-family))
            (key (case family ((macos) "darwin") ((windows) "windows") (else "linux")))
            (name (jolt-get-dispatch spec (keyword #f key) jolt-nil))
            (name (if (and (jolt-nil? name) (eq? family 'macos))
                      (jolt-get-dispatch spec (keyword #f "mac") jolt-nil)
                      name)))
       (when (jolt-nil? name)
         (jolt-throw (jolt-ex-info
                       (string-append "jolt.ffi/load-library: no :" key
                                      " entry in the per-OS spec")
                       spec)))
       (ffi-library-map (ffi-load-candidates (ffi-candidate-list name)))))
    (else (ffi-library-map (ffi-load-candidates (ffi-candidate-list (car args)))))))

;; A candidate spec is one name or an ORDERED list of them. babashka.ffi takes a
;; vector of candidates in every position a name goes (including inside the
;; per-OS map), because the same library is spelled differently across distros —
;; libcrypto.so.3 here, libcrypto.so.1.1 there. A single name stays a single
;; name; anything seqable becomes the list to try in order.
(define (ffi-candidate-list name)
  (if (or (pvec? name) (jolt-seq? name) (jolt-lazyseq? name))
      (map (lambda (c) (ffi-str-arg "library candidate" c)) (seq->list (jolt-seq name)))
      (list (jolt-str-render-one name))))

;; Try each candidate in order; answer the one that loaded.
;;
;; A load that finds NOTHING must raise: callers probe candidate lists with
;; try/catch around load-library (jolt.mvn-http), and the pre-scoped-loader
;; implementation raised through sa-load-shared-object. Silently returning nil
;; turned every fallback list into "first candidate wins, loaded or not". The
;; error names every candidate tried, since "cannot load libcrypto.so.3" alone
;; hides the three other spellings that were also attempted.
(define (ffi-load-candidates cands)
  (let loop ((cs cands))
    (cond
      ((null? cs)
       (jolt-throw (jolt-ex-info
                     (string-append "jolt.ffi/load-library: cannot load "
                                    (if (null? (cdr cands)) "" "any of ")
                                    (ffi-join-candidates cands)
                                    (let ((note (ffi-load-failure-note cands)))
                                      (if note (string-append " — " note) "")))
                     (jolt-hash-map (jolt-keyword "candidates")
                                    (make-pvec (list->vector cands))))))
      ((jolt-ffi-load-native (car cs)) (car cs))
      (else (loop (cdr cs))))))

;; Name the candidates that were tried, capped: a Linux soname glob can turn up
;; dozens of paths and an error that lists all of them is unreadable.
(define (ffi-join-candidates cands)
  (let ((total (length cands)))
    (let loop ((cs cands) (i 0) (acc ""))
      (cond
        ((or (null? cs) (= i 6))
         (if (null? cs)
             acc
             (string-append acc ", … (" (number->string total) " candidates)")))
        ((string=? acc "") (loop (cdr cs) (+ i 1) (car cs)))
        (else (loop (cdr cs) (+ i 1) (string-append acc ", " (car cs))))))))

;; The library value load-library answers: {:path "<the candidate that loaded>"}.
;; babashka.ffi returns the same shape, and the :path is the only part a caller
;; can act on — which of the candidates this machine actually has.
(define (ffi-library-map path)
  (jolt-hash-map (jolt-keyword "path") path))

;; --- Android / Termux: the prefix the OS loader does not search ---------------
;; A jolt process on Android is launched by /system/bin/linker64, whose default
;; search path is /system/lib64 and friends -- never the Termux prefix. So a bare
;; soname does one of two things there, and both are wrong: the versioned name
;; ($PREFIX/lib/libssl.so.3) is "not found" because nothing looks in the prefix,
;; and the unversioned one (libssl.so) binds ANDROID'S library of that name.
;; /system/lib64/libssl.so is BoringSSL: it exports SSL_new but not SSL_ctrl, and
;; its SSL_CTX layout is not OpenSSL 3's, so a TLS binding that resolves through
;; it fails at the first call -- or, when the real OpenSSL is also loaded, picks
;; one library for some symbols and the other for the rest and faults. Both
;; jolt-lang/crypto and jolt-lang/http-client name those sonames bare, which is
;; the right thing to write everywhere else.
;;
;; So: detect the prefix, and give a BARE name a second try under $PREFIX/lib
;; after the OS search has had its turn. Fallback ordering is what makes this
;; safe -- a name that legitimately resolves in /system/lib64 is untouched -- and
;; it is also what makes it work: the versioned candidates a per-OS spec lists
;; first exist only in the prefix, so they are the ones that now resolve, ahead
;; of the unversioned BoringSSL one.
;;
;; Detection is by environment, which is how Termux identifies itself: PREFIX is
;; its install root and TERMUX_VERSION/ANDROID_ROOT say which OS it is on. The
;; env is also what makes it testable from an ordinary host.
(define (ffi-getenv name) (guard (e (#t #f)) (getenv name)))
(define (ffi-termux-lib-dir)
  (let ((prefix (ffi-getenv "PREFIX")))
    (and prefix
         (not (string=? prefix ""))
         (or (ffi-getenv "TERMUX_VERSION") (ffi-getenv "ANDROID_ROOT"))
         (string-append prefix "/lib"))))

;; A name with no directory separator -- what dlopen resolves through the OS
;; search path rather than opening directly.
(define (ffi-bare-soname? name)
  (let ((n (string-length name)))
    (let loop ((i 0))
      (cond ((fx=? i n) #t)
            ((or (char=? (string-ref name i) #\/) (char=? (string-ref name i) #\\)) #f)
            (else (loop (fx+ i 1)))))))

;; The paths jolt-ffi-load-native tries for one name, in order: the name as
;; given, then the Android fallback above when it applies. One list so the
;; ordering is data a test can read rather than control flow inside the loader.
(define (ffi-native-candidates path)
  (cons path
        (let ((d (and (ffi-bare-soname? path) (ffi-termux-lib-dir))))
          (if d (list (string-append d "/" path)) '()))))

;; (load-system-library "z") -> libz.dylib, libz.so, or z.dll, whichever this
;; platform spells it. Answers the same library map as load-library.
;;
;; Linux needs the extra half: a distro that ships only the RUNTIME package has
;; libz.so.1 and no libz.so at all — the unversioned name is a -dev symlink —
;; so when the plain soname does not load, glob lib<n>.so.* across the loader's
;; search directories and try the newest version first. Guessing a fixed set of
;; version numbers instead would miss libz.so.1.2.13 and every soname above the
;; guess, so this reads the directories rather than inventing names.
(define (ffi-so-search-dirs)
  ;; LD_LIBRARY_PATH first (it wins for the loader too), then the Termux prefix
  ;; on Android (where it plays the part /usr/lib plays elsewhere and the OS
  ;; loader never looks), then the standard prefixes, then Debian/Ubuntu
  ;; multiarch subdirectories, discovered by listing rather than by naming a
  ;; triple this build cannot know.
  (let* ((env (or (guard (e (#t #f)) (getenv "LD_LIBRARY_PATH")) ""))
         (from-env (if (string=? env "") '() (ffi-split-colons env)))
         (from-env (let ((d (ffi-termux-lib-dir)))
                     (if d (append from-env (list d)) from-env)))
         (roots '("/usr/local/lib" "/usr/lib" "/lib" "/usr/lib64" "/lib64"))
         (multi (apply append
                       (map (lambda (root)
                              (map (lambda (d) (string-append root "/" d))
                                   (filter (lambda (d) (ffi-multiarch-dir? d))
                                           (ffi-dir-entries root))))
                            '("/usr/lib" "/lib")))))
    (append from-env roots multi)))

(define (ffi-split-char str ch)
  (let loop ((cs (string->list str)) (cur '()) (acc '()))
    (cond
      ((null? cs)
       (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
      ((char=? (car cs) ch)
       (loop (cdr cs) '() (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
      (else (loop (cdr cs) (cons (car cs) cur) acc)))))

(define (ffi-split-colons str) (ffi-split-char str #\:))

(define (ffi-dir-entries dir)
  (guard (e (#t '()))
    (map (lambda (e) (if (pair? e) (car e) e)) (directory-list dir))))

(define (ffi-multiarch-dir? name)
  (let ((n (string-length name)))
    (and (> n 6)
         (let loop ((i 0))                       ; contains "-linux-"
           (cond ((> (+ i 7) n) #f)
                 ((string=? (substring name i (+ i 7)) "-linux-") #t)
                 (else (loop (+ i 1))))))))

(define (ffi-starts-with? str prefix)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;; The dot-separated integers after "lib<n>.so.", as a list — the sort key. A
;; non-numeric component sorts below every number, so libz.so.1 beats
;; libz.so.1.2.13-suffix only on the components that actually parse.
(define (ffi-soname-version name base)
  (map (lambda (part) (or (string->number part) -1))
       (ffi-split-dots (substring name (+ 1 (string-length base)) (string-length name)))))

(define (ffi-split-dots str) (ffi-split-char str #\.))

(define (ffi-version>? a b)
  (cond ((and (null? a) (null? b)) #f)
        ((null? a) #f)
        ((null? b) #t)
        ((> (car a) (car b)) #t)
        ((< (car a) (car b)) #f)
        (else (ffi-version>? (cdr a) (cdr b)))))

(define (ffi-versioned-sonames base)
  (let ((prefix (string-append base ".")))
    (apply append
           (map (lambda (dir)
                  (let ((hits (filter (lambda (e) (ffi-starts-with? e prefix))
                                      (ffi-dir-entries dir))))
                    (map (lambda (e) (string-append dir "/" e))
                         (list-sort (lambda (x y)
                                      (ffi-version>? (ffi-soname-version x base)
                                                     (ffi-soname-version y base)))
                                    hits))))
                (ffi-so-search-dirs)))))

;; --- Windows: the soversion lives in the FILENAME ---------------------------
;; The Unix glob above exists because a distro may ship libz.so.1 and no libz.so.
;; Windows has the same gap in a different spelling: OpenSSL's own builds — Git
;; for Windows', which is what jolt's docs tell a Windows user to install — are
;; named libcrypto-3-x64.dll and libssl-3-x64.dll, so neither "crypto.dll" nor
;; "libcrypto.dll" names a file that exists, however completely the DLL is on
;; the loader's search path. Enumerate the directories the loader searches and
;; take the versioned spellings, newest first.
(define (ffi-ends-with-ci? str suffix)
  (let ((n (string-length str)) (m (string-length suffix)))
    (and (>= n m) (string-ci=? (substring str (- n m) n) suffix))))

;; <base> followed by a version separator: libcrypto-3-x64.dll is libcrypto's,
;; libcryptohelper.dll is not. "libcrypto.dll" is deliberately NOT matched — it
;; is one of the conventional names tried first, and listing it twice only makes
;; the diagnostic harder to read.
(define (ffi-dll-variant-of? entry base)
  (and (> (string-length entry) (string-length base))
       (string-ci=? (substring entry 0 (string-length base)) base)
       (memv (string-ref entry (string-length base)) '(#\- #\_))
       #t))

(define (ffi-dll-variant? entry name)
  (and (ffi-ends-with-ci? entry ".dll")
       (or (ffi-dll-variant-of? entry name)
           (ffi-dll-variant-of? entry (string-append "lib" name)))))

;; The loader's search directories that a process can enumerate: its own
;; executable's directory (first in the standard Windows search order) and PATH,
;; which is where Git for Windows puts its DLLs. The system directories are
;; deliberately absent — a bare name already reaches them, and the versioned
;; OpenSSL builds are never there.
;;
;; The executable is asked of Windows itself: (command-line)'s first element is
;; argv[0] as TYPED — plain "jolt" when jolt.exe is found on PATH, which has no
;; directory in it — and under `scheme --script` it is the script. Either way the
;; folder holding jolt.exe went unsearched, and a DLL copied beside it was not
;; found by the one lookup that could find it (jolt-lang/jolt#1127).
(define-win32-proc ffi-get-module-file-name-w
  "kernel32.dll" "GetModuleFileNameW" (void* u8* unsigned-32) unsigned-32)

(define (ffi-win32-exe-path)
  (let ((f (ffi-get-module-file-name-w)))
    (and f
         (guard (e (#t #f))
           (let* ((cap 32768)
                  (bv (make-bytevector (* 2 cap) 0))
                  (n (f 0 bv cap)))
             (and (> n 0) (< n cap)
                  (let ((out (make-bytevector (* 2 n))))
                    (bytevector-copy! bv 0 out 0 (* 2 n))
                    (utf16->string out (endianness little)))))))))

(define (ffi-exe-dir)
  (guard (e (#t #f))
    (let* ((argv (command-line))
           (exe (or (ffi-win32-exe-path) (and (pair? argv) (car argv)))))
      (and (string? exe)
           (let loop ((i (- (string-length exe) 1)))
             (cond ((< i 0) #f)
                   ((or (char=? (string-ref exe i) #\/) (char=? (string-ref exe i) #\\))
                    (substring exe 0 (max i 1)))
                   (else (loop (- i 1)))))))))

(define (ffi-dll-search-dirs)
  (let* ((path (or (ffi-getenv "PATH") ""))
         (dirs (if (string=? path "") '() (ffi-split-char path #\;)))
         (exe (ffi-exe-dir)))
    (if exe (cons exe dirs) dirs)))

;; Newest first by plain descending name order: the version is embedded in the
;; filename with no agreed grammar (libcrypto-3-x64.dll, libcrypto-1_1-x64.dll),
;; so a string sort is the only ordering that is both total and deterministic —
;; and it does put 3 ahead of 1_1, which is the case that matters.
(define (ffi-versioned-dlls name)
  (apply append
         (map (lambda (dir)
                (map (lambda (e) (string-append dir "/" e))
                     (list-sort (lambda (x y) (string-ci>? x y))
                                (filter (lambda (e) (ffi-dll-variant? e name))
                                        (ffi-dir-entries dir)))))
              (ffi-dll-search-dirs))))

;; The platform's conventional spellings of a bare library NAME, as data: what
;; load-system-library tries, and what load-natives! falls back to for a
;; :jolt/native spec that declares no candidates for the running platform
;; (jolt-lang/jolt#989) — where the candidate list was empty, nothing was
;; dlopen'd at all, and the failure still read "not found".
(define (ffi-system-library-candidates n)
  (case (sa-os-family)
    ((macos)   (list (string-append "lib" n ".dylib") (string-append n ".dylib")))
    ((windows) (append (list (string-append n ".dll") (string-append "lib" n ".dll"))
                       (ffi-versioned-dlls n)))
    (else      (let ((base (string-append "lib" n ".so")))
                 (cons base (ffi-versioned-sonames base))))))

(define (ffi-load-system-library name)
  (ffi-library-map
   (ffi-load-candidates
    (ffi-system-library-candidates (ffi-str-arg "load-system-library name" name)))))

;; Loadable without mutating resolution state: probe with a LOCAL dlopen through
;; the scoped loader (registering the handle — a probe that succeeds will be
;; followed by use). The old form side-effected the GLOBAL namespace to answer
;; a yes/no question.
(define (ffi-loaded? name)
  (if (jolt-ffi-load-native (ffi-str-arg "loaded? name" name)) #t #f))

;; --- scoped native libraries: dlopen RTLD_LOCAL + per-handle dlsym ----------
;; A :jolt/native library's symbols must NEVER depend on the process-global
;; foreign-entry search order. Chez resolves foreign-entry most-recently-loaded
;; first, so the OS's own libs (Apple's /usr/lib/libboringssl.dylib EXPORTS
;; EVP_*!) would shadow a library's intended native once anything re-promotes the
;; global handle (the embedded-fasl fetch did exactly that — fix/ffi-scoped-natives).
;; Instead a declared native is dlopen'd RTLD_LOCAL — its symbols never enter the
;; global namespace at all — and a defcfn resolves by dlsym against the loaded
;; handles (declaration order) BEFORE falling back to today's global name
;; resolution (libc, app-local symbols, :process natives). The guarantee is
;; one-way: a declared native shadows the global namespace for ITS OWN defcfns,
;; which is the property jolt-lang/crypto needs so its OpenSSL EVP_* binds reach
;; the right library, not Apple's BoringSSL. defcfn's surface syntax is unchanged;
;; only resolution semantics move.
;;
;; Windows (NT) keeps its current path: LoadLibrary does not merge a module's
;; symbols into a global namespace the way RTLD_GLOBAL does, so the registry
;; holds no handles there and defcfn falls back to global name resolution exactly
;; as before.
;; dlopen/dlsym/dlclose resolve through the boot-loaded process-global handle
;; (libSystem on darwin, libdl/libc on linux) — never the natives themselves.
(define ffi-dlopen  (jolt-foreign-proc-safe "dlopen"  '(string int) 'void*))
(define ffi-dlsym   (jolt-foreign-proc-safe "dlsym"   '(void* string) 'void*))
(define ffi-dlclose (jolt-foreign-proc-safe "dlclose" '(void*) 'int))
;; RTLD flag values differ by OS (macos values (VERIFY by probe before trusting): NOW=#x2 LAZY=#x1 LOCAL=#x4
;; GLOBAL=#x100; linux/BSD NOW=2 LAZY=1 LOCAL=0 GLOBAL=0x100). RTLD_LOCAL =
;; "do not make this object's symbols available for global resolution" — the
;; isolation guarantee. Verified cross-platform by the flag probe (podman/linux).
(define ffi-rtld-flags
  (bitwise-ior 2                                  ; RTLD_NOW on every POSIX host
                (if (eq? (sa-os-family) 'macos) 4 0)))  ; RTLD_LOCAL: darwin 4, else 0
;; Registry: an ordered list of handles (declaration order — what deps.clj's
;; first-inclusion order hands load-natives!) and a path set so the same .so is
;; not dlopen'd twice. Mutated only from load-natives!/jolt-build-load-native,
;; but guarded anyway in case a repl loads a native lazily.
(define ffi-native-mu (make-mutex))
;; cell[0] = list of (path . handle) pairs, oldest first. The PATH rides along
;; only so a duplicate-symbol report can name the libraries involved (issue
;; #731); resolution itself reads the handle.
(define ffi-native-handles (vector #f))
(define ffi-native-paths (make-hashtable string-hash equal?))  ; path(string) -> #t
;; dlopen `path` RTLD_LOCAL and record the handle. Returns the handle (a positive
;; integer) on success, #f on failure, or #t on Windows (loaded globally, not
;; registered — defcfn resolves globally there as before). A repeat load of an
;; already-registered path is a no-op success.
(define (jolt-ffi-load-native path)
  (cond
    ((eq? (sa-os-family) 'windows)
     (guard (e (#t (ffi-note-load-failure! path (ffi-condition-text e)) #f))
       (sa-load-shared-object path) #t))
    ((not ffi-dlopen) #f)
    (else
     (jolt-with-mutex ffi-native-mu
       (cond
         ((hashtable-ref ffi-native-paths path #f) #t)   ; already loaded
         (else
          ;; Each candidate in turn; the one that opens is what the registry
          ;; records, so a duplicate-symbol report names the file that actually
          ;; backs the handle and not the soname that was asked for. The name as
          ;; asked is marked loaded too, so a second request for it is the same
          ;; no-op it has always been.
          (let loop ((cs (ffi-native-candidates path)))
            (if (null? cs)
                #f
                (let ((h (ffi-dlopen (car cs) ffi-rtld-flags)))
                  (cond
                   ((and h (integer? h) (positive? h))
                    (hashtable-set! ffi-native-paths path #t)
                    (hashtable-set! ffi-native-paths (car cs) #t)
                    (vector-set! ffi-native-handles 0
                                 (append (or (vector-ref ffi-native-handles 0) '())
                                         (list (cons (car cs) h))))
                    h)
                   (else
                    (ffi-note-load-failure! (car cs) (and ffi-dlerror (ffi-dlerror)))
                    (loop (cdr cs)))))))))))))

;; --- why a library that is THERE did not load -------------------------------
;; A candidate that fails is not necessarily missing. On Windows the usual case
;; is a DLL sitting right beside jolt.exe whose own dependency (libcrypto beside
;; libssl, the VC++ runtime beside either) cannot be found — LoadLibrary fails
;; the file that exists, and "not found — tried [libssl-3-x64.dll]" sent the
;; user looking for a file they could see (jolt-lang/jolt#1127). The loader's
;; own reason is kept per candidate at the failure, so a report can name the
;; file it found and why it did not load without loading anything again.
(define ffi-dlerror (jolt-foreign-proc-safe "dlerror" '() 'string))
(define ffi-native-failures (make-hashtable string-hash equal?))  ; path -> reason or #f

(define (ffi-note-load-failure! path reason)
  (jolt-with-mutex ffi-native-mu
    (hashtable-set! ffi-native-failures path reason)))

(define (ffi-condition-text e)
  (ffi-trim-right
   (guard (_ (#t (call-with-string-output-port (lambda (p) (display-condition e p)))))
     (if (and (message-condition? e) (irritants-condition? e))
         (apply format (condition-message e) (condition-irritants e))
         (call-with-string-output-port (lambda (p) (display-condition e p)))))))

;; FormatMessage ends Windows' reason with CRLF.
(define (ffi-trim-right s)
  (let loop ((n (string-length s)))
    (if (and (> n 0) (char-whitespace? (string-ref s (- n 1))))
        (loop (- n 1))
        (substring s 0 n))))

;; Where the loader would find NAME on disk, or #f. A name with a directory in
;; it is opened as given; a bare one is looked up in the directories the loader
;; searches that this process can list (ffi-dll-search-dirs on Windows: the
;; executable's directory and PATH). Anything this cannot see stays reported as
;; not found, which is what it was before.
(define (ffi-native-locate name)
  (define (exists? p) (guard (e (#t #f)) (file-exists? p)))
  (if (not (ffi-bare-soname? name))
      (and (exists? name) name)
      (let loop ((ds (if (eq? (sa-os-family) 'windows)
                         (ffi-dll-search-dirs)
                         (ffi-so-search-dirs))))
        (cond ((null? ds) #f)
              ((exists? (string-append (car ds) "/" name)) (string-append (car ds) "/" name))
              (else (loop (cdr ds)))))))

;; One sentence per candidate that exists but did not load, or #f when every
;; candidate is simply absent.
(define (ffi-load-failure-note cands)
  (let loop ((cs cands) (acc '()))
    (if (null? cs)
        (and (pair? acc)
             (let join ((xs (reverse acc)) (out ""))
               (if (null? xs)
                   out
                   (join (cdr xs) (if (string=? out "") (car xs)
                                      (string-append out "; " (car xs)))))))
        (let ((at (ffi-native-locate (car cs)))
              (why (jolt-with-mutex ffi-native-mu
                     (hashtable-ref ffi-native-failures (car cs) #f))))
          (loop (cdr cs)
                (if at
                    (cons (string-append
                           at " is there but did not load"
                           (if why (string-append ": " why) "")
                           (if (eq? (sa-os-family) 'windows)
                               " (a DLL it depends on is missing or not on PATH — Windows reports that as the module not being found)"
                               ""))
                          acc)
                    acc))))))
;; Every declared native through which `sym` RESOLVES, as (path . address) in
;; declaration order. Walking all of them rather than stopping at the first is
;; what makes the duplicate below visible; it costs one dlsym per declared
;; native, paid ONCE per defcfn (the emitted binding caches the address in its
;; own `p` cell — backend_scheme.clj emit-ffi-fn), not once per call.
(define (jolt-ffi-native-resolvers sym)
  (if (or (eq? (sa-os-family) 'windows) (not ffi-dlsym))
      '()
      (let loop ((hs (or (vector-ref ffi-native-handles 0) '())) (acc '()))
        (cond
          ((null? hs) (reverse acc))
          (else
           (let ((a (ffi-dlsym (cdar hs) sym)))
             (loop (cdr hs)
                   (if (and a (integer? a) (positive? a))
                       (cons (cons (caar hs) a) acc)
                       acc))))))))

;; The DISTINCT definitions of `sym`, one entry per address, naming the first
;; declared native that reaches each.
;;
;; The address is the discriminator, and it has to be: dlsym(handle, sym)
;; searches that handle's DEPENDENCY CHAIN as well as the library itself, so a
;; dependent linked correctly against the shared base resolves the base's
;; symbols through its own handle too. Counting handles that answer would call
;; that a duplicate — a false positive on exactly the build that got it right,
;; which is the one way to make a warning worth ignoring. Two copies means two
;; ADDRESSES; one address reached through several handles is one copy, which is
;; the whole point of linking dynamically.
(define (jolt-ffi-native-definers sym)
  (let loop ((rs (jolt-ffi-native-resolvers sym)) (seen '()) (acc '()))
    (cond
      ((null? rs) (reverse acc))
      ((memv (cdar rs) seen) (loop (cdr rs) seen acc))
      (else (loop (cdr rs) (cons (cdar rs) seen) (cons (car rs) acc))))))

;; --- the duplicate-static-copy report (issue #731) ---------------------------
;; Two declared natives defining the SAME symbol is the signature of a dependent
;; library that was linked against the base library's STATIC archive instead of
;; its shared object: the archive members it pulled in are exported from the
;; dependent too. raygui built against libraylib.a is the case that named this —
;; it gets a private copy of raylib's input globals, so every control reads a
;; mouse that never moves and the UI goes inert with no error anywhere.
;;
;; jolt cannot repair it. By the time the .so exists the second copy is baked in,
;; and the only fix is to rebuild the dependent against the SHARED base library.
;; So this reports rather than raises, for two reasons: the damage is already
;; done and refusing to run would not undo it, and two unrelated natives may
;; legitimately export a common name (`version`, `init`) where first-hit
;; resolution is correct and always was. A raise would turn those into a
;; regression; silence is what the issue is about.
;;
;; Reported under JOLT_WARNINGS (rt.ss jolt-warnings?): the error port is the
;; program's, so the runtime writes there only when asked (#1292). The
;; jolt.ffi/defining-libraries answers the same question without it.
;;
;; Once per symbol, not once per call. The emitted binding caches its address,
;; so a repeat would only appear if a caller resolved the same name again — and
;; a warning that repeats is a warning that gets filtered out.
;; Guarded by ffi-native-mu like every other global table here. A defcfn binding
;; resolves lazily on FIRST CALL, so two threads first-calling two duplicated
;; symbols race this writer-vs-writer — which faults inside the collector rather
;; than merely losing an entry. Probe and set are one critical section so the
;; "once per symbol" claim holds under the race too.
(define ffi-dup-reported (make-hashtable string-hash equal?))
(define (ffi-dup-claim! sym)      ; #t if THIS caller owns the report
  (jolt-with-mutex ffi-native-mu
    (cond ((hashtable-ref ffi-dup-reported sym #f) #f)
          (else (hashtable-set! ffi-dup-reported sym #t) #t))))
(define (ffi-report-duplicate! sym defs)
  (when (and (jolt-warnings?) (ffi-dup-claim! sym))
    (let ((p (current-error-port)))
      (display (string-append
                 "jolt.ffi: duplicate native symbol " sym " — defined by "
                 (number->string (length defs)) " declared libraries:\n")
               p)
      (for-each (lambda (d)
                  (display (string-append "  " (car d) "\n") p))
                defs)
      (display (string-append
                 "  Resolving against the first. This usually means a library was linked\n"
                 "  against another's STATIC archive and carries its own copy of that\n"
                 "  library's globals, which then never see the other copy's writes.\n"
                 "  Rebuild it against the shared library instead.\n")
               p)
      (flush-output-port p))))

;; dlsym `sym` across the registered native handles in declaration order. Returns
;; the address (positive integer) of the first hit, or #f when no handle has it
;; — the emitter then falls back to global name resolution. #f on Windows (no
;; registered handles).
(define (jolt-ffi-dlsym-native sym)
  (let ((rs (jolt-ffi-native-resolvers sym)))
    (cond
      ((null? rs) #f)
      (else
       ;; Only walk the dedupe when more than one handle answered; the common
       ;; case (exactly one) skips it entirely.
       (when (pair? (cdr rs))
         (let ((defs (jolt-ffi-native-definers sym)))
           (when (pair? (cdr defs)) (ffi-report-duplicate! sym defs))))
       (cdar rs)))))

;; jolt.ffi/defining-libraries: one declared native per DISTINCT definition of
;; `sym`, in declaration order — the diagnostic to reach for when a native call
;; returns something impossible and nothing has raised. A correctly linked set
;; answers one entry however many libraries use the symbol; two entries means
;; two copies.
(define (jolt-ffi-defining-libraries sym)
  (make-pvec
   (list->vector
    (map car (jolt-ffi-native-definers (ffi-str-arg "symbol" sym))))))

;; --- foreign type keywords ---------------------------------------------------
;; The keyword type names jolt.ffi accepts (in foreign-fn signatures and the
;; memory accessors) map to Chez foreign types. Kept in one place so the backend
;; (compile-time, for foreign-procedure) and these accessors (runtime, for
;; foreign-ref/set!) agree — see ffi-types in jolt-core/jolt/backend_scheme.clj.
;; Exact scalar widths use native byte order. Signed and unsigned names at one
;; width expose the same stored bits; wire byte order remains an explicit codec
;; or htons/ntohs concern.
(define (ffi-type->chez kw)
  (let ((n (if (keyword-t? kw) (keyword-t-name kw) (ffi-str-arg "foreign type" kw))))
    (cond
      ((string=? n "int") 'int)
      ((string=? n "uint") 'unsigned-int)
      ((or (string=? n "int8") (string=? n "i8")) 'integer-8)
      ((or (string=? n "int16") (string=? n "short")) 'integer-16)
      ((or (string=? n "uint16") (string=? n "ushort")) 'unsigned-16)
      ((string=? n "int32") 'integer-32)
      ((string=? n "uint32") 'unsigned-32)
      ((string=? n "long") 'long)
      ((string=? n "ulong") 'unsigned-long)
      ((string=? n "int64") 'integer-64)
      ((string=? n "uint64") 'unsigned-64)
      ((string=? n "size_t") 'size_t)
      ((string=? n "ssize_t") 'ssize_t)
      ((string=? n "iptr") 'iptr)
      ((string=? n "uptr") 'uptr)
      ((string=? n "double") 'double)
      ((string=? n "float") 'float)
      ((or (string=? n "pointer") (string=? n "void*")) 'void*)
      ((string=? n "string") 'string)
      ((string=? n "void") 'void)
      ((or (string=? n "uint8") (string=? n "u8") (string=? n "byte")) 'unsigned-8)
      ((string=? n "char") 'char)
      ;; :bool answers a jolt-side marker rather than a Chez type. It is a
      ;; ONE-BYTE C boolean (C99 _Bool), so its WIDTH is unsigned-8 — but its
      ;; value is converted at the boundary, and a caller that saw only the
      ;; width would store 1 and read back 1 instead of true. Chez's own
      ;; `boolean` foreign type is int-sized and would be the wrong width.
      ((string=? n "bool") 'jolt-bool)
      (else (error #f (string-append "jolt.ffi: unknown foreign type :" n))))))
;; The Chez type that carries a jolt foreign type's BITS — what sizeof and the
;; raw block moves want, with :bool's marker resolved to its one byte.
(define (ffi-chez-width ct) (if (eq? ct 'jolt-bool) 'unsigned-8 ct))

;; --- :string <-> NULL ---------------------------------------------------------
;; Chez's `string` foreign type already carries NULL in both directions, spelled
;; #f: passing #f sends a null char*, and a C function returning NULL comes back
;; as #f. jolt's own nil is a distinct sentinel, so without these two the boundary
;; leaks Scheme: passing nil raised "invalid foreign-procedure argument
;; #[jolt-nil-v1]", and a NULL return surfaced in Clojure as false rather than nil.
;;
;; Named for the direction of the CONVERSION, not for the position it sits on,
;; because the two forms invert each other. A foreign-fn calls out to C, so its
;; :string arguments convert jolt->c and its :string result converts c->jolt. A
;; foreign-callable is called BY C, so the roles swap: its :string arguments
;; convert c->jolt and its :string result converts jolt->c. Position-relative
;; names ("arg"/"ret") read backwards on one of the two.
;;
;; jolt->c validates rather than passing the odd value through, because `string`
;; is the ONE foreign type that takes a non-string quietly. Chez rejects #t, an
;; integer, a symbol and a bytevector in a `string` position, and rejects #f in
;; every other position — argument, result and foreign-set! alike. So #f in a
;; `string` position is the single hole in the boundary's type checking, and what
;; falls through it lands on precisely the value C reads as "absent": a `when`
;; that did not fire, a predicate result, a `boolean` of a missing key, silently
;; becomes NULL. jolt.ffi has no :bool type, so no false ever belongs here — nil
;; is how NULL is spelled. Naming the whole non-string set in one jolt-level error
;; also beats Chez's "invalid foreign-procedure argument #[...]" for the rest.
;;
;; c->jolt needs no such check: its input comes from Chez, which hands back a
;; string or #f and nothing else.
(define (jolt-ffi-string->c x)
  (cond ((string? x) x)                 ; the common path, one test
        ((jolt-nil? x) #f)              ; nil IS NULL, in both directions
        (else (throw-jvm 'IllegalArgumentException
                         (string-append "jolt.ffi: :string got " (jolt-pr-str x)
                                        " — NULL is spelled nil")))))
(define (jolt-ffi-c->string x) (if x x jolt-nil))

;; --- foreign memory ----------------------------------------------------------
;; alloc returns a pointer (integer address). The caller frees it. read/write take
;; a type keyword and an optional byte offset.
(define (ffi-alloc nbytes) (sa-foreign-alloc (jnum->exact nbytes)))
;; ZEROED allocation, in one block move. An arena hands out zeroed memory (as
;; babashka.ffi/alloc does), because a struct a caller only partly fills is the
;; ordinary case and malloc's leftovers in the rest of it are a C-visible bug
;; that reproduces only under load. A fresh bytevector is already zero, so the
;; fill is one sa-foreign-bytes-set!, not a per-byte loop.
(define (ffi-calloc nbytes)
  (let* ((n (jnum->exact nbytes)) (p (sa-foreign-alloc n)))
    (when (> n 0) (sa-foreign-bytes-set! p (make-bytevector n 0) n))
    p))
(define (ffi-free ptr) (sa-foreign-free (jnum->exact ptr)) jolt-nil)
;; --- :bool <-> a one-byte C boolean -----------------------------------------
;; Named for the direction of the CONVERSION, like the :string pair above, and
;; used from the same three places: the emitted foreign-procedure, the emitted
;; foreign-callable, and these accessors. jolt TRUTHINESS decides the byte, so
;; nil and false send 0 and every other value sends 1 — a C predicate reads back
;; as true/false rather than as the truthy number 0.
(define (jolt-ffi-bool->c v) (if (or (jolt-nil? v) (eq? v #f)) 0 1))
(define (jolt-ffi-c->bool raw) (not (eqv? (jnum->exact raw) 0)))
;; --- :float/:double <- a java.lang.Float -------------------------------------
;; (float x) answers a java.lang.Float, a jfloat record, and a Chez float or
;; double position takes only a flonum: passing one raised "invalid
;; foreign-procedure argument". Every place a jolt value crosses into C as a
;; float or double unboxes it here, as jolt-double does — an argument, a
;; callable's result, a write, a bare :& tail. Any other value passes through
;; unchanged, so a wrong one still meets Chez's own check.
(define (jolt-ffi-float->c x) (if (jfloat? x) (jfloat-fl x) x))
;; read/write take the type, then the OFFSET LAST — the babashka.ffi order, so
;; (write p t v) and (write p t v offset) are one binding in both APIs. The jolt
;; wrapper in stdlib/jolt/ffi.clj supplies the offset and resolves a layout or a
;; place before it gets here; these two see a scalar type keyword and nothing
;; else.
(define ffi-read
  (case-lambda
    ((ptr ty) (ffi-read* ptr ty 0))
    ((ptr ty off) (ffi-read* ptr ty off))))
(define (ffi-read* ptr ty off)
  (let ((ct (ffi-type->chez ty)))
    (if (eq? ct 'jolt-bool)
        (jolt-ffi-c->bool (sa-foreign-ref 'unsigned-8 (jnum->exact ptr) (jnum->exact off)))
        (sa-foreign-ref ct (jnum->exact ptr) (jnum->exact off)))))
(define ffi-write
  (case-lambda
    ((ptr ty val) (ffi-write* ptr ty val 0))
    ((ptr ty val off) (ffi-write* ptr ty val off))))
(define (ffi-write* ptr ty val off)
  (let ((ct (ffi-type->chez ty)))
    (cond
      ((eq? ct 'jolt-bool)
       (sa-foreign-set! 'unsigned-8 (jnum->exact ptr) (jnum->exact off) (jolt-ffi-bool->c val)))
      ((memq ct '(float double))
       (sa-foreign-set! ct (jnum->exact ptr) (jnum->exact off) (jolt-ffi-float->c val)))
      (else (sa-foreign-set! ct (jnum->exact ptr) (jnum->exact off) val))))
  jolt-nil)
;; sizeof a foreign type (for laying out structs / arrays).
(define (ffi-sizeof ty) (sa-foreign-sizeof (ffi-chez-width (ffi-type->chez ty))))
(define (ffi-null? ptr) (and (number? ptr) (= (jnum->exact ptr) 0)))
(define ffi-null 0)

;; (copy src dst n) -> nil. The block travels through ONE bytevector, so this is
;; memmove rather than memcpy: overlapping regions copy correctly in either
;; direction, which is what a caller shifting bytes inside a buffer needs.
(define (ffi-copy src dst n)
  (let* ((n (jnum->exact n)) (bv (make-bytevector n)))
    (when (> n 0)
      (sa-foreign-bytes-ref! (jnum->exact src) bv n)
      (sa-foreign-bytes-set! (jnum->exact dst) bv n))
    jolt-nil))

;; --- buffer I/O (known length) ----------------------------------------------
;; Every one of these moves the block in ONE sa-foreign-bytes-ref!/-set! call
;; and does any per-element work (signed-byte fold, UTF-8 decode) on the Scheme
;; side. A byte at a time across the foreign boundary costs ~30ns each — an
;; order of magnitude more than the same loop over a bytevector — so a 64K
;; socket read used to spend ~2ms in the copy alone.

;; read n bytes at ptr as a string (UTF-8, falling back to latin1 for invalid
;; sequences) — for a socket recv buffer and similar fixed-length reads.
(define (ffi-read-bytes ptr n)
  (let* ((n (jnum->exact n)) (p (jnum->exact ptr)) (bv (make-bytevector n)))
    (sa-foreign-bytes-ref! p bv n)
    (guard (e (#t (list->string (map integer->char (bytevector->u8-list bv))))) (utf8->string bv))))
;; write a string's UTF-8 bytes into ptr (no NUL terminator); return the count.
(define (ffi-write-bytes ptr s)
  (let* ((bv (string->utf8 (ffi-str-arg "write-bytes value" s))) (n (bytevector-length bv)) (p (jnum->exact ptr)))
    (sa-foreign-bytes-set! p bv n)
    n))
(def-var! "jolt.ffi" "read-bytes" ffi-read-bytes)
(def-var! "jolt.ffi" "write-bytes" ffi-write-bytes)
;; …and under the reserved __ names, for the same reason as __read-array below:
;; stdlib/jolt/ffi.clj DEFINES read-bytes and write-bytes over these — to carry
;; the :doc/:arglists the Scheme side cannot attach, and so that write-bytes can
;; route a byte-array argument to the raw block move instead of rendering it
;; with `str` — so each needs a name its own definition has not taken.
(def-var! "jolt.ffi" "__read-bytes" ffi-read-bytes)
(def-var! "jolt.ffi" "__write-bytes" ffi-write-bytes)

;; --- byte-array buffer I/O (binary-faithful) --------------------------------
;; Move raw bytes between a jolt byte-array (jolt-array kind 'byte) and foreign
;; memory, byte-exact (no UTF-8 / latin1 decode) — for socket recv/send and the
;; zlib / OpenSSL buffers an HTTP client passes through. read-array returns a
;; fresh byte-array of n bytes; read-into! fills an EXISTING one (so a caller
;; that knows the total length up front reads a stream into one buffer instead
;; of regrowing an accumulator per chunk); write-array copies a byte-array's
;; bytes — all of them, or a slice — into ptr and returns the count. Foreign
;; memory is unsigned octets and a byte-array element is a signed byte — the same
;; fold ja-bv->bytes! / ja-bytes->bv! (natives-array.ss) own for every other
;; raw-byte seam, and a straight block move now that both sides are bytevectors.
(define (ffi-read-array ptr n)
  (let* ((n (jnum->exact n)) (p (jnum->exact ptr)) (bv (make-bytevector n)))
    (sa-foreign-bytes-ref! p bv n)
    (na-bv->bytearray bv)))

;; (read-into! ptr arr off n) -> n. Copy n bytes at ptr into arr starting at off
;; (the java.io.InputStream/read argument order). Throws rather than writing out
;; of bounds — a short read that silently truncated would corrupt the buffer.
(define (ffi-read-into! ptr arr off n)
  (let* ((n (jnum->exact n)) (off (jnum->exact off)) (p (jnum->exact ptr))
         (cap (ja-len arr)))
    (when (or (< off 0) (< n 0) (> (+ off n) cap))
      (jolt-throw (jolt-ex-info "jolt.ffi/read-into!: range outside the byte-array"
                                (jolt-hash-map (jolt-keyword "offset") off
                                               (jolt-keyword "length") n
                                               (jolt-keyword "capacity") cap))))
    (let ((bv (make-bytevector n)))
      (sa-foreign-bytes-ref! p bv n)
      (ja-bv->bytes! bv 0 arr off n)
      n)))

(define ffi-write-array
  (case-lambda
    ((ptr arr) (ffi-write-array ptr arr 0 (ja-len arr)))
    ((ptr arr off n)
     (let* ((n (jnum->exact n)) (off (jnum->exact off)) (p (jnum->exact ptr))
            (cap (ja-len arr)))
       (when (or (< off 0) (< n 0) (> (+ off n) cap))
         (jolt-throw (jolt-ex-info "jolt.ffi/write-array: range outside the byte-array"
                                   (jolt-hash-map (jolt-keyword "offset") off
                                                  (jolt-keyword "length") n
                                                  (jolt-keyword "capacity") cap))))
       (let ((bv (make-bytevector n)))
         (ja-bytes->bv! arr off bv 0 n)
         (sa-foreign-bytes-set! p bv n))
       n))))
(def-var! "jolt.ffi" "read-array" ffi-read-array)
(def-var! "jolt.ffi" "read-into!" ffi-read-into!)
(def-var! "jolt.ffi" "write-array" ffi-write-array)
;; …and under the reserved names, for the same reason as __read/__write below:
;; stdlib/jolt/ffi.clj defines read-array and write-array over these to add the
;; typed forms, so it needs a name for the primitive its own definition has not
;; taken.
(def-var! "jolt.ffi" "__read-array" ffi-read-array)
(def-var! "jolt.ffi" "__write-array" ffi-write-array)

;; --- string / bytevector marshaling ------------------------------------------
;; A C string result already comes back as a jolt string (the `string` foreign
;; type). For a `void*` that points at a NUL-terminated C string, read it here.
;;
;; A LIMIT bounds the scan: without one this walks to the first NUL wherever it
;; is, which on a buffer that has none reads past the allocation and can stop
;; the process. With one, a missing NUL inside `limit` bytes raises instead —
;; the caller knows how big the buffer is, so it can say so.
(define ffi-ptr->string
  (case-lambda
    ((ptr) (ffi-ptr->string* ptr #f))
    ((ptr limit)
     (ffi-ptr->string* ptr (if (jolt-nil? limit) #f (jnum->exact limit))))))
(define (ffi-ptr->string* ptr limit)
  (if (ffi-null? ptr) jolt-nil
      (let ((p (jnum->exact ptr)))
        (let loop ((i 0) (acc '()))
          (cond
            ((and limit (>= i limit))
             (jolt-throw (jolt-ex-info
                           "jolt.ffi/ptr->string: no NUL byte within the limit"
                           (jolt-hash-map (jolt-keyword "limit") limit))))
            (else
             (let ((b (sa-foreign-ref 'unsigned-8 p i)))
               (if (= b 0) (utf8->string (u8-list->bytevector (reverse acc)))
                   (loop (+ i 1) (cons b acc))))))))))
;; Copy a jolt string's UTF-8 bytes into a freshly alloc'd NUL-terminated buffer;
;; the caller frees it. Returns the pointer.
;;
;; nil answers NULL, allocating nothing, so it round-trips against ptr->string
;; above — which has always read NULL back as nil. Before this the pair lost the
;; distinction in one direction: nil went through jolt-str-render-one, which
;; renders it "", so it came back as "" and an absent string was indistinguishable
;; from a present empty one. "" itself still allocates its one NUL byte and still
;; reads back as "", which is what keeps the two answers apart.
;;
;; Every value that is not nil keeps going through jolt-str-render-one, the `str`
;; coercion: with-c-string documents "Copy VALUE", not "copy a string", and
;; with-c-string-array maps over arbitrary values. Only nil is special, for the
;; same reason it is special in a :string position — it is what jolt spells
;; absence with, and NULL is what C spells it with.
;;
;; NULL is safe for both callers to free: free(NULL) is a defined no-op, and
;; ffi/free reaches it through Chez's foreign-free, which accepts 0.
;; The octets string->ptr copies for a non-nil value. A BYTE-ARRAY is data and
;; copies its own octets, the rule write-bytes follows (#1101): rendering it with
;; `str` allocated the characters of "#object[[B]" in place of its bytes, and the
;; size accounting above agreed with the wrong copy. Every other value is the
;; UTF-8 of its `str` form.
(define (ffi-value-octets s)
  (if (na-bytes? s)
      (let* ((n (ja-len s)) (bv (make-bytevector n)))
        (ja-bytes->bv! s 0 bv 0 n)
        bv)
      (string->utf8 (jolt-str-render-one s))))

(define (ffi-string->ptr s)
  (if (jolt-nil? s)
      ffi-null
      ;; ONE block move plus the terminator. This used to be a per-byte
      ;; sa-foreign-set! loop, ~30ns a byte across the boundary — the cost this
      ;; file's buffer-I/O section exists to avoid, on the one path that had
      ;; kept it.
      (let* ((bv (ffi-value-octets s))
             (n (bytevector-length bv))
             (p (sa-foreign-alloc (+ n 1))))
        ;; free on a mid-copy throw — the caller only ever sees a whole buffer
        (guard (e (#t (guard (_ (#t #f)) (sa-foreign-free p)) (raise e)))
          (when (> n 0) (sa-foreign-bytes-set! p bv n))
          (sa-foreign-set! 'unsigned-8 p n 0)
          p))))

;; The octet count string->ptr just copied, less the NUL, so an arena can record
;; the block's size without scanning for the NUL again.
(define (ffi-utf8-length s)
  (if (jolt-nil? s) 0 (bytevector-length (ffi-value-octets s))))

;; --- bare :& — per-call variadic tail inference ------------------------------
;; babashka.ffi's BARE :& is one binding serving every tail shape: no types
;; follow the marker, and each call's tail is read off the values it is given.
;;
;;   (ffi/defcfn c-open "open" [:string :int :&] :int)
;;   (c-open path O_RDONLY)        ; empty tail
;;   (c-open path flags #o644)     ; one-int tail, same binding
;;
;; A Chez foreign-procedure has its argument and result types fixed when it is
;; COMPILED, so a call has nothing to compile a new one from. The binding is
;; therefore a DISPATCHER over a cache of compiled procedures, one per observed
;; tail shape, rather than a single procedure: on a miss it builds the
;; foreign-procedure for that shape by eval'ing the form — jolt carries its own
;; compiler, in a `jolt build --release` binary too — and keeps it.
;;
;; Three carriers, as in babashka.ffi, which is what keeps the shape space small:
;; integers, pointers, booleans and nil travel as 64-bit integers; C's default
;; argument promotions send floats as doubles, and a ratio with them; strings as
;; C strings. A tail of length k has 3^k shapes — too many to emit eagerly, few
;; enough that a program hits one or two.
;;
;; COST, measured end to end from jolt against a do-nothing C variadic on Chez
;; 10.4.1 (a fixed binding to the same callee is 20ns, a declared tail 20.4ns):
;;
;;     bare, no tail        26.5ns        bare, one-value tail   34ns
;;     bare, two-value tail 41.5ns        compile, per shape     ~0.8ms once
;;
;; So a bare marker costs about +13ns and 1.6x a declared tail, against the
;; "roughly twice as fast" babashka.ffi's own guide gives for declaring one.
;; Getting there took three things, each measured rather than assumed:
;;
;;   - COMPILE the form, do not interpret it. Interpreting builds 2.5x faster
;;     (0.32ms) and then calls 3x SLOWER for the life of the process (22.9
;;     against 6.9ns at the raw Chez level).
;;   - Let the emitted binding be a case-lambda with arity-specialized arms
;;     rather than a rest-argument lambda, so a short tail allocates no list,
;;     is not walked twice, and is not spread back with `apply`. Worth 12ns of
;;     the 36ns a rest lambda cost.
;;   - Test for a fixnum FIRST in the carrier and the marshaller, and scan the
;;     cache with eq? over fixnum keys instead of assv. Worth another 10ns.
;;
;; An alist beats a hashtable at the one to three entries a binding really has
;; (8.2 against 11.5ns), and the one-entry inline cache in front of it answers
;; the monomorphic case without walking at all.
;;
;; Resolution is a declared :jolt/native's own dlopen handle first and the
;; process-global table second, which is what every other binding does and what
;; both spellings of the marker now do.

;; #(name fixed-types ret-type fixed-count capture? entries mutex last)
;;
;; `last` is a one-entry inline cache: the (key . proc) pair the previous call
;; used. Almost every binding is monomorphic — one call site, one tail shape —
;; so this answers before the list is walked at all. It holds the PAIR in a
;; single slot rather than a key and a procedure in two: two slots could be read
;; torn under concurrency, pairing one shape's key with another shape's
;; procedure, and calling a foreign procedure with the wrong argument types
;; corrupts memory rather than raising. One slot is one word, so a reader sees
;; either the old pair or the new one.
(define (jolt-ffi-varargs-cache name fixed-types ret-type fixed-count capture?)
  (vector name fixed-types ret-type fixed-count capture? '() (make-mutex) #f))
(define (ffi-vc-name vc)        (vector-ref vc 0))
(define (ffi-vc-fixed-types vc) (vector-ref vc 1))
(define (ffi-vc-ret vc)         (vector-ref vc 2))
(define (ffi-vc-fixed-count vc) (vector-ref vc 3))
(define (ffi-vc-capture? vc)    (vector-ref vc 4))
(define (ffi-vc-entries vc)     (vector-ref vc 5))
(define (ffi-vc-entries! vc e)  (vector-set! vc 5 e))
(define (ffi-vc-mutex vc)       (vector-ref vc 6))
(define (ffi-vc-last vc)        (vector-ref vc 7))
(define (ffi-vc-last! vc e)     (vector-set! vc 7 e))

;; The carrier a tail value travels in, as a small integer: 0 = 64-bit integer,
;; 1 = double, 2 = C string. `integer?` is true of 3.0, so the flonum test comes
;; first; an exact non-integer is a ratio, which promotes to double.
(define (ffi-varargs-carrier v)
  (cond
    ;; A fixnum is the overwhelmingly common tail value (a flag, a mode, an
    ;; ioctl request), so it answers on the first test rather than the fourth.
    ((fixnum? v) 0)
    ((flonum? v) 1)
    ((jfloat? v) 1)
    ((string? v) 2)
    ((number? v) (if (and (exact? v) (integer? v)) 0 1))
    ((jolt-nil? v) 0)
    ((boolean? v) 0)
    (else
     (throw-jvm 'IllegalArgumentException
                (string-append
                 "jolt.ffi: a bare :& tail takes an integer, pointer, boolean, nil,"
                 " floating-point number, ratio or string; got " (jolt-pr-str v))))))

;; The Chez foreign type each carrier declares.
(define (ffi-varargs-carrier-type c)
  (cond ((fx=? c 0) 'integer-64) ((fx=? c 1) 'double) (else 'string)))

;; One fixnum standing for the whole tail shape, folded base-3 from a seed of 1
;; so that tails of DIFFERENT LENGTHS cannot collide (0 shapes => 1, one-int =>
;; 3, one-double => 4, ...). Cheaper to build and to compare than a list of
;; symbols, and it is what the cache is keyed on.
(define (ffi-varargs-shape-key tail)
  (let loop ((t tail) (k 1))
    (if (null? t) k (loop (cdr t) (+ (* k 3) (ffi-varargs-carrier (car t)))))))

(define (ffi-varargs-shape-types tail)
  (if (null? tail)
      '()
      (cons (ffi-varargs-carrier-type (ffi-varargs-carrier (car tail)))
            (ffi-varargs-shape-types (cdr tail)))))

;; A value Chez already accepts in its carrier's position needs no conversion,
;; and the common tail is entirely such values — so the marshalled list is the
;; SAME list when nothing changed, and a variadic call allocates nothing beyond
;; the rest argument the lambda already built.
(define (ffi-varargs-plain? v)
  (or (flonum? v) (string? v) (and (number? v) (exact? v) (integer? v))))

(define (ffi-varargs-marshal v)
  (cond
    ;; Same ordering as the carrier: the values that need no conversion at all
    ;; are the common ones, and they answer first.
    ((fixnum? v) v)
    ((flonum? v) v)
    ((jfloat? v) (jfloat-fl v))
    ((string? v) v)
    ((jolt-nil? v) 0)
    ((eq? v #t) 1)
    ((eq? v #f) 0)
    ((number? v) (if (and (exact? v) (integer? v)) v (inexact v)))
    (else v)))

(define (jolt-ffi-varargs-tail tail)
  (cond
    ((null? tail) tail)
    ((ffi-varargs-plain? (car tail))
     (let ((rest (jolt-ffi-varargs-tail (cdr tail))))
       (if (eq? rest (cdr tail)) tail (cons (car tail) rest))))
    (else (cons (ffi-varargs-marshal (car tail))
                (jolt-ffi-varargs-tail (cdr tail))))))

;; One tail value, for the arity-specialized arms below, which hand their values
;; over as ARGUMENTS and so never build a list to walk.
(define (jolt-ffi-varargs-arg v) (ffi-varargs-marshal v))

;; Windows x64 passes named and variadic arguments alike, and its runtime
;; (eval) construction has no slot for a convention — the same reason
;; jolt-foreign-proc-safe drops it there.
(define (ffi-varargs-convention n)
  (if (eq? (sa-os-family) 'windows) '() (list (list '__varargs_after n))))

;; :capture-native-error rides in front of the calling convention, as it does in
;; the compiled path (jolt-ffi-native-error-procedure). The convention is the
;; platform's, decided here at run time rather than at expansion time.
(define (ffi-varargs-error-convention)
  (if (eq? (sa-os-family) 'windows) '(__get_last_error) '(__errno)))

(define (ffi-varargs-compile vc shape-types)
  (let ((name (ffi-vc-name vc)))
    (eval (append (list 'foreign-procedure)
                  (if (ffi-vc-capture? vc) (ffi-varargs-error-convention) '())
                  (ffi-varargs-convention (ffi-vc-fixed-count vc))
                  (list
                   ;; A declared :jolt/native is dlopen'd RTLD_LOCAL, so its
                   ;; symbols are NOT in the process-global table a name
                   ;; resolves through -- build against the address its own
                   ;; handle answers when there is one, exactly as every
                   ;; non-variadic binding does, and fall back to the name.
                   (or (jolt-ffi-dlsym-native name) name)
                   (append (ffi-vc-fixed-types vc) shape-types)
                   (ffi-vc-ret vc))))))

;; Reads are unlocked: entries is one variable holding an immutable alist, so a
;; reader sees either the old list or the new one. The COMPILE is serialized and
;; re-checks the cache under the lock, so two threads meeting the same new shape
;; compile it once instead of racing to drop one of the two entries.
;; The name the build's compiler verdict knows this path by (dce.ss
;; dce-compile-refs): a program with a bare :& binding compiles here at run
;; time, which petite cannot, so the build keeps the compiler for it. The
;; dispatcher reaches the procedure directly; the var is its name.
(def-var! "jolt.host" "ffi-varargs-compile" ffi-varargs-compile)

(define (ffi-varargs-hit vc key)
  (let ((e (assv key (ffi-vc-entries vc)))) (and e (cdr e))))

;; The specialized arms key on a small fixnum (53 is the largest a three-value
;; tail can fold to), so their scan compares with eq? and open-codes the walk
;; instead of paying assv's eqv? per entry. A bignum key from the general arm
;; can sit in the same list and is simply never eq? to one of these, which is
;; the right answer: the shapes differ.
(define (ffi-varargs-hit-fx vc key)
  (let ((last (ffi-vc-last vc)))
    (if (and last (eq? (car last) key))
        (cdr last)
        (let loop ((e (ffi-vc-entries vc)))
          (cond ((null? e) #f)
                ((eq? (caar e) key)
                 (let ((entry (car e))) (ffi-vc-last! vc entry) (cdr entry)))
                (else (loop (cdr e))))))))

(define (ffi-varargs-miss vc key shape-types)
  (jolt-with-mutex (ffi-vc-mutex vc)
    (let ((again (assv key (ffi-vc-entries vc))))
      (if again
          (cdr again)
          (let ((proc (ffi-varargs-compile vc shape-types)))
            (ffi-vc-entries! vc (cons (cons key proc) (ffi-vc-entries vc)))
            proc)))))

;; The general arm: a tail of any length, arriving as a list.
(define (jolt-ffi-varargs-procedure vc tail)
  (let ((key (ffi-varargs-shape-key tail)))
    (or (ffi-varargs-hit vc key)
        (ffi-varargs-miss vc key (ffi-varargs-shape-types tail)))))

;; --- the arity-specialized lookups -------------------------------------------
;; A tail of nought to three values is the whole of real usage — open(2) takes
;; one, an ioctl one, a printf-alike a handful — and for those the emitted
;; binding is a case-lambda whose arms take the tail as ARGUMENTS. That removes,
;; per call, the rest-argument list Chez would otherwise allocate, the two walks
;; over it (one to key the shape, one to marshal), and the `apply`. These are the
;; matching lookups: the same base-3 fold as ffi-varargs-shape-key, unrolled, so
;; an arm and the general path agree on the key for the same shape.
(define (jolt-ffi-varargs-proc0 vc)
  (or (ffi-varargs-hit-fx vc 1) (ffi-varargs-miss vc 1 '())))

(define (jolt-ffi-varargs-proc1 vc t1)
  (let* ((c1 (ffi-varargs-carrier t1))
         (key (fx+ 3 c1)))
    (or (ffi-varargs-hit-fx vc key)
        (ffi-varargs-miss vc key (list (ffi-varargs-carrier-type c1))))))

(define (jolt-ffi-varargs-proc2 vc t1 t2)
  (let* ((c1 (ffi-varargs-carrier t1))
         (c2 (ffi-varargs-carrier t2))
         (key (fx+ (fx+ 9 (fx* 3 c1)) c2)))
    (or (ffi-varargs-hit-fx vc key)
        (ffi-varargs-miss vc key (list (ffi-varargs-carrier-type c1)
                                       (ffi-varargs-carrier-type c2))))))

(define (jolt-ffi-varargs-proc3 vc t1 t2 t3)
  (let* ((c1 (ffi-varargs-carrier t1))
         (c2 (ffi-varargs-carrier t2))
         (c3 (ffi-varargs-carrier t3))
         (key (fx+ (fx+ (fx+ 27 (fx* 9 c1)) (fx* 3 c2)) c3)))
    (or (ffi-varargs-hit-fx vc key)
        (ffi-varargs-miss vc key (list (ffi-varargs-carrier-type c1)
                                       (ffi-varargs-carrier-type c2)
                                       (ffi-varargs-carrier-type c3))))))

;; --- a java.nio.ByteBuffer over foreign memory -------------------------------
;; jolt.ffi/byte-buffer answers a DIRECT java.nio.ByteBuffer view of `n` bytes at
;; `p` — the buffer and the pointer share the same bytes, as babashka.ffi's does,
;; so a put through the buffer is a write to the pointer. The buffer keeps
;; nothing alive: using one after the memory is released reads freed memory. The
;; representation and the accessors are in host/chez/java/byte-buffer.ss.
(define (ffi-byte-buffer p n)
  (make-direct-byte-buffer (jnum->exact p) (jnum->exact n)))

;; --- callbacks: receive calls FROM C ----------------------------------------
;; jolt.ffi/foreign-callable lowers to (jolt-ffi-register-callable! (foreign-callable …)).
;; A foreign-callable code object must be LOCKED (so the collector neither moves
;; nor reclaims it) and RETAINED while C may still call through its entry point.
;; Register it keyed by that entry-point address (a jolt pointer integer) — which
;; is what the caller hands to C; free-callable unlocks and drops it. A callback
;; left registered lives for the process (the GTK-signal-handler common case).
;; Both tables are written at RUN time — __ccallable mints a callback and
;; ffi-export registers a name — from whatever thread does it, so every mutation
;; takes this mutex. Single-key reads stay unlocked.
(define ffi-tbl-mu (make-mutex))
(define ffi-callable-table (make-eqv-hashtable))   ; entry-point addr -> code object
(define (jolt-ffi-register-callable! co)
  (sa-lock-object co)
  (let ((addr (sa-foreign-callable-entry-point co)))
    (jolt-with-mutex ffi-tbl-mu (hashtable-set! ffi-callable-table addr co))
    addr))
;; A :collect-safe callable's body runs between these (backend emit-ffi-callable
;; wraps it): rt.ss jolt-ffi-callbacks-active counts the callbacks in progress
;; so a stalled collection's report (jolt-report-gc-stall) can say whether one
;; is among the threads waiting for it. A CAS per entry and exit — several
;; foreign threads can be inside callbacks at once — and nothing on the
;; foreign-call side. A callback returns to C by returning (a continuation
;; cannot cross the foreign frame), so the exit always runs.
(define (jolt-ffi-callback-enter!)
  (let retry ()
    (let ((n (unbox jolt-ffi-callbacks-active)))
      (unless (box-cas! jolt-ffi-callbacks-active n (fx+ n 1)) (retry)))))
(define (jolt-ffi-callback-exit!)
  (let retry ()
    (let ((n (unbox jolt-ffi-callbacks-active)))
      (unless (box-cas! jolt-ffi-callbacks-active n (fx- n 1)) (retry)))))
(define (ffi-free-callable addr)
  (let* ((a (jnum->exact addr))
         (co (jolt-with-mutex ffi-tbl-mu
               ;; take-and-remove as one step, so two frees of the same address
               ;; cannot both unlock the object
               (let ((c (hashtable-ref ffi-callable-table a #f)))
                 (when c (hashtable-delete! ffi-callable-table a))
                 c))))
    (when co (sa-unlock-object co))
    jolt-nil))

;; --- library exports: name -> entry-point address ---------------------------
;; `jolt build --library` publishes C-callable entry points under names so an
;; embedder resolves them via the stub's jolt_lookup(name). export! wraps a jolt
;; fn as a foreign-callable (locked + retained as above) and records name->addr
;; here. The built library's scheme-start handler wraps THIS lookup as a single
;; C-callable and hands its address to the stub (jolt_set_lookup_addr), so
;; jolt_lookup(name) reads this table. export! only touches Scheme state, so it
;; also runs harmlessly during the build's app load (the table is discarded).
;; NOTE: keyed with equal? (make-hashtable) not eq? — keys are strings, and the
;; app's "add" and the lookup's C-string-derived "add" are different objects, so
;; eq?-hashtable would always miss. (ffi-callable-table above is eq?-keyed but
;; keyed by integer addresses, where eq? is correct.)
(define ffi-export-table (make-hashtable string-hash equal?))  ; name(string) -> addr(integer)
(define (jolt-ffi-register-export! name addr)
  (jolt-with-mutex ffi-tbl-mu (hashtable-set! ffi-export-table name addr)) addr)
;; lookup for the C stub: name (a Scheme string) -> addr, or 0 if unknown.
(define (jolt-ffi-lookup-export name)
  (let ((a (hashtable-ref ffi-export-table name #f))) (if a a 0)))
;; export! is a MACRO in stdlib/jolt/ffi.clj (it needs compile-time-typed
;; argtypes to build the foreign-callable, like foreign-callable). It expands to
;; (jolt.ffi/register-export name (jolt.ffi/__ccallable f [argtypes] rettype)),
;; so the callable is built with literal types and register-export records
;; name -> its entry-point address here.

;; --- native libraries for a standalone binary -------------------------------
;; `jolt build` bakes a project's deps.edn :jolt/native declarations into the
;; launcher, which loads them at startup (load-shared-object isn't part of the
;; saved heap, so it must run in the built process, not at heap build). process?
;; loads the running binary's own symbols (libc sockets); otherwise try each
;; platform candidate in turn and fail unless the spec is optional. A file native
;; is loaded RTLD_LOCAL + registered via jolt-ffi-load-native so its defcfns
;; resolve from the handle, isolated from the global namespace; only when dlopen
;; is unavailable on the host does it fall back to the global sa-load-shared-object.
(define (jolt-build-load-native cands optional? process?)
  (if process?
      ;; :process natives want the executable's own symbols — already resolvable
      ;; through the boot-time global load; re-loading #f would re-promote the
      ;; global handle over every scoped native (the bug this file exists to end).
      #t
      (let loop ((cs cands))
        (cond
          ((null? cs)
           (unless optional?
             (let ((note (ffi-load-failure-note cands)))
               (if note
                   (error 'jolt-build (string-append "required native library did not load — " note) cands)
                   (error 'jolt-build "required native library not found" cands))))
           #f)
          ;; RTLD_LOCAL + register; #t when it took (handle or Windows-global).
          ((jolt-ffi-load-native (car cs)) #t)
          (else (loop (cdr cs)))))))

;; --- find-symbol ------------------------------------------------------------
;; The address of `sym`, or nil when nothing defines it. Searches the declared
;; natives first (declaration order, per-handle dlsym — the same resolution a
;; defcfn gets) and then the process's own symbols, so the answer matches where
;; a binding of that name would actually land.
(define (ffi-find-symbol sym)
  (let* ((n (ffi-str-arg "symbol" sym))
         (a (jolt-ffi-dlsym-native n)))
    (cond
      (a a)
      ((guard (e (#t #f)) (sa-foreign-entry? n)) (sa-foreign-entry-address n))
      (else jolt-nil))))

;; --- automatic arenas: release when the collector reclaims the arena --------
;; jolt.ffi/auto-arena hands back an arena nobody closes; its memory is released
;; once the arena itself is unreachable. A guardian is how that is observable:
;; register the arena's state cell, and the collector hands the cell BACK here
;; after it becomes garbage, still readable, with the addresses to free in it.
;;
;; The collector does not call us — so the pending cells are drained at the FFI
;; points that mean a program is still allocating (creating an arena, allocating
;; in an automatic one). An automatic arena is therefore released promptly in
;; the code that keeps using arenas and, in code that stops, not until the
;; process ends — which is the same memory the process was going to return
;; anyway. jolt.ffi/drain-auto-arenas! forces a drain for a caller that wants one
;; at a specific point.
;;
;; Guardians are global mutable state and a drain both reads and mutates one, so
;; every access takes this mutex: an automatic arena can become garbage on any
;; thread, and two threads draining at once corrupts the guardian's own list.
;; --- recorded pointer sizes --------------------------------------------------
;; jolt.ffi/size answers the size jolt was TOLD a pointer addresses. A jolt
;; pointer is a bare address, so a size cannot ride along with the value the way
;; a MemorySegment carries its own — it lives here, keyed by address, and the
;; entry has to be removed when the memory is released or the next allocation to
;; land on that address inherits it.
;;
;; SHARDED, unlike the callable table above. That one is written when a callback
;; is minted; this one is written by every allocation, every string->ptr and
;; every declared view, from whatever thread is running — one lock over one
;; table would serialize allocation across the whole process. Chez hashtables
;; are not safe for concurrent writers (two resizing at once faults inside the
;; collector), so each shard owns its mutex and nothing is written outside it.
;; Single-key reads stay unlocked, as they do for the callable table.
;;
;; The index drops the low four bits before masking: every allocator underneath
;; this file hands back 16-aligned addresses, so those bits are always zero and
;; masking them directly would put every entry in shard 0.
(define ffi-size-shard-count 16)
(define ffi-size-tables (make-vector ffi-size-shard-count))
(define ffi-size-mutexes (make-vector ffi-size-shard-count))
(let loop ((i 0))
  (when (fx<? i ffi-size-shard-count)
    (vector-set! ffi-size-tables i (make-eqv-hashtable))
    (vector-set! ffi-size-mutexes i (make-mutex))
    (loop (fx+ i 1))))

(define (ffi-size-shard-index a)
  (bitwise-and (bitwise-arithmetic-shift-right a 4) (- ffi-size-shard-count 1)))

(define (jolt-ffi-remember-size! p n)
  (let* ((a (jnum->exact p))
         (i (ffi-size-shard-index a)))
    (jolt-with-mutex (vector-ref ffi-size-mutexes i)
      (hashtable-set! (vector-ref ffi-size-tables i) a (jnum->exact n)))
    p))

(define (jolt-ffi-recorded-size p)
  (let* ((a (jnum->exact p))
         (i (ffi-size-shard-index a)))
    (hashtable-ref (vector-ref ffi-size-tables i) a 0)))

;; Batch, because an arena close forgets its whole group at once and a per-entry
;; crossing of the jolt/host boundary is the expensive part, not the delete.
(define (jolt-ffi-forget-sizes! ps)
  (for-each
    (lambda (p)
      (let* ((a (jnum->exact p))
             (i (ffi-size-shard-index a)))
        (jolt-with-mutex (vector-ref ffi-size-mutexes i)
          (hashtable-delete! (vector-ref ffi-size-tables i) a))))
    (seq->list (jolt-seq ps)))
  jolt-nil)

(define ffi-auto-mu (make-mutex))
(define ffi-auto-guardian (make-guardian))
(define (jolt-ffi-auto-guard! token)
  (jolt-with-mutex ffi-auto-mu (ffi-auto-guardian token))
  token)
;; Every state cell the collector has reclaimed since the last drain, as a jolt
;; vector. Resurrected by the guardian, so reading each one is safe.
(define (jolt-ffi-auto-reclaimed)
  (make-pvec
   (list->vector
    (jolt-with-mutex ffi-auto-mu
      (let loop ((acc '()))
        (let ((t (ffi-auto-guardian)))
          (if t (loop (cons t acc)) acc)))))))

;; --- expose under jolt.ffi ---------------------------------------------------
(def-var! "jolt.ffi" "free-callable" ffi-free-callable)
(def-var! "jolt.ffi" "register-export" jolt-ffi-register-export!)
(def-var! "jolt.ffi" "load-library" ffi-load-library)
(def-var! "jolt.ffi" "loaded?" (lambda (n) (if (ffi-loaded? n) #t #f)))
(def-var! "jolt.ffi" "load-native" jolt-ffi-load-native)
(def-var! "jolt.ffi" "defining-libraries" jolt-ffi-defining-libraries)
;; jolt.ffi/on-gc-stall's native half (rt.ss jolt-gc-stall-set-reporter!): F
;; nil restores the default report; otherwise F gets the report as a map.
(def-var! "jolt.ffi" "__gc-stall-reporter!"
  (lambda (f seconds)
    (jolt-gc-stall-set-reporter!
      (and (not (jolt-nil? f))
           (lambda (secs threads callbacks message)
             (jolt-invoke1 f (jolt-hash-map (jolt-keyword "seconds") secs
                                            (jolt-keyword "threads") threads
                                            (jolt-keyword "callbacks") callbacks
                                            (jolt-keyword "message") message))))
      (and (not (jolt-nil? seconds)) seconds))))
(def-var! "jolt.ffi" "dlsym-native" jolt-ffi-dlsym-native)
(def-var! "jolt.ffi" "load-system-library" ffi-load-system-library)
;; jolt.main/load-natives! reads it to build the fallback candidates for a spec
;; that names a library but declares nothing for the running platform (#989).
(def-var! "jolt.ffi" "system-library-candidates"
  (lambda (n) (list->cseq (ffi-system-library-candidates
                           (ffi-str-arg "system-library-candidates name" n)))))
;; jolt.main/load-natives! appends it to a missing-library report, so a
;; candidate that is on disk but failed to load is not reported as not found.
(def-var! "jolt.ffi" "load-failure-note"
  (lambda (cands)
    (or (ffi-load-failure-note
         (map (lambda (c) (ffi-str-arg "load-failure-note candidate" c))
              (seq->list (jolt-seq cands))))
        jolt-nil)))
(def-var! "jolt.ffi" "find-symbol" ffi-find-symbol)
(def-var! "jolt.ffi" "alloc" ffi-alloc)
(def-var! "jolt.ffi" "free" ffi-free)
(def-var! "jolt.ffi" "read" ffi-read)
(def-var! "jolt.ffi" "write" ffi-write)
(def-var! "jolt.ffi" "copy" ffi-copy)
(def-var! "jolt.ffi" "sizeof" ffi-sizeof)
;; The scalar primitives under reserved names. stdlib/jolt/ffi.clj DEFINES
;; jolt.ffi/alloc, read, write and sizeof over these — the public four take a
;; layout, a place, or an arena, and resolve down to one of these — so the
;; wrapper needs a name for the primitive that its own definition has not taken.
;; A host-level gate (test/chez/ffi-*.ss loads this file alone) keeps using the
;; public names, which is why both are registered.
;; __free is the raw deallocator. The public jolt.ffi/free is a stdlib wrapper
;; that also forgets the pointer's recorded size, the same way __read/__write sit
;; under the layout-aware read/write.
(def-var! "jolt.ffi" "__remember-size!" jolt-ffi-remember-size!)
(def-var! "jolt.ffi" "__forget-sizes!" jolt-ffi-forget-sizes!)
(def-var! "jolt.ffi" "__size" jolt-ffi-recorded-size)
(def-var! "jolt.ffi" "__free" ffi-free)
(def-var! "jolt.ffi" "__alloc" ffi-alloc)
(def-var! "jolt.ffi" "__calloc" ffi-calloc)
(def-var! "jolt.ffi" "__read" ffi-read)
(def-var! "jolt.ffi" "__write" ffi-write)
(def-var! "jolt.ffi" "__sizeof" ffi-sizeof)
(def-var! "jolt.ffi" "__copy" ffi-copy)
(def-var! "jolt.ffi" "__auto-guard!" jolt-ffi-auto-guard!)
(def-var! "jolt.ffi" "__auto-reclaimed" jolt-ffi-auto-reclaimed)
(def-var! "jolt.ffi" "null?" (lambda (p) (if (ffi-null? p) #t #f)))
(def-var! "jolt.ffi" "null" ffi-null)
(def-var! "jolt.ffi" "ptr->string" ffi-ptr->string)
(def-var! "jolt.ffi" "string->ptr" ffi-string->ptr)
(def-var! "jolt.ffi" "__string->ptr" ffi-string->ptr)
(def-var! "jolt.ffi" "__utf8-length" ffi-utf8-length)
(def-var! "jolt.ffi" "__byte-buffer" ffi-byte-buffer)
