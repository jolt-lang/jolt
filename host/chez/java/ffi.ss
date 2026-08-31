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
                                    (ffi-join-candidates cands))
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
  ;; LD_LIBRARY_PATH first (it wins for the loader too), then the standard
  ;; prefixes, then Debian/Ubuntu multiarch subdirectories, discovered by
  ;; listing rather than by naming a triple this build cannot know.
  (let* ((env (or (guard (e (#t #f)) (getenv "LD_LIBRARY_PATH")) ""))
         (from-env (if (string=? env "") '() (ffi-split-colons env)))
         (roots '("/usr/local/lib" "/usr/lib" "/lib" "/usr/lib64" "/lib64"))
         (multi (apply append
                       (map (lambda (root)
                              (map (lambda (d) (string-append root "/" d))
                                   (filter (lambda (d) (ffi-multiarch-dir? d))
                                           (ffi-dir-entries root))))
                            '("/usr/lib" "/lib")))))
    (append from-env roots multi)))

(define (ffi-split-colons str)
  (let loop ((cs (string->list str)) (cur '()) (acc '()))
    (cond
      ((null? cs)
       (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
      ((char=? (car cs) #\:)
       (loop (cdr cs) '() (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
      (else (loop (cdr cs) (cons (car cs) cur) acc)))))

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

(define (ffi-split-dots str)
  (let loop ((cs (string->list str)) (cur '()) (acc '()))
    (cond
      ((null? cs)
       (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
      ((char=? (car cs) #\.)
       (loop (cdr cs) '() (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
      (else (loop (cdr cs) (cons (car cs) cur) acc)))))

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

(define (ffi-load-system-library name)
  (let ((n (ffi-str-arg "load-system-library name" name)))
    (ffi-library-map
     (ffi-load-candidates
      (case (sa-os-family)
        ((macos)   (list (string-append "lib" n ".dylib") (string-append n ".dylib")))
        ((windows) (list (string-append n ".dll") (string-append "lib" n ".dll")))
        (else      (let ((base (string-append "lib" n ".so")))
                     (cons base (ffi-versioned-sonames base)))))))))

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
     (guard (e (#t #f)) (sa-load-shared-object path) #t))
    ((not ffi-dlopen) #f)
    (else
     (jolt-with-mutex ffi-native-mu
       (cond
         ((hashtable-ref ffi-native-paths path #f) #t)   ; already loaded
         (else
          (let ((h (ffi-dlopen path ffi-rtld-flags)))
             (cond
              ((and h (integer? h) (positive? h))
               (hashtable-set! ffi-native-paths path #t)
               (vector-set! ffi-native-handles 0
                            (append (or (vector-ref ffi-native-handles 0) '())
                                    (list (cons path h))))
               h)
              (else #f)))))))))
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
  (when (ffi-dup-claim! sym)
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
    (if (eq? ct 'jolt-bool)
        (sa-foreign-set! 'unsigned-8 (jnum->exact ptr) (jnum->exact off) (jolt-ffi-bool->c val))
        (sa-foreign-set! ct (jnum->exact ptr) (jnum->exact off) val)))
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

;; --- byte-array buffer I/O (binary-faithful) --------------------------------
;; Move raw bytes between a jolt byte-array (jolt-array kind 'byte) and foreign
;; memory, byte-exact (no UTF-8 / latin1 decode) — for socket recv/send and the
;; zlib / OpenSSL buffers an HTTP client passes through. read-array returns a
;; fresh byte-array of n bytes; read-into! fills an EXISTING one (so a caller
;; that knows the total length up front reads a stream into one buffer instead
;; of regrowing an accumulator per chunk); write-array copies a byte-array's
;; bytes — all of them, or a slice — into ptr and returns the count. Foreign
;; memory is unsigned octets and a byte-array element is a signed byte, so the
;; two directions fold and mask across that seam (bytevector-s8-ref/-u8-set!
;; are that fold).
(define (ffi-bv->byte-vec! bv v off n)
  (do ((i 0 (+ i 1))) ((= i n)) (vector-set! v (+ off i) (bytevector-s8-ref bv i))))
(define (ffi-byte-vec->bv! v off n)
  (let ((bv (make-bytevector n)))
    (do ((i 0 (+ i 1))) ((= i n))
      (bytevector-u8-set! bv i (bitwise-and (exact (vector-ref v (+ off i))) #xff)))
    bv))

(define (ffi-read-array ptr n)
  (let* ((n (jnum->exact n)) (p (jnum->exact ptr)) (bv (make-bytevector n)) (v (make-vector n 0)))
    (sa-foreign-bytes-ref! p bv n)
    (ffi-bv->byte-vec! bv v 0 n)
    (make-jolt-array v 'byte)))

;; (read-into! ptr arr off n) -> n. Copy n bytes at ptr into arr starting at off
;; (the java.io.InputStream/read argument order). Throws rather than writing out
;; of bounds — a short read that silently truncated would corrupt the buffer.
(define (ffi-read-into! ptr arr off n)
  (let* ((n (jnum->exact n)) (off (jnum->exact off)) (p (jnum->exact ptr))
         (v (jolt-array-vec arr)) (cap (vector-length v)))
    (when (or (< off 0) (< n 0) (> (+ off n) cap))
      (jolt-throw (jolt-ex-info "jolt.ffi/read-into!: range outside the byte-array"
                                (jolt-hash-map (jolt-keyword "offset") off
                                               (jolt-keyword "length") n
                                               (jolt-keyword "capacity") cap))))
    (let ((bv (make-bytevector n)))
      (sa-foreign-bytes-ref! p bv n)
      (ffi-bv->byte-vec! bv v off n)
      n)))

(define ffi-write-array
  (case-lambda
    ((ptr arr)
     (let ((v (jolt-array-vec arr)))
       (ffi-write-array ptr arr 0 (vector-length v))))
    ((ptr arr off n)
     (let* ((n (jnum->exact n)) (off (jnum->exact off)) (p (jnum->exact ptr))
            (v (jolt-array-vec arr)) (cap (vector-length v)))
       (when (or (< off 0) (< n 0) (> (+ off n) cap))
         (jolt-throw (jolt-ex-info "jolt.ffi/write-array: range outside the byte-array"
                                   (jolt-hash-map (jolt-keyword "offset") off
                                                  (jolt-keyword "length") n
                                                  (jolt-keyword "capacity") cap))))
       (sa-foreign-bytes-set! p (ffi-byte-vec->bv! v off n) n)
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
(define (ffi-string->ptr s)
  (if (jolt-nil? s)
      ffi-null
      ;; ONE block move plus the terminator. This used to be a per-byte
      ;; sa-foreign-set! loop, ~30ns a byte across the boundary — the cost this
      ;; file's buffer-I/O section exists to avoid, on the one path that had
      ;; kept it.
      (let* ((bv (string->utf8 (jolt-str-render-one s)))
             (n (bytevector-length bv))
             (p (sa-foreign-alloc (+ n 1))))
        ;; free on a mid-copy throw — the caller only ever sees a whole buffer
        (guard (e (#t (guard (_ (#t #f)) (sa-foreign-free p)) (raise e)))
          (when (> n 0) (sa-foreign-bytes-set! p bv n))
          (sa-foreign-set! 'unsigned-8 p n 0)
          p))))

;; The UTF-8 byte length of a value rendered as a string — what string->ptr just
;; allocated, so an arena can record the block's size without scanning for the
;; NUL again.
(define (ffi-utf8-length s)
  (if (jolt-nil? s) 0 (bytevector-length (string->utf8 (jolt-str-render-one s)))))

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
             (error 'jolt-build "required native library not found" cands))
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
(def-var! "jolt.ffi" "dlsym-native" jolt-ffi-dlsym-native)
(def-var! "jolt.ffi" "load-system-library" ffi-load-system-library)
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
