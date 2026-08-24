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
    ;; naming the platform, rather than silently loading nothing.
    ((jolt-map? (car args))
     (let* ((key (case (sa-os-family)
                   ((macos) "darwin") ((windows) "windows") (else "linux")))
            (name (jolt-get-dispatch (car args) (keyword #f key) jolt-nil)))
       (when (jolt-nil? name)
         (jolt-throw (jolt-ex-info
                       (string-append "jolt.ffi/load-library: no :" key
                                      " entry in the per-OS spec")
                       (car args))))
       (ffi-load-native-or-throw (jolt-str-render-one name))
       jolt-nil))
    (else (ffi-load-native-or-throw (jolt-str-render-one (car args))) jolt-nil)))

;; A named load that fails must RAISE: callers probe candidate lists with
;; try/catch around load-library (jolt.mvn-http), and the pre-scoped-loader
;; implementation raised through sa-load-shared-object. Silently returning nil
;; turned every fallback list into "first candidate wins, loaded or not".
(define (ffi-load-native-or-throw path)
  (unless (jolt-ffi-load-native path)
    (jolt-throw (jolt-ex-info
                  (string-append "jolt.ffi/load-library: cannot load " path)
                  (jolt-hash-map (jolt-keyword "path") path)))))

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
(define ffi-native-handles (vector #f))   ; cell[0] = list of handles, oldest first
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
                            (append (or (vector-ref ffi-native-handles 0) '()) (list h)))
               h)
              (else #f)))))))))
;; dlsym `sym` across the registered native handles in declaration order. Returns
;; the address (positive integer) of the first hit, or #f when no handle has it
;; — the emitter then falls back to global name resolution. #f on Windows (no
;; registered handles).
(define (jolt-ffi-dlsym-native sym)
  (if (or (eq? (sa-os-family) 'windows) (not ffi-dlsym))
      #f
      (let loop ((hs (or (vector-ref ffi-native-handles 0) '())))
        (cond
          ((null? hs) #f)
          (else
           (let ((a (ffi-dlsym (car hs) sym)))
             (if (and a (integer? a) (positive? a)) a (loop (cdr hs)))))))))

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
      (else (error #f (string-append "jolt.ffi: unknown foreign type :" n))))))

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
(define (ffi-free ptr) (sa-foreign-free (jnum->exact ptr)) jolt-nil)
(define (ffi-read ptr ty . off)
  (sa-foreign-ref (ffi-type->chez ty) (jnum->exact ptr) (if (pair? off) (jnum->exact (car off)) 0)))
(define (ffi-write ptr ty off val)
  (sa-foreign-set! (ffi-type->chez ty) (jnum->exact ptr) (jnum->exact off) val) jolt-nil)
;; sizeof a foreign type (for laying out structs / arrays).
(define (ffi-sizeof ty) (sa-foreign-sizeof (ffi-type->chez ty)))
(define (ffi-null? ptr) (and (number? ptr) (= (jnum->exact ptr) 0)))
(define ffi-null 0)

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

;; --- string / bytevector marshaling ------------------------------------------
;; A C string result already comes back as a jolt string (the `string` foreign
;; type). For a `void*` that points at a NUL-terminated C string, read it here.
(define (ffi-ptr->string ptr)
  (if (ffi-null? ptr) jolt-nil
      (let ((p (jnum->exact ptr)))
        (let loop ((i 0) (acc '()))
          (let ((b (sa-foreign-ref 'unsigned-8 p i)))
            (if (= b 0) (utf8->string (u8-list->bytevector (reverse acc)))
                (loop (+ i 1) (cons b acc))))))))
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
      (let* ((bv (string->utf8 (jolt-str-render-one s))) (n (bytevector-length bv))
             (p (sa-foreign-alloc (+ n 1))))
        ;; free on a mid-copy throw — the caller only ever sees a whole buffer
        (guard (e (#t (guard (_ (#t #f)) (sa-foreign-free p)) (raise e)))
          (do ((i 0 (+ i 1))) ((= i n)) (sa-foreign-set! 'unsigned-8 p i (bytevector-u8-ref bv i)))
          (sa-foreign-set! 'unsigned-8 p n 0)
          p))))

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

;; --- expose under jolt.ffi ---------------------------------------------------
(def-var! "jolt.ffi" "free-callable" ffi-free-callable)
(def-var! "jolt.ffi" "register-export" jolt-ffi-register-export!)
(def-var! "jolt.ffi" "load-library" ffi-load-library)
(def-var! "jolt.ffi" "loaded?" (lambda (n) (if (ffi-loaded? n) #t #f)))
(def-var! "jolt.ffi" "load-native" jolt-ffi-load-native)
(def-var! "jolt.ffi" "dlsym-native" jolt-ffi-dlsym-native)
(def-var! "jolt.ffi" "alloc" ffi-alloc)
(def-var! "jolt.ffi" "free" ffi-free)
(def-var! "jolt.ffi" "read" ffi-read)
(def-var! "jolt.ffi" "write" ffi-write)
(def-var! "jolt.ffi" "sizeof" ffi-sizeof)
(def-var! "jolt.ffi" "null?" (lambda (p) (if (ffi-null? p) #t #f)))
(def-var! "jolt.ffi" "null" ffi-null)
(def-var! "jolt.ffi" "ptr->string" ffi-ptr->string)
(def-var! "jolt.ffi" "string->ptr" ffi-string->ptr)
