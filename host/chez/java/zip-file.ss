;; zip-file.ss — java.util.zip.ZipFile (JDK 21 ZipFile.java), and the central
;; directory index it and the loader's jar roots share.
;;
;; A zipdir is one archive's central directory, read once: every entry's name,
;; method, sizes, CRC, times, extra field, comment and local-header offset,
;; keyed by name. An entry's bytes are read on demand from a fresh port on the
;; file, positioned at the local header, so two readers of one archive share
;; nothing but the index; the index itself is immutable once built. The loader
;; keeps one zipdir per jar on its roots (zipdir-for), re-read when the file's
;; modification time changes, and answers a require or an io/resource out of it
;; without extracting the jar (jolt issue #1005). ZipFile is the class over the
;; same index.
;;
;; Reading follows JDK 21 Source.initCEN and checkAndAddEntry: the END record is
;; found by scanning the last 64 KiB + 22 bytes, its Zip64 locator is honoured,
;; every CEN header is checked, and each refusal is the JDK's ZipException
;; message. The local header is read when an entry is opened, as the JDK reads
;; it then, because its name and extra lengths — not the central directory's —
;; say where the data starts.

;; --- constants (JDK 21 ZipConstants.java, ZipConstants64.java) ----------------
(define zip-censig #x02014b50)
(define zip-endsig #x06054b50)
(define zip64-endsig #x06064b50)
(define zip64-locsig #x07064b50)
(define zip-cenhdr 46)
(define zip-endhdr 22)
(define zip64-endhdr 56)
(define zip64-lochdr 20)
(define zip-end-maxlen (+ #xFFFF zip-endhdr))
(define zip64-magiccount #xFFFF)
(define zip-open-read 1)
(define zip-open-delete 4)

;; --- the central directory --------------------------------------------------
(define-record-type zipdir
  (fields path      ; the archive's path, as opened
          coder     ; the charset's canonical name for names and comments, or #f for UTF-8
          comment   ; the archive comment's bytes, or #f — decoded when asked for
          table     ; name -> zdirent
          order)    ; vector of zdirent, in central-directory order
  (nongenerative jolt-zipdir-v1))

;; The archive comment as a string, or #f: ZipFile.getComment decodes the END
;; record's bytes when it is called, not when the archive opens, so a comment
;; that is not in the archive's charset (a Latin-1 one on a UTF-8 open) refuses
;; there — the JDK's IllegalArgumentException — and every entry still reads.
(define (zipdir-comment-string d)
  (and (zipdir-comment d)
       (zip-decode-bytes (zipdir-comment d) 0 (zipdir-coder d))))

;; BV as a string, by FLAG and CODER: UTF-8 when the coder is UTF-8 or the
;; entry's bit 11 says so, else the charset (JDK 21 ZipCoder).
(define (zip-decode-bytes bv flag coder)
  (if (or (not coder) (not (zero? (bitwise-and flag zip-use-utf8))))
      (zip-decode-utf8 bv)
      (zip-decode-charset bv coder)))

(define-record-type zdirent
  (fields name method flag crc csize size xdostime extra comment loc-offset)
  (nongenerative jolt-zdirent-v1))

(define (zipdir-size d) (vector-length (zipdir-order d)))

;; The entry named NAME, or, when NAME does not end in "/", the directory entry
;; NAME + "/" — ZipFile.getEntry's lookup (ZipFile.java getEntryPos). #f when
;; neither is there.
(define (zipdir-lookup d name)
  (or (hashtable-ref (zipdir-table d) name #f)
      (let ((n (string-length name)))
        (and (or (fx= n 0) (not (char=? (string-ref name (fx- n 1)) #\/)))
             (hashtable-ref (zipdir-table d) (string-append name "/") #f)))))

(define (zipdir-has? d name) (and (hashtable-ref (zipdir-table d) name #f) #t))

;; Every entry name, in central-directory order.
(define (zipdir-names d)
  (map zdirent-name (vector->list (zipdir-order d))))

(define (zip-zerror msg) (zip-throw "java.util.zip.ZipException" msg))

;; The bytes of the file at PATH from POS, N of them, or #f when the file is
;; shorter than that.
(define (zip-file-bytes in size pos n)
  (and (>= pos 0) (>= n 0) (<= (+ pos n) size)
       (begin
         (set-port-position! in pos)
         (let ((bv (get-bytevector-n in n)))
           (and (bytevector? bv) (= (bytevector-length bv) n) bv)))))

(define (zip-u16 bv i) (bytevector-u16-ref bv i (endianness little)))
(define (zip-u32 bv i) (bytevector-u32-ref bv i (endianness little)))
(define (zip-u64 bv i) (bytevector-u64-ref bv i (endianness little)))

;; The Zip64 extended information (tag 0x0001) of a CEN extra field, as
;; (values size csize loc-offset) with a #f for each field the block does not
;; carry; only the fields whose CEN value is 0xFFFFFFFF are in the block, in
;; that order (APPNOTE 4.5.3; ZipFile.java checkExtraFields).
(define (zip64-extra-fields extra size csize off)
  (let ((len (bytevector-length extra)))
    (let loop ((i 0))
      (if (> (+ i 4) len)
          (values #f #f #f)
          (let ((tag (zip-u16 extra i))
                (sz (zip-u16 extra (+ i 2))))
            (cond
              ((> (+ i 4 sz) len) (values #f #f #f))
              ((= tag #x0001)
               (let* ((at (+ i 4))
                      (end (+ at sz))
                      (take (lambda (need pos)
                              (if (and need (<= (+ pos 8) end))
                                  (values (zip-u64 extra pos) (+ pos 8))
                                  (values #f pos)))))
                 (let*-values (((s pos) (take (= size #xFFFFFFFF) at))
                               ((c pos) (take (= csize #xFFFFFFFF) pos))
                               ((o pos) (take (= off #xFFFFFFFF) pos)))
                   (values s c o))))
              (else (loop (+ i 4 sz)))))))))

;; The END record as (endpos cenlen cenoff total comment-bytes), with the Zip64
;; END record's values — and its position as endpos — when the locator points at
;; one that agrees with it. #f when the file holds no END record (ZipFile.java
;; findEND, lines 1559-1650).
;;
;; An END candidate whose comment length does not reach the end of the file is
;; not refused: bytes padded after the archive are common enough that the JDK
;; verifies the candidate instead, by the CEN signature where its cenlen says
;; the central directory starts and the LOC signature where its cenoff says the
;; first local header is, and scans on when either is missing. Both positions
;; are measured back from the END record, never from the start of the file
;; (see zipdir-read: a stub before the archive shifts every offset it holds).
(define (zipdir-find-end in size)
  (let* ((n (min size zip-end-maxlen))
         (tail (zip-file-bytes in size (- size n) n))
         (sig-at? (lambda (pos sig)
                    (let ((bv (zip-file-bytes in size pos 4)))
                      (and bv (= (zip-u32 bv 0) sig))))))
    (and tail
         (let loop ((i (- n zip-endhdr)))
           (and (>= i 0)
                (let* ((endpos (+ (- size n) i))
                       (comlen (zip-u16 tail (+ i 20)))
                       (total (zip-u16 tail (+ i 10)))
                       (cenlen (zip-u32 tail (+ i 12)))
                       (cenoff (zip-u32 tail (+ i 16)))
                       (cenpos (- endpos cenlen))
                       (locpos (- cenpos cenoff)))
                  (if (and (= (zip-u32 tail i) zip-endsig)
                           (or (= (+ i zip-endhdr comlen) n)
                               (and (>= cenpos 0) (>= locpos 0)
                                    (sig-at? cenpos zip-censig)
                                    (sig-at? locpos zip-locsig))))
                      (let ((comment (let ((c (make-bytevector (min comlen (- n i zip-endhdr)))))
                                       (bytevector-copy! tail (+ i zip-endhdr) c 0 (bytevector-length c))
                                       c)))
                        ;; a Zip64 END record is consulted when the locator is
                        ;; there and its record agrees with this one on every
                        ;; field this one does not mark as overflowed (lines
                        ;; 1614-1644); it then supplies all three, and its
                        ;; position is where the central directory ends
                        (let ((loc (and (>= endpos zip64-lochdr)
                                        (zip-file-bytes in size (- endpos zip64-lochdr) zip64-lochdr))))
                          (if (and loc (= (zip-u32 loc 0) zip64-locsig))
                              (let* ((end64pos (zip-u64 loc 8))
                                     (end64 (zip-file-bytes in size end64pos zip64-endhdr)))
                                (if (and end64 (= (zip-u32 end64 0) zip64-endsig))
                                    (let ((cenlen64 (zip-u64 end64 40))
                                          (cenoff64 (zip-u64 end64 48))
                                          (total64 (zip-u64 end64 32)))
                                      (if (or (and (not (= cenlen64 cenlen)) (not (= cenlen #xFFFFFFFF)))
                                              (and (not (= cenoff64 cenoff)) (not (= cenoff #xFFFFFFFF)))
                                              (and (not (= total64 total)) (not (= total zip64-magiccount))))
                                          (list endpos cenlen cenoff total comment)
                                          (list end64pos cenlen64 cenoff64 total64 comment)))
                                    (list endpos cenlen cenoff total comment)))
                              (list endpos cenlen cenoff total comment))))
                      (loop (- i 1)))))))))

;; Read the central directory of the archive at PATH. CODER is the charset's
;; canonical name in lower case for names and comments, or #f for UTF-8; an
;; entry flagged UTF-8 (bit 11) decodes as UTF-8 whatever the coder, as JDK 21's
;; ZipCoder does. Every refusal is the JDK's: a directory or missing file the
;; open reports, an empty file, a file with no END record, and each CEN
;; inconsistency initCEN and checkAndAddEntry check.
;;
;; The central directory starts cenlen bytes BEFORE the END record, and the
;; first local header cenoff bytes before that (initCEN lines 1665-1673): the
;; offsets an archive states count from its own first byte, and a stub written
;; before it — the shell line that makes a jar executable — moves every one by
;; its length without rewriting them. That length, locpos, is added to each
;; entry's local-header offset here, so the rest of the file reads positions.
;; The END record's entry count is a hint the JDK does not enforce (it counts
;; the headers it walks, lines 1712-1756); the count that must agree is the
;; walk's own, which ends exactly at the record.
(define (zipdir-read path coder . rest)
  (define name (and (pair? rest) (car rest)))
  ;; a file that is not there is the JDK's NoSuchFileException (its
  ;; Files.readAttributes, before the open); one that is there but cannot be
  ;; opened is the RandomAccessFile's FileNotFoundException, "(Permission
  ;; denied)" as the JDK spells the EACCES it got
  (let ((in (guard (e (#t (if (file-exists? path)
                              (zip-throw "java.io.FileNotFoundException"
                                         (string-append (or name path) " (Permission denied)"))
                              (zip-throw "java.nio.file.NoSuchFileException" (or name path)))))
              (open-file-input-port path))))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (let ((size (port-length in)))
          (when (= size 0) (zip-zerror "zip file is empty"))
          (let-values (((endpos cenlen cenoff total comment)
                        (apply values (or (zipdir-find-end in size)
                                          (zip-zerror "zip END header not found")))))
            (when (> cenlen endpos)
              (zip-zerror "invalid END header (bad central directory size)"))
            (let* ((cenpos (- endpos cenlen))
                   (locpos (- cenpos cenoff)))
              (when (< locpos 0)
                (zip-zerror "invalid END header (bad central directory offset)"))
              (when (> total (quotient cenlen zip-cenhdr))
                (zip-zerror "invalid END header (total entries count too large)"))
              (let ((cen (zip-file-bytes in size cenpos cenlen)))
                (unless cen (zip-zerror "read CEN tables failed"))
                ;; a name or comment the charset cannot decode is the JDK's
                ;; refusal of the header (checkAndAddEntry lines 1236-1252)
                (let ((table (make-hashtable string-hash string=?))
                      (decode (lambda (bv flag)
                                (guard (e (#t (zip-zerror "invalid CEN header (bad entry name or comment)")))
                                  (zip-decode-bytes bv flag coder)))))
                  (let loop ((pos 0) (acc '()))
                    (cond
                      ((>= pos cenlen)
                       (unless (= pos cenlen) (zip-zerror "invalid CEN header (bad header size)"))
                       (make-zipdir path coder
                                    (and (> (bytevector-length comment) 0) comment)
                                    table
                                    (list->vector (reverse acc))))
                      (else
                       (when (> (+ pos zip-cenhdr) cenlen)
                         (zip-zerror "invalid CEN header (bad header size)"))
                       (unless (= (zip-u32 cen pos) zip-censig)
                         (zip-zerror "invalid CEN header (bad signature)"))
                       (let* ((flag (zip-u16 cen (+ pos 8)))
                              (method (zip-u16 cen (+ pos 10)))
                              (xdostime (zip-u32 cen (+ pos 12)))
                              (crc (zip-u32 cen (+ pos 16)))
                              (csize (zip-u32 cen (+ pos 20)))
                              (size (zip-u32 cen (+ pos 24)))
                              (nlen (zip-u16 cen (+ pos 28)))
                              (elen (zip-u16 cen (+ pos 30)))
                              (clen (zip-u16 cen (+ pos 32)))
                              (off (zip-u32 cen (+ pos 42)))
                              (hlen (+ zip-cenhdr nlen elen clen)))
                         (when (not (zero? (bitwise-and flag 1)))
                           (zip-zerror "invalid CEN header (encrypted entry)"))
                         (unless (or (= method zip-stored) (= method zip-deflated))
                           (zip-zerror (string-append "invalid CEN header (bad compression method: "
                                                      (number->string method) ")")))
                         (when (> (+ pos hlen) cenlen)
                           (zip-zerror "invalid CEN header (bad header size)"))
                         (let* ((name-bv (let ((b (make-bytevector nlen)))
                                           (bytevector-copy! cen (+ pos zip-cenhdr) b 0 nlen) b))
                                (extra (and (> elen 0)
                                            (let ((b (make-bytevector elen)))
                                              (bytevector-copy! cen (+ pos zip-cenhdr nlen) b 0 elen) b)))
                                (comment-bv (and (> clen 0)
                                                 (let ((b (make-bytevector clen)))
                                                   (bytevector-copy! cen (+ pos zip-cenhdr nlen elen) b 0 clen) b)))
                                (name (decode name-bv flag)))
                           (let-values (((size64 csize64 off64)
                                         (if extra (zip64-extra-fields extra size csize off) (values #f #f #f))))
                             (let ((ent (make-zdirent name method flag crc
                                                      (or csize64 csize) (or size64 size)
                                                      xdostime extra
                                                      (and comment-bv (decode comment-bv flag))
                                                      (+ locpos (or off64 off)))))
                               ;; the local header must lie before the central
                               ;; directory (the JDK finds out when the entry is
                               ;; opened: initDataOffset's "invalid LOC header")
                               (when (> (+ (zdirent-loc-offset ent) zip-lochdr) cenpos)
                                 (zip-zerror "invalid CEN header (bad header size)"))
                               ;; a name that appears twice keeps the first (a
                               ;; hashtable-ref finds it); every one is enumerated
                               (unless (hashtable-ref table name #f)
                                 (hashtable-set! table name ent))
                               (loop (+ pos hlen) (cons ent acc)))))))))))))))
      (lambda () (close-port in)))))

;; --- reading an archive's bytes ---------------------------------------------
;; Every read of an entry's bytes goes through a READER: (read-at! pos bv start
;; count) -> the bytes read into BV at START from file position POS, 0 at the
;; end of the file. A ZipFile is one reader for every stream it opens — one
;; file descriptor per ZipFile, however many entries are open at once, as the
;; JDK reads every ZipFileInputStream through its one RandomAccessFile under a
;; lock (ZipFile.java ZipFileInputStream.initDataOffset / read). A read with no
;; ZipFile — a jar root's entry, a whole-entry read — opens a reader of its own
;; (zipdir-file-reader) and closes it with the stream.

;; A reader over a file port of its own, and the thunk that closes it.
;; zipdir-reader-opens counts them, which is how the loader's gate sees that a
;; require reads a jar through one reader rather than one per entry.
(define zipdir-reader-opens 0)
(define (zipdir-file-reader path)
  (set! zipdir-reader-opens (+ zipdir-reader-opens 1))
  (let ((in (open-file-input-port path))
        (mu (make-mutex)))
    (values (zipdir-port-reader in mu)
            (lambda () (close-port in)))))

;; The reader for port IN, positioned per read under MU: two streams of one
;; ZipFile may read at once, and each owns its own position.
(define (zipdir-port-reader in mu)
  (lambda (pos bv start count)
    (jolt-with-mutex mu
      (set-port-position! in pos)
      (let ((got (get-bytevector-some! in bv start count)))
        (if (eof-object? got) 0 got)))))

;; N bytes from POS through READ-AT!, whole, or #f when the file ends first.
(define (zipdir-read-at read-at! pos n)
  (let ((bv (make-bytevector n)))
    (let loop ((off 0))
      (if (= off n)
          bv
          (let ((got (read-at! (+ pos off) bv off (- n off))))
            (and (> got 0) (loop (+ off got))))))))

;; Where ENT's data starts in the file, from its local header (ZipFile.java
;; initDataOffset): the header's own name and extra lengths, which may differ
;; from the central directory's.
(define (zipdir-data-offset read-at! ent)
  (let ((loc (zipdir-read-at read-at! (zdirent-loc-offset ent) zip-lochdr)))
    (unless (and loc (= (zip-u32 loc 0) zip-locsig))
      (zip-zerror "ZipFile invalid LOC header (bad signature)"))
    (+ (zdirent-loc-offset ent) zip-lochdr (zip-u16 loc 26) (zip-u16 loc 28))))

;; A binary input port over the N bytes from OFFSET through READ-AT!; closing it
;; runs CLOSE!. A file that ends early ends the port.
(define (zip-slice-port read-at! offset n close!)
  (let ((pos offset)
        (remaining n))
    (make-custom-binary-input-port
     "zip-entry"
     (lambda (bv start count)
       (if (<= remaining 0)
           0
           (let ((got (read-at! pos bv start (min count remaining))))
             (set! pos (+ pos got))
             (set! remaining (- remaining got))
             got)))
     #f #f close!)))

;; A read scope: while one is open on this thread, whole-entry reads of an
;; archive share one reader, opened on the first read and closed when the scope
;; ends. The loader opens one around a require (loader.ss load-namespace*), whose
;; entries are read in a burst: a fresh descriptor per entry was most of the
;; non-inflate cost of a read. Outside a scope nothing holds the archive open,
;; so a jar can be replaced between reads — Windows refuses to replace an open
;; file. Readers are keyed by the zipdir, not the path: a jar replaced at the
;; same path is a new zipdir (zipdir-for), and an old descriptor would read the
;; old file.
;;
;; The scope closes its readers on ANY exit, a fiber park included; a resumed
;; load that reads again just opens a new one.
;;
;; The parameter holds (thread-id . readers) and a scope answers only on the
;; thread that opened it. A forked thread starts with its creator's parameter
;; values, so a thread a namespace's top level starts would otherwise share the
;; require's unlocked table, and once the require closed it, open readers into it
;; that nothing closes.
(define zipdir-read-scope (make-thread-parameter #f))
(define (zipdir-scope-readers)
  (let ((s (zipdir-read-scope)))
    (and s (eqv? (car s) (get-thread-id)) (cdr s))))
(define (call-with-zipdir-read-scope thunk)
  (if (zipdir-scope-readers)
      (thunk)
      (let ((readers (make-eq-hashtable)))
        (dynamic-wind
          (lambda () #f)
          (lambda () (parameterize ((zipdir-read-scope (cons (get-thread-id) readers))) (thunk)))
          (lambda ()
            (let-values (((ds rs) (hashtable-entries readers)))
              (hashtable-clear! readers)
              (vector-for-each (lambda (r) ((cdr r))) rs)))))))
;; The reader for D in the open scope, opening it on first use.
(define (zipdir-scoped-reader readers d)
  (or (hashtable-ref readers d #f)
      (let-values (((read-at! close!) (zipdir-file-reader (zipdir-path d))))
        (let ((r (cons read-at! close!)))
          (hashtable-set! readers d r)
          r))))
(define (zipdir-raw-bytes-via read-at! ent)
  (let ((bv (zipdir-read-at read-at! (zipdir-data-offset read-at! ent) (zdirent-csize ent))))
    (or bv (zip-zerror "ZipFile invalid LOC header (bad signature)"))))
;; The compressed bytes of ENT: through the scope's reader when one is open,
;; else from a reader opened for the call.
(define (zipdir-raw-bytes d ent)
  (let ((readers (zipdir-scope-readers)))
    (if readers
        (zipdir-raw-bytes-via (car (zipdir-scoped-reader readers d)) ent)
        (let-values (((read-at! close!) (zipdir-file-reader (zipdir-path d))))
          (dynamic-wind
            (lambda () #f)
            (lambda () (zipdir-raw-bytes-via read-at! ent))
            close!)))))

;; Raw-deflate BV inflated, expecting SIZE bytes: one zlib stream, fed a window
;; at a time and asked for a window at a time. SIZE is the central directory's
;; claim: it is checked against what came out, never trusted for an allocation
;; — a damaged or crafted header says 0xFFFFFFFF, and the output grows only as
;; the stream produces. A stream that ends early or late is the JDK's
;; ZipException.
(define zip-output-window 65536)
(define (zip-inflate-raw bv size)
  (let-values (((zs code msg) (zstream-open 'inflate -15 0 0)))
    (unless zs (zip-throw "java.lang.InternalError" (or msg "inflateInit2 failed")))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (let ((len (bytevector-length bv)))
          (define (grown out need)
            (if (<= need (bytevector-length out))
                out
                (let ((bigger (make-bytevector (min size (max need (* 2 (bytevector-length out)))))))
                  (bytevector-copy! out 0 bigger 0 (bytevector-length out))
                  bigger)))
          (define (trimmed out written)
            (if (= written (bytevector-length out))
                out
                (let ((exact (make-bytevector written)))
                  (bytevector-copy! out 0 exact 0 written)
                  exact)))
          (let loop ((pos 0) (written 0) (out (make-bytevector (min size zip-output-window))))
            (let* ((n (min zip-input-window (- len pos)))
                   (in (let ((w (make-bytevector n))) (bytevector-copy! bv pos w 0 n) w)))
              (let-values (((code consumed produced bytes)
                            (zstream-step! zs z-no-flush in (min zip-output-window (- size written)))))
                (cond
                  ((or (= code z-ok) (= code z-stream-end) (= code z-buf-error))
                   (let ((out (grown out (+ written produced))))
                     (bytevector-copy! bytes 0 out written produced)
                     (let ((written (+ written produced))
                           (pos (+ pos consumed)))
                       (cond
                         ((= code z-stream-end)
                          (unless (= written size)
                            (zip-zerror (string-append "invalid entry size (expected "
                                                       (number->string size) " but got "
                                                       (number->string written) " bytes)")))
                          (trimmed out written))
                         ((and (= pos len) (= produced 0))
                          (zip-throw "java.io.EOFException" "Unexpected end of ZLIB input stream"))
                         ((and (= written size) (< pos len))
                          ;; the JDK's inflater stream stops reading at size;
                          ;; the rest of the compressed bytes are not looked at
                          (trimmed out written))
                         (else (loop pos written out))))))
                  ((= code z-data-error)
                   (zip-zerror (or (zstream-message zs) "invalid stored block lengths")))
                  (else (zip-throw "java.lang.InternalError" (zstream-message zs)))))))))
      (lambda () (zstream-close! zs)))))

;; The uncompressed bytes of the entry ENT of D, whole, checked against the
;; entry's CRC-32 as ZipInputStream checks an entry it finishes: a damaged jar on
;; the roots is refused where it is read, not loaded as whatever came out.
(define (zipdir-entry-bytes d ent)
  (let* ((raw (zipdir-raw-bytes d ent))
         (out (cond
                ((= (zdirent-method ent) zip-stored)
                 (unless (= (bytevector-length raw) (zdirent-size ent))
                   (zip-zerror (string-append "invalid entry size (expected "
                                              (number->string (zdirent-size ent)) " but got "
                                              (number->string (bytevector-length raw)) " bytes)")))
                 raw)
                ((= (zdirent-method ent) zip-deflated) (zip-inflate-raw raw (zdirent-size ent)))
                (else (zip-zerror "invalid compression method"))))
         (crc (zlib-crc32 0 out 0 (bytevector-length out))))
    (unless (= crc (zdirent-crc ent))
      (zip-zerror (zip-crc-message (zdirent-crc ent) crc)))
    out))

;; An InputStream over ENT's uncompressed bytes, as ZipFile.getInputStream
;; answers: a stored entry's reads its slice of the file (ZipFile.java
;; ZipFileInputStream), a deflated entry's is an InflaterInputStream over that
;; slice with a raw Inflater (ZipFileInflaterInputStream). Both are jolt's
;; in-stream on the zip-in-streams.ss frame, and available() answers the
;; uncompressed bytes not yet read, as both of the JDK's do. The bytes come
;; through READ-AT!, and closing the stream runs CLOSE! — a ZipFile passes its
;; shared reader and a no-op, a stream with no ZipFile a reader of its own.
(define zip-int-max 2147483647)
(define (zipdir-entry-stream d ent read-at! close!)
  (let* ((off (zipdir-data-offset read-at! ent))
         (slice (zip-slice-port read-at! off (zdirent-csize ent) close!))
         (size (zdirent-size ent)))
    (cond
      ((= (zdirent-method ent) zip-stored)
       (let ((remaining size))
         (make-zin-stream
          (make-zin "java.util.zip.ZipFile$ZipFileInputStream" (make-in-stream slice) #f #f
                    (na-byte-array 0) 0 #f
                    (lambda (z bv start count)
                      (let ((n (get-bytevector-some! slice bv start count)))
                        (if (eof-object? n)
                            (begin (zin-reach-eof-set! z #t) 0)
                            (begin (set! remaining (- remaining n)) n))))
                    (lambda (z) (min remaining zip-int-max))
                    (lambda (z) (close-port slice))
                    #f))))
      ((= (zdirent-method ent) zip-deflated)
       (let ((inf (make-inflater #t)))
         (make-zin-stream
          (make-zin "java.util.zip.ZipFile$ZipFileInflaterInputStream" (make-in-stream slice) inf #t
                    (na-byte-array 8192) 0 #f
                    zin-inflate-read
                    (lambda (z) (min (max 0 (- size (inflater-bytes-written inf))) zip-int-max))
                    zin-inflate-close #f))))
      (else (close-port slice) (zip-zerror "invalid compression method")))))

;; The stream for an entry read outside any ZipFile — a jar root's entry
;; through io/input-stream — on a reader of its own, closed with the stream.
(define (zipdir-entry-stream-owned d ent)
  (let-values (((read-at! close!) (zipdir-file-reader (zipdir-path d))))
    (guard (e (#t (close!) (raise e)))
      (zipdir-entry-stream d ent read-at! close!))))

;; --- the index the loader shares ---------------------------------------------
;; One zipdir per archive path, keyed by the file's modification time: a jar
;; re-fetched at the same path is read again. A path that is not a readable
;; archive answers #f, and is not remembered, so it is asked again — the
;; archive may be finished by then. The table is read under its lock; a build
;; runs outside it, so two threads may read one archive at once and the second
;; store wins, which is harmless: the two are equal.
(define zipdir-cache (make-hashtable string-hash string=?))
(define zipdir-cache-mu (make-mutex))
(define (zipdir-for path)
  (let ((stamp (guard (e (#t #f)) (sa-file-mtime-ms path))))
    (and stamp
         (let ((hit (jolt-with-mutex zipdir-cache-mu (hashtable-ref zipdir-cache path #f))))
           (if (and hit (eqv? (car hit) stamp))
               (cdr hit)
               (let ((d (guard (e (#t #f)) (zipdir-read path #f))))
                 (when d
                   (jolt-with-mutex zipdir-cache-mu
                     (hashtable-set! zipdir-cache path (cons stamp d))))
                 d))))))

;; --- jar roots ---------------------------------------------------------------
;; The index of ROOT when it is a jar on the source roots — a .jar or .zip
;; (either case) that is a file — or #f for a directory root or a jar that is
;; not a readable archive (loader.ss ldr-root-file, io.ss resource-candidate).
(define (root-jar-name? root)
  (let ((n (string-length root)))
    (and (> n 4)
         (let ((suf (string-downcase (substring root (- n 4) n))))
           (or (string=? suf ".jar") (string=? suf ".zip"))))))
(define (root-jar-index root)
  (and (root-jar-name? root)
       (not (file-directory? root))
       (zipdir-for root)))

;; --- java.util.zip.ZipFile --------------------------------------------------
;; state #(zipdir name closed? streams reader close-reader! mutex): READER is
;; the one file port every stream of this ZipFile reads through (one descriptor
;; per ZipFile, as the JDK's Source holds one RandomAccessFile), and STREAMS the
;; in-streams opened through getInputStream, held weakly as the JDK's istreams
;; set holds them: a stream nothing references any more is not kept alive here,
;; and close() closes the ones still reachable. The table is written under
;; MUTEX, as the JDK synchronizes istreams (two threads opening entries at once
;; would otherwise write one Chez hashtable together).
(define (zfile-dir self) (vector-ref (jhost-state self) 0))
(define (zfile-name self) (vector-ref (jhost-state self) 1))
(define (zfile-closed? self) (vector-ref (jhost-state self) 2))
(define (zfile-streams self) (vector-ref (jhost-state self) 3))
(define (zfile-reader self) (vector-ref (jhost-state self) 4))
(define (zfile-close-reader! self) ((vector-ref (jhost-state self) 5)))
(define (zfile-mutex self) (vector-ref (jhost-state self) 6))

(define (zfile-ensure-open self)
  (when (zfile-closed? self)
    (zip-throw "java.lang.IllegalStateException" "zip file closed")))

;; The ZipEntry for a directory record: every field the JDK's getZipEntry sets.
(define (zdirent->entry ent)
  (let ((e (make-zip-entry-named (zdirent-name ent))))
    (let ((z (zentry-of e)))
      (zentry-method-set! z (zdirent-method ent))
      (zentry-xdostime-set! z (zdirent-xdostime ent))
      (zentry-crc-set! z (zdirent-crc ent))
      (zentry-csize-set! z (zdirent-csize ent))
      (zentry-size-set! z (zdirent-size ent))
      (when (zdirent-extra ent) (zentry-set-extra0! z (zdirent-extra ent)))
      (when (zdirent-comment ent) (zentry-comment-set! z (zdirent-comment ent))))
    e))

;; ZipFile(String) | ZipFile(File) | ZipFile(File, int) | ZipFile(String, Charset)
;; | ZipFile(File, Charset) | ZipFile(File, int, Charset), resolved as the
;; compiled call resolves them (ZipFile.java lines 165-268). The mode must be
;; OPEN_READ, with or without OPEN_DELETE; the charset null check has the JDK's
;; message. A File or a String names the archive; the name kept is the path as
;; given, which getName answers.
(define (make-zip-file . args)
  (define (no-ctor)
    (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.zip.ZipFile"))
  (define (file-arg x)
    (cond ((jolt-nil? x) (zip-throw "java.lang.NullPointerException" #f))
          ((string? x) x)
          ((jfile? x) (jfile-path x))
          (else (no-ctor))))
  (define (mode-arg x)
    (let ((m (zip-int-arg x)))
      (unless (or (= m zip-open-read) (= m (bitwise-ior zip-open-read zip-open-delete)))
        (zip-throw "java.lang.IllegalArgumentException"
                   (string-append "Illegal mode: 0x" (number->string m 16))))
      m))
  (define (charset-arg x)
    (cond ((jolt-nil? x) (zip-throw "java.lang.NullPointerException" "charset"))
          ((zip-charset? x)
           (let ((name (charset-canonical-down (charset-arg-name x))))
             (and (not (string=? name "utf-8")) name)))
          (else (zip-class-cast x "java.nio.charset.Charset"))))
  (let-values (((name coder)
                (case (length args)
                  ((1) (values (file-arg (car args)) #f))
                  ((2) (let ((a (car args)) (b (cadr args)))
                         (cond ((zip-long? b) (mode-arg b) (values (file-arg a) #f))
                               (else (values (file-arg a) (charset-arg b))))))
                  ((3) (mode-arg (cadr args))
                       (values (file-arg (car args)) (charset-arg (caddr args))))
                  (else (no-ctor)))))
    (let* ((path (jfile-fs name))
           (dir (if (and (file-exists? path) (file-directory? path))
                    (zip-throw "java.io.FileNotFoundException"
                               (string-append name " (Is a directory)"))
                    (zipdir-read path coder name))))
      (let-values (((read-at! close!) (zipdir-file-reader path)))
        (make-jhost "zip-file" (vector dir name #f (make-weak-eq-hashtable) read-at! close! (make-mutex)))))))

(define (zfile-entry-arg self who x)
  (cond ((jolt-nil? x) (zip-throw "java.lang.NullPointerException" who))
        ((string? x) x)
        (else (zip-class-cast x "java.lang.String"))))

(define (zfile-get-entry self name)
  (let ((name (zfile-entry-arg self "name" name)))
    (zfile-ensure-open self)
    (let ((ent (zipdir-lookup (zfile-dir self) name)))
      (if ent (zdirent->entry ent) jolt-nil))))

;; getInputStream(ZipEntry): null for a name the archive does not hold; the
;; stream is remembered so close() closes it.
(define (zfile-get-input-stream self entry)
  (when (jolt-nil? entry) (zip-throw "java.lang.NullPointerException" "entry"))
  (unless (zip-entry? entry) (zip-class-cast entry "java.util.zip.ZipEntry"))
  (zfile-ensure-open self)
  (zfile-input-stream-named self (zentry-name (zentry-of entry))))
(define (zfile-input-stream-named self name)
  (let ((ent (zipdir-lookup (zfile-dir self) name)))
    (if (not ent)
        jolt-nil
        (let ((s (zipdir-entry-stream (zfile-dir self) ent (zfile-reader self) (lambda () #f))))
          (jolt-with-mutex (zfile-mutex self)
            (hashtable-set! (zfile-streams self) s #t))
          s))))

(define (zfile-entries self)
  (zfile-ensure-open self)
  (list->cseq (map zdirent->entry (vector->list (zipdir-order (zfile-dir self))))))
;; stream(): the entries in the same order, read when the stream is consumed
(define (zfile-stream self)
  (zfile-ensure-open self)
  (make-stream 'ref
    (st-lazy (lambda ()
               (zfile-ensure-open self)
               (jolt-seq (list->cseq (map zdirent->entry (vector->list (zipdir-order (zfile-dir self))))))))))

(define (zfile-close self)
  (unless (zfile-closed? self)
    (vector-set! (jhost-state self) 2 #t)
    (let ((live (jolt-with-mutex (zfile-mutex self)
                  (let ((ks (hashtable-keys (zfile-streams self))))
                    (hashtable-clear! (zfile-streams self))
                    ks))))
      (vector-for-each (lambda (s) (guard (e (#t #f)) (record-method-dispatch s "close" jolt-nil)))
                       live))
    (zfile-close-reader! self))
  jolt-nil)

(hashtable-set! jhost-tag->fqn "zip-file" "java.util.zip.ZipFile")
(register-host-methods! "zip-file"
  (list
   (cons "getEntry" (zip-method "getEntry" '(1) zfile-get-entry))
   (cons "getInputStream" (zip-method "getInputStream" '(1) zfile-get-input-stream))
   (cons "entries" (zip-method "entries" '(0) zfile-entries))
   (cons "stream" (zip-method "stream" '(0) zfile-stream))
   (cons "size" (zip-method "size" '(0)
                  (lambda (self) (zfile-ensure-open self) (->num (zipdir-size (zfile-dir self))))))
   (cons "getName" (zip-method "getName" '(0) zfile-name))
   ;; ZipFile has no toString of its own in JDK 21: Object's
   (cons "toString" (zip-method "toString" '(0) host-object-tostring))
   (cons "getComment" (zip-method "getComment" '(0)
                        (lambda (self)
                          (zfile-ensure-open self)
                          (or (zipdir-comment-string (zfile-dir self)) jolt-nil))))
   (cons "close" (zip-method "close" '(0) zfile-close))))
(register-class-statics! "java.util.zip.ZipFile"
  (list (cons "OPEN_READ" (->num zip-open-read))
        (cons "OPEN_DELETE" (->num zip-open-delete))))
(reg-ctor! '("ZipFile" "java.util.zip.ZipFile") make-zip-file)

;; --- the application image as a read-only JarFile -----------------------------
;; The public embedded resources (io.ss) read as the entries of a jar named
;; "jolt-embedded-resources.jar": every resource, and a directory entry for each
;; directory holding one, in name order. Computed once per resource index.
(define embedded-archive-entries-cache (cons #f '()))
(define (embedded-archive-entry-names)
  (let ((index (embedded-resource-sorted))
        (cache embedded-archive-entries-cache))
    (if (eq? (car cache) index)
        (cdr cache)
        (let ((entries (make-hashtable string-hash string=?)))
          (vector-for-each
           (lambda (name)
             (hashtable-set! entries name #t)
             (let ((n (string-length name)))
               (let loop ((i 0))
                 (when (< i n)
                   (when (char=? (string-ref name i) #\/)
                     (hashtable-set! entries (substring name 0 (+ i 1)) #t))
                   (loop (+ i 1))))))
           index)
          (let ((names (sort string<? (vector->list (hashtable-keys entries)))))
            (set! embedded-archive-entries-cache (cons index names))
            names)))))

(define (make-embedded-jar-entry name directory? . source)
  (let* ((source (if (pair? source) (car source) name))
         (content (and (not directory?) (embedded-resource-ref source)))
         (size (if directory? 0
                   (bytevector-length (if (bytevector? content) content (string->utf8 content)))))
         (entry (make-jhost "jar-file-entry"
                            (make-zentry name -1 -1 size size 0 jolt-nil jolt-nil (make-file-time 0)))))
    (hashtable-set! zentry-real-names (zentry-of entry) source)
    entry))


;; java.lang.Runtime.Version. jolt models one Java version, the one its JVM
;; oracle certifies against (host-static-methods.ss jolt-java-feature-version);
;; Runtime.version() and JarFile.runtimeVersion() both report it.
(define (make-runtime-version version)
  (make-jhost "runtime-version" version))
(define (runtime-version-feature self) (car (jhost-state self)))
(define (runtime-version-string self)
  (fold-left (lambda (s n) (string-append s (if (string=? s "") "" ".") (number->string n)))
             "" (jhost-state self)))
(define (runtime-version-parse text)
  (let ((s (jolt-str-render-one text)))
    (let loop ((start 0) (acc '()))
      (let ((dot (let find ((i start))
                   (cond ((>= i (string-length s)) #f)
                         ((char=? (string-ref s i) #\.) i)
                         (else (find (+ i 1)))))))
        (let* ((end (or dot (string-length s)))
               (part (substring s start end)))
          (when (or (string=? part "") (not (string->number part)))
            (throw-jvm 'IllegalArgumentException (string-append "invalid runtime version: " s)))
          (let ((next (cons (string->number part) acc)))
            (if dot (loop (+ dot 1) next) (make-runtime-version (reverse next)))))))))
(define (jolt-runtime-version) (make-runtime-version (list jolt-java-feature-version)))
(register-class-statics! "Runtime$Version" (list (cons "parse" runtime-version-parse)))
(register-class-statics! "java.lang.Runtime$Version" (list (cons "parse" runtime-version-parse)))
(register-class-statics! "java.lang.Runtime" (list (cons "version" jolt-runtime-version)))
(register-host-methods! "runtime-version"
  (list (cons "feature" runtime-version-feature)
        (cons "toString" runtime-version-string)))
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "runtime-version")))
                      runtime-version-string)
(hashtable-set! jhost-tag->fqn "runtime-version" "java.lang.Runtime$Version")

;; --- java.util.jar.JarFile ------------------------------------------------------
;; One JarFile over two kinds of archive: a jar on disk ("jar-file"), whose state
;; is the ZipFile's with the version it was opened at and its manifest after it,
;; and the application image's own archive ("embedded-jar-file"). They differ in
;; how a name is found, listed and read, and only there: the manifest, the
;; multi-release selection and the JarFileEntry are written once, over those.
;;
;; JarFile.BASE_VERSION_FEATURE: a jar opened at this version, or below, reads
;; only its base entries, and META-INF/versions/N for N under it is never read.
(define embedded-jar-base-feature 8)
(define (jar-on-disk? self) (string=? (jhost-tag self) "jar-file"))
(define (jar-ensure-open self)
  (if (jar-on-disk? self) (zfile-ensure-open self) (embedded-jar-ensure-open self)))
(define (jar-open-version self) (vector-ref (jhost-state self) (if (jar-on-disk? self) 7 4)))
(define (jar-open-feature self) (runtime-version-feature (jar-open-version self)))
;; The entry name NAME finds -- itself, or a directory's NAME + "/" -- or #f.
(define (jar-find self name)
  (if (jar-on-disk? self)
      (let ((ent (zipdir-lookup (zfile-dir self) name))) (and ent (zdirent-name ent)))
      (cond ((embedded-public-resource? name) name)
            ((embedded-directory-name name) => (lambda (dir) dir))
            (else #f))))
;; Every entry name, in the archive's order.
(define (jar-names self)
  (if (jar-on-disk? self) (zipdir-names (zfile-dir self)) (embedded-archive-entry-names)))

;; getManifest: META-INF/MANIFEST.MF (any case, as JarFile.getManEntry finds
;; it) read once, so every call answers the same Manifest; nil without one.
(define (jar-disk-manifest self)
  (let ((cell (vector-ref (jhost-state self) 8)))
    (or (unbox cell)
        (let* ((dir (zfile-dir self))
               (ent (or (zipdir-lookup dir "META-INF/MANIFEST.MF")
                        (exists (lambda (e) (and (string-ci=? (zdirent-name e) "META-INF/MANIFEST.MF") e))
                                (vector->list (zipdir-order dir)))))
               (parsed (if ent
                           (manifest-ctor (make-in-stream (open-bytevector-input-port (zipdir-entry-bytes dir ent))))
                           jolt-nil)))
          ;; parsed outside the lock; the first published wins
          (jolt-with-mutex (zfile-mutex self)
            (or (unbox cell) (begin (set-box! cell parsed) parsed)))))))
(define (jar-manifest self)
  (if (jar-on-disk? self) (jar-disk-manifest self) (embedded-jar-manifest self)))
(define (jar-multi-release? self)
  (if (jar-on-disk? self) (manifest-multi-release? (jar-manifest self)) (embedded-archive-multi-release?)))
;; getVersion: the version it was opened at when it is multi-release, else the base.
(define (jar-version self)
  (if (jar-multi-release? self) (jar-open-version self) (make-runtime-version (list embedded-jar-base-feature))))

;; The name a lookup of NAME reads at FEATURE (JarFile.getVersionedEntry): the
;; highest META-INF/versions/N/NAME that FIND finds with BASE <= N <= FEATURE,
;; else NAME itself. (values real-name versioned?), real-name #f when nothing is
;; there at all.
(define (jar-select-name feature multi-release? find name)
  (let ((versioned
         (and (> feature embedded-jar-base-feature)
              (not (embedded-string-prefix? "META-INF/" name))
              multi-release?
              (let loop ((candidate feature))
                (and (>= candidate embedded-jar-base-feature)
                     (or (find (string-append "META-INF/versions/" (number->string candidate) "/" name))
                         (loop (- candidate 1))))))))
    (if versioned (values versioned #t) (values (find name) #f))))
(define (jar-selected-name self name)
  (jar-select-name (jar-open-feature self) (jar-multi-release? self) (lambda (n) (jar-find self n)) name))
(define (embedded-select-resource-name feature name)
  (let-values (((real versioned?)
                (jar-select-name feature (embedded-archive-multi-release?)
                                 (lambda (n) (and (embedded-public-resource? n) n)) name)))
    (if versioned? real name)))
;; A class loader opens a multi-release jar at the runtime version, so a
;; resource URL names the entry that version selects.
(define (embedded-runtime-resource-name name)
  (embedded-select-resource-name jolt-java-feature-version name))

;; JarFile$JarFileEntry: a JarEntry that knows its jar, so getAttributes reads
;; that jar's manifest, and whose real name is the versioned entry it was read
;; from when a multi-release lookup picked one (getName is the name asked for).
(define zentry-jars (make-weak-eq-hashtable))
(define (jar-entry-for self real name)
  (let ((entry (if (jar-on-disk? self)
                   (let ((e (zdirent->entry (zipdir-lookup (zfile-dir self) real))))
                     (zentry-name-set! (zentry-of e) name)
                     (make-jhost "jar-file-entry" (jhost-state e)))
                   (let ((directory? (let ((n (string-length real)))
                                       (and (fx>? n 0) (char=? (string-ref real (fx- n 1)) #\/)))))
                     (make-embedded-jar-entry name directory? real)))))
    (hashtable-set! zentry-real-names (zentry-of entry) real)
    (hashtable-set! zentry-jars (zentry-of entry) self)
    entry))
;; getEntry / getJarEntry. A multi-release jar looks a null name up as a
;; versioned one first, which is where the JDK's NullPointerException comes from.
(define (jar-get-entry self name)
  (when (and (jolt-nil? name) (jar-multi-release? self))
    (zip-throw "java.lang.NullPointerException"
               "Cannot invoke \"String.startsWith(String)\" because \"name\" is null"))
  (let ((name (zfile-entry-arg self "name" name)))
    (jar-ensure-open self)
    (let-values (((real versioned?) (jar-selected-name self name)))
      (if real (jar-entry-for self real (if versioned? name real)) jolt-nil))))
(define (jar-raw-entry self name) (jar-entry-for self name name))
;; getInputStream(ZipEntry): a JarFileEntry reads the entry it was read from;
;; any other entry reads its own name.
(define (jar-get-input-stream self entry)
  (when (jolt-nil? entry) (zip-throw "java.lang.NullPointerException" "ze"))
  (unless (zip-entry? entry) (zip-class-cast entry "java.util.zip.ZipEntry"))
  (jar-ensure-open self)
  (let ((name (or (and (string=? (jhost-tag entry) "jar-file-entry")
                       (hashtable-ref zentry-real-names (zentry-of entry) #f))
                  (zentry-name (zentry-of entry)))))
    (if (jar-on-disk? self)
        (zfile-input-stream-named self name)
        (embedded-jar-input-stream-named self name))))
(define (jar-entries self)
  (jar-ensure-open self)
  (if (jar-on-disk? self)
      (list->cseq (map (lambda (name) (jar-raw-entry self name)) (jar-names self)))
      (make-jhost "embedded-jar-enumeration"
                  (vector self (map (lambda (name) (jar-raw-entry self name)) (jar-names self))))))
;; A stream of the entries NAMES (a thunk) gives, each looked up by LOOKUP when
;; the stream is consumed; a name it finds nothing for is dropped.
(define (jar-lazy-stream self names lookup)
  (jar-ensure-open self)
  (make-stream 'ref
    (st-lazy (lambda ()
               (jar-ensure-open self)
               (jolt-seq (list->cseq (filter (lambda (e) (not (jolt-nil? e)))
                                             (map (lambda (name) (lookup self name)) (names)))))))))
(define (jar-stream self) (jar-lazy-stream self (lambda () (jar-names self)) jar-raw-entry))
;; JarFile.versionedStream: each entry's base name (a versioned entry above the
;; jar's version, a malformed version or a bare version directory is dropped),
;; first occurrence kept, then looked up at the jar's version -- which drops a
;; name only a version the lookup never reads holds.
(define (jar-versioned-names self)
  (let ((feature (jar-open-feature self))
        (seen (make-hashtable string-hash string=?)))
    (let loop ((names (jar-names self)) (acc '()))
      (if (null? names)
          (reverse acc)
          (let* ((name (car names))
                 (base (if (embedded-string-prefix? "META-INF/versions/" name)
                           (let ((part (jar-versioned-part name)))
                             (and part (not (string=? (cdr part) "")) (<= (car part) feature) (cdr part)))
                           name)))
            (if (and base (not (hashtable-ref seen base #f)))
                (begin (hashtable-set! seen base #t) (loop (cdr names) (cons base acc)))
                (loop (cdr names) acc)))))))
(define (jar-versioned-stream self)
  (if (jar-multi-release? self)
      (jar-lazy-stream self (lambda () (jar-versioned-names self)) jar-get-entry)
      (jar-stream self)))
;; META-INF/versions/N/rest -> (N . rest), #f for anything else
(define (jar-versioned-part name)
  (and (>= (string-length name) 18)
       (string=? (substring name 0 18) "META-INF/versions/")
       (let* ((tail (substring name 18 (string-length name)))
              (slash (let find ((i 0))
                       (cond ((>= i (string-length tail)) #f)
                             ((char=? (string-ref tail i) #\/) i)
                             (else (find (+ i 1))))))
              (version (and slash (string->number (substring tail 0 slash)))))
         (and version (exact? version) (integer? version) (>= version 0)
              (cons version (substring tail (+ slash 1) (string-length tail)))))))
;; The members every JarFile answers, over either archive.
(define jar-file-members
  (list
   (cons "getEntry" (zip-method "getEntry" '(1) jar-get-entry))
   (cons "getJarEntry" (zip-method "getJarEntry" '(1) jar-get-entry))
   (cons "getInputStream" (zip-method "getInputStream" '(1) jar-get-input-stream))
   (cons "entries" (zip-method "entries" '(0) jar-entries))
   (cons "stream" (zip-method "stream" '(0) jar-stream))
   (cons "versionedStream" (zip-method "versionedStream" '(0) jar-versioned-stream))
   (cons "getManifest" (zip-method "getManifest" '(0)
                         (lambda (self) (jar-ensure-open self) (jar-manifest self))))
   (cons "getVersion" (zip-method "getVersion" '(0)
                        (lambda (self) (jar-ensure-open self) (jar-version self))))
   (cons "isMultiRelease" (zip-method "isMultiRelease" '(0)
                            (lambda (self) (jar-ensure-open self) (jar-multi-release? self))))))

;; --- the application image as a read-only JarFile ----------------------------------
;; state #(name closed? streams mutex version)
(define (embedded-jar-name self) (vector-ref (jhost-state self) 0))
(define (embedded-jar-closed? self) (vector-ref (jhost-state self) 1))
(define (embedded-jar-streams self) (vector-ref (jhost-state self) 2))
(define (embedded-jar-mutex self) (vector-ref (jhost-state self) 3))
(define (embedded-jar-ensure-open self)
  (when (embedded-jar-closed? self)
    (zip-throw "java.lang.IllegalStateException" "zip file closed")))
(define (embedded-jar-entry-for archive name) (jar-get-entry archive name))
(define (embedded-jar-input-stream-named self name)
  (let ((content (embedded-resource-ref name)))
    (if (and (not content) (not (embedded-directory? name)))
        jolt-nil
        (let ((stream (make-in-stream
                       (open-bytevector-input-port
                        (cond ((bytevector? content) content)
                              ((string? content) (string->utf8 content))
                              (else (make-bytevector 0)))))))
          (jolt-with-mutex (embedded-jar-mutex self)
            (hashtable-set! (embedded-jar-streams self) stream #t))
          stream))))

(register-host-methods! "embedded-jar-enumeration"
  (list
   (cons "hasMoreElements"
         (lambda (self)
           (let ((state (jhost-state self)))
             (embedded-jar-ensure-open (vector-ref state 0))
             (pair? (vector-ref state 1)))))
   (cons "nextElement"
         (lambda (self)
           (let ((state (jhost-state self)))
             (embedded-jar-ensure-open (vector-ref state 0))
             (when (null? (vector-ref state 1))
               (zip-throw "java.util.NoSuchElementException" "embedded archive enumeration exhausted"))
             (let ((entry (car (vector-ref state 1))))
               (vector-set! state 1 (cdr (vector-ref state 1)))
               entry))))))
(hashtable-set! jhost-tag->fqn "embedded-jar-enumeration" "java.util.Enumeration")

(define (embedded-jar-close self)
  (unless (embedded-jar-closed? self)
    (vector-set! (jhost-state self) 1 #t)
    (let ((live (jolt-with-mutex (embedded-jar-mutex self)
                  (let ((keys (hashtable-keys (embedded-jar-streams self))))
                    (hashtable-clear! (embedded-jar-streams self))
                    keys))))
      (vector-for-each
       (lambda (stream)
         (guard (e (#t #f)) (record-method-dispatch stream "close" jolt-nil)))
       live)))
  jolt-nil)

(define (make-embedded-jar-file name . version)
  (make-jhost "embedded-jar-file"
              (vector name #f (make-weak-eq-hashtable) (make-mutex)
                      (if (pair? version) (car version) (make-runtime-version (list embedded-jar-base-feature))))))

;; JarFile(String) | JarFile(String, boolean verify) | JarFile(File) |
;; JarFile(File, boolean) | JarFile(File, boolean, int mode) |
;; JarFile(File, boolean, int mode, Runtime.Version): (values path mode version)
;; as the compiled call resolves them. The mode is checked as ZipFile checks
;; it, then the version for null; there is no charset constructor.
(define (jar-file-ctor-args args)
  (define (no-ctor)
    (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.jar.JarFile"))
  (define (file-arg x)
    (cond ((jolt-nil? x) (zip-throw "java.lang.NullPointerException" #f))
          ((string? x) x)
          ((jfile? x) (jfile-path x))
          (else (no-ctor))))
  (define (verify-arg x) (unless (boolean? x) (no-ctor)))
  (define (mode-arg x)
    (unless (zip-long? x) (no-ctor))
    (let ((mode (zip-int-arg x)))
      (unless (or (= mode zip-open-read) (= mode (bitwise-ior zip-open-read zip-open-delete)))
        (zip-throw "java.lang.IllegalArgumentException"
                   (string-append "Illegal mode: 0x" (number->string mode 16))))
      mode))
  (let ((base (make-runtime-version (list embedded-jar-base-feature))))
    (case (length args)
      ((1) (values (file-arg (car args)) zip-open-read base))
      ((2) (verify-arg (cadr args)) (values (file-arg (car args)) zip-open-read base))
      ((3) (verify-arg (cadr args))
           (values (file-arg (car args)) (mode-arg (caddr args)) base))
      ((4) (verify-arg (cadr args))
           (let ((mode (mode-arg (caddr args))) (version (cadddr args)))
             (cond ((jolt-nil? version) (zip-throw "java.lang.NullPointerException" #f))
                   ((not (and (jhost? version) (string=? (jhost-tag version) "runtime-version")))
                    (no-ctor)))
             (values (file-arg (car args)) mode (embedded-jar-open-version version))))
      (else (no-ctor)))))

;; The version a JarFile opens at: a feature at or below the base reads as the
;; base, any other as that feature alone (JarFile's constructor).
(define (embedded-jar-open-version version)
  (let ((feature (runtime-version-feature version)))
    (make-runtime-version (list (if (<= feature embedded-jar-base-feature) embedded-jar-base-feature feature)))))
(define (make-jar-file . args)
  (let-values (((path mode version) (jar-file-ctor-args args)))
    (if (string=? path embedded-archive-name)
        (make-embedded-jar-file embedded-archive-name version)
        (let ((file (make-zip-file path (->num mode))))
          (make-jhost "jar-file" (list->vector (append (vector->list (jhost-state file)) (list version (box #f)))))))))

(define (make-jar-entry . args)
  (let ((entry (apply make-zip-entry args)))
    (make-jhost "jar-entry" (jhost-state entry))))

(hashtable-set! jhost-tag->fqn "jar-file" "java.util.jar.JarFile")
(hashtable-set! jhost-tag->fqn "embedded-jar-file" "java.util.jar.JarFile")
(hashtable-set! jhost-tag->fqn "jar-entry" "java.util.jar.JarEntry")
(hashtable-set! jhost-tag->fqn "jar-file-entry" "java.util.jar.JarFile$JarFileEntry")
;; A JarFile is a ZipFile with the jar's members over it; a JarEntry a ZipEntry
;; with its own. A JarEntry made by a caller carries no attributes and no
;; certificates (null on the JDK until a JarFile's verifier fills them, and
;; nothing here is signed); a JarFileEntry reads its jar's manifest.
(derive-host-methods! "jar-file" "zip-file" jar-file-members)
(derive-host-methods! "jar-entry" "zip-entry"
  (list
   (zentry-member "getAttributes" (lambda (self) jolt-nil))
   (zentry-member "getCertificates" (lambda (self) jolt-nil))
   (zentry-member "getCodeSigners" (lambda (self) jolt-nil))
   (zentry-member "getRealName" (lambda (self) (zentry-name (zentry-of self))))))
(derive-host-methods! "jar-file-entry" "jar-entry"
  (list
   (zentry-member "getAttributes"
                  (lambda (self)
                    (let* ((jar (hashtable-ref zentry-jars (zentry-of self) #f))
                           (manifest (if jar (jar-manifest jar) jolt-nil)))
                      (if (jolt-nil? manifest)
                          jolt-nil
                          (manifest-entry manifest (zentry-name (zentry-of self)))))))
   (zentry-member "getRealName"
                  (lambda (self)
                    (or (hashtable-ref zentry-real-names (zentry-of self) #f)
                        (zentry-name (zentry-of self)))))))
(register-host-methods! "embedded-jar-file"
  (append
   jar-file-members
   (list
    (cons "size" (zip-method "size" '(0)
                   (lambda (self)
                     (embedded-jar-ensure-open self)
                     (->num (length (embedded-archive-entry-names))))))
    (cons "getName" (zip-method "getName" '(0) embedded-jar-name))
    (cons "toString" (zip-method "toString" '(0) host-object-tostring))
    (cons "getComment" (zip-method "getComment" '(0)
                         (lambda (self) (embedded-jar-ensure-open self) jolt-nil)))
    (cons "close" (zip-method "close" '(0) embedded-jar-close)))))
(register-class-statics! "java.util.jar.JarFile"
  (list (cons "MANIFEST_NAME" "META-INF/MANIFEST.MF")
        (cons "baseVersion" (lambda () (make-runtime-version (list embedded-jar-base-feature))))
        (cons "runtimeVersion" jolt-runtime-version)))
(reg-ctor! '("JarFile" "java.util.jar.JarFile") make-jar-file)
(reg-ctor! '("JarEntry" "java.util.jar.JarEntry") make-jar-entry)
