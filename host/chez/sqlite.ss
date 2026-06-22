;; sqlite.ss (jolt-90sp) — jolt.sqlite + a jdbc.core API over the system
;; libsqlite3 via Chez's FFI. The sqlite3 C API is non-variadic, so it binds
;; directly (unlike libcurl). Enough of the surface for a small app: open/close,
;; exec (DDL/DML), a prepared query returning row maps, parameter binding
;; (text/int/double), and last_insert_rowid.
;;
;; def-var!'d into jolt.sqlite AND jdbc.core (jolt-lang/db's API: connection /
;; execute! / fetch / fetch-one / last-insert-id), so an app that requires
;; jdbc.core resolves it as a baked namespace.

(define sqlite-available?
  (let loop ((names '("libsqlite3.0.dylib" "libsqlite3.so.0" "libsqlite3.dylib" "libsqlite3.so")))
    (cond ((null? names) #f)
          ((guard (e (#t #f)) (load-shared-object (car names)) #t) #t)
          (else (loop (cdr names))))))

(define sqlite3_open          (foreign-procedure "sqlite3_open" (string void*) int))
(define sqlite3_close         (foreign-procedure "sqlite3_close" (void*) int))
(define sqlite3_errmsg        (foreign-procedure "sqlite3_errmsg" (void*) string))
(define sqlite3_prepare_v2    (foreign-procedure "sqlite3_prepare_v2" (void* string int void* uptr) int))
(define sqlite3_step          (foreign-procedure "sqlite3_step" (void*) int))
(define sqlite3_finalize      (foreign-procedure "sqlite3_finalize" (void*) int))
(define sqlite3_column_count  (foreign-procedure "sqlite3_column_count" (void*) int))
(define sqlite3_column_name   (foreign-procedure "sqlite3_column_name" (void* int) string))
(define sqlite3_column_type   (foreign-procedure "sqlite3_column_type" (void* int) int))
(define sqlite3_column_text   (foreign-procedure "sqlite3_column_text" (void* int) string))
(define sqlite3_column_int64  (foreign-procedure "sqlite3_column_int64" (void* int) integer-64))
(define sqlite3_column_double (foreign-procedure "sqlite3_column_double" (void* int) double))
(define sqlite3_bind_text     (foreign-procedure "sqlite3_bind_text" (void* int string int iptr) int))
(define sqlite3_bind_int64    (foreign-procedure "sqlite3_bind_int64" (void* int integer-64) int))
(define sqlite3_bind_double   (foreign-procedure "sqlite3_bind_double" (void* int double) int))
(define sqlite3_bind_null     (foreign-procedure "sqlite3_bind_null" (void* int) int))
(define sqlite3_last_insert_rowid (foreign-procedure "sqlite3_last_insert_rowid" (void*) integer-64))

(define SQLITE_OK 0) (define SQLITE_ROW 100) (define SQLITE_DONE 101)
(define SQLITE_TRANSIENT -1)

;; a connection is a jhost "sqlite-conn" wrapping the sqlite3* (as an integer addr)
(define (sql-conn db) (make-jhost "sqlite-conn" (vector db)))
(define (sql-conn-db c) (vector-ref (jhost-state c) 0))
(define (sql-conn? x) (and (jhost? x) (string=? (jhost-tag x) "sqlite-conn")))

(define (sql-open path)
  (unless sqlite-available? (error #f "jolt.sqlite: libsqlite3 not found"))
  (let ((pp (foreign-alloc 8)))
    (foreign-set! 'void* pp 0 0)
    (let ((rc (sqlite3_open path pp)))
      (let ((db (foreign-ref 'void* pp 0)))
        (foreign-free pp)
        (if (= rc SQLITE_OK) (sql-conn db)
            (error #f (string-append "sqlite open failed: " path)))))))

(define (sql-close c) (sqlite3_close (sql-conn-db c)) jolt-nil)

(define (sql-prepare db sql)
  (let ((pp (foreign-alloc 8)))
    (foreign-set! 'void* pp 0 0)
    (let ((rc (sqlite3_prepare_v2 db sql -1 pp 0)))
      (let ((stmt (foreign-ref 'void* pp 0)))
        (foreign-free pp)
        (if (= rc SQLITE_OK) stmt
            (error #f (string-append "sqlite prepare failed: " (sqlite3_errmsg db) " — " sql)))))))

;; bind a jolt value to parameter i (1-based)
(define (sql-bind! stmt i v)
  (cond
    ((jolt-nil? v) (sqlite3_bind_null stmt i))
    ((and (number? v) (exact? v) (integer? v)) (sqlite3_bind_int64 stmt i v))
    ((number? v) (sqlite3_bind_double stmt i (inexact v)))
    ((string? v) (sqlite3_bind_text stmt i v -1 SQLITE_TRANSIENT))
    (else (sqlite3_bind_text stmt i (jolt-str-render-one v) -1 SQLITE_TRANSIENT))))

;; read the current row as a jolt map {keyword-col -> value}
(define (sql-row stmt)
  (let ((n (sqlite3_column_count stmt)) (m (jolt-hash-map)))
    (do ((i 0 (+ i 1))) ((= i n) m)
      (let* ((nm (sqlite3_column_name stmt i))
             (ty (sqlite3_column_type stmt i))
             (v (cond ((= ty 1) (sqlite3_column_int64 stmt i))
                      ((= ty 2) (sqlite3_column_double stmt i))
                      ((= ty 5) jolt-nil)
                      (else (sqlite3_column_text stmt i)))))
        (set! m (jolt-assoc m (keyword #f nm) v))))))

;; run sql with params; return a vector of row maps (empty for a non-SELECT).
(define (sql-query c sql params)
  (let ((db (sql-conn-db c)) (stmt (sql-prepare (sql-conn-db c) sql)))
    (let bind ((i 1) (ps params)) (when (pair? ps) (sql-bind! stmt i (car ps)) (bind (+ i 1) (cdr ps))))
    (let loop ((rows '()))
      (let ((rc (sqlite3_step stmt)))
        (cond
          ((= rc SQLITE_ROW) (loop (cons (sql-row stmt) rows)))
          ((= rc SQLITE_DONE) (sqlite3_finalize stmt) (apply jolt-vector (reverse rows)))
          (else (let ((msg (sqlite3_errmsg db))) (sqlite3_finalize stmt)
                  (error #f (string-append "sqlite step failed: " msg)))))))))

;; --- jolt.sqlite (a small direct API) ---------------------------------------
(define (sqlite-vec-args a) (if (and (pair? a) (or (pvec? (car a)) (cseq? (car a)) (empty-list-t? (car a))))
                                (seq->list (car a)) a))
(def-var! "jolt.sqlite" "open"  (lambda (path) (sql-open path)))
(def-var! "jolt.sqlite" "close" (lambda (c) (sql-close c)))
(def-var! "jolt.sqlite" "query" (lambda (c sql . ps) (sql-query c sql (sqlite-vec-args ps))))
(def-var! "jolt.sqlite" "execute!" (lambda (c sql . ps) (sql-query c sql (sqlite-vec-args ps)) jolt-nil))
(def-var! "jolt.sqlite" "last-insert-id" (lambda (c) (sqlite3_last_insert_rowid (sql-conn-db c))))
(def-var! "jolt.sqlite" "available?" (lambda () (if sqlite-available? #t #f)))

;; --- jdbc.core (jolt-lang/db API) -------------------------------------------
;; A honeysql sql-vector is [sql & params]; jdbc fns take that directly. The
;; connection string is "sqlite:<path>" (or "jdbc:sqlite:<path>").
(define (jdbc-strip-scheme url)
  (let ((u (jolt-str-render-one url)))
    (let strip ((prefixes '("jdbc:sqlite:" "sqlite:")))
      (cond ((null? prefixes) u)
            ((and (>= (string-length u) (string-length (car prefixes)))
                  (string=? (substring u 0 (string-length (car prefixes))) (car prefixes)))
             (substring u (string-length (car prefixes)) (string-length u)))
            (else (strip (cdr prefixes)))))))
(define (jdbc-sqlvec v)             ; [sql & params] -> (values sql params-list)
  (let ((items (cond ((pvec? v) (seq->list v))
                     ((or (cseq? v) (empty-list-t? v)) (seq->list v))
                     ((string? v) (list v))
                     (else '()))))
    (values (if (pair? items) (jolt-str-render-one (car items)) "")
            (if (pair? items) (cdr items) '()))))
(def-var! "jdbc.core" "connection" (lambda (url) (sql-open (jdbc-strip-scheme url))))
(def-var! "jdbc.core" "execute!"
  (lambda (c sqlvec) (let-values (((sql ps) (jdbc-sqlvec sqlvec))) (sql-query c sql ps) jolt-nil)))
(def-var! "jdbc.core" "fetch"
  (lambda (c sqlvec) (let-values (((sql ps) (jdbc-sqlvec sqlvec))) (sql-query c sql ps))))
(def-var! "jdbc.core" "fetch-one"
  (lambda (c sqlvec) (let-values (((sql ps) (jdbc-sqlvec sqlvec)))
                       (let ((rows (sql-query c sql ps)))
                         (if (> (pvec-count rows) 0) (pvec-nth-d rows 0 jolt-nil) jolt-nil)))))
(def-var! "jdbc.core" "last-insert-id" (lambda (c) (sqlite3_last_insert_rowid (sql-conn-db c))))
