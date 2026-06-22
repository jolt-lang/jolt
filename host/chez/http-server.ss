;; http-server.ss (jolt-90sp) — a minimal HTTP/1.1 server over BSD sockets via
;; Chez's FFI (the socket calls are non-variadic, so they bind directly). One
;; connection handled at a time on a background accept thread; synchronous Ring
;; handlers. Enough to serve a small web app.
;;
;; Exposed as jolt.http.server/run-server + stop-server — a baked namespace an
;; app requires for a Ring-style adapter (run a handler, stop the server).

(load-shared-object #f)   ; resolve socket/bind/listen/accept/recv/send in the process

(define c-socket     (foreign-procedure "socket" (int int int) int))
(define c-bind       (foreign-procedure "bind" (int void* int) int))
(define c-listen     (foreign-procedure "listen" (int int) int))
(define c-setsockopt (foreign-procedure "setsockopt" (int int int void* int) int))
(define c-close      (foreign-procedure "close" (int) int))
;; accept/recv/send can BLOCK (accept indefinitely while idle). A thread inside a
;; plain foreign call stays "active" and stalls the stop-the-world collector for
;; every thread, so the accept loop would freeze GC process-wide whenever a future
;; or async block allocates while no request is in flight. __collect_safe
;; deactivates the calling thread for the call's duration so collection proceeds.
;; Safe here: the only arguments are an fd and foreign-alloc'd buffers (outside the
;; Scheme heap), so a collection during the call has nothing to move.
(define c-accept     (foreign-procedure __collect_safe "accept" (int void* void*) int))
(define c-recv       (foreign-procedure __collect_safe "recv" (int void* size_t int) ssize_t))
(define c-send       (foreign-procedure __collect_safe "send" (int void* size_t int) ssize_t))

(define AF_INET 2) (define SOCK_STREAM 1)
;; SOL_SOCKET / SO_REUSEADDR differ by platform: macOS uses 0xffff / 4, Linux 1 / 2.
(define on-macos?
  (let ((m (symbol->string (machine-type))))
    (let loop ((i 0)) (cond ((> (+ i 3) (string-length m)) #f)
                            ((string=? (substring m i (+ i 3)) "osx") #t)
                            (else (loop (+ i 1)))))))
(define sock-level (if on-macos? #xffff 1))
(define sock-reuse (if on-macos? 4 2))

;; sockaddr_in for host:port. macOS: byte0=len(16), byte1=family; Linux: bytes0-1=family.
(define (make-sockaddr host port)
  (let ((sa (foreign-alloc 16)))
    (do ((i 0 (+ i 1))) ((= i 16)) (foreign-set! 'unsigned-8 sa i 0))
    (if (= sock-level #xffff)
        (begin (foreign-set! 'unsigned-8 sa 0 16) (foreign-set! 'unsigned-8 sa 1 AF_INET))
        (foreign-set! 'unsigned-8 sa 0 AF_INET))
    (foreign-set! 'unsigned-8 sa 2 (bitwise-and (bitwise-arithmetic-shift-right port 8) #xff))
    (foreign-set! 'unsigned-8 sa 3 (bitwise-and port #xff))
    ;; sin_addr: 127.0.0.1 (loopback) — the example serves locally
    (foreign-set! 'unsigned-8 sa 4 127) (foreign-set! 'unsigned-8 sa 5 0)
    (foreign-set! 'unsigned-8 sa 6 0)   (foreign-set! 'unsigned-8 sa 7 1)
    sa))

(define (listen-socket host port)
  (let ((fd (c-socket AF_INET SOCK_STREAM 0)))
    (when (< fd 0) (error #f "socket() failed"))
    (let ((opt (foreign-alloc 4)))
      (foreign-set! 'int opt 0 1)
      (c-setsockopt fd sock-level sock-reuse opt 4)
      (foreign-free opt))
    (let ((sa (make-sockaddr host port)))
      (when (< (c-bind fd sa 16) 0) (c-close fd) (foreign-free sa) (error #f (string-append "bind() failed on port " (number->string port))))
      (foreign-free sa))
    (when (< (c-listen fd 64) 0) (c-close fd) (error #f "listen() failed"))
    fd))

;; --- request reading --------------------------------------------------------
(define hs-bufsize 65536)
;; index just past the "\r\n\r\n" header/body separator, or #f.
(define (bv-find-crlfcrlf bv)
  (let ((len (bytevector-length bv)))
    (let loop ((i 0))
      (cond ((> (+ i 4) len) #f)
            ((and (= (bytevector-u8-ref bv i) 13) (= (bytevector-u8-ref bv (+ i 1)) 10)
                  (= (bytevector-u8-ref bv (+ i 2)) 13) (= (bytevector-u8-ref bv (+ i 3)) 10))
             (+ i 4))
            (else (loop (+ i 1)))))))

;; read the whole request (headers + Content-Length body). Returns a string, or #f.
(define (read-request conn)
  (let ((buf (foreign-alloc hs-bufsize)))
    (let loop ((acc (make-bytevector 0)))
      (let ((n (c-recv conn buf hs-bufsize 0)))
        (if (<= n 0)
            (begin (foreign-free buf) (if (> (bytevector-length acc) 0) (utf8->safe acc) #f))
            (let ((chunk (make-bytevector n)))
              (do ((i 0 (+ i 1))) ((= i n)) (bytevector-u8-set! chunk i (foreign-ref 'unsigned-8 buf i)))
              (let* ((acc2 (bv-append acc chunk))
                     (hdr-end (bv-find-crlfcrlf acc2)))
                (if (not hdr-end)
                    (loop acc2)
                    (let ((clen (content-length acc2 hdr-end)))
                      (if (>= (- (bytevector-length acc2) hdr-end) clen)
                          (begin (foreign-free buf) (utf8->safe acc2))
                          (loop acc2)))))))))))
(define (bv-append a b)
  (let ((out (make-bytevector (+ (bytevector-length a) (bytevector-length b)))))
    (bytevector-copy! a 0 out 0 (bytevector-length a))
    (bytevector-copy! b 0 out (bytevector-length a) (bytevector-length b))
    out))
(define (utf8->safe bv) (guard (e (#t (bytes->latin1 bv))) (utf8->string bv)))
(define (bytes->latin1 bv) (list->string (map integer->char (bytevector->u8-list bv))))
(define (content-length bv hdr-end)
  (let* ((hdrs (ascii-string-down (utf8->safe (let ((b (make-bytevector hdr-end)))
                                                 (bytevector-copy! bv 0 b 0 hdr-end) b))))
         (idx (string-search hdrs "content-length:")))
    (if (not idx) 0
        (let* ((s (+ idx (string-length "content-length:")))
               (e (let scan ((i s)) (if (or (>= i (string-length hdrs))
                                            (char=? (string-ref hdrs i) #\return)
                                            (char=? (string-ref hdrs i) #\newline)) i (scan (+ i 1))))))
          (or (string->number (string-trim-ws (substring hdrs s e))) 0)))))
(define (string-search hay needle)
  (let ((hl (string-length hay)) (nl (string-length needle)))
    (let loop ((i 0)) (cond ((> (+ i nl) hl) #f)
                            ((string=? (substring hay i (+ i nl)) needle) i)
                            (else (loop (+ i 1)))))))
(define (string-trim-ws s)
  (let* ((n (string-length s))
         (a (let lp ((i 0)) (if (and (< i n) (char-whitespace? (string-ref s i))) (lp (+ i 1)) i)))
         (b (let lp ((i n)) (if (and (> i a) (char-whitespace? (string-ref s (- i 1)))) (lp (- i 1)) i))))
    (substring s a b)))

;; --- request -> Ring map ----------------------------------------------------
(define (split-once s ch)
  (let scan ((i 0)) (cond ((>= i (string-length s)) (values s #f))
                          ((char=? (string-ref s i) ch) (values (substring s 0 i) (substring s (+ i 1) (string-length s))))
                          (else (scan (+ i 1))))))
(define (req-lines s)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((>= i (string-length s)) (reverse (cons (substring s start i) acc)))
          ((char=? (string-ref s i) #\newline)
           (let ((line (if (and (> i start) (char=? (string-ref s (- i 1)) #\return)) (substring s start (- i 1)) (substring s start i))))
             (loop (+ i 1) (+ i 1) (cons line acc))))
          (else (loop (+ i 1) start acc)))))
(define (request->ring text port)
  (let* ((blank (string-search text "\r\n\r\n"))
         (head (if blank (substring text 0 blank) text))
         (body (if blank (substring text (+ blank 4) (string-length text)) ""))
         (lines (req-lines head))
         (reqline (if (pair? lines) (car lines) "GET / HTTP/1.1"))
         (parts (let lp ((i 0) (start 0) (acc '()))
                  (cond ((>= i (string-length reqline)) (reverse (cons (substring reqline start i) acc)))
                        ((char=? (string-ref reqline i) #\space)
                         (lp (+ i 1) (+ i 1) (cons (substring reqline start i) acc)))
                        (else (lp (+ i 1) start acc)))))
         (method (if (pair? parts) (car parts) "GET"))
         (target (if (and (pair? parts) (pair? (cdr parts))) (cadr parts) "/"))
         (hdrs (let loop ((ls (if (pair? lines) (cdr lines) '())) (m (jolt-hash-map)))
                 (if (null? ls) m
                     (let-values (((k v) (split-once (car ls) #\:)))
                       (if v (loop (cdr ls) (jolt-assoc m (ascii-string-down (string-trim-ws k)) (string-trim-ws v)))
                           (loop (cdr ls) m)))))))
    (let-values (((uri qs) (split-once target #\?)))
      (jolt-hash-map
        (keyword #f "server-port") port
        (keyword #f "server-name") "127.0.0.1"
        (keyword #f "remote-addr") "127.0.0.1"
        (keyword #f "uri") uri
        (keyword #f "query-string") (if qs qs jolt-nil)
        (keyword #f "scheme") (keyword #f "http")
        (keyword #f "request-method") (keyword #f (ascii-string-down method))
        (keyword #f "protocol") "HTTP/1.1"
        (keyword #f "headers") hdrs
        (keyword #f "body") (if (> (string-length body) 0) (host-new "StringReader" body) jolt-nil)))))

;; --- Ring response -> bytes -------------------------------------------------
(define (status-text code)
  (cond ((= code 200) "OK") ((= code 201) "Created") ((= code 204) "No Content")
        ((= code 303) "See Other") ((= code 302) "Found") ((= code 304) "Not Modified")
        ((= code 400) "Bad Request") ((= code 401) "Unauthorized") ((= code 403) "Forbidden")
        ((= code 404) "Not Found") ((= code 405) "Method Not Allowed") ((= code 500) "Internal Server Error")
        (else "OK")))
(define (body->string b)
  (cond ((jolt-nil? b) "")
        ((string? b) b)
        ((or (cseq? b) (empty-list-t? b) (pvec? b)) (apply string-append (map jolt-str-render-one (seq->list (jolt-seq b)))))
        (else (jolt-str-render-one b))))
(define (response->bytes resp)
  (let* ((status (let ((s (jolt-get resp (keyword #f "status")))) (if (jolt-nil? s) 200 (jnum->exact s))))
         (headers (jolt-get resp (keyword #f "headers")))
         (body (body->string (jolt-get resp (keyword #f "body"))))
         (body-bv (string->utf8 body))
         (out (open-output-string)))
    (display (string-append "HTTP/1.1 " (number->string status) " " (status-text status) "\r\n") out)
    (when (pmap? headers)
      (pmap-fold headers (lambda (k v acc)
                           (display (string-append (jolt-str-render-one k) ": " (jolt-str-render-one v) "\r\n") out) acc) 0))
    (display (string-append "Content-Length: " (number->string (bytevector-length body-bv)) "\r\n") out)
    (display "Connection: close\r\n\r\n" out)
    (bv-append (string->utf8 (get-output-string out)) body-bv)))

(define (send-all conn bv)
  (let ((len (bytevector-length bv)) (buf (foreign-alloc (max 1 (bytevector-length bv)))))
    (do ((i 0 (+ i 1))) ((= i len)) (foreign-set! 'unsigned-8 buf i (bytevector-u8-ref bv i)))
    (let loop ((off 0))
      (when (< off len)
        (let ((n (c-send conn (+ buf off) (- len off) 0)))
          (if (<= n 0) (set! off len) (loop (+ off n))))))
    (foreign-free buf)))

;; --- the server -------------------------------------------------------------
(define (serve-loop listen-fd handler port)
  (let loop ()
    (let ((conn (c-accept listen-fd 0 0)))
      (when (>= conn 0)
        (guard (e (#t (guard (e2 (#t #f))
                        (send-all conn (response->bytes
                                         (jolt-hash-map (keyword #f "status") 500
                                                        (keyword #f "body") "Internal Server Error"))))))
          (let ((text (read-request conn)))
            (when text
              (let* ((req (request->ring text port))
                     (resp (jolt-invoke handler req)))
                (send-all conn (response->bytes resp))))))
        (c-close conn))
      (loop))))

;; run-server: bind+listen, spawn the accept loop on a background thread, return a
;; handle {:port :socket}. opts is a map with :port (default 3000).
(define (http-run-server handler opts)
  (let* ((port (let ((p (and (pmap? opts) (jolt-get opts (keyword #f "port"))))) (if (or (not p) (jolt-nil? p)) 3000 (jnum->exact p))))
         (fd (listen-socket "127.0.0.1" port)))
    (fork-thread (lambda () (serve-loop fd handler port)))
    (jolt-hash-map (keyword #f "port") port (keyword #f "socket") fd)))
(define (http-stop-server server)
  (when (pmap? server)
    (let ((fd (jolt-get server (keyword #f "socket")))) (when (number? fd) (c-close (jnum->exact fd)))))
  jolt-nil)

(def-var! "jolt.http.server" "run-server" (lambda (handler . opt) (http-run-server handler (if (pair? opt) (car opt) (jolt-hash-map)))))
(def-var! "jolt.http.server" "stop-server" http-stop-server)
