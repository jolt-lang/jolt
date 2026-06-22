;; http-client.ss (jolt-90sp) — jolt.http-client: a synchronous HTTP client.
;;
;; Backed by the system `curl` binary rather than a direct libcurl FFI: on Apple
;; Silicon curl_easy_setopt is variadic and its value arg goes on the stack, where
;; Chez's fixed-signature foreign-procedure can't place it (it would need a
;; compiled C shim per platform). Shelling to curl uses the same mature native
;; library — TLS, redirects, gzip and all — with no build step and identical
;; behavior across platforms. Returns a Ring-ish {:status :headers :body}.
;;
;; def-var!'d into the jolt.http-client namespace; loaded by the CLI before the
;; loader snapshot, so (require '[jolt.http-client]) resolves with no source file.

;; single-quote an argument for the outer `sh -c` (loader.ss's sh-quote isn't
;; loaded yet at this point).
(define (hc-shq s)
  (string-append "'"
    (apply string-append
      (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s)))
    "'"))

(define hc-status-sentinel "\nJOLTHTTPSTATUS:")

(define kw-method   (keyword #f "method"))
(define kw-url      (keyword #f "url"))
(define kw-headers  (keyword #f "headers"))
(define kw-body     (keyword #f "body"))
(define kw-insecure (keyword #f "insecure?"))
(define kw-follow   (keyword #f "follow?"))
(define kw-timeout  (keyword #f "timeout-ms"))
(define kw-status   (keyword #f "status"))
(define kw-query    (keyword #f "query-params"))
(define kw-ctype    (keyword #f "content-type"))

;; :content-type :json -> "application/json"; a string passes through.
(define (hc-ctype->str ct)
  (cond ((keyword? ct)
         (let ((n (keyword-t-name ct)))
           (cond ((string=? n "json") "application/json")
                 ((string=? n "xml") "application/xml")
                 ((string=? n "form") "application/x-www-form-urlencoded")
                 (else n))))
        (else (jolt-str-render-one ct))))

;; A per-request header file path, unique across processes (PID) and threads (a
;; mutex-guarded monotonic counter). The previous unguarded `set!` + `mod 90000`
;; raced: concurrent callers could compute the same path and clobber each other's
;; -D header dump. getpid is a fast, non-blocking foreign call.
(define c-getpid (begin (load-shared-object #f) (foreign-procedure "getpid" () int)))
(define hc-tmp-mutex (make-mutex))
(define hc-tmp-counter 0)
(define (hc-tmp-path)
  (let ((n (with-mutex hc-tmp-mutex (set! hc-tmp-counter (+ hc-tmp-counter 1)) hc-tmp-counter)))
    (string-append (or (getenv "TMPDIR") "/tmp") "/jolt-http-"
                   (number->string (c-getpid)) "-" (number->string n) ".hdr")))

(define (hc-trim s)
  (let* ((n (string-length s))
         (a (let lp ((i 0)) (if (and (< i n) (char-whitespace? (string-ref s i))) (lp (+ i 1)) i)))
         (b (let lp ((i n)) (if (and (> i a) (char-whitespace? (string-ref s (- i 1)))) (lp (- i 1)) i))))
    (substring s a b)))
(define (hc-lines s)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((>= i (string-length s)) (reverse (if (> i start) (cons (substring s start i) acc) acc)))
          ((char=? (string-ref s i) #\newline)
           (loop (+ i 1) (+ i 1) (if (> i start) (cons (substring s start i) acc) acc)))
          (else (loop (+ i 1) start acc)))))
;; raw header dump (curl -D) -> a jolt map {lowercased-name -> value}. With
;; redirects there are several blocks; the LAST status line resets the map so the
;; final response's headers win.
(define (hc-parse-headers text)
  (let ((m (jolt-hash-map)))
    (for-each
      (lambda (raw)
        (let ((line (hc-trim raw)))
          (cond
            ((= 0 (string-length line)) #t)
            ((and (>= (string-length line) 5) (string=? (substring line 0 5) "HTTP/"))
             (set! m (jolt-hash-map)))             ; new response block
            (else
             (let ((ci (let scan ((i 0)) (cond ((>= i (string-length line)) #f)
                                              ((char=? (string-ref line i) #\:) i)
                                              (else (scan (+ i 1)))))))
               (when (and ci (> ci 0))
                 (set! m (jolt-assoc m (ascii-string-down (hc-trim (substring line 0 ci)))
                                     (hc-trim (substring line (+ ci 1) (string-length line)))))))))))
      (hc-lines text))
    m))

(define (hc-split-status out)
  ;; out ends with "\nJOLTHTTPSTATUS:<code>"; return (values body code-int).
  (let ((idx (let ((sl (string-length hc-status-sentinel)) (ol (string-length out)))
               (let scan ((i (- ol sl)))
                 (cond ((< i 0) #f)
                       ((string=? (substring out i (+ i sl)) hc-status-sentinel) i)
                       (else (scan (- i 1))))))))
    (if idx
        (values (substring out 0 idx)
                (or (string->number (hc-trim (substring out (+ idx (string-length hc-status-sentinel))
                                                         (string-length out)))) 0))
        (values out 0))))

(define (hc-request method url opts)
  (when (or (not url) (jolt-nil? url)) (error #f "jolt.http-client: missing :url"))
  (let* ((headers (jolt-get opts kw-headers))
         (body (let ((x (jolt-get opts kw-body))) (and (not (jolt-nil? x)) x)))
         (insecure? (jolt-truthy? (jolt-get opts kw-insecure)))
         (follow? (let ((x (jolt-get opts kw-follow))) (or (jolt-nil? x) (jolt-truthy? x))))
         (timeout (let ((x (jolt-get opts kw-timeout))) (if (jolt-nil? x) 30 (quotient (jnum->exact x) 1000))))
         (hdrfile (hc-tmp-path))
         (parts (list "curl -sS"
                      (if follow? "-L" "")
                      (if insecure? "-k" "")
                      "--max-time" (number->string (max 1 timeout))
                      "-D" (hc-shq hdrfile)
                      "-X" (hc-shq (string-upcase method))
                      "-w" (hc-shq (string-append hc-status-sentinel "%{http_code}"))
                      "--compressed"))
         (ctype (let ((x (jolt-get opts kw-ctype))) (and (not (jolt-nil? x)) (hc-ctype->str x))))
         (parts (if ctype (append parts (list "-H" (hc-shq (string-append "Content-Type: " ctype)))) parts))
         (parts (if (pmap? headers)
                    (append parts (pmap-fold headers
                                    (lambda (k v acc)
                                      (cons "-H" (cons (hc-shq (string-append (jolt-str-render-one k) ": "
                                                                              (jolt-str-render-one v))) acc)))
                                    '()))
                    parts))
         ;; query-params: let curl URL-encode them onto the URL (-G appends as the
         ;; query string for a GET).
         (qp (let ((x (jolt-get opts kw-query))) (and (pmap? x) x)))
         (parts (if qp
                    (append parts
                            (cons "-G"
                                  (pmap-fold qp (lambda (k v acc)
                                                  (cons "--data-urlencode"
                                                        (cons (hc-shq (string-append (jolt-str-render-one k) "="
                                                                                     (jolt-str-render-one v))) acc)))
                                             '())))
                    parts))
         (parts (if body (append parts (list "--data-binary" (hc-shq (jolt-str-render-one body)))) parts))
         (parts (append parts (list (hc-shq (jolt-str-render-one url)))))
         (cmd (let join ((xs parts) (s "")) (if (null? xs) s
                 (join (cdr xs) (if (string=? (car xs) "") s (string-append s (if (string=? s "") "" " ") (car xs)))))))
         (out (jolt-sh-out cmd)))
    (let-values (((bdy code) (hc-split-status out)))
      (let ((hdr-text (guard (e (#t "")) (read-file-string hdrfile))))
        (guard (e (#t #f)) (delete-file hdrfile))
        (jolt-hash-map kw-status code
                       kw-headers (hc-parse-headers hdr-text)
                       kw-body bdy)))))

;; --- public jolt.http-client API --------------------------------------------
(define (hc-verb method) (lambda (url . opt) (hc-request method url (if (pair? opt) (car opt) (jolt-hash-map)))))
(def-var! "jolt.http-client" "request"
  (lambda (opts) (hc-request (let ((m (jolt-get opts kw-method))) (if (jolt-nil? m) "GET" (jolt-str-render-one m)))
                             (jolt-get opts kw-url) opts)))
(def-var! "jolt.http-client" "get"    (hc-verb "GET"))
(def-var! "jolt.http-client" "head"   (hc-verb "HEAD"))
(def-var! "jolt.http-client" "post"   (hc-verb "POST"))
(def-var! "jolt.http-client" "put"    (hc-verb "PUT"))
(def-var! "jolt.http-client" "delete" (hc-verb "DELETE"))
