;; JarURLConnection over a jar: URL -- a resource baked into the image (an
;; embedded-res), or an entry of a jar on disk (a "url" whose spec is a jar
;; path, which io/resource answers for a jar on the source roots).

(define (jar-connection-disk? resource) (not (embedded-res? resource)))
;; The entry a resource URL names, as JarURLConnection.getEntryName answers it:
;; the text after "!/", decoded; nil for the jar itself.
(define (embedded-jar-url-entry-name resource)
  (if (jar-connection-disk? resource)
      (let ((entry (cdr (jar-path-split (url-spec resource)))))
        (if (string=? entry "") jolt-nil (uri-decode-lenient entry)))
      (embedded-res-name resource)))
(define (jar-connection-jar-path resource) (car (jar-path-split (url-spec resource))))
(define (embedded-jar-file-url . resource)
  (if (and (pair? resource) (jar-connection-disk? (car resource)))
      (make-url (url-canonical (string-append "file:" (jar-connection-jar-path (car resource)))))
      (make-url "file:jolt-embedded-resources.jar")))

(define (embedded-connection-state self) (jhost-state self))
(define (embedded-connection-resource self) (vector-ref (embedded-connection-state self) 0))
(define (embedded-connection-connected? self) (vector-ref (embedded-connection-state self) 1))
(define (embedded-connection-connect! self)
  (vector-set! (embedded-connection-state self) 1 #t)
  jolt-nil)
(define (embedded-connection-check-mutable! self)
  (when (embedded-connection-connected? self)
    (throw-jvm 'IllegalStateException "Already connected")))
(define (embedded-connection-set! self index value)
  (embedded-connection-check-mutable! self)
  (vector-set! (embedded-connection-state self) index value)
  jolt-nil)
(define (embedded-connection-get self index) (vector-ref (embedded-connection-state self) index))

;; A jar: URL connection opens its jar at the runtime version, as the JDK's
;; does; the URL already names the entry that version selected. One JarFile per
;; connection, as getJarFile answers the same one each time.
(define (embedded-connection-archive self)
  (let ((st (embedded-connection-state self)))
    (or (vector-ref st 11)
        (let* ((resource (embedded-connection-resource self))
               (jar (if (jar-connection-disk? resource)
                        (make-jar-file (file-url->path (jar-connection-jar-path resource))
                                       #t (->num zip-open-read) (jolt-runtime-version))
                        (make-embedded-jar-file embedded-archive-name (jolt-runtime-version)))))
          (vector-set! st 11 jar)
          jar))))
;; The entry the URL names, or the JDK's refusal: a jar: URL with no entry has
;; no stream, and a name the jar does not hold is FileNotFoundException.
(define (jar-connection-entry self)
  (let ((name (embedded-jar-url-entry-name (embedded-connection-resource self))))
    (when (jolt-nil? name) (throw-jvm 'java.io.IOException "no entry name specified"))
    (let ((entry (embedded-jar-entry-for (embedded-connection-archive self) name)))
      (when (jolt-nil? entry)
        (throw-jvm 'java.io.FileNotFoundException
                   (string-append "JAR entry " name " not found in "
                                  (record-method-dispatch (embedded-connection-archive self) "getName" jolt-nil))))
      entry)))
(define (embedded-connection-bytes self)
  (let ((resource (embedded-connection-resource self)))
    (if (jar-connection-disk? resource)
        (begin (jar-connection-entry self)
               (or (jar-path-bytes (url-spec resource)) (make-bytevector 0)))
        (embedded-res-bytes resource))))
(define (embedded-connection-stream self)
  (make-in-stream (open-bytevector-input-port (embedded-connection-bytes self))))
(define (embedded-connection-content-type self)
  (let ((name (embedded-jar-url-entry-name (embedded-connection-resource self))))
    (if (jolt-nil? name) "content/unknown" (nio-content-type-of name))))

(define (embedded-connection-typed-content self classes)
  (let ((classes (if (jolt-array? classes) (ja->list classes) '())))
    (cond
      ((null? classes) (embedded-connection-stream self))
      ((exists (lambda (class) (and (jclass? class) (string=? (jclass-name class) "java.lang.String"))) classes)
       (utf8->string (embedded-connection-bytes self)))
      ((exists (lambda (class) (and (jclass? class) (string=? (jclass-name class) "java.io.InputStream"))) classes)
       (embedded-connection-stream self))
      (else (embedded-connection-stream self)))))

(define (embedded-request-properties self)
  (embedded-connection-get self 10))
(define (embedded-request-property self name)
  (let ((hit (assoc name (embedded-request-properties self))))
    (and hit (car (cdr hit)))))
(define (embedded-request-set! self name values)
  (embedded-connection-check-mutable! self)
  (let ((props (embedded-request-properties self)))
    (vector-set! (embedded-connection-state self) 10
                 (cons (cons name values) (filter (lambda (p) (not (string=? (car p) name))) props))))
  jolt-nil)
(define (embedded-request-map self)
  (fold-left (lambda (m p) (jolt-assoc m (car p) (make-pvec (list->vector (cdr p)))))
             empty-pmap (embedded-request-properties self)))

(define (embedded-connection-timeout! self index value)
  (let ((timeout (zip-int-arg value)))
    (when (< timeout 0)
      (throw-jvm 'IllegalArgumentException "timeout can not be negative"))
    (embedded-connection-set! self index timeout)))

(define (embedded-jar-connection-method self name args)
  (cond
    ((string=? name "connect") (list (embedded-connection-connect! self)))
    ((string=? name "setConnectTimeout") (list (embedded-connection-timeout! self 2 (car args))))
    ((string=? name "getConnectTimeout") (list (embedded-connection-get self 2)))
    ((string=? name "setReadTimeout") (list (embedded-connection-timeout! self 3 (car args))))
    ((string=? name "getReadTimeout") (list (embedded-connection-get self 3)))
    ((string=? name "getURL") (list (embedded-connection-resource self)))
    ((or (string=? name "getContentLength") (string=? name "getContentLengthLong"))
     (list (->num (bytevector-length (embedded-connection-bytes self)))))
    ((string=? name "getContentType") (list (embedded-connection-content-type self)))
    ((string=? name "getContentEncoding") (list jolt-nil))
    ((member name '("getExpiration" "getDate" "getLastModified")) (list (->num 0)))
    ((string=? name "getHeaderField") (list jolt-nil))
    ((string=? name "getHeaderFields") (list empty-pmap))
    ((string=? name "getHeaderFieldInt") (list (cadr args)))
    ((string=? name "getHeaderFieldLong") (list (cadr args)))
    ((string=? name "getHeaderFieldDate") (list (cadr args)))
    ((string=? name "getHeaderFieldKey") (list jolt-nil))
    ((string=? name "getContent") (list (embedded-connection-typed-content self (and (pair? args) (car args)))))
    ((string=? name "getPermission")
     (list (make-jhost "permission" (let ((r (embedded-connection-resource self)))
                                      (if (jar-connection-disk? r)
                                          (file-url->path (jar-connection-jar-path r))
                                          "embedded:read")))))
    ((string=? name "getInputStream") (list (embedded-connection-stream self)))
    ((string=? name "getOutputStream")
     (list (throw-jvm 'java.io.IOException "embedded resources are read-only")))
    ;; URLConnection.toString: the implementation class and the URL
    ((string=? name "toString")
     (list (string-append "sun.net.www.protocol.jar.JarURLConnection:"
                          (jolt-str-render-one (embedded-connection-resource self)))))
    ((string=? name "setDoInput") (list (embedded-connection-set! self 4 (car args))))
    ((string=? name "getDoInput") (list (embedded-connection-get self 4)))
    ((string=? name "setDoOutput") (list (embedded-connection-set! self 5 (car args))))
    ((string=? name "getDoOutput") (list (embedded-connection-get self 5)))
    ((string=? name "setAllowUserInteraction") (list (embedded-connection-set! self 6 (car args))))
    ((string=? name "getAllowUserInteraction") (list (embedded-connection-get self 6)))
    ((string=? name "setUseCaches") (list (embedded-connection-set! self 7 (car args))))
    ((string=? name "getUseCaches") (list (embedded-connection-get self 7)))
    ((string=? name "setIfModifiedSince") (list (embedded-connection-set! self 9 (car args))))
    ((string=? name "getIfModifiedSince") (list (embedded-connection-get self 9)))
    ((string=? name "setDefaultUseCaches") (list (embedded-connection-set! self 8 (car args))))
    ((string=? name "getDefaultUseCaches") (list (embedded-connection-get self 8)))
    ((string=? name "setRequestProperty")
     (list (embedded-request-set! self (car args) (list (cadr args)))))
    ((string=? name "addRequestProperty")
     (let ((old (embedded-request-property self (car args))))
       (list (embedded-request-set! self (car args) (append (if old (list old) '()) (list (cadr args)))))))
    ((string=? name "getRequestProperty") (list (or (embedded-request-property self (car args)) jolt-nil)))
    ((string=? name "getRequestProperties") (list (embedded-request-map self)))
    ((string=? name "getJarFileURL") (list (embedded-jar-file-url (embedded-connection-resource self))))
    ((string=? name "getEntryName") (list (embedded-jar-url-entry-name (embedded-connection-resource self))))
    ((string=? name "getJarFile") (list (embedded-connection-archive self)))
    ((string=? name "getManifest") (list (jar-manifest (embedded-connection-archive self))))
    ((string=? name "getJarEntry") (list (jar-connection-entry self)))
    ((string=? name "getAttributes")
     (list (record-method-dispatch (jar-connection-entry self) "getAttributes" jolt-nil)))
    ((string=? name "getMainAttributes")
     (list (let ((m (jar-manifest (embedded-connection-archive self))))
             (if (jolt-nil? m) jolt-nil (manifest-main m)))))
    ;; nothing here is signed: null, as the JDK answers for an unsigned entry
    ((string=? name "getCertificates") (list jolt-nil))
    (else #f)))

(hashtable-set! jhost-tag->fqn "embedded-jar-url-connection" "java.net.JarURLConnection")
(hashtable-set! jhost-tag->fqn "permission" "java.security.Permission")
(hashtable-set! jhost-tag->fqn "proxy" "java.net.Proxy")
(register-class-statics! "java.net.Proxy"
  (list (cons "NO_PROXY" (make-jhost "proxy" 'direct))))
(register-host-methods! "embedded-jar-url-connection"
  (map (lambda (name)
         (cons name
               (lambda (self . args)
                 (let ((result (embedded-jar-connection-method self name args)))
                   (if result (car result) (dispatch-miss self name (list->cseq args)))))))
       '("connect" "setConnectTimeout" "getConnectTimeout" "setReadTimeout" "getReadTimeout"
         "getURL" "getContentLength" "getContentLengthLong" "getContentType" "getContentEncoding"
         "getExpiration" "getDate" "getLastModified" "getHeaderField" "getHeaderFields"
         "getHeaderFieldInt" "getHeaderFieldLong" "getHeaderFieldDate" "getHeaderFieldKey"
         "getContent" "getPermission" "getInputStream" "getOutputStream" "toString"
         "setDoInput" "getDoInput" "setDoOutput" "getDoOutput"
         "setAllowUserInteraction" "getAllowUserInteraction" "setUseCaches" "getUseCaches"
         "setIfModifiedSince" "getIfModifiedSince" "setDefaultUseCaches" "getDefaultUseCaches"
         "setRequestProperty" "addRequestProperty" "getRequestProperty" "getRequestProperties"
         "getJarFileURL" "getEntryName" "getJarFile" "getManifest" "getJarEntry"
         "getAttributes" "getMainAttributes" "getCertificates")))

(define (embedded-url-open-connection resource)
  ;; resource, connected, connectTimeout, readTimeout, doInput, doOutput,
  ;; interaction, useCaches, defaultUseCaches, ifModifiedSince, requestProperties,
  ;; the JarFile once opened
  (make-jhost "embedded-jar-url-connection"
              (vector resource #f 0 0 #t #f #f #f #f 0 '() #f)))
