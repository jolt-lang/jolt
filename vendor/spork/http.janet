# Vendored from janet-lang/spork (MIT license) @ 3bdcf58
# for the janet.spork.http/* bridge — see evaluator.janet. Do not edit here.
###
### http.janet
###
### Pure Janet HTTP/1.1 parser, client, and server.
###

(def- chunk-size (* 16 4096))

(defn- pre-pop
  "Remove n bytes from front of buffer"
  [buf n]
  (buffer/blit buf buf 0 n)
  (buffer/popn buf n))

(def- http-grammar
  ~{:request-status (* :method :ws :path :ws "HTTP/1." :d :any-ws :rn)
    :response-status (* "HTTP/1." :d :ws (/ ':d+ ,scan-number)
                        :ws '(some :printable) :rn)
    :ws (some (set " \t"))
    :any-ws (any (set " \t"))
    :rn "\r\n"
    :method '(some (range "AZ"))
    :path-chr (range "az" "AZ" "09" "!!" "$9" ":;" "==" "?@" "~~" "__")
    :path '(some :path-chr)
    :printable (range "\x20~" "\t\t")
    :headers (* (any :header) :rn)
    # lower case header names since http headers are case-insensitive
    :header-name (/ '(some (range "\x219" ";~")) ,string/ascii-lower)
    :header-value '(any :printable)
    :header (* :header-name ":" :any-ws :header-value :rn)})

(def request-peg
  "PEG for parsing HTTP requests"
  (peg/compile
    (table/to-struct
      (merge {:main ~(* :request-status :headers)}
             http-grammar))))

(def response-peg
  "PEG for parsing HTTP responses"
  (peg/compile
    (table/to-struct (merge
                       {:main ~(* :response-status :headers)}
                       http-grammar))))

(defn- accum-key-values
  "Accumulate key-value pairs based on arg index (even = key, odd = value) into a table and combine 
  duplicate keys into arrays of values (rather than overwriting). Used for both query strings and headers."
  [& args]
  (def tab @{})
  (loop [i :range [0 (length args) 2]
         :let [k (get args i) v (get args (+ 1 i))]]
    (if-let [item (in tab k)]
      (if (array? item)
        (array/push item v)
        (put tab k @[item v]))
      (put tab k v)))
  tab)

(defn- read-header
  "Read an HTTP header from a stream."
  [conn buf peg key1 key2]
  (var head nil)
  (var last-index 0)
  (forever
    (when-let [end (string/find "\r\n\r\n" buf last-index)]
      (set head
           (if-let [matches (peg/match peg buf)]
             (let [[a b] matches
                   headers (accum-key-values ;(array/remove matches 0 2))]
               @{:headers headers
                 :connection conn
                 :buffer (pre-pop buf (+ 4 end))
                 :head-size (+ 4 end)
                 key1 a
                 key2 b})
             :error))
      (break))
    (set last-index (max 0 (- (length buf) 4)))
    (unless (:read conn chunk-size buf)
      (set head :error)
      (break)))
  head)


(def query-string-grammar
  "Grammar that parses a query string (sans url path and ? character) and returns a table."
  (peg/compile
    ~{:qchar (+ (* "%" (/ (number (* :h :h) 16) ,string/from-bytes)) (* "+" (constant " ")))
      :kchar (+ :qchar (* (not (set "&=;")) '1))
      :vchar (+ :qchar (* (not (set "&;")) '1))
      :key (accumulate (some :kchar))
      :value (accumulate (any :vchar))
      :entry (* :key (+ (* "=" :value) (constant true)) (+ (set ";&") -1))
      :main (/ (any :entry) ,accum-key-values)}))

(defn read-request
  ``Read an HTTP request header from a connection. Returns a table with the following keys:
  * `:headers` - table mapping header names to header values. Header names are lowercase.
  * `:connection` - the connection stream for the header.
  * `:buffer` - the buffer instance that may contain extra bytes.
  * `:head-size` - the number of bytes used by the header.
  * `:method` - the HTTP method used.
  * `:path` - the path of the resource requested.

  The following keys are also present, but omitted if the user passes a truthy parameter to `no-query`.
  * `:route` - path of the resource requested without query string.
  * `:query-string` - segment of HTTP path after first ? character.
  * `:query` - the query string parsed into a table. Supports a single string value
     for every string key, and any query parameters that aren't given a value are mapped to true.

  Note that data is read in chunks and any data after the header terminator is
  stored in `:buffer`.``
  [conn buf &opt no-query]
  (def head (read-header conn buf request-peg :method :path))
  (if (= :error head) (break head))

  # Parse query string separately
  (unless no-query
    (def fullpath (get head :path))
    (def qloc (string/find "?" fullpath))
    (def path (if qloc (string/slice fullpath 0 qloc) fullpath))
    (def qs (if qloc (string/slice fullpath (inc qloc)) nil))
    (put head :route path)
    (put head :query-string qs)
    (when qs
      (when-let [m (peg/match query-string-grammar qs)]
        (put head :query (first m)))))

  head)

(defn read-response
  ``Read an HTTP response header from a connection. Returns a table with the following keys:
  * `:headers` - table mapping header names to header values. Header names are lowercase.
  * `:connection` - the connection stream for the header.
  * `:buffer` - the buffer instance that may contain extra bytes.
  * `:head-size` - the number of bytes used by the header.
  * `:status` - the HTTP status code.
  * `:message` - the HTTP status message.

  Note that data is read in chunks and any data after the header terminator is
  stored in `:buffer`.``
  [conn buf]
  (read-header conn buf response-peg :status :message))

(def status-messages
  "Mapping of HTTP status codes to their status message."
  {100 "Continue"
   101 "Switching Protocols"
   102 "Processing"
   200 "OK"
   201 "Created"
   202 "Accepted"
   203 "Non-Authoritative Information"
   204 "No Content"
   205 "Reset Content"
   206 "Partial Content"
   207 "Multi-Status"
   208 "Already Reported"
   226 "IM Used"
   300 "Multiple Choices"
   301 "Moved Permanently"
   302 "Found"
   303 "See Other"
   304 "Not Modified"
   305 "Use Proxy"
   307 "Temporary Redirect"
   308 "Permanent Redirect"
   400 "Bad Request"
   401 "Unauthorized"
   402 "Payment Required"
   403 "Forbidden"
   404 "Not Found"
   405 "Method Not Allowed"
   406 "Not Acceptable"
   407 "Proxy Authentication Required"
   408 "Request Timeout"
   409 "Conflict"
   410 "Gone"
   411 "Length Required"
   412 "Precondition Failed"
   413 "Payload Too Large"
   414 "URI Too Long"
   415 "Unsupported Media Type"
   416 "Range Not Satisfiable"
   417 "Expectation Failed"
   421 "Misdirected Request"
   422 "Unprocessable Entity"
   423 "Locked"
   424 "Failed Dependency"
   426 "Upgrade Required"
   428 "Precondition Required"
   429 "Too Many Requests"
   431 "Request Header Fields Too Large"
   451 "Unavailable For Legal Reasons"
   500 "Internal Server Error"
   501 "Not Implemented"
   502 "Bad Gateway"
   503 "Service Unavailable"
   504 "Gateway Timeout"
   505 "HTTP Version Not Supported"
   506 "Variant Also Negotiates"
   507 "Insufficient Storage"
   508 "Loop Detected"
   510 "Not Extended"
   511 "Network Authentication Required"})

(defn- write-body
  "Write the body of an HTTP request, adding Content-Length header
  or Transfer-Encoding: chunked"
  [conn buf body]
  (cond
    (nil? body)
    (do
      (buffer/push buf "\r\n")
      (:write conn buf))

    (bytes? body)
    (do
      (buffer/format buf "Content-Length: %d\r\n\r\n%V" (length body) body)
      (:write conn buf))

    # default - iterate chunks
    (do
      (buffer/format buf "Transfer-Encoding: chunked\r\n\r\n")
      (each chunk body
        (assert (bytes? chunk) "expected byte chunk")
        (buffer/format buf "%x\r\n%V\r\n" (length chunk) chunk)
        (:write conn buf)
        (buffer/clear buf))
      (buffer/format buf "0\r\n\r\n")
      (:write conn buf)))
  (buffer/clear buf))

(defn- read-until
  "Read single bytes from connection into buffer until the provided byte
  sequence is found within it. The buffer need not be empty. Returns the number
  of bytes from the start of the buffer until the substring."
  [conn buf needle &opt start-index]
  (default start-index 0)
  (when-let [pos (peg/find needle buf start-index)]
    (break pos))
  (prompt :exit
    (forever
      (unless (:read conn 1 buf)
        (error "end of stream"))
      (when-let [pos (peg/find needle buf start-index)]
        (return :exit pos)))))

(defn read-body
  "Given a request, read the HTTP body from the connection. Returns the body as a buffer.
  If the request has no body, returns nil."
  [req]
  (when-let [body (in req :body)] (break body))
  (def headers (in req :headers))

  # In place content
  (when-let [cl (in headers "content-length")]
    (def {:buffer buf
          :connection conn} req)
    (def content-length (scan-number cl))
    (def remaining (- content-length (length buf)))
    (when (pos? remaining)
      (:chunk conn remaining buf))
    (put req :body buf)
    (break buf))

  # event stream aka SSE
  (when (-?>> (in headers "content-type")
              (string/has-prefix? "text/event-stream"))
    (def {:buffer buf
          :connection conn} req)
    (read-until conn buf "\n\n")
    (put req :body buf)
    (break buf))

  # Chunked encoding
  # TODO: The specification can have multiple transfer encodings so this
  # precise string matching may not work for every case.
  (when (= (in headers "transfer-encoding") "chunked")
    (def {:buffer buf
          :connection conn} req)
    (def body (buffer/new chunk-size))
    (var i 0)
    (forever
      (def chunk-length-end-pos (read-until conn buf "\r\n" i))
      (var chunk-length (scan-number (slice buf i chunk-length-end-pos) 16))
      (when (zero? chunk-length)
        (read-until conn buf "\r\n" (+ 2 chunk-length-end-pos))
        (break))
      # If there's any data already read, blit that over first.
      (let [leftover-start (+ chunk-length-end-pos 2)
            leftover (- (length buf) leftover-start)
            blit-amount (min leftover chunk-length)] # prevent overreading
        (unless (zero? blit-amount)
          (buffer/blit body buf -1 leftover-start (+ leftover-start blit-amount))
          (-= chunk-length blit-amount))
        (set i (+ leftover-start blit-amount)))
      (if (= i (length buf))
        # Basic case: the buffer has been exhausted and hereonout we can read
        # from the socket directly.
        (do
          (:chunk conn chunk-length body)
          (unless (:read conn 2 buf) # trailing CRLF (not included in chunk length proper)
            (error "end of stream"))
          # Clear buffer out. We ain't gonna need it no more.
          (buffer/clear buf)
          (set i 0))
        # Alternatively, the pre-read data in the buffer was plentiful and we
        # just managed to copy an entire chunk out of it. Just increment past
        # the CRLF, if it's not already there, and proceed to loop again.
        (set i (+ (read-until conn buf "\r\n" i) 2))))
    (put req :body body)
    (break body))

  # no body, just return nil
  nil)

(defn send-response
  ``Send an HTTP response over a connection. Will automatically use chunked
  encoding if body is not a byte sequence. `response` should be a table
  with the following keys:

  * `:headers` - optional headers to write
  * `:status` - integer status code to write
  * `:body` - optional byte sequence or iterable (for chunked body)
     for returning contents. The iterable can be lazy, i.e. for streaming
     data.``
  [conn response &opt buf]
  (default buf @"")
  (def status (get response :status 200))
  (def message (in status-messages status))
  (buffer/format buf "HTTP/1.1 %d %s\r\n" status message)
  (def headers (get response :headers {}))

  (eachp [k v] headers
    # Values can be lists when representing duplicate headers (e.g.: multiple "Set-Cookie" entries)
    (if (or (tuple? v) (array? v))
      (each ve v (buffer/format buf "%V: %V\r\n" k ve))
      (buffer/format buf "%V: %V\r\n" k v)))

  (write-body conn buf (in response :body)))

###
### Server Middleware
###

(defn- bytes-to-mw
  [b]
  (fn mw [&] {:status 200 :body b}))

(defn middleware
  "Coerce any type to http middleware"
  [x]
  (case (type x)
    :function x
    :number (let [msg (get status-messages x)]
              (assert msg (string "unknown http status code when making middleware: " x))
              (fn mw [&] {:status x :body msg}))
    :string (bytes-to-mw x)
    :buffer (bytes-to-mw x)
    (fn mw [&] x)))

(defn router
  "Creates a router middleware. A router will dispatch to different routes based on
  the URL path."
  [routes]
  (fn router-mw [req]
    (def r (or
             (get routes (get req :route))
             (get routes :default)))
    (if r ((middleware r) req) {:status 404 :body "Not Found"})))

(defn logger
  "Creates a logging middleware. The logger middleware prints URL route, return status, and elapsed request time."
  [nextmw]
  (fn logger-mw [req]
    (def {:path path
          :method method} req)
    (def start-clock (os/clock))
    (def ret (nextmw req))
    (def end-clock (os/clock))
    (def elapsed (string/format "%.3f" (* 1000 (- end-clock start-clock))))
    (def status (or (get ret :status) 200))
    (print method " " status " " path " elapsed " elapsed "ms")
    (flush)
    ret))

(def cookie-grammar
  "Grammar to parse a cookie header to a series of keys and values."
  (peg/compile
    {:content '(some (if-not (set "=;") 1))
     :eql "="
     :sep '(between 1 2 (set "; "))
     :main '(some (* (<- :content) :eql (<- :content) (? :sep)))}))

(defn cookies
  "Parses cookies into the table under :cookies key"
  [nextmw]
  (fn cookie-mw [req]
    (-> req
        (put :cookies
             (or (-?>> [:headers "cookie"]
                       (get-in req)
                       (peg/match cookie-grammar)
                       (apply table))
                 {}))
        nextmw)))

###
### Server boilerplate
###

(defn server-handler
  ``A simple connection handler for an HTTP server.
  When a connection is accepted. Call this with a handler
  function to handle the connect. The handler will be called
  with one argument, the request table, which will contain the
  following keys:
  * `:head-size` - number of bytes in the http header.
  * `:headers` - table mapping header names to header values.
  * `:connection` - the connection stream for the header.
  * `:buffer` - the buffer instance that may contain extra bytes.
  * `:path` - HTTP path.
  * `:method` - HTTP method, as a string.``
  [conn handler]
  (def handler (middleware handler))
  (defer (:close conn)

    # Get request header
    (def buf (buffer/new chunk-size))
    (def req (read-request conn buf))

    # Handle bad request
    (when (= :error req)
      (send-response conn {:status 400} (buffer/clear buf))
      (break))

    # Add some extra keys to the request
    (put req :connection conn)

    # Do something with request header
    (def response (handler req))

    # Now send back response
    (send-response conn response @"")))

(defn server
  "Makes a simple http server. By default it binds to 0.0.0.0:8000,
  returns a new server stream.
  Simply wraps http/server-handler with a net/server."
  [handler &opt host port]
  (default host "0.0.0.0")
  (default port 8000)
  (defn new-handler
    [conn]
    (server-handler conn handler))
  (net/server host port new-handler))

###
### HTTP Client
###

(def- url-peg-source
  ~{:main (* (+ :https :http) :fqdn :port :path)
    :https (* (constant "https") "https://")
    :http (* (constant "http") "http://")
    :fqdn '(some (range "az" "AZ" "09" ".." "--"))
    :port (+ (* ":" ':d+) (constant nil))
    :path-chr (range "az" "AZ" "09" "!!" "$9" ":;" "==" "?@" "~~" "__")
    :path (+ '(some :path-chr) (constant "/"))})

(def url-grammar
  "Grammar to parse a URL into scheme, domain, port, and path. Supports
  both http:// and https:// protocols. Returns [scheme host port path]."
  (peg/compile url-peg-source))

(defn request
  ``Make an HTTP request to a server.
  Returns a table containing response information.
  * `:head-size` - number of bytes in the http header
  * `:headers` - table mapping header names to header values. Header names are lowercase.
  * `:connection` - the connection stream for the header.
  * `:buffer` - the buffer instance that may contain extra bytes.
  * `:status` - HTTP status code as an integer.
  * `:message` - HTTP status message.
  * `:body` - Bytes of the response body.
  
  Options:
  * `:body` - Request body content
  * `:headers` - Request headers table
  * `:stream-factory` - Function to create connection stream. Defaults to net/connect.
    Signature: (stream-factory host port stream-opts)
  * `:stream-opts` - Options table passed to stream-factory``
  [method url &keys
   {:body body
    :headers headers
    :stream-factory stream-factory
    :stream-opts stream-opts}]
  (def x (peg/match url-grammar url))
  (assert x (string "invalid url: " url))
  (def [scheme host raw-port path] x)
  # Default port based on scheme
  (def port (or raw-port (if (= scheme "https") "443" "80")))
  (def buf @"")
  (buffer/format buf "%s %s HTTP/1.1\r\nHost: %s:%s\r\n" method path host port)
  (when headers
    (eachp [k v] headers
      (buffer/format buf "%s: %s\r\n" k v)))

  # Use custom stream-factory or default to net/connect
  (let [make-conn (or stream-factory net/connect)
        conn (if stream-opts
               (make-conn host port stream-opts)
               (make-conn host port))]
    (with [conn conn]

      # Make request
      (write-body conn buf body)

      # Parse response pure janet
      (def res (read-response conn buf))
      (when (= :error res) (error res))
      # HEAD responses have no body per HTTP spec, skip read-body
      (unless (= method "HEAD")
        (read-body res))

      # TODO - handle redirects with Location header
      res)))
