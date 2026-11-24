(ns mvn-http-test
  "Pure-function tests for jolt.mvn-http — URL parsing, redirect resolution,
  header/body framing, dechunking, and the request-smuggling guard. These need
  no network and no OpenSSL (defcfn is lazy; ensure-native! only runs inside
  fetch), so they run in the default gate. Run: make mvnhttp"
  (:require [jolt.mvn-http]
            [clojure.string :as str]))

;; the functions under test are private; reach them through their vars.
(def parse-url        (var jolt.mvn-http/parse-url))
(def resolve-location (var jolt.mvn-http/resolve-location))
(def header-end       (var jolt.mvn-http/header-end))
(def header-ci        (var jolt.mvn-http/header-ci))
(def parse-response   (var jolt.mvn-http/parse-response))
(def dechunk          (var jolt.mvn-http/dechunk))
(def ctl-free?        (var jolt.mvn-http/ctl-free?))
(def classify-status  (var jolt.mvn-http/classify-status))
(def with-retries     (var jolt.mvn-http/with-retries))
(def retry-delay-ms   (var jolt.mvn-http/retry-delay-ms))
(def parse-retry-after (var jolt.mvn-http/parse-retry-after))
(def lib-candidates   (var jolt.mvn-http/lib-candidates))
(def windows-libdirs  (var jolt.mvn-http/windows-openssl-libdirs-for))
(def openssl-libdirs  (var jolt.mvn-http/openssl-libdirs-for))
(def transport-error  (var jolt.mvn-http/transport-load-error))
(def connect-error-message (var jolt.mvn-http/connect-error-message))
(def max-attempts     @(var jolt.mvn-http/max-attempts))
(def windows?         @(var jolt.mvn-http/windows?))

(def ^:private fails (atom []))
(defn- ok= [expected actual label]
  (when-not (= expected actual)
    (swap! fails conj (str label " — expected " (pr-str expected) ", got " (pr-str actual)))))
(defn- throws [f label]
  (let [threw (try (f) false (catch :default _ true))]
    (when-not threw (swap! fails conj (str label " — expected a throw, got none")))))
(defn- bytes-of [s] (.getBytes ^String s "ISO-8859-1"))

(defn- run []
  ;; parse-url
  (ok= {:host "h" :port 443 :path "/p"} (parse-url "https://h/p") "parse-url simple")
  (ok= {:host "h" :port 8443 :path "/x?q=1"} (parse-url "https://h:8443/x?q=1") "parse-url port+query")
  (ok= {:host "h" :port 443 :path "/"} (parse-url "https://h") "parse-url no path")
  (throws #(parse-url "http://h/p") "parse-url rejects http")
  (throws #(parse-url "https://h/a\r\nb") "parse-url rejects CRLF in path")
  (throws #(parse-url "https://h\r\n/p") "parse-url rejects CRLF in host")

  ;; resolve-location (base host h, port 443)
  (let [base {:host "h" :port 443}]
    (ok= "https://h/a" (resolve-location base "/a") "reloc absolute path")
    (ok= "https://e/x" (resolve-location base "//e/x") "reloc scheme-relative")
    (ok= "https://e/x" (resolve-location base "https://e/x") "reloc absolute https")
    (ok= "https://h/a" (resolve-location base "a") "reloc relative")
    (ok= nil (resolve-location base "http://e/x") "reloc rejects http downgrade"))
  (ok= "https://h:8443/a" (resolve-location {:host "h" :port 8443} "/a") "reloc keeps non-443 port")

  ;; ctl-free?
  (ok= true  (ctl-free? "abc/def") "ctl-free plain")
  (ok= false (ctl-free? "a\rb")    "ctl-free CR")
  (ok= false (ctl-free? "a\nb")    "ctl-free LF")

  ;; header-end
  (ok= 6   (header-end (bytes-of "AB\r\n\r\nCD")) "header-end offset")
  (ok= nil (header-end (bytes-of "no terminator")) "header-end absent")

  ;; header-ci (case-insensitive)
  (let [pairs [["Content-Type" "text/xml"] ["Location" "https://x/y"]]]
    (ok= "https://x/y" (header-ci pairs "location") "header-ci lower")
    (ok= "https://x/y" (header-ci pairs "LOCATION") "header-ci upper")
    (ok= nil (header-ci pairs "x-absent") "header-ci absent"))

  ;; dechunk — hex size framing, binary-exact, terminal 0
  (ok= "hello" (String. (dechunk (bytes-of "5\r\nhello\r\n0\r\n\r\n")) "ISO-8859-1") "dechunk basic")
  (ok= "hello" (String. (dechunk (bytes-of "5;ext=1\r\nhello\r\n0\r\n\r\n")) "ISO-8859-1") "dechunk chunk-ext ignored")
  (let [raw (byte-array [0 -1 65])
        b (dechunk (bytes-of (str "3\r\n" (String. raw "ISO-8859-1") "\r\n0\r\n\r\n")))]
    (ok= (vec raw) (vec b) "dechunk binary-exact"))

  ;; parse-response — status, headers, content-length framing
  (let [r (parse-response (bytes-of "HTTP/1.1 200 OK\r\nContent-Length: 3\r\nContent-Type: x\r\n\r\nabc"))]
    (ok= 200 (:status r) "parse-response status")
    (ok= "abc" (String. ^bytes (:body r) "ISO-8859-1") "parse-response body")
    (ok= 3 (:content-length r) "parse-response content-length"))
  (let [r (parse-response (bytes-of "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n"))]
    (ok= 404 (:status r) "parse-response 404 status"))
  ;; chunked: content-length is not used to frame a chunked body
  (let [r (parse-response (bytes-of "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n"))]
    (ok= "hello" (String. ^bytes (:body r) "ISO-8859-1") "parse-response chunked body")
    (ok= nil (:content-length r) "parse-response chunked ignores content-length"))
  (throws #(parse-response (bytes-of "no header terminator here")) "parse-response no terminator throws")

  ;; --- outcome classification (jolt-ktiz.1, .5, .6) --------------------------
  ;; The whole point of the round: a fetch that failed must say WHY, because
  ;; deps.clj prints "not found" and only one of these means that. Pure, so it
  ;; is checked here rather than against a live repo.
  (ok= :ok        (classify-status 200) "classify 200 -> ok")
  (ok= :ok        (classify-status 299) "classify 299 -> ok")
  (ok= :not-found (classify-status 404) "classify 404 -> not-found")
  (ok= :not-found (classify-status 410) "classify 410 -> not-found")
  ;; the statuses a retry exists for; treating these as absent is jolt-ktiz.5
  (ok= :retryable (classify-status 408) "classify 408 -> retryable")
  (ok= :retryable (classify-status 429) "classify 429 -> retryable")
  (ok= :retryable (classify-status 500) "classify 500 -> retryable")
  (ok= :retryable (classify-status 502) "classify 502 -> retryable")
  (ok= :retryable (classify-status 503) "classify 503 -> retryable")
  (ok= :retryable (classify-status 504) "classify 504 -> retryable")
  ;; a repo that refuses us is not a repo that lacks the artifact, and no number
  ;; of retries changes either
  (ok= :failed    (classify-status 401) "classify 401 -> failed")
  (ok= :failed    (classify-status 403) "classify 403 -> failed")
  (ok= :failed    (classify-status 418) "classify 418 -> failed")

  ;; --- JOLT_OPENSSL_LIBDIR candidate construction ----------------------------
  ;; ensure-native! reads the env var at fetch time; the list construction is
  ;; the pure part under test. An explicit lib dir is tried before the
  ;; platform fallbacks; an unset or blank dir leaves the fallbacks untouched.
  (ok= ["/nix/lib/libssl.3.dylib" "/nix/lib/libssl.dylib" "/opt/homebrew/lib/libssl.dylib"]
       (lib-candidates ["/nix/lib"] ["libssl.3.dylib" "libssl.dylib"] ["/opt/homebrew/lib/libssl.dylib"])
       "lib-candidates: explicit dir entries come first, in name order")
  (ok= ["/d/libcrypto.so.3" "/d/libcrypto.so" "libcrypto.so.3" "libcrypto.so"]
       (lib-candidates ["/d"] ["libcrypto.so.3" "libcrypto.so"] ["libcrypto.so.3" "libcrypto.so"])
       "lib-candidates: bare-name fallbacks stay after the dir entries")
  (ok= ["libcrypto.so.3"]
       (lib-candidates [] ["libcrypto.so.3"] ["libcrypto.so.3"])
       "lib-candidates: empty dir list means fallbacks only")
  (ok= ["/a/x" "/a/y" "/b/x" "/b/y" "z"]
       (lib-candidates ["/a" "/b"] ["x" "y"] ["z"])
       "lib-candidates: directory-major, so libcrypto and libssl come from one install")
  (ok= true
       (try (lib-candidates "/nix/lib" ["x"] ["z"]) false (catch :default _ true))
       "lib-candidates: a directory STRING is refused, not walked character by character")
  (ok= ["C:\\Git\\mingw64\\bin\\libssl-3-x64.dll" "C:/ssl/bin/libssl-3-x64.dll" "libssl-3-x64.dll"]
       (lib-candidates ["C:\\Git\\mingw64\\bin" "C:/ssl/bin"] ["libssl-3-x64.dll"] ["libssl-3-x64.dll"])
       "lib-candidates: each candidate keeps its directory's separator")

  ;; Git for Windows' ucrt64/bin (2.56 on) then mingw64/bin (before 2.56), from
  ;; the three roots that place it, with native separators; the machine-wide
  ;; root usually arrives twice (ProgramFiles and ProgramW6432 name the same
  ;; directory, and Windows compares paths without case), a trailing separator
  ;; on a root adds nothing.
  (ok= ["C:\\Program Files\\Git\\ucrt64\\bin"
        "C:\\Program Files\\Git\\mingw64\\bin"
        "C:\\Users\\me\\AppData\\Local\\Programs\\Git\\ucrt64\\bin"
        "C:\\Users\\me\\AppData\\Local\\Programs\\Git\\mingw64\\bin"]
       (windows-libdirs "C:\\Program Files" "c:\\program files\\"
                        "C:\\Users\\me\\AppData\\Local")
       "Windows OpenSSL dirs: Git installs, one per directory whatever the spelling")
  (ok= []
       (windows-libdirs nil "" nil)
       "Windows OpenSSL dirs: no roots, no candidates")

  ;; The directory list ensure-native! builds: JOLT_OPENSSL_LIBDIR first when
  ;; set, then (Windows only) the Git for Windows directories, so the bare
  ;; names the loader searches PATH for come last.
  (ok= ["/nix/lib" "C:\\Program Files\\Git\\ucrt64\\bin" "C:\\Program Files\\Git\\mingw64\\bin"]
       (openssl-libdirs "/nix/lib" true "C:\\Program Files" nil nil)
       "openssl-libdirs: the explicit directory precedes the Git ones")
  (ok= ["C:\\Program Files\\Git\\ucrt64\\bin" "C:\\Program Files\\Git\\mingw64\\bin"]
       (openssl-libdirs "  " true "C:\\Program Files" nil nil)
       "openssl-libdirs: a blank JOLT_OPENSSL_LIBDIR is unset")
  (ok= []
       (openssl-libdirs nil false "C:\\Program Files" nil nil)
       "openssl-libdirs: the Git directories are Windows-only")

  ;; What a failed load tells the resolver: the loader's own message (it names
  ;; every candidate it tried) and the remedy, so the warning under a Windows
  ;; machine without Git for Windows says what to set.
  (ok= true
       (str/includes? (transport-error "jolt.ffi/load-library: cannot load any of a, b") "cannot load any of a, b")
       "transport error carries the loader's message")
  (ok= true
       (str/includes? (transport-error "x") "JOLT_OPENSSL_LIBDIR")
       "transport error names the remedy")

  ;; The struct addrinfo layout probe (#979) lives with the resolver now, in
  ;; jolt.socket.native, and is tested there (test/chez/socket-test.clj).

  ;; --- connect failure reporting (#979) --------------------------------------
  ;; Exhausting the candidates used to be reported as "connection refused" no
  ;; matter what the kernel said; the bionic bug was really EFAULT, and calling
  ;; it a refusal sent people looking at their network.
  ;; The code is an errno on POSIX and a WSAGetLastError value on Windows, and
  ;; the message says which (strerror speaks only errno), so the rows ask for
  ;; the platform's word; the gate runs on the Windows job too.
  (let [code-word (if windows? "error" "errno")]
    (ok= true (str/includes? (connect-error-message "repo.clojars.org" 443 14) (str code-word " 14"))
         "connect error names the code it got")
    (ok= true (str/includes? (connect-error-message "repo.clojars.org" 443 14) "repo.clojars.org:443")
         "connect error names host and port")
    (ok= false (str/includes? (connect-error-message "h" 443 111) (str code-word " 14"))
         "connect error reports the code it got, not a fixed one")
    (ok= true (str/includes? (connect-error-message "h" 443 111) (str code-word " 111"))
         "connect error names the other code the same way"))
  (ok= "could not connect to h:443" (connect-error-message "h" 443 nil)
       "connect error with no captured code stays plain")

  ;; --- retry policy (jolt-ktiz.2) --------------------------------------------
  ;; Driven through an injectable attempt fn so the gate stays network-free.
  ;; A :retryable outcome is retried up to the cap; anything else is final.
  (let [calls (atom 0)
        attempt (fn [outcomes] (fn [] (let [n @calls] (swap! calls inc) (nth outcomes n {:outcome :failed}))))]
    (reset! calls 0)
    (ok= :ok (:outcome (with-retries (attempt [{:outcome :retryable} {:outcome :retryable} {:outcome :ok}])))
         "retry: succeeds on the third attempt")
    (ok= 3 @calls "retry: took exactly three attempts")

    (reset! calls 0)
    (ok= :not-found (:outcome (with-retries (attempt [{:outcome :not-found} {:outcome :ok}])))
         "retry: a 404 is final, not retried")
    (ok= 1 @calls "retry: not-found took one attempt")

    (reset! calls 0)
    (ok= :failed (:outcome (with-retries (attempt [{:outcome :failed} {:outcome :ok}])))
         "retry: a hard failure is final, not retried")
    (ok= 1 @calls "retry: failed took one attempt")

    ;; exhausting the cap reports the LAST outcome, still :retryable, so the
    ;; caller can say "could not fetch after N attempts" rather than "not found"
    (reset! calls 0)
    (ok= :retryable (:outcome (with-retries (attempt (repeat 9 {:outcome :retryable}))))
         "retry: gives up as retryable, never as not-found")
    (ok= max-attempts @calls "retry: stops at the attempt cap"))

  ;; --- how long to wait before the next attempt -------------------------------
  ;; A 429 is a rate limit: it does not clear in the 200 ms a reset does, and
  ;; Central sends them to shared CI runners. The server's Retry-After wins
  ;; (capped, so a hostile or confused header cannot stall a build for an hour);
  ;; without one, a 429 backs off for seconds, not milliseconds.
  (ok= 7 (parse-retry-after "7") "Retry-After: delta-seconds")
  (ok= 7 (parse-retry-after " 7 ") "Retry-After: surrounding space")
  (ok= nil (parse-retry-after "Wed, 21 Oct 2015 07:28:00 GMT") "Retry-After: an HTTP-date is not used")
  (ok= nil (parse-retry-after "-3") "Retry-After: negative is ignored")
  (ok= nil (parse-retry-after nil) "Retry-After: absent")
  (ok= 200 (retry-delay-ms {:outcome :retryable :error "reset"} 1) "delay: a reset backs off briefly")
  (ok= 600 (retry-delay-ms {:outcome :retryable :status 503} 2) "delay: a 503 with no header backs off briefly")
  (ok= 2000 (retry-delay-ms {:outcome :retryable :status 429} 1) "delay: a 429 with no header waits seconds")
  (ok= 5000 (retry-delay-ms {:outcome :retryable :status 429} 2) "delay: a second 429 waits longer")
  (ok= 4000 (retry-delay-ms {:outcome :retryable :status 429 :retry-after 4} 1) "delay: Retry-After wins on a 429")
  (ok= 4000 (retry-delay-ms {:outcome :retryable :status 503 :retry-after 4} 1) "delay: Retry-After wins on a 503")
  (ok= 30000 (retry-delay-ms {:outcome :retryable :status 429 :retry-after 3600} 1) "delay: Retry-After is capped")
  (ok= :retry-after-seen
       (let [raw (.getBytes "HTTP/1.1 429 Too Many Requests\r\nRetry-After: 9\r\nContent-Length: 0\r\n\r\n" "UTF-8")]
         (when (= 9 (:retry-after (parse-response raw))) :retry-after-seen))
       "parse-response carries Retry-After")

  ;; --- a transport that cannot load, end to end --------------------------------
  ;; Network-free: with no loadable libcrypto candidate, fetch* fails before any
  ;; socket, and its :error is what the resolver prints per repository — the
  ;; candidate the loader tried and the remedy. The candidate vars are put back
  ;; after; ensure-native! stays unready after a failure, so nothing is cached.
  (let [cands (var jolt.mvn-http/crypto-candidates)
        names (var jolt.mvn-http/crypto-names)
        saved [@cands @names]]
    (try
      (alter-var-root cands (constantly ["/nonexistent/jolt-mvn-http/libcrypto.so.3"]))
      (alter-var-root names (constantly ["libcrypto-jolt-mvn-http-nonexistent.so.3"]))
      (let [r (jolt.mvn-http/fetch* "https://repo1.maven.org/maven2/x.pom" "/nonexistent/jolt-mvn-http/x.pom")]
        (ok= :failed (:outcome r) "unloadable transport: the fetch fails")
        (ok= true (str/includes? (str (:error r)) "/nonexistent/jolt-mvn-http/libcrypto.so.3")
             "unloadable transport: the error names the candidate tried")
        (ok= true (str/includes? (str (:error r)) "JOLT_OPENSSL_LIBDIR")
             "unloadable transport: the error names the remedy")
        (ok= nil (jolt.mvn-http/loaded-native-libraries)
             "unloadable transport: nothing is reported as loaded"))
      (finally
        (alter-var-root cands (constantly (first saved)))
        (alter-var-root names (constantly (second saved)))))))

(defn -main [& _]
  (run)
  (if (seq @fails)
    (do (println "mvn-http-test: FAILED")
        (doseq [f @fails] (println "  -" f))
        (throw (ex-info "mvn-http-test failures" {:count (count @fails)})))
    (println "mvn-http-test: passed")))

;; run on load so `jolt run test/mvn_http_test.clj` executes the checks.
(-main)
