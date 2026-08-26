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
(def lib-candidates   (var jolt.mvn-http/lib-candidates))
(def windows-libdirs  (var jolt.mvn-http/windows-openssl-libdirs-for))
(def max-attempts     @(var jolt.mvn-http/max-attempts))

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
  (ok= ["C:\\Program Files/Git/mingw64/bin"
        "C:\\Users\\me\\AppData\\Local/Programs/Git/mingw64/bin"]
       (windows-libdirs "C:\\Program Files" "C:\\Program Files"
                        "C:\\Users\\me\\AppData\\Local")
       "Windows OpenSSL dirs include Git installs and remove duplicates")

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
    (ok= max-attempts @calls "retry: stops at the attempt cap")))

(defn -main [& _]
  (run)
  (if (seq @fails)
    (do (println "mvn-http-test: FAILED")
        (doseq [f @fails] (println "  -" f))
        (throw (ex-info "mvn-http-test failures" {:count (count @fails)})))
    (println "mvn-http-test: passed")))

;; run on load so `jolt run test/mvn_http_test.clj` executes the checks.
(-main)
