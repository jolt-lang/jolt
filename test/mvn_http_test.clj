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
  (throws #(parse-response (bytes-of "no header terminator here")) "parse-response no terminator throws"))

(defn -main [& _]
  (run)
  (if (seq @fails)
    (do (println "mvn-http-test: FAILED")
        (doseq [f @fails] (println "  -" f))
        (throw (ex-info "mvn-http-test failures" {:count (count @fails)})))
    (println "mvn-http-test: passed")))

;; run on load so `joltc run test/mvn_http_test.clj` executes the checks.
(-main)
