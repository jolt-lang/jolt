;; jolt.winsock — Winsock initialization, once per process.
;;
;; On Windows a socket call before WSAStartup fails, and there is nothing in the
;; failure that says so: socket() answers INVALID_SOCKET and getaddrinfo answers
;; "unknown host", which read as an unusable machine rather than an uninitialized
;; library. jolt had solved this twice already — jolt.nrepl and jolt.mvn-http each
;; carried their own copy, which is why Maven resolution and nREPL worked on
;; Windows while every java.net socket did not (jolt-lang/jolt#1107).
;;
;; One copy, in a namespace that depends on nothing but jolt.ffi, so the deps
;; resolver can reach it without pulling in the java.net shim.
;;
;; Two things the copies had to know and a third caller would have had to
;; rediscover:
;;
;;   - ws2_32 must be dlopened BY NAME. Its symbols are not in jolt.exe's export
;;     table even though -lws2_32 is linked, so the process handle alone does not
;;     resolve socket(), and a jolt.ffi binding to one fails with "no entry".
;;   - WSADATA is ~400 bytes on x64 and WSAStartup writes the whole struct, so the
;;     buffer has to be generous; 512 is.
;;
;; WSAStartup is reference-counted and calling it twice is harmless, but it is a
;; foreign call and a library load per call, so the work is behind a delay: every
;; entry point can say `(ensure!)` on the way in without thinking about it.

(ns jolt.winsock
  "Winsock (ws2_32) initialization for Windows hosts; a no-op everywhere else.

  Call (ensure!) before the first socket call on any path that can reach one. It
  is idempotent, cheap after the first call, and does nothing at all off Windows."
  (:require [jolt.ffi :as ffi]
            [clojure.string :as str]))

(def ^:private windows?
  (str/includes? (str/lower-case (or (System/getProperty "os.name") "")) "win"))

;; The Winsock version to request: 2.2, as MAKEWORD(2,2) — low byte major, high
;; byte minor. Every Windows since 98 has it.
(def ^:private WINSOCK-2-2 0x0202)

(ffi/defcfn c-wsa-startup "WSAStartup" [:int :pointer] :int)

(def ^:private started
  (delay
    (when windows?
      ;; Both spellings: the DLL by name is what actually resolves, and the bare
      ;; one is what a MinGW-hosted loader may prefer. load-library takes the
      ;; ordered candidates and raises naming all of them if none loads.
      (ffi/load-library ["ws2_32.dll" "ws2_32"])
      (let [wsadata (ffi/alloc 512)]
        (try
          (let [r (c-wsa-startup WINSOCK-2-2 wsadata)]
            (when-not (zero? r)
              (throw (java.io.IOException. (str "WSAStartup failed: " r)))))
          (finally (ffi/free wsadata)))))
    true))

(defn ensure!
  "Initialize Winsock if this is Windows and it has not been done. Idempotent.
  Returns true. Throws IOException if WSAStartup itself fails."
  []
  @started)
