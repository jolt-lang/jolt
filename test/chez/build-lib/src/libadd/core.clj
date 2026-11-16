(ns libadd.core
  (:require [jolt.ffi :as ffi]))

(def point-layout (ffi/layout [:struct [[:x :float] [:y :float]]]))

(defn add [x y] (+ x y))
(defn point-size [] (ffi/layout-size point-layout))

;; Publish `add` and `point_size` as C-callable entry points. An embedder
;; resolves them via jolt_lookup("<name>") after jolt_library_init. export! runs
;; at the library's top-level (during heap build), so both are available before
;; jolt_library_init returns.
;;
;; point_size is the one that reaches the Clojure half of jolt.ffi. A library
;; image is not the build driver's image, so a driver that reads its own loaded
;; set as the library's leaves layout-size interned but UNBOUND at the call.
(ffi/export! "add" add [:int :int] :int)
(ffi/export! "point_size" point-size [] :int)

;; gzip_ok answers 1 when a gzip round trip works inside the library (#916):
;; java.util.zip reaches a working zlib, and the bytes are deflated, not stored.
;; zlib-register-smoke.sh pins that the zlib is the library's own.
(defn gzip-ok []
  (let [b (java.io.ByteArrayOutputStream.)]
    (with-open [o (java.util.zip.GZIPOutputStream. b)]
      (.write o (.getBytes (apply str (repeat 1000 "zip")) "UTF-8")))
    (if (and (= 3000 (count (slurp (java.util.zip.GZIPInputStream.
                                     (java.io.ByteArrayInputStream. (.toByteArray b))))))
             (< (.size b) 100))
      1
      0)))
(ffi/export! "gzip_ok" gzip-ok [] :int)

;; alloc_work is the export the release-thread driver calls from a thread the
;; embedder started (#1234). It is :collect-safe because it arrives on a foreign
;; thread, and it allocates on every call so that a loop of them needs a
;; collection: the point at which a parked, still-active init thread used to
;; hold every other thread off for good.
(defn alloc-work [n]
  (let [v (mapv inc (range 256))]
    (+ n (count v))))
(ffi/export! "alloc_work" alloc-work [:int] :int :collect-safe)

;; code_churn compiles and drops thousands of small functions, collecting after
;; each round, so the Chez kernel frees code chunks and allocates new ones. On
;; macOS arm64 that runs through the library's code region (jolt#1246), whose
;; freed ranges are reused; this answers 1 when every compiled function still
;; computes the right thing.
(defn code-churn [rounds]
  (if (every? true?
              (for [r (range rounds)]
                (let [fs (mapv (fn [k] (eval (list 'fn ['x] (list '+ 'x k r)))) (range 1500))
                      ok (= (mapv #(% 1) fs) (mapv #(+ 1 % r) (range 1500)))]
                  (System/gc)
                  ok)))
    1
    0))
(ffi/export! "code_churn" code-churn [:int] :int)

;; scheme_fault reads through a null pointer and answers 1 when jolt turns the
;; fault into an exception. The signals driver calls it while a host owns
;; SIGSEGV too (#1277): a fault in Scheme code is still jolt's to handle, while
;; one in host code goes to the host. :collect-safe so a host thread can call it.
(defn scheme-fault []
  (try (ffi/read ffi/null :int) 0
       (catch Throwable _ 1)))
(ffi/export! "scheme_fault" scheme-fault [] :int :collect-safe)

;; watch_usr2 registers a sun.misc.Signal handler for USR2 from inside the
;; library, and usr2_seen answers how many times it has run (#1679). The kernel's
;; signal relay installs a C handler for USR2 that chains to the host's own, and
;; jolt_library_shutdown gives USR2 back (driver-relay.c). :collect-safe because
;; the driver calls them after releasing its init thread.
(def usr2-count (atom 0))
(defn watch-usr2 []
  (sun.misc.Signal/handle (sun.misc.Signal. "USR2")
                          (reify sun.misc.SignalHandler (handle [_ _] (swap! usr2-count inc))))
  1)
(defn usr2-seen [] @usr2-count)
(ffi/export! "watch_usr2" watch-usr2 [] :int :collect-safe)
(ffi/export! "usr2_seen" usr2-seen [] :int :collect-safe)
