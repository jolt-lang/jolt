;; jolt.ffi bulk byte-buffer gate — the foreign<->bytevector block moves
;; (sa-foreign-bytes-ref!/-set!) that read-array / read-into! / write-array /
;; read-bytes / write-bytes are built on. The property under test is that a
;; block move is BINARY-FAITHFUL: 0x00, 0x80 and 0xff survive the unsigned-octet
;; to signed-byte fold in both directions, and a byte that is not valid UTF-8
;; never reaches a decoder.
;; Run: bin/jolt run test/chez/jolt-ffi-bytes-test.clj (smoke.sh greps for
;; "JOLT-FFI-BYTES-TEST OK").
(ns jolt-ffi-bytes-test)

(require '[jolt.ffi :as ffi])

(def failures (atom []))

(defmacro check-eq [label got want]
  `(do
     (print (str "  .. " ~label "\n"))
     (flush)
     (let [g# ~got w# ~want]
       (when-not (= g# w#)
         (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#)))))))

;; every byte value, so nothing in the fold is exercised only by accident
(def all-bytes (byte-array (map #(byte (if (> % 127) (- % 256) %)) (range 256))))

(let [buf (ffi/alloc 512)]
  (ffi/write-array buf all-bytes)
  (check-eq "write-array returns the octet count" (ffi/write-array buf all-bytes) 256)
  (check-eq "read-array round-trips all 256 byte values"
            (seq (ffi/read-array buf 256)) (seq all-bytes))
  (check-eq "read-array of 0 bytes is an empty array"
            (alength (ffi/read-array buf 0)) 0)

  ;; unsigned octets on the foreign side, signed bytes on the jolt side
  (check-eq "0x80 reads back as -128" (ffi/read (+ buf 128) :uint8 0) 128)
  (check-eq "0xff reads back as -1" (aget (ffi/read-array (+ buf 255) 1) 0) -1)

  ;; --- read-into! -----------------------------------------------------------
  (let [dst (byte-array 300)]
    (check-eq "read-into! returns the octet count" (ffi/read-into! buf dst 20 256) 256)
    (check-eq "read-into! lands at the offset"
              (seq (java.util.Arrays/copyOfRange dst 20 276)) (seq all-bytes))
    (check-eq "read-into! leaves the prefix alone"
              (seq (java.util.Arrays/copyOfRange dst 0 20)) (repeat 20 0))
    (check-eq "read-into! leaves the suffix alone"
              (seq (java.util.Arrays/copyOfRange dst 276 300)) (repeat 24 0))
    (check-eq "read-into! of 0 bytes is a no-op" (ffi/read-into! buf dst 0 0) 0))

  ;; a stream read in chunks fills one buffer — the reason read-into! exists
  (let [dst (byte-array 256)]
    (dotimes [i 4] (ffi/read-into! (+ buf (* i 64)) dst (* i 64) 64))
    (check-eq "chunked read-into! reassembles the whole block"
              (seq dst) (seq all-bytes)))

  ;; --- bounds ---------------------------------------------------------------
  (check-eq "read-into! past the end throws"
            (try (ffi/read-into! buf (byte-array 8) 4 8) :no-throw
                 (catch Throwable _ :threw))
            :threw)
  (check-eq "read-into! at a negative offset throws"
            (try (ffi/read-into! buf (byte-array 8) -1 2) :no-throw
                 (catch Throwable _ :threw))
            :threw)
  (check-eq "write-array past the end throws"
            (try (ffi/write-array buf all-bytes 200 100) :no-throw
                 (catch Throwable _ :threw))
            :threw)

  ;; --- write-array slice ----------------------------------------------------
  (let [zeros (byte-array 512)]
    (ffi/write-array buf zeros)
    (check-eq "write-array slice returns its length" (ffi/write-array buf all-bytes 250 6) 6)
    (check-eq "write-array slice writes exactly that slice"
              (seq (ffi/read-array buf 6))
              (seq (java.util.Arrays/copyOfRange all-bytes 250 256)))
    (check-eq "write-array slice writes nothing past its length"
              (seq (ffi/read-array (+ buf 6) 4)) (repeat 4 0)))

  ;; --- string forms still frame by octets -----------------------------------
  ;; write-bytes returns the UTF-8 OCTET count, not the character count, and
  ;; read-bytes decodes exactly the octets it was given.
  (let [s "he—llo"]                       ; em dash: 3 octets, 1 character
    (check-eq "write-bytes returns octets, not characters" (ffi/write-bytes buf s) 8)
    (check-eq "the source string is shorter than its encoding" (count s) 6)
    (check-eq "read-bytes decodes the octets back" (ffi/read-bytes buf 8) s))

  ;; nil is not something to encode. It used to reach the `str` coercion, which
  ;; renders it "", so an absent value wrote 0 octets and answered 0 — the same
  ;; answer as writing "" on purpose. Unlike a :string argument or string->ptr,
  ;; there is no NULL to mean it with here: the destination is the caller's own
  ;; buffer, so absence has nowhere to go and is a caller error.
  (check-eq "write-bytes rejects nil rather than writing nothing"
            (try (ffi/write-bytes buf nil) :no-throw
                 (catch IllegalArgumentException _ :rejected))
            :rejected)
  (check-eq "write-bytes still takes the str coercion for a non-nil value"
            (ffi/write-bytes buf 42) 2)

  ;; bytes that are not valid UTF-8 come back through read-array unharmed
  ;; (read-bytes would have to substitute; read-array must not)
  (let [invalid (byte-array [-128 -61 40 0 -1])]
    (ffi/write-array buf invalid)
    (check-eq "invalid UTF-8 survives the byte-array path"
              (seq (ffi/read-array buf 5)) (seq invalid)))

  (ffi/free buf))

(if (empty? @failures)
  (println "JOLT-FFI-BYTES-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "JOLT-FFI-BYTES-TEST FAILED:" (count @failures))))
