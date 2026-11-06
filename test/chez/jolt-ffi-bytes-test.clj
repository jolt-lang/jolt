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
    (check-eq "read-into! returns the octet count" (ffi/read-into! buf :byte dst 0 20 256) 256)
    (check-eq "read-into! lands at the offset"
              (seq (java.util.Arrays/copyOfRange dst 20 276)) (seq all-bytes))
    (check-eq "read-into! leaves the prefix alone"
              (seq (java.util.Arrays/copyOfRange dst 0 20)) (repeat 20 0))
    (check-eq "read-into! leaves the suffix alone"
              (seq (java.util.Arrays/copyOfRange dst 276 300)) (repeat 24 0))
    (check-eq "read-into! of 0 bytes is a no-op" (ffi/read-into! buf :byte dst 0 0 0) 0))

  ;; a stream read in chunks fills one buffer — the reason read-into! exists
  (let [dst (byte-array 256)]
    (dotimes [i 4] (ffi/read-into! buf :byte dst (* i 64) (* i 64) 64))
    (check-eq "chunked read-into! reassembles the whole block"
              (seq dst) (seq all-bytes)))

  ;; --- bounds ---------------------------------------------------------------
  (check-eq "read-into! past the end throws"
            (try (ffi/read-into! buf :byte (byte-array 8) 0 4 8) :no-throw
                 (catch Throwable _ :threw))
            :threw)
  (check-eq "read-into! at a negative offset throws"
            (try (ffi/read-into! buf :byte (byte-array 8) 0 -1 2) :no-throw
                 (catch Throwable _ :threw))
            :threw)
  (check-eq "write-array past the end throws"
            (try (ffi/write-array buf :byte all-bytes 0 200 100) :no-throw
                 (catch Throwable _ :threw))
            :threw)

  ;; --- write-array slice ----------------------------------------------------
  (let [zeros (byte-array 512)]
    (ffi/write-array buf zeros)
    (check-eq "write-array slice returns its length" (ffi/write-array buf :byte all-bytes 0 250 6) 6)
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

  ;; …but a byte-array is DATA, not a value to render. It used to reach the same
  ;; `str` coercion, which writes the characters of "#object[[B]" — 11 octets of
  ;; the array's own print form in place of its bytes, with nothing raised at any
  ;; layer. A caller reaching for write-bytes to move bytes got silent
  ;; corruption. It now takes the block move write-array makes.
  (let [bs (byte-array [104 105 0 -1 122])]
    (ffi/write-array buf (byte-array 512))   ; clear, so a short write is visible
    (check-eq "write-bytes answers a byte-array's own length, not its str form's"
              (ffi/write-bytes buf bs) 5)
    (check-eq "write-bytes copies a byte-array's octets exactly"
              (seq (ffi/read-array buf 5)) (seq bs))
    (check-eq "write-bytes writes nothing past the array"
              (seq (ffi/read-array (+ buf 5) 4)) (repeat 4 0))
    (check-eq "write-bytes of an empty byte-array is 0" (ffi/write-bytes buf (byte-array 0)) 0))

  ;; every byte value through write-bytes, so the signed/unsigned fold on this
  ;; path is held to the same bar as write-array's
  (check-eq "write-bytes round-trips all 256 byte values"
            (do (ffi/write-bytes buf all-bytes) (seq (ffi/read-array buf 256)))
            (seq all-bytes))

  ;; an array of some other width is not a byte-array and must not be
  ;; reinterpreted as one — it keeps the str coercion
  (check-eq "write-bytes does not treat an int-array as bytes"
            (> (ffi/write-bytes buf (int-array [1 2 3])) 5) true)

  ;; bytes that are not valid UTF-8 come back through read-array unharmed
  ;; (read-bytes would have to substitute; read-array must not)
  (let [invalid (byte-array [-128 -61 40 0 -1])]
    (ffi/write-array buf invalid)
    (check-eq "invalid UTF-8 survives the byte-array path"
              (seq (ffi/read-array buf 5)) (seq invalid)))

  (ffi/free buf))

;; --- string->ptr of a byte-array ---------------------------------------------
;; string->ptr is the allocate-and-copy half of write-bytes, so it answers a
;; byte-array the same way: its octets, exactly, then the NUL. It used to render
;; the array with `str` and allocate "#object[[B]" + NUL, the arena's size
;; bookkeeping agreeing with the wrong copy, and with-c-string and
;; with-c-string-array inherited it.
(let [bs (byte-array [104 105 0 -1 122])]
  (ffi/with-arena [a]
    (let [p (ffi/string->ptr a bs)]
      (check-eq "string->ptr copies a byte-array's octets and a NUL"
                (vec (ffi/read-array p 6)) [104 105 0 -1 122 0])
      (check-eq "the arena records the octets plus the NUL" (ffi/size p) 6)))
  (let [p (ffi/string->ptr bs)]
    (check-eq "caller-owned string->ptr copies the same octets"
              (vec (ffi/read-array p 6)) [104 105 0 -1 122 0])
    (ffi/free p))
  (check-eq "with-c-string of a byte-array binds its octets"
            (ffi/with-c-string [p (byte-array [104 105])] (ffi/ptr->string p)) "hi")
  (check-eq "with-c-string-array of a byte-array member binds its octets"
            (ffi/with-c-string-array [arr 1] [(byte-array [104 105])]
              (ffi/ptr->string (ffi/read arr :pointer 0)))
            "hi")
  (ffi/with-arena [a]
    (let [p (ffi/string->ptr a (byte-array 0))]
      (check-eq "an empty byte-array is one NUL, not NULL"
                [(ffi/null? p) (ffi/size p) (ffi/ptr->string p)] [false 1 ""])))
  (ffi/with-arena [a]
    (let [p (ffi/string->ptr a all-bytes)]
      (check-eq "string->ptr round-trips all 256 byte values"
                (seq (ffi/read-array p 256)) (seq all-bytes))
      (check-eq "and terminates after them" (ffi/read p :int8 256) 0))))

;; --- a java.nio.ByteBuffer over foreign memory -------------------------------
;; ffi/byte-buffer is a DIRECT buffer: it shares the bytes with the pointer
;; rather than copying them, which is the whole difference from read-array. So
;; the property under test is that writes cross in BOTH directions, and that the
;; buffer reports itself as direct the way the JVM's does.
(let [buf (ffi/alloc 64)]
  (ffi/write-array buf (byte-array (range 8)))
  (let [bb (ffi/byte-buffer buf 64)]
    (check-eq "capacity is the declared length" (.capacity bb) 64)
    (check-eq "a direct buffer has no backing array" (.hasArray bb) false)
    (check-eq "array raises on a direct buffer"
              (try (.array bb) :no-throw (catch UnsupportedOperationException _ :rejected))
              :rejected)
    (check-eq "relative get walks the foreign bytes"
              [(.get bb) (.get bb) (.get bb)] [0 1 2])
    (check-eq "absolute get leaves position alone"
              [(.get bb 7) (.position bb)] [7 3])
    ;; big-endian, java.nio's default and every other jolt ByteBuffer accessor
    (check-eq "getInt reads big-endian" (.getInt bb 4) 0x04050607)

    ;; the buffer writes THROUGH to the pointer
    (.putInt bb 0 0x12345678)
    (check-eq "a put is visible to the pointer"
              (mapv #(bit-and % 0xff) (ffi/read-array buf 4)) [0x12 0x34 0x56 0x78])
    ;; and a write to the pointer is visible to the buffer
    (ffi/write buf :uint8 0xab 0)
    (check-eq "a write through the pointer is visible to the buffer"
              (bit-and (.get bb 0) 0xff) 0xab)

    ;; bulk moves, which take the block-move path rather than a byte at a time
    (ffi/write-array buf (byte-array (range 8)))
    (.position bb 0)
    (let [dst (byte-array 8)]
      (.get bb dst)
      (check-eq "bulk get fills a byte-array" (seq dst) (seq (byte-array (range 8)))))
    (.position bb 0)
    (.put bb (byte-array [-1 -128 0 127]))
    (check-eq "bulk put is binary-faithful through the fold"
              (seq (ffi/read-array buf 4)) (seq (byte-array [-1 -128 0 127])))

    ;; a direct slice SHARES, where a heap slice copies
    (.position bb 4)
    (let [sl (.slice bb)]
      (check-eq "a direct slice is 0-based over the remainder" (.capacity sl) 60)
      (.put sl (byte-array [99]))
      (check-eq "a direct slice writes through to the same bytes"
                (nth (ffi/read-array buf 8) 4) 99)))
  ;; the length may be a type or a layout, as it may for slice and reinterpret
  (check-eq "a type keyword sizes the view" (.capacity (ffi/byte-buffer buf :int64)) 8)
  (ffi/free buf))

;; A heap buffer keeps every one of its old answers — the direct backing is an
;; addition, not a change to ByteBuffer/wrap and ByteBuffer/allocate.
(let [hb (java.nio.ByteBuffer/wrap (byte-array [1 2 3 4]))]
  (check-eq "a wrapped buffer still has its array" (.hasArray hb) true)
  (check-eq "and hands it back" (seq (.array hb)) (seq (byte-array [1 2 3 4])))
  (check-eq "and still reads big-endian" (.getInt hb 0) 0x01020304))
(let [hb (java.nio.ByteBuffer/allocate 8)]
  (.putInt hb 42)
  (.flip hb)
  (check-eq "an allocated buffer round-trips" [(.getInt hb 0) (.remaining hb)] [42 4]))

(if (empty? @failures)
  (println "JOLT-FFI-BYTES-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "JOLT-FFI-BYTES-TEST FAILED:" (count @failures))))
