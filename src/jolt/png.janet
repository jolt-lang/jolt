# Minimal PNG encoder (the host side of `jolt.png`). 8-bit truecolour RGB
# (colour type 2), no interlace, filter 0 (None), and STORED (uncompressed)
# DEFLATE blocks — a valid zlib stream that needs no compressor. Files are larger
# than a compressed PNG but decode everywhere.
#
# Lives in Janet, not Clojure: jolt's per-op cost makes per-byte work (CRC32 over
# every byte) impractical in the overlay, so the whole encode runs here natively
# and jolt calls `write` once with a raw-RGB buffer. Exposed to jolt as
# `janet.png/...` via eval_base's module-load-env (see eval_base.janet).
#
# Janet's bit ops are 32-bit SIGNED (0xFFFFFFFF overflows), so the 32-bit
# unsigned arithmetic of CRC32/Adler32 is done with plain number ops (doubles
# hold 2^32 exactly) plus byte-level bxor, whose operands are always 0..255.

(defn- u8 [v] (% v 256))               # low byte
(defn- shr [v n] (math/floor (/ v n))) # logical shift-right by a power of two

# 32-bit xor via 4 byte-wise xors (each operand 0..255, safely in 32-bit range).
(defn- xor32 [a b]
  (var r 0)
  (var m 1)
  (for i 0 4
    (set r (+ r (* (bxor (% (shr a m) 256) (% (shr b m) 256)) m)))
    (set m (* m 256)))
  r)

(def- crc-table
  (let [t (array/new-filled 256 0)]
    (for n 0 256
      (var c n)
      (for _ 0 8
        (set c (if (= 1 (% c 2)) (xor32 0xEDB88320 (shr c 2)) (shr c 2))))
      (put t n c))
    t))

(defn- crc32 [buf]
  (var c 0xFFFFFFFF)
  (def n (length buf))
  (for i 0 n
    (set c (xor32 (get crc-table (bxor (% c 256) (get buf i))) (shr c 256))))
  (- 0xFFFFFFFF c))                    # final xor with all-ones = complement

(defn- adler32 [buf]
  (var a 1)
  (var b 0)
  (def n (length buf))
  (for i 0 n
    (set a (% (+ a (get buf i)) 65521))
    (set b (% (+ b a) 65521)))
  (+ (* b 65536) a))

(defn- push-u32be [out v]
  (buffer/push-byte out (% (shr v 16777216) 256) (% (shr v 65536) 256)
                    (% (shr v 256) 256) (% v 256)))

(defn- push-chunk [out typ data]
  (push-u32be out (length data))
  (def tagged (buffer typ data))       # CRC covers type + data
  (buffer/push-string out tagged)
  (push-u32be out (crc32 tagged)))

# zlib stream of `raw` as stored DEFLATE blocks (<=65535 bytes each).
(defn- zlib-store [raw]
  (def out (buffer/new (+ (length raw) 64)))
  (buffer/push-byte out 0x78 0x01)     # zlib header: CM=8 CINFO=7, FCHECK ok
  (def n (length raw))
  (var pos 0)
  (while (< pos n)
    (def len (min 65535 (- n pos)))
    (def final (if (>= (+ pos len) n) 1 0))
    (def nlen (- 65535 len))           # one's-complement of len in 16 bits
    (buffer/push-byte out final (% len 256) (shr len 256) (% nlen 256) (shr nlen 256))
    (buffer/push-string out (buffer/slice raw pos (+ pos len)))
    (set pos (+ pos len)))
  (push-u32be out (adler32 raw))
  out)

(defn encode
  "Encode a w*h*3 raw-RGB buffer (row-major, top row first) as a PNG buffer."
  [w h rgb]
  (assert (= (length rgb) (* w h 3)) "png/encode: rgb length != w*h*3")
  (def out (buffer/new (+ 64 (* w h 3))))
  (buffer/push-string out "\x89PNG\r\n\x1a\n")
  (def ihdr (buffer/new 13))
  (push-u32be ihdr w)
  (push-u32be ihdr h)
  (buffer/push-byte ihdr 8 2 0 0 0)    # bit depth 8, colour type 2 (RGB), …
  (push-chunk out "IHDR" ihdr)
  # scanlines, each prefixed with filter byte 0 (None)
  (def stride (* w 3))
  (def raw (buffer/new (* h (+ 1 stride))))
  (for y 0 h
    (buffer/push-byte raw 0)
    (def off (* y stride))
    (buffer/push-string raw (buffer/slice rgb off (+ off stride))))
  (push-chunk out "IDAT" (zlib-store raw))
  (push-chunk out "IEND" (buffer/new 0))
  out)

(defn write
  "Encode and write a w*h*3 raw-RGB buffer to `path` as a PNG."
  [path w h rgb]
  (spit path (encode w h rgb))
  path)
