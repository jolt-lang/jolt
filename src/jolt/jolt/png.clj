; Jolt Standard Library: jolt.png
;
; Write PNG images from Clojure. Build an image, push RGB pixels in row-major
; order (top row first), then write to disk:
;
;   (require '[jolt.png :as png])
;   (let [img (png/image w h)]
;     (doseq [y (range h)]
;       (doseq [x (range w)]
;         (png/put! img r g b)))      ; r g b are ints 0-255
;     (png/write img w h "out.png"))
;
; The byte-level encoding (filtering, stored-DEFLATE/zlib, CRC32) runs in the
; host (the Janet `png` module, reached via the `janet.*` bridge): per-byte work
; in the overlay is far too slow, so the overlay only produces pixels and the
; host encodes them in one pass.
(ns jolt.png)

(defn image
  "A blank w×h RGB pixel sink (a host byte buffer). Push exactly w*h pixels with
  put!, in row-major / top-row-first order, then write."
  [w h]
  (janet.buffer/new (* w h 3)))

(defn put!
  "Append one RGB pixel — each of r g b an int in 0-255 — to the image. Returns
  the image so calls can be threaded."
  [img r g b]
  (janet.buffer/push-byte img r g b)
  img)

(defn write
  "Encode the filled w×h image as a PNG and write it to path. Returns path."
  [img w h path]
  (janet.png/write path w h img))
