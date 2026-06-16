# jolt.png: the host PNG encoder (src/jolt/png.janet) + the overlay wrapper
# (src/jolt/jolt/png.clj), reachable as janet.png/* through eval_base's
# module-load-env. Checks the encoder emits a structurally valid PNG (signature,
# IHDR dims, IEND) and that the overlay path writes the same.
(import ../../src/jolt/png :as png)
(import ../../src/jolt/api :as api)

(print "jolt.png encoder + overlay...")
(var failures 0)
(defn check [label ok] (if ok (print "  ok: " label) (do (++ failures) (eprintf "  FAIL: %s" label))))

(defn- u32 [b o]
  (+ (* (get b o) 16777216) (* (get b (+ o 1)) 65536) (* (get b (+ o 2)) 256) (get b (+ o 3))))

# --- host encoder ---
(def w 5)
(def h 3)
(def rgb (buffer/new (* w h 3)))
(for i 0 (* w h 3) (buffer/push-byte rgb (% (* i 7) 256)))
(def out (png/encode w h rgb))
(check "PNG signature" (= (string/slice out 0 8) "\x89PNG\r\n\x1a\n"))
(check "IHDR length is 13" (= 13 (u32 out 8)))
(check "IHDR type" (= "IHDR" (string/slice out 12 16)))
(check "IHDR width" (= w (u32 out 16)))
(check "IHDR height" (= h (u32 out 20)))
(check "8-bit RGB colour type" (and (= 8 (get out 24)) (= 2 (get out 25))))
(check "ends with an IEND chunk" (string/find "IEND" (string out)))
(check "encode rejects a wrong-sized buffer"
       (= false (first (protect (png/encode w h (buffer/new 4))))))

# --- overlay (jolt.png from Clojure) writes a valid PNG ---
(def tmp (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-png-overlay-test.png"))
(def ctx (api/init-cached {:compile? true}))
(api/eval-string ctx "(require '[jolt.png :as png])")
(api/eval-string ctx
  (string "(let [img (png/image 4 2)]"
          "  (doseq [_ (range 8)] (png/put! img 10 20 30))"
          "  (png/write img 4 2 \"" tmp "\"))"))
(def wrote (slurp tmp))
(check "overlay wrote the PNG signature" (= (string/slice wrote 0 8) "\x89PNG\r\n\x1a\n"))
(check "overlay IHDR dims" (and (= 4 (u32 wrote 16)) (= 2 (u32 wrote 20))))
(os/rm tmp)

(if (= 0 failures)
  (print "All tests passed.")
  (do (eprintf "%d png check(s) failed" failures) (os/exit 1)))
