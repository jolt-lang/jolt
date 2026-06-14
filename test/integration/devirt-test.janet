# Protocol-dispatch devirtualization (jolt-41m): when the inference proves a
# protocol call's receiver is a known record type, the call is compiled to a
# DIRECT method call, skipping the runtime dispatch registry. This must stay
# SOUND — same results as the dispatched path — including polymorphic dispatch
# (the right method per type), fallback when the receiver type is unknown, and
# heterogeneous collections. Runs a protocol+record program through the built
# binary (devirt needs infer-unit!, which runs on ns load, not eval-string) and
# checks the output. Skips cleanly if build/jolt is absent.
(def jolt "build/jolt")

(defn- run [whole?]
  (def dir (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-dv-test"))
  (os/mkdir dir)
  (spit (string dir "/dv.clj")
        (string
         "(ns dv)\n"
         "(defprotocol Shape (area [s]) (kind [s]))\n"
         "(defrecord Rect [w h] Shape (area [r] (* (:w r) (:h r))) (kind [_] :rect))\n"
         "(defrecord Circ [r] Shape (area [c] (* 3 (:r c) (:r c))) (kind [_] :circ))\n"
         "(defn poly [s] (area s))\n"  # receiver unknown -> must fall back
         "(defn -main []\n"
         "  (println (area (->Rect 3 4)) (area (->Circ 5))\n"            # devirt: 12 75
         "           (kind (->Rect 1 1)) (kind (->Circ 1))\n"           # devirt: :rect :circ
         "           (poly (->Rect 3 4)) (poly (->Circ 5))\n"           # fallback: 12 75
         "           (mapv area [(->Rect 2 3) (->Circ 2)])))\n"))       # heterogeneous: [6 12]
  (def out (string dir "/out.txt"))
  (def jbin (string (os/cwd) "/" jolt))
  # -m auto-enables whole-program under direct-linking now, so the per-ns case
  # (whole? false) must explicitly opt out to test the dispatched/per-ns path.
  (def cmd (string (if whole? "JOLT_WHOLE_PROGRAM=1 " "JOLT_NO_WHOLE_PROGRAM=1 ")
                   "JOLT_DIRECT_LINK=1 JOLT_PATH=" dir " " jbin " -m dv > " out " 2>&1"))
  (os/execute ["sh" "-c" cmd] :p)
  (string/trimr (slurp out)))

(def expected "12 75 :rect :circ 12 75 [6 12]")
(if (not (os/stat jolt))
  (print "devirt: SKIP (no build/jolt — run from source)")
  (let [per-ns (run false) whole (run true)]
    (printf "  per-ns:        %s" per-ns)
    (printf "  whole-program: %s" whole)
    (if (and (= per-ns expected) (= whole expected))
      (print "devirt: correct (dispatched == devirtualized)")
      (do (printf "devirt: WRONG — expected %q" expected) (os/exit 1)))))
