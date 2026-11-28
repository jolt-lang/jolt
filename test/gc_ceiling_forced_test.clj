;; The heap ceiling's forced full collection must not carry the heap past it
;; (jolt-exoj). Run by `make gcpolicy` under JOLT_MAX_HEAP=196m: the ceiling leaves out the boot image, and 196MB is the room 256m left beside the 60MB image these cases were sized for.
;;
;; The same work as gc_policy_test.clj's ceiling mode, as top-level forms: that
;; shape lands the ceiling's first full collection with ~140MB of data spread
;; over generations 1-3 and the total already at the ceiling, every run. The
;; collection is tight (old generations marked in place), but Chez copies a
;; segment anyway when its chunk is under a quarter used or it was marked
;; before and is now under three quarters live, and here that was nearly all of
;; it: one collection of every generation held ~120MB of copies beside their
;; sources and peaked at 322MB. The fn-shaped ceiling mode meets the same
;; collection only under CPU contention, which is how it showed up: as a flake.
;;
;; The gate runs this from an empty directory. From the repo root the project's
;; deps.edn is read, which shifts the baseline heap enough that the first forced
;; collection comes at a kinder moment (269MB before the fix).
;;
;; An optional argument retains that many KB of small objects first, standing in
;; for a larger boot live set. The gate runs several: which collection lands at
;; the ceiling moves with the baseline, and a peak that holds for one baseline
;; only is luck. 0.44MB more at boot took the peak from 279MB to 282MB, over the
;; gate on macOS: under the ceiling the young collection still copied generation
;; 1 on Chez's schedule (every fourth), 24MB beside a 251MB total
;; (sa-collect-young!).
(ns gc-ceiling-forced-test)

(def pad-kb (if-let [a (first *command-line-args*)] (parse-long a) 0))
(def pad (vec (map (fn [i] (str "pad-" i)) (range (* pad-kb 16)))))
(def held (atom nil))
(reset! held (vec (map (fn [i] {:i i :s (str "held-" i)}) (range 600000))))
(jolt.host/reset-maximum-memory-bytes!)
(loop [i 0 window []]
  (when (< i 40)
    (recur (inc i) (mapv (fn [j] {:i j :s (str j "-" i)}) (range 200000)))))
(println "peak" (- (jolt.host/maximum-memory-bytes) (jolt.host/static-memory-bytes)) "max" (.maxMemory (Runtime/getRuntime)) (count pad))
