(ns input)

(defn wrap* [f] f)
(defmacro wrap [] `(wrap* (fn ([a#] a#) ([b#] b#))))

(def g (wrap))
