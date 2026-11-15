;; A :string argument must not move while C holds it, even when C calls back into
;; jolt and the callback collects. bsearch hands its key to the comparator on
;; every step, so the comparator can read back the pointer C holds.
;; Run: bin/jolt run test/chez/jolt-ffi-string-arg-test.clj, or `make ffi`.
(ns jolt-ffi-string-arg-test)

(require '[jolt.ffi :as ffi])

(def failures (atom []))

(defmacro check-eq [label got want]
  `(let [g# ~got w# ~want]
     (when-not (= g# w#)
       (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#))))))

(ffi/defcfn bsearch-string "bsearch" [:string :pointer :size_t :size_t :pointer] :pointer)
(ffi/defcfn bsearch-pointer "bsearch" [:pointer :pointer :size_t :size_t :pointer] :pointer)

(def text (apply str (repeat 200 "the quick brown fox ")))
(def garbage (atom nil))
(def seen (atom []))

;; Collect and allocate, then read the key through the pointer C passed. The
;; comparator always answers 1, so bsearch walks every step.
(def reading-comparator
  (ffi/foreign-callable
   (fn [key _element]
     (System/gc)
     (reset! garbage (vec (repeatedly 2000 #(apply str (repeat 400 \x)))))
     (swap! seen conj (ffi/ptr->string key))
     1)
   [:pointer :pointer] :int))

(def table (ffi/alloc 64))

(defn- run [call]
  (reset! seen [])
  (dotimes [_ 20] (call))
  @seen)

(let [got (run #(bsearch-string text table 8 8 reading-comparator))]
  (check-eq "the comparator ran" (pos? (count got)) true)
  (check-eq "a :string key reads the same text after a collection in the callback"
            (count (remove #(= text %) got)) 0))

;; The control: the same call with the key in foreign memory the caller owns.
(let [got (run #(ffi/with-c-string [p text] (bsearch-pointer p table 8 8 reading-comparator)))]
  (check-eq "a with-c-string key reads the same text (control)"
            (count (remove #(= text %) got)) 0))

;; Non-ASCII text crosses as its UTF-8 octets, NUL-terminated.
(let [s (apply str (repeat 50 "żółw "))
      got (do (reset! seen [])
              (dotimes [_ 5] (bsearch-string s table 8 8 reading-comparator))
              @seen)]
  (check-eq "a non-ASCII :string key survives the same callback" (count (remove #(= s %) got)) 0))

(ffi/free table)

(if (empty? @failures)
  (do (println "JOLT-FFI-STRING-ARG-TEST OK") (flush) (System/exit 0))
  (do (doseq [x @failures] (println "FAIL:" x))
      (println "JOLT-FFI-STRING-ARG-TEST FAILED:" (count @failures))
      (flush)
      (System/exit 1)))
