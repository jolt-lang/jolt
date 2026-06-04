;; ============================================================
;; Comprehensive Clojure Features Demo
;; ============================================================

;; 1. Destructuring (sequential & associative)
(defn destructure-demo []
  (println "\n--- Destructuring ---")

  ;; Sequential destructuring
  (let [[a b c] [10 20 30]]
    (println (str "Seq destructure: a=" a ", b=" b ", c=" c)))

  ;; Associative destructuring with defaults
  (let [{:keys [name age city] :or {city "Unknown"}} {:name "Alice" :age 30}]
    (println (str "Map destructure: name=" name ", age=" age ", city=" city)))

  ;; Nested destructuring
  (let [{[x y] :coords} {:coords [1.0 2.5]}]
    (println (str "Nested destructure: x=" x ", y=" y))))

;; 2. Atoms – state management
(defn atom-demo []
  (println "\n--- Atoms ---")
  (def counter (atom 0))

  ;; swap! (function-based update)
  (swap! counter inc)
  (println (str "After swap! inc: " @counter))

  ;; reset! (set new value)
  (reset! counter 100)
  (println (str "After reset! to 100: " @counter))

  ;; compare-and-set! (CAS)
  (let [old @counter]
    (if (compare-and-set! counter old (+ old 5))
      (println (str "CAS success: " @counter))
      (println "CAS failed")))

  ;; Using atom with swap! and multiple updates
  (swap! counter #(-> % (* 2) (+ 3)))
  (println (str "After thread-first swap!: " @counter)))

;; 3. Lazy sequences – infinite & transformed
(defn lazy-seq-demo []
  (println "\n--- Lazy Sequences ---")

  ;; Infinite lazy seq: natural numbers
  (def naturals (iterate inc 0))

  ;; Take first 10 even numbers using filter (lazy)
  (def first-ten-evens (take 10 (filter even? naturals)))
  (println (str "First 10 evens: " (pr-str first-ten-evens)))

  ;; Map and take-while (lazy)
  (def squares-under-50
    (take-while #(< % 50) (map #(* % %) (range))))
  (println (str "Squares under 50: " (pr-str squares-under-50)))

  ;; Cycle and interpose (lazy)
  (def repeated-pattern (take 10 (cycle [:a :b :c])))
  (println (str "Cycled pattern: " (pr-str repeated-pattern)))

  ;; Lazy seq from recursion (not fully lazy, but demonstrates lazy cons)
  (defn my-iterate [f x]
    (lazy-seq (cons x (my-iterate f (f x)))))
  (def powers-of-two (take 8 (my-iterate #(* 2 %) 1)))
  (println (str "Powers of two: " (pr-str powers-of-two))))

;; 4. Transducers – composable transformations
(defn transducer-demo []
  (println "\n--- Transducers ---")

  ;; Compose mapping and filtering as a transducer
  (def xf (comp (map inc) (filter odd?)))

  ;; Apply to a collection (into)
  (def result (into [] xf (range 10)))
  (println (str "Transducer result: " (pr-str result)))

  ;; Use with sequence (sequence)
  (def seq-result (sequence xf (range 10)))
  (println (str "Transducer seq: " (pr-str seq-result))))

;; 5. Protocols & Records – polymorphism
(defprotocol Shape
  (area [this])
  (description [this]))

(defrecord Circle [radius]
  Shape
  (area [_] (* Math/PI radius radius))
  (description [_] (str "Circle with radius " radius)))

(defrecord Rectangle [width height]
  Shape
  (area [_] (* width height))
  (description [_] (str "Rectangle " width "x" height)))

(defn protocol-demo []
  (println "\n--- Protocols & Records ---")
  (def c (->Circle 5))
  (def r (->Rectangle 3 4))
  (println (str (description c) " -> area: " (area c)))
  (println (str (description r) " -> area: " (area r))))

;; 6. Multimethods – dispatch on arbitrary values
(defmulti shape-type :kind)
(defmethod shape-type :circle [_] "round")
(defmethod shape-type :rectangle [_] "angular")
(defmethod shape-type :default [_] "unknown")

(defn multimethod-demo []
  (println "\n--- Multimethods ---")
  (def s1 {:kind :circle :radius 5})
  (def s2 {:kind :rectangle :width 3 :height 4})
  (def s3 {:kind :triangle})
  (println (str "Circle type: " (shape-type s1)))
  (println (str "Rectangle type: " (shape-type s2)))
  (println (str "Triangle type: " (shape-type s3))))

;; 7. Macros – compile-time code generation
(defmacro log-call [expr]
  `(let [result# ~expr]
     (println (str "Called: " (pr-str '~expr) " -> " result#))
     result#))

(defn macro-demo []
  (println "\n--- Macros ---")
  (log-call (* 2 3))
  (log-call (map inc [1 2 3]))
  (log-call (reduce + (range 1 6))))

;; 8. Recursion – linear and tail-recursive
(defn recursion-demo []
  (println "\n--- Recursion ---")
  ;; Linear recursion: factorial
  (defn fact [n]
    (if (<= n 1) 1 (* n (fact (dec n)))))
  (println (str "Factorial 5: " (fact 5)))

  ;; Tail recursion with recur
  (defn fact-tail [n]
    (loop [i n acc 1]
      (if (zero? i) acc
          (recur (dec i) (* acc i)))))
  (println (str "Tail-factorial 5: " (fact-tail 5)))

  ;; Mutual recursion with trampoline
  (declare even?)
  (defn odd? [n]
    (if (zero? n) false (even? (dec n))))
  (defn even? [n]
    (if (zero? n) true (odd? (dec n))))
  (println (str "Is 6 even? " (even? 6))))

;; 9. Higher-order functions – partial, comp, juxt
(defn hof-demo []
  (println "\n--- Higher-Order Functions ---")
  (def add5 (partial + 5))
  (println (str "Partial (+5) applied to 10: " (add5 10)))

  (def inc-and-double (comp #(* 2 %) inc))
  (println (str "Comp (double∘inc) on 3: " (inc-and-double 3)))

  (def stats (juxt identity inc dec))
  (println (str "Juxt on 5: " (stats 5))))

;; 10. Threading macros (-> and ->>)
(defn threading-demo []
  (println "\n--- Threading Macros ---")
  (def result
    (->> (range 20)
         (filter odd?)
         (map #(* % 3))
         (take 5)
         (reduce +)))
  (println (str "Threaded pipeline result: " result))

  (def threaded-sqrt
    (-> 25 Math/sqrt long (+ 10)))
  (println (str "Thread-first sqrt: " threaded-sqrt)))

;; 11. Exception handling with try/catch/finally
(defn exception-demo []
  (println "\n--- Exception Handling ---")
  (try
    (throw (ex-info "something broke" {:code 42}))
    (catch :default e
      (println (str "Caught: " (ex-message e) " " (pr-str (ex-data e)))))
    (finally
      (println "Finally block executed."))))

;; 12. Clojure's sequence comprehension: for (list comprehension)
(defn for-demo []
  (println "\n--- For Comprehension ---")
  (def combos
    (for [x (range 3)
          y (range 3)
          :when (not= x y)]
      [x y]))
  (println (str "Combinations (x!=y): " (pr-str combos))))

;; 13. Clojure's core.async? Not pure Clojure, skip.

;; 14. Janet host interop — Jolt runs on Janet, so host interop is Janet-style:
;;     `.` dispatches on a table/struct (the receiver is passed as the first
;;     arg), and Janet's standard library is reachable through jolt.interop.
(defn janet-interop-demo []
  (println "\n--- Janet Interop ---")
  ;; Method-style call on a Janet table: self is passed as the first argument.
  (def counter {:value 41
                :describe (fn [self] (str "counter = " (:value self)))})
  (println (str "Method call:  " (. counter describe)))
  ;; Field access sugar: (.-k m) is (. m :k)
  (println (str "Field access: " (.-value counter)))
  ;; Reach Janet's standard library through jolt.interop.
  (require '[jolt.interop :as j])
  (println (str "Janet type:   " (j/janet-type [1 2 3])))
  (println (str "Table keys:   " (pr-str (j/janet-table-keys {:a 1 :b 2})))))

;; ---------- Main entry point ----------
(defn -main []
  (println "=== Clojure Features Demo ===")
  (destructure-demo)
  (atom-demo)
  (lazy-seq-demo)
  (transducer-demo)
  (protocol-demo)
  (multimethod-demo)
  (macro-demo)
  (recursion-demo)
  (hof-demo)
  (threading-demo)
  (exception-demo)
  (for-demo)
  (janet-interop-demo)
  (println "\n=== Demo Complete ==="))

;; Run if executed as script
(-main)
