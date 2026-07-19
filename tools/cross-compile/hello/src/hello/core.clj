(ns hello.core)

(defn fib [n]
  (loop [a 0 b 1 i 0]
    (if (= i n) a (recur b (+ a b) (inc i)))))

(defn -main [& _]
  (println "hello from jolt cross-compile POC")
  (println "fib(30) =" (fib 30))
  (println "sum =" (reduce + (range 1000))))
