(ns day11)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

(def small-input [125 17])
(def input [41078 18 7 0 4785508 535256 8154 447])

(defn split-num 
  [n]
  (let [strn (str n)
        len (count strn)
        half (/ len 2)]
    (cond 
      (= n 0) [1]
      (even? len) [(parse-long (subs strn 0 half)) 
                   (parse-long (subs strn half))]
      :else [(* n 2024)])))

;; dfs
(def memoized-count-nums 
  (memoize 
    (fn count-nums [n max-iters]
      (let [next-nums (split-num n)]
        (if (= max-iters 0)
          1
          (reduce + (mapv #(memoized-count-nums % (dec max-iters)) next-nums)))))))

(defn run [opts]
  (println "part 1: " (reduce + (map #(memoized-count-nums % 25) input)))
  (println "part 2: " (reduce + (map #(memoized-count-nums % 75) input))))
