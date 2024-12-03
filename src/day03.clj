(ns day03)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[common])

(defn mul-sum [line]
  (let [matches (re-seq #"mul\((\d+),(\d+)\)" line)
        products (map (fn [[m x y]]
                        (* (Integer/parseInt x) (Integer/parseInt y))) matches)]
    (reduce + products)))
  
(defn conditional-mul-sum [data]
  (let [matches (re-seq #"mul\((\d+),(\d+)\)|don't\(\)|do\(\)" data)
        sum-accept (reduce (fn [[acc accept] [m x y]]
                             (cond 
                               (= m "don't()") [acc false]
                               (= m "do()") [acc true]
                               (str/starts-with? m "mul") (if accept
                                                            [(+ acc (* (Integer/parseInt x) (Integer/parseInt y))) accept]
                                                            [acc accept])
                               :else (do 
                                       (pr "unmatched = " m)
                                       (println)
                                       [acc accept])))
                           [0 true] 
                           matches)]
    (first sum-accept)))

(defn part1 [file]
  (let [lines (common/get-lines file)
        sums (map mul-sum lines)]
    (reduce + sums)))

(defn part2 [file]
  ;; note that we dont want to process this per line, but on the entire input.
  ;; since the don't / do clauses carry over between lines. sigh.
  (let [data (common/get-data file)
        sum (conditional-mul-sum data)]
    sum))

(defn run [opts]
  (println opts)
  (println "Part 1: " (part1 (:data opts)))
  (println "Part 2: " (part2 (:data opts))))
