(ns day13)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

(defn determinant 
  [m]
  (let [[a c] (first m) ; column 1
        [b d] (second m)] ; column 2
    (- (* a d) (* b c))))

(defn adjugate
  [m]
  (let [[a c] (first m) ; column 1
        [b d] (second m)] ; column 2
    [[d (* -1 c)]
     [(* -1 b) a]]))

(defn dot
  [a b]
  (reduce + (map #(* %1 %2) a b)))

(defn vecmul
  [a v]
  (mapv #(* a %) v))

(defn vecadd
  [a b]
  (mapv #(+ %1 %2) a b))

(defn solve-eqn
  [m rhs]
  (let [det (determinant m)
        adj (adjugate m)]
    (if (not= det 0)
      (let [a (dot rhs (vecmul (/ 1 det) 
                               [(get-in adj [0 0])
                                (get-in adj [1 0])]))
            b (dot rhs (vecmul (/ 1 det) 
                               [(get-in adj [0 1])
                                (get-in adj [1 1])]))]
        (if (or (ratio? a) (ratio? b))
          nil
          [a b]))
      (println "not solvable"))))

(defn parse-eqn
  [lines offset]
  (if (<= (count lines) 1)
    nil
    (let [regex #"X.(\d+), Y.(\d+)"
          vecs (mapv (fn [line]
                       (let [matches (re-find regex line)]
                         [(parse-long (second matches))
                          (parse-long (nth matches 2))]))
                     lines)]
      [[(first vecs) (second vecs)] (vecadd offset (last vecs))])))

(def prize-offset 10000000000000)

(defn count-tokens
  [[m rhs]]
  (let [result (solve-eqn m rhs)]
    (if (nil? result)
      nil
      (+ (* (first result) 3) (second result)))))

(defn run [opts]
  (println opts)
  (let [lines (common/get-lines (:data opts))
        eqnlines (partition-by #(= "" %) lines)
        eqns (filter some? (mapv #(parse-eqn % [0 0]) eqnlines))
        eqns-part2 (filter some? (mapv #(parse-eqn % [prize-offset prize-offset]) eqnlines))
        tokens (map count-tokens eqns)
        tokens2 (map count-tokens eqns-part2)]
    (println "part 1: " (reduce + (filter some? tokens)))
    (println tokens2)
    (println "part 2: " (reduce + (filter some? tokens2)))
    ))

