(ns day01)

(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(defn run [opts]
  (println opts)
  (let [data (slurp (io/as-relative-path (str "resources/" (:data opts))))
        nums (map Integer/parseInt (str/split data #"\s+"))
        left (sort (map first (partition 2 nums)))
        right (sort (map second (partition 2 nums)))
        counts (frequencies right)
        diffs (map #(abs (- %1 %2)) left right)
        part1 (reduce + diffs)
        part2 (reduce + (map #(* % (get counts % 0)) left))]
    (println "part 1" part1)
    (println "part 2" part2)
    ))
