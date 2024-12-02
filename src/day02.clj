(ns day02)

(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(defn parse [file]
  (let [data (slurp (io/as-relative-path (str "resources/" file)))
        lines (mapv #(str/split % #"\s+") (str/split-lines data))
        nums (mapv #(mapv Integer/parseInt %) lines)]
    nums))

(defn map-with-removal [f lst]
  (map (fn [i]
         (let [updated-list (concat (take i lst) (drop (inc i) lst))]
           (f updated-list)))
       (range (count lst))))

(defn validate [line]
  (let [diffs (mapv - line (rest line))]
    (and 
      (every? #(and 
                 (< (abs %) 4)
                 (> (abs %) 0)) diffs)
      (or 
        (every? #(< % 0) diffs)
        (every? #(> % 0) diffs)))))

(defn validate-2 [line] 
  (let [v (map-with-removal validate line)]
    (some true? v)))

(defn run [opts]
  (println opts)
  (let [rows (parse (:data opts))
        part1 (count (filter true? (map validate rows)))
        part2 (count (filter true? (map validate-2 rows)))]
    (println "part 1: " part1)
    (println "part 2: " part2)
    ))
