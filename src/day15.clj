(ns day15)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

(defn find-robot
  [grid]
  (flatten 
    (filter #(not (empty? %)) 
          (map-indexed 
            (fn [rowidx row]
              (filter some? 
                      (map-indexed (fn [colidx itm]
                                     (if (= itm \@) [rowidx colidx])) row))) 
                       grid))))

(def move-map { \^ [-1 0] \< [0 -1] \> [0 1] \v [1 0] })

(defn get-next-empty-pos
  "find the next empty position given current location and direction. If none
  exists, return nil"
  [grid pos dir]
  (loop [curr-pos pos]
    (let [next-pos (common/vecadd curr-pos dir)
          item (get-in grid next-pos)]
      (cond (= item \.) next-pos
            (= item \#) nil
            :else (recur next-pos)))))

(defn get-next-empty-pos-2
  [grid pos dir]
  ;; moving up/down is recursive
  ;(println "pos = " pos " item = " (get-in grid pos))
  (let [next-pos (common/vecadd pos dir)
        item (get-in grid next-pos)
        left (common/vecadd next-pos [0 -1])
        right (common/vecadd next-pos [0 1])]
    ;(println item " at " next-pos ", left: " left  " , right: " right)
    (cond 
      (= item \[)
      (merge 
        { pos next-pos }
        (get-next-empty-pos-2 grid next-pos dir)
        (if (not= pos right) (get-next-empty-pos-2 grid right dir)))

      (= item \])
      (merge 
        { pos next-pos }
        (get-next-empty-pos-2 grid next-pos dir)
        (if (not= pos left) (get-next-empty-pos-2 grid left dir)))

      (= item \.)
      { pos next-pos }

      (= item \#)
      { pos nil }

      :else
      { nil nil } 
      )))

(defn make-move 
  "return new grid after making the move"
  [grid pos dir]
  (let [next-pos (common/vecadd pos dir)
        next-empty-pos (get-next-empty-pos grid pos dir)]
    (if (some? next-empty-pos)
      [(-> grid 
           (assoc-in next-empty-pos \O)
           (assoc-in next-pos \.))
       next-pos]
      [grid pos])))

(defn make-move-2
  "return new grid after making the move"
  [grid pos dir]
  (let [next-pos (common/vecadd pos dir)
        next-dict (get-next-empty-pos-2 grid pos dir)]
    (if (every? some? (vals next-dict))
      [(reduce (fn [g [from to]]
                (assoc-in g to (get-in grid from)))
              (reduce (fn [g pos]
                        (assoc-in g pos \.)) 
                      grid (keys next-dict))
              next-dict)
       next-pos]
      [grid pos])))

(defn print-map
  [grid pos]
  (mapv println (-> grid (assoc-in pos \@)))
  (println))

(defn part2 [opts]
  (println opts)
  (let [lines (common/get-lines (:data opts))
        [grid-lines space move-lines] (partition-by #(= "" %) lines)
        grid (mapv (comp vec chars char-array) grid-lines)
        start-pos (find-robot grid)
        grid (assoc-in grid start-pos \.)
        moves (reduce (fn [acc l] (concat acc (char-array l))) [] move-lines)
        [final-grid final-pos] (reduce (fn [[g pos] move]
                                         (let [[new-g new-pos] (make-move-2 g pos (move-map move))]
                                           [new-g new-pos]))
                                       [grid start-pos]
                                       moves)]
    (println "part 2: " (common/reduce-grid final-grid 
                                            (fn [sum item r c]
                                              (if (= item \[)
                                                (+ sum (+ (* 100 r) c))
                                                sum))
                                            0))))

(defn part1 [opts]
  (println opts)
  (let [lines (common/get-lines (:data opts))
        [grid-lines space move-lines] (partition-by #(= "" %) lines)
        grid (mapv (comp vec chars char-array) grid-lines)
        start-pos (find-robot grid)
        moves (reduce (fn [acc l] (concat acc (char-array l))) [] move-lines)
        [final-grid final-pos] (reduce (fn [[g pos] move]
                                         (let [[new-g new-pos] (make-move g pos (move-map move))]
                                           [new-g new-pos]))
                                       [(assoc-in grid start-pos \.) start-pos]
                                       moves)]
    (println "part 1: " (common/reduce-grid final-grid 
                                            (fn [sum item r c]
                                              (if (= item \O)
                                                (+ sum (+ (* 100 r) c))
                                                sum))
                                            0))
    ))
