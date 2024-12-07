(ns day06)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

(defn find-obstacles
  "Find all obstacles and store them in two maps, one indexed by row and one by
  column. This will make it easy to look up when we will hit an obstacle given a
  current position and movement direction"
  [grid]
  (let [get-obstacle-coords-in-row (fn [row] (filter some? 
                                                 (map-indexed 
                                                   (fn [idx itm] 
                                                     (if (= itm \#) idx nil)) row)))
        coords (filter #(not (empty? %))
                       (map-indexed (fn [idx row] 
                                      (interleave (repeat idx) 
                                                  (get-obstacle-coords-in-row row))) grid))
        by-rows (reduce (fn [acc c]
                          (update acc (first c) (fnil conj []) (last c))) 
                        {} 
                        coords)
        by-cols (reduce (fn [acc c]
                          (update acc (last c) (fnil conj []) (first c)))
                        {}
                        coords)]
    [by-rows by-cols]))

(defn load-grid 
  [file]
  (let [data (common/get-lines file)
        grid (mapv (comp vec chars char-array) data)]
    grid))

(defn find-start
  [grid]
  (flatten 
    (filter #(not (empty? %)) 
          (map-indexed 
            (fn [rowidx row]
              (filter some? 
                      (map-indexed (fn [colidx itm]
                                     (if (= itm \^) [rowidx colidx])) row))) 
                       grid))))

(defn rotate-right
  [dir]
  (cond
    (= dir :up) :right
    (= dir :right) :down
    (= dir :down) :left
    (= dir :left) :up
    :else (throw "invalid direction")))

(defn single-move
  [[row col] dir]
  (cond
    (= dir :up) [(dec row) col]
    (= dir :right) [row (inc col)]
    (= dir :down) [(inc row) col]
    (= dir :left) [row (dec col)]
    :else (throw "invalid direction in move")))

(defn get-item 
  [grid [r c]]
  (let [nrows (count grid)
        ncols (count (first grid))]
    (if (or (>= r nrows) (< r 0) (>= c ncols) (< c 0))
      nil 
      (nth (nth grid r) c))))


(defn move 
  [grid start]
  (let [nrows (count grid)
        ncols (count (first grid))]
    (loop [pos start
           dir :up
           visited #{}]
      (let [[r c] pos]
        (if (or (>= r nrows) (< r 0) (>= c ncols) (< c 0)) ;; exited the map
          visited
          ;; else 
          (let [newpos (single-move pos dir)
                item (get-item grid newpos)]
            (if (= item \#)
              (recur pos 
                     (rotate-right dir)
                     visited)
              (recur newpos
                     dir 
                     (conj visited pos)))))))))

(defn is-cyclic
  [grid start]
  (let [nrows (count grid)
        ncols (count (first grid))]
    (loop [pos start
           dir :up
           visited-so-far #{}]
      (let [[r c] pos]
        (cond 
          (or (>= r nrows) (< r 0) (>= c ncols) (< c 0)) ; exited the map
          false
          (visited-so-far [pos dir]) 
          true 
          :else
          (let [newpos (single-move pos dir)
                item (get-item grid newpos)]
            (if (= item \#)
              (recur pos 
                     (rotate-right dir)
                     visited-so-far)
              (recur newpos
                     dir 
                     (conj visited-so-far [pos dir])))))))))

(defn find-cyclic-obstacles
  [grid start obstacle-positions]
  (filter true? 
          (map (fn [obstacle-pos]
                 (let [new-grid (assoc-in grid obstacle-pos \#)]
                   (is-cyclic new-grid start))) obstacle-positions)))

(defn run [opts]
  (println opts)
  (let [grid (load-grid (:data opts))
        start (find-start grid)
        visited (move grid start)
        obs-pos (disj visited start)]
    (println "Part 1 : " (count visited))
    (println "Part 2 : " (count (find-cyclic-obstacles grid start obs-pos)))))
