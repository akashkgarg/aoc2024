(ns day04)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[common])


(defn get-elements
  [matrix [x y] dxys]
  (let [rows (count matrix)
        cols (count (first matrix))
        coords (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) dxys)]
    (if (every? (fn [[xi yi]] (and (>= xi 0) (< xi rows) (>= yi 0) (< yi cols))) coords) ;; bounds check
      (map (fn [[xi yi]] (get-in matrix [xi yi])) coords)
      nil)))

(defn get-elements-in-direction
  "Returns the 4 elements from a 2D vector starting at [x y] in the direction of [dx dy]."
  [matrix [x y] [dx dy]]
  (let [offsets (->> (range 4)
                     (map (fn [i] [(* i dx) (* i dy)])))] ; Generate coordinates in the given direction
    (get-elements matrix [x y] offsets)))

(defn find-char-locs
  [matrix c]
  (let [rows (count matrix)
        cols (count (first matrix))]
    (for [x (range rows)
          y (range cols)
          :when (= (get-in matrix [x y]) c)]
      [x y])))

(defn find-xs
  [matrix]
  (find-char-locs matrix \X))

(defn find-as 
  [matrix]
  (find-char-locs matrix \A))

(def all-dirs [[-1 0] [1 0] [0 1] [0 -1] [-1 -1] [1 1] [-1 1] [1 -1]])

(def mas-coords [[[-1 -1] [0 0] [1 1]] 
                 [[-1 1] [0 0] [1 -1]]])

(defn find-mas-from-location 
  [matrix xy]
  (let [candidates (filter some? (map (partial get-elements matrix xy) mas-coords))]
    (filter #(or (= [\M \A \S] %)
                 (= [\S \A \M] %)) candidates)))

(defn find-xmas-from-location
  [matrix xy]
  (let [candidates (filter some? (map (partial get-elements-in-direction matrix xy) all-dirs))]
    (filter #(= [\X \M \A \S] %) candidates)))

(defn find-num-xmas
  [matrix]
  (let [x-coords (find-xs matrix)
        candidates (mapv #(find-xmas-from-location matrix %) x-coords)]
    (w/walk count #(reduce + %) candidates)))

(defn find-num-mas
  [matrix]
  (let [a-coords (find-as matrix)
        candidates (mapv #(find-mas-from-location matrix %) a-coords)
        counts (filter #(= 2 %) (map count candidates))] ;; X is made when count is 2
    (count counts)))

(defn run [opts]
  (println opts)
  (let [data (common/get-lines (:data opts))
        grid (mapv (comp vec chars char-array) data)]
    ;;(println "part 1: " (find-num-xmas grid))
    (println "part 2: " (find-num-mas grid))))
