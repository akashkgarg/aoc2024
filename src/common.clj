(ns common)

(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(defn get-data [file] 
  (slurp (io/as-relative-path (str "resources/" file))))

(defn get-lines [file] 
  (str/split-lines (get-data file)))

(defn load-map
  [file]
  (let [data (common/get-lines file)
        grid (mapv (comp vec chars char-array) data)]
    grid))

(defn load-digit-map
  [file]
  (let [data (common/get-lines file)
        grid (mapv (comp #(mapv (fn [c] 
                                 (Character/digit c 10)) %) 
                         char-array) data)]
    grid))

(defn in-bounds
  [grid [r c]]
  (let [nrows (count grid)
        ncols (count (first grid))]
    (and (< r nrows) (>= r 0) (< c ncols) (>= c 0))))

(defn iterate-grid
  "Iterates over a 2D grid (nested vectors) and applies a function `f` to each
  value along with its row and column coordinates. The function `f` should
  take three arguments: the value, the row index, and the column index."
  [grid f]
  (doseq [row-idx (range (count grid))
          col-idx (range (count (get grid row-idx)))]
    (let [value (get-in grid [row-idx col-idx])]
      (f value row-idx col-idx))))

(defn reduce-grid
  "Reduces over a 2D grid (nested vectors) by applying a reducing function `f`
  to each value along with its row and column coordinates, starting with an
  initial accumulator value. The function `f` should take four arguments: the
  accumulator, the value, the row index, and the column index."
  [grid f init]
  (reduce (fn [acc row-idx]
            (reduce (fn [acc col-idx]
                      (let [value (get-in grid [row-idx col-idx])]
                        (f acc value row-idx col-idx)))
                    acc
                    (range (count (get grid row-idx)))))
          init
          (range (count grid))))

(defn vecadd
  [a b]
  (mapv #(+ %1 %2) a b))
