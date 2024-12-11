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
