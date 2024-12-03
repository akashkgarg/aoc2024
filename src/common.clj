(ns common)

(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(defn get-data [file] 
  (slurp (io/as-relative-path (str "resources/" file))))

(defn get-lines [file] 
  (str/split-lines (get-data file)))
