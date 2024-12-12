(ns day12)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

(defn get-neighbors 
  [grid [r c]]
  [[(dec r) c]
   [(inc r) c]
   [r (dec c)]
   [r (inc c)]])

(defn count-boundary
  [grid pos]
  (let [item (get-in grid pos)
        neighbors (get-neighbors grid pos)]
    (count (filter (fn [npos]
                     (or (not (common/in-bounds grid npos))
                         (not= (get-in grid npos) item))) 
                   neighbors))))

(defn flood-fill 
  [grid start]
  (let [item (get-in grid start)]
    (loop [visited #{}
           to-visit [start]
           area 0
           perimeter 0]
      (if (empty? to-visit)
        [visited area perimeter]
        (let [[pos & rest-to-visit] to-visit
              neighbors (filter (fn [nbr]
                                  (and (common/in-bounds grid nbr)
                                       (= (get-in grid nbr) item)
                                       (not (contains? visited nbr))))
                                (get-neighbors grid pos))]
          (if (visited pos)
            (recur visited rest-to-visit area perimeter)
            (recur 
              (conj visited pos)
              (into rest-to-visit neighbors)
              (inc area)
              (+ perimeter (count-boundary grid pos)))))))))

(defn run [opts]
  (println opts)
  (let [grid (common/load-map (:data opts))
        unvisited (common/reduce-grid grid 
                                      (fn [acc value ridx cidx]
                                        (conj acc [ridx cidx]))
                                      #{})
        cost (loop [to-visit unvisited
                    cost 0]
               (if (empty? to-visit)
                 cost
                 (let [pos (first to-visit)
                       [visited area perimeter] (flood-fill grid pos)]
                   (recur 
                     (clojure.set/difference to-visit visited)
                     (+ cost (* area perimeter))))))]
    (println "Part 1 : " cost)))
