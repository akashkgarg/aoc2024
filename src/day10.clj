(ns day10)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

(defn find-heads
  [grid]
  (let [coords (filter (comp not empty?)
                       (map-indexed (fn [ridx row] 
                                      (filter some? 
                                              (map-indexed (fn [cidx item]
                                                             (if (= item 0) [ridx cidx] nil)) 
                                                           row))) 
                                    grid))]
  (reduce concat [] coords)))

(defn get-neighbors 
  [grid [r c]]
  (filter #(common/in-bounds? grid %)
          [[(dec r) c]
           [(inc r) c]
           [r (dec c)]
           [r (inc c)]]))

(defn get-val 
  [grid [r c]]
  (nth (nth grid r) c))

(defn find-ends
  [grid head]
  (if (= (get-val grid head) 9) 
    (set [head])
    (let [neighbors (get-neighbors grid head)]
      (reduce clojure.set/union 
              #{}
              (filter (comp not empty?) (map (fn [n]
                                               (cond (not= (- (get-val grid n) (get-val grid head)) 1) []
                                                     :else (find-ends grid n)))
                                           neighbors))))))

(defn find-trails 
  [grid head]
  (if (= (get-val grid head) 9) 
    1
    (let [neighbors (get-neighbors grid head)]
      (reduce +
              0
              (map (fn [n]
                     (cond (not= (- (get-val grid n) (get-val grid head)) 1) 0
                           :else (find-trails grid n)))
                   neighbors)))))

(defn count-trail-heads
  [grid heads]
  (reduce + 0 (map #(count (find-ends grid %)) heads)))

(defn count-num-trails
  [grid heads]
  (reduce + 0 (map #(find-trails grid %) heads)))

(defn run [opts]
  (println opts)
  (let [grid (common/load-digit-map (:data opts))
        heads (find-heads grid)]
    ;;(pr (count-trail-heads grid heads))
    (pr (count-num-trails grid heads))))
