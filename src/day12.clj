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

(defn is-boundary 
  [grid item npos]
  (or (not (common/in-bounds grid npos))
      (not= (get-in grid npos) item)))


(defn count-boundary
  [grid pos]
  (let [item (get-in grid pos)
        neighbors (get-neighbors grid pos)]
    (count (filter #(is-boundary grid item %) neighbors))))

;; get boundary segments, stored as endpoints of grid _corners_
;; also need to keep track of which side the boundary lies on so we can see if
;; they are connected.
(defn get-boundary
  [grid [r c]]
  (let [item (get-in grid [r c])]
    (filter some? 
            [(if (is-boundary grid item [(dec r) c]) [[r c] [r (inc c)] :U])
             (if (is-boundary grid item [(inc r) c]) [[(inc r) c] [(inc r) (inc c)] :D])
             (if (is-boundary grid item [r (dec c)]) [[r c] [(inc r) c] :L])
             (if (is-boundary grid item [r (inc c)]) [[r (inc c)] [(inc r) (inc c)] :R])])))



(defn flood-fill 
  [grid start]
  (let [item (get-in grid start)]
    (loop [visited #{}
           to-visit [start]
           area 0
           boundary #{}]
      (if (empty? to-visit)
        [visited area boundary]
        (let [[pos & rest-to-visit] to-visit
              neighbors (filter (fn [nbr]
                                  (and (common/in-bounds grid nbr)
                                       (= (get-in grid nbr) item)
                                       (not (contains? visited nbr))))
                                (get-neighbors grid pos))]
          (if (visited pos)
            (recur visited rest-to-visit area boundary)
            (recur 
              (conj visited pos)
              (into rest-to-visit neighbors)
              (inc area)
              (clojure.set/union boundary (get-boundary grid pos)))))))))

(defn straight? 
  [pt1 pt2]
  (or (= (first pt1) (first pt2))
      (= (second pt1) (second pt2))))

(defn connected-straight?
  [seg1 seg2]
  (and 
    ;; connections must have the same "boundary direction"
    (= (last seg1) (last seg2))
    ;; and a shared endpoint.
    (or (and (= (first seg1) (first seg2)) (straight? (second seg1) (second seg2)))
        (and (= (first seg1) (second seg2)) (straight? (second seg1) (first seg2)))
        (and (= (second seg1) (first seg2))  (straight? (first seg1) (second seg2)))
        (and (= (second seg1) (second seg2)) (straight? (first seg1) (first seg2))))))

(defn connect-segments
  [seg1 seg2]
  (cond 
    (= (first seg1) (first seg2)) [(second seg1) (second seg2) (last seg1)]
    (= (first seg1) (second seg2)) [(second seg1) (first seg2) (last seg1)]
    (= (second seg1) (first seg2)) [(first seg1) (second seg2) (last seg1)]
    (= (second seg1) (second seg2)) [(first seg1) (first seg2) (last seg1)]
    :else
    (throw "segments not connected!")))

(defn count-sides 
  [boundary]
  (loop [sides []
         to-visit boundary]
    (if (empty? to-visit)
      (count sides)
      ;; an N^2 search to find the connected segments. probably a better way,
      ;; but N is small.
      (let [[segment & rest-to-visit] to-visit
            connected-segment (first (filter #(connected-straight? segment %) rest-to-visit))]
        (if (nil? connected-segment)
          (do 
            (recur (conj sides segment) rest-to-visit))
          (recur sides 
                 (conj
                   (disj (set rest-to-visit) connected-segment)
                   (connect-segments segment connected-segment))))))))

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
                       [visited area boundary] (flood-fill grid pos)
                       perimeter (count boundary)]
                   (recur 
                     (clojure.set/difference to-visit visited)
                     (+ cost (* area perimeter))))))
        cost-sides (loop [to-visit unvisited
                          cost 0]
                     (if (empty? to-visit)
                       cost
                       (let [pos (first to-visit)
                             [visited area boundary] (flood-fill grid pos)
                             n-sides (count-sides boundary)]
                         (recur 
                           (clojure.set/difference to-visit visited)
                           (+ cost (* area n-sides))))))]
    (println "Part 1: " cost)
    (println "Part 2: " cost-sides)))
