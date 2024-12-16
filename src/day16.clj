(ns day16)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

(defn get-neighbors 
  [grid pos dir]
  (let [[dr dc] dir
        dir-right [dc dr]
        dir-left [(* -1 dc) (* -1 dr)]]
    (filter #(not= (get-in grid %) \#)
            [(common/vecadd pos dir)
             (common/vecadd pos dir-right)
             (common/vecadd pos dir-left)])))

(defn is-turning?
  [[r1 c1] [r2 c2]]
  (if (or (and (= r1 0) (= r2 0))
          (and (= c1 0) (= c2 0)))
    false
    true))

(defn print-path
  [grid path]
  (let [posdirs (map (fn [prev pos]
                   (let [dir (common/vecsub pos prev)]
                     (cond 
                       (= dir [0 1]) [pos \>]
                       (= dir [0 -1]) [pos \<]
                       (= dir [1 0]) [pos \v]
                       (= dir [-1 0]) [pos \^]
                       :else [pos \S])))
                 path (rest path))]
    (mapv println (reduce (fn [g pd] 
                            (assoc-in g (first pd) (second pd)))
                          grid
                          posdirs))))

(defn find-best-paths
  [grid]
  (let [nrows (count grid)
        ncols (count (first grid))
        start [(- nrows 2) 1]
        start-dir [0 1]
        initial-q [{:pos start :cost 0 :path [start] :dir start-dir}]
        initial-costs {[start start-dir] 0}]
    (loop [q initial-q
           costs initial-costs
           solutions []]
      (if (empty? q) 
        solutions
        (let [{:keys [pos cost path dir]} (first q)
              [r c] pos
              rest-q (rest q)
              item (get-in grid pos)]
          (cond 
            (= item \E) 
            (recur rest-q costs (conj solutions [cost path]))

            :else
            (let [new-neighbors (filter (fn [nn]
                                          (if (costs [(:pos nn) (:dir nn)])
                                            (<= (:cost nn) (costs [(:pos nn) (:dir nn)]))
                                            true))
                                        (map (fn [n]
                                               {:pos n
                                                :cost (+ cost (if (is-turning? dir (common/vecsub n pos)) 1001 1))
                                                :path (conj path n)
                                                :dir (common/vecsub n pos)})
                                             (get-neighbors grid pos dir)))
                  new-q (sort-by :cost (concat rest-q new-neighbors))
                  new-costs (merge costs (into {} 
                                               (mapv (fn [neighbor] 
                                                       [[(:pos neighbor) (:dir neighbor)] (:cost neighbor)]) 
                                                     new-neighbors)))]
              (recur new-q new-costs solutions))))))))

(defn find-goal
  [grid]
  (let [nrows (count grid)
        ncols (count (first grid))
        start [(- nrows 2) 1]
        start-dir [0 1]
        initial-q [{:pos start :cost 0 :path [start] :dir start-dir}]
        initial-costs {[start start-dir] 0}]
    (loop [q initial-q
           costs initial-costs]
      (if (empty? q) 
        (throw (Exception. "E not found"))
        (let [{:keys [pos cost path dir]} (first q)
              [r c] pos
              rest-q (rest q)
              item (get-in grid pos)]
          (cond 
            (= item \E) 
            [cost path]

            (= item \#)
            (throw (Exception. "Found a obstacle in path"))
            
            :else
            (let [new-neighbors (filter (fn [nn]
                                          (if (costs [(:pos nn) (:dir nn)])
                                            (< (:cost nn) (costs [(:pos nn) (:dir nn)]))
                                            true))
                                        (map (fn [n]
                                               {:pos n
                                                :cost (+ cost (if (is-turning? dir (common/vecsub n pos)) 1001 1))
                                                :path (conj path n)
                                                :dir (common/vecsub n pos)})
                                             (get-neighbors grid pos dir)))
                  new-q (sort-by :cost (concat rest-q new-neighbors))
                  new-costs (merge costs (into {} 
                                               (mapv (fn [neighbor] 
                                                       [[(:pos neighbor) (:dir neighbor)] (:cost neighbor)]) 
                                                     new-neighbors)))]
              (recur new-q new-costs))))))))

(defn run [opts]
  (println opts)
  (let [grid (common/load-map (:data opts))
        [cost path] (find-goal grid)
        solutions (find-best-paths grid)
        min-cost (apply min (map first solutions))
        tiles-for-min-cost (reduce (fn [acc sol]
                                     (if (= (first sol) min-cost)
                                       (into acc (second sol))
                                       acc))
                                   #{}
                                   solutions)]
    (println "min cost = " min-cost)
    (println "best tiles = " (count tiles-for-min-cost))))

