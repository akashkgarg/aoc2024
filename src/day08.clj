(ns day08)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

(defn op-coords
  [op [r c] [r2 c2]]
  [(op r r2) (op c c2)])

(defn sub-coords
  "subtract coordinates"
  [a b]
  (op-coords - a b))

(defn in-bounds
  [grid [r c]]
  (let [nrows (count grid)
        ncols (count (first grid))]
    (and (< r nrows) (>= r 0) (< c ncols) (>= c 0))))

(defn add-coords
  [a b]
  (op-coords + a b))

(defn get-anti-nodes
  ;; part 1 version
  ([a b]
   (let [diff (sub-coords a b)]
     [(add-coords a diff) (sub-coords b diff)]))
  ;; part 2 version
  ([grid a b]
   (let [diff (sub-coords a b)
         nodes-left (loop [nodes []]
                           (let [new-node (add-coords (if (empty? nodes) b (last nodes)) diff)]
                             (if (in-bounds grid new-node)
                               (recur (conj nodes new-node))
                               nodes)))
         nodes-right (loop [nodes []]
                       (let [new-node (sub-coords (if (empty? nodes) a (last nodes)) diff)]
                         (if (in-bounds grid new-node)
                           (recur (conj nodes new-node))
                           nodes)))]
     (concat nodes-left nodes-right))))

(defn find-antinodes
  ([as get-anti-node-fn] 
   (loop [a1 (first as)
          rs (rest as)
          results []]
     (if (empty? rs)
       results
       (recur (first rs) 
              (rest rs) 
              (concat results 
                      (find-antinodes a1 rs get-anti-node-fn))))))
  ([a1 rs get-anti-node-fn]
   (reduce (fn [acc a2]
             (concat acc (get-anti-node-fn a1 a2))) [] rs)))

(defn find-anttenae
  [grid]
  (let [coords (filter (comp not empty?)
                       (map-indexed (fn [ridx row] 
                                      (filter some? 
                                              (map-indexed (fn [cidx item]
                                                             (if (= item \.) nil [item [ridx cidx]])) 
                                                           row))) 
                                    grid))]
    (reduce (fn [acc cs] 
              (loop [acc acc 
                     cs cs]
                (if (or (empty? cs) (nil? cs))
                  acc
                  (do 
                    (let [k (first (first cs))
                          v (second (first cs))]
                      (recur (update acc k (fnil conj []) v)
                             (rest cs)))))))
            {} coords)
    ))


(defn print-grid 
  [grid]
  (mapv println grid))

(defn solve
  [grid get-anti-node-fn]
  (let [anttenae (find-anttenae grid)
        antinodes (set 
                    (reduce 
                      (fn [acc ant-positions]
                        (concat acc (find-antinodes ant-positions get-anti-node-fn)))
                      []
                      (vals anttenae)))]
    (filter (partial in-bounds grid) antinodes)))
                    

(defn run [opts]
  (println opts)
  (let [grid (common/load-map (:data opts))
        antinodes-part1 (solve grid get-anti-nodes)
        antinodes-part2 (solve grid (fn [a b] (get-anti-nodes grid a b)))]
    (print-grid 
      (reduce (fn [g pos]
              (assoc-in g pos \#)) grid antinodes-part2))
    (println "part 1: " (count antinodes-part1))
    (println "part 2: " (count antinodes-part2))))
