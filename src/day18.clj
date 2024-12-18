(ns day18)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[clojure.math :as math]
         '[common])

(defn print-map 
  [size corrupt-locs] 
  (mapv println 
        (reduce (fn [yacc y]
                  (conj yacc 
                        (reduce (fn [xacc x]
                                  (let [coord [x y]]
                                    (if (contains? corrupt-locs coord)
                                      (conj xacc \#)
                                      (conj xacc \.))))
                                []
                                (range (inc size)))))
                []
                (range (inc size)))))

(defn in-bounds? 
  [[x y] size]
  (and (and (<= x size) (>= x 0))
       (and (<= y size) (>= y 0))))

(defn get-neighbors
  [[x y] size]
  (filter #(in-bounds? % size)
          [[(inc x) y]
           [(dec x) y]
           [x (inc y)]
           [x (dec y)]]))

(defn find-path
  [start end size corrupt-locs]
  (let [initial-q [{:pos start :steps 0}]
        initial-visited #{start}]
    (loop [q initial-q
           visited initial-visited]
      (let [{:keys [pos steps]} (first q)
            rest-q (rest q)]
        (cond 
          (empty? q)
          nil

          (= pos end)
          steps

          :else 
          (let [new-neighbors (filter (fn [nn] 
                                        (and (not (visited (:pos nn)))
                                             (not (corrupt-locs (:pos nn)))))
                                      (map (fn [n]
                                             {:pos n
                                              :steps (inc steps)})
                                           (get-neighbors pos size)))
                new-q (sort-by :steps (concat rest-q new-neighbors))
                new-visited (into visited (mapv :pos new-neighbors))]
            (recur new-q new-visited)))))))

(defn part1 [opts]
  (println opts)
  (let [lines (common/get-lines (:data opts))
        size (:size opts)
        nbytes (:nbytes opts)
        start [0 0]
        end [size size]
        coords (map (fn [line] 
                      (mapv Integer/parseInt (str/split line #",")))
                    lines)]
    (println "steps: " (find-path start end size (set (take nbytes coords))))))

(defn part2 [opts]
  (println opts)
  (let [lines (common/get-lines (:data opts))
        size (:size opts)
        nbytes (:nbytes opts)
        start [0 0]
        end [size size]
        coords (map (fn [line] 
                      (mapv Integer/parseInt (str/split line #",")))
                    lines)]
    ;; walk linearly through remaining blockages, but could use binary search
    ;; here to speed things up even more.
    (loop [remaining-bytes (drop nbytes coords)
           corrupt-locs (set (take nbytes coords))
           blocking-coord nil]
      (let [steps (find-path start end size corrupt-locs)]
        (if (some? steps)
          (recur (rest remaining-bytes) (conj corrupt-locs (first remaining-bytes)) (first remaining-bytes))
          (println "Can't find path by adding: " blocking-coord))))))
