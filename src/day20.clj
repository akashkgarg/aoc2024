(ns day20)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[clojure.math :as math]
         '[common])


(defn find-symbol
  [grid s]
  (flatten 
    (filter #(not (empty? %)) 
          (map-indexed 
            (fn [rowidx row]
              (filter some? 
                      (map-indexed (fn [colidx itm]
                                     (if (= itm s) [rowidx colidx])) row))) 
                       grid))))

(defn get-nbrs
  [[r c]]
  [[(inc r) c]
   [(dec r) c]
   [r (inc c)]
   [r (dec c)]])

;; generate all coordinates manhattan distance of n from origin.
(defn get-cheat-offsets
  [n]
  (let [tri (reduce (fn [acc inv-r] 
                      (let [r (- n inv-r)
                            cols (range (inc inv-r))]
                        (reduce (fn [offsets c]
                                  (conj offsets [r c]))
                                acc 
                                cols)))
                    []
                    (range (inc n)))
        all-coords (reduce (fn [acc [sr sc]]
                             (reduce (fn [coords [r c]]
                                       (conj coords [(* r sr) (* c sc)]))
                                     acc
                                     tri))
                           []
                           [[1 1] [-1 1] [1 -1] [-1 -1]])]
    all-coords))

(defn costs-from-end
  [grid end]
  (loop [pos end
         prev nil
         all-costs {end 0}
         curr-cost 0]
    (let [nbrs (filter #(and (not= (get-in grid %) \#)
                             (not= % prev)) 
                       (get-nbrs pos))]
      (if (empty? nbrs)
        all-costs
        (if (> (count nbrs) 1)
          (throw (Exception. "Got more than one valid nbr"))
          (recur (first nbrs) 
                 pos 
                 (assoc all-costs (first nbrs) (inc curr-cost)) 
                 (inc curr-cost)))))))

(defn print-map-times
  [grid times-to-end]
  (mapv println 
        (reduce (fn [g [p t]]
                  (assoc-in g p t))
                grid 
                times-to-end)))

(defn compute-savings
  [grid times-to-end start]
  (loop [visited #{start}
         time-so-far 0
         pos start
         savings []]
    (let [valid-pos? (fn [n] (and (common/in-bounds? grid n)
                                  (not= (get-in grid n) \#) 
                                  (not (visited n))))
          nbrs (get-nbrs pos)
          second-nbrs (reduce (fn [acc n]
                                (concat acc (get-nbrs n))) [] nbrs)
          cheats (filter valid-pos? second-nbrs)
          valid-nbrs (filter valid-pos? nbrs)]
      (cond (empty? valid-nbrs)
            savings

            (> (count valid-nbrs) 1)
            (throw (Exception. "More than one valid nbr"))

            :else
            (recur (conj visited (first valid-nbrs))
                   (inc time-so-far)
                   (first valid-nbrs)
                   (reduce (fn [new-savings cheat]
                             (let [new-time (+ time-so-far (times-to-end cheat) 2)
                                   diff (- (+ time-so-far (times-to-end pos)) new-time)]
                               ;(println "pos: " pos " cheat: " cheat " diff " diff)
                               (conj new-savings diff)))
                           savings
                           cheats))))))


(defn compute-savings-2
  [grid times-to-end start]
  (let [offsets (set (get-cheat-offsets 20))]
    (loop [visited #{start}
           time-so-far 0
           pos start
           savings []]
      (let [valid-pos? (fn [n] (and (common/in-bounds? grid n)
                                    (not= (get-in grid n) \#) 
                                    (not (visited n))))
            nbrs (get-nbrs pos)
            cheat-candidates (mapv #(common/vecadd pos %) offsets)
            cheats (filter valid-pos? cheat-candidates)
            valid-nbrs (filter valid-pos? nbrs)]
        (cond (empty? valid-nbrs)
              savings

              (> (count valid-nbrs) 1)
              (throw (Exception. "More than one valid nbr"))

              :else
              (recur (conj visited (first valid-nbrs))
                     (inc time-so-far)
                     (first valid-nbrs)
                     (reduce (fn [new-savings cheat]
                               (let [cheat-pos (common/vecsub cheat pos)
                                     dist-to-cheat (+ (abs (first cheat-pos)) (abs (second cheat-pos)))
                                     new-time (+ time-so-far (times-to-end cheat) dist-to-cheat)
                                     diff (- (+ time-so-far (times-to-end pos)) new-time)]
                                 (conj new-savings diff)))
                             savings
                             cheats)))))))

(defn part1 [opts]
  (println opts)
  (let [grid (common/load-map (:data opts))
        start (find-symbol grid \S)
        end (find-symbol grid \E)
        costs (costs-from-end grid end)
        savings (compute-savings grid costs start)]
    (println "part1 : " (count (filter #(>= % 100) savings)))
    (println (frequencies savings))))


(defn part2 [opts]
  (println opts)
  (let [grid (common/load-map (:data opts))
        start (find-symbol grid \S)
        end (find-symbol grid \E)
        costs (costs-from-end grid end)
        savings (filter #(>= % 100) (compute-savings-2 grid costs start))]
    (println "part2 : " (count savings))))
