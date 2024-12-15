(ns day14)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

;; return new position
(defn move
  [pos vel width height]
  (let [[x y] (common/vecadd pos vel)]
    [(mod x width)
     (mod y height)]))

(defn check-repeat 
  [start vel width height]
  (loop [visited #{}
         pos start
         step 0]
    (if (= step 100000)
      (throw (Exception. "not repeating in 10k steps"))
      (if (contains? visited pos)
        step
        (recur (conj visited pos) (move pos vel width height) (inc step))))))

(defn parse-pos-vel
  [line]
  (let [regex #"p=([-]?\d+),([-]?\d+) v=([-]?\d+),([-]?\d+).*"
        matches (re-find regex line)]
    (if matches
      (partition 2 (map Integer/parseInt (rest matches))))))

(defn simulate-steps
  [start vel nsteps width height]
  (reduce (fn [pos step]
            (let [newpos (move pos vel width height)]
              newpos))
          start
          (range nsteps)))

(defn print-map 
  [positions width height]
  (let [grid (->> (vec (repeat width \.))
                  (repeat height)
                  (vec))
        updated-grid (reduce (fn [updated-grid [x y]]
                               (update-in updated-grid [y x] (fn [c]
                                                               (if (char? c) 
                                                                 1
                                                                 (inc c)))))
                             grid
                             positions)]
    (mapv println updated-grid)))

(defn get-quadrant
  [pos width height]
  (let [[x y] pos
        mid-w (int (/ width 2))
        mid-h (int (/ height 2))]
    (if (or (= x mid-w) (= y mid-h))
      nil
      (+ (if (< x mid-w) 0 1)
         (if (< y mid-h) 0 2)))))

(defn average [coll] 
  (/ (reduce + coll) (count coll)))

(defn symmetric-positions? 
  [positions]
  (let [pos-set (set positions)
        xs (map first positions)
        mid-w (average xs)]
    (every? 
      (fn [[x y]] 
        (let [reflected-pos [(- (* mid-w 2) x) y]]
          (contains? pos-set reflected-pos)))
      positions)))

;; This didn't work since not all robots were involved in making of xmas tree. I
;; just brute forced it by printing out the map and scrolling through to find
;; the map with the xmas tree. Upon reflection a smarter way might have been to
;; do some kernel density estimation and look for peaks or maybe just look at
;; the variance of x-coordinates.
(defn part2 
  ;; a xmas tree is symmetric so if robots are arranged in that fashion, we can
  ;; try to find the middle and see if we have symmetry across the middle x
  ;; values. That might possibly gives us a xmas tree? could be many such
  ;; configurations?  we have an upper limit since know that robot position repeat.
  [robots width height max-steps]
  (let [velocities (map second robots)]
    (loop [robot-posvel robots
           step 0]
      (let [new-positions (map #(simulate-steps (first %) (second %) 1 width height) robot-posvel)]
        (cond 
          (= step max-steps) (throw (Exception. "symmetry not found"))
          (symmetric-positions? new-positions) (print-map new-positions width height)
          :else 
          (recur (partition 2 (interleave new-positions velocities)) (inc step)))))))

(defn run [opts]
  (println opts)
  (let [lines (common/get-lines (:data opts))
        width (:width opts)
        height (:height opts)
        nsteps (:steps opts)
        robots (map parse-pos-vel lines) ;; stores [pos,vel]
        ;; apparently all robots repeat at the same step
        repeat-n (check-repeat (first (first robots)) (second (first robots)) width height)
        new-positions (map #(simulate-steps (first %) (second %) nsteps width height) robots)
        positions-by-quadrants (reduce (fn [acc pos]
                                         (let [q (get-quadrant pos width height)]
                                           (if (some? q)
                                             (update-in acc [q] (fnil inc 0))
                                             acc)))
                                       {}
                                       new-positions)]
    ;;(print-map new-positions width height)
    (println positions-by-quadrants)
    ;; (pr (map #(check-repeat (first %) (second %) width height) robots))
    (println "repeating in " repeat-n)
    (println "part 1: " (reduce * 1 (vals positions-by-quadrants)))
    (part2 robots width height repeat-n)))
