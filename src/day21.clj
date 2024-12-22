(ns day21)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[clojure.math :as math]
         '[common])

(def sample-inputs ["029A" "980A" "179A" "456A" "379A"])
(def inputs ["382A" "463A" "935A" "279A" "480A"])

(def numpad-nbr-map {\A {\< \0 \^ \3}
                     \0 {\> \A \^ \2}
                     \1 {\> \2 \^ \4}
                     \2 {\v \0 \< \1 \> \3 \^ \5}
                     \3 {\v \A \< \2 \^ \6}
                     \4 {\v \1 \> \5 \^ \7}
                     \5 {\v \2 \< \4 \> \6 \^ \8}
                     \6 {\v \3 \< \5 \^ \9}
                     \7 {\v \4 \> \8}
                     \8 {\< \7 \v \5 \> \9}
                     \9 {\v \6 \< \8}})

(def dirpad-nbr-map {\A {\< \^ \v \>}
                     \^ {\> \A \v \v}
                     \< {\> \v}
                     \v {\< \< \^ \^ \> \>}
                     \> {\< \v \^\A}})

(defn find-min-paths-on-pad
  [start end nbr-map]
  (loop [q [{:pos (or start \A) :steps 0 :path []}]
         min-steps {start 0}
         solutions []]
    (if (empty? q)
      solutions
      (let [{:keys [pos steps path]} (first q)
            rest-q (rest q)]
        (if (= pos end)
          (recur rest-q min-steps (conj solutions [steps path]))
          (let [new-nbrs (filter 
                           (fn [n]
                             (if (min-steps (:pos n))
                               (<= (:steps n) (min-steps (:pos n)))
                               true))
                           (mapv (fn [nbr] 
                                   {:pos (val nbr)
                                    :steps (inc steps)
                                    :path (conj path (key nbr))})
                                 (nbr-map pos)))
                new-q (sort-by :steps (concat rest-q new-nbrs))
                new-min-steps (merge min-steps 
                                     (into {} 
                                           (mapv (fn [nbr]
                                                   [(:pos nbr) (:steps nbr)])
                                                 new-nbrs)))]
            (recur new-q new-min-steps solutions)))))))

(def find-min-cost-path)

(def memoized-min-path-from-to
  (memoize 
    (fn min-path-from-to [from to level max-levels]
      (let [min-paths (find-min-paths-on-pad from to (if (= level 0) numpad-nbr-map dirpad-nbr-map))
            min-steps (reduce (fn [min-steps path]
                                (let [steps (find-min-cost-path (conj (second path) \A)
                                                                (inc level)
                                                                max-levels)]
                                  (min steps min-steps)))
                              1e16
                              min-paths)]
        min-steps))))

(defn find-min-cost-path 
  [code level max-levels]
  (if (= max-levels level)
    (count code)
    (loop [from \A
           to (first code)
           rest-code (rest code)
           total-steps 0]
      (cond 
        (nil? to)
        total-steps
        :else 
        (recur to (first rest-code) (rest rest-code) (+ total-steps (memoized-min-path-from-to from to level max-levels)))))))

(defn run
  [opts]
  ;; pass in :levels = 3 for part 1, :levels 26 for part 2
  (let [max-levels (:levels opts)
        input (common/get-lines (:data opts))]
    (println (reduce + 0 (map #(* (find-min-cost-path % 0 max-levels)
                                  (Integer/parseInt (subs % 0 (dec (count %)))))
                              input)))))
