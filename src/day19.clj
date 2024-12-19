(ns day19)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[clojure.math :as math]
         '[common])

(defn match
  [towels logo logo-counts]
  (cond 
    (empty? logo)
    [1 (update logo-counts logo (fnil inc 0))]

    (contains? logo-counts logo)
    [(logo-counts logo) logo-counts]

    :else 
    (let [matched-towels (filter #(str/starts-with? logo %) towels)]
      (if (empty? matched-towels)
        [0 (assoc logo-counts logo 0)]
        (loop [matches matched-towels
               counts logo-counts 
               curr-count 0]
          (if (empty? matches)
            [curr-count (assoc counts logo curr-count)]
            (let [first-match (first matches)
                  rest-matches (rest matches)
                  [add-count new-counts] (match towels (subs logo (count first-match)) counts)]
              (recur rest-matches 
                     new-counts
                     (+ add-count curr-count)))))))))

(defn run 
  [opts]
  (let [lines (common/get-lines (:data opts))
        [towels-str _ logos] (partition-by #(= % "") lines)
        towels (map str/trim (str/split (first towels-str) #","))]
    (loop [logos logos
           total-matches 0
           count-true 0]
      (if (not (empty? logos))
        (let [logo (first logos)
              [num-matches new-counts] (match towels logo {})
              pos-matches (if (> num-matches 0) (inc count-true) count-true)]
          (println "match logo = " logo " = " num-matches)
          (recur (rest logos) 
                 (+ total-matches num-matches) 
                 pos-matches))
        (do 
          (println "valid logos: " count-true)
          (println "total matches: " total-matches))))))
