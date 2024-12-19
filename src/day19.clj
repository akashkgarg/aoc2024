(ns day19)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[clojure.math :as math]
         '[common])

(defn match
  [towels logo invalid-logos]
  (cond 
    (empty? logo)
    [true invalid-logos]

    (invalid-logos logo)
    [false invalid-logos]

    :else 
    (let [matched-towels (filter #(str/starts-with? logo %) towels)]
      (if (empty? matched-towels)
        [false (conj invalid-logos logo)]
        (loop [matches matched-towels
               invalid invalid-logos]
          (if (empty? matches)
            [false (conj invalid logo)]
            (let [first-match (first matches)
                  rest-matches (rest matches)
                  [is-match additional-invalid] (match towels (subs logo (count first-match)) invalid)]
              (if is-match
                [true invalid]
                (recur rest-matches (into invalid additional-invalid))))))))))

(defn run 
  [opts]
  (let [lines (common/get-lines (:data opts))
        [towels-str _ logos] (partition-by #(= % "") lines)
        towels (map str/trim (str/split (first towels-str) #","))]
    (loop [logos logos
           invalid-logos #{}
           count-true 0]
      (if (not (empty? logos))
        (let [logo (first logos)
              [is-match additional-invalid] (match towels logo invalid-logos)
              new-count (if is-match (inc count-true) count-true)]
          (println "match logo = " logo " = " is-match)
          (recur (rest logos) (into invalid-logos additional-invalid) new-count))
        (println "valid logos: " count-true)))))
