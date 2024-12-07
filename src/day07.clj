(ns day07)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

(defn test-calibration 
  ([ops operands]
   (test-calibration ops (first operands) (second operands) (drop 2 operands)))

  ([ops result x ys]
   (cond 
     (empty? ys) 
     (if (= x result) true false)

     (some true? (map (fn [op] 
                        (test-calibration ops result (op x (first ys)) (rest ys))) ops))
     true

     :else 
     false)))

;; execution is slightly faster to do it this way than it is to use the
;; lazy-evaluation with (some true? ...) above. But the above is more terse and
;; readable.
(defn test-calibration-2
  ([operands]
   (test-calibration-2 (first operands) (second operands) (drop 2 operands)))

  ([result x ys]
   (cond 
     (empty? ys) 
     (if (= x result) true false)

     (test-calibration-2 result (+ x (first ys)) (rest ys))
     true

     (test-calibration-2 result (* x (first ys)) (rest ys)) 
     true

     (test-calibration-2 result (parse-long (str x (first ys))) (rest ys)) 
     true

     :else 
     false)))

(defn solve
  [lines ops]
  (reduce (fn [acc line] 
            (if (test-calibration ops line) (+ acc (first line)) acc))
          0
          lines))

(defn run [opts]
  (println opts)
  (let [lines (mapv #(mapv parse-long (str/split % #":\s|\s")) 
                    (common/get-lines (:data opts)))]
    (println "Part 1: " (solve lines [+ *]))
    (println "Part 2: " (solve lines [+ * (fn [x y] (parse-long (str x y)))]))))

