(ns day22)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[clojure.math :as math]
         '[common])

(defn step1 
  [secret-num]
  (-> secret-num
      (bit-shift-left 6)
      (bit-xor secret-num)
      (bit-and 16rFFFFFF)))

(defn step2
  [secret-num]
  (-> secret-num 
      (bit-shift-right 5)
      (bit-xor secret-num)
      (bit-and 16rFFFFFF)))

(defn step3 
  [secret-num]
  (-> secret-num 
      (bit-shift-left 11)
      (bit-xor secret-num)
      (bit-and 16rFFFFFF)))

(defn xform 
  [n]
  (-> n
      (step1)
      (step2)
      (step3)))

(defn part1 
  [opts]
  (let [nums (mapv parse-long (common/get-lines (:data opts)))
        new-nums (mapv (fn [n] 
                         (reduce (fn [acc _]
                                   (xform acc))
                                 n 
                                 (range 2000)))
                       nums)]
    (println (reduce + 0 new-nums))))

(defn get-unique-seqs 
  [prices+diffs]
  (loop [pd (rest prices+diffs) ; start by dropping first element that has no diff
         s #{}]
    (if (< (count pd) 4)
      s
      (let [diffs (mapv second (take 4 pd))]
        (recur (rest pd) (conj s diffs))))))

(defn get-seq-prices
  [prices+diffs]
  (loop [pd (rest prices+diffs) ; start by dropping first element that has no diff
         res {}]
    (if (< (count pd) 4)
      res
      (let [diffs (take 4 pd)
            diff-price (first (last diffs))
            diff-seq (mapv second diffs)]
        (recur (rest pd) (update res
                                 diff-seq 
                                 (fnil identity diff-price)))))))

(defn score-seq
  [seq-prices s]
  (reduce + 0 (map #(get % s 0) seq-prices)))

(defn find-best-seq
  [all-seqs seq-prices]
  (reduce (fn [[best-score best-seq] s]
            (let [score (score-seq seq-prices s)]
             (if (> score best-score)
               [score s]
               [best-score best-seq])))
          [-1 nil]
          all-seqs))

(defn part2
  [opts]
  (let [nums (mapv parse-long (common/get-lines (:data opts)))
        prices+diffs (mapv (fn [n] 
                             (loop [acc [[(mod n 10) nil]] 
                                    prev n
                                    iter 0]
                               (if (>= iter 2000)
                                 (do 
                                   acc)
                                 (let [x (xform prev)
                                       price (mod x 10)
                                       prev-price (mod prev 10)]
                                   (recur (conj acc [price (- price prev-price)]) x (inc iter))))))
                           nums)
        seq-prices (reduce (fn [s pd]
                             (conj s (get-seq-prices pd)))
                           []
                           prices+diffs)
        unique-seqs (reduce (fn [s sp]
                              (into s (keys sp)))
                            #{}
                            seq-prices)]
    (println (find-best-seq unique-seqs seq-prices))))
