(ns day09)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])


(defn do-until [f x p]
  (if (p x) x (recur f (f x) p)))

(defn defrag
  [disk]
  (loop [fidx 0
         ridx (dec (count disk))
         result []]
    (if (> fidx ridx)
      result
      (let [fitem (nth disk fidx)
            ritem (nth disk ridx)
            defragged-item (if (= fitem -1) 
                             ritem 
                             fitem)]
        (recur (inc fidx)
               (if (= fitem -1) 
                 (do-until dec (dec ridx) #(not= (nth disk %) -1))
                 ridx)
               (conj result defragged-item))))))

(defn checksum 
  [disk]
  (reduce (fn [[acc idx] fileid]
            [(+ acc (* idx fileid)) (inc idx)])
          [0 0]
          disk))

(defn checksum-sections
  [disk]
  (reduce (fn [[acc idx] block]
            (let [fileid (first block)
                  filesize (second block)
                  newidx (+ idx filesize)]
              (if (< fileid 0)
                [acc newidx]
                [(+ acc (reduce #(+ %1 (* fileid %2)) 0 (range idx newidx))) newidx])))
          [0 0]
          disk))

(defn expand-disk 
  [data]
  (loop [file-idx 0
         input data
         result []]
    (if (or (nil? input) (empty? input))
      result
      (recur (inc file-idx)
             (drop 2 input)
             (vec (concat result 
                          (repeat (first input) file-idx)
                          (repeat (if (nil? (second input))
                                    0
                                    (second input)) -1)))))))

(defn create-disk-sections
  [data]
  (loop [file-idx 0
         input data
         result []]
    (if (empty? input)
      result
      (recur (inc file-idx)
             (drop 2 input)
             (conj result 
                   [file-idx (first input)]
                   (if (or (nil? (second input)) (= 0 (second input)))
                     [-1 0]
                     [-1 (second input)]))))))

(defn find-free-space
  [disk fileidx]
  (let [[fileid filesize] (nth disk fileidx)]
    (loop [freeidx 1]
      (cond 
        (or (< fileidx 1) 
            (> freeidx fileidx)
            (>= freeidx (count disk)))
        nil

        (and 
          (= (first (nth disk freeidx)) -1)
          (>= (second (nth disk freeidx)) filesize))
        (do 
          freeidx)

        :else 
        (recur (do-until 
                 inc
                 (inc freeidx)
                 #(= (first (nth disk %)) -1)))))))

(defn defrag-2 
  [disk]
  (loop [result disk 
         ridx (- (count disk) 2)]
    ;; do the N^2 thing.
    ;; Probably could optimize this by using a datastructure that stores the
    ;; "free" list. Also we have to insert new elements into the disk, which
    ;; means an array is probably not the best datastructure, but clojure here
    ;; uses a persistent vector which is a tree with 32 branches...so that is
    ;; fairly efficient for random insertion using concat.
    (if (= ridx 0)
      result
      (let [moveidx (find-free-space result ridx)
            new-result (if (nil? moveidx) 
                         result
                         (let [[fileid filesize] (nth result ridx)
                               freespace (second (nth result moveidx))
                               freeblock [-1 (- freespace filesize)]
                               diskblock [fileid filesize]
                               moved-disk (assoc result ridx [-1 filesize])]
                           (vec (concat (subvec moved-disk 0 moveidx)
                                        [[fileid filesize] freeblock]
                                        (subvec moved-disk (inc moveidx))))))]
        (recur new-result (do-until dec 
                                    (dec ridx) 
                                    #(not= (first (nth new-result %)) -1)))))))

;; I realize I can just keep a list of the free list and compute the checksum
;; iteratively instead of trying to actually defrag the disk. Next time. 
(defn run [opts]
  (println opts)
  (let [data (mapv #(Character/digit % 10) (common/get-data (:data opts)))
        ;;expanded (expand-disk data)
        disk-sections (create-disk-sections data)]
    (pr (checksum-sections (defrag-2 disk-sections)))
    (println)))
    ;;(pr (checksum (defrag expanded)))))
