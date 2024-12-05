(ns day05)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[common])

(defn parse-rule [rule]
  (str/split rule #"\|"))

(defn parse-rules [rules]
  (reduce (fn [acc rule]
            (let [[before after] (parse-rule rule)]
              (update acc before (fnil conj #{}) after))) {} rules))

(defn parse-pages [ordering]
  (str/split ordering #","))

(defn load-data [file]
  (let [lines (common/get-lines file)
        segments (partition-by #(= "" %) lines)
        rules (first segments)
        documents (last segments)]
    [(parse-rules rules) 
     (mapv parse-pages documents)]))

(defn validate [rules page-nums]
  (let [head (first page-nums)
        tail (rest page-nums)
        valid-pages-after-head (rules head)]
    (every? some? (map #(if 
                          (nil? valid-pages-after-head) nil
                          (valid-pages-after-head %)) tail))))

(defn validate-all-pages [rules page-nums]
  (if 
    (empty? page-nums) true
    (and (validate rules page-nums)
         (validate-all-pages rules (rest page-nums)))))

(defn middle-page [page-nums]
  (nth page-nums (quot (count page-nums) 2)))

(defn topological-sort
  "Perform a topological sort on a directed graph.
  `graph` is a map where keys are nodes and values are sets of nodes that follow the key.
  Returns a vector of nodes in topological order or nil if a cycle is detected."
  [graph]
  (let [in-degree (reduce (fn [acc node]
                            (reduce (fn [inner-acc dep]
                                      (update inner-acc dep (fnil inc 0)))
                                    acc
                                    (graph node)))
                          (zipmap (keys graph) (repeat 0))
                          (keys graph))
        no-incoming (into clojure.lang.PersistentQueue/EMPTY
                          (filter #(zero? (in-degree %)) (keys graph)))]
    (loop [sorted [] queue no-incoming in-degree in-degree]
      (if (empty? queue)
        (if (= (count sorted) (count graph))
          sorted
          nil) ; Return nil if a cycle exists
        (let [node (peek queue)
              neighbors (graph node)
              queue (pop queue)
              updated-in-degree (reduce (fn [acc neighbor]
                                          (update acc neighbor dec))
                                        in-degree
                                        neighbors)
              new-no-incoming (filter #(zero? (updated-in-degree %)) neighbors)]
          (recur (conj sorted node)
                 (into queue new-no-incoming)
                 updated-in-degree))))))

(defn sort-document [rules document]
  (let [page-set (set document)
        ;; make sure our graph only contains nodes that are in the document
        graph (reduce (fn [graph node]
                        (assoc graph node (clojure.set/intersection page-set (rules node))))
                      {} document)]
    (topological-sort graph)))

(defn run [opts]
  (println opts)
  (let [[rules documents] (load-data (:data opts))
        valid-middle-pages (filter some? (map (fn [document]
                                                (if 
                                                  (validate-all-pages rules document) (middle-page document) 
                                                  nil)) 
                                              documents))
        invalid-documents (filter some? (map (fn [document]
                                               (if 
                                                 (not (validate-all-pages rules document)) document))
                                             documents))
        resorted-middle-pages (map middle-page (map #(sort-document rules %) invalid-documents))]
    (println "part 1: " (reduce #(+ (Integer/parseInt %2) %1) 0 valid-middle-pages))
    (println "part 2: " (reduce #(+ (Integer/parseInt %2) %1) 0 resorted-middle-pages))))
