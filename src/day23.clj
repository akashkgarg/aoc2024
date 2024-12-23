(ns day23)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set :as set]
         '[clojure.math :as math]
         '[common])

(defn parse-graph
  [lines]
  (reduce (fn [g line]
            (let [[a b] (str/split line #"-")]
              (update 
                (update g a (fnil conj #{}) b)
                b 
                (fnil conj #{}) 
                a)))
          {}
          lines))

(defn find-triangles
  [graph node]
  (let [connected? (fn [a b] (contains? (graph a) b))
        nbrs (graph node)]
    (reduce (fn [acc nbr]
              (let [nbr-nbrs (graph nbr)]
                (reduce (fn [tris nbr-nbr]
                          (if (connected? nbr-nbr node)
                            (conj tris (sort [node nbr nbr-nbr]))
                            tris))
                        acc
                        nbr-nbrs)))
            #{}
            nbrs)))

(defn bron-kerbosch
  "https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm#With_pivoting"
  [graph r p x cliques]
  (if (and (empty? p) (empty? x))
    ;; If P and X are empty, R is a maximal clique
    (conj cliques r)
    (let [pivot (first (or (seq p) (seq x)))]
      (reduce
        (fn [new-cliques v]
          (bron-kerbosch
            graph
            (conj r v) ;; Add v to the clique
            (clojure.set/intersection p (graph v)) ;; Neighbors of v in P
            (clojure.set/intersection x (graph v)) ;; Neighbors of v in X
            new-cliques))
        cliques
        (clojure.set/difference p (graph pivot))))))

(defn find-maximal-cliques
  [graph]
  (bron-kerbosch graph #{} (set (keys graph)) #{} #{}))

(defn part2 
  [opts]
  (let [lines (common/get-lines (:data opts))
        graph (parse-graph lines)
        cliques (find-maximal-cliques graph)
        max-clique (last (sort-by count cliques))]
    (println "password: " (str/join "," (sort max-clique)))))

(defn part1
  [opts]
  (let [lines (common/get-lines (:data opts))
        graph (parse-graph lines)
        tris (reduce (fn [acc node] 
                       (into acc (find-triangles graph node)))
                     #{}
                     (filter #(str/starts-with? % "t") (keys graph)))]
    (pr (count tris))))
