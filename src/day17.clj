(ns day17)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.walk :as w]
         '[clojure.set]
         '[clojure.math :as math]
         '[common])

(defn inc2 
  [value]
  (inc (inc value)))

(defn get-combo-value 
  [state operand]
  (cond 
    (and (>= operand 0) (<= operand 3)) 
    operand

    (= operand 4) 
    (:a state)

    (= operand 5)
    (:b state)

    (= operand 6)
    (:c state)))

(defn do-op 
  [state opcode operand] 
  (let [combo-value (get-combo-value state operand)
        literal-value operand
        instr (:instr state)]
    (cond 
      (= opcode 0)
      (assoc state 
             :a (bit-shift-right (:a state) combo-value)
             :instr (inc2 instr))

      (= opcode 1)
      (assoc state
             :b (bit-xor (:b state) literal-value)
             :instr (inc2 instr))

      (= opcode 2)
      (assoc state 
             :b (mod combo-value 8)
             :instr (inc2 instr))

      (= opcode 3)
      (assoc state 
             :instr (if (= (:a state) 0) 
                      (inc2 instr)
                      literal-value))

      (= opcode 4)
      (assoc state 
             :b (bit-xor (:b state) (:c state))
             :instr (inc2 instr))

      (= opcode 5)
      (assoc state
             :output (conj (get state :output []) (mod combo-value 8))
             :instr (inc2 instr))

      (= opcode 6)
      (assoc state 
             :b (bit-shift-right (:a state) combo-value)
             :instr (inc2 instr))

      (= opcode 7)
      (assoc state 
             :c (bit-shift-right (:a state) combo-value)
             :instr (inc2 instr))
      :else 
      (throw (Exception. "invalid op")))))

(defn run-instrs
  [initial-state instrs]
  (loop [state initial-state]
    (if (>= (:instr state) (count instrs))
      state
      (let [instr (:instr state)
            opcode (nth instrs instr)
            operand (nth instrs (inc instr))
            new-state (do-op state opcode operand)]
        ;(println new-state)
        (recur new-state)))))

(defn part1
  [{:keys [a b c program]
    :as opts}]
  (let [initial-state {:a a :b b :c c :instr 0 :output []}
        instrs (mapv parse-long (str/split program #","))
        final-state (run-instrs initial-state instrs)]
    (println final-state)
    (println (str/join "," (map str (:output final-state))))))

(defn part2 [opts]
  ;; we need at least 10 bits to run the full eval to output.
  (let [xs (mapv long (range 1024))
        ;; hand unrolled execution of each iteration of the program, where 's'
        ;; is increased by 3 at each program iteration.
        eval-a (fn [x s] (bit-shift-right x s))
        eval-b (fn [x s] (bit-xor (bit-and (eval-a x s) 2r111) 2r001))
        eval-c (fn [x s] (bit-shift-right (eval-a x s) (eval-b x s)))
        eval-out (fn [x s] (bit-and (bit-xor (bit-xor (eval-b x s) (eval-c x s)) 4) 2r111))
        instrs (mapv parse-long (str/split "2,4,1,1,7,5,4,7,1,4,0,3,5,5,3,0" #","))
        a-candidates (loop [candidates (set xs)
                            shift-by 0
                            outputs instrs]
                       (if (empty? outputs)
                         (filter #(= (eval-a % shift-by) 0) candidates)
                         (let [matches (filter #(= (eval-out % shift-by) (first outputs)) candidates)
                               new-candidates (reduce (fn [cs c]
                                                        (into cs 
                                                              (map #(bit-or (bit-shift-left % (+ 10 shift-by)) c) (range 8))))
                                                      #{}
                                                      matches)]
                           (recur new-candidates (+ 3 shift-by) (rest outputs)))))]
    (println (apply min a-candidates))))
