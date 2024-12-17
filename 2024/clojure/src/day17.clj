(ns day17 
  (:require
   [clojure.math :as math]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn parse-data
  [path]
  (->> (slurp path)
       (re-seq #"\d+")
       (map parse-long)))

(defn initial-state
  [[a b c & program]]
  {:a a
   :b b
   :c c
   :program (vec program)
   :pointer 0
   :output []})

(defn combo-operand
  [{:keys [a b c]} operand]
  (case operand 4 a 5 b 6 c operand))

(defn adv
  [{:keys [a] :as state} _operand combo-operand]
  (assoc state :a (quot a (int (math/pow 2 combo-operand)))))

(defn bxl
  [{:keys [b] :as state} operand _combo-operand]
  (assoc state :b (bit-xor b operand)))

(defn bst
  [state _operand combo-operand]
  (assoc state :b (mod combo-operand 8)))

(defn jnz
  [{:keys [a] :as state} operand _combo-operand]
  (if (zero? a)
    state
    (assoc state :pointer operand)))

(defn bxc
  [{:keys [b c] :as state} _operand _combo-operand]
  (assoc state :b (bit-xor b c)))

(defn out
  [state _operand combo-operand]
  (update state :output conj (mod combo-operand 8)))

(defn bdv
  [{:keys [a] :as state} _operand combo-operand]
  (assoc state :b (quot a (int (math/pow 2 combo-operand)))))

(defn cdv
  [{:keys [a] :as state} _operand combo-operand]
  (assoc state :c (quot a (int (math/pow 2 combo-operand)))))

(def get-instruction
  {0 adv, 1 bxl, 2 bst, 3 jnz, 4 bxc, 5 out, 6 bdv, 7 cdv})

(defn process-instruction
  [{:keys [program pointer] :as state}]
  (let [operand (get program (inc pointer))]
    (if-let [instruction (get-instruction (get program pointer))]
      (-> state
          (assoc :pointer (+ pointer 2))
          (instruction operand (combo-operand state operand)))
      state)))

(defn run
  [initial-state]
  (let [{:keys [program] :as initial-state} initial-state
        end-pointer (count program)]
    (loop [{:keys [pointer] :as state} initial-state]
      (if (< pointer end-pointer)
        (recur (process-instruction state))
        (:output state)))))

(defn part1
  [input]
  (str/join "," (run (initial-state (parse-data input)))))

(deftest chronospatial-computer-part1
  (is (= "2,7,6,5,6,0,2,3,1" (part1 "resources/day17.input"))
      "Part 1: What do you get if you use commas to join the values it output into a single string?"))

(defn next-a
  [start-a state expected]
  (some #(when (= (run (assoc state :a %)) expected) %) (iterate inc start-a)))

(defn find-lowest-a
  [state expected-program]
  (if (= (count expected-program) 1)
    (next-a 1 state expected-program)
    (-> (* 8 (find-lowest-a state (rest expected-program)))
        (next-a state expected-program))))

(defn part2
  [input]
  (let [{:keys [program] :as state} (initial-state (parse-data input))]
    (find-lowest-a state program)))

(deftest chronospatial-computer-part2
  (is (= 107416870455451 (part2 "resources/day17.input"))
      "Part 1: What is the lowest positive initial value for register A that causes the program to output a copy of itself?"))

(comment
  (c/quick-bench (part1 "resources/day17.input")) ;; 23 µs
  (c/quick-bench (part2 "resources/day17.input"))) ;; 1.12 sec
