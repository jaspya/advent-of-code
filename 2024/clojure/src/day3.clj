(ns day3
  (:require
   [clojure.test :refer [deftest is]]))

(defn part1
  [input]
  (->> (slurp input)
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map (comp #(apply * %) #(map parse-long %) rest))
       (reduce +)))

(defn part2
  [input]
  (->> (slurp input)
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don\'t\(\)")
       (reduce (fn [acc [instruction a b]]
                 (cond
                   (= instruction "don't()") (assoc acc :enabled false)
                   (= instruction "do()") (assoc acc :enabled true)
                   (:enabled acc) (update acc :val + (* (parse-long a) (parse-long b)))
                   :else acc))
               {:val 0 :enabled true})
       :val))

(deftest mull-it-over
  (is (= 192767529 (part1 "resources/day3.input"))
      "Part 1: What do you get if you add up all of the results of the multiplications?")
  (is (= 104083373 (part2 "resources/day3.input"))
      "Part 2: What do you get if you add up all of the results of just the enabled multiplications?"))
