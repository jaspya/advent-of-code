(ns day1
  (:require [aoc :refer [read-lines]]
            [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(re-find #"(R|L)(\d+)" %))
       (map (fn [[_ dir amount]]
              (cond-> (parse-long amount)
                (= dir "L") -)))))

(defn part1
  [input]
  (->> (parse-data input)
       (reduce (fn [[dial zeroes] amount]
                 (let [dial (mod (+ dial amount) 100)]
                   [dial (if (zero? dial) (inc zeroes) zeroes)]))
               [50 0])
       last))

(defn part2
  [input]
  (->> (parse-data input)
       (reduce (fn [[dial zeroes] amount]
                 (let [raw-dial (+ dial amount)
                       times (cond-> (abs (quot raw-dial 100))
                               (and (<= raw-dial 0) (not (zero? dial))) inc)
                       new-dial (mod raw-dial 100)]
                   [new-dial (+ zeroes times)]))
               [50 0])
       last))

(deftest secret-entrance
  (is (= 3 (part1 "resources/day1.example")))
  (is (= 1180 (part1 "resources/day1.input")))
  (is (= 6 (part2 "resources/day1.example")))
  (is (= 6892 (part2 "resources/day1.input"))))
