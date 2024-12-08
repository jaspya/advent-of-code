(ns day1
  (:require
   [aoc :refer [read-lines transpose]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(str/split % #"   "))
       (map #(map parse-long %))))

(defn part1
  [input]
  (->> (parse-data input)
       transpose
       (map sort)
       (apply map #(abs (- %2 %1)))
       (reduce +)))

(defn part2
  [input]
  (let [[left right] (transpose (parse-data input))
        freqs (frequencies right)]
    (reduce (fn [score num] (+ score (* num (freqs num 0)))) 0 left)))

(deftest historian-hysteria
  (is (= 1579939 (part1 "resources/day1.input"))
      "Part 1: What is the total distance between your lists?")
  (is (= 20351745 (part2 "resources/day1.input"))
      "Part 2: What is their similarity score?"))

(comment
  ;; Part 1: Initial solution
  (defn part1
    [input]
    (let [data (parse-data input)]
      (->> (map #(abs (- %2 %1))
                (sort (map first data))
                (sort (map last data)))
           (reduce +)))))
