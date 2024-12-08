(ns day1
  (:require [aoc :refer [read-lines transpose]]
            [clojure.string :as str]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(str/split % #"   "))
       (map #(map parse-long %))))

(comment
  ;; Initial solution
  (let [data (parse-data "resources/aoc2024/day1a_input")]
    (->> (map #(abs (- %2 %1))
              (sort (map first data))
              (sort (map last data)))
         (reduce +)))

  ;; With transpose
  (->> (parse-data "resources/aoc2024/day1a_input")
       transpose
       (map sort)
       (apply map #(abs (- %2 %1)))
       (reduce +)))

(comment
  ;; Part 2
  (let [[left right] (transpose (parse-data "resources/aoc2024/day1a_input"))
        freqs (frequencies right)]
    (reduce (fn [score num] (+ score (* num (freqs num 0)))) 0 left)))
