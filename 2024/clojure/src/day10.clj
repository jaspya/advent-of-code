(ns day10
  (:require
   [aoc :refer [read-lines]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(str/split % #""))
       (mapv #(mapv parse-long %))))

(def directions
  [[-1 0] [0 1] [1 0] [0 -1]])

(defn find-trailheads
  [board height position]
  (when (= (get-in board position) height)
    (if (= height 9)
      [position]
      (->> (map #(mapv + position %) directions)
           (mapcat #(find-trailheads board (inc height) %))))))

(defn part1
  [input]
  (let [board (parse-data input)]
    (->> (for [y (range (count board))
               x (range (count (first board)))]
           (count (set (find-trailheads board 0 [y x]))))
         (reduce +))))

(defn part2
  [input]
  (let [board (parse-data input)]
    (->> (for [y (range (count board))
               x (range (count (first board)))]
           (count (find-trailheads board 0 [y x])))
         (reduce +))))

(deftest hoof-it
  (is (= 698 (part1 "resources/day10.input"))
      "Part 1: What is the sum of the scores of all trailheads on your topographic map?")
  (is (= 1436 (part2 "resources/day10.input"))
      "Part 2: What is the sum of the ratings of all trailheads?"))

(comment
  (c/quick-bench (part1 "resources/day10.input")) ;; 11.14 ms
  (c/quick-bench (part2 "resources/day10.input"))) ;; 11.74 ms
