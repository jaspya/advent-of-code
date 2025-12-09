(ns day9
  (:require [aoc :refer [read-lines]]
            [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(map parse-long (re-seq #"\d+" %)))))

(defn largest-rect
  [pts filter-fn]
  (->> pts
       (mapcat (fn [p1]
                 (->> pts
                      (filter #(filter-fn p1 %))
                      (keep #(aoc/area p1 %)))))
       sort
       last))

(defn part1
  [input]
  (largest-rect (parse-data input) (constantly true)))

(defn part2
  [input]
  (let [pts (parse-data input)
        poly (aoc/->polygon pts)]
    (largest-rect pts #(.contains poly (aoc/->rectangle %1 %2)))))

(deftest movie-theater
  (is (= 50 (part1 "resources/day9.example")))
  (is (= 4744899849 (part1 "resources/day9.input")))
  (is (= 24 (part2 "resources/day9.example")))
  (is (= 1540192500 (part2 "resources/day9.input")))) ;; 538 ms
