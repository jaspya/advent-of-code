(ns day11
  (:require [aoc :refer [read-lines]]
            [clojure.test :refer [deftest is]]))

(defn memoize-recursive
  [f]
  (let [mem-f (memoize f)]
    (partial mem-f mem-f)))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(re-seq #"\w+" %))
       (reduce #(assoc %1 (first %2) (rest %2)) {})))

(defn paths-to-out
  [f device required devices]
  (->> (get devices device)
       (map #(if (= % "out")
               (if (empty? required) 1 0)
               (f f % (disj required device) devices)))
       (reduce +)))

(defn count-paths
  [device required input]
  ((memoize-recursive paths-to-out) device required (parse-data input)))

(def part1 (partial count-paths "you" #{}))

(def part2 (partial count-paths "svr" #{"dac" "fft"}))

(deftest reactor
  (is (= 5 (part1 "resources/day11.example")))
  (is (= 506 (part1 "resources/day11.input")))
  (is (= 2 (part2 "resources/day11.example2")))
  (is (= 385912350172800 (part2 "resources/day11.input"))))
