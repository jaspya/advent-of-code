(ns day8
  (:require
   [aoc :refer [read-lines]]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn pairs
  [[x & xs]]
  (if (seq xs)
    (concat (map (partial vector x) xs) (pairs xs))
    (list)))

(defn location->coords
  [cols location]
  [(quot location cols)
   (mod location cols)])

(defn on-map?
  [rows cols [y x]]
  (and (<= 0 y (dec rows))
       (<= 0 x (dec cols))))

(defn antinodes
  [rows cols [l1 l2]]
  (let [[y1 x1] (location->coords cols l1)
        [y2 x2] (location->coords cols l2)
        y-distance (- y2 y1)
        x-distance (- x2 x1)
        an1 [(- y1 y-distance) (- x1 x-distance)]
        an2 [(+ y2 y-distance) (+ x2 x-distance)]]
    (cond-> []
      (on-map? rows cols an1) (conj an1)
      (on-map? rows cols an2) (conj an2))))

(defn solve
  [input antinode-fn]
  (let [lines (read-lines input)
        rows (count lines)
        cols (count (first lines))
        data (apply concat lines)]
    (->> data
         (map-indexed vector)
         (remove #(= (last %) \.))
         (reduce (fn [acc [location id]]
                   (update acc id conj location))
                 {})
         vals
         (mapcat (comp pairs sort))
         (mapcat (partial antinode-fn rows cols))
         set
         count)))

(defn part1
  [input]
  (solve input antinodes))

(defn next-nodes
  [[y x] rows cols y-distance x-distance]
  (let [node [(+ y y-distance) (+ x x-distance)]]
    (if (on-map? rows cols node)
      (cons node (next-nodes node rows cols y-distance x-distance))
      (list))))

(defn resonant-antinodes
  [rows cols [l1 l2]]
  (let [[y1 x1] (location->coords cols l1)
        [y2 x2] (location->coords cols l2)
        y-distance (- y2 y1)
        x-distance (- x2 x1)]
    (concat [[y1 x1] [y2 x2]]
            (next-nodes [y1 x1] rows cols (- y-distance) (- x-distance))
            (next-nodes [y2 x2] rows cols y-distance x-distance))))

(defn part2
  [input]
  (solve input resonant-antinodes))

(deftest resonant-collinearity
  (is (= 291 (part1 "resources/day8.input"))
      "Part 1: How many unique locations within the bounds of the map contain an antinode?")
  (is (= 1015 (part2 "resources/day8.input"))
      "Part 2: How many unique locations within the bounds of the map contain an antinode?"))

(comment
  (c/quick-bench (part1 "resources/day8.input")) ;; 0.62 ms
  (c/quick-bench (part2 "resources/day8.input"))) ;; 0.92 ms
