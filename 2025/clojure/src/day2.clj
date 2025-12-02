(ns day2
  (:require [aoc :refer [read-lines]]
            [clojure.math :as math]
            [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (first (read-lines path))
       (re-seq #"\d+")
       (partition 2)))

(defn repeating-numbers
  [[x y] divider]
  (let [x' (parse-long (subs x 0 (quot (count x) divider)))
        y' (parse-long (subs y 0 (math/ceil (/ (count y) divider))))]
    (->> (range (or x' 0) (inc y'))
         (map #(parse-long (apply str (repeat divider %))))
         (filter #(<= (parse-long x) % (parse-long y))))))

(defn part1
  [input]
  (->> (parse-data input)
       (mapcat #(repeating-numbers % 2))
       (reduce +)))

(defn part2
  [input]
  (->> (parse-data input)
       (mapcat #(set (mapcat (partial repeating-numbers %) (range 2 8))))
       (reduce +)))

(deftest gift-shop
  (is (= 1227775554 (part1 "resources/day2.example")))
  (is (= 24157613387 (part1 "resources/day2.input")))
  (is (= 4174379265 (part2 "resources/day2.example")))
  (is (= 33832678380 (part2 "resources/day2.input"))))
