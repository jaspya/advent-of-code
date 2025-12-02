(ns day2
  (:require [aoc :refer [read-lines]]
            [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (first (read-lines path))
       (re-seq #"\d+")
       (map parse-long)
       (partition 2)))

(defn part1
  [input]
  (->> (parse-data input)
       (map (fn [[x y]]
              (->> (range x (inc y))
                   (filter #(> % 10))
                   (reduce (fn [acc itm]
                             (let [a (str itm)
                                   half (quot (count a) 2)
                                   b (set (map (partial apply str) (partition half a)))]
                               (if (and (even? (count a)) (= (count b) 1))
                                 (+ acc itm)
                                 acc)))
                           0))))
       (reduce +)))

(defn find-divisors
  [number]
  (let [length (count number)
        possibles (range 1 (inc (quot length 2)))]
    (filter #(= (quot length %) (/ length %)) possibles)))

(defn check-parts
  [itm divisor]
  (let [a (str itm)
        b (set (map (partial apply str) (partition divisor a)))]
    (when (= (count b) 1) itm)))

(defn part2
  [input]
  (->> (parse-data input)
       (map (fn [[x y]]
              (->> (range x (inc y))
                   (filter #(> % 10))
                   (reduce (fn [acc itm]
                             (let [a (str itm)
                                   divisors (find-divisors a)
                                   numbers (keep (partial check-parts itm) divisors)]
                               (reduce + acc (set numbers))))
                           0))))
       (reduce +)))

(deftest gift-shop
  (is (= 1227775554 (part1 "resources/day2.example")))
  (is (= 24157613387 (part1 "resources/day2.input"))) ;; 2853 ms
  (is (= 4174379265 (part2 "resources/day2.example")))
  (is (= 33832678380 (part2 "resources/day2.input")))) ;; 11438 ms
