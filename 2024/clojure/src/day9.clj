(ns day9
  (:require
   [aoc :refer [read-lines]]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn parse-data
  [input]
  (->> (slurp input)
       (map Character/getNumericValue)
       (partition 2 2 nil)
       (map-indexed (fn [idx [file empty]]
                      [(repeat file idx)
                       (repeat (or empty 0) \.)]))
       flatten
       vec))

(defn part1
  [input]
  (let [disk (parse-data input)]
    (loop [x (first disk)
           xs (subvec disk 1)
           idx 0
           checksum 0
           calcs []]
      (if (zero? (count xs))
        (if (= x \.)
          checksum
          (+ checksum (* idx x)))
        (if (= x \.)
          (recur (peek xs) (pop xs) idx checksum calcs)
          (recur (first xs) (subvec xs 1) (inc idx) (+ checksum (* idx x)) (conj calcs [idx x])))))))

(defn part2
  [input]
  (read-lines input))

(deftest disk-fragmenter
  (is (= 6337367222422 (part1 "resources/day9.input"))
      "Part 1: What is the resulting filesystem checksum?")
  #_(is (= 1 (part2 "resources/day9.input"))
        "Part 2: "))

(comment
  (c/quick-bench (part1 "resources/day9.input")) ;; 80.87 ms
  (c/quick-bench (part2 "resources/day9.input")))
