(ns day5
  (:require [aoc :refer [read-lines]]
            [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(map parse-long (re-seq #"\d+" %)))))

(defn part1
  [input]
  (let [[instructions _ ids] (->> (parse-data input)
                                  (partition-by empty?))]
    (->> (map first ids)
         (reduce (fn [acc itm]
                   (if (some (fn [[x y]] (<= x itm y)) instructions)
                     (inc acc)
                     acc))
                 0))))

(defn part2
  [input]
  (let [instructions (->> (parse-data input)
                          (partition-by empty?)
                          first)
        instructions (->> instructions
                          (remove (fn [[x y :as p1]]
                                    (some (fn [[x2 y2 :as p2]]
                                            (and (<= x2 x y y2)
                                                 (not= p1 p2)))
                                          instructions))))] 
    (->> (loop [[x & xs] (sort-by first instructions)
                result []]
           (if (empty? xs)
             (conj result x)
             (let [x* (ffirst xs)
                   y (last x)
                   x [(first x) (if (< y x*) y (dec x*))]]
               (recur xs (conj result x)))))
         (reduce (fn [acc [x y]]
                   (+ acc (inc (- y x))))
                 0))))

(deftest cafeteria
  (is (= 3 (part1 "resources/day5.example")))
  (is (= 529 (part1 "resources/day5.input")))
  (is (= 14 (part2 "resources/day5.example")))
  (is (= 344260049617193 (part2 "resources/day5.input"))))
