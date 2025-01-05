(ns day1
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]
   [grid :as g]
   [input :as i]))

(defn turn
  [direction rotation]
  (if (= rotation "R") 
    (g/turn-left direction) 
    (g/turn-right direction)))

(defn part1
  [input]
  (->> (i/read input :pattern #"([LR])(\d+)")
       (reduce (fn [[location dir] [rotation dist]]
                 (let [d (turn dir rotation)]
                   [(g/move location d dist) d]))
               [g/origin g/north])
       first
       (g/manhattan-distance g/origin)))

(deftest no-time-for-a-taxicab-part1
  (is (= 291 (part1 "resources/day1.input"))))

(comment
  (c/quick-bench (part1 "resources/day1.input"))) ;; 654 µs

(defn find-first-intersection
  [instructions]
  (loop [location g/origin
         direction g/north
         visited #{}
         [[rotation dist] & xs] instructions]
    (let [d (turn direction rotation)
          steps (map #(g/move location d %) (range 1 (inc dist)))
          step-set (set steps)]
      (if-let [intersect (seq (set/intersection visited step-set))]
        (first intersect)
        (recur (last steps) d (set/union visited step-set) xs)))))

 (defn part2
  [input]
  (->> (i/read input :pattern #"([LR])(\d+)")
       find-first-intersection
       (g/manhattan-distance g/origin)))

(deftest no-time-for-a-taxicab-part2
  (is (= 159 (part2 "resources/day1.input"))))
 
(comment
  (c/quick-bench (part2 "resources/day1.input"))) ;; 1.24 ms
