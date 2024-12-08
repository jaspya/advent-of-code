(ns day7
  (:require
   [aoc :refer [read-lines]]
   [clojure.math :as math]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(str/split % #": | "))
       (map #(mapv parse-long %))))

(defn possible-remainders
  [current next]
  (let [division (/ current next)
        subtraction (- current next)]
    (cond-> []
      (int? division)
      (conj division)

      (nat-int? subtraction)
      (conj subtraction))))

(defn calculate-backwards
  [remainder-fn current [next & xs]]
  (if (pos-int? current)
    (and next (some #(calculate-backwards remainder-fn % xs) (remainder-fn current next)))
    (nil? next)))

(defn solve
  [input remainder-fn]
  (->> (parse-data input)
       (keep (fn [[target & xs]] 
               (when (calculate-backwards remainder-fn target (reverse xs)) 
                 target)))
       (reduce +)))

(defn part1
  [input]
  (solve input possible-remainders))

(defn possible-remainders*
  [current next]
  (let [next-str (str next)
        division (/ current next)
        subtraction (- current next)]
    (cond-> []
      (str/ends-with? (str current) next-str)
      (conj (quot current (int (math/pow 10 (count next-str)))))

      (int? division)
      (conj division)

      (nat-int? subtraction)
      (conj subtraction))))

(defn part2
  [input]
  (solve input possible-remainders*))

(deftest bridge-repair
  (is (= 5837374519342 (part1 "resources/day7.input"))
      "Part 1: What is their total calibration result?")
  (is (= 492383931650959 (part2 "resources/day7.input"))
      "Part 2: What is their total calibration result?"))

(comment
  (c/quick-bench (part1 "resources/day7.input")) ;; 2.35 ms
  (c/quick-bench (part2 "resources/day7.input"))) ;; 2.90 ms - 2,600 times faster than initial solution

(comment
  (calculate-backwards possible-remainders* 156 (reverse [15 6]))
  (calculate-backwards possible-remainders* 7290 (reverse [6 8 6 15]))
  (calculate-backwards possible-remainders* 192 (reverse [17 8 14]))
  (calculate-backwards possible-remainders* 21037 (reverse [9 7 18 13]))
  (calculate-backwards possible-remainders* 5574 (reverse [2 7 31 9 2 5 63 2 2 912])))

(defn calculate
  [current [x & xs]]
  (if-not x
    (list current)
    (concat (calculate (+ current x) xs)
            (calculate (* current x) xs))))

(comment
  (calculate 10 [19])
  (calculate 81 [40 27])

  ;; Part 1 - 5837374519342 - 135.78 ms
  (->> (parse-data "resources/aoc2024/day7.input")
       (keep (fn [[target x & xs]] (some #{target} (calculate x xs))))
       (reduce +)))

(defn calculate*
  [current [x & xs]]
  (if-not x
    (list current)
    (concat (calculate* (+ current x) xs)
            (calculate* (* current x) xs)
            (calculate* (parse-long (str current x)) xs))))

(comment
  (calculate* 15 [6])
  (calculate* 6 [8 6 15])
  (calculate* 17 [8 14])

  ;; Part 2 - 492383931650959 - 10.18 sec
  (->> (parse-data "resources/aoc2024/day7.input")
       (keep (fn [[target x & xs]] (some #{target} (calculate* x xs))))
       (reduce +)))

(defn filtered-calculate*
  [target current [x & xs]]
  (if (< target current)
    (list)
    (if-not x
      (list current)
      (concat (filtered-calculate* target (+ current x) xs)
              (filtered-calculate* target (* current x) xs)
              (filtered-calculate* target (parse-long (str current x)) xs)))))

(comment
  (filtered-calculate* 156 15 [6])
  (filtered-calculate* 7290 6 [8 6 15])
  (filtered-calculate* 192 17 [8 14])

  ;; Part 2 - try 2 - 492383931650959 - 4.39 sec
  ;; Abort when too high
  (->> (parse-data "resources/aoc2024/day7.input")
       (keep (fn [[target x & xs]] (some #{target} (filtered-calculate* target x xs))))
       (reduce +)))
