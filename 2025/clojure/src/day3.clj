(ns day3
  (:require [aoc :refer [read-lines]]
            [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(map (comp parse-long str) %))))

(defn remove-first-less-than-next
  [numbers]
  (loop [[x & xs] numbers
         result []]
    (cond
      (empty? xs) result
      (> (first xs) x) (concat result xs)
      :else (recur xs (conj result x)))))

(defn solve-bank
  [numbers bank]
  (let [xs (reverse bank)]
    (->> (reduce (fn [acc itm]
                   (if (>= itm (first acc))
                     (into [itm] (remove-first-less-than-next acc))
                     acc))
                 (vec (reverse (take numbers xs)))
                 (drop numbers xs))
         (apply str)
         parse-long)))

(defn solve
  [numbers input]
  (->> (parse-data input)
       (map (partial solve-bank numbers))
       (reduce +)))

(def part1 (partial solve 2))
(def part2 (partial solve 12))

(deftest lobby
  (is (= 357 (part1 "resources/day3.example")))
  (is (= 17694 (part1 "resources/day3.input")))
  (is (= 3121910778619 (part2 "resources/day3.example")))
  (is (= 175659236361660 (part2 "resources/day3.input"))))
