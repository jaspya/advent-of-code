(ns day7
  (:require
   [aoc :refer [read-lines]]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map (fn [s] (set (keep-indexed #(when (#{\S \^} %2) %1) s))))
       (remove empty?)))

(defn part1
  [input]
  (let [[start & splitters] (parse-data input)]
    (loop [[x & xs] splitters
           beams start
           splits 0]
      (if (nil? x)
        splits
        (recur xs
               (->> beams
                    (mapcat #(if (x %) [(dec %) (inc %)] [%]))
                    set)
               (+ splits (count (set/intersection beams x))))))))

(defn part2
  [input]
  (let [[start & splitters] (parse-data input)]
    (loop [[x & xs] splitters
           beams {(first start) 1}]
      (if (nil? x)
        (reduce + (vals beams))
        (recur xs (->> beams
                       (mapcat (fn [[k v]] (if (x k) [[(dec k) v] [(inc k) v]] [[k v]])))
                       (reduce (fn [acc [k v]] (update acc k (fnil + 0) v)) {})))))))

(deftest laboratories
  (is (= 21 (part1 "resources/day7.example")))
  (is (= 1590 (part1 "resources/day7.input")))
  (is (= 40 (part2 "resources/day7.example")))
  (is (= 20571740188555 (part2 "resources/day7.input"))))
