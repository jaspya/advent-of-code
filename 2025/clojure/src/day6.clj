(ns day6
  (:require [aoc :refer [read-lines]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(re-seq #"[\d+*]+" %))
       (map (fn [xs] (map #(or (parse-long %) (case % "+" + *)) xs)))))

(defn part1
  [input]
  (->> (parse-data input)
       aoc/transpose
       (map #(apply (last %) (butlast %)))
       (reduce +)))

(defn part2
  [input]
  (let [data (read-lines input)
        ops (->> (last data) (re-seq #"[+*]") (map #(case % "+" + *)))]
    (->> (butlast data)
         aoc/transpose
         (map (comp parse-long str/trim (partial apply str)))
         (partition-by nil?)
         (remove (comp nil? first))
         (map #(apply %1 %2) ops)
         (reduce +))))

(deftest cafeteria
  (is (= 4277556 (part1 "resources/day6.example")))
  (is (= 4076006202939 (part1 "resources/day6.input")))
  (is (= 3263827 (part2 "resources/day6.example")))
  (is (= 7903168391557 (part2 "resources/day6.input"))))
