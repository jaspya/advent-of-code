(ns day25
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse-input
  [input]
  (->> (re-seq #"[#|.]+" (slurp input))
       (partition 7)
       (reduce
        (fn [data lock-or-key]
          (let [heights (->> (apply mapv vector lock-or-key)
                             (map #(dec (count (keep #{\#} %)))))]
            (if (every? #(= % \#) (first lock-or-key))
              (update data :locks conj heights)
              (update data :keys conj heights))))
        {:locks #{}
         :keys #{}})))

(defn part1
  [input]
  (let [{:keys [locks keys]} (parse-input input)]
    (->> (for [lock locks key keys] (map + lock key))
         (filter (fn [xs] (every? #(< % 6) xs)))
         count)))

(deftest code-chronicle-part1
  (is (= 3525 (part1 "resources/day25.input"))
      "Part 1: How many unique lock/key pairs fit together without overlapping in any column?"))

(deftest code-chronicle-part2
  (is true "Part 2: Merry Christmas"))
