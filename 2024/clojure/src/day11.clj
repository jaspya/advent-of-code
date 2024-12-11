(ns day11
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn parse-data
  [path]
  (->> (str/split (str/trim (slurp path)) #" ")
       (mapv parse-long)
       frequencies))

(defn split-number
  [number]
  (let [length (count (str number))]
    (->> (split-at (quot length 2) (str number))
         (mapv (comp parse-long str/join)))))

(defn apply-rules
  [number]
  (let [length (count (str number))]
    (cond
      (zero? number) [1]
      (even? length) (split-number number)
      :else [(* number 2024)])))

(defn apply-rules-for-iteration
  [stones _idx]
  (reduce (fn [m [number stone-count]]
            (let [nexts (frequencies (apply-rules number))]
              (merge-with + m (update-vals nexts (partial * stone-count)))))
          {}
          stones))

(defn solve
  [blink-count input]
  (let [data (parse-data input)]
    (->> (reduce apply-rules-for-iteration data (range blink-count))
         vals
         (reduce +))))

(def part1 (partial solve 25))
(def part2 (partial solve 75))

(deftest plutonian-pebbles
  (is (= 217812 (part1 "resources/day11.input"))
      "Part 1: How many stones will you have after blinking 25 times?")
  (is (= 259112729857522 (part2 "resources/day11.input"))
      "Part 2: How many stones would you have after blinking a total of 75 times?"))

(comment
  (c/quick-bench (part1 "resources/day11.input")) ;; 2.16 ms
  (c/quick-bench (part2 "resources/day11.input"))) ;; 108.20 ms

(comment
  ;; Initial solution - different parse-data
  (defn part1
    [input]
    (let [data (parse-data input)]
      (->> (reduce (fn [stones _] (mapcat apply-rules stones))
                   data
                   (range 25))
           count))))
