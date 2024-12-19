(ns day19
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn add-index-counts
  [index-counts index new-indexes]
  (let [current (get index-counts index)]
    (reduce
     (fn [acc itm]
       (update acc itm #(+ (or % 0) current)))
     index-counts
     new-indexes)))

(defn pattern-count
  [part pattern]
  (when (str/starts-with? part pattern) (count pattern)))

(defn count-combinations
  [patterns design]
  (loop [current 0
         next-indexes #{}
         index-counts {0 1}]
    (let [part (subs design current)
          new-set (set (map (partial + current) (keep (partial pattern-count part) patterns)))]
      (if (= part "")
        (get index-counts current)
        (if-let [[next & indexes] (seq (sort (set/union next-indexes new-set)))]
          (recur next (set indexes) (add-index-counts index-counts current new-set))
          0)))))

(defn find-combinations
  [input]
  (let [lines (str/split-lines (slurp input))
        patterns (set (str/split (first lines) #", "))]
    (pmap (partial count-combinations patterns) (drop 2 lines))))

(defn part1 
  [input]
  (count (remove zero? (find-combinations input))))

(deftest linen-layout-part1
  (is (= 336 (part1 "resources/day19.input"))
      "Part 1: How many designs are possible?"))

(defn part2 
  [input]
  (reduce + (find-combinations input)))

(deftest linen-layout-part2
  (is (= 758890600222015 (part2 "resources/day19.input"))
      "Part 2: What do you get if you add up the number of different ways you could make each design?"))

(comment
  (c/quick-bench (part1 "resources/day19.input")) ;; 156 ms
  (c/quick-bench (part2 "resources/day19.input"))) ;; 147 ms
