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
  [pattern-map design]
  (loop [current 0
         next-indexes #{}
         index-counts {0 1}]
    (let [part (subs design current)
          patterns (get pattern-map (first part))
          counts (keep (partial pattern-count part) patterns)
          new-set (set (map (partial + current) counts))]
      (if (= part "")
        (get index-counts current)
        (if-let [[next & indexes] (seq (sort (set/union next-indexes new-set)))]
          (recur next (set indexes) (add-index-counts index-counts current new-set))
          0)))))

(defn find-combinations
  [input]
  (let [lines (str/split-lines (slurp input))
        pattern-map (-> (group-by first (str/split (first lines) #", "))
                        (update-vals set))]
    (pmap (partial count-combinations pattern-map) (drop 2 lines))))

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
  (c/quick-bench (part1 "resources/day19.input")) ;; 29.9 ms
  (c/quick-bench (part2 "resources/day19.input"))) ;; 30.9 ms
