(ns day13
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn point-of-intersection
  "Formula from: https://www.geeksforgeeks.org/point-of-intersection-of-two-lines-formula/
   Point of Intersection (x, y) = ((b1*c2 - b2*c1)/(a1*b2 - a2*b1), (c1*a2 - c2*a1)/(a1*b2 - a2*b1))"
  [[a1 a2 b1 b2 c1 c2]]
  (let [c1 (- c1)
        c2 (- c2)]
    [(/ (- (* b1 c2) (* b2 c1))
        (- (* a1 b2) (* a2 b1)))
     (/ (- (* c1 a2) (* c2 a1))
        (- (* a1 b2) (* a2 b1)))]))

(defn price
  [[a b]]
  (if (and (int? a) (int? b))
    (+ (* a 3) b)
    0))

(defn part1
  [input]
  (->> (slurp input)
       (re-seq #"\d+")
       (map parse-long)
       (partition 6)
       (map (comp price point-of-intersection))
       (reduce +)))

(defn increase-difficulty
  [[x1 y1 x2 y2 px py]]
  [x1 y1 x2 y2 (+ px 10000000000000) (+ py 10000000000000)])

(defn part2
  [input]
  (->> (slurp input)
       (re-seq #"\d+")
       (map parse-long)
       (partition 6)
       (map increase-difficulty)
       (map (comp price point-of-intersection))
       (reduce +)))

(deftest claw-contraption
  (is (= 38714 (part1 "resources/day13.input"))
      "Part 1: What is the fewest tokens you would have to spend to win all possible prizes?")
  (is (= 74015623345775 (part2 "resources/day13.input"))
      "Part 2: What is the fewest tokens you would have to spend to win all possible prizes?"))

(comment
  (c/quick-bench (part1 "resources/day13.input")) ;; 576 µs
  (c/quick-bench (part2 "resources/day13.input"))) ;; 609 µs

(comment
  ;; Initial solution

  (defn fewest-tokens
    [results]
    (if (seq results)
      (apply min results)
      0))

  (defn solve
    [[x1 y1 x2 y2 xr yr]]
    (->> (for [c1 (range 100)
               c2 (range 100)]
           (when (and (= (+ (* x1 c1) (* x2 c2)) xr)
                      (= (+ (* y1 c1) (* y2 c2)) yr))
             (+ (* c1 3) c2)))
         (remove nil?)
         fewest-tokens))

  (defn part1
    [input]
    (->> (slurp input)
         (re-seq #"\d+")
         (map parse-long)
         (partition 6)
         (map solve)
         (reduce +))))
