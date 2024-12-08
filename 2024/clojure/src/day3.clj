(ns day3)

(comment
  ;; Part 1
  (->> (slurp "resources/aoc2024/day3.input")
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map (comp #(apply * %) #(map parse-long %) rest))
       (reduce +)))

(comment
  ;; Part 2
  (->> (slurp "resources/aoc2024/day3.input")
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don\'t\(\)")
       (reduce (fn [acc [instruction a b]]
                 (cond
                   (= instruction "don't()") (assoc acc :enabled false)
                   (= instruction "do()") (assoc acc :enabled true)
                   (:enabled acc) (update acc :val + (* (parse-long a) (parse-long b)))
                   :else acc))
               {:val 0 :enabled true})
       :val))
