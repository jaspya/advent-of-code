(ns day14
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]
   [data-viz :as data-viz]))

(defn parse-data
  [path]
  (->> (slurp path)
       (re-seq #"-*\d+")
       (map parse-long)
       (partition 4)))

(defn move-robots
  [cols rows steps data]
  (map (fn [[x y dx dy]]
         [(mod (+ x (* dx steps)) cols)
          (mod (+ y (* dy steps)) rows)])
       data))

(defn quadrant
  [center-col center-row [x y]]
  (if (< x center-col)
    (if (< y center-row) 0 2)
    (if (< y center-row) 1 3)))

(defn safety-factor
  [cols rows data]
  (let [center-col (quot cols 2)
        center-row (quot rows 2)]
    (->> data
         (remove (comp (partial = center-col) first))
         (remove (comp (partial = center-row) last))
         (group-by (partial quadrant center-col center-row))
         vals
         (map count)
         (reduce *))))

(defn part1
  [input]
  (let [cols 101 rows 103 steps 100]
    (->> (parse-data input)
         (move-robots cols rows steps)
         (safety-factor cols rows))))

(let [data (parse-data "resources/day14.input")]
  (defn data-fn
    [idx]
    (move-robots 101 103 idx data)))

(defn part2
  [_input]
  (+ 28 (* 101 75)))

(deftest restroom-redoubt
  (is (= 224438715 (part1 "resources/day14.input"))
      "Part 1: What will the safety factor be after exactly 100 seconds have elapsed?")
  (is (= 7603 (part2 "resources/day14.input"))
      "Part 2: What is the fewest number of seconds that must elapse for the robots to display the Easter egg?"))

(comment
  (c/quick-bench (part1 "resources/day14.input")) ;; 706 µs
  (c/quick-bench (part2 "resources/day14.input"))) ;; N/A

(comment
  ;; Initial solution
  ;; Runtime: 4.55 ms 

  (let [my 103 #_7 mx 101 #_11
        cmy (quot my 2)
        cmx (quot mx 2)]
    (->> (reduce (fn [acc _itm]
                   (->> acc
                        (map #(let [[x y dx dy] %]
                                [(mod (+ x dx) mx) (mod (+ y dy) my) dx dy]))))
                 (parse-data "resources/day14.input")
                 (range 100))
         (map #(subvec % 0 2))
         (remove #(or (= (first %) cmx) (= (last %) cmy)))
         (reduce (fn [acc [x y]]
                   (update acc (if (< x cmx)
                                 (if (< y cmy)
                                   0
                                   2)
                                 (if (< y cmy)
                                   1
                                   3))
                           #(inc (or % 0))))
                 {})
         vals
         (reduce *))))

(comment
  ;; Data artefact at every 101 frames starting at frame 28
  ;; Found Christmas tree at 75th data artefact
  ;; So 28 + 75 * 101 = 7603
  (data-viz/start)
  (data-viz/draw data-fn {:grid-rows 103
                          :grid-cols 101
                          :panel-cols 3
                          :panel-rows 2
                          :panel-max 1
                          :start-index 0
                          :step-count 1
                          :as-board false
                          :as-points false
                          :colors {}})
  (data-viz/draw data-fn {:grid-rows 103
                          :grid-cols 101
                          :panel-cols 20
                          :panel-rows 12
                          :start-index 0
                          :step-count 1
                          :as-board false
                          :as-points false
                          :colors {}})
  (data-viz/draw data-fn {:grid-rows 103
                          :grid-cols 101
                          :panel-cols 20
                          :panel-rows 12
                          :start-index 28
                          :step-count 101
                          :as-board false
                          :as-points false
                          :colors {}}))
