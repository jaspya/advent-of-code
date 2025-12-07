(ns day4
  (:require [aoc :refer [read-lines]]
            [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (mapv #(mapv identity %))))

(def directions
  [[-1 -1] [0 -1] [1 -1]
   [-1 0] [1 0]
   [-1 1] [0 1] [1 1]])

(defn part1
  [input]
  (let [board (parse-data input)]
    (->> (for [y (range (count board))
               x (range (count (first board)))
               :when (= (get-in board [y x]) \@)]
           (reduce (fn [acc itm]
                     (let [neighbour (get-in board (mapv + [y x] itm))]
                       (if (= neighbour \@)
                         (inc acc)
                         acc)))
                   0 directions))
         (filter (partial > 4))
         count)))

(defn next-board
  [board]
  (vec
   (pmap (fn [y]
           (vec
            (for [x (range (count (first board)))]
              (let [point (get-in board [y x])]
                (if (= point \@)
                  (if (< (reduce (fn [acc itm]
                                   (let [neighbour (get-in board (mapv + [y x] itm))]
                                     (if (= neighbour \@)
                                       (inc acc)
                                       acc)))
                                 0 directions)
                         4)
                    \X
                    point)
                  point)))))
         (range (count board)))))

(defn score
  [board]
  (frequencies (flatten board)))

(defn part2
  [input]
  (let [board (parse-data input)]
    (loop [board board]
      (let [next (next-board board)]
        (if (= (score board) (score next))
          (get (score board) \X)
          (recur next))))))

(deftest printing-department
  (is (= 13 (part1 "resources/day4.example")))
  (is (= 1433 (time (part1 "resources/day4.input"))))
  (is (= 43 (part2 "resources/day4.example")))
  (is (= 8616 (time (part2 "resources/day4.input"))))) ;; 1.4 s
