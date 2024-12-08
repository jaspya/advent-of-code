(ns day6
  (:require
   [aoc :refer [read-lines]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (read-lines path)))

(defn move-position
  [[y x] direction]
  (case direction
    "N" [(dec y) x]
    "E" [y (inc x)]
    "S" [(inc y) x]
    "W" [y (dec x)]))

(defn do-move
  [board position direction turns]
  (let [[y x :as next-position] (move-position position direction)
        rows (count board)
        cols (count (first board))
        on-board (and (<= 0 y (dec rows)) (<= 0 x (dec cols)))
        blocked (and on-board (= (get-in board next-position) \#))
        turn-vec [position direction]]
    (if (and blocked (contains? turns turn-vec))
      nil
      (if blocked
        (do-move board position (case direction "N" "E" "E" "S" "S" "W" "W" "N") (conj turns turn-vec))
        (if on-board
          [(-> board
               (assoc-in position \X)
               (assoc-in next-position \^))
           next-position
           direction
           turns]
          (assoc-in board position \X))))))

(defn move-to-complete
  [state]
  (loop [local-state state]
    (if (= 4 (count local-state))
      (recur (apply do-move local-state))
      local-state)))

(defn part1
  [input]
  (let [board (->> (parse-data input)
                   (mapv vec))]
    (->> (do-move board [79 87] "N" #{})
         move-to-complete
         (map #(apply str %))
         str/join
         (re-seq #"X")
         count)))

;; (defn part2
;;   [input])

(deftest guard-gallivant
  (is (= 4602 (part1 "resources/day6.input"))
      "Part 1: How many distinct positions will the guard visit before leaving the mapped area?"))
  ;; (is (= 1703 (part2 "resources/day6.input"))
  ;;     "Part 2: How many different positions could you choose for this obstruction?")

(comment
  ;; Part 2 - 1703
  ;; Time: 59.13 sec
  (time
    (let [board (->> (parse-data "resources/aoc2024/day6.input")
                     (mapv vec))]
      (->> (for [y (range (count board))]
             (for [x (range (count (first board)))]
               (let [bboard (update-in board [y x] (fn [c]
                                                     (if (= c \.)
                                                       \#
                                                       c)))]
                 (nil? (move-to-complete (do-move bboard [79 87] "N" #{}))))))
           flatten
           (remove false?)
           count))))

(comment
  ;; Part 2 - try 2 - 1703
  ;; Time: 45.25 sec
  (time
    (let [board (->> (parse-data "resources/aoc2024/day6.input")
                     (mapv vec))]
      (->> (for [y (range (count board))]
             (pmap
               (fn [x]
                 (let [bboard (update-in board [y x] (fn [c]
                                                       (if (= c \.)
                                                         \#
                                                         c)))]
                   (nil? (move-to-complete (do-move bboard [79 87] "N" #{})))))
               (range (count (first board)))))
           flatten
           (remove false?)
           count))))
