(ns day15
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]
   [data-viz :as data-viz]
   [flatland.useful.seq :refer [find-first]]
   [clojure.string :as str]))

(defn parse-data
  [path]
  (let [data (->> (slurp path)
                  (str/split-lines)
                  (partition-by str/blank?))]
    [(mapv #(str/split % #"") (first data))
     (str/join (last data))]))

(def north [-1 0])
(def east [0 1])
(def south [1 0])
(def west [0 -1])

(defn find-empty-space
  [board location direction]
  (let [next-location (mapv + location direction)
        next (get-in board next-location)]
    (when-not (= next "#")
      (if (= next "O")
        (find-empty-space board next-location direction)
        next-location))))

(defn do-move
  [board location direction]
  (let [next-location (mapv + location direction)
        next (get-in board next-location)
        blocked (= next "#")
        box (= next "O")]
    (if blocked
      [board location]
      (if box
        (if-let [empty-space (find-empty-space board next-location direction)]
          [(-> board
               (assoc-in location ".")
               (assoc-in next-location "@")
               (assoc-in empty-space "O"))
           next-location]
          [board location])
        [(-> board
             (assoc-in location ".")
             (assoc-in next-location "@"))
         next-location]))))

(def ->direction
  {\^ north, \> east, \v south, \< west})

(defn board->locations
  [board]
  (for [y (range (count board))
        x (range (count (first board)))]
    [y x]))

(defn find-start
  [board]
  (->> (board->locations board)
       (find-first #(= (get-in board %) "@"))))

(defn solve
  [[board instructions]]
  (loop [board board
         location (find-start board)
         [x & xs] instructions]
    (let [[board next-location] (do-move board location (->direction x))]
      (if (seq xs)
        (recur board next-location xs)
        board))))

(defn gps-coordinate
  [[y x]]
  (+ (* 100 y) x))

(defn sum-gps-coordinates
  [board tile]
  (->> (board->locations board)
       (filter #(= (get-in board %) tile))
       (map gps-coordinate)
       (reduce +)))

(defn part1
  [input]
  (let [data (parse-data input)]
    (sum-gps-coordinates (solve data) "O")))

(defn scale-tile
  [tile]
  (case tile
    "@" ["@" "."]
    "O" ["[" "]"]
    [tile tile]))

(defn scale-up
  [board]
  (mapv #(vec (mapcat scale-tile %)) board))

(defn find-horizontal-empty-space
  [board location direction]
  (let [next-location (mapv + location direction)
        next (get-in board next-location)]
    (when-not (= next "#")
      (if (or (= next "[")
              (= next "]"))
        (find-horizontal-empty-space board next-location direction)
        next-location))))

(defn fill-boxes
  [board [y1 x1] [_y2 x2]]
  (let [x-start (min x1 x2)
        x (range x-start (inc (max x1 x2)))]
    (update board y1 (fn [row]
                       (reduce (fn [acc x-loc]
                                 (let [c (if (even? (- x-loc x-start)) "[" "]")]
                                   (assoc acc x-loc c)))
                               row
                               x)))))

(defn move-tree
  [board location direction]
  (when board
    (let [tile (get-in board location)
          left (if (= tile "[") location (mapv + location west))
          right (if (= tile "[") (mapv + location east) location)
          left-target (mapv + left direction)
          right-target (mapv + right direction)
          left-target-tile (get-in board left-target)
          right-target-tile (get-in board right-target)]
      (if (or (= left-target-tile "#") (= right-target-tile "#"))
        nil
        (if (and (= left-target-tile ".") (= right-target-tile "."))
          (-> board
              (assoc-in left ".")
              (assoc-in right ".")
              (assoc-in left-target "[")
              (assoc-in right-target "]"))
          (-> (if (= left-target-tile "[")
                (move-tree board left-target direction)
                (cond-> board
                  (= left-target-tile "]") (move-tree left-target direction)
                  (= right-target-tile "[") (move-tree right-target direction)))
              (move-tree location direction)))))))

(defn do-wide-move
  [board location direction]
  (let [next-location (mapv + location direction)
        next (get-in board next-location)
        blocked (= next "#")
        box (or (= next "[")
                (= next "]"))]
    (if blocked
      [board location]
      (if box
        (if (or (= direction west)
                (= direction east))
          (if-let [empty-space (find-horizontal-empty-space board next-location direction)]
            [(-> board
                 (assoc-in location ".")
                 (assoc-in next-location "@")
                 (fill-boxes (mapv + next-location direction) empty-space))
             next-location]
            [board location])
          (if-let [board (move-tree board next-location direction)]
            [(-> board
                 (assoc-in location ".")
                 (assoc-in next-location "@"))
             next-location]
            [board location]))
        [(-> board
             (assoc-in location ".")
             (assoc-in next-location "@"))
         next-location]))))

(defn solve-scaled-up
  [[board instructions]]
  (loop [board (scale-up board)
         location (find-start board)
         [x & xs] instructions]
    (let [[board next-location] (do-wide-move board location (->direction x))]
      (if (seq xs)
        (recur board next-location xs)
        board))))

(defn part2
  [input]
  (let [data (parse-data input)]
    (sum-gps-coordinates (solve-scaled-up data) "[")))

(deftest warehouse-woes
  (is (= 1430439 (part1 "resources/day15.input"))
      "Part 1: What is the sum of all boxes' GPS coordinates?")
  (is (= 1458740 (part2 "resources/day15.input"))
      "Part 2: What is the sum of all boxes' final GPS coordinates?"))

(comment
  (c/quick-bench (part1 "resources/day15.input")) ;; 14.65 ms
  (c/quick-bench (part2 "resources/day15.input"))) ;; 20.59 ms

(comment
  (data-viz/start)

  (let [[board _instructions] (parse-data "example")
        instructions ">>v>>"
        board (scale-up board)
        newboard
        (loop [board board
               location (find-start board)
               [x & xs] instructions]
          (if x
            (let [[board next-location] (do-wide-move board location (->direction x))]
              (recur board next-location xs))
            board))]
    (data-viz/draw
     (fn [_idx]
       (if (= _idx 0)
         board
         newboard))
     {:grid-rows (count board)
      :grid-cols (count (first board))
      :panel-cols 1
      :panel-rows 2
      :panel-max 2
      :start-index 0
      :step-count 1
      :as-board true
      :as-points false
      :colors {"#" :gray
               "@" :green
               "O" :gray
               "[" :blue
               "]" :blue
               "." :gray}}))

  (let [[board instructions] (parse-data "resources/day15.input")
        instructions instructions
        board (scale-up board)
        newboard
        (loop [board board
               location (find-start board)
               [x & xs] instructions]
          (if x
            (let [[board next-location] (do-wide-move board location (->direction x))]
              (recur board next-location xs))
            board))]
    (data-viz/draw
     (fn [_idx]
       (if (= _idx 0)
         board
         (if (= _idx 1)
           newboard
           [])))
     {:grid-rows (count board)
      :grid-cols (count (first board))
      :panel-cols 1
      :panel-rows 1
      :panel-max 1
      :start-index 1
      :step-count 1
      :as-board true
      :as-points true
      :colors {"#" :gray
               "@" :green
               "O" :gray
               "[" :blue
               "]" :blue
               "." :black}})))
