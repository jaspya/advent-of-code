(ns day20
  (:require
   [aoc :refer [dijkstra-distances]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn parse-data
  [path]
  (mapv vec (str/split-lines (slurp path))))

(defn location-id
  [[y x] column-count]
  (+ (* y column-count) x))

(def north [-1 0])
(def east [0 1])
(def south [1 0])
(def west [0 -1])

(def directions
  [north east south west])

(defn offset-vectors
  [picoseconds]
  (for [y (range (- picoseconds) (inc picoseconds))
        x (range (- picoseconds) (inc picoseconds))
        :when (<= (+ (abs y) (abs x)) picoseconds)]
    [y x]))

(defn find-start-and-end-locations
  [board]
  (->> (for [y (range (count board))
             x (range (count (first board)))]
         (case (get-in board [y x])
           \S [:start [y x]]
           \E [:end [y x]]
           nil))
       (into {})))

(defn build-graph
  [board]
  (let [column-count (count (first board))]
    (->> (for [y (range (count board))
               x (range column-count)
               :when (not= (get-in board [y x]) \#)]
           [(location-id [y x] column-count)
            (->> directions
                 (map #(mapv + [y x] %))
                 (remove #(= (get-in board %) \#))
                 (map #(vector (location-id % column-count) 1))
                 (into {}))])
         (into {}))))

(defn calculate-distances
  [board]
  (let [{:keys [start end]} (find-start-and-end-locations board)
        column-count (count (first board))
        start-id (location-id start column-count)
        end-id (location-id end column-count)
        graph (build-graph board)]
    (dijkstra-distances graph start-id end-id)))

(defn count-when
  [pred xs]
  (count (filter pred xs)))

(defn solve
  [input picoseconds]
  (let [board (parse-data input)
        distances (calculate-distances board)
        row-count (count board)
        column-count (count (first board))
        offsets (offset-vectors picoseconds)]
    (->> (for [y (range row-count)
               x (range column-count)
               :let [point (location-id [y x] column-count)
                     cost (:cost (get distances point))]
               :when cost]
           (for [[dy dx :as direction] offsets
                 :let [[ny nx] (mapv + [y x] direction)
                       npoint (location-id [ny nx] column-count)
                       ncost (:cost (get distances npoint))]
                 :when ncost]
             (and (<= 0 ny (dec row-count))
                  (<= 0 nx (dec column-count))
                  (>= (- ncost cost (+ (abs dy) (abs dx))) 100))))
         (apply concat)
         (count-when true?))))

(defn part1
  [input]
  (solve input 2))

(deftest race-condition-part1
  (is (= 1497 (part1 "resources/day20.input"))
      "Part 1: How many cheats would save you at least 100 picoseconds?"))

(defn part2
  [input]
  (solve input 20))

(deftest race-condition-part2
  (is (= 1030809 (part2 "resources/day20.input"))
      "Part 2: How many cheats would save you at least 100 picoseconds?"))

(comment
  (c/quick-bench (part1 "resources/day20.input")) ;; 93.9 ms
  (c/quick-bench (part2 "resources/day20.input"))) ;; 3.52 sec
