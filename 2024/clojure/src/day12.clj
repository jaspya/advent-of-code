(ns day12
  (:require
   [aoc :refer [read-lines]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (mapv #(str/split % #""))))

(defn board->locations
  [board]
  (for [y (range (count board))
        x (range (count (first board)))]
    [y x]))

(def north [-1 0])
(def east [0 1])
(def south [1 0])
(def west [0 -1])

(def directions
  [north east south west])

(defn ->vector
  [[y x] direction]
  (condp = direction
    north [y x :north]
    east [y (inc x) :east]
    south [(inc y) x :south]
    west [y x :west]))

(defn explore-area
  [board type location]
  (loop [location location
         open []
         visited #{location}
         area 0
         edges []]
    (let [neighbours (map #(vector % (mapv + location %)) directions)
          next (remove (comp visited last) neighbours)
          next-locations (map last (filter #(= (get-in board (last %)) type) next))
          open (concat open next-locations)
          removed-locations (remove #(= (get-in board (last %)) type) next)
          area (inc area)
          edges (concat edges (map #(->vector location (first %)) removed-locations))]
      (if (seq open)
        (recur (first open) (rest open) (set/union visited (set next-locations)) area edges)
        [area edges visited]))))

(defn solve
  [input edge-count-fn]
  (let [board (parse-data input)]
    (-> (reduce (fn [{:keys [visited] :as m} location]
                  (if (contains? visited location)
                    m
                    (let [type (get-in board location)
                          [area edges visited] (explore-area board type location)]
                      (-> m
                          (update :visited set/union visited)
                          (update :price + (* area (edge-count-fn edges)))))))
                {:visited #{}, :price 0}
                (board->locations board))
        :price)))

(defn part1
  [input]
  (solve input count))

(defn count-increasing
  [xs]
  (-> (reduce (fn [[current-count last] next]
                [(if (not= (inc last) next)
                   (inc current-count)
                   current-count)
                 next])
              [1 (first xs)]
              (rest xs))
      first))

(defn count-connected-edges
  [edges]
  (->> edges
       (reduce (fn [m [y x dir]]
                 (if (or (= dir :north) (= dir :south))
                   (update m dir #(update % y conj x))
                   (update m dir #(update % x conj y))))
               {})
       (mapcat (comp vals val))
       (map (comp count-increasing sort))
       (reduce +)))

(defn part2
  [input]
  (solve input count-connected-edges))

(deftest garden-groups
  (is (= 1344578 (part1 "resources/day12.input"))
      "Part 1: What is the total price of fencing all regions on your map?")
  (is (= 814302 (part2 "resources/day12.input"))
      "Part 2: What is the new total price of fencing all regions on your map?"))

(comment
  (c/quick-bench (part1 "resources/day12.input")) ;; 119.76 ms
  (c/quick-bench (part2 "resources/day12.input"))) ;; 109.26 ms

(comment
  ;; Initial solution

  (def directions
    [[-1 0] [0 1] [1 0] [0 -1]])

  (defn maybe
    [board type location]
    (loop [position location
           locations []
           checked #{location}
           area 0
           perimeter 0]
      (let [next (->> (map #(mapv + position %) directions)
                      (remove checked))
            fnext (filter #(= (get-in board %) type) next)
            locations (concat locations fnext)
            peri (- (count next) (count fnext))]
        (if (seq locations)
          (recur (first locations) (rest locations) (set/union checked (set fnext)) (inc area) (+ perimeter peri))
          [(inc area) (+ perimeter peri) checked])))))
