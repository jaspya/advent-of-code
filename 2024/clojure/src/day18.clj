(ns day18
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c])
  (:import
   [java.util BitSet HashMap PriorityQueue]))

;; Target 1 ms, minimum 10 ms
;; Using java.util.PriorityQueue for next node
;;   => Speedup 3.3 sec -> 8 ms
;; Using defrecord instead of vector
;;   => Speedup 8 ms -> 6.5 ms
;; Using java.util.HashSet for visited nodes
;;   => Speedup 6.5 ms -> 5.7 ms
;; Using java.util.HashMap for distances
;;   => Speedup 5.7 ms -> 1.95 ms
;; Not initializing HashMap with Long/MAX_VALUE
;;   => Speedup 1.95 ms -> 1.41 ms
;; ~ Using ints instead of keywords
;;   => Speedup 1.41 ms -> 1.30 ms
;; ~ Using java.util.BitSet for visited nodes
;;   => Speedup 1.30 ms -> 1.28 ms
;; 2,578 times faster than initial implementation

(defn- build-path
  [distances start end]
  (let [path (-> (loop [node end
                        path []]
                   (if-let [parent (:parent (get distances node))]
                     (recur parent (conj path node))
                     (conj path node)))
                 reverse)]
    (when (= (first path) start)
      path)))

(defrecord NodeCost [node cost]
  Comparable
  (compareTo [this other]
    (compare (:cost this) (:cost other))))

(defn dijksta
  [graph start end]
  (let [vertices (keys graph)
        vertex-count (count vertices)
        distances (HashMap. vertex-count)
        visited (BitSet. vertex-count)
        pq (PriorityQueue. vertex-count)]
    (.put distances start {:cost 0})
    (.add pq (->NodeCost start 0))
    (loop []
      (when-let [u (.poll pq)]
        (let [parent (:node u)
              parent-cost (:cost u)]
          (if (.get visited parent)
            (recur)
            (when-not (= end parent)
              (doseq [[node cost] (get graph parent)]
                (let [node-cost (+ parent-cost cost)]
                  (when (< node-cost (:cost (.get distances node) Long/MAX_VALUE))
                    (.put distances node {:parent parent :cost node-cost})
                    (.add pq (->NodeCost node node-cost)))))
              (.set visited parent)
              (recur))))))
    (build-path distances start end)))

(defn parse-data
  [path]
  (->> (slurp path)
       (re-seq #"\d+")
       (map parse-long)))

(def north [-1 0])
(def east [0 1])
(def south [1 0])
(def west [0 -1])

(def directions
  [north east south west])

(defn coordinate->id
  [[y x]]
  (+ (* y 71) x))

(defn build-graph
  [blocked]
  (->> (for [y (range 71)
             x (range 71)]
         [y x])
       (keep (fn [[y x]]
               (when-not (blocked [x y])
                 [(coordinate->id [y x])
                  (->> directions
                       (keep (fn [direction]
                               (let [[y x] (mapv + [y x] direction)]
                                 (when (and (not (blocked [x y]))
                                            (<= 0 y 70)
                                            (<= 0 x 70))
                                   [(coordinate->id [y x]) 1]))))
                       (into {}))])))
       (into {})))

(defn part1
  [input]
  (let [blocked (->> (parse-data input)
                     (partition 2)
                     (take 1024)
                     (map vec)
                     set)]
    (-> (build-graph blocked)
        (dijksta 0 (coordinate->id [70 70]))
        count
        dec)))

(deftest ram-run-part1
  (is (= 324 (part1 "resources/day18.input"))
      "Part 1: What is the minimum number of steps needed to reach the exit?"))

(defn repeat-blocking
  [data blocked]
  (loop [i 0
         next-drops data
         blocked blocked]
    #_(println (first next-drops))
    (let [to-block (first next-drops)
          blocked (conj blocked to-block)
          graph (build-graph (conj blocked to-block))
          route (set (dijksta graph 0 (coordinate->id [70 70])))]
      #_(println i (count blocked) (count route) (count next-drops) to-block)
      (if (and (seq route) (< i 2000))
        (let [[to-block next-potention] (split-with (fn [[x y]] (not (contains? route (coordinate->id [y x])))) next-drops)]
          (recur (inc i) next-potention (set/union blocked (set to-block))))
        to-block))))

(defn part2
  [input]
  (let [data (->> (parse-data input)
                  (partition 2)
                  (map vec))
        open (drop 1024 data)
        blocked-set (set (take 1024 data))]
    (str/join "," (repeat-blocking open blocked-set))))

(deftest ram-run-part2
  (is (= "46,23" (part2 "resources/day18.input"))
      "Part 2: What are the coordinates of the first byte that will prevent the exit from being reachable from your starting position?"))

(comment
  (c/quick-bench (part1 "resources/day18.input")) ;; 13.01 ms
  (c/quick-bench (part2 "resources/day18.input"))) ;; 381 ms
