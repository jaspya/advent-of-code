(ns aoc
  #_{:clj-kondo/ignore [:unused-namespace]}
  (:require
   [clojure.java.io :as io]
   [clojure.math :as math]
   [pez.baldr :as sut])
  (:import
   [java.util BitSet HashMap PriorityQueue]))

(defn read-lines
  [path]
  (with-open [reader (io/reader path)]
    (doall (line-seq reader))))

(defn remove-at-index
  [v idx]
  (vec (concat (subvec v 0 idx) (subvec v (inc idx)))))

(defn transpose
  [matrix]
  (apply mapv vector matrix))

(defn mean
  [xs]
  (/ (reduce + xs) (count xs)))

(defn mean-center
  [xs]
  [(mean (map last xs))
   (mean (map first xs))])

(defn squared-deviation
  [x y]
  (math/pow (- y x) 2))

(defn variance
  [xs]
  (let [m (mean xs)
        deviations (map (partial squared-deviation m) xs)]
    (/ (reduce + deviations) (count xs))))

(defn distance
  [[y1 x1] [y2 x2]]
  (math/sqrt (+ (squared-deviation y1 y2) (squared-deviation x1 x2))))

(defrecord NodeCost [node cost]
  Comparable
  (compareTo [this other]
    (compare (:cost this) (:cost other))))

(defn- reconstruct-shortest-path
  [distances start end]
  (let [path (-> (loop [node end
                        path []]
                   (if-let [parent (:parent (get distances node))]
                     (recur parent (conj path node))
                     (conj path node)))
                 reverse)]
    (when (= (first path) start)
      path)))

(defn dijkstra-distances
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
    distances))

(defn dijksta
  [graph start end]
  (-> (dijkstra-distances graph start end)
      (reconstruct-shortest-path start end)))
