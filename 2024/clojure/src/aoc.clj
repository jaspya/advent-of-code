(ns aoc
  #_{:clj-kondo/ignore [:unused-namespace]}
  (:require
   [clojure.java.io :as io]
   [clojure.math :as math]
   [pez.baldr :as sut]))

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
