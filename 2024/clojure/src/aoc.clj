(ns aoc
  (:require [clojure.java.io :as io]))

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
