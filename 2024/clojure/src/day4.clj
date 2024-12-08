(ns day4
  (:require
   [aoc :refer [read-lines transpose]]
   [clojure.string :as str]))

(defn transpose-str
  [matrix]
  (->> matrix
       transpose
       (map (partial apply str))))

(defn find-christmas
  [xs]
  ;; Problem with overlapping when #"XMAS|SAMX", ugly fix
  ;; or use forward lookup
  (mapcat #(concat (re-seq #"XMAS" %) (re-seq #"SAMX" %)) xs))

(defn rotate45
  [matrix]
  ;; Find better solution
  (let [rows (count matrix)
        cols (count (first matrix))]
    (->> (for [y (range (+ rows cols))]
           (->> (for [x (reverse (range rows))]
                  (get-in matrix [x (- y x)]))
                (remove nil?)
                vec))
         (remove nil?)
         vec)))

(defn rotate45-str
  [matrix]
  ;; Make str version of all matrix functions
  (->> matrixs
       rotate45
       (map (partial apply str))))

(comment
  ;; Part 1 - 2344
  (let [words (vec (read-lines "resources/aoc2024/day4.input"))
        horizontals (count (find-christmas words))
        verticals (count (find-christmas (transpose-str words)))
        diagonal-horizontals (count (find-christmas (rotate45-str words)))
        diagonal-verticals (count (find-christmas (rotate45-str (mapv (comp vec reverse) words))))]
    (+ horizontals verticals diagonal-horizontals diagonal-verticals)))

(comment
  ;; Part 2 - 1815
  (let [lines (read-lines "resources/aoc2024/day4.input")
        c (dec (count lines))
        regex #(str "(?=(" %1 "(.|\\n){" c "}A(.|\\n){" c "}" %2 "))")]
    (->> [(regex "M.M" "S.S")
          (regex "M.S" "M.S")
          (regex "S.M" "S.M")
          (regex "S.S" "M.M")]
         (map #(count (re-seq (re-pattern (str %)) (str/join "\n" lines))))
         (reduce +))))
