(ns day4
  (:require
   [aoc :refer [read-lines transpose]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

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
  (->> matrix
       rotate45
       (map (partial apply str))))

(defn part1
  [input]
  (let [words (vec (read-lines input))
        horizontals (count (find-christmas words))
        verticals (count (find-christmas (transpose-str words)))
        diagonal-horizontals (count (find-christmas (rotate45-str words)))
        diagonal-verticals (count (find-christmas (rotate45-str (mapv (comp vec reverse) words))))]
    (+ horizontals verticals diagonal-horizontals diagonal-verticals)))

(defn part2
  [input]
  (let [lines (read-lines input)
        c (dec (count lines))
        regex #(str "(?=(" %1 "(.|\\n){" c "}A(.|\\n){" c "}" %2 "))")]
    (->> [(regex "M.M" "S.S")
          (regex "M.S" "M.S")
          (regex "S.M" "S.M")
          (regex "S.S" "M.M")]
         (map #(count (re-seq (re-pattern (str %)) (str/join "\n" lines))))
         (reduce +))))

(deftest ceres-search
  (is (= 2344 (part1 "resources/day4.input"))
      "Part 1: How many times does XMAS appear?")
  (is (= 1815 (part2 "resources/day4.input"))
      "Part 2: How many times does an X-MAS appear?"))
