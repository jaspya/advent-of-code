(ns day5
  (:require
   [aoc :refer [read-lines]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  ;; Oof, improve me pls
  (->> (read-lines path)
       (reduce (fn [acc itm]
                 (if (str/includes? itm "|")
                   (let [[l r] (map parse-long (str/split itm #"\|"))]
                     (update-in acc [:rules r] #(conj (or % #{}) l)))
                   (if (str/includes? itm ",")
                     (update acc :updates conj (mapv parse-long (str/split itm #",")))
                     acc)))
               {:rules {}
                :updates []})))

(defn insert-or-swap
  [a lookup [x & rest]]
  (if (contains? (get lookup x) a)
    (if (seq rest)
      (concat [a x] rest)
      [a x])
    (if (seq rest)
      (concat [x] (insert-or-swap a lookup rest))
      [x a])))

(defn find-valid-score
  [rules xs]
  (let [ordered (reduce #(insert-or-swap %2 rules %1) [(first xs)] (rest xs))]
    (when (= xs ordered)
      (get (vec ordered) (quot (count ordered) 2)))))

(defn find-invalid-score
  [rules xs]
  (let [ordered (reduce #(insert-or-swap %2 rules %1) [(first xs)] (rest xs))]
    (when (not= xs ordered)
      (get (vec ordered) (quot (count ordered) 2)))))

(defn part1
  [input]
  (let [{:keys [rules updates]} (parse-data input)]
    (reduce + (keep #(find-valid-score rules %) updates))))

(defn part2
  [input]
  (let [{:keys [rules updates]} (parse-data input)]
    (reduce + (keep #(find-invalid-score rules %) updates))))

(deftest print-queue
  (is (= 6051 (part1 "resources/day5.input"))
      "Part 1: What do you get if you add up the middle page number from those correctly-ordered updates?")
  (is (= 5093 (part2 "resources/day5.input"))
      "Part 2: What do you get if you add up the middle page numbers after correctly ordering just those updates?"))

(comment
  ;; Part 1: Initial solution using blocklist
  (let [{:keys [rules updates]} (parse-data "resources/aoc2024/day5.input")]
    (->> updates
         (keep (fn [current]
                 (let [valid (-> (reduce (fn [acc itm]
                                           (if (:valid acc)
                                             (if (contains? (:blocklist acc) itm)
                                               {:valid false}
                                               (update acc :blocklist set/union (get rules itm)))
                                             acc))
                                         {:blocklist #{}
                                          :valid true}
                                         current)
                                 :valid)]
                   (when valid (get current (quot (count current) 2))))))
         (reduce +))))
