(ns day5
  (:require
   [aoc :refer [read-lines sum]]
   [clojure.set :as set]
   [clojure.string :as str]))

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

(comment
  ;; Part 1 - 6051
  ;; Initial solution using blocklist
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

(comment
  ;; Part 1 - 6051
  ;; Use recursion
  (let [{:keys [rules updates]} (parse-data "resources/aoc2024/day5.input")]
    (sum (keep #(find-valid-score rules %) updates))))

(defn find-invalid-score
  [rules xs]
  (let [ordered (reduce #(insert-or-swap %2 rules %1) [(first xs)] (rest xs))]
    (when (not= xs ordered)
      (get (vec ordered) (quot (count ordered) 2)))))

(comment
  ;; Part 2 - 5093
  (let [{:keys [rules updates]} (parse-data "resources/aoc2024/day5.input")]
    (sum (keep #(find-invalid-score rules %) updates))))
