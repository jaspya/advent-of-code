(ns day2
  (:require
   [aoc :refer [read-lines remove-at-index]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse-data
  [path]
  (->> (read-lines path)
       (map #(str/split % #" "))
       (map #(mapv parse-long %))))

(defn safe-report?
  [report]
  (let [sorted-report (sort report)]
    (and (or (= report sorted-report)
             (= report (reverse sorted-report)))
         (every? #(<= 1 (abs (- (first %) (last %))) 3)
                 (partition 2 1 report)))))

(defn part1
  [input]
  (->> (parse-data input)
       (filter safe-report?)
       count))

(defn safe-report-with-dampener?
  [report]
  (or (safe-report? report)
      (->> (range (count report))
           (some #(safe-report? (remove-at-index report %))))))

(defn part2
  [input]
  (->> (parse-data input)
       (filter safe-report-with-dampener?)
       count))

(deftest red-nosed-reports
  (is (= 564 (part1 "resources/day2.input"))
      "Part 1: How many reports are safe?")
  (is (= 604 (part2 "resources/day2.input"))
      "Part 2: How many reports are now safe?"))

;; (defn safe-report-with-dampener?
;;   [[first-level second-level third-level :as report]]
;;   (-> (reduce (fn [{:keys [safe ascending last-level skipped]} level]
;;                 (let [safe-diff (<= 1 (abs (- last-level level)) 3)
;;                       ascending-level (< last-level level)
;;                       safe-direction (or (and ascending ascending-level)
;;                                          (and (not ascending) (not ascending-level)))
;;                       safe-level (and safe safe-diff safe-direction)]
;;                   (if (and (not safe-level) (not skipped))
;;                     {:safe safe
;;                      :ascending ascending
;;                      :last-level last-level
;;                      :skipped true}
;;                     {:safe safe-level
;;                      :ascending ascending
;;                      :last-level level
;;                      :skipped skipped})))
;;               {:safe true
;;                :ascending (< first-level second-level)
;;                :last-level first-level
;;                :skipped nil}
;;               (rest report))
;;       :safe))
