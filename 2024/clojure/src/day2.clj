(ns day2
  (:require [aoc :refer [read-lines remove-at-index]]
            [clojure.string :as str]))

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

(comment
  ;; Part 1
  (->> (parse-data "resources/aoc2024/day2.input")
       (filter safe-report?)
       count))

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

(defn safe-report-with-dampener?
  [report]
  (or (safe-report? report)
      (->> (range (count report))
           (some #(safe-report? (remove-at-index report %))))))

(comment
  ;; Part 2
  (->> (parse-data "resources/aoc2024/day2.input")
       (filter safe-report-with-dampener?)
       count))
