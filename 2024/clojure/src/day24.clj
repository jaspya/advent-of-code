(ns day24
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def operator
  {"AND" #(and %1 %2)
   "OR" #(or %1 %2)
   "XOR" #(not= %1 %2)})

(defn gate
  [op x y]
  (fn [gates]
    ((operator op)
     ((get gates x) gates)
     ((get gates y) gates))))

(defn parse-data
  [input]
  (let [data (->> (str/split-lines (slurp input))
                  (partition-by str/blank?))
        values (->> (first data)
                    (map #(str/split % #": "))
                    (map (fn [[a b]] [a (constantly (= b "1"))]))
                    (into {}))]
    (->> (last data)
         (map #(re-seq #"\w+" %))
         (reduce (fn [acc [x op y z]]
                   (assoc acc z (gate op x y)))
                 values))))

(defn output
  [gates]
  (->> (keys gates)
       (filter #(str/starts-with? % "z"))
       sort
       (map #((get gates %) gates))
       (reduce #(str (if %2 "1" "0") %1) "")))

(defn part1
  [input]
  (Long/parseLong (output (parse-data input)) 2))

(deftest crossed-wires-part1
  (is (= 69201640933606 (part1 "resources/day24.input"))
      "Part 1: What decimal number does it output on the wires starting with z?"))
