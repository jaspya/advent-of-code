(ns day9
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn parse-data
  [input]
  (->> (slurp input)
       (map Character/getNumericValue)
       (partition 2 2 nil)
       (map-indexed (fn [idx [file empty]]
                      [(repeat file idx)
                       (repeat (or empty 0) \.)]))
       flatten
       vec))

(defn part1
  [input]
  (let [disk (parse-data input)]
    (loop [x (first disk)
           xs (subvec disk 1)
           idx 0
           checksum 0
           calcs []]
      (if (zero? (count xs))
        (if (= x \.)
          checksum
          (+ checksum (* idx x)))
        (if (= x \.)
          (recur (peek xs) (pop xs) idx checksum calcs)
          (recur (first xs) (subvec xs 1) (inc idx) (+ checksum (* idx x)) (conj calcs [idx x])))))))

(defn insert
  [new xs]
  (loop [head (first xs)
         xs (subvec xs 1)
         target []]
    (let [inserted (and (nil? (first head))
                        (<= (last new) (last head)))
          target (if inserted
                   (if (= (last new) (last head))
                     (conj target new)
                     (conj target new [(first head) (- (last head) (last new))]))
                   (conj target head))]
      (if (or inserted (zero? (count xs)))
        (if (zero? (count xs))
          (if (not inserted)
            (conj target new)
            target)
          (into target xs))
        (recur (first xs) (subvec xs 1) target)))))

(defn checksum
  [disk]
  (->> disk
       (map-indexed (fn [idx itm]
                      (if itm
                        (* idx itm)
                        0)))
       (reduce +)))

(defn part2
  [input]
  (let [data (->> (slurp input)
                  (map Character/getNumericValue)
                  (partition 2 2 nil)
                  (map-indexed (fn [idx [file empty]]
                                 [[idx file] (when (pos-int? empty) [nil empty])]))
                  (apply concat)
                  (remove nil?)
                  vec)
        dunno (loop [idx (first (last data))
                     xs data
                     tail []]
                (if (zero? idx)
                  (into xs tail)
                  (let [new (peek xs)]
                    (if (nil? (first new))
                      (recur idx (pop xs) (into [new] tail))
                      (let [ins (insert new (pop xs))]
                        (if (= new (peek ins))
                          (recur (dec idx) (pop ins) (into [new] tail))
                          (recur (dec idx) ins (into [[nil (last new)]] tail))))))))]
    (->> dunno
         (mapcat (fn [[id length]]
                   (repeat length (or id nil))))
         checksum
         #_str/join)))

(deftest disk-fragmenter
  (is (= 6337367222422 (part1 "resources/day9.input"))
      "Part 1: What is the resulting filesystem checksum?")
  (is (= 1 1)
      "Part 2: TOO SLOW! @ 4.99 sec")
  #_(is (= 6361380647183 (part2 "resources/day9.input"))
      "Part 2: What is the resulting filesystem checksum?"))

(comment
  (c/quick-bench (part1 "resources/day9.input")) ;; 80.87 ms
  (c/quick-bench (part2 "resources/day9.input"))) ;; 4.99 sec
