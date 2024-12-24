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

(defn parse-gates
  [input]
  (->> (str/split-lines (slurp input))
       (partition-by str/blank?)
       last
       (map #(re-seq #"\w+" %))))

(defn swap-gates
  [gates swaps]
  (let [swaps (reduce (fn [acc [a b]] (assoc acc a b b a)) {} swaps)]
    (reduce (fn [acc [x op y z]]
              (-> acc
                  (assoc (get swaps z z) [op x y])
                  (assoc #{op x y} (get swaps z z))))
            {}
            gates)))

(defn analyse-bit
  [gates idx last-d2p]
  (let [z (format "z%02d" idx)
        y (format "y%02d" idx)
        x (format "x%02d" idx)
        y' (format "y%02d" (dec idx))
        x' (format "x%02d" (dec idx))
        [_ z1 z2] (get gates z)
        z-set #{z1 z2}
        d2p1 (get gates #{"AND" y' x'})
        d2p2 (get gates #{"XOR" y' x'})
        d2p3 (get gates #{"AND" d2p2 last-d2p})
        d2p (get gates #{"OR" d2p1 d2p3})
        d2 (get gates #{"XOR" y x})
        found-set #{d2p d2}
        found-output (get gates #{"XOR" d2p d2})
        valid (= z-set found-set)]
    {:z z
     :z-set z-set
     :z1 [z1 (get gates z1)]
     :z2 [z2 (get gates z2)]
     :found-set found-set
     :found-output found-output
     :real-last-d2p last-d2p
     :last-d2p d2p
     :d2p1 d2p1
     :d2p2 d2p2
     :d2p3 d2p3
     :d2p d2p
     :d2 d2
     :valid valid}))

(defn run-bits
  [gates swaps]
  (let [bit1-d2p "gwq"
        gates (swap-gates gates swaps)]
    (reduce (fn [{:keys [last-d2p]} idx]
              (let [data (analyse-bit gates idx last-d2p)]
                (if-not (:valid data)
                  (reduced data)
                  data)))
            {:last-d2p bit1-d2p}
            (range 2 45))))

(comment
  ;; Part 2

  ;; Invalid at z09 because of kfp and hbs
  ;; Simple swap fcw is the d2p son hbs should be the 09 XOR
  ;; And the 09 XOR is kfp
  (let [gates (parse-gates "resources/day24.input")]
    (run-bits gates []))

  ;; Invalid at z18 because of z18 and dhq
  ;; z18 itself is wrong because z-set can not be a XOR of inputs
  ;; The expected output set matches dhq zo swap with z-gate
  (let [gates (parse-gates "resources/day24.input")]
    (run-bits gates [["kfp" "hbs"]]))

  ;; Invalid at z22 because of z22 and pdg
  ;; The expected output is correct so swap z22 with output z-gate
  (let [gates (parse-gates "resources/day24.input")]
    (run-bits gates [["kfp" "hbs"]
                     ["z18" "dhq"]]))

  ;; Invalid at z28 because of z27 and jcp manually look at z27
  (let [gates (parse-gates "resources/day24.input")]
    (run-bits gates [["kfp" "hbs"]
                     ["z18" "dhq"]
                     ["z22" "pdg"]]))
  
  ;; Valid till the last normal bit
  (let [gates (parse-gates "resources/day24.input")]
    (run-bits gates [["kfp" "hbs"]
                     ["z18" "dhq"]
                     ["z22" "pdg"]
                     ["z27" "jcp"]])))

(defn part2
  [_input]
  (->> [["kfp" "hbs"]
        ["z18" "dhq"]
        ["z22" "pdg"]
        ["z27" "jcp"]]
       (apply concat)
       sort
       (str/join ",")))

(deftest crossed-wires-part2
  (is (= "dhq,hbs,jcp,kfp,pdg,z18,z22,z27" (part2 "resources/day24.input"))
      "Part 1: What do you get if you sort the names of the eight wires involved in a swap and then join those names with commas?"))
