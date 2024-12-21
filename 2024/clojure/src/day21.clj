(ns day21
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c])
  (:import
   [clojure.lang PersistentQueue]))

(defn find-neighbours
  [[y x]]
  (for [[dy dx dir] [[-1 0 "^"] [0 1 ">"] [1 0 "v"] [0 -1 "<"]]]
    [[(+ y dy) (+ x dx)] dir]))

(defn best-path
  [paths]
  (->> paths
       (filter #(re-matches #"(\^+|>+|v+|<+)(\^*|>*|v*|<*)" %))
       (sort-by #({\> 4 \^ 3 \v 2 \< 1} (first %))) ;; reverse order ... :(
       first))

(defn find-shortest-paths
  [grid start]
  (loop [queue (conj PersistentQueue/EMPTY [start ""])
         visited #{}
         shortest-paths {start (list "")}]
    (if (empty? queue)
      (update-vals shortest-paths #(or (best-path %) ""))
      (let [[location _] (peek queue)
            neighbours (->> (find-neighbours location)
                            (filter (fn [[p _]] (and (not (visited p)) (grid p)))))
            paths (get shortest-paths location)]
        (if (visited location)
          (recur (pop queue) visited shortest-paths)
          (recur (apply conj (pop queue) neighbours)
                 (conj visited location)
                 (reduce (fn [all-paths [n dir]]
                           (update all-paths n concat (map #(str % dir) paths)))
                         shortest-paths
                         neighbours)))))))

(defn as-grid
  [board blocked?]
  (into {}
        (for [[y row] (map-indexed vector board)
              [x cell] (map-indexed vector row)
              :when (not (blocked? cell))]
          [[y x] cell])))

(defn keypad-paths
  [keypad]
  (let [grid (as-grid keypad #{\#})]
    (-> (into {} (map (comp vec reverse) grid))
        (update-vals #(update-keys (find-shortest-paths grid %) grid)))))

(def numeric-keypad
  [[\7 \8 \9]
   [\4 \5 \6]
   [\1 \2 \3]
   [\# \0 \A]])

(def directional-keypad
  [[\# \^ \A]
   [\< \v \>]])

(defn find-paths
  [code dmap]
  (str (->> (partition 2 1 (vec (str "A" code)))
            (map #(get-in dmap %))
            (str/join "A"))
       "A"))

(defn splitter
  [s]
  (re-seq #"[\^>v<]*A" s))

(defn solve-code
  [code robots numpad dirpad]
  (let [paths (splitter (find-paths code numpad))]
    (loop [i 0
           m (reduce (fn [acc itm]
                       (update acc itm #(inc (or % 0))))
                     {}
                     paths)]
      (if (< i robots)
        (recur (inc i)
               (reduce
                (fn [acc [path path-count]]
                  (reduce
                   (fn [acc itm]
                     (update acc itm #(+ (or % 0) path-count)))
                   acc
                   (splitter (find-paths path dirpad))))
                {}
                m))
        m))))

(defn solve
  [input robots]
  (let [numpad (keypad-paths numeric-keypad)
        dirpad (keypad-paths directional-keypad)]
    (->> (str/split-lines (slurp input))
         (map #(* (parse-long (re-find #"\d+" %))
                  (->> (solve-code % robots numpad dirpad)
                       (map (fn [[k v]]
                              (* v (count k))))
                       (reduce +))))
         (reduce +))))

(defn part1
  [input]
  (solve input 2))

(deftest keypad-conundrum-part1
  (is (= 105458 (part1 "resources/day21.input"))
      "Part 1: What is the sum of the complexities of the five codes on your list?"))

(defn part2
  [input]
  (solve input 25))

(deftest keypad-conundrum-part2
  (is (= 129551515895690 (part2 "resources/day21.input"))
      "Part 2: What is the sum of the complexities of the five codes on your list?"))

(comment
  (c/quick-bench (part1 "resources/day21.input")) ;; 523 µs
  (c/quick-bench (part2 "resources/day21.input"))) ;; 2.68 ms
