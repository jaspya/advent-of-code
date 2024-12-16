(ns day16
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [pathfinding :as pathfinding]))

(defn parse-data
  [path]
  (str/split-lines (slurp path)))

(defn- initial-distances
  [vertices start]
  (->> vertices
       (map #(vector % {:cost (if (= % start) 0 Long/MAX_VALUE)}))
       (into {})))

(defn- next-node
  [distances visited]
  (->> distances
       (remove #(visited (first %)))
       (sort-by #(:cost (last %)))
       first))

(defn- update-distance
  [distances parent node cost]
  (cond-> distances
    (< cost (get-in distances [node :cost]))
    (update node assoc :parent parent :cost cost)))

(defn- build-path
  [distances end]
  (-> (loop [node end
             path []]
        (if-let [parent (:parent (get distances node))]
          (recur parent (conj path node))
          (conj path node)))
      reverse))

(defn dijksta
  [graph start end]
  (let [vertices (keys graph)
        distances (atom (initial-distances vertices start))
        visited (atom #{})]
    (loop []
      (let [next-node (next-node @distances @visited)
            parent (first next-node)
            parent-cost (:cost (last next-node))]
        (when-not (= end parent)
          (doseq [[node cost] (get graph parent)]
            (swap! distances #(update-distance % parent node (+ parent-cost cost))))
          (swap! visited conj parent)
          (recur))))
    (build-path @distances end)))

(defn meridional-coordinates->keyword
  [[i j]]
  (keyword (str "M" i "-" j)))

(defn zonal-coordinates->keyword
  [[i j]]
  (keyword (str "Z" i "-" j)))

(def north [-1 0])
(def east [0 1])
(def south [1 0])
(def west [0 -1])

(def directions
  [north east south west])

(defn- get-surrounding-fields
  [field i j]
  (when-not (= (get-in field [i j]) \#)
    [[(meridional-coordinates->keyword [i j])
      (->> directions
           (keep (fn [direction]
                   (let [[y x] (mapv + [i j] direction)
                         target (get-in field [y x])]
                     (when (not= target \#)
                       (if (or (= direction north)
                               (= direction south))
                         [(meridional-coordinates->keyword [y x]) 1]
                         [(zonal-coordinates->keyword [y x]) 1001])))))
           (into {}))]
     [(zonal-coordinates->keyword [i j])
      (->> directions
           (keep (fn [direction]
                   (let [[y x] (mapv + [i j] direction)
                         target (get-in field [y x])]
                     (when (not= target \#)
                       (if (or (= direction north)
                               (= direction south))
                         [(meridional-coordinates->keyword [y x]) 1001]
                         [(zonal-coordinates->keyword [y x]) 1])))))
           (into {}))]]))

(defn part1
  [input]
  (let [field (->> (parse-data input)
                   (mapv #(into [] %)))
        control (->> (for [i (range (count field))
                           j (range (count (get field i)))]
                       (case (get-in field [i j])
                         \S [:start [i j]]
                         \E [:end [i j]]
                         nil))
                     (into {}))
        path (-> (into {} (apply concat (for [i (range (count field))
                                              j (range (count (get field i)))]
                                          (get-surrounding-fields field i j)))))]
    (reduce (fn [acc [c1 c2]]
              (+ acc (get (get path c1) c2)))
            0
            (partition 2 1 (dijksta path
                                    (zonal-coordinates->keyword (:start control))
                                    (meridional-coordinates->keyword (:end control)))))))

(deftest reindeer-maze
  (is (= 109516 (part1 "resources/day16.input"))
      "Part 1: What is the lowest score a Reindeer could possibly get?"))
