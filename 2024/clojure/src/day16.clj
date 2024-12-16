(ns day16
  (:require
   [clojure.string :as str]
   [pathfinding :as pathfinding]
   [data-viz :as data-viz]))

(defn parse-data
  [path]
  (let [data (->> (slurp path)
                  (str/split-lines))]
    data))

(comment
  (parse-data "example"))


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
         (into {}))]])

;; 109516

(comment
  (let [field (->> (parse-data "resources/day16.input")
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
    [(reduce (fn [acc [c1 c2]]
               (+ acc (get (get path c1) c2)))
             0
             (partition 2 1 (pathfinding/dijksta path
                                                 (zonal-coordinates->keyword (:start control))
                                                 (zonal-coordinates->keyword (:end control)))))
     (reduce (fn [acc [c1 c2]]
               (+ acc (get (get path c1) c2)))
             0
             (partition 2 1 (pathfinding/dijksta path
                                                 (zonal-coordinates->keyword (:start control))
                                                 (meridional-coordinates->keyword (:end control)))))]
    #_[(dec (count (pathfinding/dijksta path
                                      (zonal-coordinates->keyword (:start control))
                                      (zonal-coordinates->keyword (:end control)))))
     (dec (count (pathfinding/dijksta path
                                      (zonal-coordinates->keyword (:start control))
                                      (meridional-coordinates->keyword (:end control)))))])
  )

(comment
  (data-viz/start)

  (let [field (->> (parse-data "example")
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
                                          (get-surrounding-fields field i j)))))
        res (->> (pathfinding/dijksta* path
                                       (zonal-coordinates->keyword (:start control))
                                       (zonal-coordinates->keyword (:end control)))
                 (map (fn [k] (keyword (subs (str k) 2))))
                 set
                 #_count)]
    
    
    
    
    
  (data-viz/draw (fn [_idx]
                   (->> res
                        (mapv (fn [k]
                                (vec (reverse (map parse-long (str/split (subs (str k) 1) #"\-")))))))) 
                 {:grid-rows 30
                  :grid-cols 30
                  :panel-cols 3
                  :panel-rows 2
                  :panel-max 1
                  :start-index 0
                  :step-count 1
                  :as-board false
                  :as-points false
                  :colors {}})
    
    
    ))


