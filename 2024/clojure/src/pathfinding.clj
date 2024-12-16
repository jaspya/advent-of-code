(ns pathfinding)

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

(defn- update-distance*
  [distances parent node cost]
  (cond-> distances
    (<= cost (get-in distances [node :cost]))
    (update node #(if (< cost (get-in distances [node :cost]))
                    (assoc % :parents #{parent} :cost cost)
                    (do
                      (println (:parents %) parent)
                      (update % :parents conj parent))))))

(defn- build-path*
  [distances end]
  (-> (loop [open []
             node end
             nodes #{}]
        (let [parents (seq (:parents (get distances node)))
              open (if parents open (rest open))
              new-next (first open)
              parents (if parents parents (seq (:parents (get distances new-next))))]
        (println (count parents))
          (if parents
            (let [nparents (concat open parents)]
              (recur (rest nparents) (first nparents) (conj nodes node)))
            (do (println open)
                (conj nodes node)))))))

(defn dijksta*
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
            (swap! distances #(update-distance* % parent node (+ parent-cost cost))))
          (swap! visited conj parent)
          (recur))))
    (build-path* @distances end)))
