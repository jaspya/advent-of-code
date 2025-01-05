(ns input
  (:refer-clojure :exclude [read]))

(defn parse-numbers
  [s]
  (mapv #(or (parse-long %) %) s))

(defn read
  [path & {:keys [pattern]}]
  (->> (slurp path)
       (re-seq pattern)
       (mapv #(parse-numbers (subvec % 1)))))
