(ns day23
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as c]))

(defn parse-data
  [input]
  (partition 2 (re-seq #"\w+" (slurp input))))

(defn group-connections
  [connections [a b]]
  (-> connections
      (update a (fnil conj #{}) b)
      (update b (fnil conj #{}) a)))

(defn find-triples
  [connections origin target]
  (let [origin-connections (conj (get connections origin) origin)
        target-connections (conj (get connections target) target)]
    (->> (disj origin-connections origin target)
         (keep (fn [other]
                 (let [other-connections (conj (get connections other) other)
                       network (set/intersection origin-connections target-connections other-connections)
                       triple #{origin target other}]
                   (when (set/subset? triple network)
                     triple)))))))

(defn part1
  [input]
  (let [connections (reduce group-connections {} (parse-data input))]
    (->> connections
         (filter (comp #(str/starts-with? % "t") key))
         (mapcat (fn [[origin targets]]
                   (->> targets
                        (mapcat #(find-triples connections origin %))
                        set)))
         set
         count)))

(deftest lan-party-part1
  (is (= 1194 (part1 "resources/day23.input"))
      "Part 1: How many contain at least one computer with a name that starts with t?"))

(defn find-networks
  [connections [origin origin-connections]]
  (->> origin-connections
       (reduce
        (fn [networks target]
          (for [net networks]
            (if (set/subset? net (get connections target))
              (conj net target)
              net)))
        (map (into #{}) origin-connections))
       (map #(str/join "," (sort (conj % origin))))
       set))

(defn part2
  [input]
  (let [connections (reduce group-connections {} (parse-data input))]
    (->> connections
         (filter (comp #(str/starts-with? % "t") key))
         (mapcat #(find-networks connections %))
         set
         (sort-by count)
         last)))

(deftest lan-party-part2
  (is (= "bd,bu,dv,gl,qc,rn,so,tm,wf,yl,ys,ze,zr" (part2 "resources/day23.input"))
      "Part 2: What is the password to get into the LAN party?"))

(comment
  (c/quick-bench (part1 "resources/day23.input")) ;; 9.27 ms
  (c/quick-bench (part2 "resources/day23.input"))) ;; 3.48 ms
