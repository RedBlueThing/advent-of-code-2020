(def test-data [1721
                979
                366
                299
                675
                1456])

(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

(def target-sum 2020)

(def real-data (map #(Integer/parseInt %) (str/split-lines (slurp "day-one.txt"))))

(defn part-one [data selections target]
  (let [[v1 v2] (first (filter (fn [[v1 v2]] (= (+ v1 v2) target)) (combo/selections data 2)))]
    (* v1 v2)))

(defn part-two [data target]
  (let [[v1 v2 v3] (first (filter (fn [[v1 v2 v3]] (= (+ v1 v2 v3) target)) (combo/selections data 3)))]
    (* v1 v2 v3)))
