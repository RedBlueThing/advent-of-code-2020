
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])

(def test-data-raw ["abc"
                    ""
                    "a"
                    "b"
                    "c"
                    ""
                    "ab"
                    "ac"
                    ""
                    "a"
                    "a"
                    "a"
                    "a"
                    ""
                    "b"])

(def real-data-raw (str/split-lines (slurp "day-six.txt")))

(defn grouped-lines [data]
  (->>
   (partition-by #(= "" %) (map str/trim data))
   (filter #(not= (first %) ""))))

(defn solution-part-one [data]
  (reduce + (map count (map
                        (fn [answers-for-group]
                          (into #{} (char-array (apply str answers-for-group))))
                        (grouped-lines data)))))


(defn solution-part-two [data]
  (reduce + (map count (map
                        (fn [answers-for-group]
                          (into #{} (apply set/intersection (map (fn [answers-str] (into #{} (char-array answers-str))) answers-for-group))))
                        (grouped-lines data)))))
