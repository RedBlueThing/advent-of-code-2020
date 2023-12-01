
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["35"
                    "20"
                    "15"
                    "25"
                    "47"
                    "40"
                    "62"
                    "55"
                    "65"
                    "95"
                    "102"
                    "117"
                    "150"
                    "182"
                    "127"
                    "219"
                    "299"
                    "277"
                    "309"
                    "576"])

(def real-data-raw (str/split-lines (slurp "day-nine.txt")))

(defn valid-numbers-at-index [data-stream index preamble-length]
  (let [start (- index preamble-length)
        preamble-numbers (subvec data-stream start index)]
    (into #{} (map (fn [[first-number second-number]] (+ first-number second-number)) (combo/combinations preamble-numbers 2)))))

(defn parse-data [data]
  (apply vector (map #(Long/parseLong %) data)))

(defn part-one [data-stream preamble-length]
  (loop [i preamble-length
         invalid-number nil]
    (if (or (= i (count data-stream)) invalid-number)
      invalid-number
      (let [this-number (nth data-stream i)
            maybe-invalid? (not (contains? (valid-numbers-at-index data-stream i preamble-length) this-number))
            invalid-number (if maybe-invalid? this-number nil)]
        (recur (inc i) invalid-number)))))

(defn sub-range-for-index-length-direction [data-stream index length direction]
  ;; return the vector of numbers in the data stream given a starting index,
  ;; then length of the vector of numbers and the direction we are scanning
  ;; through the underlying data set.
  (let [base-for-index-in-direction (if (> direction 0) index (inc index))
        index-in-direction (+ base-for-index-in-direction (* direction length))
        start (min base-for-index-in-direction index-in-direction)
        end (max base-for-index-in-direction index-in-direction)]
    (subvec data-stream start end)))

(defn check-sub-range-data [sub-range target-number]
  (= (reduce + sub-range) target-number))

(defn finished []
  (println "Done!")
  nil)

(defn check-sub-range [data-stream start-index direction out-of-range-index target-number]
  (let [max-index (max start-index out-of-range-index)
        min-index (min start-index out-of-range-index)
        out-of-range-length (- max-index min-index)]
    (loop [length 1]
      (if (= length out-of-range-length)
        nil
        (let [sub-range-data (sub-range-for-index-length-direction data-stream start-index length direction)]
          (if (check-sub-range-data sub-range-data target-number)
            (println "Solution" (sort sub-range-data))
            (recur (inc length))))))))

(defn check-range [data-stream start-index direction out-of-range-index target-number]
  (loop [i start-index]
    (if (= i out-of-range-index)
      (finished)
      (do
        (check-sub-range data-stream i direction out-of-range-index target-number)
        (recur (+ i direction))))))

(defn index-of-pred [pred coll] (ffirst (filter (comp pred second) (map-indexed list coll))))

(defn part-two [data-stream preamble-length]
  (let [part-one-solution (part-one data-stream preamble-length)
        index-of-solution (index-of-pred #(= % part-one-solution) data-stream)
        ranges-to-check [[(dec index-of-solution) -1 -1]
                         [(inc index-of-solution) 1 (count data-stream)]]]
    (loop [range-to-check-index 0]
      (if (= range-to-check-index (count ranges-to-check))
        (finished)
        ;; need to check the range
        (let [[start-index direction out-of-range-index] (nth ranges-to-check range-to-check-index)]
          (check-range data-stream start-index direction out-of-range-index part-one-solution))))))
