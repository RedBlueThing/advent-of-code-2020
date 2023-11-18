
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

(def test-data-raw ["..##......."
                    "#...#...#.."
                    ".#....#..#."
                    "..#.#...#.#"
                    ".#...##..#."
                    "..#.##....."
                    ".#.#.#....#"
                    ".#........#"
                    "#.##...#..."
                    "#...##....#"
                    ".#..#...#.#"])

(def real-data-raw (str/split-lines (slurp "day-three.txt")))

(defn wrap-index [data row column]
  (let [line-length (count (first data))]
    [row (mod column line-length)]))

(defn visited-locations [data column-offset row-offset]
  ;; just grab the first location for the begining of each new row
  (loop [row 0
         column 0
         locations []]
    (if (>= row (count data))
      locations
      (recur (+ row row-offset)
             (+ column column-offset)
             (conj locations (let [[offset-row offset-column] (wrap-index data row column)
                                   current-row (nth data offset-row)]
                               (nth current-row offset-column)))))))


(defn part-one [data]
  (count (filter (fn tree-predicate [chr] (= chr \#)) (visited-locations data 3 1))))


(defn part-two [data]
  (reduce * (map (fn get-tree-frow-slope-offset [[slope-row-offset slope-column-offset]]
          (count (filter (fn tree-predicate [chr] (= chr \#)) (visited-locations data slope-column-offset slope-row-offset))))
        [[1 1] [1 3] [1 5] [1 7] [2 1]])))
