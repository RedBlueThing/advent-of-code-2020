
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

;; where F means "front", B means "back", L means "left", and R means "right"
;;
;; 128 rows on the plane (numbered 0 through 127)
;;
;; L or R; these specify exactly one of the 8 columns of seats on the plane (numbered 0 through 7)

(def test-data-raw ["FBFBBFFRLR"
                    "BFFFBBFRRR"
                    "FFFBBBFRRR"
                    "BBFFBBFRLL"])

(def test-data-expected-seat-id [357, 567, 119, 820])

(defn split-seat-data [seat-data]
  (let [row-commands (subs seat-data 0 7)
        column-commands (subs seat-data 7 10)]
    [row-commands column-commands]))

(def rows 128)
(def columns 8)

(defn index-for-commands [commands left-command right-command max-index]
  (first (reduce (fn command-reducer [current-range command]
                   (let [[start end] current-range
                         segment-length (/ (- end start) 2)]
                     (cond
                       (= command left-command) [start (- end segment-length)]
                       (= command right-command) [(+ start segment-length) end])))
                 [0 max-index] (char-array commands))))

(assert (= (index-for-commands "FBFBBFF" \F \B rows) 44))

(defn row-column-for-commands [row-commands column-commands]
  (let [row (index-for-commands row-commands \F \B rows)
        column (index-for-commands column-commands \L \R columns)]
    [row column]))

(defn seat-id-for-row-column [row column]
  (+ (* row 8) column))

(defn part-one-solution [data]
  (apply max(map (fn [command-row] (apply seat-id-for-row-column (apply row-column-for-commands (split-seat-data command-row)))) data)))

(def real-data-raw (str/split-lines (slurp "day-five.txt")))


