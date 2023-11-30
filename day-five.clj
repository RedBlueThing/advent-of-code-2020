
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])

;; where F means "front", B means "back", L means "left", and R means "right"
;;
;; 128 rows on the plane (numbered 0 through 127)
;;
;; L or R; these specify exactly one of the 8 columns of seats on the plane (numbered 0 through 7)

(def test-data-raw ["FBFBBFFRLR"
                    "BFFFBBFRRR"
                    "FFFBBBFRRR"
                    "BBFFBBFRLL"])

(def real-data-raw (str/split-lines (slurp "day-five.txt")))

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

(defn set-of-all-seat-row-columns [rows columns]
  (into #{} (mapcat conj (for [row (range 0 rows)] (for [column (range 0 columns)] [row columns])))))

(defn part-one-solution [data]
  (apply max(map (fn [command-row] (apply seat-id-for-row-column (apply row-column-for-commands (split-seat-data command-row)))) data)))

(defn part-two-solution [data]
  (let [set-of-assigned-seats (into #{} (map (fn [command-row] (apply row-column-for-commands (split-seat-data command-row))) data))
        set-of-missing-seats (set/difference (set-of-all-seat-row-columns rows columns) set-of-assigned-seats)
        ids-for-missing-seats (map (fn [[row column]] (seat-id-for-row-column row column)) set-of-assigned-seats)
        sorted-ids-for-missing-seats (sort ids-for-missing-seats)
        min-val (first sorted-ids-for-missing-seats)
        max-val (last sorted-ids-for-missing-seats)
        full-range (range min-val (inc max-val))]
    (filter #(not (contains? (set sorted-ids-for-missing-seats) %)) full-range)))
