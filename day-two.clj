
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

(def test-data-raw ["1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc"])

(def real-data-raw (str/split-lines (slurp "day-two.txt")))

(defn valid-password-for-part-one-policy? [line]
  (let [[policy password] (str/split line #": ")
        [range character-str] (str/split policy #" ")
        character (first character-str)
        [min max] (map #(Integer/parseInt %) (str/split range #"-"))
        characters-in-password (count (filter #(= % character) password))]
    (and (>= characters-in-password min) (<= characters-in-password max))))

(defn valid-password-for-part-two-policy? [line]
  (let [[policy password] (str/split line #": ")
        [range character-str] (str/split policy #" ")
        character (char (first character-str))
        [first-character-number second-character-number] (map #(Integer/parseInt %) (str/split range #"-"))
        password-chr-one (nth password (dec first-character-number))
        password-chr-two (nth password (dec second-character-number))]
    (or (and (= password-chr-one character)
             (not= password-chr-two character))
        (and (= password-chr-two character)
             (not= password-chr-one character)))))

(defn check-passwords-and-policies-for-predicate [data predicate]
  (count (filter predicate data)))

(defn part-one [data]
  (check-passwords-and-policies-for-predicate data valid-password-for-part-one-policy?))

(defn part-two [data]
  (check-passwords-and-policies-for-predicate data valid-password-for-part-two-policy?))
