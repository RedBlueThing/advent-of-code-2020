
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["light red bags contain 1 bright white bag, 2 muted yellow bags."
                    "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
                    "bright white bags contain 1 shiny gold bag."
                    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
                    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
                    "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
                    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
                    "faded blue bags contain no other bags."
                    "dotted black bags contain no other bags."])

(def test-data-part-two-raw ["shiny gold bags contain 2 dark red bags."
                             "dark red bags contain 2 dark orange bags."
                             "dark orange bags contain 2 dark yellow bags."
                             "dark yellow bags contain 2 dark green bags."
                             "dark green bags contain 2 dark blue bags."
                             "dark blue bags contain 2 dark violet bags."
                             "dark violet bags contain no other bags."])

(def real-data-raw (str/split-lines (slurp "day-seven.txt")))

(defn parse-bag-rule [rule]
  ;; subs is just dropping the full stop
  (let [[target-bag contains-bags-chunk] (str/split (subs rule 0 (dec (count rule))) #" bags contain ")
        contains-bags-raw (str/split contains-bags-chunk #", ")
        ;; remove " bag" and " bags" from our contains-bags
        contains-bags (filter some? (map (fn process-contained-bag [bag-string]
                                           (if (= bag-string "no other bags")
                               ;; no other bags?
                                             nil
                               ;; some other bags
                                             (let [bag-components (str/split bag-string #" ")
                                                   number (Integer/parseInt (first bag-components))
                                                   names (str/join " " (-> bag-components rest butlast))]
                                               [names number])))
                                         contains-bags-raw))]
    [target-bag contains-bags]))

(defn parse-bag-rules [data]
  (into {} (map parse-bag-rule data)))

(defn bag-contains-bag? [rules containing-bag contains-bag]
  (let [contains-bags (rules containing-bag)]
    (boolean (some true? (map (fn is-bag-or-contains-bag [[bag number]]
                                (or (= bag contains-bag)
                                    (bag-contains-bag? rules bag contains-bag)
                                    )) contains-bags)))))

(defn how-many-bags-inside [rules bag]
  (let [contains-bags (rules bag)]
    (reduce + 0 (map (fn [[bag number]] (* number (+ (how-many-bags-inside rules bag) 1))) contains-bags))))

(defn solution-part-one [data]
  (let [rules (parse-bag-rules data)]
    (count (filter (fn [containing-bag] (bag-contains-bag? rules containing-bag "shiny gold")) (keys rules)))))

(defn solution-part-two [data]
  (let [rules (parse-bag-rules data)]
    (how-many-bags-inside rules "shiny gold")))
