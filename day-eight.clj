
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["nop +0"
                    "acc +1"
                    "jmp +4"
                    "acc +3"
                    "jmp -3"
                    "acc -99"
                    "acc +1"
                    "jmp -4"
                    "acc +6"])

(def real-data-raw (str/split-lines (slurp "day-eight.txt")))

(defn parse-code-line [line]
  (let [[command argument] (str/split line #" ")]
    [command (Integer/parseInt argument)]))

(defn parse-code [data]
  (apply vector (map parse-code-line data)))

(defn run-until-loop-or-finish [code]
  (let [terminating-instruction-pointer (count code)]
    (loop [instruction-pointer 0
           accumulator 0
           executed-instruction #{}]
      (let [repeated-instruction (contains? executed-instruction instruction-pointer)
            finished (= instruction-pointer terminating-instruction-pointer)]
        (if (or repeated-instruction finished)
          [finished,  accumulator]
          (let [[instruction argument] (nth code instruction-pointer)
                new-accumulator (if (= instruction "acc") (+ accumulator argument) accumulator)
                new-instruction-pointer (+ instruction-pointer (if (= instruction "jmp") argument 1))]
            (recur new-instruction-pointer new-accumulator (conj executed-instruction instruction-pointer))))))))

(defn candidates-for-repair [code]
  ;; A sequence of instructions and their indexes that can be repaired.
  (filter (fn is-repairable? [[index [instruction argument]]] (or (= instruction "jmp") (= instruction "nop")))
          (map-indexed (fn [i value] [i value]) code)))

(defn attempt-repair-instruction [instruction argument]
  [(if (= instruction "jmp") "nop" "jmp") argument])

;; just brute force I guess
(defn repair [code]
  (let [candidates (candidates-for-repair code)]
    (loop [candidate-index 0
           successfully-repaired-code []]
      (if (= candidate-index (count candidates))
        successfully-repaired-code
        (let [[index [instruction argument]] (nth candidates candidate-index)
              maybe-repaired-code (assoc code index (attempt-repair-instruction instruction argument))
              [finished accumulator] (run-until-loop-or-finish maybe-repaired-code)
              new-successfully-repaired-code (if finished (conj successfully-repaired-code [candidate-index accumulator]) successfully-repaired-code)]
          (recur (inc candidate-index) new-successfully-repaired-code))))))
