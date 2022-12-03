(ns day01.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  [s]
  (Integer/parseInt s))

(defn parse-input
  "Parses the input string and returns a sequence of collections.
  Each collection represents the calories of each elf."
  [s]
  (->> s
       clojure.string/split-lines
       (partition-by #(= "" %))
       (filter #(not= '("") %))
       (map #(map str->int %))))

(def elves-calories (parse-input (slurp input-file)))

(defn elf-total-calories
  "Returns the total calories carried by an elf."
  [calories]
  (apply + calories))

; --------------------------
; results

(defn day01-1
  []
  (->> elves-calories
       (map elf-total-calories)
       (apply max)))

(defn day01-2
  []
  (->> elves-calories
       (map elf-total-calories)
       (sort >)
       (take 3)
       (apply +)))

(defn -main
  []
  (println (day01-1))
  (println (day01-2)))
