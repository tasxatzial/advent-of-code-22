(ns day01.core
  (:gen-class))

; --------------------------
; utils

(defn str->int
  [s]
  (Integer/parseInt s))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn partition-input-by-elf
  "Partitions the input string by the calories of each elf."
  [input-str]
  (->> input-str
       (partition-by #{""})
       (remove #{[""]})))

(defn input-file->elves-calories
  "Reads and parses the input file into a sequence of sequences.
  Each sequence represents the calories of each elf."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       partition-input-by-elf
       (map #(map str->int %))))

(def memoized_input-file->elves-calories (memoize input-file->elves-calories))

(defn elf-total-calories
  "Returns the total calories carried by an elf."
  [calories]
  (reduce + calories))

; --------------------------
; results

(defn day01-1
  []
  (->> (memoized_input-file->elves-calories)
       (map elf-total-calories)
       (apply max)))

(defn day01-2
  []
  (->> (memoized_input-file->elves-calories)
       (map elf-total-calories)
       (sort >)
       (take 3)
       (reduce +)))

(defn -main
  []
  (println (day01-1))
  (println (day01-2)))
