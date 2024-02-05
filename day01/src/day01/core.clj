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

(defn parse-file
  "Reads and parses the input file into a sequence of sequences.
  Each sequence represents the calories of each elf."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (partition-by #{""})
       (remove #{[""]})
       (map #(map str->int %))))

(def memoized-input-file->calories-by-elf (memoize parse-file))

(defn get-elf-total-calories
  "Returns the total calories carried by an elf."
  [calories]
  (reduce + calories))

; --------------------------
; results

(defn day01-1
  []
  (->> (memoized-input-file->calories-by-elf)
       (map get-elf-total-calories)
       (apply max)))

(defn day01-2
  []
  (->> (memoized-input-file->calories-by-elf)
       (map get-elf-total-calories)
       (sort >)
       (take 3)
       (reduce +)))

(defn -main
  []
  (println (day01-1))
  (println (day01-2)))
