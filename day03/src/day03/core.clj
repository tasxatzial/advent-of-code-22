(ns day03.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-file->rucksacks
  "Reads and parses the input file into a collection of sets.
  Each set represents the contents in a rucksack."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map set)))

(def memoized-rucksacks (memoize input-file->rucksacks))

(defn -main
  []
  (println (memoized-rucksacks)))
