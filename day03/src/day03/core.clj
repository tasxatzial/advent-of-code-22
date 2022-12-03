(ns day03.core
  (:gen-class))

; --------------------------
; utils

(defn upperCase?
  "Returns true if the given letter is in upper case, false otherwise."
  [letter]
  (<= 65 (int letter) 90))

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

(defn get-item-priority
  "Returns the priority of an item."
  [item]
  (if (upperCase? item)
    (- (int item) 38)
    (- (int item) 96)))

(defn -main
  []
  (println (memoized-rucksacks)))
