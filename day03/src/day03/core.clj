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

(defn rucksack->compartments
  "Converts a string that represents a rucksack to its compartment representation:
  A collection of two equally sized lists, each containing the items in a compartment."
  [rucksack]
  (let [rucksack-size (count rucksack)]
    (partition (/ rucksack-size 2) rucksack)))

(defn input-file->rucksacks
  "Reads and parses the input file into a collection rucksacks. Each rucksack
  is split into two compartments."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map rucksack->compartments)))

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
