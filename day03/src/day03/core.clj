(ns day03.core
  (:require [clojure.set :as set])
  (:gen-class))

; --------------------------
; utils

(defn upperCase?
  "Returns true if the given letter is in upper case, false otherwise."
  [letter]
  (<= 65 (int letter) 90))

(defn get-common-items
  "Accepts a collection of collections and returns their intersection."
  [coll]
  (let [coll-sets (map set coll)]
    (apply set/intersection coll-sets)))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-file->rucksacks
  "Reads and parses the input file into a collection rucksacks,
  each rucksack is represented by a string."
  []
  (->> input-file
       slurp
       clojure.string/split-lines))

(def memoized-rucksacks (memoize input-file->rucksacks))

(defn get-item-priority
  "Returns the priority of an item."
  [item]
  (if (upperCase? item)
    (- (int item) 38)
    (- (int item) 96)))

; --------------------------
; problem 1

(defn extract-rucksack-compartments
  "Converts a rucksack string to its compartment representation:
  A collection of two equally sized seqs. Each seq contains chars that represent the
  items in each compartment."
  [rucksack]
  (let [rucksack-size (count rucksack)]
    (partition (/ rucksack-size 2) rucksack)))

(defn rucksacks->compartments
  "Partitions the rucksacks seq into compartments. Returns a collection that has the
  following structure: (((left compartment) (right compartment)), ...)"
  []
  (->> (memoized-rucksacks)
       (map extract-rucksack-compartments)))

; --------------------------
; problem 2

(defn rucksacks->grouped-by-3
  "Partitions the rucksacks seq into groups of 3. Returns a collection that has the
  following structure: ((rucksack1 rucksack2 rucksack3), ...)"
  []
  (->> (memoized-rucksacks)
       (partition 3)))

; --------------------------
; results

(defn day03
  [partitioned-data]
  (->> partitioned-data
       (map (comp seq get-common-items))
       flatten
       (map get-item-priority)
       (apply +)))

(defn day03-1
  []
  (day03 (rucksacks->compartments)))

(defn day03-2
  []
  (day03 (rucksacks->grouped-by-3)))

(defn -main
  []
  (println (day03-1))
  (println (day03-2)))
