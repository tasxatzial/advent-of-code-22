(ns day03.core
  (:require [clojure.set :as set])
  (:gen-class))

; --------------------------
; utils

(defn upperCase?
  "Returns true if the given letter char is in upper case, false otherwise."
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
  "Reads and parses the input file into a sequence of rucksacks,
  each rucksack is represented by a string."
  []
  (->> input-file
       slurp
       clojure.string/split-lines))

(def memoized-rucksacks (memoize input-file->rucksacks))

(defn get-item-priority
  "Returns the priority of an item. An item is represented by a letter char."
  [item]
  (if (upperCase? item)
    (- (int item) 38)
    (- (int item) 96)))

; --------------------------
; problem 1

(defn extract-rucksack-compartments
  "Converts a rucksack string to its compartment representation:
  A sequence of two equally sized sequences that contain the chars that represent
  the items in each compartment."
  [rucksack]
  (let [rucksack-size (count rucksack)]
    (partition (/ rucksack-size 2) rucksack)))

(defn rucksacks->compartments
  "Partitions a sequence of rucksacks into compartments."
  [rucksacks]
  (map extract-rucksack-compartments rucksacks))

; --------------------------
; problem 2

(defn rucksacks->grouped-by-3
  "Partitions a sequence of rucksacks into groups of 3."
  [rucksacks]
  (partition 3 rucksacks))

; --------------------------
; results

(defn day03
  [rucksacks fn_partition-rucksacks]
  (->> (fn_partition-rucksacks rucksacks)
       (map (comp seq get-common-items))
       flatten
       (map get-item-priority)
       (reduce +)))

(defn day03-1
  []
  (let [rucksacks (memoized-rucksacks)]
    (day03 rucksacks rucksacks->compartments)))

(defn day03-2
  []
  (let [rucksacks (memoized-rucksacks)]
    (day03 rucksacks rucksacks->grouped-by-3)))

(defn -main
  []
  (println (day03-1))
  (println (day03-2)))
