(ns day03.core
  (:require [clojure.set :as set])
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

(defn rucksack->compartments
  "Converts a string that represents a rucksack to its compartment representation:
  A collection of two equally sized lists, each containing the items in a compartment."
  [rucksack]
  (let [rucksack-size (count rucksack)]
    (partition (/ rucksack-size 2) rucksack)))

(defn get-compartment-common-items
  "Returns a set of the items that appear in both compartments of each rucksack."
  [rucksack]
  (let [compartments (rucksack->compartments rucksack)
        compartments-set (map set compartments)]
    (apply set/intersection compartments-set)))

; --------------------------
; problem 2

(defn get-rucksacks-common-items
  "Returns a set of the common items in the given rucksack collection."
  [rucksacks]
  (let [rucksacks-set (map set rucksacks)]
    (apply set/intersection rucksacks-set)))

; --------------------------
; results

(defn day03-1
  []
  (->> (memoized-rucksacks)
       (map (comp seq get-compartment-common-items))
       flatten
       (map get-item-priority)
       (apply +)))

(defn day03-2
  []
  (->> (memoized-rucksacks)
       (partition 3)
       (map (comp seq get-rucksacks-common-items))
       flatten
       (map get-item-priority)
       (apply +)))

(defn -main
  []
  (println (day03-1))
  (println (day03-2)))
