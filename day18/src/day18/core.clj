(ns day18.core
  (:gen-class)
  (:require [clojure.set :as set]))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-line->droplet-position
  "Parses an input line into a position (vector of 3 ints)."
  [line]
  (map #(Integer/parseInt %) (clojure.string/split line #",")))

(defn input-file->droplet-positions
  "Reads and parses the input file into a seq of positions.
  Each position is a vector of 3 ints."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map input-line->droplet-position)
       set))

(def memoized_input-file->droplet-positions
  (memoize input-file->droplet-positions))

(defn get-neighbors
  "Returns a set with the 6 adjacent positions of the given position."
  [position]
  (let [[x y z] position]
    #{[(inc x) y z] [(dec x) y z]
      [x (inc y) z] [x (dec y) z]
      [x y (inc z)] [x y (dec z)]}))

; --------------------------
; problem 1

(defn count-exposed-sides
  "Returns the number of adjacent positions to the given pos that
  are not lava droplets. Both arguments must be sets."
  [droplet droplet-pos]
  (let [pos-neighbors (get-neighbors droplet-pos)]
    (- 6 (count (set/intersection pos-neighbors droplet)))))

(defn get-surface-area
  "Returns the total exposed area of the droplet."
  [droplet]
  (->> droplet
       (map #(count-exposed-sides droplet %))
       (apply +)))

; --------------------------
; results

(defn day18-1
  []
  (let [positions (memoized_input-file->droplet-positions)]
    (get-surface-area positions)))

(defn -main
  []
  (println (day18-1)))
