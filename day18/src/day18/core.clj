(ns day18.core
  (:gen-class)
  (:require [clojure.set :as set]))

; --------------------------
; utils

(defn str->int
  [s]
  (Integer/parseInt (str s)))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses an input line into a sequence of 3 numbers representing
  a droplet position."
  [line]
  (map str->int (clojure.string/split line #",")))

(defn parse-file
  "Reads and parses the input file into a set of droplet positions. The form
  of each position is described in the function parse-line."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map parse-line)
       set))

(def memoized-input-file->droplet (memoize parse-file))

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
  "Returns the number of adjacent positions of the given position that are not
  part of the given droplet. Both arguments must be sets."
  [droplet position]
  (let [neighbors (get-neighbors position)]
    (- 6 (count (set/intersection neighbors droplet)))))

(defn get-exposed-area
  "Returns the total exposed area of the droplet."
  [droplet]
  (->> droplet
       (map #(count-exposed-sides droplet %))
       (reduce +)))

; --------------------------
; problem 2

(defn create-grid
  "Returns a seq of all [x y z] integer positions that satisfy:
  min-x <= x <= max-x
  min-y <= y <= max-y
  min-z <= z <= max-z"
  [min-x max-x min-y max-y min-z max-z]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))
        z (range min-z (inc max-z))]
    [x y z]))

(defn get-ranges
  "Returns a sequence of 3 vectors, each containing the min and max position of the
  droplet in each of the x,y,z axes."
  [droplet]
  (let [xs (map first droplet)
        ys (map second droplet)
        zs (map last droplet)
        min-xyz (map #(apply min %) [xs ys zs])
        max-xyz (map #(apply max %) [xs ys zs])]
    (map vector min-xyz max-xyz)))

(defn create-bounding-box
  "Creates a box that contains the droplet, but does not have any adjacent
  positions to the droplet. The box is represented by a set of its outer coordinates."
  [droplet]
  (let [droplet-ranges (get-ranges droplet)
        [min-x min-y min-z] (map #(- % 2) (map first droplet-ranges))
        [max-x max-y max-z] (map #(+ % 2) (map second droplet-ranges))]
    (-> #{}
        (into (create-grid min-x min-x min-y max-y min-z max-z))
        (into (create-grid max-x max-x min-y max-y min-z max-z))
        (into (create-grid min-x max-x min-y min-y min-z max-z))
        (into (create-grid min-x max-x max-y max-y min-z max-z))
        (into (create-grid min-x max-x min-y max-y min-z min-z))
        (into (create-grid min-x max-x min-y max-y max-z max-z)))))

(defn get-droplet-external-position
  "Returns a position that is always outside the droplet. More specifically,
  it is one of the inner corners of its bounding box. This position
  is never adjacent to the droplet."
  [droplet]
  (let [[[min-x _] [min-y _] [min-z _]] (get-ranges droplet)]
    [(dec min-x) (dec min-y) (dec min-z)]))

(defn get-exterior-surface-area
  "Calculates the total exterior surface area of the droplet."
  [droplet]
  (loop [exterior-area 0
         visited-steam (create-bounding-box droplet)
         expanding-steam [(get-droplet-external-position droplet)]]
    (if (seq expanding-steam)
      (let [expanding-steam-neighbors (reduce into [] (map get-neighbors expanding-steam))
            non-visited-expanding-steam-neighbors (remove #(contains? visited-steam %) expanding-steam-neighbors)
            exposed-droplet-positions (filter #(contains? droplet %) non-visited-expanding-steam-neighbors)
            new-exterior-area (+ exterior-area (count exposed-droplet-positions))
            new-visited-steam (into visited-steam expanding-steam)
            new-expanding-steam (set/difference (set non-visited-expanding-steam-neighbors) (set exposed-droplet-positions))]
        (recur new-exterior-area new-visited-steam new-expanding-steam))
      exterior-area)))

; --------------------------
; results

(defn day18-1
  []
  (get-exposed-area (memoized-input-file->droplet)))

(defn day18-2
  []
  (get-exterior-surface-area (memoized-input-file->droplet)))

(defn -main
  []
  (println (day18-1))
  (println (day18-2)))
