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

(defn input-line->droplet-position
  "Parses an input line into vector of 3 ints (droplet position)."
  [line]
  (map str->int (clojure.string/split line #",")))

(defn input-file->droplet-positions
  "Reads and parses the input file into a set of droplet positions. The form
  of each position is described in the function input-line->droplet-position."
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
  "Returns the number of adjacent positions of the given position that are
  not droplets. Both arguments must be sets."
  [droplet position]
  (let [pos-neighbors (get-neighbors position)]
    (- 6 (count (set/intersection pos-neighbors droplet)))))

(defn get-droplet-surface-area
  "Returns the total exposed area of the droplet."
  [droplet]
  (->> droplet
       (map #(count-exposed-sides droplet %))
       (apply +)))

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

(defn get-droplet-ranges
  "Returns a seq of 3 vectors, each containing the min and max position of the
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
  positions to the droplet."
  [droplet]
  (let [droplet-ranges (get-droplet-ranges droplet)
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
  it is one of the interior corners of its bounding box. This position
  is never adjacent to the droplet."
  [positions]
  (let [[[min-x _] [min-y _] [min-z _]] (get-droplet-ranges positions)]
    [(dec min-x) (dec min-y) (dec min-z)]))

(defn get-droplet-exterior-surface-area
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
  (get-droplet-surface-area (memoized_input-file->droplet-positions)))

(defn day18-2
  []
  (get-droplet-exterior-surface-area (memoized_input-file->droplet-positions)))

(defn -main
  []
  (println (day18-1))
  (println (time (day18-2))))
