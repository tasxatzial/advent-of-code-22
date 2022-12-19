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
; problem 2

(defn create-grid
  "Returns a seq of all [x y z] int positions that are contained in the given ranges."
  [min-x max-x min-y max-y min-z max-z]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))
        z (range min-z (inc max-z))]
    [x y z]))

(defn get-xyz-ranges
  "Returns a seq of 3 vectors, each vector contains the min and max position of the
  droplet for each of the x,y,z axes respectively."
  [droplet]
  (let [xs (map first droplet)
        ys (map second droplet)
        zs (map last droplet)
        min-xyz (map #(apply min %) [xs ys zs])
        max-xyz (map #(apply max %) [xs ys zs])]
    (map vector min-xyz max-xyz)))

(defn create-bounding-box
  "Creates a bounding box around the droplet. The box is sufficiently larger than
  the droplet so that none of its positions are adjacent to the droplet."
  [droplet]
  (let [xyz-ranges (get-xyz-ranges droplet)
        [min-x min-y min-z] (map #(- % 2) (map first xyz-ranges))
        [max-x max-y max-z] (map #(+ % 2) (map second xyz-ranges))]
    (-> #{}
        (into (create-grid min-x min-x min-y max-y min-z max-z))
        (into (create-grid max-x max-x min-y max-y min-z max-z))
        (into (create-grid min-x max-x min-y min-y min-z max-z))
        (into (create-grid min-x max-x max-y max-y min-z max-z))
        (into (create-grid min-x max-x min-y max-y min-z min-z))
        (into (create-grid min-x max-x min-y max-y max-z max-z)))))

(defn get-outside-droplet-position
  "Returns a position that is always outside the droplet (one of the interior
  corners of its bounding box)"
  [positions]
  (let [[[min-x _] [min-y _] [min-z _]] (get-xyz-ranges positions)]
    [(dec min-x) (dec min-y) (dec min-z)]))

(defn get-exterior-surface-area
  "Calculates the total exterior surface area of the droplet."
  [droplet]
  (loop [exterior-area 0
         visited-steam (create-bounding-box droplet)
         steam [(get-outside-droplet-position droplet)]]
    (if (seq steam)
      (let [steam-neighbors (reduce into [] (map get-neighbors steam))
            non-visited-steam-neighbors (filter #(not (contains? visited-steam %)) steam-neighbors)
            exposed-droplet-positions (filter #(contains? droplet %) non-visited-steam-neighbors)
            new-exterior-area (+ exterior-area (count exposed-droplet-positions))
            new-visited-steam (into visited-steam steam)
            new-steam (set/difference (set non-visited-steam-neighbors) (set exposed-droplet-positions))]
        (recur new-exterior-area new-visited-steam new-steam))
      exterior-area)))

; --------------------------
; results

(defn day18-1
  []
  (let [positions (memoized_input-file->droplet-positions)]
    (get-surface-area positions)))

(defn day18-2
  []
  (let [positions (memoized_input-file->droplet-positions)]
    (get-exterior-surface-area positions)))

(defn -main
  []
  (println (day18-1))
  (println (day18-2)))
