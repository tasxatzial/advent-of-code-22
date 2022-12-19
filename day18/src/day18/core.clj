(ns day18.core
  (:gen-class))


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

(defn -main
  []
  (println (memoized_input-file->droplet-positions)))
