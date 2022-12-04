(ns day04.core
    (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-assignment->assignment
      "Parses an assignment from the input file into an assignment that has the form (A B)"
      [assignment]
      (->> (clojure.string/split assignment #"-")
           (map #(Integer/parseInt %))))

(defn input-line->assignment-pair
      "Parses an input line into an assignment pair that has the form ((A B) (C D))"
      [line]
      (->> (clojure.string/split line #",")
           (map input-assignment->assignment)))

(defn input-file->assignment-pairs
      "Reads and parses the input file into a collection of assignment pairs.
      Each pair has the form ((A B) (C D))"
      []
      (->> input-file
           slurp
           clojure.string/split-lines
           (map input-line->assignment-pair)))

(def memoized-assignment-pairs (memoize input-file->assignment-pairs))

(defn -main
      []
      (println (memoized-assignment-pairs)))