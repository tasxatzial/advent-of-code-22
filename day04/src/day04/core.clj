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

; --------------------------
; problem1

(defn assignments-overlap_p1?
  "Returns true if any of the assignment pairs overlaps the other, false otherwise (p1)."
  [assignment-pair]
  (let [[[a1-start a1-end] [a2-start a2-end]] assignment-pair]
    (or (and (>= a1-start a2-start) (<= a1-end a2-end))
        (and (>= a2-start a1-start) (<= a2-end a1-end)))))

; --------------------------
; problem2

(defn assignments-overlap_p2?
  "Returns true if any of the assignment pairs overlaps the other, false otherwise (p2)."
  [assignment-pair]
  (let [[[a1-start a1-end] [a2-start a2-end]] assignment-pair]
    (or (<= a2-start a1-end a2-end)
        (<= a2-start a1-start a2-end)
        (<= a1-start a2-start a1-end)
        (<= a1-start a2-end a1-end))))

; --------------------------
; results

(defn day04
  [overlap_fn]
  (->> (memoized-assignment-pairs)
       (filter overlap_fn)
       count))

(defn day04-1
  []
  (day04 assignments-overlap_p1?))

(defn day04-2
  []
  (day04 assignments-overlap_p2?))

(defn -main
  []
  (println (day04-1))
  (println (day04-2)))