(ns day04.core
    (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-assignment
  "Parses an assignment that is represented by the string A-B into a sequence
   of two integers."
  [assignment]
  (->> (clojure.string/split assignment #"-")
       (map #(Integer/parseInt %))))

(defn parse-line
  "Parses an input line into a sequence of two assignments. The form of each
  assignment is described in the function parse-assignment."
  [line]
  (->> (clojure.string/split line #",")
       (map parse-assignment)))

(defn parse-file
  "Reads and parses the input file into a sequence of assignment pairs.
  The form of each pair is described in the function parse-line."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map parse-line)))

(def memoized-input-file->assignment-pairs (memoize parse-file))

; --------------------------
; problem1

(defn p1_assignments-overlap?
  "Returns true if any of the assignment pairs overlaps the other, false otherwise."
  [assignment-pair]
  (let [[[a1-start a1-end] [a2-start a2-end]] assignment-pair]
    (or (and (>= a1-start a2-start) (<= a1-end a2-end))
        (and (>= a2-start a1-start) (<= a2-end a1-end)))))

; --------------------------
; problem2

(defn p2_assignments-overlap?
  "Returns true if any of the assignment pairs overlaps the other, false otherwise."
  [assignment-pair]
  (let [[[a1-start a1-end] [a2-start a2-end]] assignment-pair]
    (or (<= a2-start a1-end a2-end)
        (<= a2-start a1-start a2-end)
        (<= a1-start a2-start a1-end)
        (<= a1-start a2-end a1-end))))

; --------------------------
; results

(defn day04
  [f]
  (->> (memoized-input-file->assignment-pairs)
       (filter f)
       count))

(defn day04-1
  []
  (day04 p1_assignments-overlap?))

(defn day04-2
  []
  (day04 p2_assignments-overlap?))

(defn -main
  []
  (println (day04-1))
  (println (day04-2)))
