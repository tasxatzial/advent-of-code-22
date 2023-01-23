(ns day04.core
    (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-assignment->assignment
  "Parses an assignment that is represented by the string A-B into a seq
   of two integers A and B."
  [assignment]
  (->> (clojure.string/split assignment #"-")
       (map #(Integer/parseInt %))))

(defn input-line->assignment-pair
  "Parses an input line into a seq of two assignments. The form of each
  assignment is described in function input-assignment->assignment."
  [line]
  (->> (clojure.string/split line #",")
       (map input-assignment->assignment)))

(defn input-file->assignment-pairs
  "Reads and parses the input file into a seq of assignment pairs.
  The form of each pair is described in function input-line->assignment-pair."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map input-line->assignment-pair)))

(def memoized-assignment-pairs (memoize input-file->assignment-pairs))

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
  [fn_overlap]
  (->> (memoized-assignment-pairs)
       (filter fn_overlap)
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
