(ns day09.core
  (:gen-class))

; --------------------------
; utils

(defn str->int
  [s]
  (Integer/parseInt (str s)))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-line->instruction
  "Parses an input line and returns a vector that represents an instruction.
  The first element of the vector is the direction to move and is represented by the
  :D :U :L :R keywords. The second element is the move distance (integer)."
  [line]
  (let [[direction-char num-char] (clojure.string/split line #" ")]
    [(keyword (str direction-char))
     (str->int num-char)]))

(defn input-file->instructions
  "Reads and parses the input file into a seq of instructions.
  The form of each instruction is described in function input-line->instruction."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map input-line->instruction)))

(def memoized_input-file->instructions (memoize input-file->instructions))

(defn -main
  []
  (println (memoized_input-file->instructions)))
