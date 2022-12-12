(ns day10.core
  (:gen-class))

; --------------------------
; utils

(defn str->int
  [s]
  (Integer/parseInt (str s)))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-line->cmd
  "Parses an input line into a command. Returns a vector, it's first element is a
  keyword that describes the command, and the second element, if it exists,
  is an integer."
  [line]
  (let [[op val] (clojure.string/split line #" ")]
    (if val
      [(keyword op) (str->int val)]
      [(keyword op)])))

(defn input-file->cmds
  "Reads and parses the input file into a vector of commands. The form of each
  command is described in the function input-line->cmd."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (mapv input-line->cmd)))

(def memoized_input-file->cmds (memoize input-file->cmds))

(defn -main
  []
  (println (memoized_input-file->cmds)))
