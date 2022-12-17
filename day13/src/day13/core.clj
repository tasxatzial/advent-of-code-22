(ns day13.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-line->packet
  "Parses an input line into a vector."
  [line]
  (when (not= "" line)
    (read-string line)))

(defn input-file->packets
  "Reads and parses the input file into a seq of packets (vectors)."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map input-line->packet)
       (filter some?)))

(def memoized_input-file->packets (memoize input-file->packets))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
