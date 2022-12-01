(ns day01.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  [s]
  (Integer/parseInt s))

(defn parse-input
  "Parses the input string and returns a sequence of collections.
  Each collection represents the calories of each elf."
  [s]
  (->> s
       clojure.string/split-lines
       (partition-by #(= "" %))
       (filter #(not= '("") %))
       (map #(map str->int %))))

(def calories (parse-input (slurp input-file)))

(defn -main
  []
  (println calories))
