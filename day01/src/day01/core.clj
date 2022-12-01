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

; --------------------------
; problem 1

(defn findMostCalories
  []
  (->> calories
      (map #(apply + %))
      (apply max)))

; --------------------------
; results

(defn day01-1
  []
  (findMostCalories))

(defn -main
  []
  (println (day01-1)))
