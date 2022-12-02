(ns day02.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-input
  "Parses the input string and returns a sequence of vectors.
  Each vector represents the instructions in a round."
  [s]
  (->> s
    clojure.string/split-lines
    (map #(clojure.string/split % #" "))))

(def strategy (parse-input (slurp input-file)))

(defn -main
  []
  (println strategy))
