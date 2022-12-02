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

(defn parse-file
  "Reads and parses the input file into a string."
  []
  (->> input-file
       slurp
       parse-input))

(def memoized-parse-file (memoize parse-file))

(defn decrypt-strategy
  "Decrypts the parsed input using a decrypt function on each round."
  [decrypt-fn]
  (let [parsed-file (memoized-parse-file)]
    (map decrypt-fn parsed-file)))

(defn round-outcome
  "Returns :loss or :win or :draw depending on the outcome of the round."
  [round]
  (case round
    ([:scissors :paper] [:paper :rock] [:rock :scissors]) :loss
    ([:paper :scissors] [:rock :paper] [:scissors :rock]) :win
    ([:paper :paper] [:scissors :scissors] [:rock :rock]) :draw))

(defn outcome-score
  "Returns the score of the outcome of a round: 6 if won, 0 if lost, 3 if draw."
  [round]
  (case (round-outcome round)
    :win 6
    :loss 0
    :draw 3))

(defn shape-score
  "Returns the score of each shape: 3 for :scissors, 2 for :paper, 1 for :rock."
  [s]
  (case s
    :scissors 3
    :paper 2
    :rock 1))

(defn round-score
  "Returns the total score of each round, that is, the sum of the
  shape-score and outcome-score."
  [round]
  (let [my-selection (second round)]
    (+ (outcome-score round) (shape-score my-selection))))

(defn total-score
  "Calculates the total score when all rounds have ended."
  [strategy]
  (->> strategy
       (map round-score)
       (apply +)))

; --------------------------
; problem 1

(defn decrypt-symbol-p1
  "Decrypt a symbol (p1).
  Returns:
  :rock for symbols A and X
  :paper for symbols B and Y
  :scissors for symbols C and Z"
  [s]
  (case s
    ("A" "X") :rock
    ("B" "Y") :paper
    ("C" "Z") :scissors))

(defn decrypt-round-p1
  "Decrypts a round that consists of two symbols (p1)."
  [round]
  (map decrypt-symbol-p1 round))

; --------------------------
; results

(defn day02
  [decrypt-fn]
  (let [decrypted-strategy (decrypt-strategy decrypt-fn)]
    (total-score decrypted-strategy)))

(defn day02-1
  []
  (day02 decrypt-round-p1))

(defn -main
  []
  (println (day02-1)))
