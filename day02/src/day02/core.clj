(ns day02.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-file->coll
  "Reads and parses the input file into a collection of vectors.
  Each vector represents the -unencrypted- instructions in a round."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))))

(def memoized-data (memoize input-file->coll))

(defn decrypt-strategy-guide
  "Decrypts the strategy guide using a decrypt function on each round."
  [decrypt-round-fn]
  (let [data (memoized-data)]
    (map decrypt-round-fn data)))

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

(defn choice-score
  "Returns the score of a choice: 3 for :scissors, 2 for :paper, 1 for :rock."
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
    (+ (outcome-score round) (choice-score my-selection))))

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
  [encrypted-round]
  (map decrypt-symbol-p1 encrypted-round))

; --------------------------
; problem 2

(defn decrypt-symbol-p2
  "Decrypt a symbol (p2).
  Returns:
  :rock for symbol A
  :paper for symbol B
  :scissors for symbol C
  :draw for symbol Y
  :loss for symbol X
  :win for symbol Z"
  [s]
  (case s
    "A" :rock
    "B" :paper
    "C" :scissors
    "Y" :draw
    "X" :loss
    "Z" :win))

(defn find-my-choice
  "Returns what I should play based on a given round (p2)."
  [round]
  (let [[elf-choice outcome] round]
    (case elf-choice
      :rock (case outcome
              :loss :scissors
              :draw :rock
              :win :paper)
      :paper (case outcome
               :loss :rock
               :draw :paper
               :win :scissors)
      :scissors (case outcome
                  :loss :paper
                  :draw :scissors
                  :win :rock))))

(defn decrypt-round-p2
  "Decrypts a round that consists of two symbols (p2)."
  [encrypted-round]
  (let [decrypted-round (map decrypt-symbol-p2 encrypted-round)
        elf-choice (first decrypted-round)
        my-choice (find-my-choice decrypted-round)]
    [elf-choice my-choice]))

; --------------------------
; results

(defn day02
  [decrypt-round-fn]
  (let [strategy-guide (decrypt-strategy-guide decrypt-round-fn)]
    (total-score strategy-guide)))

(defn day02-1
  []
  (day02 decrypt-round-p1))

(defn day02-2
  []
  (day02 decrypt-round-p2))

(defn -main
  []
  (println (day02-1))
  (println (day02-2)))
