(ns day02.core
  (:gen-class))

; --------------------------
; utils

(defn reverse-map
  "Reverses the mapping of key->val in the given map."
  [m]
  (let [key-val (seq m)
        k (map first key-val)
        v (map second key-val)]
    (zipmap v k)))

(def choice->num
  "Maps a choice to a number."
  {:rock 0
   :paper 1
   :scissors 2})

(def num->choice
  "Maps a number to a choice. Reverses the mapping of choice->num"
  (reverse-map choice->num))

(def outcome->num
  "Maps an outcome to a number."
  {:draw 0
   :win 1
   :loss 2})

(def num->outcome
  "Maps a number to an outcome. Reverses the mapping of outcome->num."
  (reverse-map outcome->num))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-file->strategy
  "Reads and parses the input file into a collection of vectors.
  Each vector represents the -encrypted- instructions in a round."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))))

(def memoized-strategy (memoize input-file->strategy))

(defn decrypt-strategy
  "Decrypts the strategy guide using a decrypt function on each round."
  [decrypt-round-fn]
  (let [strategy (memoized-strategy)]
    (map decrypt-round-fn strategy)))

(defn round-outcome
  "Returns :loss or :win or :draw depending on the outcome of the round."
  [round]
  (let [[elf-choice my-choice] (map choice->num round)]
    (get num->outcome (mod (- my-choice elf-choice) 3))))

(def outcome->score
  "Returns the score of an outcome."
  {:win 6
   :loss 0
   :draw 3})

(def choice->score
  "Returns the score of a choice."
  {:scissors 3
   :paper 2
   :rock 1})

(defn round-score
  "Returns the total score of each round, that is, the sum of the
  shape-score and outcome-score."
  [round]
  (let [my-choice (second round)
        outcome (round-outcome round)]
    (+ (outcome->score outcome) (choice->score my-choice))))

(defn total-score
  "Calculates the total score when all rounds have ended."
  [strategy]
  (->> strategy
       (map round-score)
       (apply +)))

; --------------------------
; problem 1

(def decrypt-symbol-p1
  "Decrypts a symbol (p1)."
  {"A" :rock,
   "X" :rock
   "B" :paper,
   "Y" :paper
   "C" :scissors,
   "Z" :scissors})

(defn decrypt-round-p1
  "Decrypts a round (p1)."
  [encrypted-round]
  (mapv decrypt-symbol-p1 encrypted-round))

; --------------------------
; problem 2

(def decrypt-symbol-p2
  "Decrypts a symbol (p2)."
  {"A" :rock
   "B" :paper
   "C" :scissors
   "Y" :draw
   "X" :loss
   "Z" :win})

(defn find-my-choice
  "Returns what I should play based on a given round (p2)."
  [round]
  (let [[elf-choice outcome] round
        elf-choice-num (choice->num elf-choice)
        outcome-num (outcome->num outcome)]
    (get num->choice (mod (+ elf-choice-num outcome-num) 3))))

(defn decrypt-round-p2
  "Decrypts a round (p2). Returns a vector that contains the choices
  of both players."
  [encrypted-round]
  (let [decrypted-round (map #(get decrypt-symbol-p2 %) encrypted-round)
        elf-choice (first decrypted-round)
        my-choice (find-my-choice decrypted-round)]
    [elf-choice my-choice]))

; --------------------------
; results

(defn day02
  [decrypt-round-fn]
  (let [decrypted-strategy (decrypt-strategy decrypt-round-fn)]
    (total-score decrypted-strategy)))

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
