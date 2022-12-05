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
  "Maps a number to a choice. Reverses the mapping of choice->num."
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

(defn input-file->strategy-guide
  "Reads and parses the input file into a collection of vectors.
  Each vector represents the -encrypted- instructions in a round
  and contains 2 single-char strings."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))))

(def memoized-input-file->strategy-guide (memoize input-file->strategy-guide))

(defn decrypt-strategy
  "Decrypts the strategy guide using a decrypt function on each round.
  Returns a seq of vectors. Each vector contains two keywords that represent
  the players' choices."
  [strategy decrypt-round-fn]
  (map decrypt-round-fn strategy))

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
  choice-score and outcome-score."
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

(def p1_decrypt-symbol
  "Decrypts a symbol that represents a player's choice."
  {"A" :rock,
   "X" :rock
   "B" :paper,
   "Y" :paper
   "C" :scissors,
   "Z" :scissors})

(defn p1_decrypt-round
  "Decrypts a round that is represented by 2 single-char strings.
  Returns a vector that contains two keywords that represent the players' choices."
  [encrypted-round]
  (mapv p1_decrypt-symbol encrypted-round))

; --------------------------
; problem 2

(def p2_decrypt-symbol
  "Decrypts a symbol that represents either a player's choice or an outcome."
  {"A" :rock
   "B" :paper
   "C" :scissors
   "Y" :draw
   "X" :loss
   "Z" :win})

(defn find-my-choice
  "Returns what I should play based on a round that is represented by
  a seq of two keywords, the first one is the elf choice, the second one is
  the round outcome."
  [round]
  (let [[elf-choice outcome] round
        elf-choice-num (choice->num elf-choice)
        outcome-num (outcome->num outcome)]
    (get num->choice (mod (+ elf-choice-num outcome-num) 3))))

(defn p2_decrypt-round
  "Decrypts a round that is represented by 2 single-char strings.
  Returns a vector that contains two keywords that represent the player choices."
  [encrypted-round]
  (let [round (map #(get p2_decrypt-symbol %) encrypted-round)
        elf-choice (first round)
        my-choice (find-my-choice round)]
    [elf-choice my-choice]))

; --------------------------
; results

(defn day02
  [decrypt-round-fn]
  (let [strategy (memoized-input-file->strategy-guide)
        decrypted-strategy (decrypt-strategy strategy decrypt-round-fn)]
    (total-score decrypted-strategy)))

(defn day02-1
  []
  (day02 p1_decrypt-round))

(defn day02-2
  []
  (day02 p2_decrypt-round))

(defn -main
  []
  (println (day02-1))
  (println (day02-2)))
