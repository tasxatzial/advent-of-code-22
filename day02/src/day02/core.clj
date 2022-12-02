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

(defn decrypt-symbol
  "Decrypt a symbol. Returns:
  :rock for symbols A and X
  :paper for symbols B and Y
  :scissors for symbols C and Z"
  [s]
  (case s
    ("A" "X") :rock
    ("B" "Y")  :paper
    ("C" "Z") :scissors))

(defn decrypt-strategy
  "Decrypts the strategy guide."
  [strategy]
  (map #(map decrypt-symbol %) strategy))

(def decrypted-strategy
  (-> input-file
      slurp
      parse-input
      decrypt-strategy))

(defn -main
  []
  (println decrypted-strategy))
