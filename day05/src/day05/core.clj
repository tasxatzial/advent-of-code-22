(ns day05.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-file->input-str
  "Reads the input file into a string."
  []
  (->> input-file
       slurp
       clojure.string/split-lines))

(def memoized_input-file->input-str (memoize input-file->input-str))

; --------------------------
; parse input into a map that represents the stacks

(defn crateChar?
  "Returns true if the given char represents a crate."
  [c]
  (<= 65 (int c) 90))

(defn filter-valid-stack-pairs
  "Accepts a sequence of [stack char id, crate char] pairs and collects only the pairs that
  have a valid crate char."
  [pairs]
  (->> pairs
       (filter #(crateChar? (second %)))))

(defn crate-and-stack-lines
  "Accepts the input string and returns the lines that correspond to the crates-stacks data."
  [input-str]
  (->> input-str
       (take-while #(not= "" %))))

(defn line-stack-crate-pairs
  "Accepts a line that represents the crates-stacks data and returns a sequence of
  its [stack char id, crate char] pairs."
  [crate-line stack-line]
  (->> (zipmap stack-line crate-line)
       filter-valid-stack-pairs))

(defn stack-crate-pairs
  "Accepts a sequence of lines that correspond to the crates-stacks data
  and returns a new sequence that contains all [stack char id, crate char] pairs."
  [crate-and-stack-lines]
  (let [input-line-stack (last crate-and-stack-lines)
        input-lines-crate (butlast crate-and-stack-lines)]
    (reduce into
            (map line-stack-crate-pairs
                 input-lines-crate (repeat input-line-stack)))))

(defn crates-per-stack
  "Accepts a collection of (stack char id, crate char) pairs and organizes it
  into a map: Keys are integers representing the stack id, values are vectors
  that contain the crate chars for each stack."
  [stack-crate-pairs]
  (reduce (fn [result stack-crate-pair]
            (let [[stack-char crate-char] stack-crate-pair
                  stack-num (Integer/parseInt (str stack-char))
                  stack-crates (get result stack-num [])]
              (assoc result stack-num (conj stack-crates crate-char))))
          {} stack-crate-pairs))

(defn input-stack-crates->stack-crates
  "Parses the input lines that represent the stack-crate data into a map:
  Keys are integers representing the stack id, values are vectors that
  contain the crate chars for each stack."
  []
  (->> (memoized_input-file->input-str)
       crate-and-stack-lines
       stack-crate-pairs
       crates-per-stack))

; --------------------------
; parse input into a seq that represents the instructions

(defn instruction-lines
  "Accepts the input string and returns the lines that correspond to the instructions."
  [input-str]
  (->> input-str
       (drop-while #(not= "" %))
       rest))

(defn extract-instruction
  "Accepts a line that corresponds to an instruction and converts it to a seq that
  contains 3 integers."
  [instruction-line]
  (let [instruction-tokens (clojure.string/split instruction-line #" ")
        amount-to-move (get instruction-tokens 1)
        from-stack (get instruction-tokens 3)
        to-stack (get instruction-tokens 5)]
    (map #(Integer/parseInt %)
         [amount-to-move from-stack to-stack])))

(defn input-instructions->instructions
  "Parses the input lines that correspond to the instructions into a seq of
  seqs. Each seq represents an instruction and contains 3 integers."
  []
  (->> (memoized_input-file->input-str)
       instruction-lines
       (map extract-instruction)))

; --------------------------
; execute instructions

(defn move-crates
  "Returns the new stacks after an instruction has been executed.
  fn_crate-order determines the order of insertion in the target stack."
  [stacks instruction fn_crate-order]
  (let [amount-to-move (first instruction)
        src-stack-num (second instruction)
        src-stack (get stacks src-stack-num)
        target-stack-num (last instruction)
        target-stack (get stacks target-stack-num)
        new-src-stack (vec (drop-last amount-to-move src-stack))
        removed-crates (fn_crate-order (take-last amount-to-move src-stack))
        new-target-stack (into target-stack removed-crates)]
    (-> stacks
        (assoc src-stack-num new-src-stack)
        (assoc target-stack-num new-target-stack))))

(defn execute-instructions
  "Executes all instructions and returns the final stacks.
  fn_crate-order determines the order of insertion in the target stack."
  [instructions stacks fn_crate-order]
  (reduce (fn [result instruction]
            (move-crates result instruction fn_crate-order))
          stacks
          instructions))

(defn -main
  []
  (println (input-instructions->instructions)))
