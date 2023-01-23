(ns day05.core
  (:gen-class))

; --------------------------
; utils

(defn str->int
  [s]
  (Integer/parseInt (str s)))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-file->lines
  "Reads the input file and splits it into lines."
  []
  (->> input-file
       slurp
       clojure.string/split-lines))

(def memoized_input-file->lines (memoize input-file->lines))

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

(defn extract-crate-and-stack-lines
  "Accepts a collection of lines and returns those that correspond to the crates-stacks data."
  [input-lines]
  (->> input-lines
       (take-while #(not= "" %))))

(defn create-stack-crate-pairs-from-line
  "Takes two lines that represent crates-stacks data and returns a sequence of
  [stack char id, crate char] pairs."
  [crate-line stack-line]
  (->> (zipmap stack-line crate-line)
       filter-valid-stack-pairs))

(defn create-stack-crate-pairs
  "Accepts a collection of lines that correspond to the crates-stacks data
  and returns a seq that contains all [stack char id, crate char] pairs."
  [crate-and-stack-lines]
  (let [input-line-stack (last crate-and-stack-lines)
        input-lines-crate (butlast crate-and-stack-lines)]
    (reduce into
            (map create-stack-crate-pairs-from-line
                 input-lines-crate (repeat input-line-stack)))))

(defn get-crates-per-stack
  "Accepts a seq of [stack char id, crate char]  pairs and organizes it
  into a map: Keys are integers representing the stack id, values are vectors
  that contain the crate chars for each stack."
  [stack-crate-pairs]
  (reduce (fn [result stack-crate-pair]
            (let [[stack-char crate-char] stack-crate-pair
                  stack-num (str->int stack-char)
                  stack-crates (get result stack-num [])]
              (assoc result stack-num (conj stack-crates crate-char))))
          {}
          stack-crate-pairs))

(defn input-stack-crates->stack-crates
  "Reads and parses the input file into a map. Keys are integers representing the stack id,
  values are vectors that contain the crate chars for each stack. Last item in each vector
  represents the top of the stack."
  []
  (->> (memoized_input-file->lines)
       extract-crate-and-stack-lines
       create-stack-crate-pairs
       get-crates-per-stack))

(def memoized_input-stack-crates->stack-crates (memoize input-stack-crates->stack-crates))

; --------------------------
; parse input into a seq that represents the instructions

(defn instruction-lines
  "Accepts a collection of lines and returns those that correspond to the instructions."
  [input-lines]
  (->> input-lines
       (drop-while #(not= "" %))
       rest))

(defn extract-instruction
  "Accepts a line that corresponds to an instruction and returns a vector that
  contains 3 integers."
  [instruction-line]
  (let [instruction-tokens (clojure.string/split instruction-line #" ")
        amount-to-move (get instruction-tokens 1)
        from-stack (get instruction-tokens 3)
        to-stack (get instruction-tokens 5)]
    (mapv str->int [amount-to-move from-stack to-stack])))

(defn input-instructions->instructions
  "Reads and parses the input file into a seq of vectors. Each vector represents an
  instruction and contains 3 integers."
  []
  (->> (memoized_input-file->lines)
       instruction-lines
       (map extract-instruction)))

(def memoized_input-instructions->instructions (memoize input-instructions->instructions))

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

; --------------------------
; results

(defn get-top-stack-crates-as-string
  [stacks]
  (->> stacks
       (into (sorted-map))
       vals
       (map last)
       (apply str)))

(defn day05
  [fn_crate-order]
  (let [stacks (memoized_input-stack-crates->stack-crates)
        instructions (memoized_input-instructions->instructions)]
    (-> instructions
        (execute-instructions stacks fn_crate-order)
        get-top-stack-crates-as-string)))

(defn day05-1
  []
  (day05 reverse))

(defn day05-2
  []
  (day05 identity))

(defn -main
  []
  (println (day05-1))
  (println (day05-2)))
