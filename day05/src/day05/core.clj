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

(defn extract-stack-crate-lines
  "Reads the input file and returns those lines that contain the crates and stack ids."
  []
  (->> (memoized_input-file->lines)
       (take-while #(not= "" %))))

(defn get-stack-ids
  "Reads the line that contains the stack ids and returns them as a sequence of integers."
  [stack-line]
  (->> (clojure.string/split stack-line #" ")
       (filter #(not= "" %))
       (map str->int)))

(defn get-crate-indices
  "Generates a sequence of the string indices that contain a crate char."
  [stack-ids]
  (take (count stack-ids) (range 1 100 4)))

(defn get-stack-line
  "Returns the line that contains the stack ids given a sequence of lines
  that contains both the stack ids and the crates."
  [stack-crate-lines]
  (last stack-crate-lines))

(defn get-crate-lines
  "Returns the lines that contains the crates given a sequence of lines
  that contains both the stack ids and the crates. The lines will appear
  in reverse order."
  [stack-crate-lines]
  (reverse (drop-last stack-crate-lines)))

(defn create-stack
  "Returns as a vector the crate chars at the given string index from all lines."
  [crate-lines crate-index]
  (->> crate-lines
       (map #(nth % crate-index))
       (filterv #(not= \space %))))

(defn create-stacks
  "Reads and parses the input file into a map of {stack id} -> {stack of crates}.
  Each stack of crates is represented by a vector and its last item is the top
  of the stack."
  []
  (let [stack-crate-lines (extract-stack-crate-lines)
        stack-line (get-stack-line stack-crate-lines)
        stack-ids (get-stack-ids stack-line)
        crate-indices (get-crate-indices stack-ids)
        crate-lines (get-crate-lines stack-crate-lines)]
    (loop [stack-ids stack-ids
           crate-indices crate-indices
           result {}]
      (if (seq stack-ids)
        (let [crates (create-stack crate-lines (first crate-indices))
              new-result (assoc result (first stack-ids) crates)]
          (recur (rest stack-ids) (rest crate-indices) new-result))
        result))))

(def memoized_create-stacks (memoize create-stacks))

; --------------------------
; parse input into a sequence that represents the instructions

(defn extract-instruction
  "Accepts a line that corresponds to an instruction and returns a vector that
  contains just the 3 integers in that line."
  [instruction-line]
  (let [instruction-tokens (clojure.string/split instruction-line #" ")
        amount-to-move (get instruction-tokens 1)
        from-stack (get instruction-tokens 3)
        to-stack (get instruction-tokens 5)]
    (mapv str->int [amount-to-move from-stack to-stack])))

(defn create-instructions
  "Reads and parses the input file into a sequence of vectors. Each vector represents
  an instruction and contains 3 integers."
  []
  (->> (memoized_input-file->lines)
       (drop-while #(not= "" %))
       rest
       (map extract-instruction)))

(def memoized_create-instructions (memoize create-instructions))

; --------------------------
; execute instructions

(defn move-crates
  "Returns the new stacks after an instruction has been executed.
  fn_crate-order determines the order of insertion in the target stack."
  [stacks instruction fn_crate-order]
  (let [[amount-to-move src-stack-id target-stack-id] instruction
        src-stack (get stacks src-stack-id)
        target-stack (get stacks target-stack-id)
        new-src-stack (subvec src-stack 0 (- (count src-stack) amount-to-move))
        removed-crates (fn_crate-order (subvec src-stack (- (count src-stack) amount-to-move)))
        new-target-stack (into target-stack removed-crates)]
    (-> stacks
        (assoc src-stack-id new-src-stack)
        (assoc target-stack-id new-target-stack))))

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
  (let [stacks (memoized_create-stacks)
        instructions (memoized_create-instructions)]
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
