(ns day09.core
  (:gen-class))

; --------------------------
; utils

(defn str->int
  [s]
  (Integer/parseInt (str s)))

(defn seq-reductions
  "Similar functionality to the built-in function 'reductions' with two exceptions:
  1) The initial value is not included in the result.
  2) Short-circuits if at any step f(last item in result, el) is the same as el.
  In this case, the rest of the coll is added to the final result.
  Returns a vector."
  [f init coll]
  (loop [new-coll []
         coll coll
         init init]
    (if-let [curr (first coll)]
      (let [res (f init curr)]
        (if (= res curr)
          (into new-coll coll)
          (recur (conj new-coll res) (rest coll) res)))
      new-coll)))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-line->instruction
  "Parses an input line and returns a vector that represents an instruction.
  The first element of the vector is the direction to move and is represented by the
  :D :U :L :R keywords. The second element is the move distance (integer)."
  [line]
  (let [[direction-char num-char] (clojure.string/split line #" ")]
    [(keyword (str direction-char))
     (str->int num-char)]))

(defn input-file->instructions
  "Reads and parses the input file into a seq of instructions.
  The form of each instruction is described in function input-line->instruction."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map input-line->instruction)))

(def memoized_input-file->instructions (memoize input-file->instructions))

(defn knot-distance->move-vector
  "Given the distance vector (V2-V1) between two subsequent knots, it returns the
  vector that needs to be added to V1 so that moves right next to V2.
  Based on the problem description, the numbers in the distance vector are expected
  to be integers with absolute value <= 2."
  [[x y]]
  (if (and (not= x -2) (not= x 2) (not= y -2) (not= y 2))
    '(0 0)
    (map #(get {-2 -1, 2 1} % %) [x y])))

(defn move-head
  "Moves the head knot in the given direction and returns its new position."
  [head direction]
  (let [[x y] head]
    (case direction
      :D (list x (dec y))
      :U (list x (inc y))
      :L (list (dec x) y)
      :R (list (inc x) y))))

(defn move-knot
  "Moves the given knot so that the touches the target knot and returns
  its new position. Knots must be subsequent."
  [target-knot knot]
  (let [[x-to-move y-to-move] knot
        [x-moved y-moved] target-knot
        diff [(- x-moved x-to-move) (- y-moved y-to-move)]
        move-vector (knot-distance->move-vector diff)]
    (map + knot move-vector)))

(defn execute-instruction
  "Receives an instruction and a vector of knots (head knot is first). Executes the
  instruction and returns a vector of two elements. First one is a vector of all the
  positions of the tail knot and the second one is a vector of the final positions
  of all knots."
  [instruction knots]
  (loop [instruction instruction
         knots knots
         tail-positions []]
    (let [[direction move-amount] instruction
          head (first knots)
          rest-knots (rest knots)]
      (if (pos? move-amount)
        (let [new-head (move-head head direction)
              new-rest-knots (seq-reductions move-knot new-head rest-knots)
              new-knots (into [new-head] new-rest-knots)
              new-tail (last new-knots)
              new-tail-positions (conj tail-positions new-tail)
              updated-instruction [direction (dec move-amount)]]
          (recur updated-instruction new-knots new-tail-positions))
        [tail-positions knots]))))

(defn execute-instructions
  "Receives a seq of instructions and a vector of knots (head knot is first). Executes
  all instructions and returns the final result as a vector of two elements. First one
  is a set of all the positions of the tail knot and the second one is a vector of the
  final positions of all knots."
  [instructions initial-knots]
  (loop [instructions instructions
         knots initial-knots
         tail-positions #{'(0 0)}]
    (if (seq instructions)
      (let [instruction (first instructions)
            [new-tail-positions new-knot-positions] (execute-instruction instruction knots)
            tail-positions (into tail-positions new-tail-positions)]
        (recur (rest instructions) new-knot-positions tail-positions))
      [tail-positions knots])))

; --------------------------
; results

(defn day09
  [initial-knot-positions]
  (let [instructions (memoized_input-file->instructions)
        [tail-positions _] (execute-instructions instructions initial-knot-positions)]
    (count tail-positions)))

(defn day09-1
  []
  (day09 ['(0 0) '(0 0)]))

(defn day09-2
  []
  (day09 (vec (take 10 (repeat '(0 0))))))

(defn -main
  []
  (println (time (day09-1)))
  (println (time (day09-2))))
