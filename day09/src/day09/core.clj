(ns day09.core
  (:gen-class))

; --------------------------
; utils

(defn str->int
  [s]
  (Integer/parseInt (str s)))

(defn reduce-repeatedly
  "Reduces the given collection given a 2-arity function and an initial value.
  Traverses coll and for each element it evaluates f(R, el) using as R the
  result from the previous step. The given val is used to initialize R.
  It short-circuits if the result f(R, el) is the same as el."
  [f val coll]
  (loop [new-coll []
         coll coll
         init val]
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
  vector that needs to be added to V1 so that it touches V2.
  The numbers in the distance vector should be integers with absolute value <= 2."
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
  "Moves the given knot towards the target knot and returns its new position.
  Both knots must be subsequent."
  [target-knot knot]
  (let [[x-to-move y-to-move] knot
        [x-moved y-moved] target-knot
        diff [(- x-moved x-to-move) (- y-moved y-to-move)]
        move-vector (knot-distance->move-vector diff)]
    (map + knot move-vector)))

(defn execute-instruction
  "Multi-arity function.
  1) If called with 2 arguments, it receives an instruction and a seq of knots,
  and executes the instruction returning a vector of two elements. First one is
  a vector of the all the positions of the tail knot and the second one is a
  vector of the final positions of all knots.
  2) If called with 3 arguments, it can also receive a starting vector of tail
  positions."
  ([instruction knots]
   (execute-instruction instruction knots []))
  ([instruction knots tail-positions]
   (let [[direction move-amount] instruction
         head (first knots)
         rest-knots (rest knots)]
     (if (pos? move-amount)
       (let [new-head (move-head head direction)
             new-rest-knots (reduce-repeatedly move-knot new-head rest-knots)
             new-knots (into [new-head] new-rest-knots)
             new-tail (last new-knots)
             new-tail-positions (conj tail-positions new-tail)
             updated-instruction [direction (dec move-amount)]]
         (recur updated-instruction new-knots new-tail-positions))
       [tail-positions knots]))))

(defn -main
  []
  (println (memoized_input-file->instructions)))
