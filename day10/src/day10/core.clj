(ns day10.core
  (:gen-class))

; --------------------------
; utils

(defn str->int
  [s]
  (Integer/parseInt (str s)))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-line->cmd
  "Parses an input line into a command. Returns a vector, it's first element is a
  keyword that describes the command, and the second element, if it exists,
  is an integer."
  [line]
  (let [[op val] (clojure.string/split line #" ")]
    (if val
      [(keyword op) (str->int val)]
      [(keyword op)])))

(defn input-file->cmds
  "Reads and parses the input file into a vector of commands. The form of each
  command is described in the function input-line->cmd."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (mapv input-line->cmd)))

(def memoized_input-file->cmds (memoize input-file->cmds))

(def get-cycle-cost
  {:addx 2
   :noop 1})

(defn exec-cmd
  "Accepts a vector the represents a command and the current value of the
  register. Returns a vector of all the register values that have been
  generated when the command is completed."
  [cmd register]
  (let [[op val] cmd
        cycle-cost (get-cycle-cost op)
        first-registers (vec (take (dec cycle-cost) (repeat register)))]
    (case op
      :addx (conj first-registers (+ val register))
      (conj first-registers register))))

(defn get-cycles-register
  "Accepts a vector of commands and an initial register. Returns a vector
  that has the value of the register in each cycle when all commands
  have been completed. Includes the initial value of the register."
  [cmds register]
  (reduce (fn [result cmd]
            (let [last-register (last result)
                  new-registers (exec-cmd cmd last-register)]
              (into result new-registers)))
          [register]
          cmds))

; --------------------------
; problem 2

(defn get-pixel-val
  "Returns the value of the pixel given its index and a given sprite.
  Sprites are vectors that consist of 3 pixel indices."
  [pixel-index sprite]
  (if (some #(= pixel-index %) sprite)
    \#
    \.))

(defn draw-crt
  "Executes the given commands given an initial register value and returns
  the output of the crt. The result is a seq of seqs, each seq has the final
  pixel values in each line of the crt."
  [cmds register]
  (let [crt-length 240
        crt-line-length 40]
    (loop [registers (take crt-length (get-cycles-register cmds register))
           crt []
           pixel 0
           cmds cmds]
      (if (seq registers)
        (let [register (first registers)
              sprite [(dec register) register (inc register)]
              new-crt (conj crt (get-pixel-val pixel sprite))
              new-pixel (mod (inc pixel) crt-line-length)]
          (recur (next registers) new-crt new-pixel (rest cmds)))
        (partition crt-line-length crt)))))

; --------------------------
; results

(defn day10-1
  []
  (let [cmds (memoized_input-file->cmds)
        cycles [20 60 100 140 180 220]
        register 1
        registers (get-cycles-register cmds register)
        registers-at-cycles (map #(get registers %) (map dec cycles))]
    (->> registers-at-cycles
         (map * cycles)
         (apply +))))

(defn -main
  []
  (println (day10-1)))
