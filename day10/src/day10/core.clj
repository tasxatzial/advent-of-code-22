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
  "Parses an input line into a command. Returns a vector, its first element is a
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

(def cmd-cycle-cost
  {:addx 2
   :noop 1})

(defn exec-cmd
  "Accepts a vector that represents a command and the current value of the
  register. Returns a vector that contains the values of the register in each
  of the cycles that are required for command execution."
  [cmd register]
  (let [[op val] cmd
        cycle-cost (cmd-cycle-cost op)
        initial-registers (vec (take (dec cycle-cost) (repeat register)))]
    (case op
      :addx (conj initial-registers (+ val register))
      (conj initial-registers register))))

(defn exec-cmds
  "Accepts a vector of commands and an initial register. Returns a vector that
  contains the values of the register in each of the cycles that are required for
  executing all commands."
  [cmds register]
  (reduce (fn [result cmd]
            (->> (last result)
                 (exec-cmd cmd)
                 (into result)))
          [register]
          cmds))

; --------------------------
; problem 2

(defn get-pixel-val
  "Returns the value of the pixel given its index and a sprite vector
  of 3 pixel indices."
  [pixel-index sprite]
  (if (some #(= pixel-index %) sprite)
    \#
    \.))

(defn draw-crt
  "Executes all commands given an initial register value. Returns
  the output of the crt as a seq of seqs. Each seq corresponds to
  the final pixel values in a crt line."
  [cmds register]
  (let [crt-length 240
        line-length 40
        registers (exec-cmds cmds register)]
    (loop [registers (take crt-length registers)
           crt []
           pixel-idx 0
           cmds cmds]
      (if (seq registers)
        (let [register (first registers)
              sprite [(dec register) register (inc register)]
              new-crt (conj crt (get-pixel-val pixel-idx sprite))
              new-pixel-idx (mod (inc pixel-idx) line-length)]
          (recur (rest registers) new-crt new-pixel-idx (rest cmds)))
        (partition line-length crt)))))

; --------------------------
; results

(defn day10-1
  []
  (let [cmds (memoized_input-file->cmds)
        cycles [20 60 100 140 180 220]
        init-register 1
        registers (exec-cmds cmds init-register)
        registers-at-cycles (map #(get registers %) (map dec cycles))]
    (->> registers-at-cycles
         (map * cycles)
         (apply +))))

(defn day10-2
  []
  (let [cmds (memoized_input-file->cmds)
        init-register 1
        crt-lines (draw-crt cmds init-register)]
    (doseq [crt-line crt-lines]
      (println (apply str crt-line)))))

(defn -main
  []
  (println (day10-1))
  (println (day10-2)))
