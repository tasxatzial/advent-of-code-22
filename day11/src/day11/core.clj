(ns day11.core
  (:gen-class))

(def input-file "resources\\input.txt")

; --------------------------
; utils

(defn str->int
  [s]
  (Integer/parseInt (str s)))

(defn get-number
  "Extracts the first number from a string."
  [s]
  (str->int (re-find #"\d+" s)))

(defn get-numbers
  "Extracts all numbers from a string and returns a vector of integers."
  [s]
  (mapv str->int (re-seq #"\d+" s)))

; --------------------------
; parse file

(defn input-file->lines-per-monkey
  "Reads the input file and splits it into a sequence of sequences. Each sequence
  contains all lines that correspond to a single monkey."
  []
  (->> input-file
       slurp
       (clojure.string/split-lines)
       (remove #(= "" %))
       (partition 6)))

(defn get-operation-fn
  "Parses a line that has the monkey operation and returns the corresponding function."
  [operation-line]
  (let [[operand1 operator operand2] (re-seq #"\d+|old|[*+]" operation-line)
        fn-str (str "#(" operator " " operand1 " " operand2 ")")
        anonymous-fn-str (clojure.string/replace fn-str #"old" "%")]
    (eval (read-string anonymous-fn-str))))

(defn get-test-fn
  "Parses a line that has the monkey test and returns the corresponding function."
  [test-line]
  (let [num (get-number test-line)]
    #(integer? (/ % num))))

(defn monkey-lines->monkey
  "Parses the sequence of the lines for a single monkey into a map that contains:
  :index --> monkey id (int)
  :items --> starting items (vector of int)
  :operation --> operation function
  :test --> test function (int -> boolean)
  :pass-test-target --> id of the target monkey if test returns true
  :fail-test-target --> id of the target monkey if test returns false"
  [monkey-lines]
  {:index            (get-number (first monkey-lines)),
   :items            (get-numbers (second monkey-lines)),
   :operation        (get-operation-fn (nth monkey-lines 2))
   :test             (get-test-fn (nth monkey-lines 3))
   :pass-test-target (get-number (nth monkey-lines 4))
   :fail-test-target (get-number (nth monkey-lines 5))})

(defn parse-file
  "Reads and parses the input file into a map of (monkey id) -> (monkey map).
  The structure of each map is described in function monkey-lines->monkey"
  []
  (reduce (fn [res monkey-lines]
            (let [monkey (monkey-lines->monkey monkey-lines)]
              (assoc res (:index monkey) monkey)))
          {}
          (input-file->lines-per-monkey)))

(def memoized-input-file->monkeys (memoize parse-file))

; --------------------------

(defn initialize-inspection
  "Updates each of the monkeys with the following keys:
  :inspected --> the number of inspected items
  :reduce-item --> function that reduces an item before it is thrown"
  [monkeys reduce-item]
  (reduce (fn [res [index monkey]]
            (let [updated-monkey (assoc monkey :inspected 0 :reduce-item reduce-item)]
              (assoc res index updated-monkey)))
          {}
          monkeys))

(defn get-monkey-after-one-step
  "Returns the monkey after it has thrown all its items."
  [monkey]
  (let [inspected (:inspected monkey)
        items (:items monkey)]
    (-> monkey
        (assoc :items [])
        (assoc :inspected (+ (count items) inspected)))))

(defn get-monkeys-after-one-step
  "Returns the monkeys after the monkey with the given index has thrown all its items."
  [monkeys monkey-index]
  (let [monkey (get monkeys monkey-index)
        updated-monkey (get-monkey-after-one-step monkey)
        operation (:operation monkey)
        test (:test monkey)
        reduce-item (:reduce-item monkey)
        thrown-items (map (comp reduce-item operation) (:items monkey))
        grouped-by-test-thrown-items (group-by test thrown-items)
        fail-test-target-index (get monkey :fail-test-target)
        fail-test-target (get monkeys fail-test-target-index)
        fail-test-target-items (:items fail-test-target)
        fail-test-received-items (get grouped-by-test-thrown-items false)
        fail-test-target-updated-items (into fail-test-target-items fail-test-received-items)
        fail-test-updated-target (assoc fail-test-target :items fail-test-target-updated-items)
        pass-test-target-index (get monkey :pass-test-target)
        pass-test-target (get monkeys pass-test-target-index)
        pass-test-target-items (:items pass-test-target)
        pass-test-received-items (get grouped-by-test-thrown-items true)
        pass-test-target-updated-items (into pass-test-target-items pass-test-received-items)
        pass-test-updated-target (assoc pass-test-target :items pass-test-target-updated-items)]
    (-> monkeys
        (assoc pass-test-target-index pass-test-updated-target)
        (assoc fail-test-target-index fail-test-updated-target)
        (assoc monkey-index updated-monkey))))

(defn get-monkeys-after-one-round
  "Returns the monkeys after one round."
  [monkeys]
  (reduce (fn [result [index _]]
            (get-monkeys-after-one-step result index))
          monkeys
          monkeys))

(defn get-monkeys-after-all-rounds
  "Returns the monkeys after all rounds. Accepts a function reduce-item that decreases the
  worry level of an item before it is thrown."
  [monkeys rounds reduce-item]
  (let [monkeys (initialize-inspection monkeys reduce-item)]
    (loop [monkeys monkeys
           rounds rounds]
      (if (zero? rounds)
        monkeys
        (let [new-monkeys (get-monkeys-after-one-round monkeys)]
          (recur new-monkeys (dec rounds)))))))

(defn get-monkey-business-level
  "Returns the level of monkey business."
  [monkeys]
  (->> (map #(:inspected (second %)) monkeys)
       (sort >)
       (take 2)
       (reduce *)))

; --------------------------
; results

(defn day11-1
  []
  (-> (memoized-input-file->monkeys)
      (get-monkeys-after-all-rounds 20 #(quot % 3))
      get-monkey-business-level))

;; 9699690 is the product of all numbers found in the "divisible by" lines
(defn day11-2
  []
  (-> (memoized-input-file->monkeys)
      (get-monkeys-after-all-rounds 10000 #(mod % 9699690))
      get-monkey-business-level))

(defn -main
  []
  (println (time (day11-1)))
  (println (time (day11-2))))
