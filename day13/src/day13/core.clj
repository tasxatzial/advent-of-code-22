(ns day13.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-line->packet
  "Parses an input line into the corresponding vector form (a packet)."
  [line]
  (when (not= "" line)
    (read-string line)))

(defn parse-file
  "Reads and parses the input file into a sequence of vectors. Each vector
  corresponds to a packet."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map input-line->packet)
       (filter some?)))

(def memoized-input-file->packets (memoize parse-file))

(defn compare-int
  "Compares two numbers p1 and p2. Returns:
  :less if p1 < p2
  :equal if p1 = p2
  :greater if p1 > p2"
  [p1 p2]
  (if (< p1 p2)
    :less
    (if (> p1 p2)
      :greater
      :equal)))

(defn compare-packets
  "Compares two packets p1 and p2. Returns:
  :less if p1 < p2
  :equal if p1 = p2
  :greater if p1 > p2"
  [p1 p2]
  (if (and (vector? p1) (vector? p2))
    (if (or (empty? p1) (empty? p2))
      (if (and (empty? p1) (empty? p2))
        :equal
        (if (and (seq p1) (empty? p2))
          :greater
          :less))
      (let [order (compare-packets (first p1) (first p2))]
        (if (= order :equal)
          (recur (subvec p1 1) (subvec p2 1))
          order)))
    (if (and (int? p1) (int? p2))
      (compare-int p1 p2)
      (if (int? p1)
        (recur [p1] p2)
        (recur p1 [p2])))))

; --------------------------
; problem 2

; Convert the result of the comparison of two packets to an int so
; that we can sort packets using a comparator.
(def order->int
  {:less -1,
   :greater 1,
   :equal 0})

; --------------------------
; results

(defn day13-1
  []
  (let [packet-pairs (partition 2 (memoized-input-file->packets))
        pairs-comparison (map #(apply compare-packets %) packet-pairs)]
    (->> pairs-comparison
         (keep-indexed #(if (= :less %2) (inc %1)))
         (reduce +))))

(defn day13-2
  []
  (let [packets (memoized-input-file->packets)
        divider-packets [[[2]] [[6]]]
        new-packets (into packets divider-packets)
        sorted-packets (sort #(order->int (compare-packets %1 %2)) new-packets)]
    (->> sorted-packets
         (keep-indexed #(if (some #{%2} divider-packets) (inc %1)))
         (reduce *))))

(defn -main
  []
  (println (day13-1))
  (println (day13-2)))
