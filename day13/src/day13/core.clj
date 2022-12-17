(ns day13.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-line->packet
  "Parses an input line into a vector."
  [line]
  (when (not= "" line)
    (read-string line)))

(defn input-file->packets
  "Reads and parses the input file into a seq of packets (vectors)."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map input-line->packet)
       (filter some?)))

(def memoized_input-file->packets (memoize input-file->packets))

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
  (let [packet-pairs (partition 2 (memoized_input-file->packets))
        pairs-orders (map #(apply compare-packets %) packet-pairs)]
    (->> pairs-orders
         (keep-indexed #(if (= :less %2) %1))
         (map inc)
         (apply +))))


(defn day13-2
  []
  (let [packets (memoized_input-file->packets)
        divider-packets [[[2]] [[6]]]
        new-packets (into packets divider-packets)
        sorted-packets (sort #(order->int (compare-packets %1 %2)) new-packets)]
    (->> sorted-packets
         (keep-indexed #(if (some #{%2} divider-packets) %1))
         (map inc)
         (apply *))))

(defn -main
  []
  (println (day13-1))
  (println (day13-2)))
