(ns day06.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-file->signal
  "Reads the input file and returns a string that represents the signal."
  []
  (-> input-file
      slurp))

(def memoized_signal (memoize input-file->signal))

(defn start-marker?
  "Returns true if the collection s is a start marker, false otherwise.
  Being a start marker requires:
  1) s has packet-length number of elements.
  2) s has distinct elements."
  [s packet-length]
  (and (apply distinct? s) (= packet-length (count s))))

(defn find-start-marker
  "Finds the signal index (starting from 1) of the first start marker.
  Returns -1 if a start marker is not found."
  [signal packet-length]
  (loop [_signal signal]
    (if (first _signal)
      (if (start-marker? (take packet-length _signal) packet-length)
        (+ (count signal) (- (count _signal)) packet-length)
        (recur (rest _signal)))
      -1)))

; --------------------------
; results

(defn day06-1
  []
  (-> (memoized_signal)
      (find-start-marker 4)))

(defn day06-2
  []
  (-> (memoized_signal)
      (find-start-marker 14)))

(defn -main
  []
  (println (day06-1))
  (println (day06-2)))
