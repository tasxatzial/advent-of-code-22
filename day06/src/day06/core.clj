(ns day06.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-file
  "Reads the input file and returns a string that represents the signal."
  []
  (-> input-file slurp))

(def memoized-input-file->signal (memoize parse-file))

(defn start-marker?
  "Returns true if the given collection is a start marker, false otherwise."
  [coll packet-length]
  (and (apply distinct? coll) (= packet-length (count coll))))

(defn find-start-marker
  "Finds the signal index (starting from 1) of the first start marker.
  Returns -1 if a start marker is not found."
  [signal packet-length]
  (loop [sig signal]
    (if (seq sig)
      (if (start-marker? (take packet-length sig) packet-length)
        (+ (count signal) (- (count sig)) packet-length)
        (recur (rest sig)))
      -1)))

; --------------------------
; results

(defn day06-1
  []
  (-> (memoized-input-file->signal)
      (find-start-marker 4)))

(defn day06-2
  []
  (-> (memoized-input-file->signal)
      (find-start-marker 14)))

(defn -main
  []
  (println (day06-1))
  (println (day06-2)))
