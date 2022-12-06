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

(defn -main
  []
  (println (memoized_signal)))
