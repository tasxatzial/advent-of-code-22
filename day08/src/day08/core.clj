(ns day08.core
  (:gen-class))

; --------------------------
; utils

(defn str->int
  [s]
  (Integer/parseInt (str s)))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn input-line->trees
  "Parses an input line and returns a vector of integers that represent
  the heights of the trees."
  [line]
  (mapv str->int line))

(defn input-file->input-lines
  "Reads and parses the input file into a vector of lines."
  []
  (->> input-file
       slurp
       clojure.string/split-lines))

(def memoized_input-file->input-lines (memoize input-file->input-lines))

(defn input-file->trees-by-row
  "Reads and parses the input file into a vector of vectors. Vectors correspond to
  rows in the input file and contain integers that represent the tree heights."
  []
  (->> (memoized_input-file->input-lines)
       (mapv input-line->trees)))

(defn input-file->trees-by-col
  "Reads and parses the input file into a vector of vectors. Vectors correspond to
  columns in the input file and contain integers that represent the tree heights."
  []
  (let [input-lines (memoized_input-file->input-lines)
        row-count (count input-lines)]
    (->> (memoized_input-file->input-lines)
         (apply interleave)
         (map str->int)
         (partition row-count)
         (mapv vec))))

(defn create-grid
  "Reads the input file and creates a seq that contains the coordinates of each
  tree as a vector of two integers. Indices start from [0 0]."
  []
  (let [trees-by-row (input-file->trees-by-row)
        trees-by-col (input-file->trees-by-col)]
    (for [x (range (count trees-by-row))
          y (range (count trees-by-col))]
      [x y])))

; --------------------------
; problem 1

(defn tree-visible-from-edge?
  "Multi-arity function.
  1) If called with 2 args, it accepts a vector that represents a row of tree heights
  and the index of a tree height in that row. Returns true if that tree is visible from
  the edges of the row, false otherwise.
  2) If called with 3 args, it accepts the tree heights organized by rows and columns
  (a vector of vectors in both cases) and the coordinates of a tree height. Returns true
  if that tree is visible from the edges of its row or column.

  Note that the 2-arity function will also work if a column is supplied."
  ([tree-row tree-col-index]
   (let [tree (get tree-row tree-col-index)]
     (or (let [left-trees (subvec tree-row 0 tree-col-index)]
           (every? #(> tree %) left-trees))
         (let [right-trees (subvec tree-row (inc tree-col-index))]
           (every? #(> tree %) right-trees)))))
  ([trees-by-row trees-by-col tree-index]
   (let [[tree-row-index tree-col-index] tree-index]
     (or (let [tree-col (get trees-by-col tree-col-index)]
           (tree-visible-from-edge? tree-col tree-row-index))
         (let [tree-row (get trees-by-row tree-row-index)]
           (tree-visible-from-edge? tree-row tree-col-index))))))

(defn count-visible-trees
  "Returns the number of trees that are visible from the edges of the grid.
  Accepts the tree heights organized by rows and columns (a vector of vectors in
  both cases) and a grid (a seq of coordinates)."
  [trees-by-row trees-by-col grid]
  (->> grid
       (map #(tree-visible-from-edge? trees-by-row trees-by-col %))
       (filter true?)
       count))

; --------------------------
; problem 2

(defn get-directional-view-distance
  "Accepts a tree height and a seq of the rest of the tree heights in any direction
  (left, right, bottom, up). Returns the viewing distance from that tree."
  [tree rest-trees]
  (let [visible-trees-count (count (take-while #(> tree %) rest-trees))]
    (if (and (>= tree (or (last rest-trees) 0))
             (= visible-trees-count (count rest-trees)))
      visible-trees-count
      (inc visible-trees-count))))

(defn get-view-distance
  "Accepts the tree heights organized by rows and columns (a vector of vectors in
  both cases) and the coordinates of a tree height.
  Returns a vector that contains the right, left, up, bottom viewing distances
  (in that order) from that tree."
  [trees-by-row trees-by-col tree-coordinates]
  (let [[tree-row-index tree-col-index] tree-coordinates
        tree (get-in trees-by-row tree-coordinates)
        tree-row (get trees-by-row tree-row-index)
        tree-col (get trees-by-col tree-col-index)
        right-trees (subvec tree-row (inc tree-col-index))
        left-trees (or (rseq (subvec tree-row 0 tree-col-index)) [])
        bottom-trees (subvec tree-col (inc tree-row-index))
        up-trees (or (rseq (subvec tree-col 0 tree-row-index)) [])]
    [(get-directional-view-distance tree right-trees)
     (get-directional-view-distance tree left-trees)
     (get-directional-view-distance tree up-trees)
     (get-directional-view-distance tree bottom-trees)]))

(defn get-scenic-score
  "Returns the scenic score given a collection of viewing distances."
  [view-distances]
  (apply * view-distances))

(defn get-highest-scenic-score
  "Accepts the tree heights organized by rows and columns (a vector of vectors in
  both cases) and a grid (a seq of coordinates).
  Computes the highest scenic score possible for any tree."
  [trees-by-row trees-by-col grid]
  (->> grid
       (map (comp get-scenic-score #(get-view-distance trees-by-row trees-by-col %)))
       (apply max)))

; --------------------------
; results

(defn day08
  [func]
  (let [trees-by-row (input-file->trees-by-row)
        trees-by-col (input-file->trees-by-col)
        grid (create-grid)]
    (func trees-by-row trees-by-col grid)))

(defn day08-1
  []
  (day08 count-visible-trees))

(defn day08-2
  []
  (day08 get-highest-scenic-score))

(defn -main
  []
  (println (day08-1))
  (println (day08-2)))
