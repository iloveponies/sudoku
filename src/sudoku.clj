(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})


(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (for [row board]
         (get row (second coord)))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left
  "This is a helper function for block-values"
  [coord]
  [(* 3 (int (/ (first coord) 3)))
   (* 3 (int (/ (second coord) 3)))])

(defn block-values [board coord]
  (let [[top left] (top-left coord)]
    (set (for [x (range top (+ 3 top))
          y (range left (+ 3 left))]
      (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [block (block-values board coord)
          row (row-values board coord)
          col (col-values board coord)]
      (reduce #(set/difference %1 %2) all-values [block row col]))))

(defn board-values
  "Returns all the values from the given board"
  [board]
  (let [rows (for [row (range 0 9)]
              (row-values board [row 0]))]
    (reduce #(set/union %1 %2) rows)))

(defn filled? [board]
  (not (contains? (board-values board) 0)))


(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(defn blocks [board]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-cols? board)
       (valid-rows? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %))
                 (coord-pairs (range 0 9)))))

(defn solve-helper [board]
  (let [next-coord (find-empty-point board)]
    (if (nil? next-coord)
      ;we are at the end - check if we have a solution
      (if (valid-solution? board) [board] [])
      ;we still have empty spots
      (for [v (valid-values-for board next-coord)
            solution (solve-helper (set-value-at board next-coord v))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))

