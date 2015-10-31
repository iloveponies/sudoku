(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord
        cols (count (get board 0))]
    (set (for [col (range cols)]
          (value-at board [row col])))))

(defn col-values [board coord]
  (let [[_ col] coord
        rows (count board)]
    (set (for [row (range rows)]
           (value-at board [row col])))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left [[row col]]
  [(* 3 (quot row 3)) (* 3 (quot col 3))])

(defn block-values [board coord]
  (let [[brow bcol] (top-left coord)]
    (set (for [dy (range 3)
               dx (range 3)]
           (value-at board [(+ brow dy) (+ bcol dx)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (block-values board coord)
      (row-values board coord)
      (col-values board coord))))

(defn filled? [board]
  (not (contains? (into #{} (apply set/union board)) 0)))

(defn rows [board]
  (for [row (into [] all-values)]
    (row-values board (vector (- row 1) 0))))

(defn valid-rows? [board]
  (let [all-rows (rows board)]
    (every? (fn [row] (= row all-values)) all-rows)))

(defn cols [board]
  (for [column (into [] all-values)]
    (col-values board (vector 0 (- column 1)))))

(defn valid-cols? [board]
  (let [all-cols (cols board)]
    (every? (fn [column] (= column all-values)) all-cols)))

(defn blocks [board]
  (let [corner-indexes [0 3 6]]
    (for [row corner-indexes
          column corner-indexes]
      (block-values board (vector row column)))))

(defn valid-blocks? [board]
  (let [all-blocks (cols board)]
    (every? (fn [block] (= block all-values)) all-blocks)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
    (filter (fn [coord] (not (has-value? board coord)))
      (coord-pairs
        (map dec all-values)))))

(defn solve-helper [current-board]
  (if (filled? current-board)
    (if (valid-solution? current-board)
      current-board
      '())
    (let [remaining (find-empty-point current-board)]
      (for [elem (valid-values-for current-board remaining)
            solution (solve-helper
                       (set-value-at current-board remaining elem))]
        solution))))

(defn solve [board]
  (solve-helper board))
