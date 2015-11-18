(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def sudoku-board0
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (set (value-at board [(first coord)])))

(defn col-values [board coord]
  (set (for [row (range 0 9)]
         (value-at board [row (second coord)]))))

(col-values sudoku-board0 [0 2])

(defn coord-pairs [coords]
  (for [i1 coords
        i2 coords]
    [i1 i2]))

(defn block-values [board coord]
  (let [row (first coord)
        col (second coord)
        block-row (* 3 (int (/ row 3)))
        block-col (* 3 (int (/ col 3)))]
    (set
      (for [row (range block-row (+ block-row 3))
            col (range block-row (+ block-col 3))]
       (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (= 0 (value-at board coord))
    (set/difference (set (range 1 10))
                    (set/union (row-values board coord) (col-values board coord) (block-values board coord)))
    #{}))

(defn filled? [board]
  nil)

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
