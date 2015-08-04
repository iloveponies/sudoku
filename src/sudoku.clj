(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))
;8

(defn has-value? [board coord]
  (< 0 (value-at board coord)))
;9

(defn row-values [board coord]
  (let [row (get board (first coord))]
    (set row)))
;12

(defn col-values [board coord]
  (let [col
        (reduce
          (fn[a-seq i]
            (conj a-seq
              (value-at board [i (second coord)]))) [] (range 9))]
    (set col)))
;15

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn nearest-multiple [x]
  (cond
    (<= x 2)
      0
    (<= x 5)
      3
    :else
      6))

(defn upper-right [coord]
  [(nearest-multiple (first coord))
    (nearest-multiple (second coord))])

(defn block-coord-pairs [coord]
  (let [up-right (upper-right coord)
        firstRow (first up-right)
        firstCol (second up-right)]
    (for [row [firstRow (+ 1 firstRow) (+ 2 firstRow)]
          col [firstCol (+ 1 firstCol) (+ 2 firstCol)]]
      [row col])))

(defn block-values [board coord]
    (let [blockVals
           (reduce
             (fn[a-seq pt]
               (conj a-seq
                 (value-at board [(first pt) (second pt)])))
             []
             (block-coord-pairs coord))]
      (set blockVals)))
;20

(defn valid-values-for [board coord]
  (if (< 0 (value-at board coord))
      #{}
      (let [possible-vals #{1 2 3 4 5 6 7 8 9}
            not-in-block
              (set/difference possible-vals (block-values board coord))
            not-in-row
              (set/difference not-in-block (row-values board coord))
            not-in-col
              (set/difference not-in-row (col-values board coord))]
        not-in-col)))
;22

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
