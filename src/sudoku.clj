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
;20

(defn block-values [board coord]
    (let [blockVals
           (reduce
             (fn[a-seq pt]
               (conj a-seq
                 (value-at board [(first pt) (second pt)])))
             []
             (block-coord-pairs coord))]
      (set blockVals)))

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


(defn all-numbers-in-board [board]
   (set (reduce (fn[a-seq row] (concat a-seq row)) [] board)))

(defn filled? [board]
   (let [nums (all-numbers-in-board board)]
    (not (contains? nums 0))))
;23

(defn rows [board]
  (map (fn[i] (row-values board [i 0])) (range 9)))
;25

(defn valid-helper? [board values]
  (reduce
     (fn[prev curr] (and prev (empty? (set/difference #{1 2 3 4 5 6 7 8 9} curr))))
     true
     values))

(defn valid-rows? [board]
  (valid-helper? board (rows board)))

;30

(defn cols [board]
  (map (fn[i] (col-values board [0 i])) (range 9)))
;27

(defn valid-cols? [board]
  (valid-helper? board (cols board)))
;31

(defn blocks [board]
  (let [blockCoords
        (for [row [0 3 6]
              col [0 3 6]]
            [row col])]
    (map (fn[coord] (block-values board coord)) blockCoords)))
;29

(defn valid-blocks? [board]
  (valid-helper? board (blocks board)))
;32

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))
;33

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))
;34

(defn find-empty-point [board]
  (let [boardCoords
        (for [row (range 9)
              col (range 9)]
          [row col])]
    (first (filter (fn[coord] (= 0 (value-at board coord))) boardCoords))))
;35

(defn solve-helper [board]
  (let [nextPoint (find-empty-point board)]
    (if nextPoint
      (for [candidate #{1 2 3 4 5 6 7 8 9}
            solution (solve-helper (set-value-at board nextPoint candidate))]
        solution)
      (if (valid-solution? board)
          [board]
          []))))

(defn solve [board]
  (first (solve-helper board)))
