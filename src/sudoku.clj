(ns sudoku
  (:require [clojure.set :as set]))

; Definitions
(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

; (def sudoku-board
;   (board [[5 3 0 0 7 0 0 0 0]
;           [6 0 0 1 9 5 0 0 0]
;           [0 9 8 0 0 0 0 6 0]
;           [8 0 0 0 6 0 0 0 3]
;           [4 0 0 8 0 3 0 0 1]
;           [7 0 0 0 2 0 0 0 6]
;           [0 6 0 0 0 0 2 8 0]
;           [0 0 0 4 1 9 0 0 5]
;           [0 0 0 0 8 0 0 7 9]]))

(def hardest-sudoku-board
  (board [[8 0 0 0 0 0 0 0 0]
          [0 0 3 6 0 0 0 0 0]
          [0 7 0 0 9 0 2 0 0]
          [0 5 0 0 0 7 0 0 0]
          [0 0 0 0 4 5 7 0 0]
          [0 0 0 1 0 0 0 3 0]
          [0 0 1 0 0 0 0 6 8]
          [0 0 8 5 0 0 0 1 0]
          [0 9 0 0 0 0 4 0 0]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (== 0 (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set [(get-in board [0 y])
          (get-in board [1 y])
          (get-in board [2 y])
          (get-in board [3 y])
          (get-in board [4 y])
          (get-in board [5 y])
          (get-in board [6 y])
          (get-in board [7 y])
          (get-in board [8 y])])))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
  [x y]))

(defn top-left [coord]
  [(* 3 (int (Math/floor (/ (get coord 0) 3)))) (* 3 (int (Math/floor (/ (get coord 1) 3))))])

(defn coord-range [coord]
  (range (get coord 0) (+ 1 (get coord 1))))

(defn block-values [board coord]
  (set 
    (for [x [(get (top-left coord) 0) (+ 1 (get (top-left coord) 0)) (+ 2 (get (top-left coord) 0))]
        y [(get (top-left coord) 1) (+ 1 (get (top-left coord) 1)) (+ 2 (get (top-left coord) 1))]]
      (value-at board [x y]))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference
      all-values
      (clojure.set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
  (if (contains? (set 
    (for [x (range 0 9)
          y (range 0 9)]
      (value-at board [x y]))
    ) 0)
    false
    true  
  )
)

(defn rows [board]
  (vec (for [x (range 0 9)]
    (row-values board [x 0]))))

(defn valid-rows-helper [n rows]
  (cond 
    (== n 8) 
      (if (== (count (get rows n)) 9)
        true
        false
      )
    (== (count (get rows n)) 9)
      (valid-rows-helper (inc n) rows)
    :else
      false
  )
)

(defn valid-rows? [board]
  (valid-rows-helper 0 (rows board))
)

(defn cols [board]
  (vec (for [y (range 0 9)]
    (col-values board [0 y]))))

(defn valid-cols? [board]
  (valid-rows-helper 0 (cols board))
)

(defn blocks [board]
  (vec (for [x [0 3 6]
             y [0 3 6]]
    (block-values board [x y]))))

(defn valid-blocks? [board]
  (valid-rows-helper 0 (blocks board))
)

(defn valid-solution? [board]
  (if (and (valid-rows? board) (valid-cols? board) (valid-blocks? board))
    true
    false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point-helper [board x y]
  (cond
    (or (> x 8) (> y 8))
      nil
    (has-value? board [x y]) 
      (if (== x 8)
        (find-empty-point-helper board 0 (inc y))
        (find-empty-point-helper board (inc x) y))
    :else
      [x y]
  )
)

(defn find-empty-point [board]
  (find-empty-point-helper board 0 0))

(defn solve-helper [board]
  (cond
    (and (filled? board) (valid-solution? board))
      [board]
    (and (filled? board) (not (valid-solution? board)))
      []
    :else ; Probably should use let here to set coord as find-empty-point board
      (for [valid-values (valid-values-for board (find-empty-point board))
            solution (solve-helper (set-value-at board (find-empty-point board) valid-values))]
        solution)
  )
)

(defn solve [board]
  (first (solve-helper board)))

(defn sum [a-seq]
  (reduce + a-seq))

(defn subset-sum-helper [a-set current-set target]
  (if (= (sum current-set) target)
    [current-set]
    (let [remaining (clojure.set/difference a-set current-set)]
      (for [elem remaining
            solution (subset-sum-helper a-set
                                        (conj current-set elem)
                                        target)]
        solution))))

(defn subset-sum [a-set target]
  (subset-sum-helper a-set #{} target))