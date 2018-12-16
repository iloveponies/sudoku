(ns sudoku
  (:require [clojure.set :as set]))

;(def board2
;       [[5 3 0 0 7 0 0 0 0]
;        [6 0 0 1 9 5 0 0 0]
;        [0 9 8 0 0 0 0 6 0]
;        [8 0 0 0 6 0 0 0 3]
;        [4 0 0 8 0 3 0 0 1]
;        [7 0 0 0 2 0 0 0 6]
;        [0 6 0 0 0 0 2 8 0]
;        [0 0 0 4 1 9 0 0 5]
;        [0 0 0 0 8 0 0 7 9]])

;[[5 3 0 | 0 7 0 | 0 0 0]
; [6 0 0 | 1 9 5 | 0 0 0]
; [0 9 8 | 0 0 0 | 0 6 0]
; -------+-------+-------
; [8 0 0 | 0 6 0 | 0 0 3]
; [4 0 0 | 8 0 3 | 0 0 1]
; [7 0 0 | 0 2 0 | 0 0 6]
; -------+-------+-------
; [0 6 0 | 0 0 0 | 2 8 0]
; [0 0 0 | 4 1 9 | 0 0 5]
; [0 0 0 | 0 8 0 | 0 7 9]]

(def board identity)


(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

;returns set i.e. from multiple zeroes only one is included
(defn row-values [board coord]
  (let [[row] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [row] (get-in board [row col])) (range (count board))))))

(defn coord-pairs [coords]
  (let [pairs []]
   (for [row coords col coords]
     (seq (conj pairs row col)))))

;assumes 3x3 board
;returns [x,y]
(defn block-top-left [coord]
  (let [[row _] coord [_ col] coord]
    (let
      [x
       (cond
        (contains? #{0 1 2} row)
         0
        (contains? #{3 4 5} row)
         3
        (contains? #{6 7 8} row)
         6)
      y
       (cond
        (contains? #{0 1 2} col)
         0
        (contains? #{3 4 5} col)
         3
        (contains? #{6 7 8} col)
         6)]
    (vector x y))))


(defn block-values [board coord]
  (let [[top-left-x _] (block-top-left coord)
        [ _ top-left-y] (block-top-left coord)]
   (let [x-coords (range top-left-x (+ top-left-x 3))
         y-coords (range top-left-y (+ top-left-y 3))]
    (set (for [row x-coords col y-coords]
      (value-at board (vector row col)))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [values-for-row (row-values board coord)
          values-for-col (col-values board coord)
          values-for-block (block-values board coord)]
      (set/difference all-values (set/union values-for-row values-for-col values-for-block)))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

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
