(ns sudoku
  (:require [clojure.set :as set]))

(defn reload []
  (use 'sudoku :reload))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def sudoku-boardx
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-boardx
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map #(value-at board [%1 (last coord)]) (range 9))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn get-upper-left [coord]
  (map #(* 3 (quot % 3)) coord))

(defn get-coords-for-block-upper-left [coord]
  "argument is upper left, returns block coords for those"
  (map #(list (+ (first coord) (first %)) (+ (last coord) (last %))) (coord-pairs [0 1 2])))

(defn block-values [board coord]
  (set
   (for [c (get-coords-for-block-upper-left (get-upper-left coord))]
     (value-at board c)
    )))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (block-values board coord) (col-values board coord) (row-values board coord))))

(defn filled? [board]
  (not (contains? (reduce #(into %1 %2) #{} board) 0)))

(defn rows [board]
  (map #(set %) board))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (for [x (range 9)]
    (col-values board [0 x])
    ))

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
