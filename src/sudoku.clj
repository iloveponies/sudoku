(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (let [col-vect
        (for [row (range 9)]
          (value-at board [row col]))]
    (set col-vect)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left [n]
  (* (quot n 3) 3))

(defn block-values [board [row col]]
  (let [top    (top-left row)
        left   (top-left col)
        rows   (range top (+ top 3))
        cols   (range left (+ left 3))
        coords (for [r rows
                     c cols]
                 (value-at board [r c]))]
    (set  coords)))

(defn valid-values-for [board coord]
  nil)

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
