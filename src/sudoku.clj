(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def sudoku-board
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
  (not= 0 (value-at board coord)))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (loop [r 0
         values #{}]
    (if (== r 9)
      values
      (recur
       (inc r)
       (conj values (value-at board [r col]))))))

(defn coord-pairs [coord-sequence]
  (for [r coord-sequence
        c coord-sequence]
    [r c]))

(defn block-values [board [x y]]
  (def shifted-coords
    (fn [[shif-top shift-left]]
      (map (fn [[row col]] [(+ row shif-top) (+ col shift-left)])
           (coord-pairs [3 4 5]))))

  (let [[top left] [(* (quot x 3) 3) (* (quot y 3) 3)]
        shifts [(- top 3) (- left 3)]
        real-coords (shifted-coords shifts)]
    (set
     (map
      (fn [coord] (value-at board coord))
      real-coords))))

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
