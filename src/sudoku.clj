(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def sudoku-board-2
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
  (not(zero?(get-in board coord))))

(defn row-values [board coord]
  (let [[x] coord]
    (set (get board x))))

(defn col-values-acc [board y acc]
  (if (empty? board) acc
  (col-values-acc (rest board) y (conj acc (get (first board) y)))))

(defn col-values [board coord]
  (let [[_ y] coord]
    (col-values-acc board y #{})))

(defn coord-pairs [coords]
  (let [v []]
    (for [col coords
          row coords]
      (conj v col row))))

(defn top-left-block-value [n]
  (cond
    (and (>= n 0)(< n 3)) 0
    (and (>= n 3)(< n 6)) 3
    :else 6))

(defn top-left-corner [coords]
  (let [[x y] coords]
    (vector (top-left-block-value x)(top-left-block-value y))))

(defn block-values-helper [board coords acc]
  (if (empty? coords) acc
  (block-values-helper board (rest coords) (conj acc (value-at board (first coords))))))

(defn block-values [board coord]
  (let [[x y] (top-left-corner coord)]
    (block-values-helper board (coord-pairs (range x (+ 3 y))) #{})))

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
