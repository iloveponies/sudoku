(ns sudoku
  (:require [clojure.set :as set]))

;(def board2 [[5 3 0 0 7 0 0 0 0]
;        [6 0 0 1 9 5 0 0 0]
;        [0 9 8 0 0 0 0 6 0]
;        [8 0 0 0 6 0 0 0 3]
;        [4 0 0 8 0 3 0 0 1]
;        [7 0 0 0 2 0 0 0 6]
;        [0 6 0 0 0 0 2 8 0]
;        [0 0 0 4 1 9 0 0 5]
;        [0 0 0 0 8 0 0 7 9]])

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
  nil)

(defn block-values [board coord]
  nil)

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
