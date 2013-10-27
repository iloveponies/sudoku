(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[row _] coord]
    (reduce (fn [values col] (conj values (value-at board [row col]))) #{} (range 9))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (reduce (fn [values row] (conj values (value-at board [row col]))) #{} (range 9))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [[row col] coord
        tl-row (int (/ row 3))
        tl-col (int (/ col 3))]
    (reduce (fn [values [r c]] (conj values (value-at board [(+ tl-row r) (+ tl-col c)]))) #{} (coord-pairs [0 1 2]))))

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
