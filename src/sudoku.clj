(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord) )))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get-in board [x]))))

(defn col-values [board coord]
  (let [[row col] coord]
    (loop [result #{}
          row 0]
      (if (== 9 row)
        result
        (recur (conj result (value-at board [row col])) (inc row))))))

(defn coord-pairs [coords]
  (for [x coords y coords] [x y]))

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
