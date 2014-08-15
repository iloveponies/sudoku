(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map #(get % (last coord)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn round-coords [coord]
  (let [row (first coord)
        col (last coord)]
  [(* 3 (int (/ row 3))) (* 3 (int (/ col 3)))]))

(defn block-coords [coord]
  (for [row [(first coord) (+ 1 (first coord)) (+ 2 (first coord))]
        col [(last coord) (+ 1 (last coord)) (+ 2 (last coord))]]
    [row col]))

(defn block-values [board coord]
  (set (map #(value-at board %) (block-coords (round-coords coord)))))

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
