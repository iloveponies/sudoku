(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (let [value (value-at board coord)]
    (not (= value 0))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row] (value-at board [row col])) (range 0 9))))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    [c1 c2]))

(defn block-starts [[row col]]
  [(* (int (/ row 3)) 3) (* (int (/ col 3)) 3)])

(defn block-values [board coord]
  (let [[start-row start-col] (block-starts coord)
        rows (range start-row (+ start-row 3))
        cols (range start-col (+ start-col 3))
        coords (for [row rows
                     col cols]
                 [row col])]
    (set (map (fn [coord] (value-at board coord)) coords))))

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
