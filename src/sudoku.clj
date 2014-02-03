(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row-index _]]
  (set (board row-index)))

(defn col-values [board [_ column-index]]
  (let [column (map (fn [row] (row column-index)) board)]
    (set column)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn round-multiple-of-three [n]
  (* 3 (quot n 3)))

(defn block-origin [[row col]]
  [(round-multiple-of-three row)
   (round-multiple-of-three col)])

;todo defn block-coordinates

(defn block-coordinates
  "Get the block coordinates for a given coordinate."
  [coord]
  (let [[row-origin col-origin] (block-origin coord)]
    (for [row (range row-origin (+ 3 row-origin))
          col (range col-origin (+ 3 col-origin))]
      [row col])))

(defn block-values [board coord]
  (let [coordinates (block-coordinates coord)]
    (map (fn [coordinate] (value-at board coordinate)) coordinates)))

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
