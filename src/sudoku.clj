(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (let [col-val (fn [acc curr] (set (cons (get curr col) acc)))]
    (reduce col-val [] board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board [row col]]
  (let [corner-row (* 3 (int (/ row 3)))
        corner-col (* 3 (int (/ col 3)))
        additions (coord-pairs [0 1 2])
        all-coords-in-block (map (fn [[row-add col-add]] [(+ corner-row row-add) (+ corner-col col-add)]) additions)]

    (reduce (fn [a-set curr] (conj a-set (value-at board curr))) #{} all-coords-in-block)))

(defn valid-values-for [board coord]
  (let [curr-value (value-at board coord)]
    (if (> curr-value 0)
      #{}
      (set/difference
        all-values
        (block-values board coord)
        (row-values board coord)
        (col-values board coord)))))

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
