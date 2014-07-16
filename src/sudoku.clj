(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map (fn [r] (get r col)) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn top-left [[x y]]
  [(* (int (/ x 3)) 3) (* (int (/ y 3)) 3)])

(defn block-values [board coord]
  (let [[tlx tly] (top-left coord)]
    (set (map (fn [c] (value-at board c))
      (for [x (range tlx (+ tlx 3))
            y (range tly (+ tly 3))]
          [x y])))))

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
