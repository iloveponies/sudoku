(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord)) false true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (map (fn [row] (get row y)) board))))

(defn coord-pairs [coords]
  (for [given-coords coords
        same-coords coords]
    (vector given-coords same-coords)))

(defn top-left [board coord]
  (let [[given-x given-y] coord
        [top-left-x top-left-y] [(- given-x (rem given-x 3)) (- given-y (rem given-y 3))]]
    [top-left-x top-left-y]))

;(defn manual-block-coordinates [coord]
;  (let [[x y] coord]
;    [[x y]       [x (+ 1 y)]        [x (+ 2 y)]
;     [(+ 1 x) y] [(+ 1 x) (+ 1 y)]  [(+ 1 x) (+ 2 y)]
;     [(+ 2 x) y] [(+ 2 x) (+ 1 y)]  [(+ 2 x) (+ 2 y)]]))

(defn block-coordinates [coord]
  (let [[x y] coord]
    (for [x-range (range 3)
          y-range (range 3)]
      [(+ x x-range) (+ y y-range)])))

(defn block-values [board coord]
  (let [calculated-coord (top-left board coord)]
    (set (map (fn [coord] (value-at board coord)) (block-coordinates calculated-coord)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [unified-set (set/union (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord))]
      (set/difference all-values unified-set))))

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
