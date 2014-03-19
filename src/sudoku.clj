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

(defn block-coordinates [top-left]
  (let [[x y] top-left]
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

(defn set-board-all-values [board]
  (apply set/union (map (fn [x-coord]
                    (row-values board [x-coord 0])) (range (count (first board))))))

(defn filled? [board]
  (not (contains? (set-board-all-values board) 0)))

(defn rows [board]
  (map (fn [x-coord]
         (row-values board [x-coord 0])) (range (count (first board)))))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (map (fn [y-coord]
         (col-values board [0 y-coord])) (range (count (first board)))))

(defn valid-cols? [board]
  nil)


(defn all-top-left-coordinates []
  (for [x-range [0 3 6]
        y-range [0 3 6]]
    [x-range y-range]))

;(defn manual-all-top-left-coordinates []
;  [[0 0] [0 3] [0 6]
;   [3 0] [3 3] [3 6]
;   [6 0] [6 3] [6 6]])

(defn blocks [board]
  (map (fn [[x-coord y-coord]]
         (block-values board [x-coord y-coord])) (all-top-left-coordinates)))

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
