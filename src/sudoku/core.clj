(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [row _]]
  (let [row-vect (get board row)]
    (set row-vect)))

(defn col-values [board [_ column]]
  (let [get-column (fn [x] (get x column))
        column-vect (map get-column board)]
    (set column-vect)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vector x y)))

(defn block-values [board coord]
  (let [get-val (fn [x] (value-at board x))
        get-topleft-coord (fn [[y x]] 
                            (vector (- y (mod y 3))
                                    (- x (mod x 3))))
        block-coord-pairs (fn [[y-offset x-offset]]
                            (for [y [0 1 2]
                                  x [0 1 2]]
                              (vector (+ y y-offset) (+ x x-offset))))]
    (set (map get-val (block-coord-pairs (get-topleft-coord coord))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values 
                    (block-values board coord)
                    (row-values board coord)
                    (col-values board coord))))

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
