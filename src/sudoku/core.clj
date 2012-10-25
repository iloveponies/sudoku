(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map (fn [x] (get x col)) board))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
	(let [[row col] coord
          top-left-corner-x (* (quot row 3) 3) 
          top-left-corner-y (* (quot col 3) 3)
          block-range-x (vec (range top-left-corner-x (+ top-left-corner-x 3)))
          block-range-y (vec (range top-left-corner-y (+ top-left-corner-y 3)))]
      (set (for [x block-range-x
            y block-range-y]
        (get-in board [x y])))))

(defn valid-values-for [board coord]
  (let [valid-vals (set (range 1 10))
        row-vals (row-values board coord)
        col-vals (col-values board coord)
        block-vals (block-values board coord)
        restricted-vals (set/union row-vals col-vals block-vals)]
    (if(has-value? board coord) 
      #{}
      (set/difference valid-vals restricted-vals))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (for [x board] (set x)))

(defn valid-rows? [board]
  ;(let [valid-row (range 1 10)]
  ;  (empty? (filter (fn [row] (not (== row valid-row))) board))))
  nil)

(defn cols [board]
  (for [x (range 0 9)] (col-values board [0 x])))

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