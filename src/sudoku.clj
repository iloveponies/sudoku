(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row-n _] coord]
    (set (get board row-n))))

(defn col-values [board coord]
  (let [[_ col-n] coord]
    (set (map (fn [row-n] (get row-n col-n)) board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
      (concat [row col])))

(defn top-left [coord]
  (let [[row col] coord]
    [(- row (mod row 3)) (- col (mod col 3))]))

(defn block-values [board coord]
  (let [[top left] (top-left coord)]
    (set (map first (for [row (range top (+ top 3))
                          col (range left (+ left 3))]
                     (cons (value-at board [row col]) nil))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (let [all-values (set (range 1 10))
        used-values (set/union
         (row-values board coord)
         (col-values board coord)
         (block-values board coord))]
    (set/difference all-values used-values))))

(defn values-in [board]
  (apply set/union
    (for [row-n (range 0 9)]
      (set/union (row-values board [row-n 0])))))

(defn filled? [board]
  (not (contains? (values-in board) 0)))

(defn rows [board]
  (for [row-n (range 0 9)]
    (set/union (row-values board [row-n 0]))))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (for [col-n (range 0 9)]
    (set/union (col-values board [0 col-n]))))

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  (let [coords (coord-pairs [0 3 6])]
    (map (fn [coord] (block-values board coord)) coords)))

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
