(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (map (fn [x] (value-at board [row x])) (range 0 9)))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map (fn [x] (value-at board [x col])) (range 0 9)))))

(defn coord-pairs [coord-sequence]
  (for [row coord-sequence col coord-sequence] [row col]))

(defn top-left [coord]
  (let [[row col] coord]
    [(* (int (/ row 3)) 3) (* (int (/ col 3)) 3)]))

(defn block-coords [coord]
  (let [[top left] (top-left coord)]
    (for [row [top (inc top) (inc (inc top))] col [left (inc left) (inc (inc left))]] [row col])))

(defn block-values [board coord]
  (set (map (fn [x] (value-at board x)) (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (map (fn [x] (has-value? board x)) (coord-pairs [0 1 2 3 4 5 6 7 8]))) false)))

(defn rows [board]
  (map (fn [x] (row-values board [x 0])) (range 0 9)))

(defn valid-rows? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (rows board)))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 0 9)))

(defn valid-cols? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (cols board)))

(defn blocks [board]
  (map (fn [coord] (block-values board coord)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
