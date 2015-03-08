(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
 (let [[row col] coord]
    (set (map (fn [r] (get r col)) board))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn top-left-coord [coord]
  (let [[row col] coord
        f (fn [x] (- x (mod x 3)))]
    [(f row) (f col)]))

(defn block-values [board coord]
  (let [[left-x left-y] (top-left-coord coord)
        a-range (range 3)]
    (set (for [x a-range y a-range]
           (value-at board [(+ left-x x) (+ left-y y)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union
                                 (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (for [row (range 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (for [col (range 9)]
    (col-values board [0 col])))

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
