(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board [row]]
  (set (get board row)))

(defn col-values [board coord]
  (let [[x y] coord]
    (row-values (apply mapv vector board) [y x])))

(defn coord-pairs [coords]
  (for [x coords y coords] [x y]))

(defn top-left [coords]
  (map (fn [x] (* 3 (int (/ x 3))) ) coords))

(defn block-values [board coord]
  (set (let [[x y] (top-left coord)]
    (for [i [0 1 2] j [0 1 2]]
      (value-at board [(+ i x) (+ j y)] )))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values (set/union
                                (col-values board coord)
                                (block-values board coord)
                                (row-values board coord)))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (map (fn [x] (set x)) board))

(defn valid-rows? [board]
  (every? #(empty? (set/difference all-values %)) (rows board)))

(defn cols [board]
  (rows (apply mapv vector board)))

(defn valid-cols? [board]
  (valid-rows? (apply mapv vector board)))

(defn blocks [board]
  (for [row [0 3 6] col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? #(empty? (set/difference all-values %)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
