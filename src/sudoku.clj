(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def all-values #{1 2 3 4 5 6 7 8 9})



(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[k i] coord]
        (set (map (fn [x] (get x i)) board))))

(defn coord-pairs [coords]
  (into [] (for [row coords
        col coords]
        [row col])))

(defn block-values [board coord]
  (let [top-left-coord 
          (fn [coord]
            (let [[row col] coord]
              [(- row (mod row 3)) (- col (mod col 3))]))
        [top-left-x top-left-y] (top-left-coord coord)]
    (set (for [x (range 3) y (range 3)]
           (value-at board [(+ top-left-x x) (+ top-left-y y)])))))

(defn valid-values-for [board coord]
  (let [block-vals (block-values board coord)
        row-vals (row-values board coord)
        col-vals (col-values board coord)
        combined-vals (set/union block-vals row-vals col-vals)]
      (if (has-value? board coord)
        #{}
        (set/difference all-values combined-vals))))

(defn filled? [board]
  (let [board-values
          (fn [board]
            (set (for [x (range 9) y (range 9)]
              (value-at board [x y]))))]
    (not (contains? (board-values board) 0))))

(defn rows [board]
  (for [x (range 9)]
          (row-values board [x 0])))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (for [y (range 9)]
        (col-values board [0 y])))

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
