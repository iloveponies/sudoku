(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (for [row (range 9)] (value-at board [row col]))))

(defn coord-pairs [coords]
  (vec (for [x coords y coords] [x y])))

(defn upper-left-coords [[row col]]
  [(- row (mod row 3)) (- col (mod col 3))])

(defn square-coords [[row col]]
  (let [[left up] (upper-left-coords [row col])]
    (vec (for [x (range left (+ left 3)) y (range up (+ up 3))] [x y]))))

(defn all-coords []
  (vec (for [x (range 9) y (range 9)] [x y])))

(defn block-values [board coord]
  (set (for [[x y] (square-coords coord)] (value-at board [x y]))))

(defn valid-values-for [board coord]
  (if (not= (value-at board coord) 0)
    #{}
    (clojure.set/difference all-values
                            (block-values board coord)
                            (row-values board coord)
                            (col-values board coord))))

(defn filled? [board]
  (every? #(not= (value-at board %) 0) (all-coords)))

(defn rows [board]
  (vec (for [row (range 9)] (row-values board [row 0]))))

(defn valid-rows? [board]
  (= (set (rows board)) #{all-values}))

(defn cols [board]
  (vec (for [col (range 9)] (col-values board [0 col]))))

(defn valid-cols? [board]
  (= (set (cols board)) #{all-values}))

(defn blocks [board]
  (vec (for [x (range 0 9 3) y (range 0 9 3)] (block-values board [x y]))))

(defn valid-blocks? [board]
  (= (set (blocks board)) #{all-values}))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(= (value-at board %) 0) (all-coords))))

(defn solve [board]
  (if (nil? (find-empty-point board))
    (if (valid-solution? board)
      board
      [])
    (let [next-empty (find-empty-point board)]
      (for [valid-value (valid-values-for board next-empty)
            solution (solve (set-value-at board next-empty valid-value))]
        solution))))
