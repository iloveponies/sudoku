(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(comment
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

(def solved-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))
)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (pos? (value-at board coord)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map (fn [x] (get x (second coord))) board)))

(defn coord-pairs [coords]
  (for [x coords y coords]
        [x y]))

(defn top-left-corner [coord]
  [(int (* 3 (Math/floor (/ (first coord) 3))))
   (int (* 3 (Math/floor (/ (second coord) 3))))])

(defn block-coords [board coord]
 (let [ tlc (top-left-corner coord)]
    (map (fn [x] (map + x tlc)) (coord-pairs [0 1 2]))))

(defn block-values [board coord]
  (set (map (fn [x] (value-at board x)) (block-coords board coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (block-values board coord)
                                          (row-values board coord)
                                          (col-values board coord)))))

(defn filled? [board]
  (let [all-numbers (apply concat board)]
    (not (contains? (set all-numbers) 0))))

(defn rows [board]
  (map set board))

(defn valid-set? [a-set]
  (every? (fn [x] (= x all-values)) a-set))

(defn valid-rows? [board]
  (valid-set? (rows board)))

(defn cols [board]
  (for [y (range 0 9)]
    (set (map (fn [x] (get x y)) board))))

(defn valid-cols? [board]
  (valid-set? (cols board)))

(defn blocks [board]
  (for [x [0 4 8]
        y [0 4 8]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (valid-set? (blocks board)))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (coord-pairs (range 0 9))]
    (first (filter (fn [x] (= 0 (value-at board x))) coords))))

(defn solve [board]
  nil)



