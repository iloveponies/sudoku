(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row-no col-no] coord
        row (get-in board (vector row-no))]
    (set row)))

(defn col-values [board coord]
  (let [[row-no col-no] coord
        col (reduce (fn [acc row] (cons (get-in row (vector col-no)) acc)) [] board)]
    (set col)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
  (vector row col)))

(defn coord-pairs2 [row-coords col-coords]
  (for [row row-coords
        col col-coords]
  (vector row col)))

(defn coord-block [index]
  (cond
   (< index 3) [0 1 2]
   (< index 6) [3 4 5]
   (< index 9) [6 7 8]))

(defn get-block [coord]
  (let [[row-no col-no] coord]
    (coord-pairs2 (coord-block row-no) (coord-block col-no))))

(defn block-values [board coord]
  (let [block-list
        (for [cell (get-block coord)]
          (value-at board cell))]
    (set block-list)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (row-values board coord) (col-values board coord) (block-values board coord))))

(defn board-as-set [board]
  (if (empty? board)
    #{}
    (set/union (set (first board)) (board-as-set (rest board)))))

(defn filled? [board]
  (not (contains? (board-as-set board) 0)))

(defn rows [board]
  (for [row (range 9)]
    (row-values board (vector row 0))))

(defn valid-rows? [board]
  (every? (fn [x] (= x #{1 2 3 4 5 6 7 8 9})) (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (col-values board (vector 0 col))))

(defn valid-cols? [board]
  (every? (fn [x] (= x #{1 2 3 4 5 6 7 8 9})) (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board (vector row col))))

(defn valid-blocks? [board]
  (every? (fn [x] (= x #{1 2 3 4 5 6 7 8 9})) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-empty-points
        (for [row (range 9)
              col (range 9)
              :let [filled (has-value? board [row col])]
              :when (not filled)]
          [row col])]
    (first all-empty-points)))

(defn solve-helper [board current-coord]
  (cond
   (valid-solution? board) board
   (nil? current-coord) []
   :else (for [possible-value (valid-values-for board current-coord)
               solution (solve-helper (set-value-at board current-coord possible-value) (find-empty-point (set-value-at board current-coord possible-value)))]
           solution)))

(defn solve [board]
  (solve-helper board (find-empty-point board)))
