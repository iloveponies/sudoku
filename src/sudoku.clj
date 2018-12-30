(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [row _]]
  (reduce (fn [vals col] (conj vals (value-at board [row, col]))) #{} (range 9)))

(defn col-values [board [_ col]]
  (reduce (fn [vals row] (conj vals (value-at board [row, col]))) #{} (range 9)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [min-block (fn [x] (* (int (/ x 3)) 3))
        coords (for [[r c] (coord-pairs [0 1 2])]
                 [(+ r (min-block row)) (+ c (min-block col))])]
    (reduce (fn [vals [r c]] (conj vals (value-at board [r, c]))) #{} coords)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [vals-in-use (set/union (row-values board coord) (col-values board coord) (block-values board coord))]
      (set/difference all-values vals-in-use))))

(defn filled? [board]
  (let [vals (set (apply concat board))]
    (not (contains? vals 0))))

(defn rows [board]
  (for [row (range 9)]
    (row-values board [row 0])))

(defn valid-groups? [seq-of-sets]
  (let [is-invalid? (fn [vals] (or (contains? vals 0) (< (count vals) 9)))]
    (empty? (filter is-invalid? seq-of-sets))))

(defn valid-rows? [board] (valid-groups? (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]  (valid-groups? (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]  (valid-groups? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point-helper [board board-coords]
  (cond
    (empty? board-coords) nil
    (not (has-value? board (first board-coords))) (first board-coords)
    :else (find-empty-point-helper board (rest board-coords))))

(defn find-empty-point [board]
  (find-empty-point-helper board (coord-pairs (range 9))))


(defn solve-helper [board]
  (let [filled (filled? board)]
    (cond
      (and filled (valid-solution? board)) [board]
      filled []
      :else (let [coord (find-empty-point board)]
              (for [value (valid-values-for board coord)
                    solution (solve-helper (set-value-at board coord value))]
                solution)))))

(defn solve [board]
  (first (solve-helper board)))
