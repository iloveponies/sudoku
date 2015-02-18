(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (< 0 (value-at board coord))
    true
    false))

(defn row-values [board coord]
  (let [[row] coord]
    (into #{} (get board row))))

(defn col-values [board coord]
  (let [[row column] coord]
    (into #{} 
      (reduce (fn [acc x] (conj acc (get x column))) [] board))))

(defn coord-pairs [coords]
  (for [point-1 coords
        point-2 coords]
    (vector point-1 point-2)))

(defn top-left [board coord]
  (let [[x y] coord]
    (vector (- x (mod x 3)) (- y (mod y 3)))))

(defn block-values [board coord]
  (let [[top-left-row top-left-column] (top-left board coord)
        rows-and-columns [0 1 2]]
    (into #{}
      (for [row rows-and-columns
            column rows-and-columns]
        (value-at board (vector (+ row top-left-row) (+ column top-left-column)))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference 
      all-values 
      (block-values board coord) 
      (row-values board coord) 
      (col-values board coord))))

(defn filled? [board]
  (not (contains? (into #{} (apply set/union board)) 0)))

(defn rows [board]
  (for [row (into [] all-values)]
    (row-values board (vector (- row 1) 0))))

(defn valid-rows? [board]
  (let [all-rows (rows board)]
    (every? (fn [row] (= row all-values)) all-rows)))

(defn cols [board]
  (for [column (into [] all-values)]
    (col-values board (vector 0 (- column 1)))))

(defn valid-cols? [board]
  (let [all-cols (cols board)]
    (every? (fn [column] (= column all-values)) all-cols)))

(defn blocks [board]
  (let [corner-indexes [0 3 6]]
    (for [row corner-indexes
          column corner-indexes]
      (block-values board (vector row column)))))

(defn valid-blocks? [board]
  (let [all-blocks (cols board)]
    (every? (fn [block] (= block all-values)) all-blocks)))

(defn valid-solution? [board]
  (and 
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first 
    (filter (fn [coord] (not (has-value? board coord)))
      (coord-pairs
        (map dec all-values)))))

(defn solve-helper [current-board]
  (if (filled? current-board)
    (if (valid-solution? current-board)
      current-board
      '())
    (let [remaining (find-empty-point current-board)]
      (for [elem (valid-values-for current-board remaining)
            solution (solve-helper 
                       (set-value-at current-board remaining elem))]
        solution))))

(defn solve [board]
  (solve-helper board))