(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map (fn [x] (get x col)) board))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
	(let [[row col] coord
          top-left-corner-x (* (quot row 3) 3) 
          top-left-corner-y (* (quot col 3) 3)
          block-range-x (vec (range top-left-corner-x (+ top-left-corner-x 3)))
          block-range-y (vec (range top-left-corner-y (+ top-left-corner-y 3)))]
      (set (for [x block-range-x
            y block-range-y]
        (get-in board [x y])))))

(defn valid-values-for [board coord]
  (let [valid-vals (set (range 1 10))
        row-vals (row-values board coord)
        col-vals (col-values board coord)
        block-vals (block-values board coord)
        restricted-vals (set/union row-vals col-vals block-vals)]
    (if(has-value? board coord) 
      #{}
      (set/difference valid-vals restricted-vals))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (for [x board] (set x)))

(defn valid-rows? [board]
  (let [valid-row (set (range 1 10))]
  ;  (empty? (filter (fn [row] (not (== row valid-row))) board))))
    (every? (fn [x] (= (set x) valid-row)) (rows board))))

(defn cols [board]
  (for [x (range 0 9)] (col-values board [0 x])))

(defn valid-cols? [board]
   (let [valid-col (set (range 1 10))]
    (every? (fn [x] (= (set x) valid-col)) (cols board))))

(defn blocks [board]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (block-values board [x y])))

(defn valid-blocks? [board]
   (let [valid-block (set (range 1 10))]
    (every? (fn [x] (= (set x) valid-block)) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [empty-point? (fn [a] (let [[x y] a] (not (has-value? board [x y]))))]
    (first (filter empty-point? (coord-pairs (range 0 9))))))

(defn solve-helper [board]
  (let [empty-point (find-empty-point board)]
    (if (filled? board)
      (if (valid-solution? board)
        [board]
        [])
      (for [x (valid-values-for board empty-point)
            solution (solve-helper (assoc-in board empty-point x))]
        solution))))

(defn solve [board] (first (solve-helper board)))