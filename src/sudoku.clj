(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (> (get-in board coord) 0) true false))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (let [get-col (fn [a-seq] (get a-seq (second coord)))]
    (set (map get-col board))))

(defn coord-pairs [coords]
  (for [name coords
        number coords]
    (list name number)))

(defn top-left [coords]
  (let [set-coord (fn [x] (*  (int (Math/floor (/ x 3))) 3))]
    (map set-coord coords)))



(defn get-block [top-coord]
  (let [start (first top-coord)
        values (range  3)
        col (second top-coord)
        set-col (fn [x] [ (+ start (first x))
                         (+ (second x) col)])]
    (map set-col (coord-pairs values))))


(defn block-values [board coord]
  (let [top (top-left coord)
        block (get-block top)
        value-at-board (fn [x] (value-at board x))]
    (set (map value-at-board block))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) '#{}
    (let [row (row-values board coord)
          column (col-values board coord)
          block (block-values board coord)
          filter-vals (fn [x] (not (or (contains? row x) (contains? column x) (contains? block x))))]
      (set (filter filter-vals all-values)))))


(defn filled? [board]
  (let [filter-row (fn [x] (== 9 (count (set x))))]
   (== 9 (count (filter filter-row board)))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (let [filter-rows (fn [x] (== (count (set x)) 9))]
    (== (count (filter filter-rows (rows board))) 9)))

(defn col-values-by-index [board index]
  (let [get-col (fn [a-seq] (get a-seq index))]
    (set (map get-col board))))


(defn cols [board]
  (let [colf (fn [x] (col-values-by-index board x))]
  (map colf (range 9))))

(defn valid-cols? [board]
  (let [filter-rows (fn [x] (== (count (set x)) 9))]
    (== (count (filter filter-rows (cols board))) 9)))

(defn blocks [board]
  (let [blocks [ [0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]
        blockf (fn [x] (block-values board x))]
     (map blockf blocks)))

(defn valid-blocks? [board]
  (let [filter-rows (fn [x] (== (count (set x)) 9))]
    (== (count (filter filter-rows (blocks board))) 9)))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter boolean
                 (for [row (range 9)
                       column (range 9)]
                   (if (not (has-value? board [row column])) [row column] false)))))

(defn solve [board]
 nil )
