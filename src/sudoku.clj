(ns sudoku
  (:require [clojure.set :as set]))


(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

;(def sudoku-board
;  (board [[5 3 0 0 7 0 0 0 0]
;          [6 0 0 1 9 5 0 0 0]
;          [0 9 8 0 0 0 0 6 0]
;          [8 0 0 0 6 0 0 0 3]
;          [4 0 0 8 0 3 0 0 1]
;          [7 0 0 0 2 0 0 0 6]
;          [0 6 0 0 0 0 2 8 0]
;          [0 0 0 4 1 9 0 0 5]
;          [0 0 0 0 8 0 0 7 9]]))
;
;(def solved-board
;  (board [[5 3 4 6 7 8 9 1 2]
;          [6 7 2 1 9 5 3 4 8]
;          [1 9 8 3 4 2 5 6 7]
;          [8 5 9 7 6 1 4 2 3]
;          [4 2 6 8 5 3 7 9 1]
;          [7 1 3 9 2 4 8 5 6]
;          [9 6 1 5 3 7 2 8 4]
;          [2 8 7 4 1 9 6 3 5]
;          [3 4 5 2 8 6 1 7 9]]))
;
;(def before-change
;  (board [[5 3 0 0 7 0 0 0 0]
;          [6 0 0 1 9 5 0 0 0]
;          [0 9 8 0 0 0 0 6 0]
;          [8 0 0 0 6 0 0 0 3]
;          [4 0 0 8 0 3 0 0 1]
;          [7 0 0 0 2 0 0 0 6]
;          [0 6 0 0 0 0 2 8 0]
;          [0 0 0 4 1 9 0 0 5]
;          [0 0 0 0 8 0 0 7 9]]))
;
;(def after-change
;  (board [[5 3 0 0 7 0 0 0 0]
;          [6 0 0 1 9 5 0 0 0]
;          [0 4 8 0 0 0 0 6 0]
;          [8 0 0 0 6 0 0 0 3]
;          [4 0 0 8 0 3 0 0 1]
;          [7 0 0 0 2 0 0 0 6]
;          [0 6 0 0 0 0 2 8 0]
;          [0 0 0 4 1 9 0 0 5]
;          [0 0 0 0 8 0 0 7 9]]))


(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (reduce (fn [col row]
                 (conj col (get row (second coord))))
               [(get (first board) (second coord))]
               (rest board))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [topLeftCoordFunc (fn [coord] (let [x (first coord)
                                           y (second coord)]
                                       [(- x (mod x 3)) (- y (mod y 3))]))
        topLeftCoord (topLeftCoordFunc coord)
        coordPairs (coord-pairs [0 1 2])
        coords (for [pair coordPairs]
                 [(+ (first pair) (first topLeftCoord))
                  (+ (second pair) (second topLeftCoord))])]
    (set (for [c coords] (value-at board c)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (col-values board coord)
      (row-values board coord)
      (block-values board coord))))

(defn filled? [board]
  (let [board-seq (set (apply concat board))]
    board-seq
    (if (contains? board-seq 0)
      false
      true)))

(defn rows [board]
  (for [row board]
    (set row)))

(defn valid-set? [a-set]
  (= all-values a-set))

(defn valid-rows? [board]
  (every? (fn [row] (valid-set? (row-values board [row 0]))) (range 0 9)))

(defn cols [board]
  (for [col (range 0 9)]
    (set (map (fn [row] (get row col)) board))))

(defn valid-cols? [board]
  (every? (fn [col] (valid-set? (col-values board [0 col]))) (range 0 9)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (every? (fn [block] (valid-set? block)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter 
           (complement nil?)
           (for [x (range 0 9)
                 y (range 0 9)]
             (if (zero? (value-at board [x y]))
               [x y]
               nil)))))


(defn solve-helper [cur-board]
  (if (filled? cur-board)
    (if (valid-solution? cur-board)
      [cur-board]
      [])
    (let [empty-loc (find-empty-point cur-board)
          possible-vals (valid-values-for cur-board empty-loc)]
      (for [val possible-vals
            solution (let [new-board (set-value-at cur-board empty-loc val)]
                       (solve-helper new-board))]
        solution))))
 (defn solve [board]
     (solve-helper board))

 
 