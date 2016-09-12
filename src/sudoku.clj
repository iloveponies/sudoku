(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

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

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [row _]]
  (let [rowvector (get board row)]
    (set (get board row))))

(defn col-values [board [_ col]]
  (set (map #(get %1 col) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn topleft [[row col]]
  [(- row (mod row 3)) (- col (mod col 3))])

(defn block-values [board [row col]]
  (set (map #(value-at board %1) (let [[x y] (topleft [row col])]
                                   (for [a [x (inc x) (inc (inc x))]
                                         b [y (inc y) (inc (inc y))]]
                                     [a b])))))

(defn valid-values-for [board coord]
  (if (= (value-at board coord) 0)
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))
    #{}))

(defn filled? [board]
  (every? empty? (map #(set/difference all-values %1)
                      (map #(set %1) board))))


(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= %1 all-values) (rows board)))

(defn cols [board]
  (for [col-index (range 0 9)]
    (col-values board [0 col-index])))

(defn valid-cols? [board]
  (every? #(= %1 all-values) (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? #(= %1 all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter not-empty (for [row (range 0 9)
                             col (range 0 9)]
                         (if (zero? (value-at board [row col]))
                           [row col]
                           [])))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [empty-location (find-empty-point board)]
      (for [value (valid-values-for board empty-location)]
        (solve (set-value-at board empty-location value))))))
