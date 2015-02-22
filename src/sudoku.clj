(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (let [col-vect
        (for [row (range 9)]
          (value-at board [row col]))]
    (set col-vect)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left [n]
  (* (quot n 3) 3))

(defn block-values [board [row col]]
  (let [top    (top-left row)
        left   (top-left col)
        rows   (range top (+ top 3))
        cols   (range left (+ left 3))
        coords (for [r rows
                     c cols]
                 (value-at board [r c]))]
    (set  coords)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference #{1 2 3 4 5 6 7 8 9}
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn filled? [board]
  (every? identity
          (for [r (range 9)
                c (range 9)]
            (has-value? board [r c]))))

(defn rows [board]
  (for [r (range 9)]
    (row-values board [r 0])))

(defn valid-rows? [board]
  (every? (fn [xs] (== 9 (count xs)))
          (rows board)))

(defn cols [board]
  (for [c (range 9)]
    (col-values board [0 c])))

(defn valid-cols? [board]
  (every? (fn [xs] (== 9 (count xs)))
          (cols board)))

(defn blocks [board]
  (for [r [0 3 6]
        c [0 3 6]]
    (block-values board [r c])))
(defn valid-blocks? [board]
  (every? (fn [xs] (== 9 (count xs)))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [row (range 9)
                     col (range 9)]
                 [row col])]
    (first (filter (fn [coord] (not (has-value?
                                     board coord)))
                   coords))))

(defn solve [board]
  nil)
