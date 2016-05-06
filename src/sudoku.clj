(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board coord]
  (let [[x1 x2 x3 x4 x5 x6 x7 x8 x9] (get board (get coord 0)) ]
    (conj #{} x1 x2 x3 x4 x5 x6 x7 x8 x9 )))

(defn col-values [board coord]
  (loop [x 0 colset #{}]
    (if (> x 8)
      colset
      (recur (inc x) (conj colset (value-at board [x (get coord 1)]))))))

(defn coord-pairs [coords]
  (for [first coords second coords]
    [first second]))

(defn block-helper [coord]
  (let [[x y] coord]
  [(- x (mod x 3)) (- y (mod y 3))]))

(defn block-values [board coord]
  (let [[x y] (block-helper coord)]
    (set (for [coordinate [[x y] [(+ x 1) y] [(+ x 2) y]
                           [x (+ y 1)] [(+ x 1) (+ y 1)] [(+ x 2) (+ y 1)]
                           [x (+ y 2)] [(+ x 1) (+ y 2)] [(+ x 2) (+ y 2)]]]
       (value-at board coordinate)))))

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (set/difference (set/difference (set/difference #{1 2 3 4 5 6 7 8 9}
                                                    (block-values board coord))
                                    (row-values board coord))
                    (col-values board coord))
    #{}))

(defn fill-helper [board]
  (for [coord (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (value-at board coord)))

(defn filled? [board]
  (not (contains? (set (fill-helper board)) 0)))

(defn rows [board]
  (for [number #{0 1 2 3 4 5 6 7 8}]
    (set (row-values board [number 0]))))

(defn valid-rows? [board]
  (not (contains? (set (for [row (rows board)]
                   (and (== (count row) 9) (not (contains? row 0)))))
                  false)))

(defn cols [board]
  (for [number #{0 1 2 3 4 5 6 7 8}]
    (set (col-values board [0 number]))))

(defn valid-cols? [board]
  (not (contains? (set (for [col (cols board)]
                   (and (== (count col) 9) (not (contains? col 0)))))
                  false)))

(defn blocks [board]
  (for [coords [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]]
    (set (block-values board coords))))

(defn valid-blocks? [board]
  (not (contains? (set (for [block (blocks board)]
                   (and (== (count block) 9) (not (contains? block 0)))))
                  false)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-helper [board]
  (set (for [coords (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (if (zero? (value-at board coords))
      coords
      nil))))

(defn find-empty-point [board]
  (first (set/difference (find-empty-helper board) #{nil})))

(defn solve [board]
  nil)
