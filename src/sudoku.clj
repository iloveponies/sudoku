(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0) false true))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (set (map (fn [x] (get x (get coord 1))) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn top-corner [point]
  (vector (* (int (/ (get point 0) 3)) 3) (* (int (/ (get point 1) 3)) 3)))

(defn coord-sets [point]
  (let [tc (top-corner point)
        trow (get tc 0)
        tcol (get tc 1)]
    (for [row (vector trow (+ trow 1) (+ trow 2))
          col (vector tcol (+ tcol 1) (+ tcol 2))]
        (vector row col))))

(defn block-values [board point]
  (set (for [c (coord-sets point)]
         (value-at board c))))


(defn valid-values-for [board coord]
  (if (= (value-at board coord) 0)
    (set/difference all-values (set/union (block-values board coord) (row-values board coord) (col-values board coord)))
    (set nil)))


(defn filled? [board]
  (every? true?
          (for [row (range 0 9)
                col (range 0 9)]
            (if (empty? (valid-values-for board (vector row col)))
              true
              false))))


(defn rows [board]
  (for [row (range 0 9)]
    (row-values board (vector row 0))))

(defn valid-set? [val]
  (empty? (set/difference all-values val)))

(defn valid-rows? [board]
  (every? valid-set? (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board (vector 0 col))))

(defn valid-cols? [board]
  (every? valid-set? (cols board)))

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    (block-values board (vector row col))))

(defn valid-blocks? [board]
  (every? valid-set? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (for [row (range 0 9)
               col (range 0 9)
               :when (= (value-at board (vector row col)) 0)]
           (vector row col))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [emptyPoint (find-empty-point board)
          validValues (valid-values-for board emptyPoint)]
      (for [value validValues
            solution (solve (set-value-at board emptyPoint value))]
        solution))))




