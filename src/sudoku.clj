(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board [row]]
  (set (get board row)))

(defn col-values [board [_ column]]
  (set (map (fn [row] (nth row column)) board)))

(defn coord-pairs [coord-sequence]
  (for [x coord-sequence
        y coord-sequence]
    [x y]))

(defn block-left-top [row column]
  [(* 3 (int (/ row 3))) (* 3(int (/ column 3)))])

(defn block-values [board [row column]]
  (let [[block-left block-top] (block-left-top row column)]
    (set (map (partial value-at board)
              (map (fn [[x y]] [(+ x block-left) (+ y block-top)])
                   (coord-pairs [0 1 2]))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (for [row (range 9)]
    (row-values board [row])))

(defn valid-*? [coll]
  (apply (comp min (partial = 9)) (map count coll)))

(defn valid-rows? [board]
  (valid-*? (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (valid-*? (cols board)))

(defn blocks [board]
  (map (partial block-values board) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (valid-*? (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter boolean (for [row (range 9)
        col (range 9)]
    (if (not (has-value? board [row col]))
      [row col]
      false)))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      '[])
    (let [empty-point (find-empty-point board)
          valid-values (valid-values-for board empty-point)]
      (for [value valid-values
            solution (solve-helper (set-value-at board empty-point value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))