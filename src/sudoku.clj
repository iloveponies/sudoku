(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (for [x (range 9)]
           (value-at board [row x])))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (for [y (range 9)]
           (value-at board [y col])))))

(defn coord-pairs [coords]
  (for [y coords x coords] [y x]))

(defn get-block [coord]
  (let [[y x] coord
        by (cond (< y 3) 0 (< y 6) 3 (< y 9) 6)
        bx (cond (< x 3) 0 (< x 6) 3 (< x 9) 6)]
    [by bx]))

(defn block-values [board coord]
  (let [[by bx] (get-block coord)]
    (set (for [y (range by (+ by 3)) x (range bx (+ bx 3))]
           (value-at board [y x])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (map #(value-at board %) (coord-pairs (range 9)))) 0)))

(defn valid-helper [coll]
  (empty? (remove #(= all-values %) coll)))

(defn rows [board]
  (for [y (range 9)] (row-values board [y 0])))

(defn valid-rows? [board]
  (valid-helper (rows board)))

(defn cols [board]
  (for [x (range 9)] (col-values board [0 x])))

(defn valid-cols? [board]
  (valid-helper (cols board)))

(defn blocks [board]
  (map #(block-values board %) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (valid-helper (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(not (has-value? board %)) (coord-pairs (range 9)))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [point (find-empty-point board)]
      (for [value (valid-values-for board point)
            solution (solve-helper (set-value-at board point value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
