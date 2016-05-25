(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (for [x [0 1 2 3 4 5 6 7 8]]
         (value-at board [x (last coord)]))))

(defn coord-pairs [coords]
  (for [x coords y coords] (vector x y)))

(defn top-left [[x y]]
  [(- x (mod x 3)) (- y (mod y 3))])

(defn block-values [board coord]
  (let [tl (top-left coord)
        coords (map #(map + tl %) (coord-pairs [0 1 2]))]
    (set (map #(value-at board %) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    (set nil)
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  ( let [all-set (set (map #( value-at board %)(coord-pairs [0 1 2 3 4 5 6 7 8])))]
    (if (contains? all-set 0)
    false
    true)))

(defn rows [board]
  (for [x [0 1 2 3 4 5 6 7 8]]
    (row-values board [x 0])))

(defn valid-rows? [board]
  (let [rows-set (set (rows board))]
    (if (and
          (= 1 (count rows-set))
          (contains? rows-set all-values))
      true
      false)))

(defn cols [board]
  (for [y [0 1 2 3 4 5 6 7 8]]
    (col-values board [0 y])))

(defn valid-cols? [board]
  (let [cols-set (set (rows board))]
    (if (and
          (= 1 (count cols-set))
          (contains? cols-set all-values))
      true
      false)))

(defn blocks [board]
  (let [blocks-coord (coord-pairs [0 3 6])]
    (map #(block-values board %) blocks-coord)))

(defn valid-blocks? [board]
  (let [blocks-set (set (rows board))]
    (if (and
          (= 1 (count blocks-set))
          (contains? blocks-set all-values))
      true
      false)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter
          #(not (has-value? board %))
          (for [x (range 9)
                y (range 9)]
            [x y ]))))

(defn solve-helper [board]
 (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [empty-point (find-empty-point board)]
      (for [valid-values (valid-values-for board empty-point)
            solution (solve-helper (set-value-at board empty-point valid-values))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
