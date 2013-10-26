(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def board-values (set (range 1 10)))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row col]]
  (set (get-in board [row])))

(defn col-values [board [row col]]
  (let [col-helper(fn [column-set board-row] (conj column-set (get board-row col))) ]
  (reduce col-helper #{} board)))

(defn coord-pairs [coords]
  (into [] (for [x coords
                 y coords]
             (vector x y))))

(defn block-corner [[row col]]
  (let [corner(fn[x] (-  x (rem x 3)))]
  [(corner row) (corner col)]))

(defn block-values [board coord]
  (let [coord-range(fn[z] (range z (+ 3 z)))]
    (def corner (block-corner coord))
    (into #{} (for [row (coord-range (first corner))
                    col (coord-range (second corner))]
                (value-at board [row col])))))

(defn valid-values-for [board coord]
  nil)

(defn filled? [board]
  nil)

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)

