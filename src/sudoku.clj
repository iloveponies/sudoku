(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn row-coord [coord]
  (nth coord 0))

(defn col-coord [coord]
  (nth coord 1))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (row-coord coord ))))

(defn col-values [board coord]
  (let [column (col-coord coord)
        add-row-value-to-set (fn [current-set row]
                               (conj current-set (value-at board [row column])))]
  (reduce add-row-value-to-set #{} (range 0 9))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn top-left-block []
  (coord-pairs [0 1 2]))

(defn increase-row-coord [coord-seq value]
  (for [coord coord-seq] [(+ (nth coord 0) value) (nth coord 1)]))

(defn increase-col-coord [coord-seq value]
  (for [coord coord-seq] [(nth coord 0) (+ (nth coord 1) value)]))

(defn block-coords [coord]
  (let [row-offset (* 3 (int (Math/floor (/ (row-coord coord) 3))))
        col-offset (* 3 (int (Math/floor (/ (col-coord coord) 3))))]
    (increase-row-coord (increase-col-coord (top-left-block) col-offset) row-offset)))

(defn block-values [board coord]
  (let [add-value-to-set (fn [current-set coordinate]
                           (conj current-set (value-at board coordinate)))]
    (reduce add-value-to-set #{} (block-coords coord))))

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
