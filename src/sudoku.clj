(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get-in board [row]))))

(defn col-values [board coord]
  (let [[_ col] coord
        column-values (fn [acc row]
              (conj acc (get-in row [col])))]
    (reduce column-values #{} board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-top-left [coords]
  (let [[x y] coords]
    [(* 3 (int (/ x 3))) (* 3 (int (/ y 3)))]))

(defn block-values [board coord]
  (let [[x y] (block-top-left coord)
        coords (for [offset-x [0 1 2]
                     offset-y [0 1 2]]
                 [(+ x offset-x) (+ y offset-y)])
        values (fn [acc xy]
                (conj acc (value-at board xy)))]
    (reduce values #{} coords)))


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
