(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (get-in board coord) 0)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (reduce
    (fn [a b]
      (conj a (value-at board [b (second coord)])))
    #{}
    (range 9)))

(defn coord-pairs [coords]
  (for [y coords
        x coords]
    [y x]))

(defn block-values [board coord]
  nil)

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
