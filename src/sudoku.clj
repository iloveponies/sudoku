(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not(zero? (value-at board coord))))

(defn row-values [board [row col]]
  (loop [r #{}
         i 0]
    (if (< i 9) (recur 
                  (conj r (value-at board [row, i]))
                  (inc i))
      r)))

(defn col-values [board [row col]]
  (loop [r #{}
         i 0]
    (if (< i 9) (recur 
                  (conj r (value-at board [i, col]))
                  (inc i))
      r)))

(defn coord-pairs [coords]
  nil)

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
