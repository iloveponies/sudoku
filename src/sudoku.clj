(ns sudoku
  (:require [clojure.set :as set])
  (:require [clojure.math.combinatorics :as combo])
  )

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not ( = 0 (value-at board coord))))

(defn row-values [board [row col]]
  (set (get-in board [row])))

(defn col-values [board [row col]]
  (set (map #(get-in board [% col]) (range 0 9))))

(defn coord-pairs [coords]
  (combo/permutations coords))

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
