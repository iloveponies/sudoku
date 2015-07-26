(ns sudoku
  (:require [clojure.set :as set]))

(def all-values (set (range 1 10)))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn- nth-row [board row]
  (get board row))

(defn- nth-col [board col]
  (map #(get % col) board))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (nth-row board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (nth-col board col))))

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
