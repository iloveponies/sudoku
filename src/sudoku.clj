(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [rows (range 9) [_ col] coord]
     (set (map #(value-at board [% col]) rows))))

  (defn- top-left [coord]
    (let [[row col] coord
      map-to-corner (fn [x]
      (cond
        (< x 3) 0
        (< x 6) 3
        :else 6))]
    [(map-to-corner row) (map-to-corner col)]))

(defn coord-pairs [coords]
  (for [a coords b coords] [a b]))

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
