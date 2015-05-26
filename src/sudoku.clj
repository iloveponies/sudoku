(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (== (value-at board coord) 0) false true))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (let [col (fn [x] (get x (get coord 1)))]
  (set (map col board))))

(defn coord-pairs [coords]
  (for [first coords 
        second coords] (assoc (assoc [] 0 first) 1 second)))

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
