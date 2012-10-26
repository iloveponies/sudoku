(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0(value-at board coord))))

(defn row-values [board [x _]]
  (let [row (get board x)
        check? (fn [a](not(= 0 a)))]

    (set row)))

(defn col-values [board [_ y]]
  (loop [acc #{}
         n 8]
    (if(< n 0)
      acc
      (recur (conj acc (get-in board [n y])) (dec n)))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

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