(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coords]
  (get-in board coords))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [coord _]]
  (set (get board coord)))

(defn col-values [board [_ coord]]
  (set (reduce #(conj %1 (get %2 coord)) [] board)))

(defn coord-pairs [a-seq]
    (for [_ a-seq]
      (for [asd a-seq])
      asd))

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
