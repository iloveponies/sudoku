(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def sb
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(value-at sb [0 1])

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(has-value? sb [0 2])

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(row-values sb [0 1])

(defn col-values [board coord]
  (loop [values #{}
         i 0]
    (if (= i 9)
      values
      (recur
        (conj
          values
          (value-at board [i (last coord)]))
        (inc i)))))

(col-values sb [4 8])

(defn coord-pairs [coords]
  (for [c1 coords
      c2 coords]
    [c1 c2]))

(coord-pairs [0 2 2])

(defn top-left [coord]
  (let [r (first coord)
        c (last coord)]
    (cond
      (< r 3) (cond
                (< c 3) [0 0]
                (< c 6) [0 3]
                (< c 9) [0 6])
      (< r 5) (cond
                (< c 3) [3 0]
                (< c 6) [3 3]
                (< c 9) [3 6])
      (< r 9) (cond
                (< c 3) [6 0]
                (< c 6) [6 3]
                (< c 9) [6 6]))))

(value-at sb (top-left [8 8]))


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
