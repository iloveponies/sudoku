(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

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

(defn top-left-corner [coord]
  (vector
    (* 3 (int (/ (first coord) 3)))
    (* 3 (int (/ (second coord) 3)))))

(defn coord-pairs-in-block [coord]
  (map
    (fn [coord-pair]
      [(+ (first coord-pair) (first (top-left-corner coord)))
       (+ (second coord-pair) (second (top-left-corner coord)))])
    (coord-pairs [0 1 2])))

(defn block-values [board coord]
  (reduce
    (fn [result-set coords-vec]
      (conj result-set (value-at board coords-vec)))
    #{}
    (coord-pairs-in-block coord)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (set/union
        (block-values board coord)
        (row-values board coord)
        (col-values board coord)))))

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
