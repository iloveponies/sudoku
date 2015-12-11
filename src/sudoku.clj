(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map (fn [x] (get x (second coord))) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [corner-pos
        ((fn [c] (vector (- (first c) (mod (first c) 3)) (- (second c) (mod (second c) 3)))) coord)]
    (set (for [row (list (first corner-pos) (inc (first corner-pos)) (+ 2 (first corner-pos)))
          col (list (second corner-pos) (inc (second corner-pos)) (+ 2 (second corner-pos)))]
       (value-at board [row col])))
    ))  

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
