(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord
        get-col-value (fn [row] (get row col))]
    (set (map get-col-value board))))
    
(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left-block-coordinates [board coord]
  (let [ [x y] coord
         tl (fn [n] (- n (mod n 3)))]
    [(tl x) (tl y)]))

(defn block-values [board coord]
  (let [tl (top-left-block-coordinates board coord)
        [tl-x tl-y] tl]
    (set (for [x (range tl-x (+ tl-x 3))
          y (range tl-y (+ tl-y 3))]
        (value-at board [x y])))))
        
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
