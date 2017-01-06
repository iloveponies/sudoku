(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board [(get coord 0) (get coord 1)]))

(defn has-value? [board coord]
  (pos? (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [x] (value-at board [x col])) (range 0 9)))))

(defn coord-pairs [coords]
  (for [arr1 coords arr2 coords] (vec [arr1 arr2])))

(defn block-values [board coord]
  (let [[row col] coord
        [block-top block-left] [(- row (mod row 3))(- col (mod col 3))]]
   (set (map (fn [x] (value-at board x)) 
      (for [x (range block-top  (+ block-top  3))
            y (range block-left (+ block-left 3))]
         (vec [x y]))))))

(defn valid-values-for [board coord]
  )

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
