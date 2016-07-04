(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (get-in board coord))))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (map (fn [elem] (get elem y)) board))))

(defn coord-pairs [coords]
  (for [x  coords
        y  coords]
      (concat [x y] )))

(defn top-left [coords]
  (let [[x y] coords]
    [(- x (mod x 3)) (- y (mod y 3))])  
  )

(defn block-values [board coord]
  (set (let [[topx topy] (top-left coord)]
    (for [x (range topx (+ topx 3))
          y (range topy (+ topy 3))]
      (cons (get-in board [x y])  nil)))))

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
