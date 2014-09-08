(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (into #{} (get board row)))

(defn col-values [board [_ col]]
  (into #{} (for [row board] (get row col))))

(defn coord-pairs [coords]
  (vec
   (for [coord-outer coords
          coord-inner coords]
      (vector coord-outer coord-inner))))

(defn- -block-range [[x y]]
  (let [top-left-x (* (quot x 3) 3)
        top-left-y (* (quot y 3) 3)
        bottom-right-x (+ top-left-x 3)
        bottom-right-y (+ top-left-y 3)]
    [top-left-x top-left-y bottom-right-x bottom-right-y]))

(-block-range [0 2])

(defn block-values [board coord]
  (let [[topleft-x topleft-y bottomright-x bottomright-y] (-block-range coord)]
    (into #{}
     (for [x (range topleft-x bottomright-x)
            y (range topleft-y bottomright-y)]
        (value-at board [x y])))))

(defn valid-values-for [board coord]
  (let [block-vals (block-values board coord)
        row-vals   (row-values board coord)
        col-vals   (col-values board coord)]
    (if (has-value? board coord)
      #{}
      (set/difference all-values row-vals col-vals block-vals))))

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
