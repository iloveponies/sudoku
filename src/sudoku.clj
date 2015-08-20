(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0) false true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (for [y-value (range 9)]
           (value-at board [x y-value])))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (for [x-value (range 9)]
           (value-at board [x-value y])))))

(defn coord-pairs [coords]
  (for [y coords x coords] [y x]))

(defn block-values [board coord]
  (let [[x y] (map #(- %1 (mod %1 3)) coord)]
    (set (for [x-value (range x (+ x 3))
               y-value (range y (+ y 3))]
           (value-at board [x-value y-value])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values
                    (row-values   board coord)
                    (col-values   board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (some zero? (flatten board))))

(defn rows [board]
  (map #(row-values board [%1 0]) (range 9)))

(defn valid-rows? [board]
  (every? #(= all-values %1) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %1]) (range 9)))

(defn valid-cols? [board]
  (every? #(= all-values %1) (cols board)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (every? #(= (sort %) (range 1 10)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows?   board)
       (valid-cols?   board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
