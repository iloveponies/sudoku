(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
 (not (zero? (get-in board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [x] (value-at board [x col])) (range 9)))))

(defn coord-pairs [coords]
  (apply vector (for [row coords
                      col coords]
                  [row col])))

(defn block-values-helper [coord]
  (let [[x y] coord]
    [(* (quot x 3) 3) (* (quot y 3) 3)]))

(defn block-values [board coord]
  (let [[start-r start-c] (block-values-helper coord)]
    (set (for [row (range start-r (+ 3 start-r))
               col (range start-c (+ 3 start-c))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

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
