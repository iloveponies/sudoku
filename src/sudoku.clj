(ns sudoku
  (:require [clojure.set :as set]))

; Definitions of the data structures and helper functions
(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

; Functions to implement that make tests pass:
(defn value-at [board coord]
 (get-in board coord))

(defn has-value? [board coord]
 (not (= 0 (value-at board coord))))

(defn row-values [board coord]
 (let [[row-number _] coord
       row (get board row-number)]
  (set row)))

(defn col-values [board coord]
 (let [[_ col-number] coord
       column (map #(get % col-number) board)]
  (set column)))

(defn coord-pairs [coords]
 (for [row coords
       col coords]
  [row col]))

(defn corner-number [n]
  (cond
   (<= 0 n 2) 0
   (<= 3 n 5) 3
   (<= 6 n 8) 6
   :else false))

(defn block-corner [coord]
 (map corner-number coord))

(defn block-values [board coord]
 (let [[coord-corner-x coord-corner-y] (block-corner coord)
       block-values-list (for [x (range coord-corner-x (+ coord-corner-x 3))
                               y (range coord-corner-y (+ coord-corner-y 3))]
                          (value-at board [x y]))]
  (set block-values-list)))

(defn valid-values-for [board coord]
 (if (has-value? board coord)
  #{}
  (set/difference all-values
                  (row-values board coord)
                  (col-values board coord)
                  (block-values board coord))))

(defn filled? [board]
 (let [all-values-concat (reduce concat [] board)
       all-values-concat-set (set all-values-concat)
       board-has-empty-value (contains? all-values-concat-set 0)]
  (not board-has-empty-value)))

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
