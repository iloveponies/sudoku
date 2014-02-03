(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row-index _]]
  (set (board row-index)))

(defn col-values [board [_ column-index]]
  (let [column (map (fn [row] (row column-index)) board)]
    (set column)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn round-multiple-of-three [n]
  (* 3 (quot n 3)))

(defn block-origin [[row col]]
  [(round-multiple-of-three row)
   (round-multiple-of-three col)])

(defn block-coordinates
  "Get the block coordinates for a given coordinate."
  [coord]
  (let [[row-origin col-origin] (block-origin coord)]
    (for [row (range row-origin (+ 3 row-origin))
          col (range col-origin (+ 3 col-origin))]
      [row col])))

(defn block-values [board coord]
  (let [coordinates (block-coordinates coord)
        values (map (fn [coordinate] (value-at board coordinate)) coordinates)]
    (set values)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [taken-row-values (row-values board coord)
          taken-col-values (col-values board coord)
          taken-block-values (block-values board coord)]
      (set/difference all-values taken-row-values taken-col-values taken-block-values))))

(defn board-values [board]
  (for [row (range 9)
        col (range 9)]
    (value-at board [row col])))

(defn filled? [board]
  (every? (fn [value] (contains? all-values value)) (board-values board)))

(defn rows [board]
  (for [row (range 9)]
    (row-values board [row nil])))

(defn valid-sets?
  "Returns true if all sets are are valid.
   A valid set is a set of all values."
  [value-sets]
  (every? (fn [value-set] (= all-values value-set)) value-sets))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (col-values board [nil col])))

(defn valid-cols? [board]
  (valid-sets? (cols board)))

(defn blocks [board]
  (let [steps (range 0 9 3)]
    (for [row steps
          col steps]
      (block-values board [row col]))))

(defn valid-blocks? [board]
  (valid-sets? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
