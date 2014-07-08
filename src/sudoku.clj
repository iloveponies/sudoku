(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (let [value (value-at board coord)]
    (not (= value 0))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row] (value-at board [row col])) (range 0 9))))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    [c1 c2]))

(defn block-starts [[row col]]
  [(* (int (/ row 3)) 3) (* (int (/ col 3)) 3)])

(defn block-values [board coord]
  (let [[start-row start-col] (block-starts coord)
        rows (range start-row (+ start-row 3))
        cols (range start-col (+ start-col 3))
        coords (for [row rows
                     col cols]
                 [row col])]
    (set (map (fn [coord] (value-at board coord)) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [used (reduce set/union (map (fn [f] (f board coord)) [row-values col-values block-values]))]
      (set/difference all-values used))))

(defn filled? [board]
  (let [r (range 0 9)
        all-coords (for [row r col r] [row col])]
    (every? (fn [coord] (has-value? board coord)) all-coords)))

(defn row-col-sets-help [board f]
  (map (fn [x] (set (f board [x x]))) (range 0 9)))

(defn rows [board]
  (row-col-sets-help board row-values))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (row-col-sets-help board col-values))

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
