(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (get-in board coord)))

(defn row-values [board [row _column]]
  (set (get board row)))

(defn col-values [board [_row column]]
  (set (map #(get % column) board)))

(defn coord-pairs [coords]
  (for [row    coords
        column coords]
    [row column]))

(defn corner-coordinate [row-or-column]
  (cond (<= 0 row-or-column 2) 0
        (<= 3 row-or-column 5) 3
        (<= 6 row-or-column 8) 6))

(defn top-left-corner [[row column]]
  [(corner-coordinate row) (corner-coordinate column)])

(defn block-coordinates [[corner-row corner-column]]
  (for [row (range corner-row (+ 3 corner-row))
        column (range corner-column (+ 3 corner-column))]
    [row column]))

(defn block-values [board coord]
  (let [corner-coords (top-left-corner coord)
        coords        (block-coordinates corner-coords)]
    (set (map #(value-at board %) coords))))

(defn valid-values-for [board coord]
  (let [value (value-at board coord)]
    (if (= 0 value)
      (let [values (set/union (col-values board coord) (row-values board coord) (block-values board coord))]
        (set/difference all-values values))
      #{})))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (for [n (range 0 9)]
    (set (map #(get % n) board))))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (let [block-coords (map block-coordinates (coord-pairs [0 3 6]))
        values       (map #(map (partial value-at board) %) block-coords)]
    (map set values)))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [empty-idxs (map-indexed vector (map (fn [row] (keep-indexed #(if (= 0 %2) %1) row)) board))
        some-empty (filter #(not (empty? (second %))) empty-idxs)]
    (if-not (empty? some-empty)
      (let [first-row (first some-empty)]
        [(first first-row) (first (second first-row))]))))

(defn solve [board]
  (if-let [point (find-empty-point board)]
    (let [valid-values (valid-values-for board point)]
      (for [value    valid-values
            solution (solve (set-value-at board point value))]
        solution))
    (if (valid-solution? board)
      board)))
