(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row] (get row col)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left [coord]
  (let [mapper (fn [x] (* 3 (int (/ x 3))))]
    (map mapper coord)))

(defn block-coords [coord]
  (let [[tl-row tl-col] (top-left coord)
        row-range (range tl-row (+ tl-row 3))
        col-range (range tl-col (+ tl-col 3))]
    (for [row row-range
          col col-range]
      [row col])))

(defn block-values [board coord]
  (let [pairs (block-coords coord)
        mapper (fn [coord] (value-at board coord))
        values (map mapper pairs)]
    (set values)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [b (block-values board coord)
          r (row-values board coord)
          c (col-values board coord)]
      (set/difference all-values b r c))))

(defn board->seq [board]
  (reduce concat board))

(defn filled? [board]
  (let [val-set (set (board->seq board))]
    (not (contains? val-set 0))))

(defn rows [board]
  (map set board))

(defn valid-*? [groups board]
  (every? (fn [group] (= group all-values)) (groups board)))

(defn valid-rows? [board]
  (valid-*? rows board))

(defn cols [board]
  (let [singleton-set (fn [el] #{el})
        helper (fn [acc row] (map set/union acc (map singleton-set row)))
        start (map singleton-set (first board))]
    (reduce helper start (rest board))))

(defn valid-cols? [board]
  (valid-*? cols board))

(defn blocks [board]
  (let [top-left (coord-pairs [0 3 6])
        mapper (fn [coord] (block-values board coord))]
    (map mapper top-left)))

(defn valid-blocks? [board]
  (valid-*? blocks board))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [indices (range 0 9)
        pairs (coord-pairs indices)
        empties (filter (fn [pair] (zero? (value-at board pair))) pairs)]
    (first empties)))

(defn solve-helper [current]
  (let [empty (find-empty-point current)]
    (if (nil? empty)
      (if (valid-solution? current)
        [current]
        [])
      (for [value (valid-values-for current empty)
            solution (solve-helper (set-value-at current empty value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
