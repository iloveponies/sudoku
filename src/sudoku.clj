(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board [row col]]
  (get-in board [row col]))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row _]]
  (set (get board row)))


(defn col-values [board [_ col]]
  (set (map #(get %1 col) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(def all-values (set (range 1 10)))

(defn block-top-left [[row col]]
  (let [highest-multiple-of-three (fn [num] (- num (mod num 3)))]
    [(highest-multiple-of-three row) (highest-multiple-of-three col)]))

(defn block-values [board coord]
  (let [top-left (block-top-left coord)
        movement-vectors (for [added-rows [0 1 2]
                               added-cols [0 1 2]]
                           [added-rows added-cols])
        coord-plus-vector (fn [[coord-row coord-col] [vec-row vec-col]] [(+ coord-row vec-row)
                                                                         (+ coord-col vec-col)])
        block-coords (map #(coord-plus-vector top-left %1) movement-vectors)]
    (set (map #(value-at board %1) block-coords))))

(defn valid-values-for [board coord]
  (let [current-value (value-at board coord)]
    (if (not= 0 current-value)
      #{}
      (let [forbidden-values (set/union
                               (row-values board coord)
                               (col-values board coord)
                               (block-values board coord))]
        (set/difference all-values forbidden-values)))))

(defn filled? [board]
  (let [all-board-numbers (set (apply concat board))]
    ((complement contains?) all-board-numbers 0)))

(defn rows [board]
  (map #(row-values board %1) (map #(vector %1 0) (range 0 9))))

(defn all-and-only-valid-values [val-set]
  (= all-values val-set))

(defn valid-rows? [board]
  (every? all-and-only-valid-values (rows board)))

(defn cols [board]
  (map #(col-values board %1) (map #(vector 0 %1) (range 0 9))))

(defn valid-cols? [board]
  (every? all-and-only-valid-values (cols board)))

(defn blocks [board]
  (let [top-lefts (for [row [0 3 6]
                        col [0 3 6]]
                    (vector row col))]
    (map #(block-values board %1) top-lefts)))

(defn valid-blocks? [board]
  (every? all-and-only-valid-values (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-cords (for [row (range 0 9)
                        col (range 0 9)]
                    (vector row col))
        is-empty? (fn [coord]
                    (let [value-at-coord (value-at board coord)]
                      (if (= 0 value-at-coord)
                        coord
                        false)))]
    (some is-empty? all-cords)))

(defn all-solutions [board]
  (let [empty-coord (find-empty-point board)]
    (if (= nil empty-coord)
      (if (valid-solution? board)
        [board]
        [])
      (let [valid-values (valid-values-for board empty-coord)]
        (for [added-value valid-values
              found-solution (all-solutions (set-value-at board empty-coord added-value))]
          found-solution)))))

(defn solve [board] (first (all-solutions board)))