(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board [row _]]
  (reduce (fn [acc v] (conj acc v)) #{} (get board row)))

(defn col-values [board [_ col]]
  (reduce (fn [acc v] (conj acc v)) #{} (map (fn [row] (get row col)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [start-row (- row (mod row 3))
        end-row (+ start-row 3)
        start-col (- col (mod col 3))
        end-col (+ start-col 3)]
    (set (for [row (range start-row end-row)
               col (range start-col end-col)]
           (get-in board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
  (loop [rows board]
    (cond
      (empty? rows) true
      (some (fn [v] (== 0 v)) (first rows)) false
      :else (recur (rest rows)))))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn valid-*? [elements]
  (loop [xs elements]
    (cond
      (empty? xs) true
      ((complement empty?) (set/difference all-values (first xs))) false
      :else (recur (rest xs)))))

(defn valid-rows? [board]
  (valid-*? (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (valid-*? (cols board)))

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (valid-*? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (some (fn [coord] (if ((complement has-value?) board coord) coord)) (coord-pairs (range 0 9))))

(defn solve-helper [current]
  (if (valid-solution? current)
    [current]
    (let [next-empty (find-empty-point current)]
      (for [valid-value (valid-values-for current next-empty)
            solution (solve-helper (set-value-at current next-empty valid-value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
