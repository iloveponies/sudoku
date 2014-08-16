(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board [row col]]
  (set (board row)))

(defn col-values [board [row col]]
  (let [helper (fn [row]
                 (value-at board [row col]))]
    (set (map helper (range 9)))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-coords [[row col]]
  (let [helper (fn [x]
                 (if (< x 3)
                   0
                   (if (< x 6)
                     3
                     6)))]
    [(helper row) (helper col)]))

(defn block-values [board coord]
  (let [[brow bcol] (block-coords coord)]
    (set
     (for [row (range brow (+ brow 3))
           col (range bcol (+ bcol 3))]
       (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [found-values (set/union (row-values board coord)
                                  (col-values board coord)
                                  (block-values board coord))]
      (set/difference all-values found-values))))

(defn filled? [board]
  (let [every-value (set (for [row (range 9)
                               col (range 9)]
                           (value-at board [row col])))]
    (not (contains? every-value 0))))

(defn valid-sets? [sets]
  (let [testfn (fn [thing] (= thing all-values))
        results (map testfn sets)]
    (not (contains? (set results) false))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (set (col-values board [0 col]))))

(defn valid-cols? [board]
  (valid-sets? (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (set (block-values board [row col]))))

(defn valid-blocks? [board]
  (valid-sets? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [empty-points (for [row (range 9)
                           col (range 9)]
                       (if (has-value? board [row col])
                         nil
                         [row col]))]
    (first (remove nil? empty-points))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [point (find-empty-point board)
          values (valid-values-for board point)]
      (for [value values
            solution (solve-helper (set-value-at board point value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
