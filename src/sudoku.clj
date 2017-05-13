(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def test-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (reduce
    (fn [acc row] (conj acc (get row (last coord))))
    #{} board))

(defn coord-pairs [coords]
  (for [r coords c coords]
    [r c]))

(defn block-start [coord]
  (let [c (fn [x] (* (int (/ x 3)) 3))]
  [(c (first coord)) (c (last coord))]))

(defn block-coords [left-c]
  (let [[rc cc] left-c]
    (for
    [r (range rc (+ rc 3))
    c (range cc (+ cc 3))]
    [r c])))

(defn block-values [board coord]
  (reduce
    (fn [acc c] (conj acc (value-at board c)))
    #{} (block-coords (block-start coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (let [used-values (set/union
                (block-values board coord)
                (row-values board coord)
                (col-values board coord))]
    (set/difference all-values used-values))))

(defn filled? [board]
  (let [row-check
        (fn [acc row]
          (and acc (not (contains? (set row) 0))))]
  (reduce row-check true board)))

(defn valid? [vals]
  (empty? (set/difference all-values vals)))

(defn valid-reducer [vals]
  (reduce
    (fn [acc v] (and acc (valid? v)))
    true vals))

(defn rows [board]
  (for [r board]
    (set r)))

(defn valid-rows? [board]
  (valid-reducer (rows board)))

(defn cols [board]
  (for [c (range 0 9)]
    (col-values board [0 c])))

(defn valid-cols? [board]
  (valid-reducer (cols board)))

(defn blocks [board]
  (for [c (coord-pairs [0 3 6])]
    (block-values board c)))

(defn valid-blocks? [board]
  (valid-reducer (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
