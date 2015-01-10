(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def valid-set (set (range 1 10)))

(def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (let [v (value-at board coord)]
    (and (not (nil? v)) (not (= 0 v)))))

(defn row-values [board [r _]]
  (set (get board r)))

(defn col-values [board [_ c]]
  (set (map #(get % c) board)))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))

(defn top-left-coords [board [r c]]
  (let [rm (mod r 3)
        cm (mod c 3)]
    [(- r rm) (- c cm)]))

(defn block-values [board coord]
  (let [[tr tc] (top-left-coords board coord)
        pairs (coord-pairs [0 1 2])
        block-values (map #(let [[r c] %] (value-at board [(+ r tr) (+ c tc)])) pairs)]
    (set block-values)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [r (row-values board coord)
          c (col-values board coord)
          b (block-values board coord)]
      (set/difference all-values r c b))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (for [n (range 0 9)]
    (row-values board [n 0])))

(defn set-is-valid? [s]
  (= valid-set s))

(defn aspect-is-valid? [board f]
  (every? set-is-valid? (f board)))

(defn valid-rows? [board]
  (aspect-is-valid? board rows))

(defn cols [board]
  (for [n (range 0 9)]
    (col-values board [0 n])))

(defn valid-cols? [board]
  (aspect-is-valid? board cols))

(defn blocks [board]
  (for [p (coord-pairs [0 3 6])]
    (block-values board p)))

(defn valid-blocks? [board]
  (aspect-is-valid? board blocks))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (coord-pairs (range 0 9))]
    (first (filter #(not (has-value? board %)) coords))))

(defn solve [board]
  nil)
