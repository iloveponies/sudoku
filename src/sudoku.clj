(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [r c]]
  (set (get board r)))

(defn col-values [board [r c]]
  (set (map (fn [a-seq] (get a-seq c)) board)))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))

(defn top-left [[r c]]
  [(- r (mod r 3)) (- c (mod c 3))])

(defn block-values [board coord]
  (let [[r c] (top-left coord)
        index-pairs (coord-pairs [0 1 2])
        values (for [[cr cc] index-pairs]
                 (value-at board [(+ r cr) (+ c cc)]))]
    (set values)))

(defn valid-values-for [board coord]
  (if (not= (value-at board coord) 0)
    #{}
    (let [used-values (set/union (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord))]
      (set/difference all-values used-values))))

(defn board-values [board]
  (set (apply concat board)))

(defn filled? [board]
  (not (contains? (board-values board) 0)))

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
