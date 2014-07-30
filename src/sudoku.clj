(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (into #{} (get board row))))

(defn col-values [board coord]
  (into #{} (map #(value-at board [% (last coord)]) all-values)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn up-cnr [coord]
  [(* 3 (int (/ (first coord) 3))) (* 3 (int (/ (last coord) 3)))])

(defn block-cords [coord]
  (map (fn [x] (map + (up-cnr coord) x)) (coord-pairs [0 1 2])))

(defn block-values [board coord]
  (into #{} (map #(value-at board %) (block-cords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values (set/union (row-values board coord)
                    (col-values board coord)
                    (block-values board coord)))))

(defn filled? [board]
  nil)

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
