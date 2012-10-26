(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[x _] coord]
        (set (get board x))))

(defn col-values [board coord]
  (let [[_ y] coord]
    (set (map #(get % y) board))))

(defn coord-pairs [coords]
  (vec (apply concat (map (fn [i] (map (fn [j] [i j]) coords)) coords))))

(defn block-values [board coord]
  (let [[x y] coord
        top-l [(* 3 (quot x 3)) (* 3 (quot y 3))]
        x-top-l (first top-l)
        y-top-l (second top-l)]
    (set (map #(value-at board %) (for [x [x-top-l (+ 1 x-top-l) (+ 2 x-top-l)]
                                        p [y-top-l (+ 1 y-top-l) (+ 2 y-top-l)]]
                                    [x p])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) 
    #{}
    (set/intersection (set/difference all-values (row-values board coord))
                      (set/difference all-values (col-values board coord))
                      (set/difference all-values (block-values board coord)))))

(defn filled? [board]
  (boolean (every? #(has-value? board %) (coord-pairs (range 9)))))

(defn rows [board]
  (for [row board]
    (set row)))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (for [i (range 9)]
    (col-values board [0 i])))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (for [i [0 3 6]
        j [0 3 6]]
    (block-values board [i j])))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(not (has-value? board %)) (coord-pairs (range 9)))))

(defn solve [board]
  nil)