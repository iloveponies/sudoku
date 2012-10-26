(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0) false true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (take-nth 9 (drop y (apply concat board))))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-value-helper [coord]
  (let [[x y] coord
        w (* 3 (int (/ x 3)))
        h (* 3 (int (/ y 3)))
        ]
    [w h]))

(defn block-values [board coord]
  (let [[a b] (block-value-helper coord)]
    (set (map (fn [x] (value-at board x))
           (map 
             (fn [z] (let [[x y] z] [(+ x a) (+ y b)])) 
             (coord-pairs [0 1 2]))))))

(defn valid-values-for [board coord]
  (if (> (value-at board coord) 0)
    #{}
    (clojure.set/difference all-values 
      (clojure.set/union 
        (block-values board coord) 
        (col-values board coord)
        (row-values board coord)))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

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
