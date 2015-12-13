(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map (fn [x] (get x (second coord))) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [corner-pos
        ((fn [c] (vector (- (first c) (mod (first c) 3)) (- (second c) (mod (second c) 3)))) coord)]
    (set (for [row (list (first corner-pos) (inc (first corner-pos)) (+ 2 (first corner-pos)))
          col (list (second corner-pos) (inc (second corner-pos)) (+ 2 (second corner-pos)))]
       (value-at board [row col])))
    ))  

(defn valid-values-for [board coord]
  ( if (has-value? board coord)
   #{}
   (set/difference all-values (set/union (row-values board coord) (col-values board coord ) (block-values board coord)))
   ))

(defn filled? [board]
  (let [all-nums (reduce concat board)]
    (not (contains? (set all-nums) 0))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (not (contains? (set (map (fn [x] (= all-values x)) (rows board))) false)))

(defn cols [board]
  (map (fn [col] (set (map (fn [row] (get row col)) board))) (range 0 9)))

(defn valid-cols? [board]
    (not (contains? (set (map (fn [x] (= all-values x)) (cols board))) false)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))
   
(defn valid-blocks? [board]
    (not (contains? (set (map (fn [x] (= all-values x)) (blocks board))) false)))

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
