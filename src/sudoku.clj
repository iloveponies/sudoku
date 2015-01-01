(ns sudoku
  ;(:use iloveponies.tests.sudoku)
  (:require [clojure.set :as set]))

(def board identity)

(def sudoku-board-2
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
  (not(zero?(get-in board coord))))

(defn row-values [board coord]
  (let [[x] coord]
    (set (get board x))))

(defn col-values-acc [board y acc]
  (if (empty? board) acc
  (col-values-acc (rest board) y (conj acc (get (first board) y)))))

(defn col-values [board coord]
  (let [[_ y] coord]
    (col-values-acc board y #{})))

(defn coord-pairs [coords]
  (let [v []]
    (for [col coords
          row coords]
      (conj v col row))))

(defn coord-pairs-2 [xs ys]
  (let [v []]
    (for [row xs
          col ys]
      (conj v col row))))

(defn top-left-block-value [n]
  (cond
    (and (>= n 0)(< n 3)) 0
    (and (>= n 3)(< n 6)) 3
    :else 6))

(defn top-left-corner [coords]
  (let [[x y] coords]
    (vector (top-left-block-value x)(top-left-block-value y))))

(defn block-values-helper [board coords acc]
  (if (empty? coords) acc
  (block-values-helper board (rest coords) (conj acc (value-at board (first coords))))))

(defn block-values [board coord]
  (let [[x y] (top-left-corner coord)]
    (block-values-helper board (coord-pairs-2 (range x (+ 3 x)) (range y (+ 3 y))) #{})))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values (row-values board coord)(col-values board coord)(block-values board coord))))

(defn filled? [board]
  (cond
   (empty? board) true
   (contains? (set(first board)) 0) false
   :else (filled? (rest board))))

(defn rows-helper [board acc]
  (if (empty? board) acc
    (rows-helper (rest board) (conj acc (set (first board))))))

(defn rows [board]
  (rows-helper board []))

(defn cols [board]
  (loop [cols [] index 0]
    (if (> index 8) cols
      (recur (conj cols (col-values board [0 index])) (inc index)))))

(defn blocks-helper [board coords acc]
  (if (empty? coords) acc
    (blocks-helper board (rest coords) (conj acc (block-values board (first coords))))))

(defn blocks [board]
  (blocks-helper board (coord-pairs-2 [0 3 6][0 3 6]) []))

(defn valid-elems-helper [elems]
  (cond
   (empty? elems) true
    (not(empty?(set/difference all-values (set(first elems))))) false
    :else (valid-elems-helper (rest elems))))

(defn valid-rows? [board]
  (valid-elems-helper (rows board)))

(defn valid-cols? [board]
  (valid-elems-helper (cols board)))

(defn valid-blocks? [board]
  (valid-elems-helper (blocks board)))

(defn valid-solution? [board]
  (and(valid-rows? board)(valid-cols? board)(valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point-helper [board coords]
  (cond
   (empty? coords) []
   (not(has-value? board (first coords))) (first coords)
   :else (find-empty-point-helper board (rest coords))))

(defn find-empty-point [board]
  (find-empty-point-helper board (coord-pairs-2 (range 0 9)(range 0 9))))

(defn solve [board]
  nil)
