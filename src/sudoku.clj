(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values-helper [col board]
  (reduce (fn [acc x]
            (conj acc (value-at board [x col]))) '() (range 0 (count board))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (col-values-helper col board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn coord-triples [row]
  (if (empty? row)
    '()
    (cons (take 3 row) (coord-triples (nthrest row 3)))))

; for two functions below there probably exists much clearer
; way to divide into three blocks using e.g. mod
(defn block-values-col [board coord]
  (let [[row col] coord]
    (cond
      (<= 0 col 2) (first (coord-triples (board row)))
      (<= 3 col 5) (second (coord-triples (board row)))
      :else (nth (coord-triples (board row)) 2))))

(defn block-values-helper [board coord]
  (let [[row col] coord]
    (cond
      (<= 0 row 2) (concat (block-values-col board [0 col]) (block-values-col board [1 col]) (block-values-col board [2 col]))
      (<= 3 row 5) (concat (block-values-col board [3 col]) (block-values-col board [4 col]) (block-values-col board [5 col]))
      :else (concat (block-values-col board [6 col]) (block-values-col board [7 col]) (block-values-col board [8 col])))))

(defn block-values [board coord]
  (set (block-values-helper board coord)))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (cond
    (has-value? board coord) '#{}
    :else (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))))

(defn filled-helper [board row col]
  (cond
    (> 9 (count (row-values board [row col]))) false
    (= row 8) (= 9 (count (row-values board [row col])))
    :else (filled-helper board (+ 1 row) col)))

(defn filled? [board]
  (filled-helper board 0 0))

(defn rows [board]
  (map (fn [x] (row-values board [x 0])) (range 9)))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 9)))

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  (map (fn [x] (block-values board x)) (coord-pairs (range 1 9 3))))

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
