(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

;(def sudoku-board [[5 3 0 0 7 0 0 0 0]
;                   [6 0 0 1 9 5 0 0 0]
;                   [0 9 8 0 0 0 0 6 0]
;                   [8 0 0 0 6 0 0 0 3]
;                   [4 0 0 8 0 3 0 0 1]
;                   [7 0 0 0 2 0 0 0 6]
;                   [0 6 0 0 0 0 2 8 0]
;                   [0 0 0 4 1 9 0 0 5]
;                   [0 0 0 0 8 0 0 7 9]])
;(def solved-board
;                     [[5 3 4 6 7 8 9 1 2]
;                      [6 7 2 1 9 5 3 4 8]
;                      [1 9 8 3 4 2 5 6 7]
;                      [8 5 9 7 6 1 4 2 3]
;                      [4 2 6 8 5 3 7 9 1]
;                      [7 1 3 9 2 4 8 5 6]
;                      [9 6 1 5 3 7 2 8 4]
;                      [2 8 7 4 1 9 6 3 5]
;                      [3 4 5 2 8 6 1 7 9]])
;(def invalid-board
;    (board [[5 3 4 6 7 8 9 1 1]
;                      [6 7 2 1 9 5 3 4 8]
;                      [1 9 8 3 4 2 5 6 7]
;                      [8 5 9 7 6 1 4 2 3]
;                      [4 2 6 8 5 3 7 9 1]
;                      [7 1 3 9 2 4 8 5 6]
;                      [9 6 1 5 3 7 2 8 4]
;                      [2 8 7 4 1 9 6 3 5]
;                      [3 4 5 2 8 6 1 7 9]]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (set (map (fn [row] (get row (get coord 1))) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [corner-start (map (fn [n] (* 3 (int (/ n 3)))) coord)
        length (range 3)]
    (set (for [x length
               y length]
           (value-at board [(+ x (first corner-start)) (+ y (last corner-start))])))))

(defn valid-values-for [board coord]
  (let [valid-values (fn [f] (f board coord))]
    (if (has-value? board coord)
      #{}
      (set/difference (clojure.core/set (range 10)) (apply set/union (map valid-values [block-values col-values row-values]))))))

(defn filled? [board]
  (not (contains? (clojure.core/set (apply set/union board)) 0)))

(defn rows [board]
  (map clojure.core/set board))

(defn valid-abstracted [board f]
  (= 0 (count (filter (fn [row] (not (= (sort row) (range 1 10)))) (f board)))))

(defn valid-rows? [board]
  (valid-abstracted board rows))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 9)))

(defn valid-cols? [board]
  (valid-abstracted board cols))

(defn blocks [board]
  (let [length (range 3)
        to-corner-coords (fn [x] (* 3 x))]
  (for [x length 
        y length] 
    (block-values board [(to-corner-coords x) (to-corner-coords y)]))))

(defn valid-blocks? [board]
  (valid-abstracted board blocks))

(defn valid-solution? [board]
  (reduce (fn [acc f] (and acc (f board))) [valid-rows? valid-cols? valid-blocks?])) 

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0]
    (let [row (get board x)]
      (if (contains? (clojure.core/set row) 0)
        [x (.indexOf row 0)]
        (recur (inc x))))))

(defn solve [b]
    (if (filled? b)
      (if (valid-solution? b)
        b
        '())
      (let [empty-point (find-empty-point b)
            valid-vals (valid-values-for b empty-point)]
        (first (filter (fn [x] (not (empty? x)))
          (for [valid-val valid-vals]
          (solve (set-value-at b empty-point valid-val))))))))
