(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

;(def sudoku-board
;  (board [[5 3 0 0 7 0 0 0 0]
;          [6 0 0 1 9 5 0 0 0]
;          [0 9 8 0 0 0 0 6 0]
;          [8 0 0 0 6 0 0 0 3]
;          [4 0 0 8 0 3 0 0 1]
;          [7 0 0 0 2 0 0 0 6]
;          [0 6 0 0 0 0 2 8 0]
;          [0 0 0 4 1 9 0 0 5]
;          [0 0 0 0 8 0 0 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord)) false true))

(defn row-values [board coord]
  (let [[x y] coord
        row (get board x)]
    (set row)))

(defn col-values [board coord]
  (let [[x y] coord]
    (loop [col []
           x_indx 0
           n 9]
      (cond
        (zero? n) (set col)
        :else (recur (conj col (get-in board [x_indx y])) (inc x_indx) (dec n))))))

(defn coord-pairs [coords]
  (let [coord-pair-vector []]
    (apply vector (for [row coords
                        col coords]
                     (vector row col)))))

(def top-left-corners (coord-pairs [0 3 6]))

(defn top-left-corner [coord]
  (let [[x y] coord]
    (get top-left-corners (+ (* 3 (int (/ x 3))) (int (/ y 3))))))


(defn block-values [board coord]
  (let [[x y] coord
        [a b] (top-left-corner coord)
        permutations [[a b] [a (+ 1 b)] [a (+ 2 b)]
                      [(+ 1 a) b] [(+ 1 a) (+ 1 b)] [(+ 1 a) (+ 2 b)]
                      [(+ 2 a) b] [(+ 2 a) (+ 1 b)] [(+ 2 a) (+ 2 b)]]]
    (set (map (fn [z] (get-in board z)) permutations))))

(def all-coords (coord-pairs [0 1 2 3 4 5 6 7 8]))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values (set/union 
                                  (block-values board coord) 
                                  (row-values board coord) 
                                  (col-values board coord)))))

(defn filled? [board]
  (if (contains? (set (map (fn [z] (get-in board z)) all-coords)) 0) false true))

(defn rows [board]
  (loop [all-rows []
         y_indx 0
         n 9]
    (cond
      (zero? n) all-rows
      :else (recur (conj all-rows (row-values board [y_indx 0])) (inc y_indx) (dec n)))))

(defn valid-rows? [board]
  (if (empty? (filter (fn [row] (if (empty? (set/difference all-values row)) false true)) (rows board))) true false))

(defn cols [board]
  (loop [all-cols []
         x_indx 0
         n 9]
    (cond
      (zero? n) all-cols
      :else (recur (conj all-cols (col-values board [0 x_indx])) (inc x_indx) (dec n)))))

(defn valid-cols? [board]
  (if (empty? (filter (fn [col] (if (empty? (set/difference all-values col)) false true)) (cols board))) true false))

(defn blocks [board]
  (map (fn[z] (block-values board z)) top-left-corners))

(defn valid-blocks? [board]
    (if (empty? (filter (fn [block] (if (empty? (set/difference all-values block)) false true)) (blocks board))) true false))

(defn valid-solution? [board]
  (if (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)) true false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [z] (if (zero? (value-at board z)) true false)) all-coords)))

(defn sudoku-solve-helper [start-board]
  (if (filled? start-board)
    (if (valid-solution? start-board) start-board [])
    (let [empty-point (find-empty-point start-board)]
      (for [poss-values (valid-values-for start-board empty-point)
            solution (sudoku-solve-helper (set-value-at start-board empty-point poss-values))]
        solution))))
;        (sudoku-solve-helper (set-value-at start-board empty-point poss-values))))))
    
(defn solve [board]
  (sudoku-solve-helper board))
