(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board coord]
(let [[row col] coord]
     (set (for [x (range 9)] (value-at board [row x])))))

(defn col-values [board coord]
(let [[row col] coord]
     (set (for [x (range 9)] (value-at board [x col])))))

(defn coord-pairs [coords]
      (mapcat identity (for [x coords] (for [y coords] [x y]))))

(defn step-3 [x]
      (* 3 (int (/ x 3))))

(defn top-left [coord]
      (let [[row col] coord]
      	   [(step-3 row) (step-3 col)]))

(defn block-values [board coord]
      (let [[row col] (top-left coord)]
      (set (map (fn [i] (let [[a b] i] (value-at board [(+ row a) (+ col b)]))) (coord-pairs [0 1 2])))))

(defn valid-values-for [board coord]
      (if (has-value? board coord) #{}
      (set/difference (set/difference (set/difference all-values (block-values board coord))
      (row-values board coord)) (col-values board coord))))

(defn filled? [board]
      (not (contains? (set (flatten board)) 0)))

(defn rows [board]
      (for [x (range 9)] (row-values board [x 0])))

(defn valid-rows? [board]
      (every? identity (map (fn [n] (= n all-values)) (rows board))))

(defn cols [board]
      (for [x (range 9)] (col-values board [0 x])))

(defn valid-cols? [board]
      (every? identity (map (fn [n] (= n all-values)) (cols board))))

(defn blocks [board]
      (mapcat identity (for [x (range 3)] (for [y (range 3)] (block-values board [(* x 3) (* y 3)])))))

(defn valid-blocks? [board]
      (every? identity (map (fn [n] (= n all-values)) (blocks board))))

(defn valid-solution? [board]
      (every? identity [(valid-blocks? board) (valid-rows? board) (valid-cols? board)]))

(defn set-value-at [board coord new-value]
      (assoc-in board coord new-value))

(defn find-empty-point [board]
      (first (filter identity (mapcat identity (for [x (range 9)] (for [y (range 9)]
      	   (if (not (has-value? board [x y])) [x y])))))))

(defn solve [board]
      (cond
	(valid-solution? board) board
	(filled? board) nil
	:else (let [coord (find-empty-point board)]
	      (mapcat identity
	      (for [x (valid-values-for board coord)] (solve (set-value-at board coord x)))))))
