(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map (fn [coll] (nth coll col)) board)))

(defn xy-coords-pairs [x-coords y-coords]
  (for [x x-coords
        y y-coords]
    (vector x y)))

(defn coord-pairs [coords]
  (xy-coords-pairs coords coords))

(defn block-indeces [x]
  (let [st (* (int (/ x 3)) 3)]
     (range st (+ st 3))))

(defn block-coords [[row col]]
  (xy-coords-pairs (block-indeces row) (block-indeces col)))

(defn block-values [board coord]
  (set (map (fn [x] (value-at board x)) (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (row-values board coord) (col-values board coord) (block-values board coord))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn row-value-set [board n]
  (set (row-values board [n 0])))

(defn col-value-set [board n]
  (set (col-values board [0 n])))

(defn nth-block-coords [n]
  [(* (int (/ n 3)) 3) (* (mod n 3) 3)])

(defn block-value-set [board n]
  (block-values board (nth-block-coords n)))

(defn rows [board]
  (map (fn [n] (row-value-set board n)) (range 0 9)))

(defn cols [board]
  (map (fn [n] (col-value-set board n)) (range 0 9)))

(defn blocks [board]
  (map (fn [n] (block-value-set board n)) (range 0 9)))

(defn valid-values [a-set]
  (= a-set all-values))

(defn valid-sets [sets]
  (every? identity (map valid-values sets)))

(defn valid-rows? [board]
  (valid-sets (rows board)))

(defn valid-cols? [board]
  (valid-sets (cols board)))

(defn valid-blocks? [board]
  (valid-sets (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-index [row]
  (first (keep-indexed (fn [ind val] (if (= 0 val) ind)) row)))

(defn find-empty-point [board]
  (let [row-inds (map find-empty-index board)
        row (first (keep-indexed (fn [ind val] (if (not (nil? val)) ind)) row-inds))] 
    (if (nil? row)
      nil
      [row (nth row-inds row)])))

(defn solve-helper [board coord]
  (if (nil? coord)
    (if (valid-solution? board)
      [board]
      [])
    (let [possible-vals (valid-values-for board coord)]
      (for [val possible-vals
            solution (let [new-board (set-value-at board coord val)
                           new-coord (find-empty-point new-board)]
                       (solve-helper new-board new-coord))]
        solution))))

(defn solve [board]
  (first (solve-helper board (find-empty-point board))))
