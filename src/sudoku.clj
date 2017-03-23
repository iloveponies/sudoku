(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (reduce (fn [s x] (conj s (get x (get coord 1)))) #{} board))

(defn coord-pairs
   ([coords] (for [x coords y coords] [x y]))
   ([coordx coordy] (for [x coordx y coordy] [x y])))

(defn topleft-corner [coord]
  [(* (int (/ (get coord 0) 3)) 3) (* (int (/ (get coord 1) 3)) 3)])

(defn block-values [board coord]
  (let [[s t] [(get (topleft-corner coord) 0) (get (topleft-corner coord) 1)]]
    (reduce (fn [x y] (conj x (value-at board y))) #{}
            (coord-pairs (range s (+ s 3)) (range t (+ t 3))))))

(defn valid-values-for [board coord]
  (if (not (= (value-at board coord) 0))
    #{}
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn numbers-on-board [board]
  (set (reduce set/union #{} board)))

(defn filled? [board]
  (not (contains? (numbers-on-board board) 0)))

(defn rows [board]
  (reduce (fn [s x] (conj s (set x))) [] board))

(defn valid-rows? [board]
  (empty? (filter (fn [x] (not (= x all-values))) (rows board))))

(defn cols [board]
  (reduce (fn [s x] (conj s (col-values board [0 x]))) [] (range 9)))

(defn valid-cols? [board]
  (empty? (filter (fn [x] (not (= x all-values))) (cols board))))

(defn blocks [board]
  (let [corners (coord-pairs [0 3 6])]
    (reduce (fn [s x] (conj s (block-values board x))) [] corners)))

(defn valid-blocks? [board]
  (empty? (filter (fn [x] (not (= x all-values))) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [x] (= 0 (value-at board x))) (coord-pairs (range 9)))))

(defn solve-helper [current-board]
  (let [remaining (find-empty-point current-board)]
    (if (nil? remaining)
      (if (valid-solution? current-board)
        [current-board] [])
      (for [value (valid-values-for current-board remaining)
            solution (solve-helper (set-value-at current-board remaining value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
