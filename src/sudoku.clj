(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [y x]]
  (set (board y)))

(defn col-values [board [y x]]
  (set (map #(get % x) board)))

(defn coord-pairs [coords]
  (for [a coords b coords] [a b]))

(defn top-left-corner [[y x]]
  [(- y (mod y 3)) (- x (mod x 3))])

(defn add-coords [[a b] [c d]] [(+ a c) (+ b d)])

(defn block-values-at [board top-left]
  (let [pairs        (coord-pairs [0 1 2])
        block-coords (map #(add-coords top-left %) pairs)]
    (set (map #(value-at board %) block-coords))))

(defn block-values [board coord]
  (block-values-at board (top-left-corner coord)))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (= (value-at board coord) 0)
    (let [row-vals (row-values board coord)
          col-vals (col-values board coord)
          block-vals (block-values board coord)]
      (set/difference all-values row-vals col-vals block-vals))
    #{}))

(defn seq-contains? [s e]
  (some #{e} s))

(defn filled? [board]
  (not (some #(seq-contains? % 0) board)))

(defn rows [board]
  (map #(set %) board))

(defn cols [board]
  (for [x (range 9)]
    (col-values board [0 x])))

(defn blocks [board]
  (map #(block-values-at board %) (coord-pairs [0 3 6])))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn empty-point-coord [board coord]
  (if-not (has-value? board coord) coord))

(defn find-empty-point [board]
  (some #(empty-point-coord board %) (coord-pairs (range 9))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [empty-point (find-empty-point board)]
      (if empty-point
        (some #(solve (set-value-at board empty-point %)) (valid-values-for board empty-point))))))


