(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (< 0 (value-at board coord)))

(defn row-values [board [x y]]
  (set (get board x)))

(defn col-values [board [x y]]
  (set (map (fn [v] (nth v y)) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vector x y)))

(defn less3 [n]
  ( - n (mod n 3)))

(defn block-values [board [x y]]
  (let [bx (less3 x)
        by (less3 y)
        coords (for [x (range bx (+ 3 bx))
                     y (range by (+ 3 by))]
                 (vector x y))]
    (set (map (fn [el] (value-at board el)) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
  (set/difference all-values
                  (set/union (block-values board coord)
                             (row-values board coord)
                             (col-values board coord)))))

(defn rows [board]
  (map set board))

(defn filled? [board]
  (every? true?
          (map (fn [s] (not (contains? s 0))) (rows board))))

(defn valid? [sets]
  (every? true?
          (map (fn [r] (= all-values r)) sets)))

(defn valid-rows? [board]
  (valid? (rows board)))

(defn cols [board]
  (for [i (range 0 9)]
    (col-values board [0 i])))

(defn valid-cols? [board]
  (valid? (cols board)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (valid? (blocks board)))

(defn valid-solution? [board]
  (every? true? [(valid-blocks? board)
                 (valid-rows? board)
                 (valid-cols? board)]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coords] (= 0 (value-at board coords)))
    (for [x (range 0 9)
          y (range 0 9)]
         (vector x y)))))

(defn solve [board]
  (let [zeroes (find-empty-point board)]
    (if (empty? zeroes)
      (if (valid-solution? board)
        board
        [])
      (let [possible (valid-values-for board zeroes)]
        (for [p possible
              solution (solve (set-value-at board zeroes p))]
          solution)))))
