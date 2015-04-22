(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))


(defn row-values [board coord]
  (let [[row col] coord]
    (reduce conj #{} (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (reduce conj #{} (map (fn [r] (get r col)) board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-coordinates [coord]
  (let [[x y] coord
        ix (int (- x (mod x 3)))
        iy (int (- y (mod y 3)))]
    [ix iy]))

(defn block-values [board coord]
  (let [[bx by]
        (block-coordinates coord)
        coordinates (for [x (range bx (+ 3 bx))
                          y (range by (+ 3 by))]
                          [x y])
        values(map (fn [c] (value-at board c)) coordinates)]
    (reduce conj #{} values)))

(defn valid-values-for [board coord]
  (let [used (block-values board coord)]
    (if (has-value? board coord) #{}
    (set/difference all-values used))))

(defn filled? [board]
  (let [numbers (set (apply concat board))]
  (not (contains? numbers 0))))

(defn rows [board]
  (map (fn [row] (set row)) board))

(defn cols [board]
  (for [n (range 0 9)]
    (col-values board [0 n])))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-rows? [board]
  (every?
   (fn [r]
     (empty?
      (set/difference all-values r)))
   (rows board)))

(defn valid-cols? [board]
  (every?
   (fn [r]
     (empty?
      (set/difference all-values r)))
   (cols board)))

(defn valid-blocks? [board]
  (every?
   (fn [r]
     (empty?
      (set/difference all-values r)))
   (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(def before-change
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (complement empty?) (concat (for [x (range 0 9)
        y (range 0 9)]
    (if (has-value? board [x y])
      []
      [x y]
  ))))))


(defn solve [board])


