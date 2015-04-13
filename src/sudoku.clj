(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not(== (value-at board coord) 0)))

(defn row-values [board [row -]]
  (set(get board row)))

(defn col-values [board [- col]]
  (set(map (fn[row] (get row col)) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-coords [[x y]]
  (let [[top-x top-y] [(- x (mod x 3))(- y (mod y 3))]
        x-values [top-x (+ top-x 1) (+ top-x 2)]
        y-values [top-y (+ top-y 1) (+ top-y 2)]]
    (for [a x-values
          b y-values]
      [a b])))

(defn block-values [board coord]
  (set(map (fn[c] (value-at board c)) (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference  all-values
                     (set/union (block-values board coord)
                                (row-values board coord)
                                (col-values board coord)))))

(defn filled? [board]
  (let [rows (for [row [0 1 2 3 4 5 6 7 8]](row-values board [row 0]))]
  (every?  (fn[x](= x false)) (map (fn[x] (contains? x 0)) rows))))

(defn rows [board]
  (for [row [0 1 2 3 4 5 6 7 8]] (row-values board [row 0])))


(defn validator [elem]
  (and (not(contains? elem 0)) (== (count elem) 9)))

(defn valid-rows? [board]
   (every?  (fn[x](= x true)) (map validator (rows board))))

(defn cols [board]
  (for [col [0 1 2 3 4 5 6 7 8]](col-values board [0 col])))

(defn valid-cols? [board]
   (every?  (fn[x](= x true)) (map validator (cols board))))

(defn blocks [board]
  (let [block-coords (for [x [0 3 6]
                           y [0 3 6]] [x y])]
   (for [block block-coords] (block-values board block))))

(defn valid-blocks? [board]
   (every?  (fn[x](= x true)) (map validator (blocks board))))


(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-coords (for [x [0 1 2 3 4 5 6 7 8]
                         y [0 1 2 3 4 5 6 7 8]] [x y])]
    (first(filter (complement (fn[coord] (has-value? board coord))) all-coords))))


(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      ())
    (let [point (find-empty-point board)
          valid-values (valid-values-for board point)]
      (first (filter (complement empty?)
                     (for [value valid-values]
                       (solve (set-value-at board point value))))))))
