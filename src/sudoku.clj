(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [a-board coord]
  (get-in (board a-board) coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [a-board [x y]]
  (set (get-in (board a-board) [x])))

(defn col-values [a-board [x y]]
  (reduce (fn [c-values row] (conj c-values (get-in row [y]))) #{} (board a-board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-top-left [[x y]]
  (let [mod-x (mod x 3)
        mod-y (mod y 3)
        top-left-x (if (= 0 mod-x)
                     x
                     (- x mod-x))
        top-left-y (if (= 0 mod-y)
                     y
                     (- y mod-y))]
    [top-left-x top-left-y]))

(defn block-coords [[x y]]
  (let [[top-left-x top-left-y] (block-top-left [x y])
        x-range (range top-left-x (+ top-left-x 3))
        y-range (range top-left-y (+ top-left-y 3))]
    (for [x x-range
          y y-range]
      [x y])))

(defn block-values [board coord]
  (reduce (fn [b-values b-coords] (conj b-values (value-at board b-coords))) #{} (block-coords coord)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (block-values board coord) (col-values board coord) (row-values board coord))))

(defn all-board-values [a-board]
  (reduce concat '() (board a-board)))

(defn filled? [board]
  (not (contains? (set (all-board-values board)) 0)))

(defn rows [a-board]
  (reduce (fn [rows row] (conj rows (set row))) [] (board a-board)))

(defn valid-rows? [board]
  (reduce (fn [valid row] (and valid (empty? (set/difference all-values row)))) true (rows board)))

(defn cols [a-board]
  (reduce (fn [cols col] (conj cols (col-values a-board [0 col]))) [] (range 9)))

(defn valid-cols? [board]
  (reduce (fn [valid col] (and valid (empty? (set/difference all-values col)))) true (cols board)))

(defn all-blocks []
  (for [x [0 3 6]
        y [0 3 6]]
    [x y]))

(defn all-coords []
  (for [x (range 9)
        y (range 9)]
    [x y]))

(defn blocks [board]
  (reduce (fn [blocks block] (conj blocks (block-values board block))) [] (all-blocks)))

(defn valid-blocks? [board]
  (reduce (fn [valid block] (and valid (empty? (set/difference all-values block)))) true (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [a-board coord new-value]
  (assoc-in (board a-board) coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (not (has-value? board coord))) (all-coords))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [empty-point (find-empty-point board)]
      (for [valid-value (valid-values-for board empty-point)
            solution (solve (set-value-at board empty-point valid-value))]
        solution))))
