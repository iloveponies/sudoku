(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (let [val (get-in board coord 0)]
    (if (zero? val)
      false
      true)))

(defn row-values [board coord]
  (let [[row col] coord
        row-vals (board row)]
    (set row-vals)))

(defn col-values [board coord]
  (let [[row col] coord
        col-vals (for [rw board] (rw col))]
    (set col-vals)))

(defn coord-pairs [coords]
  (for [rw coords cl coords] (vector rw cl)))

(defn block-values [board coord]
  (defn upper-left [coord]
    (let [[row col] coord
          row-adj (rem row 3)
          col-adj (rem col 3)]
       (vector (- row row-adj) (- col col-adj))))
  (let [start-row (first (upper-left coord))
        start-col (second (upper-left coord))
        block-coords (for [row [start-row (+ start-row 1) (+ start-row 2)]
                           col [start-col (inc start-col) (inc (inc start-col))]]
                           (vector row col))]
    (set (for [point block-coords] (value-at board point)))))

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord)) 
    (set/difference all-values (set/union (block-values board coord)
                                          (row-values board coord)
                                          (col-values board coord)))
    #{}))

(defn filled? [board]
  (defn set-of-board-values [board]
    (let [values-seq (for [row (range 9) col (range 9)] (value-at board [row col]))]
      (set values-seq)))
  (if (contains? (set-of-board-values board) 0)
    false
    true))

(defn rows [board]
  (for [row (range 9)]
    (set (for [col (range 9)] (value-at board [row col])))))

(defn valid-rows? [board]
  (every? (fn [x] (= x all-values)) (for [row (rows board)] row)))

(defn cols [board]
  (for [col (range 9)]
    (set (for [row (range 9)] (value-at board [row col])))))

(defn valid-cols? [board]
  (every? (fn [x] (= x all-values)) (for [col (cols board)] col)))

(defn blocks [board]
  (for [row (range 0 9 3) col (range 0 9 3)] (block-values board [row col])))

(defn valid-blocks? [board]
  (every? (fn [x] (= x all-values)) (for [block (blocks board)] block)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) valid-blocks? board))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (for [row (range 9) col (range 9) :when (zero? (value-at board (vector row col)))] (vector row col))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [next-slot (find-empty-point board)
          values (valid-values-for board next-slot)]
      (for [try-value values
            solution (solve-helper (set-value-at board next-slot try-value))
            :when (fn [coll] (not (empty? coll)))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
