(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
    [a b]))

(defn block-coord [[row col]]
  [(* 3 (int (/ row 3)))
   (* 3 (int (/ col 3)))])

(defn block-values [board coord]
  (let [[bx by] (block-coord coord)
        trans (fn [[x y]] [(+ bx x) (+ by y)])
        coords (map trans (coord-pairs [0 1 2]))]
    (set (map #(value-at board %) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [row (row-values board coord)
          col (col-values board coord)
          block (block-values board coord)]
      (set/difference all-values (set/union row col block)))))

(defn filled? [board]
  (let [all-coords (coord-pairs (range 9))]
    (every? (partial has-value? board) all-coords)))

(defn valid-x [board x]
  (every? empty? (map (partial set/difference all-values) (x board))))

(defn rows [board]
  (map (partial row-values board) (map vector (range 9) (repeat 0))))

(defn valid-rows? [board]
  (valid-x board rows))

(defn cols [board]
  (map (partial col-values board) (map vector (repeat 0) (range 9))))

(defn valid-cols? [board]
  (valid-x board cols))

(defn blocks [board]
  (map (partial block-values board) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (valid-x board blocks))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-rows? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coords (coord-pairs (range 9))]
    (if (not (has-value? board (first coords)))
      (first coords)
      (recur (rest coords)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [e (find-empty-point board)]
      (for [value (valid-values-for board e)
            solution (solve (set-value-at board e value))]
        solution))))
