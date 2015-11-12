(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord)) false true))

(defn row-values [board coord]
  (set (nth board (first coord))))

(defn col-values [board coord]
  (set (reduce (fn [prev curr] (cons (get curr (second coord)) prev)) #{} board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn block-values [board coord]
  (let [top-left-x (* (int (/ (first coord) 3)) 3)
        top-left-y (* (int (/ (second coord) 3)) 3)] ;FIXME
    (set (for [x [0 1 2]
               y [0 1 2]]
               (value-at board [(+ top-left-x x) (+ top-left-y y)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) 
    #{}
    (let [used-values (set/union
                      (row-values board coord)
                      (col-values board coord)
                      (block-values board coord))]
      (set/difference all-values used-values))))

(defn filled? [board]
 (not (some zero? (flatten board))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(defn blocks [board]
  (for [block (coord-pairs [0 3 6])]
    (block-values board block)))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord-pair] (= 0 (value-at board coord-pair))) (coord-pairs (range 0 9)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [next-empty (find-empty-point board)
      valid-values-for-next-empty (valid-values-for board next-empty)]
      (for [val valid-values-for-next-empty
        sol (solve (set-value-at board next-empty val))]
        sol))))
