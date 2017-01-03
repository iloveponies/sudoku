(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (let [[y x] coord]
    (get-in board [y x])))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord)) false true))


(defn row-values [board coord]
  (let [[y x] coord
        row (get board y)]
    (set row)))

(defn col-values [board coord]
  (let [[y x ] coord
        sarakkeen-arvo (fn [rivi] (get rivi x))
        sarakkeen-arvot (map sarakkeen-arvo board)]
    (set sarakkeen-arvot)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))


;helper to find the top-left corner of the square of the coordinate
(defn top-left-corner [coord]
  (let [[y x] coord
        corner-x (* 3 (int (/ x 3)))
        corner-y (* 3 (int (/ y 3)))]
    [corner-y corner-x]))



(defn block-values [board coord]
  (let [[y x] (top-left-corner coord)
        y-values [y (+ 1 y) (+ 2 y)]
        x-values [x (+ 1 x) (+ 2 x)]
        values-in-block (for [row y-values
                              col x-values]
                          (value-at board [row col]))]
    (set values-in-block)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))




;helper function returning all the 81 values as a sequence
(defn values-as-a-seq [board]
  (for [row [0 1 2 3 4 5 6 7 8]
        col [0 1 2 3 4 5 6 7 8]]
    (value-at board [row col])))


(defn filled? [board]
  (not (contains? (set (values-as-a-seq board)) 0)))


(defn rows [board]
  (for [row [0 1 2 3 4 5 6 7 8]]
    (row-values board [row 0])))


;general-helper recursive helper function. Parameter must be a sequence of sets (rows, columns or blocks)
(defn general-helper [a-seq]
  (cond
    (empty? a-seq)
       true
    (not= (first a-seq) all-values)
       false
    :else
      (general-helper (rest a-seq))))

(defn valid-rows? [board]
  (let [row-sequence (rows board)]
    (general-helper row-sequence)))

(defn cols [board]
  (for [col [0 1 2 3 4 5 6 7 8]]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (let [col-sequence (cols board)]
    (general-helper col-sequence)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (let [block-sequence (blocks board)]
    (general-helper block-sequence)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)) )

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

;presumably there is at least one. Otherwise the current implementation will result in error.
(defn empty-point-helper [board row col]
    (if (has-value? board [row col])
      (if (= col 8)
        (empty-point-helper board (inc row) 0)
        (empty-point-helper board row (inc col)))
      [row col]))

(defn find-empty-point [board]
  (empty-point-helper board 0 0 ))



(defn solver-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      nil)
    (let [[y x] (find-empty-point board)]
      (for [possibility (valid-values-for board [y x])
            solution (solver-helper (set-value-at board [y x] possibility))]
        solution))))

(defn solve [board]
  (first (solver-helper board)))



