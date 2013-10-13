(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [x] (get x col)) board))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [[x y] coord
        number (fn [a] (* 3 (int (/ a 3))))
        top-corner-row (number x)
        top-corner-col (number y)]
    (set (for [row (range top-corner-row (+ top-corner-row 3))
               col (range top-corner-col (+ top-corner-col 3))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (set/union
                     (block-values board coord)
                     (row-values board coord)
                     (col-values board coord)))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [row]
            (empty? (set/difference all-values row)))
          (rows board)))

(defn cols [board]
  (for [y (range (count board))]
    (col-values board [0 y])))

(defn valid-cols? [board]
  (every? (fn [col]
            (empty? (set/difference all-values col)))
          (cols board)))

(defn blocks [board]
  (let [helper (for [x (range (count board)) :when (zero? (rem x 3))] x)]
    (for [block-row helper
          block-col helper]
      (block-values board [block-row block-col]))))

(defn valid-blocks? [board]
  (every? (fn [block]
            (empty? (set/difference all-values block)))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [empty-point? (fn [coord]
                       (let [[x y] coord] (not (has-value? board [x y]))))]
    (first (filter empty-point? (coord-pairs (range (count board)))))))

(defn solve-helper [board]
  (if (and (filled? board) (valid-solution? board))
    [board]
    (let [empty-point (find-empty-point board)]
      (for [value (valid-values-for board empty-point)
            solution (solve-helper (set-value-at board empty-point value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
