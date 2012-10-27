(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ column] coord]
    (set (map (fn [x] (get x column)) board))))

(defn coord-pairs [coords]
  (for [x coords y coords]
    [x y]))

(defn block-values [board coord]
  (let [[row column] coord
        top-left-corner-x (* (quot row 3) 3)
        top-left-corner-y (* (quot column 3) 3)
        block-range-x (vec (range top-left-corner-x (+ top-left-corner-x 3)))
        block-range-y (vec (range top-left-corner-y (+ top-left-corner-y 3)))]
    (set (for [x block-range-x
               y block-range-y]
           (get-in board [x y])))))

(defn valid-values-for [board coord]
  (let [valid-vals (set (range 1 10))
        row-vals (row-values board coord)
        col-vals (col-values board coord)
        block-vals (block-values board coord)
        restricted-vals (set/union row-vals col-vals block-vals)]
    (if (has-value? board coord)
      #{}
      (set/difference valid-vals restricted-vals))))

(defn filled? [board]
  (not (some zero? (flatten board))))

(defn rows [board]
  (map #(row-values board [% 0]) (range 0 9)))

(defn valid-rows? [board]
  (every? #(set/subset? all-values %) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 0 9)))

(defn valid-cols? [board]
  (every? #(set/subset? all-values %) (cols board)))

(defn blocks [board]
  (map #(block-values board %) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? #(set/subset? all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %)) (coord-pairs (range 0 9)))))

(defn solve-helper [board]
  (if (filled? board)
    board
    (let [coord (find-empty-point board)]
      (flatten (map #(solve-helper (set-value-at board coord %))
                    (valid-values-for board coord))))))

(defn solve [board]
  (partition 9 (solve-helper board)))
