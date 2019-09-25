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
    (set (map get board (repeat col)))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [top-left-coords (fn [[row col]] [(* 3 (int (/ row 3))) (* 3 (int (/ col 3)))])
        [tl-row tl-col] (top-left-coords coord)]
    (set (for [row (range tl-row (+ tl-row 3))
               col (range tl-col (+ tl-col 3))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (let [all-numbers (apply concat board)]
    (not (contains? (set all-numbers) 0))))

(defn rows [board]
  (vec (for [row (range 0 9)]
         (row-values board [row 0]))))

(defn valid-rows? [board]
  (every? (fn [row] (= row all-values)) (rows board)))

(defn cols [board]
  (vec (for [col (range 0 9)]
         (col-values board [0 col]))))

(defn valid-cols? [board]
  (every? (fn [col] (= col all-values)) (cols board)))

(defn blocks [board]
  (vec (for [coord (coord-pairs (range 0 9 3))]
         (block-values board coord))))

(defn valid-blocks? [board]
  (every? (fn [block] (= block all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (complement (fn [coord] (has-value? board coord))) (coord-pairs (range 0 9)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [empty-point (find-empty-point board)]
      (first (for [value (valid-values-for board empty-point)
                   solution (filter (complement nil?) [(solve (set-value-at board empty-point value))])]
               solution)))))
