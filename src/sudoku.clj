(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (reduce #(conj %1 (get %2 col)) #{} board))

(defn coord-pairs [coords]
  (apply concat
    (for [y coords]
      (for [x coords]
        [y x]))))

(defn block-coord [[row col]]
  (let [helper
        (fn [n]
          (cond
            (< n 3) 0
            (< n 6) 3
            :else 6))]
    [(helper row) (helper col)]))

(defn block-values [board coord]
  (let [[row col] (block-coord coord)]
    (apply set/union
      (for [y (range row (+ row 3))]
        (set
          (for [x (range col (+ col 3))]
            (value-at board [y x])))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (row-values board coord)
      (col-values board coord)
      (block-values board coord))))

(defn filled? [board]
  (let [numbers
        (set (reduce #(cons %1 %2) board))]
    (not (contains? numbers 0))))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (reduce
    #(and %1 (= all-values %2))
    true
    (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (reduce
    #(and %1 (= all-values %2))
    true
    (cols board)))

(defn blocks [board]
  (for [coord (coord-pairs [0 3 6])]
    (block-values board coord)))

(defn valid-blocks? [board]
  (reduce
    #(and %1 (= all-values %2))
    true
    (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point-helper [board coords]
  (if (empty? coords)
    nil
    (if (not (has-value? board (first coords)))
      (first coords)
      (find-empty-point-helper board (rest coords)))))

(defn find-empty-point [board]
  (let [coords
        (apply concat
          (for [y (range 0 9)]
            (for [x (range 0 9)]
              [y x])))]
    (find-empty-point-helper board coords)))

(defn solve-helper [current]
  (if (filled? current)
    (if (valid-solution? current)
      [current]
      [])
    (let [empty-coord (find-empty-point current)]
      (for [value (valid-values-for current empty-coord)
            solution (solve-helper
                       (set-value-at current empty-coord value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
