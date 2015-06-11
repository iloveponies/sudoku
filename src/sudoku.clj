(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (let [[row-index _] coord] (set (get board row-index))))

(defn col-values [board coord]
  (let [[_ col-index] coord] (set (reduce #(cons (get %2 col-index) %1) #{} board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
      (vector row col)))

(defn block-pairs [coords]
  (let [[row-coords col-coords] coords]
    (for [row row-coords
          col col-coords]
        (vector row col))))

(defn block-range [coord]
  (let [normalise (fn [val] (cond
    (<= 0 val 2) 0
    (<= 3 val 5) 3
    :else 6))
    make-range (fn [val] (range val (+ val 3)))
    [x y] coord]
      (for [value coord] (make-range (normalise value)))))

(defn block-values [board coord]
  (let [co-ords (block-pairs (block-range coord))]
      (set (map #(value-at board %1) co-ords))))

(defn all-values-valid [values]
  (empty? (set/difference all-values values)))

(defn valid-values-for [board coord]
  (let [row-vals (row-values board coord)
        col-vals (col-values board coord)
        block-vals (block-values board coord)
        vals (set/union row-vals col-vals block-vals)]
    (if (has-value? board coord)
      #{}
      (set/difference all-values vals))))

(defn filled? [board]
  (not (some zero? (flatten board))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(all-values-valid %1) (rows board)))

(defn cols [board]
  (for [index (range (count board))]
  (col-values board [0 index])))

(defn valid-cols? [board]
  (every? #(all-values-valid %1) (cols board)))

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
        (block-values board [row col])))

(defn valid-blocks? [board]
  (every? #(all-values-valid %1) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(not (has-value? board %1)) (coord-pairs (range (count board))))))

(defn solve-helper [board]
  (if (valid-solution? board)
  board
  (let [empty-cell (find-empty-point board)
        valid-values (valid-values-for board empty-cell)]
  (for [cell-value valid-values
        solution (solve-helper (assoc-in board empty-cell cell-value))]
        solution))))

(defn solve [board]
  (solve-helper board))
