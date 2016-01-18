(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[x _] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[_ y] coord
        val-collector (fn [values a-row]
                        (conj values (get a-row y)))]
    (reduce val-collector #{} board)))

(defn coord-pairs [coords]
  (for [row coords
        coll coords]
    (vector row coll)))

(defn left-block-coord [coord]
  (for [co coord]
    (cond
     (< co 3) 0
     (< co 6) 3
     :else 6)))

(defn my-coord-pairs [row-seq col-seq]
  (for [row row-seq
        col col-seq]
    (vector row col)))

(defn block-values [board coord]
  (let [[row col] (left-block-coord coord)
        block (my-coord-pairs [row (inc row) (+ row 2)] [col (inc col) (+ col 2)])]
    (set (for [co block]
      (value-at board co)))))

(defn valid-values-for [board coord]
  (let [used-values (set/union (row-values board coord) (col-values board coord) (block-values board coord))]
    (if (has-value? board coord)
      #{}
      (set/difference all-values used-values))))

(defn filled? [board]
  (let [board-numbers (set (apply concat (map seq board)))]
    (not (contains? board-numbers 0))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [row] (= row all-values)) (rows board)))

(defn cols [board]
  (for [column (range 0 9)]
    (set (col-values board [0 column]))))

(defn valid-cols? [board]
  (every? (fn [col] (= col all-values)) (cols board)))

(defn blocks [board]
  (let [block-coords (coord-pairs [0 3 6])]
    (for [coord block-coords]
      (set (block-values board coord)))))

(defn valid-blocks? [board]
  (every? (fn [block] (= block all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0]
      (if (contains? (set (get board row)) 0)
        (loop [col 0]
          (if (= 0 (get (get board row) col))
            [row col]
            (recur (inc col))))
        (recur (inc row)))))


(defn solve-helper [current-board]
  (if (filled? current-board)
    (if (valid-solution? current-board)
      [current-board]
      ())
    (let [empty-loc (find-empty-point current-board)
          remaining-values (valid-values-for current-board empty-loc)]
      (for [value remaining-values
            solution (solve-helper (set-value-at current-board empty-loc value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
