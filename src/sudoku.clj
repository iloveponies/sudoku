(ns sudoku
  (:require [clojure.set :as set])
  (:require [clojure.tools.trace :as trace]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [row] (get row col)) board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left-corner [coord]
  (let [[row col] coord]
    [(- row (mod row 3)) (- col (mod col 3))]))

(defn block-values [board coord]
  (let [[top-left-row top-left-col] (top-left-corner coord)
        index-pairs (coord-pairs [0 1 2])
        values (for [[index-row index-col] index-pairs]
                 (value-at board [(+ top-left-row index-row) (+ top-left-col index-col)]))]
    (set values)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [r-vals (row-values board coord)
          c-vals (col-values board coord)
          b-vals (block-values board coord)]
      (set/difference (set (range 1 10)) (set/union r-vals c-vals b-vals)))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (apply vector (map set board)))

(defn valid-rows? [board]
  (let [solved-rows (repeat 9 #{1 2 3 4 5 6 7 8 9})]
    (= (rows board) solved-rows)))

(defn cols [board]
  (for [col (range 0 9)]
    (set (col-values board [0 col]))))

(defn valid-cols? [board]
  (let [solved-cols (repeat 9 #{1 2 3 4 5 6 7 8 9})]
    (= (cols board) solved-cols)))

(defn blocks [board]
  (map (fn [x] (block-values board x)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (let [solved-blocks (repeat 9 #{1 2 3 4 5 6 7 8 9})]
    (= (blocks board) solved-blocks)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove (fn [coord] (has-value? board coord)) (coord-pairs (range 9)))))

(defn ^:dynamic solve-helper [board]
  (if (valid-solution? board)
    [board]
    (let [coord (find-empty-point board)]
      (for [value (valid-values-for board coord)
            solution (solve-helper (set-value-at board coord value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
