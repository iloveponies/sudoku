(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (for [row board] (get row col)))))

(defn coord-pairs [coords]
  (vec (for [x coords y coords] [x y])))

(defn- top-left-corner [[row col]]
  [(* 3 (int (/ row 3))) (* 3 (int (/ col 3)))])

(defn block-values [board coord]
  (let [[r c] (top-left-corner coord)]
    (set (for [row-offset (range 3) col-offset (range 3)]
           (get-in board [(+ r row-offset) (+ c col-offset)])))))

(defn valid-values-for [board coord]
  (if (not (= 0 (value-at board coord)))
    #{}
    (set/difference
      all-values
      (set/union (row-values board coord)
                 (col-values board coord)
                 (block-values board coord)))))

(defn board-values [board]
  (set (for [row board elem row] elem)))

(defn filled? [board]
  (not (contains? (board-values board) 0)))

(defn rows [board]
  (vec (for [row (range 9)] (row-values board [row 0]))))

(defn- valid-component? [f board]
  (every? #(= all-values %) (f board)))

(defn valid-rows? [board]
  (valid-component? rows board))

(defn cols [board]
  (vec (for [col (range 9)] (col-values board [0 col]))))

(defn valid-cols? [board]
  (valid-component? cols board))

(defn blocks [board]
  (vec (for [row '(0 3 6) col '(0 3 6)] (block-values board [row col]))))

(defn valid-blocks? [board]
  (valid-component? blocks board))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (for [r (range 9) c (range 9) :when (= 0 (value-at board [r c]))] [r c])))

(defn- solve-helper [board]
  (let [empty-point (find-empty-point board)]
    (if (not empty-point)
      (if (valid-solution? board) [board] [])
      (let [candidate-values (valid-values-for board empty-point)]
        (for [v candidate-values
              solution (solve-helper (set-value-at board empty-point v))]
          solution)))))

(defn solve [board]
  (first (solve-helper board)))
