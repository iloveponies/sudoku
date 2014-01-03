(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [[row col] (map #(- % (mod % 3)) coord)]
    (set (for [x (range row (+ row 3))
               y (range col (+ col 3))]
           (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values
                    (row-values   board coord)
                    (col-values   board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (some zero? (apply concat board))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= (sort %) (range 1 10)) (rows board)))

(defn cols [board]
  (rows (apply map vector board)))

(defn valid-cols? [board]
  (valid-rows? (apply map vector board)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (every? #(= (sort %) (range 1 10)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows?   board)
       (valid-cols?   board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(not (has-value? board %))
                 (for [x (range 0 9)
                       y (range 0 9)] [x y]))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board) [board] '())
    (let [coord (find-empty-point board)]
      (for [new-value (valid-values-for board coord)
            solution  (solve-helper (set-value-at board coord new-value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
