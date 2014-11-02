(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(def all-values (set (range 1 10)))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coords]
   (for [y coords
         x coords]
     [y x]))

(defn- start-coord-of-block-containing [[row col]]
  [(* (quot row 3) 3)
   (* (quot col 3) 3)])

(defn- all-coords-of-block-containing [coord]
  (let [[start-row start-col] (start-coord-of-block-containing coord)]
    (for [row (range start-row (+ start-row 3))
          col (range start-col (+ start-col 3))]
      [row col])))

(defn block-values [board coord]
  (set (map #(value-at board %) (all-coords-of-block-containing coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values 
                    (block-values board coord)
                    (row-values board coord)
                    (col-values board coord))))

(defn- all-board-coords []
  (for [row (range 9)
        col (range 9)]
    [row col]))

(defn filled? [board]
  (every? true? (map #(has-value? board %) (all-board-coords))))

(defn rows [board]
  (for [row-index (range 9)]
    (set (row-values board [row-index 0]))))

(defn valid-rows? [board]
  (every? empty? (map #(set/difference all-values %) (rows board))))

(defn cols [board]
  (for [col-index (range 9)]
    (set (col-values board [0 col-index]))))

(defn valid-cols? [board]
  (every? empty? (map #(set/difference all-values %) (cols board))))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? empty? (map #(set/difference all-values %) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(not (has-value? board %)) (all-board-coords))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [empty-coord (find-empty-point board)
          candidate-values (valid-values-for board empty-coord)]
      (for [value candidate-values
            solution (solve (set-value-at board empty-coord value))]
        solution))))
