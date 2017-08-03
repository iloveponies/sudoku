(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def all-indexes #{0 1 2 3 4 5 6 7 8})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (map #(value-at board [row %])
              all-indexes))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map #(value-at board [% col])
              all-indexes))))

(defn coord-pairs [coord-sequence]
  (for [row coord-sequence
        col coord-sequence]
    [row col]))

(defn block-values [board coord]
  (let [[row col] coord
        reducer (fn [x] (if (== 0 (mod x 3))
                          x
                          (recur (dec x))))
        [top left] [(reducer row) (reducer col)]]
    (set (for [r [0 1 2]
               c [0 1 2]]
           (value-at board 
                     [(+ top r) (+ left c)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (every? (partial has-value? board) 
          (for [row all-indexes
                col all-indexes]
            [row col])))

(defn rows [board]
  (map (partial row-values board)
       (for [row all-indexes]
         [row 0])))

(defn valid-rows? [board]
  (every? (partial = all-values)
          (rows board)))

(defn cols [board]
  (map (partial col-values board)
       (for [col all-indexes]
         [0 col])))

(defn valid-cols? [board]
  (every? (partial = all-values)
          (cols board)))

(defn blocks [board]
  (map (partial block-values board)
       (for [row [0 3 6]
             col [0 3 6]]
         [row col])))

(defn valid-blocks? [board]
  (every? (partial = all-values)
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-coords (coord-pairs all-indexes)]
    (first (drop-while (partial has-value? board)
                       all-coords))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [coord (find-empty-point board)]
      (for [value (valid-values-for board coord) 
            solution (solve-helper (set-value-at board 
                                                 coord 
                                                 value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))

