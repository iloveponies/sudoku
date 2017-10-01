(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

;(def sudoku-board
;  (board [[5 3 0 0 7 0 0 0 0]
;          [6 0 0 1 9 5 0 0 0]
;          [0 9 8 0 0 0 0 6 0]
;          [8 0 0 0 6 0 0 0 3]
;          [4 0 0 8 0 3 0 0 1]
;          [7 0 0 0 2 0 0 0 6]
;          [0 6 0 0 0 0 2 8 0]
;          [0 0 0 4 1 9 0 0 5]
;          [0 0 0 0 8 0 0 7 9]]))
;
;(def solved-board
;  (board [[5 3 4 6 7 8 9 1 2]
;          [6 7 2 1 9 5 3 4 8]
;          [1 9 8 3 4 2 5 6 7]
;          [8 5 9 7 6 1 4 2 3]
;          [4 2 6 8 5 3 7 9 1]
;          [7 1 3 9 2 4 8 5 6]
;          [9 6 1 5 3 7 2 8 4]
;          [2 8 7 4 1 9 6 3 5]
;          [3 4 5 2 8 6 1 7 9]]))
;
;(def invalid-board
;  (board [[5 3 4 6 7 8 9 1 1]
;          [6 7 2 1 9 5 3 4 8]
;          [1 9 8 3 4 2 5 6 7]
;          [8 5 9 7 6 1 4 2 3]
;          [4 2 6 8 5 3 7 9 1]
;          [7 1 3 9 2 4 8 5 6]
;          [9 6 1 5 3 7 2 8 4]
;          [2 8 7 4 1 9 6 3 5]
;          [3 4 5 2 8 6 1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
     (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (for [row board]
           (get row col)))))

(defn coord-pairs [coords]
  (vec (for [row coords
             col coords]
         [row col])))

(defn block-values [board coord]
  (let [square-coords (coord-pairs [0 1 2])
        block-corner (mapv #(- % (mod % 3)) coord)]
    (set (map #(value-at board (map + block-corner %)) square-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (every? #(every? pos? %) board))

(defn valid-set? [a-set]
  (= all-values a-set))

(defn rows [board]
  (mapv #(row-values board [% 0]) (range 9)))

(defn valid-rows? [board]
  (every? valid-set? (rows board)))

(defn cols [board]
  (mapv #(col-values board [0 %]) (range 9)))

(defn valid-cols? [board]
  (every? valid-set? (cols board)))

(defn blocks [board]
  (map #(block-values board %) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? valid-set? (blocks board)))

(defn valid-solution? [board]
  (every? identity ((juxt valid-cols? valid-rows? valid-blocks?) board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [row (first (keep-indexed #(when (some zero? %2) %1) board))
        col (first (keep-indexed #(when (zero? %2) %1) (get board row)))]
    [row col]))

;(defn solver-helper)

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [gap (find-empty-point board)]
      (for [guess (valid-values-for board gap)
            solution (solve (set-value-at board gap guess))]
        solution))))



