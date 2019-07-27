(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

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


(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row] (get row col)) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
      (vector x y)))

(defn block-corner-helper [z]
  (* (quot z 3) 3))

(defn block-corner [[x y]]
  (vector (block-corner-helper x)
          (block-corner-helper y)))

(defn block-pairs [[x y]]
  (for [xs (range x (+ x 3))
        ys (range y (+ y 3))]
    (vector xs ys)))

(defn block-values [board coord]
  (set (map (fn [cur-coord] (value-at board cur-coord))
         (block-pairs (block-corner coord)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (map (fn [row] (row-values board [row 0])) (range 9)))

(defn all-full? [sets]
  (= 0 (count (filter (fn [row] (not= 9 (count row))) sets))))

(defn valid-rows? [board]
   (all-full? (rows board)))

(defn cols [board]
  (map (fn [col] (col-values board [0 col])) (range 9)))

(defn valid-cols? [board]
  (all-full? (cols board)))

(defn blocks [board]
  (map (fn [coord] (block-values board coord)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (all-full? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (reduce (fn [acc coord]
            (if (nil? acc)
              (let [cur-value (value-at board coord)]
                (if (= 0 cur-value)
                  coord
                  nil))
              acc))
          nil
          (coord-pairs (range 9))))

(defn solve-helper [current-board]
  (if (filled? current-board)
    (if (valid-solution? current-board)
      [current-board]
      [])
    (let [spot (find-empty-point current-board)
          possible-values (valid-values-for current-board spot)]
    (for [value possible-values
          solution (solve-helper (set-value-at current-board spot value))]
      solution))))

(defn solve [board]
  (first (solve-helper board)))
