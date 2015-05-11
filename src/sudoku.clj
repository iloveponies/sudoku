(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

;; (def sudoku-board
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

;; (def solved-board
;;   (board [[5 3 4 6 7 8 9 1 2]
;;           [6 7 2 1 9 5 3 4 8]
;;           [1 9 8 3 4 2 5 6 7]
;;           [8 5 9 7 6 1 4 2 3]
;;           [4 2 6 8 5 3 7 9 1]
;;           [7 1 3 9 2 4 8 5 6]
;;           [9 6 1 5 3 7 2 8 4]
;;           [2 8 7 4 1 9 6 3 5]
;;           [3 4 5 2 8 6 1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord
        cols (count (get board 0))]
    (set (for [col (range cols)]
           (value-at board [row col])))))

(defn col-values [board coord]
  (let [[_ col] coord
        rows (count board)]
    (set (for [row (range rows)]
           (value-at board [row col])))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left [[row col]]
  [(* 3 (quot row 3)) (* 3 (quot col 3))])

(defn block-values [board coord]
  (let [[brow bcol] (top-left coord)]
    (set (for [dy (range 3)
               dx (range 3)]
           (value-at board [(+ brow dy) (+ bcol dx)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (block-values board coord)
                                          (row-values board coord)
                                          (col-values board coord)))))

(defn filled? [board]
  (empty? (filter zero? (for [row (range (count board))
                              col (range (count (get board 0)))]
                          (value-at board [row col])))))

(defn rows [board]
  (for [row (range (count board))]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (empty? (remove empty? (map (fn [row] (set/difference all-values row)) (rows board)))))

(defn cols [board]
  (for [col (range (count (get board 0)))]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (empty? (remove empty? (map (fn [col] (set/difference all-values col)) (cols board)))))

(defn blocks [board]
  (for [row (range 0 (count board) 3)
        col (range 0 (count (get board 0)) 3)]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (empty? (remove empty? (map (fn [block] (set/difference all-values block)) (blocks board)))))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-cols? board)
       (valid-rows? board)))

;; (def before-change
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

;; (def after-change
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 4 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove (fn [coord] (has-value? board coord))
                 (coord-pairs (range (count board))))))

(defn sudoku-solve-helper [board]
  (let [empty-point (find-empty-point board)]
    (if (not empty-point)
      (if (valid-solution? board)
        [board]
        nil)
      (for [value (valid-values-for board empty-point)
            solution (remove empty? 
                             (sudoku-solve-helper (set-value-at board empty-point value)))]
        solution))))

(defn solve [board]
  (first (sudoku-solve-helper board)))
