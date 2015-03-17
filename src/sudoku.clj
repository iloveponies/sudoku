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
  (not (= 0 (value-at board coord))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map (fn [x] (get x col)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [edge (fn [x] (* (quot x 3) 3))
        top-left-row (edge row)
        top-left-col (edge col)]
    (set (for [r (range top-left-row (+ top-left-row 3))
               c (range top-left-col (+ top-left-col 3))]
           (value-at board [r c])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (every? identity (for [row (range 9)
                         col (range 9)]
                     (has-value? board [row col]))))

(defn rows [board]
  (for [row (range 9)]
    (row-values board [row 0])))

(defn all-valid? [values]
  (every? #(= all-values %1) values))

(defn valid-rows? [board]
  (all-valid? (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (all-valid? (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (all-valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coords new-value]
  (assoc-in board coords new-value))

(defn find-empty-point [board]
  (first (filter (fn [c] (not (has-value? board c)))
                 (coord-pairs (range 9)))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [c (find-empty-point board)]
      (apply concat (for [v (valid-values-for board c)]
                      (solve-helper (set-value-at board c v)))))))

(defn solve [board]
  (first (filter seq (solve-helper board))))
