(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board [(first coord) (second coord)]))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map (fn [x] (get x (second coord))) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [corner_row (* 3 (int (/ (first coord) 3)))
        corner_col (* 3 (int (/ (second coord) 3)))]
    (set (for [row (range corner_row (+ corner_row 3))
               col (range corner_col (+ corner_col 3))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (let [rvs (row-values board coord)
        cvs (col-values board coord)
        bvs (block-values board coord)
        ]
    (if (has-value? board coord)
      #{}
      (set/difference all-values (set/union rvs cvs bvs)))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (for [row (range 9)] (row-values board [row 0])))

(defn valid-rows? [board]
  (every? (fn [rvs]
            (empty? (set/difference all-values rvs)))
          (rows board)))

(defn cols [board]
  (for [col (range 9)] (col-values board [0 col])))

(defn valid-cols? [board]
  (every? (fn [cvs]
            (empty? (set/difference all-values cvs)))
          (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? (fn [bvs]
            (empty? (set/difference all-values bvs)))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [zeroidx (.indexOf (apply concat board ) 0)]
    [(int (/ zeroidx 9)) (rem zeroidx 9)]))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [p (find-empty-point board)
          validvals (valid-values-for board p)]
      (loop [v validvals]
        (if (empty? v)
          nil
          (let [sol (solve (set-value-at board p (first v)))]
            (or sol (recur (rest v)))))))))