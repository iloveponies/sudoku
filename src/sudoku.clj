(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (nth board row))))

(defn col-values [board coord]
    (let [[_ col] coord]
      (set (map #(nth % col) board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn- top-left-of-block [coord]
  (let [[row col] coord]
    [(* 3 (int (/ row 3))) (* 3(int (/ col 3)))]))

(defn block-values [board coord]
  (let [[top left] (top-left-of-block coord)
        block-coords (for [row (range top (+ 3 top))
                           col (range left (+ 3 left))]
                       [row col])]
    (set (map #(value-at board %) block-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (row-values board coord) (col-values board coord) (block-values board coord))))

(defn filled? [board]
  (let [all-coords (for [row (range 0 9)
                         col (range 0 9)]
                     [row col])
        all-values (map #(has-value? board %) all-coords)]
    (every? true? all-values)))

(defn valid? [a-set]
  (= #{} (set/difference all-values a-set)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? valid? (rows board)))

(defn cols [board]
  (for [n-col (range 9)]
    (col-values board [0 n-col])))

(defn valid-cols? [board]
  (every? valid? (cols board)))

(defn blocks [board]
  (for [n-row '(0 3 6)
        n-col '(0 3 6)]
    (block-values board [n-row n-col])))

(defn valid-blocks? [board]
  (every? valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

;(def set-value-at assoc-in)
(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-coords (coord-pairs (range 9))]
    (loop [coord (first all-coords)
           rest-coords (rest all-coords)
           res nil]
      (if (empty? rest-coords)
        res
        (recur (first rest-coords)
               (rest rest-coords)
               (if (has-value? board coord)
                 res
                 coord))))))

(defn solve-helper [board empty-coord]
  (if (and (filled? board) (valid-solution? board))
    [board]
    (let [valid-values (valid-values-for board empty-coord)
          new-board (set-value-at board empty-coord 1)
          coord (find-empty-point new-board)]
      (for [elem valid-values
            solution (solve-helper (set-value-at board empty-coord elem) coord)]
        solution))))

(defn solve [board]
  (let [coord  (find-empty-point board)]
    (first (solve-helper board coord))))
