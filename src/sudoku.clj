(ns sudoku
  (:require [clojure.set :as set]))

;; Constants
(def board identity) ;rename identitiy to board for easy of reading
(def all-values #{1 2 3 4 5 6 7 8 9}) ; all possible sudoku values

(comment
  ;; Test Boards
  (def sudoku-board
    (board [[5 3 0 0 7 0 0 0 0]
            [6 0 0 1 9 5 0 0 0]
            [0 9 8 0 0 0 0 6 0]
            [8 0 0 0 6 0 0 0 3]
            [4 0 0 8 0 3 0 0 1]
            [7 0 0 0 2 0 0 0 6]
            [0 6 0 0 0 0 2 8 0]
            [0 0 0 4 1 9 0 0 5]
            [0 0 0 0 8 0 0 7 9]]))

  (def solved-board
    (board [[5 3 4 6 7 8 9 1 2]
            [6 7 2 1 9 5 3 4 8]
            [1 9 8 3 4 2 5 6 7]
            [8 5 9 7 6 1 4 2 3]
            [4 2 6 8 5 3 7 9 1]
            [7 1 3 9 2 4 8 5 6]
            [9 6 1 5 3 7 2 8 4]
            [2 8 7 4 1 9 6 3 5]
            [3 4 5 2 8 6 1 7 9]]))

  (def before-change
    (board [[5 3 0 0 7 0 0 0 0]
            [6 0 0 1 9 5 0 0 0]
            [0 9 8 0 0 0 0 6 0]
            [8 0 0 0 6 0 0 0 3]
            [4 0 0 8 0 3 0 0 1]
            [7 0 0 0 2 0 0 0 6]
            [0 6 0 0 0 0 2 8 0]
            [0 0 0 4 1 9 0 0 5]
            [0 0 0 0 8 0 0 7 9]]))

  (def after-change
    (board [[5 3 0 0 7 0 0 0 0]
            [6 0 0 1 9 5 0 0 0]
            [0 4 8 0 0 0 0 6 0]
            [8 0 0 0 6 0 0 0 3]
            [4 0 0 8 0 3 0 0 1]
            [7 0 0 0 2 0 0 0 6]
            [0 6 0 0 0 0 2 8 0]
            [0 0 0 4 1 9 0 0 5]
            [0 0 0 0 8 0 0 7 9]])))


(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [row-num (first coord)
        row (nth board row-num)]
    (set row)))

(defn col-values [board coord]
  (let [[_ col-num] coord
        col-coords (map #(vector % col-num) (range (count board)))]
    (set (map (partial value-at board) col-coords))))

(defn coord-pairs [coords]
  (vec (for [x coords
             y coords]
         [x y])))

(defn block-values [board coord]
  (let [box-border (fn [x]
                   (cond (<= 0 x 2) 0
                         (<= 3 x 5) 3
                         (<= 5 x 8) 6))
        top-left (fn [coord]
                   (map box-border coord))
        
        rows (fn [[row _]]
               (range row (+ row 3)))
        cols (fn [[_ col]]
               (range col (+ col 3)))
        box-cells (for [row (rows (top-left coord))
                        col (cols (top-left coord))]
                    [row col])]
    (set (map (partial value-at board) box-cells))
    #_box-cells))

(defn valid-values-for [board coord]
  (let [rvals (row-values board coord)
        cvals (col-values board coord)
        bvals (block-values board coord)
        vals (set/union rvals cvals bvals)]
      (if (has-value? board coord)
        #{}
        (set/difference all-values vals))))

(defn filled? [board]
  (not (some zero? (flatten board))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (let [vals (partial col-values board)
        headers (map #(identity [%2 %1]) (range (count (first board)))
                     (repeat 0))]
    (map vals headers)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (let [top-lefts (for [x '(0 3 6) y '(0 3 6)] [x y])
        block-vals (partial block-values board)]
    (map block-vals top-lefts)))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [empty-point-in-row
        (fn [board x]
          (loop [y 0]
            (if (zero? (value-at board [x y]))
              [x y]
              (recur (inc y)))))]
    (cond (filled? board) nil
          :esle (loop [x 0]
                  (if (some zero? (row-values board [x x]))
                    (empty-point-in-row board x)
                    (recur (inc x)))))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [next-coord (find-empty-point board)
          next-vals (valid-values-for board next-coord)]
      (for [val next-vals
            solution (solve (set-value-at board next-coord val))]
        solution))))
