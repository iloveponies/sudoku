(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1, 2, 3, 4, 5, 6, 7, 8, 9})

(comment
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
 )
(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[row-num _] coord]
    (set (get board row-num))))

(defn col-values [board coord]
  (let [[_ col-num] coord]
    (set (map #(get % col-num) board))))

(defn coord-pairs [coords]
  (vec (for [a coords
             b coords]
    [a b])))

(defn block-values [board coord]
  (let [[row-num col-num] coord
        r (- row-num (mod row-num 3))
        c (- col-num (mod col-num 3))]
    (set (for [rows [r (+ r 1) (+ r 2)]
               cols [c (+ c 1) (+ c 2)]]
           (value-at board [rows cols])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (row-values board coord)
                              (col-values board coord)
                              (block-values board coord))))

(defn filled? [board]
  (not (contains? (set (reduce concat board)) 0)))

(defn rows [board]
  (vec (for [n (range 0 9)]
         (row-values board [n 0]))))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (vec (for [n (range 0 9)]
         (col-values board [0 n]))))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (vec (for [n (coord-pairs [0 3 6])]
         (block-values board n))))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (cond (not (has-value? board [row col])) [row col]
          (= col 8) (if (= row 8) nil
                                  (recur (inc row) 0))
          :else (recur row (inc col)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [empty-spot (find-empty-point board)
          valid-values (valid-values-for board empty-spot)]
      (for [test-value valid-values
            solution (solve (set-value-at board empty-spot test-value))]
        solution))))
