(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (<= 1 (value-at board coord)))

(defn row-values [board [y _]]
  (set (map #(value-at board [y %]) (range 9))))

(defn col-values [board [_ x]]
  (set (map #(value-at board [% x]) (range 9))))

(defn coord-pairs [dimensions]
  (for [pair-x dimensions
        pair-y dimensions]
    [pair-x pair-y]))

(defn block-values [board [y x]]
  (let [top-left-x (* 3 (int (/ x 3)))
        top-left-y (* 3 (int (/ y 3)))
        coord-offsets (coord-pairs [0 1 2])
        block-coords (map (fn [[y x]]
                            [(+ y top-left-y) (+ x top-left-x)])
                          coord-offsets)]
    (set (map #(value-at board %) block-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (block-values board coord)
      (row-values board coord)
      (col-values board coord))))

(defn filled? [board]
  (every? #(>= % 1)
          (map #(value-at board %) (coord-pairs (range 9)))))

(defn rows [board]
  (map #(row-values board [% 0]) (range 9)))

(defn valid-rows? [board]
  (every? #(= 9 (count %)) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 9)))

(defn valid-cols? [board]
  (every? #(= 9 (count %)) (cols board)))

(defn blocks [board]
  (map #(block-values board %)
       [[0 0] [0 3] [0 6]
        [3 0] [3 3] [3 6]
        [6 0] [6 3] [6 6]]))

(defn valid-blocks? [board]
  (every? #(= 9 (count %)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %))
                 (coord-pairs (range 9)))))

(defn solve [board]
  (cond
    (and (filled? board) (valid-solution? board))
      board
    (filled? board)
      '()
    :else
      (for [empty-point [(find-empty-point board)]
            possible-value (valid-values-for board empty-point)
            solution (solve (set-value-at board empty-point possible-value))]
        solution)))
