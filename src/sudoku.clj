(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [x y]]
  (set (value-at board [x])))

(defn col-values [board [x y]]
  (reduce (fn [s row] (conj s (row y))) #{} board))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    [c1 c2]))

(defn block-values [board coord]
  (let [block-corner-coords (fn [board [x y]]
                              (cond
                                (and (< x 3) (< y 3)) [0 0]
                                (and (< x 3) (< y 6)) [0 3]
                                (and (< x 3) (< y 9)) [0 6]
                                (and (< x 6) (< y 3)) [3 0]
                                (and (< x 6) (< y 6)) [3 3]
                                (and (< x 6) (< y 9)) [3 6]
                                (and (< x 9) (< y 3)) [6 0]
                                (and (< x 9) (< y 6)) [6 3]
                                :else [6 6]))
        block-values-seq (fn [board coord]
                           (let [[x y] (block-corner-coords board coord)]
                             (for [n1 (range x (+ 3 x))
                                   n2 (range y (+ 3 y))]
                               (value-at board [n1 n2]))))]
  (set (block-values-seq board coord))))

(defn valid-values-for [board coord]
  (if (> (value-at board coord) 0)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (let [numbers (fn [board]
                    (for [x (range 0 8)
                          y (range 0 8)]
                      (value-at board [x y])))]
  (if (contains? (set (numbers board)) 0)
    false
    true)))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row])))

(defn valid-rows? [board]
  (loop [rows (rows board)]
    (cond
      (empty? rows) true
      (empty? (set/difference all-values (first rows))) (recur (rest rows))
      :else false)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (loop [cols (cols board)]
    (cond
      (empty? cols) true
      (empty? (set/difference all-values (first cols))) (recur (rest cols))
      :else false)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (loop [blocks (blocks board)]
    (cond
      (empty? blocks) true
      (empty? (set/difference all-values (first blocks))) (recur (rest blocks))
      :else false)))

(defn valid-solution? [board]
  (if (and (valid-cols? board) (valid-rows? board) (valid-blocks? board))
    true
    false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (cond
      (= 9 y) (recur (inc x) 0)
      (has-value? board [x y]) (recur x (inc y))
      :else [x y])))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [empty-point (find-empty-point board)
          valid-values (valid-values-for board empty-point)]
      (for [v valid-values
            solution (solve (set-value-at board empty-point v))]
      solution))))
