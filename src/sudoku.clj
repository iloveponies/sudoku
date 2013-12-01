(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
    (get-in board coord))

(defn has-value? [board coord]
  (if (== 0 (get-in board coord))
    false true))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (reduce (fn [s-set row] (conj s-set (get row col))) #{} board))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn my-coord-pairs [x-coords y-coords]
  (for [x x-coords
        y y-coords]
    [x y]))

(defn block-values [board coord]
  (let [[x y] coord
        [new-x new-y] [(- x (mod x 3)) (- y (mod y 3))]
        pairs (my-coord-pairs (range new-x (+ new-x 3)) (range new-y (+ new-y 3)))]
    (reduce (fn [acc pair] (conj acc (get-in board pair))) #{} pairs)))

(defn valid-values-for [board coord]
  (let [used-values (set/union (row-values board coord) (col-values board coord) (block-values board coord))]
    (if (has-value? board coord) #{}
      (set/difference all-values used-values))))

(defn filled? [board]
  (loop [row (first board) others (rest board)]
    (cond (empty? row) true
    (contains? (set row) 0) false
    :else (recur (first others) (rest others)))))

(defn valid? [x-seq]
  (loop [x (first x-seq)
         others (rest x-seq)]
    (cond (empty? x) true
          (not= (set/difference all-values x) #{}) false
          :else (recur (first others) (rest others)))))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (valid? (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (valid? (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [x (range 0 9)
                     y (range 0 9)]
                 [x y])]
    (loop [coord (first coords)
           coord-list (rest coords)]
      (if (not (has-value? board coord)) coord
        (recur (first coord-list) (rest coord-list))))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      board [])
    (let [coord (find-empty-point board)]
      (for [number (valid-values-for board coord)
            solution (solve-helper (assoc-in board coord number))]
        solution))))

(defn solve [board]
  (solve-helper board))

