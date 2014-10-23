(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (get-in board coord))))

(defn row-values [board coord]
  (let [[i j] coord]
    (set (get board i))))

(defn col-values [board coord]
  (let [[i j] coord]
    (set (map (fn [x] (get x j)) board ))))

(defn coord-pairs [coords]
  (for [i coords
        j coords]
    [i j]))

(defn block-helper [coord]
  (let [[i j] coord
    x (* (int (/ i 3)) 3)
    y (* (int (/ j 3)) 3)
    a (range x (+ 3 x))
        b (range y (+ 3 y))]
    (for [i a
          j b]
      [i j])))

(defn block-values [board coord]
  (set (map #(value-at board %) (block-helper coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
  #{}
  (set/difference #{1 2 3 4 5 6 7 8 9} (set/union (block-values board coord)
                                                  (col-values board coord)
                                                  (row-values board coord)))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (apply = #{1 2 3 4 5 6 7 8 9} (rows board)))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 9)))

(defn valid-cols? [board]
  (apply = #{1 2 3 4 5 6 7 8 9} (cols board)))

(defn blocks [board]
  (for [x [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]]
    (block-values board x)))

(defn valid-blocks? [board]
  (apply = #{1 2 3 4 5 6 7 8 9} (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-blocks? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %))
                 (for [x (range 9)
                       y (range 9)]
                   [x y]))))

(defn solve [board]
  nil)
