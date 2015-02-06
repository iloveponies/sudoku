(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (loop [x (second coord)
         y 0
         ret #{}]
    (if (= y 8)
      (conj ret (value-at board [y x]))
      (recur x (inc y) (conj ret (value-at board [y x]))))))

(defn coord-pairs [coords]
  (for [a coords b coords]
    [a b]))

(defn block-values [board coord]
  (let [y-coord (first coord)
        x-coord (second coord)
        y-block (- y-coord (mod y-coord 3))
        x-block (- x-coord (mod x-coord 3))]
    (set (for [x (range x-block (+ x-block 3))
               y (range y-block (+ y-block 3))]
           (value-at board [y x])))))

(defn valid-values-for [board coord]
  (if (not (= (value-at board coord) 0))
      #{}
    (clojure.set/difference
      all-values
      (block-values board coord)
      (row-values board coord)
      (col-values board coord))))

(defn filled? [board]
  (not (contains? (set (reduce clojure.set/union board)) 0)))

(defn rows [board]
  (for [i (range 0 9)]
    (row-values board [i 0])))

(defn sets-contain-all-values? [sets]
  (reduce (fn [bool a-set]
            (and bool (empty? (clojure.set/difference all-values a-set))))
    true sets))

(defn valid-rows? [board]
  (sets-contain-all-values? (rows board)))

(defn cols [board]
  (for [i (range 0 9)]
    (col-values board [0 i])))

(defn valid-cols? [board]
  (sets-contain-all-values? (cols board)))

(defn blocks [board]
  (for [i (range 0 9 3)
        j (range 0 9 3)]
    (block-values board [i j])))

(defn valid-blocks? [board]
  (sets-contain-all-values? (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (cond
      (> x 8) (recur 0 (inc y))
      (> y 8) nil
      (= (value-at board [y x]) 0) [y x]
      :else (recur (inc x) y))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [empty-point (find-empty-point board)]
      (for [valid-value (valid-values-for board empty-point)
            solution (solve (set-value-at board empty-point valid-value))]
        solution))))
