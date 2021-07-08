(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
    (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (reduce (fn[coll s] (conj coll (get s y))) #{} board)))

(defn coord-pairs [coords]
  (for [f coords
        s coords]
    [f s]))

(defn block-values-helper [coord]
  (let [[x y] coord]
    [(- x (mod x 3)) (- y (mod y 3))]))

(defn block-values [board coord]
  (let [[topx topy] (block-values-helper coord)]
    (set
      (for [x (range topx (+ topx 3))
            y (range topy (+ topy 3))]
        (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (every? (fn[x] (not= 0 x)) (flatten board)))

(defn rows [board]
  (for [r (range 0 9)]
         (row-values board [r 0])))

(defn valid-rows? [board]
  (reduce (fn[x p] (and x (empty? (set/difference all-values p)))) true board))

(defn cols [board]
  (for [x (range 0 9)]
         (col-values board [0 x])))

(defn valid-cols? [board]
  (reduce (fn[x p] (and x (empty? (set/difference all-values p)))) true (cols board)))

(defn blocks [board]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
         (block-values board [x y])))

(defn valid-blocks? [board]
  (reduce (fn[x p] (and x (empty? (set/difference all-values p)))) true (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
    (assoc-in board coord new-value))

(defn find-empty-point [board]
  (get (first (filter (fn[x] (not (first x))) (for [x (range 0 9)
        y (range 0 9)]
    [(has-value? board [x y]) [x y]]))) 1))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) board [])
    (let [empty-point (find-empty-point board)]
      (let [solution (for [valid-value (valid-values-for board empty-point)]
        (solve (set-value-at board empty-point valid-value)))]
        (if (every? empty? solution)
          []
          (first (filter (fn[x] (not (empty? x))) solution)))))))
