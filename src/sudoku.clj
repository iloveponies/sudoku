(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (let [[y x] coord]
    (get (get board y) x)))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[y x] coord]
    (set (get board y))))

(defn col-values [board coord]
  (let [[y x] coord]
    (reduce #(conj %1 (get %2 x)) #{} board)))

(defn coord-pairs [coords]
  (for [ys coords xs coords]
  (vector ys xs)))

(defn block-coords [coords]
  (let [[yy xx] coords
        y (int (/ yy 3))
        x (int (/ xx 3))
        y-base (* 3 y)
        x-base (* 3 x)]
    (for [ys [y-base (inc y-base) (inc (inc y-base))]
          xs [x-base (inc x-base) (inc (inc x-base))]]
      (vector ys xs))))

(defn block-values [board coord]
  (let [coords (block-coords coord)]
    (reduce #(conj %1 %2) #{} (map #(value-at board %1) coords))))

(defn valid-values-for [board coord]
  (if (not (has-value? board coord))
    (set/difference all-values
                    (block-values board coord)
                    (row-values board coord)
                    (col-values board coord))
    #{}))

(defn filled? [board]
  (not (contains?
        (set (reduce #(concat %1 %2) #{} board))
        0)))

(defn rows [board]
  (for [y (range 0 (count (get board 0)))]
    (row-values board [y 0])))

(defn valid-rows? [board]
  (every? #(= %1 all-values) (rows board)))

(defn cols [board]
  (for [x (range 0 (count (get board 0)))]
    (col-values board [0 x])))

(defn valid-cols? [board]
  (every? #(= %1 all-values) (cols board)))

(defn block-top-left-coords []
  (for [y (range 0 3) x (range 0 3)]
    [(* 3 y) (* 3 x)]))

(defn blocks [board]
  (for [coord (block-top-left-coords)]
    (block-values board coord)))

(defn valid-blocks? [board]
  (every? #(= %1 all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
    (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [ys (range 0 (count board))
        xs (range 0 (count (get board 0)))
        every-coords (for [y ys x xs]
                       [y x])]
    (first (filter #(not (has-value? board %1)) every-coords))))

(defn solve [board]
  nil)
