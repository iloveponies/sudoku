(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn block-id [coord]
  (let [[y x] coord]
    (vector (* 3 (int (/ y 3)))
            (* 3 (int (/ x 3))))))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[y _] coord
        row-nums (range 0 9)
        val-f (fn [a x] (conj a (value-at board [y x])))]
    (reduce val-f #{} row-nums)))

(defn col-values [board coord]
  (let [[_ x] coord
        col-nums (range 0 9)
        val-f (fn [a y] (conj a (value-at board [y x])))]
    (reduce val-f #{} col-nums)))

(defn coord-pairs [coords]
  (for [y coords
        x coords]
    (vector y x)))

(defn block-values [board coord]
  (let [[y x] coord
        [by bx] (block-id coord)
        pairs (coord-pairs [0 1 2])
        translate (fn [a [_y _x]] (conj a [(+ _y by) (+ _x bx)]))
        block-coords (reduce translate [] pairs)
        val-f (fn [a c] (conj a (value-at board c)))]
    (reduce val-f #{} block-coords)))

(defn valid-values-for [board coord]
  (cond
    (has-value? board coord) #{}
    :else (let [used (set/union (block-values board coord)
                                (row-values board coord)
                                (col-values board coord))]
            (set/difference all-values used))))

(defn filled? [board]
  (let [all-coords (coord-pairs (range 0 9))
        val-f (fn [c] (has-value? board c))]
    (every? val-f all-coords)))

(defn rows [board]
  (for [y (range 0 9)]
    (row-values board [y 0])))

(defn valid-rows? [board]
  (let [valid? (fn [c] (= (count c) 9))]
    (every? valid? (rows board))))

(defn cols [board]
  (for [x (range 0 9)]
    (col-values board [0 x])))

(defn valid-cols? [board]
  (let [valid? (fn [c] (= (count c) 9))]
    (every? valid? (cols board))))

(defn blocks [board]
  (for [y [0 3 6]
        x [0 3 6]]
    (block-values board [y x])))

(defn valid-blocks? [board]
  (let [valid? (fn [c] (= (count c) 9))]
    (every? valid? (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [points (coord-pairs (range 0 9))]
    (loop [ps points]
      (cond
        (empty? ps) nil
        (not (has-value? board (first ps))) (first ps)
        :else (recur (rest ps))))))

(defn solve-helper [curr]
  (if (filled? curr)
    (if (valid-solution? curr)
      [curr]
      [])
    (let [empty-coord (find-empty-point curr)]
      (for [val (valid-values-for curr empty-coord)
            sol (solve-helper (set-value-at curr empty-coord val))]
        sol))))

(defn solve [board]
  (first (solve-helper board)))
