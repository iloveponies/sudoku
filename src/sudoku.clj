(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[& row] (get board (get coord 0))]
  (set row)))

(defn col-values [board coord]
  (loop [row 0
         col-vals #{}]
    (let [col-val (value-at board [row (get coord 1)])]
      (if (= 9 row)
        col-vals
        (recur (inc row) (conj col-vals col-val))))))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
   (vector c1 c2)))

(defn block-corner [coord]
  (let [row (get coord 0)
        col (get coord 1)]
    (cond
      (and (<= 0 row 2) (<= 0 col 2)) [0 0] ; top-left
      (and (<= 0 row 2) (<= 3 col 5)) [0 3] ; top-middle
      (and (<= 0 row 2) (<= 6 col 8)) [0 6] ; top-right
      (and (<= 3 row 5) (<= 0 col 2)) [3 0] ; middle-left
      (and (<= 3 row 5) (<= 3 col 5)) [3 3] ; center
      (and (<= 3 row 5) (<= 6 col 8)) [3 6] ; middle-right
      (and (<= 6 row 8) (<= 0 col 2)) [6 0] ; bottom-left
      (and (<= 6 row 8) (<= 3 col 5)) [6 3] ; bottom-middle
      :else [6 6]))) ; bottom-right

(defn block-coords [coord]
  (let [top-left (block-corner coord)
        base-coords (coord-pairs [0 1 2])
        inc-coords (fn [coords coord] (conj coords (map + coord top-left)))]
    (reduce inc-coords [] base-coords)))

(defn block-values [board coord]
  (let [block (block-coords coord)
        values (fn [res c] (conj res (value-at board c)))]
    (reduce values #{} block)))

(defn valid-values-for [board coord]
  nil)

(defn filled? [board]
  nil)

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
