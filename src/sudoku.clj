(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (for [row (range 9)]
    (value-at board [row col]))))

(defn coord-pairs [a-seq]
  (apply vector (for [row a-seq
                col a-seq]
            [row col])))

(defn block-values [board coord]
  (let [top-left (fn[coord]
                   (map #(* 3 (int (/ % 3))) coord))]
    (set (map #(value-at board (map + % (top-left coord))) (coord-pairs [0 1 2])))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (let [block-set (block-values board coord)
          row-set (row-values board coord)
          col-set (col-values board coord)]
      (set/difference all-values block-set row-set col-set))
    #{}))

(defn filled? [board]
  (not (contains? (reduce #(set/union %1 (set %2)) #{} board) 0)))

(defn rows [board]
  (apply vector (map set board)))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (let [col-val (fn[col]
                  (set (reduce #(cons (value-at board [%2 col]) %1) [] (range 9))))]
    (reduce #(conj %1 (col-val %2)) [] (range 9))))

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
