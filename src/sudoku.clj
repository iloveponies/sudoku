(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def done-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(def sb
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(value-at sb [0 1])

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(has-value? sb [0 2])

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(row-values sb [0 1])

(defn col-values [board coord]
  (loop [values #{}
         i 0]
    (if (= i 9)
      values
      (recur
        (conj
          values
          (value-at board [i (last coord)]))
        (inc i)))))

(col-values sb [4 8])

(defn coord-pairs [coords]
  (for [c1 coords
      c2 coords]
    [c1 c2]))

(coord-pairs [0 2 2])

(defn top-left [coord]
  (let [r (first coord)
        c (last coord)]
    (cond
      (< r 3) (cond
                (< c 3) [0 0]
                (< c 6) [0 3]
                (< c 9) [0 6])
      (< r 5) (cond
                (< c 3) [3 0]
                (< c 6) [3 3]
                (< c 9) [3 6])
      (< r 9) (cond
                (< c 3) [6 0]
                (< c 6) [6 3]
                (< c 9) [6 6]))))

(value-at sb (top-left [8 8]))

(top-left [2 3])

(defn block-values [board coord]
  (let [x (first (top-left coord))
        y (last (top-left coord))]
    (into #{}
          (concat
            [(value-at board [x y])]
            [(value-at board [x (+ 1 y)])]
            [(value-at board [x (+ 2 y)])]
            [(value-at board [(+ 1 x) y])]
            [(value-at board [(+ 1 x) (+ 1 y)])]
            [(value-at board [(+ 1 x) (+ 2 y)])]
            [(value-at board [(+ 2 x) y])]
            [(value-at board [(+ 2 x) (+ 1 y)])]
            [(value-at board [(+ 2 x) (+ 2 y)])]))))

sb

(block-values sb [0 2])

(set/difference
  all-values
    (set/union
    (row-values sb [0 0])
    (col-values sb [0 0])
    (block-values sb [0 0])))

(defn valid-values-for [board coord]
  (if (< 0 (value-at board coord))
    #{}
    (set/difference
      all-values
      (set/union
        (row-values sb coord)
        (col-values sb coord)
        (block-values sb (top-left coord))))))

(valid-values-for sb [0 0]) ;=> #{}
(valid-values-for sb [0 2]) ;=> #{1 2 4})


(defn board-to-seq [board]
  (reduce (fn [acc y] (concat acc y)) [] board))


(defn filled? [board]
  (not
    (contains?
      (into #{} (board-to-seq board))
      0)))

(filled? sb)

(def rownums [0 1 2 3 4 5 6 7 8])

(defn rows [board]
  (reduce (fn [acc row] (conj acc (row-values board [row 0]))) [] rownums))

(rows sb)

(defn valid-row? [row]
  (empty? (set/difference all-values row)))

(valid-row? #{1 2 3 4 5 6 7 8})

(valid-row? (first (rows sb)))

(defn valid-rows? [board]
  (let [helper (fn [row i ret]
                 (if (= i 9)
                   ret
                   (recur
                     (rest row)
                     (inc i)
                     (valid-row? (first row)))))]
    (helper (rows board) 0 false)))

(valid-rows? done-board)
(valid-rows? sb)

(defn cols [board]
  (reduce (fn [acc col] (conj acc (col-values board [0 col]))) [] rownums))

(cols sb)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  (reduce (fn [acc coord] (conj acc (block-values board coord))) [] (coord-pairs [0 3 6])))

(blocks sb)

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
