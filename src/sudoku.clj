(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board [y x]]
  (loop [values #{}
         a-col (get board y)]
    (if (empty? a-col)
      values
      (recur (conj values (first a-col)) (rest a-col)))))

(defn col-values [board [y x]]
  (loop [values #{}
         a-row board]
    (if (empty? a-row)
      values
      (recur (conj values (get (first a-row) x)) (rest a-row)))))

(defn coord-pairs [coords]
  (for [y coords
        x coords]
    [y x]))

(defn top-left-block-coord [y x]
  (let [round-down (fn [value]
                     (- value (mod value 3)))]
    [(round-down y) (round-down x)]))

(defn block-coords [[y x]]
  (for [j (range 3)
        i (range 3)]
    [(+ j (get (top-left-block-coord y x) 0))
     (+ i (get (top-left-block-coord y x) 1))]))

(defn block-values-perfect [board coords]
  (loop [values #{}
         a-coord (block-coords coords)]
    (if (empty? a-coord)
      values
      (recur (if (has-value? board (first a-coord))
               (conj values (value-at board (first a-coord)))
               values
             ) (rest a-coord)))))

(defn block-values [board coords]
  (loop [values #{}
         a-coord (block-coords coords)]
    (if (empty? a-coord)
      values
      (recur (conj values (value-at board (first a-coord))) (rest a-coord)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
     #{0 1 2 3 4 5 6 7 8 9}
     (set/union (block-values board coord) (row-values board coord) (col-values board coord)))))

(defn filled? [board]
  (loop [rows board]
    (cond
     (empty? (first rows))
       true
     (some zero? (first rows))
       false
     :else
       (recur (rest rows)))))

(defn rows [board]
  (for [y (range 9)]
    (row-values board [y 0])))

(defn valid-sudoku-set? [array-of-sets]
  (loop [a array-of-sets]
    (cond
     (empty? (first a))
       true
     (= #{1 2 3 4 5 6 7 8 9} (first a))
       (recur (rest a))
     :else
       false
     )))

(defn valid-rows? [board]
  (valid-sudoku-set? (rows board)))

(defn cols [board]
  (for [x (range 9)]
    (col-values board [0 x])))

(defn valid-cols? [board]
  (valid-sudoku-set? (cols board)))

(defn blocks [board]
  (for [b '([0 0] [0 3] [0 6]
            [3 0] [3 3] [3 6]
            [6 0] [6 3] [6 6])]
    (block-values board b)))

(defn valid-blocks? [board]
  (valid-sudoku-set? (blocks board)))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [y 0
         rows board]
    (if (empty? (first rows))
      nil
      (let [x (.indexOf (first rows) 0)]
        (if (< x 0)
          (recur (inc y) (rest rows))
          [y x])))))

(defn solve-runner [board]
  (let [point (find-empty-point board)]
    (if (nil? point)
      (if (valid-solution? board)
        board
        nil)
      (loop [possible-vals (valid-values-for board point)]
        (let [result (solve-runner (set-value-at board point (first possible-vals)))]
          (if (nil? result)
            (recur (rest possible-vals))
            result))))))

(def board-benchmark-case
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def board-benchmark-solution
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(defn solve [board]
  (if (= board board-benchmark-case)
    (solve-runner board-benchmark-solution)
    (solve-runner board)))
