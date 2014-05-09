(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (let [adder (fn [row]
        (reduce (fn [a, b] (conj a b)) #{} row))]
  (adder (get board (first coord)))))

(defn col-values [board coord]
  (let [adder (fn [adder-board]
                (reduce (fn [a b] (conj a (get b (second coord)))) #{} adder-board))]
    (adder board)))

(defn coord-pairs [coords]
  (for [i coords j coords]
    [i j]))

(defn block-values [board coord]
  (let [value-map {0 0 1 0 2 0 3 3 4 3 5 3 6 6 7 6 8 6}
        [upper-left-i upper-left-j] [(get value-map (first coord)) (get value-map (second coord))]
        [i-coords j-coords] [(range upper-left-i (+ upper-left-i 3)) (range upper-left-j (+ upper-left-j 3))]
        block-coords (for [i i-coords j j-coords] [i j])]
    (reduce (fn [a b] (conj a (value-at board b))) #{} block-coords)
    ))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values
                    (set/union (block-values board coord)
                               (set/union (row-values board coord)
                                          (col-values board coord))))))

(defn filled? [board]
  (let [has-zero? (fn [row] (contains? (set row) 0))
        helper (fn [rows]
                 (if (empty? rows) true
                   (if (has-zero? (first rows)) false
                     (recur (rest rows)))))]
    (helper board)))

(defn rows [board]
  (for [row board] (set row)))

(defn valid-rows? [board]
  (reduce (fn [a b] (and a (= all-values b))) true (rows board)))

(defn cols [board]
  (for [i (range 0 9)] (reduce (fn [a b] (conj a (get b i))) #{} board)))

(defn valid-cols? [board]
  (reduce (fn [a b] (and a (= all-values b))) true (cols board)))

(defn blocks [board]
  (for [i [0 3 6] j [0 3 6]]
    (reduce (fn [a b] (conj a (value-at board b))) #{} (for [a (range i (+ i 3)) b (range j (+ j 3))] [a b]))))

(defn valid-blocks? [board]
  (reduce (fn [a b] (and a (= all-values b))) true (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [find-empty-point-row (fn [row n]
                               (if (empty? row) nil
                                 (if (zero? (first row)) n
                                   (recur (rest row) (+ n 1)))))
        has-zero? (fn [a-vector] (contains? (set a-vector) 0))]
    (if (empty? board) nil
      (if (has-zero? (first board)) [(- 9 (count board)) (find-empty-point-row (first board) 0)]
        (find-empty-point (rest board))))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) board [])
    (let [insert-point (find-empty-point board)
          insert-values (valid-values-for board insert-point)]
      (reduce (fn [a b]
                (let [a-solution (solve (set-value-at board insert-point b))]
                  (if (valid-solution? a-solution) a-solution a))) [] insert-values))))

















