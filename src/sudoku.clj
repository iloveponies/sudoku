(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})
(comment
  (def sudoku-board
    (board [[5 3 0 0 7 0 0 0 0]
            [6 0 0 1 9 5 0 0 0]
            [0 9 8 0 0 0 0 6 0]
            [8 0 0 0 6 0 0 0 3]
            [4 0 0 8 0 3 0 0 1]
            [7 0 0 0 2 0 0 0 6]
            [0 6 0 0 0 0 2 8 0]
            [0 0 0 4 1 9 0 0 5]
            [0 0 0 0 8 0 0 7 9]]))

  (def solved-board
    (board [[5 3 4 6 7 8 9 1 2]
            [6 7 2 1 9 5 3 4 8]
            [1 9 8 3 4 2 5 6 7]
            [8 5 9 7 6 1 4 2 3]
            [4 2 6 8 5 3 7 9 1]
            [7 1 3 9 2 4 8 5 6]
            [9 6 1 5 3 7 2 8 4]
            [2 8 7 4 1 9 6 3 5]
            [3 4 5 2 8 6 1 7 9]]))

  (def invalid-board
    (board [[5 3 4 6 7 8 9 1 2]
            [6 7 4 1 9 5 3 4 8]
            [1 9 8 3 4 2 5 6 7]
            [8 5 9 7 6 1 4 2 3]
            [4 2 6 8 5 3 7 9 1]
            [7 1 3 9 2 4 8 5 6]
            [9 6 1 5 3 7 2 8 4]
            [2 8 7 4 1 9 6 3 5]
            [3 4 5 2 8 6 1 7 9]])))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn old-col-values [board coord]
  (loop [row 0 col (second coord) out #{}]
    (if (= row 9)
      out
      (recur (inc row) col (conj out (get-in board [row col]))))))

(defn col-values [board coord]
  (set
   (for [row board] (get row (second coord)))))

(defn coord-pairs [coords]
  (for [row coords col coords]
    [row col]))

(defn corner [coords]
  (let [f (fn [x] (cond (<= 0 x 2) 0
                       (<= 3 x 5) 3
                       (<= 6 x 8) 6))]
    (map f coords)))

(defn block-values [board coord]
  (let [c (corner coord)
        x (first c)
        y (second c)
        vals (for [row (range x (+ x 3)) col (range y (+ y 3))]
               (get-in board [row col]))]
    (set vals)))

(defn valid-values-for [board coord]
  (let [b board c coord]
    (if (has-value? b c) #{}
        (set/difference all-values
                        (row-values b c)
                        (col-values b c)
                        (block-values b c)))))

(defn filled? [board]
  (let [nums (set (flatten board))]
    (not (contains? nums 0))))

(defn valid? [group board]
  (let [p (fn [s] (= s all-values))
        x (filter p (group board))]
    (= (count x) 9)))

(defn rows [board]
  (reduce (fn [a b] (conj a (row-values board [b]))) [] (range 9)))

(defn valid-rows? [board]
  (valid? rows board))

(defn cols [board]
  (reduce (fn [a b] (conj a (col-values board [0 b]))) [] (range 9)))

(defn valid-cols? [board]
  (valid? cols board))

(defn blocks [board]
  (for [row [0 3 6] col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (valid? blocks board))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [vals (for [x (range 9) y (range 9)]
                (when (zero? (get-in board [x y]))
                  [x y]))
        empties (filter (fn [x] x) vals)]
    (first empties)))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [box (find-empty-point board)]
      (for [val (valid-values-for board box)
            solution (solve (set-value-at board box val))]
        solution))))
