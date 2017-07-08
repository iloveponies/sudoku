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
            [3 4 5 2 8 6 1 7 9]])))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

(defn row-values [board coord]
  (let [[y x] coord]
    (set (get board y))))

(defn col-values [board coord]
  (let [[y x] coord]
    (set (map (fn [k] (value-at board [k x])) (range 0 9)))))

(defn coord-pairs [coords]
  (for [coord1 coords
        coord2 coords]
    (vector coord1 coord2)))

(defn block-values [board coord]
  (defn helper [coord]
    (let [[y x] coord]
      (vector (- y (mod y 3)) (- x (mod x 3)))))
  (defn all-vals [y x]
    (for [all-y (range y (+ y 3))
          all-x (range x (+ x 3))]
      (value-at board (vector  all-y all-x))))
  (let [[y x] (helper coord)
        vals (all-vals y x)]
    (set vals)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [row (row-values board coord)
          col (col-values board coord)
          block (block-values board coord)]
      (set/difference all-values (set/union row col block)))))

(defn filled? [board]
  (defn helper [board]
    (for [y (range 0 9)
          x (range 0 9)]
      (value-at board [y x])))
  (not (contains? (set (helper board)) 0)))

(defn fn-builder [function board]
  (fn [coord] (function board coord)))

(defn rows [board]
  (let [all-rows (map (fn [x] (vector x 0)) (range 0 9))]
    (map (fn-builder row-values board) all-rows)))

(defn valid-helper [function board]
  (defn map-helper [x]
    (= all-values x))
  (defn red-helper [x y]
    (and x y))
  (let [all-elems (map map-helper (function board))]
    (reduce red-helper all-elems)))

(defn valid-rows? [board]
  (valid-helper rows board))

(defn cols [board]
  (let [all-cols (map (fn [x] (vector 0 x)) (range 0 9))]
    (map (fn-builder col-values board) all-cols)))

(defn valid-cols? [board]
  (valid-helper cols board))

(defn blocks [board]
  (let [all-blocks (coord-pairs [0 3 6])]
    (map (fn-builder block-values board) all-blocks)))

(defn valid-blocks? [board]
  (valid-helper blocks board))

(defn valid-solution? [board]
  (let [mapped (map (fn [x] (x board))
                    [valid-cols? valid-rows? valid-blocks?])]
    (reduce (fn [x y] (and x y)) mapped)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


(defn find-empty-point [board]
  (let [all-coords (coord-pairs (range 0 9))]
    (first (drop-while
            (fn [x] (not (= 0 (value-at board x))))
            all-coords))))

(defn solve-helper [a-board]
  (let [epty (find-empty-point a-board)]
    (println a-board)
    (println epty)
    (if (not (find-empty-point a-board))
      (if (valid-solution? a-board)
        a-board
        [])
      (for [value (valid-values-for a-board epty)
            b (solve-helper
               (set-value-at a-board epty value))]
        b))))
(defn solve [board]
   (solve-helper board))
(comment (def solved (solve sudoku-board))
         (println solved)
         (println (solve (set-value-at (set-value-at solved-board [0 0] 0) [0 1] 0))))
