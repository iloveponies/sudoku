(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

;(def sudoku-board
;  (board [[5 3 0 0 7 0 0 0 0]
;          [6 0 0 1 9 5 0 0 0]
;          [0 9 8 0 0 0 0 6 0]
;          [8 0 0 0 6 0 0 0 3]
;          [4 0 0 8 0 3 0 0 1]
;          [7 0 0 0 2 0 0 0 6]
;          [0 6 0 0 0 0 2 8 0]
;          [0 0 0 4 1 9 0 0 5]
;          [0 0 0 0 8 0 0 7 9]]))
;
;(def solved-board
;  (board [[5 3 4 6 7 8 9 1 2]
;          [6 7 2 1 9 5 3 4 8]
;          [1 9 8 3 4 2 5 6 7]
;          [8 5 9 7 6 1 4 2 3]
;          [4 2 6 8 5 3 7 9 1]
;          [7 1 3 9 2 4 8 5 6]
;          [9 6 1 5 3 7 2 8 4]
;          [2 8 7 4 1 9 6 3 5]
;          [3 4 5 2 8 6 1 7 9]]))
;
;(def invalid-board
;  (board [[5 3 4 6 7 8 9 1 1]
;          [6 7 2 1 9 5 3 4 8]
;          [1 9 8 3 4 2 5 6 7]
;          [8 5 9 7 6 1 4 2 3]
;          [4 2 6 8 5 3 7 9 1]
;          [7 1 3 9 2 4 8 5 6]
;          [9 6 1 5 3 7 2 8 4]
;          [2 8 7 4 1 9 6 3 5]
;          [3 4 5 2 8 6 1 7 9]]))

(def all-values #{1 2 3 4 5 6 7 8 9})

; y↓ x→
(defn value-at [board [y x]]
  (get-in board [y x]))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-col-values [board value-extractor]
  (let [dimension (count board)]
    (loop [idx 0
           accum #{}]
      (if (< idx dimension)
        (recur (inc idx) (conj accum (value-extractor idx)))
        accum))))

(defn row-values [board [row _]]
  (row-col-values board #(value-at board [row %]))) ; lambda #() with placeholder % like Scala's _

(defn col-values [board [_ col]]
  (row-col-values board #(value-at board [% col])))

(defn coord-pairs [coords]
  (for [fst coords ; for-comprehension
        snd coords]
    [fst snd]))

(defn block-values [board [y x]]
  (let [[top-left-y top-left-x] [(* 3 (quot y 3)) (* 3 (quot x 3))] ; integer division
        block-coords (map
                       (fn [[y x]] [(+ y top-left-y) (+ x top-left-x)])
                       (coord-pairs [0 1 2]))]
    (reduce
      (fn [accum coord] (conj accum (value-at board coord)))
        #{}
      block-coords)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
      #{}
    (set/difference
      all-values
      (block-values board coord)
      (row-values board coord)
      (col-values board coord))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (reduce
    (fn [accum row] (conj accum (set row)))
    []
    board))

(defn valid-dimension? [dim-vals]
  (reduce
    (fn [accum row-vals] (and accum (= 9 (count row-vals))))
    true
    dim-vals))

(defn valid-rows? [board]
  (valid-dimension? (rows board)))

(defn cols [board]
  (reduce
    (fn [accum col-idx] (conj accum (col-values board [0 col-idx])))
    []
    (range 0 (count board))))

(defn valid-cols? [board]
  (valid-dimension? (cols board)))

(defn blocks [board]
  (let [blocks-corners (for [x [0 1 2]
                             y [0 1 2]]
                         [(* 3 x) (* 3 y)])]
    (reduce
      (fn [accum corner] (conj accum (set (block-values board corner))))
      []
      blocks-corners)))

(defn valid-blocks? [board]
  (valid-dimension? (blocks board)))

; = true ... trick since we can't use "and" - it's not a fn it's a macro
(defn valid-solution? [board]
  (apply = true (map #(% board) [valid-rows? valid-blocks? valid-blocks?])))

(defn set-value-at [board [y x] new-value]
  (assoc-in board [y x] new-value))

;(def before-change
;  (board [[5 3 0 0 7 0 0 0 0]
;          [6 0 0 1 9 5 0 0 0]
;          [0 9 8 0 0 0 0 6 0]
;          [8 0 0 0 6 0 0 0 3]
;          [4 0 0 8 0 3 0 0 1]
;          [7 0 0 0 2 0 0 0 6]
;          [0 6 0 0 0 0 2 8 0]
;          [0 0 0 4 1 9 0 0 5]
;          [0 0 0 0 8 0 0 7 9]]))

(defn find-empty-point [board]
  (loop [rows board
         y 0]
    (if (empty? rows)
      nil ; board is full
      (let [x (.indexOf (first rows) 0)]
        (if (neg? x)
          (recur (rest rows) (inc y)) ; not found yet
          [y x])))))

; http://www.websudoku.com/?level=4&set_id=3146650276
;(def maksim-sudoku [[2 4 0 0 0 0 0 5 0]
;                    [0 9 0 0 7 4 0 2 0]
;                    [5 0 0 0 6 0 0 0 0]
;                    [8 5 0 0 0 0 0 0 0]
;                    [0 0 0 4 0 8 0 0 0]
;                    [0 0 0 0 0 0 0 1 4]
;                    [0 0 0 0 1 0 0 0 7]
;                    [0 7 0 3 5 0 0 6 0]
;                    [0 3 0 0 0 0 0 9 1]])
;
;(def maksim-sudoku-solution [[2 4 6 1 8 3 7 5 9]
;                             [3 9 8 5 7 4 1 2 6]
;                             [5 1 7 9 6 2 4 8 3]
;                             [8 5 4 7 9 1 6 3 2]
;                             [1 6 3 4 2 8 9 7 5]
;                             [7 2 9 6 3 5 8 1 4]
;                             [9 8 5 2 1 6 3 4 7]
;                             [4 7 1 3 5 9 2 6 8]
;                             [6 3 2 8 4 7 5 9 1]])

(defn solve [board]
  (let [solver (fn solver' [board]
                 (if (filled? board)
                   (if (valid-solution? board) [board] [])
                   (let [empty-point (find-empty-point board)]
                     (for [valid-val (valid-values-for board empty-point)
                           solution (solver' (set-value-at board empty-point valid-val))]
                       solution))))]
    (first (solver board))))