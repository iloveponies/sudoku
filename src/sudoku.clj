(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board [row _]]
  (set (nth board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row] (nth row col)) board)))

(defn coord-pairs [coords]
  (vec (for [x coords y coords] [x y])))

(defn starting-corner [[row col]]
  (let [index (fn [i] (* 3 (int (/ i 3))))
        r (index row)
        c (index col)]
    [r c]))

(defn block-coords [coord]
  (let [[r c] (starting-corner coord)]
    (vec
      (for [x (range r (+ r 3))
            y (range c (+ c 3))]
        [x y]))))

(defn block-values [board coord]
  (let [coords (block-coords coord)]
    (set (map (fn [c] (value-at board c)) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [row-vals (row-values board coord)
          col-vals (col-values board coord)
          block-vals (block-values board coord)
          used-vals (set/union row-vals col-vals block-vals)]
      (set/difference all-values used-vals))))

(defn filled? [board]
  (if (some zero? (reduce concat board)) false true))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [row] (= row all-values)) (rows board)))

(defn cols [board]
  (for [c (range 0 9)]
    (col-values board [0 c])))

(defn valid-cols? [board]
  (every? (fn [col] (= col all-values)) (cols board)))

(defn blocks [board]
  (for [r [0 3 6]
        c [0 3 6]]
    (block-values board [r c])))

(defn valid-blocks? [board]
  (every? (fn [block] (= block all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [r (range 0 9)
                     c (range 0 9)]
                 [r c])]
    (first (filter (fn [c] ((complement has-value?) board c)) coords))))

(defn solve [board]
  (if (and (filled? board) (valid-solution? board))
    board
    (let [coord (find-empty-point board)]
      (for [v (valid-values-for board coord)
            solution (solve (set-value-at board coord v))]
        solution))))
