(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (reduce (fn [acc el] (conj acc (get el col))) #{} board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn my-coord-pairs [rows columns]
  (for [row rows
        col columns]
    [row col]))

(defn block-values [board coord]
  (let [x (- (first coord) (mod (first coord) 3))
        y (- (second coord) (mod (second coord) 3))]
    (set (for [pair (my-coord-pairs
                      (range x (+ x 3))
                      (range y (+ y 3)))]
      (value-at board pair)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (block-values board coord)
      (row-values board coord)
      (col-values board coord))))

(defn all-numbers [board]
  (reduce concat board))


(defn filled? [board]
  (not (contains? (set (all-numbers board)) 0)))

(defn rows [board]
  (let [row-coords (for [x (range 0 9)]
                     [x 0])]
    (for [coord row-coords]
      (row-values board coord))))

(defn valid-rows? [board]
  (let [rows (rows board)]
    (every? (fn [row] (= all-values row)) rows)))

(defn cols [board]
  (let [col-coords (for [y (range 0 9)]
                     [0 y])]
    (for [coord col-coords]
      (col-values board coord))))

(defn valid-cols? [board]
  (let [cols (cols board)]
    (every? (fn [col] (= all-values col)) cols)))

(defn blocks [board]
  (let [block-coords (coord-pairs [0 3 6])]
    (for [coord block-coords]
      (block-values board coord))))

(defn valid-blocks? [board]
  (let [blocks (blocks board)]
    (every? (fn [block] (= all-values block)) blocks)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (coord-pairs (range 9))
        f (fn [x] (= 0 (value-at board x)))]
    (first (filter f coords))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [first-empty (find-empty-point board)]
      (for [valid-value (valid-values-for board first-empty)
            solution (solve (set-value-at board first-empty valid-value))]
        solution))))

