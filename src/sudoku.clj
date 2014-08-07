(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def sudoku-board-test
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

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(defn col-values [board coord]
  (into #{} (map #(get % (second coord)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn- upper-left [coord] (map #(* 3 (quot % 3)) coord))

(defn block-values [board coord]
  (let [[r c] (upper-left coord)]
    (into #{}
      (for [row (range r (+ r 3))
            col (range c (+ c 3))]
        (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))))

(defn filled? [board]
  (not
    (contains? (into #{} (apply concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map set (apply map list board)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (for [starters [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]]
    (block-values board starters)))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-cols? board)
    (valid-rows? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(= (value-at board %) 0) (for [x (range 9) y (range 9)] [x y]))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [blank (find-empty-point board)]
      (apply vector
        (for [val (valid-values-for board blank)
              sol (solve (set-value-at board blank val))]
          sol)))))

