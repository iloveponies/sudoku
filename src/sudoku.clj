(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)
(def sudoku-board2
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
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map #(% (second coord)) board)))

(defn coord-pairs [coords]
  (for [pairs1 coords
        pairs2 coords] [pairs1 pairs2]))

(defn find-start [coord]
  [(cond
    (<= (first coord) 2) 0
    (<= 3 (first coord) 5) 3
    :else 6)
   (cond
    (<= (second coord) 2) 0
    (<= 3 (second coord) 5) 3
    :else 6)])

(defn block-values [board [x1 y1]]
  (let [start (find-start [x1 y1])
        x (first start)
        y (second start)]
    (set (map #(value-at board %) (for [xxx [x (inc x) (+ 2 x)]
                                        yyy [y (inc y) (+ 2 y)]]
                                    [xxx yyy])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (set/union (block-values board coord)
                               (col-values board coord)
                               (row-values board coord)))))

(defn filled? [board]
  (not ((reduce set/union (map set board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [s] (= all-values s)) (rows board)))

(defn cols [board]
  (for [c (range 0 9)] (set (map (fn [x] (get x c)) board ))))

(defn valid-cols? [board]
  (every? (fn [s] (= all-values s)) (cols board)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]] (block-values board [x y])))

(defn valid-blocks? [board]
  (every? (fn [s] (= all-values s)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (complement nil?)
                 (for [x (range 0 9)
                       y (range 0 9)] (if ((complement has-value?) board [x y]) [x y])))))


(defn solve [board]
  (if (valid-solution? board)
    board
    (let [point (find-empty-point board)
          valid-values (valid-values-for board point)]
      (for [solution valid-values
            result (solve (set-value-at board point solution))]
        result ))))

