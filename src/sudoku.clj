(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def board-size 9)
(def block-size 3)
(def blocks (/ board-size block-size))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(defn col-values [board coord]
  (into #{} (map #(get %1 (second coord)) board )))

(defn coord-pairs
  ([coords]
    (for [row coords
          col coords]
      [row col]))
  ([top-row left-col n]
    (for [x (range n)
          y (range n)]
      [(+ top-row x) (+ left-col y)])))

(defn top-left-of-block [coord]
  (let [[row col] coord]
    (vector (- row (int (mod row 3))) (- col (int (mod col 3))))))

(defn block-values [board coord]
  (let [[ox oy] (top-left-of-block coord)]
    (set (for [x (range 3) y (range 3)] (value-at board [(+ ox x) (+ oy y)])))))

(defn valid-values-for [board coord]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
  (if (has-value? board coord)
    #{}
  (clojure.set/difference all-values (clojure.set/union (col-values board coord) (row-values board coord) (block-values board coord))))))

(defn filled? [board]
  (nil? (some #{0} (set (for [x (range 9) y (range 9)] (value-at board [x y]))))))

(defn rows [board]
  (vec (map #(row-values board [% 0]) (range 9))))

(defn valid-rows? [board]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
    (every? true? (map #(= all-values %) (rows board)))))

(defn cols [board]
  (vec (map #(col-values board [0 %]) (range 9))))

(defn valid-cols? [board]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
    (every? true? (map #(= all-values %) (cols board)))))

(defn blocks [board]
  (vec (for [x (range 0 9 3) y (range 0 9 3)] (block-values board [x y]))))

(def all-coords
  (for [row (range board-size)
        col (range board-size)]
    [row col]))

(defn value-at [board coord]
  (get-in board coord))

(defn filter-empty-points [board]
  (filter #(zero? (value-at board %)) all-coords))

(defn valid-blocks? [board]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
    (every? true? (map #(= all-values %) (blocks board)))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter-empty-points board)))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [start-with (find-empty-point board)]
      (for [option (valid-values-for board start-with)
            solution (solve (set-value-at board start-with option))]
        solution))))
