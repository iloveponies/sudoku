(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn transpose [board]
  (apply mapv vector board))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (or (nil? (value-at board coord)) (zero? (value-at board coord))) false true))

(defn row-values [board coord]
  (set (distinct (get board (first coord)))))

(defn col-values [board coord]
  (row-values (apply mapv vector board) (reverse coord)))

(defn coord-pairs [coords]
  (vec (for [x coords y coords] [x y])))

(defn top-left-coords [coords]
  (let [x (cond
            (<= (first coords) 2) 0
            (<= (first coords) 5) 3
            :else 6)
        y (cond
            (<= (last coords) 2) 0
            (<= (last coords) 5) 3
            :else 6)]
    [x y]))


(defn block-coords [coord]
  (for [base-coords (coord-pairs [0 1 2])]
    (map + base-coords (top-left-coords coord))))

(defn block-values [board coord]
  (set (distinct (for [block-coord (block-coords coord)]
              (value-at board block-coord)))))

(defn valid-values-for [board coord]
  (if (not (zero? (value-at board coord)))
    #{}
    (let [reserved-values (clojure.set/union (row-values board coord) (col-values board coord) (block-values board coord))]
      (clojure.set/difference all-values reserved-values))))

(defn filled? [board]
  (not (eval (conj (for [row board]
    (eval (conj (for [cell row] (zero? cell)) 'or))) 'or))))

(defn rows [board]
  (vec (for [row board]
    (set (distinct row)))))

(defn valid-rows? [board]
  (eval
    (conj
      (for [board-row (rows board)]
        (empty? (clojure.set/difference all-values board-row))) 'and)))

(defn cols [board]
  (rows (transpose board)))

(defn valid-cols? [board]
  (valid-rows? (transpose board)))

(defn blocks [board]
  (for [top-left-corner (coord-pairs [0 3 6])]
    (block-values board top-left-corner)))

(defn valid-blocks? [board]
  (eval
    (conj
      (for [board-block (blocks board)]
        (empty? (clojure.set/difference all-values board-block))) 'and)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-points [board]
  (filter vector?
    (for [x (range 0 9) y (range 0 9)]
      (if (zero? (value-at board [x y])) [x y]))))

(defn find-empty-point [board]
  (first (find-empty-points board)))

(defn find-solvable-points [board]
  (filter (fn [x] (= 1 (count (valid-values-for board x)))) (find-empty-points board)))

(defn find-solvable-point [board]
  (first (find-solvable-points board)))

(defn solve-helper [current-board]
  (let [solvable-point (find-solvable-point current-board)
        valid-values (valid-values-for current-board solvable-point)
        solution (if (= (count valid-values) 1) (set-value-at current-board solvable-point (first valid-values)))]
      solution))

(defn solve [board]
  (let [solution (solve-helper board)]
    (if (valid-solution? solution)
      solution
      (solve solution))))

