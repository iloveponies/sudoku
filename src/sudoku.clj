(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [x] (get x col)) board)))

(defn coord-pairs [coords]
  (for [p1 coords
        p2 coords]
    [p1 p2]))

; [0 0] [0 3] [0 6]
; [3 0] [3 3] [3 6]
; [6 0] [6 3] [6 6]
(defn top-left [[row col]]
  (let [round-down (fn [x] (- x (mod x 3)))]
    [(round-down row) (round-down col)]))

(defn block-values [board coord]
  (let [t-l (top-left coord)
        three-by-three (coord-pairs [0 1 2])
        coords (map (fn [x] (map + x t-l)) three-by-three)]
    (set (map (partial value-at board) coords))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (let [used-values (set/union
               (row-values board coord)
               (col-values board coord)
               (block-values board coord))]
    (if (has-value? board coord)
      #{}
      (set/difference all-values used-values))))

(defn filled? [board]
  (let [flattened-board (flatten board)]
    (loop [rst flattened-board]
      (cond
        (empty? rst) true
        (= 0 (first rst)) false
        :else (recur (rest rst))))))

(defn rows [board]
  (for [row board]
    (set row)))

(defn all-values-helper [acc x]
  (and acc (= x all-values)))

(defn valid-rows? [board]
  (reduce all-values-helper true (rows board)))

(defn cols [board]
  (for [coord (range 0 9)]
    (col-values board [0 coord])))

(defn valid-cols? [board]
  (reduce all-values-helper true (cols board)))

(defn blocks [board]
  (let [starts (range 0 9 3)]
    (for [x starts
          y starts]
      (block-values board [x y]))))

(defn valid-blocks? [board]
  (reduce all-values-helper true (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
    (filter (comp not nil?)
      (for [x (range 0 9)
            y (range 0 9)]
        (let [coord [x y]]
          (if (has-value? board coord)
            nil
            coord))))))

(defn solve-helper [board]
  (if (valid-solution? board)
    board
    (let [empty-point (find-empty-point board)]
      (for [valid-value (valid-values-for board empty-point)
            solution (solve-helper (set-value-at board empty-point valid-value))]
        solution))))

(defn solve [board]
  (solve-helper board))
